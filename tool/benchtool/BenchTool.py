from abc import ABC, abstractmethod
from typing import Callable, Optional

from benchtool.Mutant import Parser
from benchtool.Types import (
    Config,
    Entry,
    LogLevel,
    Original,
    ReplaceLevel,
    TrialArgs,
    TrialConfig,
    Variant,
    Variable,
)
from benchtool.Util import ChangeDir, scandir_filter, recursive_scandir_filter

import glob
import itertools
import json
import os
import shutil as sh
import subprocess
import sys
import tempfile
import logging

class BenchTool(ABC):
    results: str
    _logger = logging.getLogger(__name__)
    __temp: str
    _replace_level: ReplaceLevel = ReplaceLevel.REPLACE
    _config: Config
    __variant: Optional[Variant]

    def __init__(
        self,
        config: Config,
        results: str,
        log_level: int | str = logging.INFO,
        replace_level: ReplaceLevel = ReplaceLevel.REPLACE,
        log_file: str | None = None,
    ):
        self.results = results
        self._config = config
        self._log_level = log_level
        self._replace_level = replace_level
        self.__temp = tempfile.mkdtemp()

        FORMAT = {
            'fmt': '%(asctime)s [%(levelname)s] [%(filename)s/%(funcName)s] %(message)s',
            'datefmt': '%Y-%m-%d %H:%M:%S',
        }
        logging.basicConfig(level=logging.DEBUG, format=FORMAT['fmt'], datefmt=FORMAT['datefmt'])

        self._logger.addHandler(logging.StreamHandler())
        self._logger.handlers[0].setFormatter(logging.Formatter(fmt=FORMAT['fmt'], datefmt=FORMAT['datefmt']))

        if log_file:
            self._logger.addHandler(logging.FileHandler(log_file))
            self._logger.handlers[1].setFormatter(logging.Formatter(fmt=FORMAT['fmt'], datefmt=FORMAT['datefmt']))

        try:
            os.mkdir(results)
        except FileExistsError:
            self._log(f"Results directory {results} already exists.", logging.WARNING)

        sh.copytree(self._config.path, os.path.join(self.__temp, self._config.path))

    def temp(self) -> str:
        return self.__temp

    def set_log_level(self, log_level: LogLevel):
        """Sets log level."""
        self._log_level = log_level

    def all_workloads(self) -> list[Entry]:
        """
        Assumes that all top-level directories in `config.path`
        other than `config.ignore` are workloads.

        :return: List of workloads.
        """

        def is_workload(entry) -> bool:
            return os.path.isdir(entry) and entry.name != self._config.ignore

        return scandir_filter(self._config.path, is_workload)

    def all_variants(self, workload: Entry) -> list[Variant]:
        """
        Assumes that all variants are in `config.impl` file.

        :return: List of variants in `workload`.
        """

        p = Parser(self._config)
        return p.extract(p.parse(workload))

    def apply_variant(
        self, workload: Entry, variant: Variant, no_base=False
    ) -> Callable[[TrialConfig], None]:
        """
        Overwrites `config.impl` file for `workload` with contents
        of the provided `variant`.

        :return: A function that can be used to run a trial.
        """

        if no_base and variant.name == "base":
            return lambda _: None

        with self._change_dir(self.__temp):
            self._log(f"Applying variant {variant}", LogLevel.DEBUG)
            self.__apply_variant_in_impl(workload, variant)

            self._log(f"Building with mutant: {variant.name}", LogLevel.INFO)
            self._build(workload.path)

        self.__variant = variant

        return self.__trial

    def just_build(self, workload: Entry) -> Callable[[TrialConfig], None]:
        """

        :return: A function that can be used to run a trial.
        """

        with self._change_dir(self.__temp):
            self._log("Just building", LogLevel.INFO)
            self._build(workload.path)

        self.__variant = Original(workload.path, "")

        return self.__trial

    def all_variables(self, workload: Entry) -> list[Variable]:
        """
        Assumes that all variables are in `variables.json` file.

        :return: List of variables in `workload`.
        """

        variables_path = os.path.join(workload.path, "variables.json")

        if not os.path.isfile(variables_path):
            return []

        with open(variables_path, "r") as f:
            variables = json.load(f)
            variables = list(
                map(
                    lambda x: Variable(
                        name=x["name"],
                        folder=x["folder"],
                        recursive=x["recursive"],
                        files=x["files"],
                        variants=x["variants"],
                    ),
                    variables,
                )
            )

        return variables

    def all_variable_mixtures(
        self, variables: list[Variable]
    ) -> list[list[tuple[Variable, int]]]:
        """
        Given a list of variables, returns all possible combinations of their variants.
        """
        if len(variables) == 0:
            return []
        if len(variables) == 1:
            return [[(variables[0], i)] for i in range(len(variables[0].variants))]
        else:
            singulars = [
                self.all_variable_mixtures([variables[i]])
                for i in range(len(variables))
            ]
            mixtures = itertools.product(*singulars)
            return list(mixtures)

    def update_variable(
        self, workload: Entry, variable: Variable, version: int
    ) -> Callable[[TrialConfig], None]:
        """
        Overwrites `config.impl` file for `workload` with contents
        of the provided `variant`.

        :return: A function that can be used to run a trial.
        """

        old = variable.variants[variable.current]
        new = variable.variants[version]

        with self._change_dir(
            os.path.join(self.__temp, workload.path, variable.folder)
        ):
            self._log(f"Updating variable {variable.name}", LogLevel.DEBUG)

            for pattern in variable.files:
                files = glob.glob(pattern, recursive=variable.recursive)
                if len(files) == 0:
                    self._log(
                        f"No files found for pattern {pattern}",
                        LogLevel.ERROR,
                    )
                    raise Exception(
                        f"No files found for pattern {pattern}",
                    )
                for file in files:
                    with open(file, "r") as f:
                        data = f.read()
                        if data.count(old) == 0:
                            self._log(
                                f"Variable({variable.name}) {old} not found in {file}",
                                LogLevel.ERROR,
                            )
                            raise Exception(
                                f"Variable({variable.name}) {old} not found in {file}"
                            )
                        else:
                            data = data.replace(old, new)
                    with open(file, "w") as f:
                        f.write(data)

            self._log(
                f"Switched Variable({variable.name}) from {old} to {new}", LogLevel.INFO
            )

        variable.current = version
        return self.__trial

    def all_strategies(self, workload: Entry) -> list[Entry]:
        """
        Assumes that all files in the `config.strategy` folder of `workload`
        that end in `config.ext` are strategies.

        :return: List of strategies in `workload`.
        """

        def is_strategy(entry) -> bool:
            return os.path.isfile(entry) and entry.name.endswith(self._config.ext)

        strategies = os.path.join(workload.path, self._config.strategies)
        entries = recursive_scandir_filter(strategies, is_strategy)

        def get_base(e: Entry) -> Entry:
            # Remove file extension to get base name.
            return Entry(e.name[: -len(self._config.ext)], e.path)

        return [get_base(e) for e in entries]

    @abstractmethod
    def all_properties(self, workload: Entry) -> list[Entry]:
        pass

    @abstractmethod
    def _build(self, workload_path: str):
        """
        Takes a path and returns the command to build the workloads.
        """
        pass

    @abstractmethod
    def _run_trial(self, workload_path: str, args: TrialArgs):
        """
        Takes a path and an argument structure, and returns the command to run
        the workloads.
        """
        pass

    def _log(self, msg: str, level: LogLevel):
        self._logger.log(level, msg)

    def _shell_command(self, cmd: list[str]) -> None:
        """
        Helper for running a subprocess with `subprocess`.
        """
        try:
            subprocess.call(
                cmd,
                stdout=sys.stdout
                if self._log_level == LogLevel.DEBUG
                else subprocess.DEVNULL,
                stderr=sys.stderr
                if self._log_level == LogLevel.DEBUG
                else subprocess.DEVNULL,
            )
        except Exception as e:
            self._log(f"Error running {cmd}: {e}", LogLevel.ERROR)
            sys.exit(1)

    def _change_dir(self, path: str) -> ChangeDir:
        """
        Helper for changing working directory.

        Usage:
        ```
        with change_dir(path):
            ...
        ```
        """
        return ChangeDir(path)

    def __apply_variant_in_impl(self, workload: Entry, variant: Variant) -> None:
        """
        Helper for applying variant.
        """

        # Overwrite `config.impl` file with current variant.
        with open(variant.filename, "w") as f:
            f.write(variant.body)

    def __trial(self, cfg: TrialConfig) -> None:
        """
        Generate one set of data for `workload`.

        Assumes that `workload` is already instantiated
        (via `apply_variant`) with the current variant.

        This is private; it should not be called directly.
        Instead you should call `apply_variant` first.
        """
        if not self.__variant:
            raise Exception("Cannot run trial without variant")

        strategy_label = cfg.label if cfg.label else cfg.strategy

        if not cfg.file:
            experiment = f"{cfg.workload.name},{strategy_label},{self.__variant.name},{cfg.property}"
        else:
            experiment = cfg.file

        file = os.path.join(self.results, f"{experiment}.json")

        if not cfg.experiment_id:
            cfg.experiment_id = experiment

        if os.path.isfile(file):
            match self._replace_level:
                case ReplaceLevel.REPLACE:
                    pass
                case ReplaceLevel.SKIP:
                    self._log(f"Skipping {experiment}", LogLevel.WARNING)
                    return
                case ReplaceLevel.FAIL:
                    raise Exception(f"Already have data for {experiment}")

        self._log(f"Running {experiment}", LogLevel.INFO)
        with self._change_dir(self.__temp):
            self._run_trial(
                cfg.workload.path,
                TrialArgs(
                    file=file,
                    experiment_id=cfg.experiment_id,
                    trials=cfg.trials,
                    workload=cfg.workload.name,
                    strategy=cfg.strategy,
                    mutant=self.__variant.name,
                    property=cfg.property,
                    timeout=cfg.timeout,
                    label=strategy_label,
                    short_circuit=cfg.short_circuit,
                ),
            )

    @abstractmethod
    def _preprocess(self, workload: Entry) -> None:
        """
        Takes a workload and does the required preprocessing.
        """
        pass

    def common(self) -> Entry:
        """
        Returns the common library entry.
        """
        return Entry(
            self._config.ignore,
            os.path.join(self.__temp, self._config.path, self._config.ignore),
        )
