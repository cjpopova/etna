import os
import pathlib

from benchtool.Coq import Coq
from benchtool.Types import BuildConfig, TrialConfig, ReplaceLevel, LogLevel
from benchtool.Tasks import tasks


def collect(results: str):
    tool = Coq(results=results, replace_level=ReplaceLevel.REPLACE, log_level=LogLevel.DEBUG)
    for workload in tool.all_workloads():
        if workload.name not in ['BSTProplang', 'RBTProplang', 'STLCProplang']:
            continue

        tool._preprocess(workload)

        for variant in tool.all_variants(workload):
            if variant.name == 'base':
                continue

            run_trial = None

            for strategy in tool.all_strategies(workload):
                for property in tool.all_properties(workload):
                    property = 'test_' + property
                    workloadname = workload.name[:-8]
                    if property[10:] not in tasks[workloadname][variant.name]:
                        continue
                    
                    # Don't compile tasks that are already completed.
                    finished = set(os.listdir(results))
                    file = f'{workload.name},{strategy.name},{variant.name},{property},deeper'
                    if f'{file}.json' in finished:
                        continue

                    if not run_trial:
                        run_trial = tool.apply_variant(workload, variant, BuildConfig(
                            path=workload.path,
                            clean=False,
                            build_common=False,
                            build_strategies=True,
                            build_fuzzers=True,
                            no_base=True,
                        ))

                    cfg = TrialConfig(workload=workload,
                                        strategy=strategy.name,
                                        property=property,
                                        file=file,
                                        trials=10,
                                        timeout=60,
                                        short_circuit=True)
                    run_trial(cfg)


if __name__ == '__main__':
    filepath = pathlib.Path(__file__).resolve().parent
    collect(pathlib.Path(filepath, 'results'))

