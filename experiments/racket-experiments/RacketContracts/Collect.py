import os
import pathlib

from benchtool.Tasks import tasks
from benchtool.Racket import Racket
from benchtool.Types import BuildConfig, TrialConfig, ReplaceLevel

import logging

"""
Instead of properties (the equivalent of one test), we only need to loop over at the module level
    (it may still be useful to look at the properties to understand which exports are relevant to test later)

Try validating that `self.__apply_variant_in_impl(workload, variant)` produces a file config.impl with the code we need


Maybe I should look at Strategies and see if that is the code I need to sub out instead of here
"""

def collect(results: str):
    tool = Racket(results=results,log_level=logging.DEBUG, replace_level=ReplaceLevel.SKIP, log_file='myapp.log')

    for workload in tool.all_workloads():
        tool._log(f'Collecting {workload.name}...', logging.INFO)
        if workload.name in ['SYSTEMF']: #, 'BST', 'RBT', 'STLC']:
            for variant in tool.all_variants(workload):
                tool._log(f'Collecting {workload.name} {variant.name}...', logging.INFO)
                run_trial = None

                for strategy in tool.all_strategies(workload):           
                    tool._log(f'Collecting {workload.name} {variant.name} {strategy.name}...', logging.INFO)

                    properties = tool.all_properties(workload) if workload.name != 'SYSTEMF' else ['prop_SinglePreserve', 'prop_MultiPreserve'] 

                    for property in properties:
                        tool._log(f'Collecting {workload.name} {variant.name} {strategy.name} {property}...', logging.INFO)
                        if workload.name != "SYSTEMF" and property.split('_')[1] not in tasks[workload.name][variant.name]:
                            tool._log(f'Skipping {workload.name} {variant.name} {strategy.name} {property}...', logging.INFO)
                            continue
                        property = 'test_' + property       
                        
                        # Don't compile tasks that are already completed.
                        finished = set(os.listdir(results))
                        file = f'{workload.name},{strategy.name},{variant.name},{property}'
                        if f'{file}.json' in finished:
                            tool._log(f'Skipping {workload.name} {variant.name} {strategy.name} {property}...', logging.INFO)
                            continue
                        
                        experiment_id = os.environ.get("ETNA_EXPERIMENT_ID") or file


                        if not run_trial:
                            run_trial = tool.apply_variant(workload, variant, BuildConfig(
                                    path=workload.path,
                                    clean=False,
                                    build_common=False,
                                    build_strategies=True,
                                    build_fuzzers=False,
                                    no_base=True,
                                ))

                        tool._log(f'Running {workload.name} {variant.name} {strategy.name} {property}...', logging.INFO)
                        cfg = TrialConfig(workload=workload,
                                        strategy=strategy.name,
                                        property=property,
                                        experiment_id=experiment_id,
                                        trials=10,
                                        timeout=60,
                                        short_circuit=True)
                        #run_trial(cfg)


if __name__ == '__main__':
    filepath = pathlib.Path(__file__).resolve().parent
    collect(pathlib.Path(filepath, 'results'))
