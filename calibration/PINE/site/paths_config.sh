#!/bin/bash
####################################################################################
# paths_config.sh -- Global path configuration for shell workflow scripts
#
# AUTO-GENERATED from paths_config.py. Edit paths_config.py only -- this file
# reads all values from the Python config so the two can never go out of sync.
#
# This file is sourced by every workflow shell script
# (run_model_training.sh, run_calibration.sh, run_noahmp_batch.sh,
#  06_calibration_validation.sh, 07_importance_analysis.sh).
####################################################################################

SCRIPT_DIR_CFG="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Read all path variables from paths_config.py using Python
eval "$(python3 - "${SCRIPT_DIR_CFG}" << 'PYEOF'
import sys, importlib.util

project_dir = sys.argv[1]
spec = importlib.util.spec_from_file_location("paths_config", f"{project_dir}/paths_config.py")
cfg = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cfg)

# Export every uppercase string attribute
for name in dir(cfg):
    if name.startswith('_'):
        continue
    val = getattr(cfg, name)
    if isinstance(val, str):
        print(f'{name}="{val}"')
    elif isinstance(val, (int, float)):
        print(f'{name}={val}')
PYEOF
)"
