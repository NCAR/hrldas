"""
paths_config.example.py -- Reference values for the original PSO (Pasoh, Malaysia)
example. Copy to paths_config.py and edit for your own site.
"""

# === EDIT THESE TO MATCH YOUR ENVIRONMENT ===

BASE_DIR = "/home/petrichor/ymwang/snap/Emulator-based_calibration/calibration-PSO"
SITE_NAME = "PSO"
FORCING_SOURCE = "/home/petrichor/ymwang/snap/Noah-mp/data/PSO_single_point"

NOAHMP_EXE = f"{BASE_DIR}/noahmp/point_run/hrldas.exe"
NOAHMP_LD_LIBRARY_PATH = "/home/petrichor/ymwang/.local/lib:/usr/local/cuda-12.1/lib64"
HRLDAS_SETUP_FILE = f"{FORCING_SOURCE}/hrldas_setup_single_point.nc"

OBS_DIR = f"{BASE_DIR}/data/obs"
OBS_FILE_30MIN = f"{OBS_DIR}/{SITE_NAME}_obs_30min.csv"

# Malaysia PSO is UTC+8
UTC_OFFSET_HOURS = 8

# === DERIVED -- usually do not edit ===

NOAHMP_DIR = f"{BASE_DIR}/noahmp/point_run"
TBL_GENERATOR_DIR = f"{BASE_DIR}/noahmp/TBL_generator"
PARAM_DIR = f"{BASE_DIR}/data/raw/param"
SRC_DIR = f"{BASE_DIR}/src"
LOG_DIR_BASE = f"{BASE_DIR}/logs"
