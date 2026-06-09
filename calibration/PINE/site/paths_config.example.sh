#!/bin/bash
####################################################################################
# paths_config.example.sh -- Reference values for the original PSO (Pasoh, Malaysia)
# example.
#
# NOTE: paths_config.sh is now auto-generated from paths_config.py.
# You only need to edit paths_config.py. This example file is kept for reference.
####################################################################################

# === EDIT THESE IN paths_config.py ===

BASE_DIR="/home/petrichor/ymwang/snap/Emulator-based_calibration/calibration-PSO"
SITE_NAME="PSO"
FORCING_SOURCE="/home/petrichor/ymwang/snap/Noah-mp/data/PSO_single_point"

NOAHMP_EXE="${BASE_DIR}/noahmp/point_run/hrldas.exe"
NOAHMP_LD_LIBRARY_PATH="/home/petrichor/ymwang/.local/lib:/usr/local/cuda-12.1/lib64"
HRLDAS_SETUP_FILE="${FORCING_SOURCE}/hrldas_setup_single_point.nc"

OBS_DIR="${BASE_DIR}/data/obs"
OBS_FILE_30MIN="${OBS_DIR}/${SITE_NAME}_obs_30min.csv"

# Malaysia PSO is UTC+8
UTC_OFFSET_HOURS=8

# === DERIVED -- usually do not edit ===

NOAHMP_DIR="${BASE_DIR}/noahmp/point_run"
TBL_GENERATOR_DIR="${BASE_DIR}/noahmp/TBL_generator"
PARAM_DIR="${BASE_DIR}/data/raw/param"
SRC_DIR="${BASE_DIR}/src"
LOG_DIR_BASE="${BASE_DIR}/logs"
