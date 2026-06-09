"""
paths_config.py -- Global path configuration (single source of truth)

*** BEFORE YOUR FIRST RUN, EDIT THE PATHS BELOW TO POINT AT YOUR SITE'S DATA. ***

This module is imported by every Python script that needs a path
(e.g. `import paths_config` then `paths_config.OBS_DIR`).

Shell scripts read the same values via `paths_config.sh`, which is
auto-generated from this file -- you only need to edit this one.

The values shipped here reproduce the original Pasoh (Malaysia) example used in
the paper. They are NOT defaults for a new site -- treat them as placeholders.
"""

# =============================================================================
# === EDIT THESE TO MATCH YOUR ENVIRONMENT ===
# =============================================================================

# 1. Absolute path to YOUR clone of this repository.
#    Every other path is derived from this (unless overridden below).
BASE_DIR = "/home/petrichor/ymwang/snap/Emulator-based_calibration/Code-for-Assessing-and-enhancing-Noah-MP-over-tropical-forests"

# 2. Site identifier. Used in forcing/observation filenames and figure titles.
SITE_NAME = "PSO"

# 3. Directory containing your LDASIN_DOMAIN1 forcing files (30-min cadence).
#    Noah-MP reads these directly.
FORCING_SOURCE = "/home/petrichor/ymwang/snap/Noah-mp/data/PSO_single_point"

# 4. Noah-MP runtime requirements.
#    - NOAHMP_EXE: the compiled HRLDAS executable.
#      Replace `noahmp/point_run/hrldas.exe` with a binary built for your machine.
#    - NOAHMP_LD_LIBRARY_PATH: colon-separated directories holding the shared
#      libraries hrldas.exe was linked against (typically libnetcdff, libnetcdf).
#    - HRLDAS_SETUP_FILE: the single-point setup NetCDF (vegtyp, soiltyp, lat/lon).
NOAHMP_EXE = f"{BASE_DIR}/noahmp/point_run/hrldas.exe"
NOAHMP_LD_LIBRARY_PATH = "/home/petrichor/ymwang/.local/lib:/usr/local/cuda-12.1/lib64"
HRLDAS_SETUP_FILE = f"{FORCING_SOURCE}/hrldas_setup_single_point.nc"

# 5. Observation data.
#    After you run `python3 src/prepare_observation_data.py` for your site,
#    the resulting 30-min observation CSV lives here.
OBS_DIR = f"{BASE_DIR}/data/obs"
OBS_FILE_30MIN = f"{OBS_DIR}/{SITE_NAME}_obs_30min.csv"

# 6. UTC offset (in hours) of the site's local time, used to convert Noah-MP UTC
#    output timestamps to local time for diurnal-cycle plotting.
#    Examples:  Malaysia (PSO)  =  8     Panama (BCI)  = -5     UTC site = 0
UTC_OFFSET_HOURS = 8

# =============================================================================
# === DERIVED -- usually do not edit ===
# =============================================================================

NOAHMP_DIR = f"{BASE_DIR}/noahmp/point_run"
TBL_GENERATOR_DIR = f"{BASE_DIR}/noahmp/TBL_generator"
PARAM_DIR = f"{BASE_DIR}/data/raw/param"
SRC_DIR = f"{BASE_DIR}/src"
LOG_DIR_BASE = f"{BASE_DIR}/logs"
