#!/bin/bash
####################################################################################
# Calibration Workflow Script (V2 - Loop All Calibrations)
#
# Improvements:
#   1. --calibration_dir option to specify existing calibration results
#   2. Loop through ALL calibration results for validation
#
# All site/machine-specific paths come from paths_config.sh -- edit that first.
#
# Usage: bash run_calibration.sh [OPTIONS]
####################################################################################

set -e

# =============================================================================
# Load site/machine paths
# =============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/paths_config.sh"

VALIDATION_RESULTS_DIR="${BASE_DIR}/validation_results"

# Default settings
NUM_CALIBRATION=10
MAX_ITER=100
POPSIZE=15
SKIP_CALIBRATION=false
SKIP_NOAHMP=false
SKIP_ANALYSIS=false

# Model directory (auto-detect latest)
MODEL_DIR=""

# User-specified calibration directory (for --skip-calibration)
USER_CALIBRATION_DIR=""

# Timestamp for this run
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_DIR="${LOG_DIR_BASE}/calibration_${TIMESTAMP}"

# Variables to validate
VARIABLES="SOIL_M LH HFX"

# Loss weights for target variables (default: equal weights for SOIL_M, LH, HFX)
# Use --weights "1 1.5 2.5" --weight_vars "LH HFX SOIL_M" for custom weighting
WEIGHTS=""
WEIGHT_VARS=""

# =============================================================================
# Helper Functions
# =============================================================================
print_header() {
    echo ""
    echo "================================================================================="
    echo " $1"
    echo "================================================================================="
}

print_info() {
    echo -e "\033[1;34m[INFO]\033[0m $1"
}

print_success() {
    echo -e "\033[1;32m[SUCCESS]\033[0m $1"
}

print_error() {
    echo -e "\033[1;31m[ERROR]\033[0m $1"
}

print_warning() {
    echo -e "\033[1;33m[WARNING]\033[0m $1"
}

# Function to extract time ranges from config
get_time_config() {
    BASE_DIR="${BASE_DIR}" python3 << 'PYEOF'
import os, sys
sys.path.insert(0, os.environ['BASE_DIR'])
import config_forward_comprehensive as config

print(f"FULL_START={config.TIME_RANGE_FULL['start']}")
print(f"FULL_END={config.TIME_RANGE_FULL['end']}")
print(f"CAL_START={config.TIME_RANGE_CALIBRATION['start']}")
print(f"CAL_END={config.TIME_RANGE_CALIBRATION['end']}")
print(f"VAL_START={config.TIME_RANGE_VALIDATION['start']}")
print(f"VAL_END={config.TIME_RANGE_VALIDATION['end']}")
PYEOF
}

# =============================================================================
# Parse Arguments
# =============================================================================
while [[ $# -gt 0 ]]; do
    case $1 in
        --model_dir)
            MODEL_DIR="$2"
            shift 2
            ;;
        --num_calibration)
            NUM_CALIBRATION="$2"
            shift 2
            ;;
        --max_iter)
            MAX_ITER="$2"
            shift 2
            ;;
        --popsize)
            POPSIZE="$2"
            shift 2
            ;;
        --skip-calibration)
            SKIP_CALIBRATION=true
            shift
            ;;
        --calibration_dir)
            USER_CALIBRATION_DIR="$2"
            shift 2
            ;;
        --skip-noahmp)
            SKIP_NOAHMP=true
            shift
            ;;
        --skip-analysis)
            SKIP_ANALYSIS=true
            shift
            ;;
        --variables)
            VARIABLES="$2"
            shift 2
            ;;
        --weights)
            WEIGHTS="$2"
            shift 2
            ;;
        --weight_vars)
            WEIGHT_VARS="$2"
            shift 2
            ;;
        --help|-h)
            cat << EOF
Usage: $0 [OPTIONS]

Calibration Workflow - Calibrate Noah-MP parameters using LSTM emulator

Options:
  --model_dir DIR         Path to trained emulator model directory
                          (default: auto-detect latest)
  --num_calibration N     Number of independent calibration runs (default: 10)
  --max_iter N            Maximum optimization iterations (default: 100)
  --popsize N             Population size for differential evolution (default: 15)
  --variables VARS        Variables to validate (default: "SOIL_M LH HFX")
  --weights "W1 W2 ..."   Loss weights, paired 1:1 with --weight_vars.
                          Default: 1.0 for every variable in MAIN_TARGETS.
  --weight_vars "V1 V2"   Variable names that --weights apply to.
  --skip-calibration      Skip calibration step (use existing results)
  --calibration_dir DIR   Specify calibration results directory
                          (used with --skip-calibration, e.g.,
                           calibration_results/20250101_120000)
  --skip-noahmp           Skip Noah-MP validation runs
  --skip-analysis         Skip analysis and plotting
  -h, --help              Show this help message

Time ranges are configured in config_forward_comprehensive.py
Paths are configured in paths_config.sh (edit before first run)

Examples:
  # Run full calibration workflow
  bash run_calibration.sh --num_calibration 20 --max_iter 200

  # Run with custom variable weights
  bash run_calibration.sh --weights "1 1.5 2.5" --weight_vars "LH HFX SOIL_M"

  # Use specific existing calibration directory
  bash run_calibration.sh --skip-calibration --calibration_dir calibration_results/20250101_120000

EOF
            exit 0
            ;;
        *)
            print_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# =============================================================================
# Initialize
# =============================================================================
print_header "CALIBRATION WORKFLOW"
print_info "Timestamp: ${TIMESTAMP}"
print_info "Base directory: ${BASE_DIR}"
print_info "Site name: ${SITE_NAME}"

mkdir -p "${LOG_DIR}"
print_info "Log directory: ${LOG_DIR}"

cd "${BASE_DIR}"

# Auto-detect model directory if not specified
if [ -z "${MODEL_DIR}" ]; then
    MODEL_DIR=$(ls -td results_forward_comprehensive/*/ 2>/dev/null | head -1)
    if [ -z "${MODEL_DIR}" ]; then
        print_error "No trained model found. Run run_model_training.sh first."
        exit 1
    fi
fi
print_info "Model directory: ${MODEL_DIR}"

# Get time configuration
print_info "Loading time configuration from config_forward_comprehensive.py..."
eval "$(get_time_config)"

echo ""
print_info "Time Configuration:"
print_info "  Full range:        ${FULL_START} to ${FULL_END}"
print_info "  Calibration range: ${CAL_START} to ${CAL_END}"
print_info "  Validation range:  ${VAL_START} to ${VAL_END}"
echo ""

# Observation files (site-specific name comes from paths_config.sh)
OBS_CALIBRATION="${OBS_DIR}/${SITE_NAME}_obs_${CAL_START}_${CAL_END}.csv"
OBS_FULL="${OBS_FILE_30MIN}"

if [ ! -f "${OBS_CALIBRATION}" ]; then
    print_warning "Calibration observation file not found: ${OBS_CALIBRATION}"
    print_info "Falling back to full 30-min observation file: ${OBS_FULL}"
    OBS_CALIBRATION="${OBS_FULL}"
fi

if [ ! -f "${OBS_CALIBRATION}" ]; then
    print_error "Observation file not found. Please check ${OBS_DIR}"
    print_error "Run src/prepare_observation_data.py to generate observations."
    exit 1
fi

print_info "Using observation file: ${OBS_CALIBRATION}"

# Define calibration output directory
CALIBRATION_OUTPUT="${BASE_DIR}/calibration_results/${TIMESTAMP}"

# =============================================================================
# Step 1: Run Calibration using Emulator (CALIBRATION Time Range)
# =============================================================================
if [ "${SKIP_CALIBRATION}" = false ]; then
    print_header "Step 1: Parameter Calibration (${CAL_START} to ${CAL_END})"

    print_info "Running ${NUM_CALIBRATION} independent calibrations..."
    print_info "Using differential evolution optimizer"
    print_info "Max iterations: ${MAX_ITER}, Population size: ${POPSIZE}"

    # Build optional --weights / --weight_vars args
    WEIGHT_ARGS=()
    if [ -n "${WEIGHTS}" ]; then
        WEIGHT_ARGS+=(--weights ${WEIGHTS})
    fi
    if [ -n "${WEIGHT_VARS}" ]; then
        WEIGHT_ARGS+=(--weight_vars ${WEIGHT_VARS})
    fi

    python3 05_calibration_applying_emulator_multiple_runs.py \
        --model_dir "${MODEL_DIR}" \
        --forcing "data/raw/forcing/forcing_${SITE_NAME}_daily_calibration.nc" \
        --obs "${OBS_CALIBRATION}" \
        --bounds value_bounds.csv \
        --num_calibration ${NUM_CALIBRATION} \
        --max_iter ${MAX_ITER} \
        --popsize ${POPSIZE} \
        --output "${CALIBRATION_OUTPUT}" \
        "${WEIGHT_ARGS[@]}" \
        --loss_type nrmse \
        2>&1 | tee "${LOG_DIR}/01_calibration.log"

    print_success "Calibration completed!"
    print_info "Results saved to: ${CALIBRATION_OUTPUT}"
else
    print_info "Skipping calibration step"

    if [ -n "${USER_CALIBRATION_DIR}" ]; then
        if [[ "${USER_CALIBRATION_DIR}" == /* ]]; then
            CALIBRATION_OUTPUT="${USER_CALIBRATION_DIR}"
        else
            CALIBRATION_OUTPUT="${BASE_DIR}/${USER_CALIBRATION_DIR}"
        fi

        if [ ! -d "${CALIBRATION_OUTPUT}" ]; then
            print_error "Specified calibration directory does not exist: ${CALIBRATION_OUTPUT}"
            exit 1
        fi
        print_info "Using user-specified calibration directory: ${CALIBRATION_OUTPUT}"
    else
        CALIBRATION_OUTPUT=$(ls -td ${BASE_DIR}/calibration_results/*/ 2>/dev/null | head -1)
        if [ -z "${CALIBRATION_OUTPUT}" ]; then
            print_error "No calibration results found. Run calibration first or specify --calibration_dir"
            exit 1
        fi
        print_info "Auto-detected latest calibration: ${CALIBRATION_OUTPUT}"
    fi
fi

CALIBRATION_OUTPUT="${CALIBRATION_OUTPUT%/}"

# Find ALL calibration subdirectories
CALIBRATION_SUBDIRS=($(ls -d ${CALIBRATION_OUTPUT}/calibration_* 2>/dev/null | sort -V))
NUM_CAL_RESULTS=${#CALIBRATION_SUBDIRS[@]}

if [ ${NUM_CAL_RESULTS} -eq 0 ]; then
    print_error "No calibration_* subdirectories found in ${CALIBRATION_OUTPUT}"
    exit 1
fi

print_info "Found ${NUM_CAL_RESULTS} calibration result(s) to validate"

# =============================================================================
# Step 2: Validation - Run Noah-MP with Different Parameter Sets (FULL Time Range)
# Loop through ALL calibration results
# =============================================================================
if [ "${SKIP_NOAHMP}" = false ]; then
    print_header "Step 2: Validation Runs (${FULL_START} to ${FULL_END})"

    # Function to run a single Noah-MP simulation
    run_noahmp_validation() {
        local tbl_file="$1"
        local run_name="$2"
        local output_base_dir="$3"
        local run_dir="${output_base_dir}/run_${run_name}"

        echo "[INFO] Running Noah-MP with ${run_name} parameters..."

        mkdir -p "${run_dir}/output"
        cp "${NOAHMP_DIR}/hrldas.exe" "${run_dir}/"
        cp "${NOAHMP_DIR}/namelist.hrldas" "${run_dir}/"
        cp "${tbl_file}" "${run_dir}/NoahmpTable.TBL"

        # Symlink forcing data (path from paths_config.sh)
        ln -sf "${FORCING_SOURCE}" "${run_dir}/forcing"

        cd "${run_dir}"
        sed -i "s|INDIR.*=.*|INDIR = './forcing'|" namelist.hrldas
        sed -i "s|OUTDIR.*=.*|OUTDIR = './output'|" namelist.hrldas

        export LD_LIBRARY_PATH="${NOAHMP_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH}"
        ./hrldas.exe > run.log 2>&1

        if ls output/*.LDASOUT_DOMAIN1 1> /dev/null 2>&1; then
            echo "[SUCCESS] ${run_name} simulation completed"
        else
            echo "[ERROR] ${run_name} simulation failed. Check ${run_dir}/run.log"
        fi

        cd "${BASE_DIR}"
    }

    # Loop through each calibration result
    for cal_subdir in "${CALIBRATION_SUBDIRS[@]}"; do
        CAL_NAME=$(basename "${cal_subdir}")
        TIMESTAMP_DIR=$(basename "${CALIBRATION_OUTPUT}")
        CALIBRATION_ID="${TIMESTAMP_DIR}/${CAL_NAME}"

        print_info "=========================================="
        print_info "Processing: ${CALIBRATION_ID}"
        print_info "=========================================="

        VALIDATION_OUTPUT_DIR="${VALIDATION_RESULTS_DIR}/${CALIBRATION_ID}"
        mkdir -p "${VALIDATION_OUTPUT_DIR}"

        # Step 2a: Convert calibrated parameters to TBL format
        print_info "Converting calibrated parameters to TBL format..."
        EMULATOR_PARAM_FILE="${VALIDATION_OUTPUT_DIR}/emulator_calibrated_params.txt"

        python3 "${SRC_DIR}/convert_calibrated_params.py" \
            --input "${cal_subdir}/calibrated_parameters.csv" \
            --output "${EMULATOR_PARAM_FILE}" \
            2>&1 | tee -a "${LOG_DIR}/02_validation_${CAL_NAME}.log"

        # Step 2b: Generate TBL files
        print_info "Generating TBL files..."
        cd "${TBL_GENERATOR_DIR}"

        # Emulator-calibrated
        python3 noahmp_apply_samples.py \
            --samples "${EMULATOR_PARAM_FILE}" \
            --base_table NoahmpTable.TBL \
            --out_root "${VALIDATION_OUTPUT_DIR}/emulator_tbl" \
            --n_rows 1 \
            --verbose

        EMULATOR_TBL="${VALIDATION_OUTPUT_DIR}/emulator_tbl/NoahmpTable_4emu_1/NoahmpTable.TBL"

        # Default parameters
        python3 noahmp_apply_samples.py \
            --samples "${PARAM_DIR}/default_param.txt" \
            --base_table NoahmpTable.TBL \
            --out_root "${VALIDATION_OUTPUT_DIR}/default_tbl" \
            --n_rows 1

        DEFAULT_TBL="${VALIDATION_OUTPUT_DIR}/default_tbl/NoahmpTable_4emu_1/NoahmpTable.TBL"

        cd "${BASE_DIR}"

        # Step 2c: Run Noah-MP with each parameter set
        print_info "Running Noah-MP simulations for ${CAL_NAME}..."

        run_noahmp_validation "${EMULATOR_TBL}" "emulator" "${VALIDATION_OUTPUT_DIR}" 2>&1 | tee -a "${LOG_DIR}/02_validation_${CAL_NAME}.log"
        run_noahmp_validation "${DEFAULT_TBL}" "default" "${VALIDATION_OUTPUT_DIR}" 2>&1 | tee -a "${LOG_DIR}/02_validation_${CAL_NAME}.log"

        # Step 2d: Parse Noah-MP outputs
        print_info "Parsing Noah-MP outputs to CSV..."

        for run_type in emulator default; do
            output_file=$(ls ${VALIDATION_OUTPUT_DIR}/run_${run_type}/output/*.LDASOUT_DOMAIN1 2>/dev/null | head -1)
            if [ -n "${output_file}" ]; then
                python3 "${SRC_DIR}/parse_noahmp_outputs.py" \
                    --input "${output_file}" \
                    --output "${VALIDATION_OUTPUT_DIR}/${run_type}_output.csv" \
                    --utc_offset_hours "${UTC_OFFSET_HOURS}"
            fi
        done

        print_success "Validation completed for ${CAL_NAME}"
    done

    print_success "All validation runs completed!"
else
    print_info "Skipping Noah-MP validation runs"
fi

# =============================================================================
# Step 3: Analysis and Plotting
# =============================================================================
if [ "${SKIP_ANALYSIS}" = false ]; then
    print_header "Step 3: Analysis and Plotting"

    for cal_subdir in "${CALIBRATION_SUBDIRS[@]}"; do
        CAL_NAME=$(basename "${cal_subdir}")
        TIMESTAMP_DIR=$(basename "${CALIBRATION_OUTPUT}")
        CALIBRATION_ID="${TIMESTAMP_DIR}/${CAL_NAME}"
        VALIDATION_OUTPUT_DIR="${VALIDATION_RESULTS_DIR}/${CALIBRATION_ID}"

        print_info "Analyzing: ${CALIBRATION_ID}"

        if [ ! -f "${VALIDATION_OUTPUT_DIR}/emulator_output.csv" ]; then
            print_warning "Skipping ${CAL_NAME}: validation outputs not found"
            continue
        fi

        python3 06_calibration_validation.py \
            --obs "${OBS_FULL}" \
            --emulator_output "${VALIDATION_OUTPUT_DIR}/emulator_output.csv" \
            --default_output "${VALIDATION_OUTPUT_DIR}/default_output.csv" \
            --output_dir "${VALIDATION_OUTPUT_DIR}" \
            --variables ${VARIABLES} \
            2>&1 | tee -a "${LOG_DIR}/03_analysis_${CAL_NAME}.log"

        print_success "Analysis completed for ${CAL_NAME}"
    done

    print_success "All analysis completed!"
else
    print_info "Skipping analysis and plotting"
fi

# =============================================================================
# Summary
# =============================================================================
print_header "CALIBRATION WORKFLOW COMPLETED"
echo ""
print_info "Summary:"
print_info "  Timestamp:          ${TIMESTAMP}"
print_info "  Log directory:      ${LOG_DIR}"
print_info "  Model used:         ${MODEL_DIR}"
print_info "  Calibration runs:   ${NUM_CAL_RESULTS}"
print_info "  Calibration period: ${CAL_START} to ${CAL_END}"
print_info "  Validation period:  ${VAL_START} to ${VAL_END}"
print_info "  Full time range:    ${FULL_START} to ${FULL_END}"
echo ""
print_info "Results:"
print_info "  - Calibration results: ${CALIBRATION_OUTPUT}"
print_info "  - Validation results:  ${VALIDATION_RESULTS_DIR}/$(basename ${CALIBRATION_OUTPUT})/"
echo ""
print_success "Calibration workflow completed!"
