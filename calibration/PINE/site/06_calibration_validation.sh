#!/bin/bash

####################################################################################
# Calibration Validation Script
#
# Purpose: Validate emulator-based calibration by comparing Noah-MP model results
#          with two parameter sets against real observations:
#          1. Emulator-calibrated parameters
#          2. Default parameters
#
# All site/machine-specific paths come from paths_config.sh -- edit that first.
#
# Usage: bash 06_calibration_validation.sh CALIBRATION_ID [OPTIONS]
####################################################################################

set -e  # Exit on error

# =============================================================================
# Load site/machine paths
# =============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/paths_config.sh"

CALIBRATION_RESULTS_DIR="${BASE_DIR}/calibration_results"
VALIDATION_RESULTS_DIR="${BASE_DIR}/validation_results"

# Default observation file (from paths_config.sh)
OBS_FILE="${OBS_FILE_30MIN}"

# Variables to validate
VARIABLES="SOIL_M LH HFX"

####################################################################################
usage() {
    cat << EOF
Usage: $0 CALIBRATION_ID [OPTIONS]

Validate calibration results by running Noah-MP with different parameter sets
and comparing against observations.

Arguments:
    CALIBRATION_ID      ID of calibration result to validate (e.g., calibration_1)

Options:
    --obs_file FILE     Path to observation CSV file
                        (default: \${OBS_FILE_30MIN} from paths_config.sh)
    --variables VARS    Space-separated list of variables to validate
                        (default: "SOIL_M LH HFX")
    --skip_default      Skip running Noah-MP with default parameters
    --skip_noahmp       Skip all Noah-MP runs (use existing outputs)
    -h, --help          Show this help message

Examples:
    bash 06_calibration_validation.sh calibration_1
    bash 06_calibration_validation.sh calibration_1 --obs_file data/obs/custom_obs.csv
    bash 06_calibration_validation.sh calibration_1 --variables "LH HFX"
    bash 06_calibration_validation.sh calibration_1 --skip_default
EOF
    exit 1
}

print_info()    { echo -e "\033[1;34m[INFO]\033[0m $1"; }
print_success() { echo -e "\033[1;32m[SUCCESS]\033[0m $1"; }
print_error()   { echo -e "\033[1;31m[ERROR]\033[0m $1"; }
print_warning() { echo -e "\033[1;33m[WARNING]\033[0m $1"; }

####################################################################################
convert_calibrated_params() {
    local calibration_dir="$1"
    local output_file="$2"

    echo "[INFO] Converting calibrated parameters to TBL format..."

    python3 "${SRC_DIR}/convert_calibrated_params.py" \
        --input "${calibration_dir}/calibrated_parameters.csv" \
        --output "${output_file}"

    if [ $? -eq 0 ]; then
        echo "[SUCCESS] Parameters converted successfully: ${output_file}"
    else
        echo "[ERROR] Failed to convert parameters"
        exit 1
    fi
}

####################################################################################
generate_tbl_file() {
    local param_file="$1"
    local output_name="$2"
    local output_dir="$3"

    echo "[INFO] Generating TBL file for ${output_name}..."

    cd "${TBL_GENERATOR_DIR}"

    python3 noahmp_apply_samples.py \
        --samples "${param_file}" \
        --base_table NoahmpTable.TBL \
        --out_root "${output_dir}/${output_name}" \
        --n_rows 1

    if [ $? -eq 0 ]; then
        echo "[SUCCESS] TBL file generated: ${output_dir}/${output_name}/NoahmpTable_4emu_1/NoahmpTable.TBL"
        cd "${BASE_DIR}"
        return 0
    else
        echo "[ERROR] Failed to generate TBL file for ${output_name}"
        cd "${BASE_DIR}"
        return 1
    fi
}

####################################################################################
run_noahmp() {
    local tbl_file="$1"
    local output_name="$2"
    local run_dir="$3"

    echo "[INFO] Running Noah-MP with ${output_name} parameters..." >&2

    mkdir -p "${run_dir}"
    cd "${run_dir}"

    cp "${NOAHMP_DIR}/hrldas.exe" .
    cp "${NOAHMP_DIR}/namelist.hrldas" .
    cp "${tbl_file}" NoahmpTable.TBL

    # Symlink forcing
    ln -sf "${FORCING_SOURCE}" "${run_dir}/forcing"

    sed -i "s|INDIR.*=.*|INDIR = './forcing'|g" namelist.hrldas
    sed -i "s|OUTDIR = .*|OUTDIR = \"./output/\"|g" namelist.hrldas

    # Library path for NetCDF (from paths_config.sh)
    export LD_LIBRARY_PATH="${NOAHMP_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH}"

    mkdir -p output
    ./hrldas.exe > noahmp.log 2>&1

    if [ $? -eq 0 ]; then
        output_file=$(ls output/*.LDASOUT_DOMAIN1 2>/dev/null | head -1)

        if [ -n "$output_file" ]; then
            echo "[SUCCESS] Noah-MP run completed: ${output_file}" >&2
            abs_output_file="${run_dir}/${output_file}"
            cd "${BASE_DIR}"
            echo "${abs_output_file}"
            return 0
        else
            echo "[ERROR] Noah-MP output file not found" >&2
            cd "${BASE_DIR}"
            return 1
        fi
    else
        echo "[ERROR] Noah-MP run failed. Check log: ${run_dir}/noahmp.log" >&2
        cd "${BASE_DIR}"
        return 1
    fi
}

####################################################################################
parse_noahmp_output() {
    local output_file="$1"
    local csv_file="$2"

    echo "[INFO] Parsing Noah-MP output to CSV..."

    python3 "${SRC_DIR}/parse_noahmp_outputs.py" \
        --input "${output_file}" \
        --output "${csv_file}" \
        --utc_offset_hours "${UTC_OFFSET_HOURS}"

    if [ $? -eq 0 ]; then
        echo "[SUCCESS] Output parsed successfully: ${csv_file}"
    else
        echo "[ERROR] Failed to parse output"
        exit 1
    fi
}

####################################################################################
# MAIN
####################################################################################
if [ $# -lt 1 ]; then
    usage
fi

CALIBRATION_ID="$1"
shift

SKIP_DEFAULT=false
SKIP_NOAHMP=false

while [ $# -gt 0 ]; do
    case "$1" in
        --obs_file)
            OBS_FILE="$2"
            shift 2
            ;;
        --variables)
            VARIABLES="$2"
            shift 2
            ;;
        --skip_default)
            SKIP_DEFAULT=true
            shift
            ;;
        --skip_noahmp)
            SKIP_NOAHMP=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "[ERROR] Unknown option: $1"
            usage
            ;;
    esac
done

echo ""
echo "####################################################################################"
echo "#                     CALIBRATION VALIDATION WORKFLOW                            #"
echo "####################################################################################"
echo ""
echo "[INFO] Configuration:"
echo "  Calibration ID:     ${CALIBRATION_ID}"
echo "  Observation file:   ${OBS_FILE}"
echo "  Variables:          ${VARIABLES}"
echo "  Skip default:       ${SKIP_DEFAULT}"
echo "  Skip Noah-MP runs:  ${SKIP_NOAHMP}"
echo ""

# Check inputs
CALIBRATION_DIR="${CALIBRATION_RESULTS_DIR}/${CALIBRATION_ID}"
if [ ! -d "${CALIBRATION_DIR}" ]; then
    echo "[ERROR] Calibration directory not found: ${CALIBRATION_DIR}"
    exit 1
fi

if [ ! -f "${CALIBRATION_DIR}/calibrated_parameters.csv" ]; then
    echo "[ERROR] Calibrated parameters file not found: ${CALIBRATION_DIR}/calibrated_parameters.csv"
    exit 1
fi

if [ ! -f "${OBS_FILE}" ]; then
    echo "[ERROR] Observation file not found: ${OBS_FILE}"
    exit 1
fi

VALIDATION_OUTPUT_DIR="${VALIDATION_RESULTS_DIR}/${CALIBRATION_ID}"
mkdir -p "${VALIDATION_OUTPUT_DIR}"

echo "[SUCCESS] Validation output directory: ${VALIDATION_OUTPUT_DIR}"
echo ""

# Step 1: Convert calibrated parameters
echo "[INFO] ========== STEP 1: Convert Calibrated Parameters =========="
EMULATOR_PARAM_FILE="${VALIDATION_OUTPUT_DIR}/emulator_calibrated_params.txt"
convert_calibrated_params "${CALIBRATION_DIR}" "${EMULATOR_PARAM_FILE}"
echo ""

if [ "${SKIP_NOAHMP}" = false ]; then
    # Step 2: Generate TBL files
    echo "[INFO] ========== STEP 2: Generate TBL Files =========="

    generate_tbl_file "${EMULATOR_PARAM_FILE}" "emulator_tbl" "${VALIDATION_OUTPUT_DIR}"
    EMULATOR_TBL="${VALIDATION_OUTPUT_DIR}/emulator_tbl/NoahmpTable_4emu_1/NoahmpTable.TBL"

    if [ "${SKIP_DEFAULT}" = false ]; then
        generate_tbl_file "${PARAM_DIR}/default_param.txt" "default_tbl" "${VALIDATION_OUTPUT_DIR}"
        DEFAULT_TBL="${VALIDATION_OUTPUT_DIR}/default_tbl/NoahmpTable_4emu_1/NoahmpTable.TBL"
    fi

    echo ""

    # Step 3: Run Noah-MP
    echo "[INFO] ========== STEP 3: Run Noah-MP Simulations =========="

    EMULATOR_RUN_DIR="${VALIDATION_OUTPUT_DIR}/run_emulator"
    EMULATOR_OUTPUT=$(run_noahmp "${EMULATOR_TBL}" "emulator" "${EMULATOR_RUN_DIR}")

    if [ "${SKIP_DEFAULT}" = false ]; then
        DEFAULT_RUN_DIR="${VALIDATION_OUTPUT_DIR}/run_default"
        DEFAULT_OUTPUT=$(run_noahmp "${DEFAULT_TBL}" "default" "${DEFAULT_RUN_DIR}")
    fi

    echo ""

    # Step 4: Parse outputs
    echo "[INFO] ========== STEP 4: Parse Noah-MP Outputs =========="

    EMULATOR_CSV="${VALIDATION_OUTPUT_DIR}/emulator_output.csv"
    parse_noahmp_output "${EMULATOR_OUTPUT}" "${EMULATOR_CSV}"

    if [ "${SKIP_DEFAULT}" = false ]; then
        DEFAULT_CSV="${VALIDATION_OUTPUT_DIR}/default_output.csv"
        parse_noahmp_output "${DEFAULT_OUTPUT}" "${DEFAULT_CSV}"
    else
        DEFAULT_CSV=""
    fi

    echo ""
else
    echo "[WARNING] Skipping Noah-MP runs. Using existing outputs..."
    EMULATOR_CSV="${VALIDATION_OUTPUT_DIR}/emulator_output.csv"
    DEFAULT_CSV="${VALIDATION_OUTPUT_DIR}/default_output.csv"
fi

# Step 5: Validation analysis
echo "[INFO] ========== STEP 5: Run Validation Analysis =========="

VALIDATION_ARGS=(
    --obs "${OBS_FILE}"
    --emulator_output "${EMULATOR_CSV}"
    --output_dir "${VALIDATION_OUTPUT_DIR}"
    --variables ${VARIABLES}
)

if [ -n "${DEFAULT_CSV}" ] && [ -f "${DEFAULT_CSV}" ]; then
    VALIDATION_ARGS+=(--default_output "${DEFAULT_CSV}")
fi

python3 "${BASE_DIR}/06_calibration_validation.py" "${VALIDATION_ARGS[@]}"

if [ $? -eq 0 ]; then
    echo ""
    echo "[SUCCESS] ====================================================="
    echo "[SUCCESS]   CALIBRATION VALIDATION COMPLETED SUCCESSFULLY!"
    echo "[SUCCESS] ====================================================="
    echo "[SUCCESS] Results saved to: ${VALIDATION_OUTPUT_DIR}"
    echo ""
else
    echo "[ERROR] Validation analysis failed"
    exit 1
fi
