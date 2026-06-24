#!/bin/bash
####################################################################################
# Parameter Importance Analysis for Noah-MP Calibration
#
# This script orchestrates the parameter importance analysis workflow:
# 1. Prepare parameter combinations and run directories
# 2. Run Noah-MP for all combinations
# 3. Convert NetCDF outputs to CSV
# 4. Analyze results and compute importance values
#
# Usage:
#   bash 07_importance_analysis.sh                    # Full workflow (prepare + run + analyze)
#   bash 07_importance_analysis.sh --prepare-only     # Only prepare directories
#   bash 07_importance_analysis.sh --run-only         # Only run models (assumes prepared)
#   bash 07_importance_analysis.sh --analyze-only     # Only analyze (assumes models ran)
#   bash 07_importance_analysis.sh -n 4               # Run with 4 parallel jobs
#
# Arguments:
#   --method METHOD     Analysis method: 'oat' or 'shapley' (default: shapley)
#   --calib-dir DIR     Calibration results directory (default: validation_results/20260114_121456/calibration_1)
#   --period PERIOD     Analysis period: 'calibration', 'validation', or 'full' (default: full)
#   -n JOBS             Number of parallel jobs for model runs (default: 1)
#   --prepare-only      Only prepare parameter combinations
#   --run-only          Only run models
#   --analyze-only      Only analyze results
#
####################################################################################

set -e  # Exit on error

# =============================================================================
# Configuration
# =============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/paths_config.sh"

PYTHON_SCRIPT="${BASE_DIR}/07_importance_analysis.py"

# Export Noah-MP shared library path for hrldas.exe runs
export LD_LIBRARY_PATH="${NOAHMP_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH}"

# Default settings
METHOD="shapley"
CALIB_DIR="validation_results/20260203_112416/calibration_1/"
PERIOD="full"
PARALLEL_JOBS=1
PREPARE_ONLY=false
RUN_ONLY=false
ANALYZE_ONLY=false

# =============================================================================
# Parse Arguments
# =============================================================================

while [[ $# -gt 0 ]]; do
    case $1 in
        --method)
            METHOD="$2"
            shift 2
            ;;
        --calib-dir)
            CALIB_DIR="$2"
            shift 2
            ;;
        --period)
            PERIOD="$2"
            shift 2
            ;;
        -n)
            PARALLEL_JOBS="$2"
            shift 2
            ;;
        --prepare-only)
            PREPARE_ONLY=true
            shift
            ;;
        --run-only)
            RUN_ONLY=true
            shift
            ;;
        --analyze-only)
            ANALYZE_ONLY=true
            shift
            ;;
        -h|--help)
            head -35 "$0" | tail -30
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Resolve paths
if [[ "${CALIB_DIR:0:1}" != "/" ]]; then
    CALIB_DIR_FULL="${BASE_DIR}/${CALIB_DIR}"
else
    CALIB_DIR_FULL="${CALIB_DIR}"
fi

IMPORTANCE_DIR="${CALIB_DIR_FULL}/importance_analysis"

# =============================================================================
# Functions
# =============================================================================

log_section() {
    echo ""
    echo "========================================================================"
    echo "$1"
    echo "========================================================================"
    echo ""
}

run_model() {
    local run_dir=$1
    local combo_name=$(basename "$run_dir")
    
    echo "  Running: $combo_name"
    cd "$run_dir"
    
    # Run Noah-MP
    if ./hrldas.exe > run.log 2>&1; then
        echo "    $combo_name: Model run SUCCESS"
        return 0
    else
        echo "    $combo_name: Model run FAILED (see $run_dir/run.log)"
        return 1
    fi
}

convert_output() {
    local run_dir=$1
    local combo_name=$(basename "$run_dir")
    local output_dir="${run_dir}/output"
    
    # Find LDASOUT NetCDF file
    local nc_file=$(ls "${output_dir}"/*.LDASOUT_DOMAIN1 2>/dev/null | head -1)
    
    if [ -z "$nc_file" ]; then
        echo "    $combo_name: No LDASOUT file found"
        return 1
    fi
    
    local csv_file="${output_dir}/combined_output.csv"
    
    # Convert using parse_noahmp_outputs.py
    echo "    $combo_name: Converting NetCDF to CSV..."
    python3 "${SRC_DIR}/parse_noahmp_outputs.py" \
        --input "$nc_file" \
        --output "$csv_file" \
        --no-timezone > /dev/null 2>&1
    
    if [ -f "$csv_file" ]; then
        echo "    $combo_name: Conversion SUCCESS"
        return 0
    else
        echo "    $combo_name: Conversion FAILED"
        return 1
    fi
}

export -f run_model
export -f convert_output
export SRC_DIR

# =============================================================================
# Main Workflow
# =============================================================================

cd "$BASE_DIR"

echo "Parameter Importance Analysis"
echo "Method: ${METHOD}"
echo "Calibration directory: ${CALIB_DIR_FULL}"
echo "Period: ${PERIOD}"
echo "Parallel jobs: ${PARALLEL_JOBS}"
echo ""

# -----------------------------------------------------------------------------
# Step 1: Prepare parameter combinations
# -----------------------------------------------------------------------------

if [ "$RUN_ONLY" = false ] && [ "$ANALYZE_ONLY" = false ]; then
    log_section "STEP 1: Preparing parameter combinations"
    
    python3 "$PYTHON_SCRIPT" \
        --mode prepare \
        --method "$METHOD" \
        --calib-dir "$CALIB_DIR"
    
    if [ "$PREPARE_ONLY" = true ]; then
        echo ""
        echo "Preparation complete. Run directories created in:"
        echo "  ${IMPORTANCE_DIR}"
        echo ""
        echo "Next steps:"
        echo "  1. Run models: bash 07_importance_analysis.sh --run-only"
        echo "  2. Analyze:    bash 07_importance_analysis.sh --analyze-only"
        exit 0
    fi
fi

# -----------------------------------------------------------------------------
# Step 2: Run Noah-MP for all combinations
# -----------------------------------------------------------------------------

if [ "$PREPARE_ONLY" = false ] && [ "$ANALYZE_ONLY" = false ]; then
    log_section "STEP 2: Running Noah-MP for all combinations"
    
    # Get list of run directories
    if [ ! -d "$IMPORTANCE_DIR" ]; then
        echo "Error: Importance analysis directory not found: $IMPORTANCE_DIR"
        echo "Please run with --prepare-only first."
        exit 1
    fi
    
    RUN_DIRS=($(ls -d "${IMPORTANCE_DIR}"/run_* 2>/dev/null))
    TOTAL_RUNS=${#RUN_DIRS[@]}
    
    if [ $TOTAL_RUNS -eq 0 ]; then
        echo "Error: No run directories found in $IMPORTANCE_DIR"
        exit 1
    fi
    
    echo "Total combinations to run: $TOTAL_RUNS"
    echo "Parallel jobs: $PARALLEL_JOBS"
    echo ""
    
    # Run models
    COMPLETED=0
    FAILED=0
    
    if [ $PARALLEL_JOBS -gt 1 ]; then
        # Parallel execution using xargs
        printf '%s\n' "${RUN_DIRS[@]}" | xargs -P $PARALLEL_JOBS -I {} bash -c 'run_model "$@"' _ {}
    else
        # Sequential execution with progress
        for run_dir in "${RUN_DIRS[@]}"; do
            COMPLETED=$((COMPLETED + 1))
            echo "[$COMPLETED/$TOTAL_RUNS]"
            run_model "$run_dir" || FAILED=$((FAILED + 1))
        done
    fi
    
    echo ""
    echo "Model runs completed."
    
    # Convert outputs
    log_section "STEP 2b: Converting outputs to CSV"
    
    for run_dir in "${RUN_DIRS[@]}"; do
        convert_output "$run_dir" || true
    done
    
    echo ""
    echo "Output conversion completed."
    
    if [ "$RUN_ONLY" = true ]; then
        echo ""
        echo "Model runs and conversions complete."
        echo ""
        echo "Next step:"
        echo "  Analyze results: bash 07_importance_analysis.sh --analyze-only"
        exit 0
    fi
fi

# -----------------------------------------------------------------------------
# Step 3: Analyze results
# -----------------------------------------------------------------------------

if [ "$PREPARE_ONLY" = false ] && [ "$RUN_ONLY" = false ]; then
    log_section "STEP 3: Analyzing results and computing importance"
    
    python3 "$PYTHON_SCRIPT" \
        --mode analyze \
        --method "$METHOD" \
        --calib-dir "$CALIB_DIR" \
        --period "$PERIOD"
    
    echo ""
    log_section "ANALYSIS COMPLETE"
    
    echo "Results saved to:"
    echo "  ${IMPORTANCE_DIR}/importance_results_${METHOD}_${PERIOD}.json"
    echo "  ${IMPORTANCE_DIR}/importance_*.csv"
    echo ""
    echo "To analyze different periods, run:"
    echo "  python3 07_importance_analysis.py --mode analyze --method $METHOD --period calibration"
    echo "  python3 07_importance_analysis.py --mode analyze --method $METHOD --period validation"
fi
