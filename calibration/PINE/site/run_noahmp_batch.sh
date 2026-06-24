#!/bin/bash
####################################################################################
# Batch Noah-MP Run Script
#
# Runs Noah-MP simulations for all parameter sets in parallel.
# Uses 30-minute resolution forcing data.
#
# All site/machine-specific paths come from paths_config.sh -- edit that first.
#
# Usage: bash run_noahmp_batch.sh [OPTIONS]
####################################################################################

set -e

# =============================================================================
# Load site/machine paths
# =============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/paths_config.sh"

POINT_RUN_DIR="${NOAHMP_DIR}"
PARAM_FILE="${PARAM_DIR}/noahmp_param_sets.txt"
BASE_TABLE="${TBL_GENERATOR_DIR}/NoahmpTable.TBL"
OUTPUT_DIR="${BASE_DIR}/data/raw/sim_results"
FORCING_DIR="${FORCING_SOURCE}"

# Default values
N_SAMPLES=1000
N_PARALLEL=4
START_SAMPLE=1
END_SAMPLE=0  # 0 means use N_SAMPLES

# =============================================================================
# Parse Arguments
# =============================================================================
while [[ $# -gt 0 ]]; do
    case $1 in
        --samples)
            N_SAMPLES="$2"
            shift 2
            ;;
        --parallel)
            N_PARALLEL="$2"
            shift 2
            ;;
        --start)
            START_SAMPLE="$2"
            shift 2
            ;;
        --end)
            END_SAMPLE="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --samples N     Number of samples (default: 1000)"
            echo "  --parallel N    Parallel runs (default: 4)"
            echo "  --start N       Start sample (default: 1)"
            echo "  --end N         End sample (default: N_SAMPLES)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

if [ "${END_SAMPLE}" -eq 0 ]; then
    END_SAMPLE=${N_SAMPLES}
fi

echo "======================================================================"
echo "Noah-MP Batch Simulation"
echo "======================================================================"
echo "Running samples ${START_SAMPLE} to ${END_SAMPLE}"
echo "Parallel runs: ${N_PARALLEL}"
echo "Forcing data: ${FORCING_DIR}"
echo "Parameter file: ${PARAM_FILE}"
echo "Base table: ${BASE_TABLE}"
echo "======================================================================"

# =============================================================================
# Check required files
# =============================================================================
if [ ! -f "${PARAM_FILE}" ]; then
    echo "ERROR: Parameter file not found: ${PARAM_FILE}"
    exit 1
fi

if [ ! -f "${BASE_TABLE}" ]; then
    echo "ERROR: Base table not found: ${BASE_TABLE}"
    exit 1
fi

if [ ! -f "${POINT_RUN_DIR}/hrldas.exe" ]; then
    echo "ERROR: hrldas.exe not found: ${POINT_RUN_DIR}/hrldas.exe"
    exit 1
fi

if [ ! -d "${FORCING_DIR}" ]; then
    echo "ERROR: Forcing directory not found: ${FORCING_DIR}"
    exit 1
fi

# Export Noah-MP shared library path
export LD_LIBRARY_PATH="${NOAHMP_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH}"

# =============================================================================
# Create output directory
# =============================================================================
mkdir -p "${OUTPUT_DIR}"

# =============================================================================
# Step 1: Generate all TBL files first
# =============================================================================
echo ""
echo "Step 1: Generating NoahmpTable.TBL files for all samples..."
cd "${TBL_GENERATOR_DIR}"

python3 noahmp_apply_samples.py \
    --samples "${PARAM_FILE}" \
    --base_table "${BASE_TABLE}" \
    --out_root "${OUTPUT_DIR}" \
    --n_rows ${END_SAMPLE}

echo "TBL files generated."

# =============================================================================
# Step 2: Run simulations in parallel
# =============================================================================
echo ""
echo "Step 2: Running Noah-MP simulations..."

run_single_simulation() {
    local sample_idx=$1
    local work_dir="${OUTPUT_DIR}/sample_${sample_idx}"
    local tbl_dir="${OUTPUT_DIR}/NoahmpTable_4emu_${sample_idx}"

    if [ ! -f "${tbl_dir}/NoahmpTable.TBL" ]; then
        echo "Sample ${sample_idx}: SKIP (no TBL file)"
        return 1
    fi

    mkdir -p "${work_dir}/output"

    cp "${POINT_RUN_DIR}/hrldas.exe" "${work_dir}/"
    cp "${POINT_RUN_DIR}/namelist.hrldas" "${work_dir}/"
    cp "${tbl_dir}/NoahmpTable.TBL" "${work_dir}/"

    ln -sf "${FORCING_DIR}" "${work_dir}/forcing"

    cd "${work_dir}"
    sed -i "s|INDIR.*=.*|INDIR = './forcing'|" namelist.hrldas
    sed -i "s|OUTDIR.*=.*|OUTDIR = './output'|" namelist.hrldas

    ./hrldas.exe > run.log 2>&1

    if ls output/*.LDASOUT_DOMAIN1 1> /dev/null 2>&1; then
        echo "Sample ${sample_idx}: SUCCESS"
        return 0
    else
        echo "Sample ${sample_idx}: FAILED"
        return 1
    fi
}

export -f run_single_simulation
export OUTPUT_DIR POINT_RUN_DIR FORCING_DIR

echo "Starting parallel simulations..."
seq ${START_SAMPLE} ${END_SAMPLE} | xargs -P ${N_PARALLEL} -I {} bash -c 'run_single_simulation {}'

echo ""
echo "======================================================================"
echo "Completed all simulations!"

n_success=$(ls -d ${OUTPUT_DIR}/sample_*/output/*.LDASOUT_DOMAIN1 2>/dev/null | wc -l)
n_total=$((END_SAMPLE - START_SAMPLE + 1))
echo "Successful runs: ${n_success} / ${n_total}"
echo "======================================================================"
