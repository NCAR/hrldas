#!/bin/bash
# Minimal module loads for HRLDAS + Noah-MP on Derecho/Casper
# Using GCC + ncarcompilers; MPI not loaded by default.
set -euo pipefail

module --force purge
module load ncarenv/24.12
module load gcc/12.4.0
module load ncarcompilers/1.0.0
module load netcdf/4.9.3

# Optional MPI if needed for parallel builds/runs
# module load cray-mpich
# or another MPI as appropriate for your build

# Print for provenance
module list 2>&1 | cat
