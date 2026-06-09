# Source Scripts Directory

This directory contains utility Python scripts used by the workflow.

## Scripts

### 1. convert_calibrated_params.py

Purpose: Convert calibrated parameters from CSV to TBL generator format

Input: calibrated_parameters.csv from calibration results

Output: Space-delimited text file for TBL generator

Usage:
python3 src/convert_calibrated_params.py \
 --input calibration_results/calibration_1/calibrated_parameters.csv \
 --output emulator_params.txt

### 2. parse_noahmp_outputs.py

Purpose: Parse Noah-MP NetCDF outputs to CSV format with daily aggregation

Input: Noah-MP NetCDF output file (\*.LDASOUT_DOMAIN1)

Output: CSV file with daily aggregated data

Usage:
python3 src/parse_noahmp_outputs.py \
 --input run_emulator/output/201507301730.LDASOUT_DOMAIN1 \
 --output emulator_output.csv

Variables Extracted:

- HFX → H (Sensible heat flux, W/m²)
- LH → LE (Latent heat flux, W/m²)
- GPP → GPP (Gross Primary Production)
- ECAN, ETRAN, EDIR (Evaporation components, mm/day)
- ET (Total evapotranspiration, calculated)

## Dependencies

Required Python packages:

- pandas
- numpy
- xarray (for NetCDF parsing)

Install with: pip install pandas numpy xarray

Last Updated: 2025-11-24
