# NoahMP Parameter Table Generator

This project generates multiple NoahMP parameter table files by applying parameter samples to a base table file. It's designed to facilitate ensemble simulations or parameter sensitivity analysis with the NoahMP land surface model.

## Overview

The tool takes parameter samples from `noahmp_1000samples.txt` and applies them to a base `NoahmpTable.TBL` file to generate multiple customized parameter tables. Each row in the samples file produces one modified table file.

## Files

- **`noahmp_apply_samples.py`** - Main Python script for command-line execution
- **`noahmp_apply_samples.ipynb`** - Jupyter notebook version for Windows/interactive use
- **`var_info_matrix.csv`** - Mapping between sample column names and NoahMP parameter locations. You may add your new ones.
- **`value_bounds.csv`** - You may edit the value bounds to introduce constraints at your discretion. **Note that it should synchronize with the 'value_bounds.csv' in base directory.**

## Parameter Mapping

The samples file contains 28 parameters organized into three sections:

### 1. Radiation Parameters (`&noahmp_rad_parameters`)

- Variables: `ALBSAT_VIS`, `ALBSAT_NIR`, `ALBDRY_VIS`, `ALBDRY_NIR`
- Fixed type: 4 (soil color = 4)

### 2. USGS Land Use Parameters (`&noahmp_usgs_parameters`)

- Suffix mapping:
  - `_EBF` → Type 13 (Evergreen Broadleaf Forest)
  - `_CP` → Type 2 (Dryland Cropland and Pasture)
  - `_SAV` → Type 10 (Savanna)
- Variables: `VCMX25`, `HVT`, `CWPVT`, `Z0MVT`

### 3. Soil Statistical Parameters (`&noahmp_soil_stas_parameters`)

- Suffix mapping:
  - `_CL` → Type 9 (Clay Loam)
  - `_loam` → Type 6 (Loam)
  - `_SCL` → Type 7 (Sandy Clay Loam)
- Variables: `WLTSMC`, `REFSMC`, `MAXSMC`, `SATDK`

## Usage

### Command Line (Python script)

```bash
python noahmp_apply_samples.py --samples noahmp_calibrated_param.txt --base_table NoahmpTable.TBL --out_root ./TropicalSiteCalibrated/ --n_rows 5
```

### Parameters

- `--samples`: Path to space-delimited samples file
- `--base_table`: Path to base NoahmpTable.TBL file
- `--out_root`: Output directory (default: current directory)
- `--n_rows`: Number of sample rows to process (default: 10)

### Jupyter Notebook

Run `noahmp_apply_samples.ipynb` with hardcoded parameters for interactive execution on Windows.

## Output

The tool creates directories `NoahmpTable_4emu_1/`, `NoahmpTable_4emu_2/`, etc., each containing a modified `NoahmpTable.TBL` file with parameters replaced according to the corresponding sample row.

## Technical Details

The script:

1. Parses sample column names to identify parameter sections, variable names, and type indices
2. Locates parameter assignment lines in the base table using regex
3. Replaces comma-separated values at specific type positions
4. Preserves original formatting and inline comments
5. Generates one output file per sample row
