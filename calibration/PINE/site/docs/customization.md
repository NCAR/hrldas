# Customizing the framework for your site, variables, and parameters

This guide tells you exactly which files to edit when you want to:
1. point the framework at a new site,
2. calibrate against a different target variable,
3. drop or add a forcing variable,
4. change the calibrated parameter set.

For "I just want to run with the shipped PSO defaults" see the README's Quick
Start instead.

---

## 0. One-time setup before any of the below

Edit **`paths_config.py`** to point at your environment. `paths_config.sh` is
auto-generated from `paths_config.py`, so you only need to edit one file.

You will set: `BASE_DIR`, `SITE_NAME`, `FORCING_SOURCE`, `NOAHMP_EXE`,
`NOAHMP_LD_LIBRARY_PATH`, `HRLDAS_SETUP_FILE`, `OBS_DIR`, `OBS_FILE_30MIN`,
`UTC_OFFSET_HOURS`.

After this, `BASE_DIR`-relative paths in every workflow script Just Work.

---

## 1. Change the site

1. Set `SITE_NAME`, `FORCING_SOURCE`, `HRLDAS_SETUP_FILE`, `UTC_OFFSET_HOURS`
   in both `paths_config.sh` and `paths_config.py`.
2. Prepare your observations:
   ```bash
   python3 src/prepare_observation_data.py \
       --input  raw_obs/MySite_fluxes.csv \
       --datetime_col TIMESTAMP_END \
       --time_format "%Y%m%d%H%M" \
       --columns LH=LE HFX=H SOIL_M=SWC_1_1_1 \
       --utc_offset_hours -5 \
       --site_name MySite \
       --output_dir data/obs
   ```
   This writes `data/obs/MySite_obs_30min.csv` and `..._daily.csv`.
3. In `config_forward_comprehensive.py`, set `TIME_RANGE_FULL`,
   `TIME_RANGE_CALIBRATION`, `TIME_RANGE_VALIDATION` to match your forcing /
   observation time span.
4. In `data/raw/param/`, copy `default_param_template.txt` to
   `default_param.txt` and edit values to match your vegetation/soil class.

That's the complete site-swap checklist.

---

## 2. Change the calibration target variables

Default: `['SOIL_M', 'LH', 'HFX']`.

To, for example, replace `HFX` with `GPP`:

1. **`config_forward_comprehensive.py`**
   - Confirm `GPP` is in `TARGET_VARIABLES`. If not, add a dict:
     ```python
     {'name': 'GPP', 'aggregation': 'mean',
      'description': 'Gross primary production (...)',
      'category': 'water_fluxes'}
     ```
   - Change `MAIN_TARGETS = ['SOIL_M', 'LH', 'GPP']`.
   - Add a sensible weight in `OUTPUT_WEIGHTS`, e.g. `'GPP': 5.0`.
2. **Observation CSV**: ensure the observation file you pass to
   calibration/validation has a `GPP` column with the same units as Noah-MP's.
3. **Re-run preprocessing -> training -> calibration**. The hardcoded
   `--weights nargs=3` is gone; use:
   ```bash
   bash run_calibration.sh \
       --variables "SOIL_M LH GPP" \
       --weights "1 1 5" \
       --weight_vars "SOIL_M LH GPP"
   ```

---

## 3. Add or remove a forcing variable

Forcing variables live in `FORCING_VARIABLES` in
`config_forward_comprehensive.py`. Each entry maps an LDASIN source name to
the Noah-MP internal name plus aggregation rule.

To add a new forcing (e.g. `CO2`):

1. Append a dict to `FORCING_VARIABLES`:
   ```python
   {'name': 'CO2', 'source_name': 'CO2', 'noahmp_name': 'CO2',
    'aggregation': 'mean', 'description': 'CO2 mole fraction (ppm)'}
   ```
2. Confirm your LDASIN files actually contain `CO2`.
3. Re-run `extract_forcing_data.py` (it iterates `FORCING_VARIABLES`).
4. Re-train the emulator (input dimension changes, so any pre-trained model
   becomes incompatible).

To remove a forcing, just drop the dict and re-run from preprocessing.

---

## 4. Change the calibrated parameter set

The calibrated parameters are defined in two files:

- **`noahmp/TBL_generator/var_info_matrix.csv`** -- which columns of
  `NoahmpTable.TBL` each parameter writes into.
- **`value_bounds.csv`** -- lower/upper bound for each parameter.

To add a parameter (e.g. `MFSNO`):

1. Add a row to `var_info_matrix.csv` describing its TBL location.
2. Add bounds to `value_bounds.csv`:
   ```
   MFSNO,1.0,5.0
   ```
3. Re-run `python3 noahmp/TBL_generator/generate_samples.py` to redo LHS.
4. Re-run the full Noah-MP batch and re-train the emulator.

To exclude a parameter from calibration without removing it (e.g. you have a
field-measured LAI), add it to `FIXED_PARAMETERS`:

```python
# in config_forward_comprehensive.py
FIXED_PARAMETERS = {'LAI_EBF': 6.52}
```

That value is then forced into the parameter vector at every calibration
evaluation; only the other parameters are searched.

---

## 5. Things you can leave alone

The following are deliberately NOT exposed for user editing in this release:

- The LHS sampler in `noahmp/TBL_generator/generate_samples.py`
  (it is deterministic via `np.random.seed(42)`; changing it invalidates any
  shipped models).
- The 64/16/20 train/val/test split in `02_train`.
- The `HVT > HVB` physical constraint in `src/constraints.py`.
- The emulator architecture (AttentionLSTM).
- The differential-evolution optimizer.
