# PINE (Physically Interpretable Noah-MP Emulator) for parameter-calibration

An LSTM-emulator-based rapid parameter-calibration system for the Noah-MP land
surface model.

The code needed to reproduce the experiments is available in this repository and
at [https://doi.org/10.5194/gmd-19-2197-2026](https://doi.org/10.5194/gmd-19-2197-2026).

## Citation

If you use this code in your research, please cite:

> Cheng, Y., Wang, Y., Furtado, K., He, C., Chen, F., Ziegler, A. D., Chen, S.,
> Detto, M., Mao, Y., Pan, B., Kosugi, Y., Lion, M., Noguchi, S., Takanashi, S.,
> Melling, L., and Zhang, B.: Assessing and enhancing Noah-MP land surface
> modeling over tropical forests using machine learning techniques, *Geosci. Model
> Dev.*, 19, 2197–2217, [https://doi.org/10.5194/gmd-19-2197-2026](https://doi.org/10.5194/gmd-19-2197-2026), 2026.

## Workflow Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                     run_model_training.sh                           │
│  1. Generate parameter samples (LHS)                                │
│  2. Extract forcing data (full/calibration/validation periods)      │
│  3. Run Noah-MP (1000 parameter sets × full period)                 │
│  4. Preprocess data (calibration period)                            │
│  5. Train LSTM emulator                                             │
└─────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────┐
│                       run_calibration.sh                            │
│  1. Calibrate parameters using emulator (calibration period)        │
│  2. Run Noah-MP validation with calibrated parameters (full period) │
│  3. Analyze and plot results (calibration/validation periods)       │
└─────────────────────────────────────────────────────────────────────┘
```

## Quick Start

**Before your first run, EDIT `paths_config.py` to point at your site's data.**
(`paths_config.sh` is auto-generated from `paths_config.py` — you only need to
edit the Python file.) The shipped values reproduce the Pasoh (PSO, Malaysia)
example from the paper; they are placeholders for any other machine.

You will also need:

- A compiled `hrldas.exe` binary at `noahmp/point_run/hrldas.exe` built for
  your environment (the shipped binary is unlikely to run on your machine).
- Your site's 30-min LDASIN forcing files in `FORCING_SOURCE`.
- An observation CSV converted via `src/prepare_observation_data.py` (see
  below).
- Python dependencies: `pip install -r requirements.txt`, or
  `conda env create -f environment.yml`.

Then:

```bash
cd $PROJECT_DIR

# (one time) prepare your observation file
python3 src/prepare_observation_data.py \
    --input  raw_obs/MySite_fluxes.csv \
    --datetime_col TIMESTAMP_END \
    --columns LH=LE HFX=H SOIL_M=SWC \
    --utc_offset_hours -5 \
    --site_name MySite --output_dir data/obs

# (one time) copy the default-parameter template and edit
cp data/raw/param/default_param_template.txt data/raw/param/default_param.txt

# Full model training workflow
bash run_model_training.sh --samples 1000 --parallel 4

# Full calibration workflow
bash run_calibration.sh --num_calibration 10 --max_iter 100
```

## Time Range Configuration

Defined in `config_forward_comprehensive.py`. The shipped values are the
PSO example:

| Period      | Range                   | Purpose                                  |
| ----------- | ----------------------- | ---------------------------------------- |
| FULL        | 2005-01-01 ~ 2009-12-31 | Noah-MP runs, validation                 |
| CALIBRATION | 2005-01-01 ~ 2006-12-31 | Emulator training, parameter calibration |
| VALIDATION  | 2007-01-01 ~ 2009-12-30 | Independent validation                   |

Edit these to match your site's forcing/observation time span.

## Customizing target variables, forcing variables, parameters, and site

See **`docs/customization.md`** for the step-by-step checklist.
At a glance:

- Calibration targets live in `MAIN_TARGETS` in `config_forward_comprehensive.py`
  (default: `['SOIL_M', 'LH', 'HFX']`).
- Per-variable loss weights are passed via `--weights`/`--weight_vars` to
  `run_calibration.sh` and 05_*.py (paired 1:1).
- Forcing variables live in `FORCING_VARIABLES` in the same config file.
- Calibrated parameters: edit `value_bounds.csv` and
  `noahmp/TBL_generator/var_info_matrix.csv`, then re-run from sample
  generation.

## Detailed Steps

### Model Training (`run_model_training.sh`)

```bash
# Full run
bash run_model_training.sh

# Skip completed steps
bash run_model_training.sh --skip-param-gen --skip-forcing
bash run_model_training.sh --skip-noahmp
bash run_model_training.sh --skip-preprocess --skip-train

# Optional parameters
--samples N      # Number of parameter samples (default: 1000)
--parallel N     # Number of parallel jobs (default: 4)
```

**Understanding sample counts:** There are two related sample-count settings:

1. **`--samples N`** (passed to `run_model_training.sh`): How many Noah-MP
   simulations to run with different parameter sets. This controls LHS
   generation, TBL file creation, and the batch Noah-MP runs. The same value is
   forwarded to preprocessing so it knows how many sample directories to scan.
2. **`MAX_SAMPLES`** (in `config_forward_comprehensive.py`): Upper limit on how
   many samples the preprocessing/training scripts will load. This should be
   `>=` the number you passed to `--samples`. The default (1000) works for most
   cases; reduce it only if you want to train on a subset of available runs.

**Output files** (paths derived from `paths_config.py`):

- `data/raw/param/noahmp_param_sets.txt` — parameter samples
- `data/raw/forcing/forcing_${SITE_NAME}_*.nc` — forcing data
- `data/raw/sim_results/sample_*/` — Noah-MP outputs
- `data/processed_data_forward_comprehensive.pkl` — training data
- `results_forward_comprehensive/*/` — trained model

### Parameter Calibration (`run_calibration.sh`)

```bash
bash run_calibration.sh
bash run_calibration.sh --model_dir results_forward_comprehensive/AttentionLSTM_xxx/
bash run_calibration.sh --num_calibration 20 --max_iter 200

# Custom per-variable weights
bash run_calibration.sh --weights "1 1.5 2.5" --weight_vars "LH HFX SOIL_M"

# Skip steps
bash run_calibration.sh --skip-calibration   # use existing calibration results
bash run_calibration.sh --skip-noahmp        # skip validation Noah-MP runs
```

**Output files:**

- `calibration_results/*/calibration_*/` — calibration results (one per run)
  - `calibrated_parameters.csv` — calibrated parameters
  - `calibration_result.json` — detailed metrics
- `validation_results/*/` — validation results
  - `validation_metrics_by_period.csv` — metrics by period
  - `timeseries_*.png/pdf` — time-series comparison
  - `scatter_*.png/pdf` — scatter plots

## SATDK Parameter Handling

The SATDK parameter uses different forms at different stages:

| Stage                         | Value Form  | Description                               |
| ----------------------------- | ----------- | ----------------------------------------- |
| Parameter sample generation   | Raw value   | `[8.9e-06, 5e-04]`                        |
| TBL file generation           | Raw value   | Written directly to Noah-MP configuration |
| Emulator training             | log10 value | Automatically converted → `[-5.05, -3.3]` |
| Parameter calibration         | log10 value | Uses `SATDK(log)` bounds                  |
| Calibration result conversion | log → raw   | `10^x` conversion back to physical value  |

**value_bounds.csv format:**

```csv
variable,Lower bound,Upper bound
SATDK,8.90E-06,5.00E-04
SATDK(log),-5.05,-3.3
```

## Variable Description

### Forcing Variables (8)

| Variable | Description                         |
| -------- | ----------------------------------- |
| T2D      | 2m air temperature (K)              |
| Q2D      | 2m specific humidity (kg/kg)        |
| PSFC     | Surface pressure (Pa)               |
| U2D, V2D | 2m wind speed components (m/s)      |
| LWDOWN   | Downward longwave radiation (W/m²)  |
| SWDOWN   | Downward shortwave radiation (W/m²) |
| RAINRATE | Precipitation rate (mm/s)           |

### Target Variables (29)

- **Energy balance** (5): FSA, FIRA, HFX, LH, GRDFLX
- **Water fluxes** (5): ECAN, ETRAN, EDIR, UGDRNOFF_RATE, SFCRNOFF_RATE
- **Water storage** (5): SOIL_M (L1–L4), CANLIQ
- **Temperature** (5): SOIL_T (L1–L2), TG, TV, TRAD
- **Energy components** (9): SAV, SAG, IRC, IRG, SHC, SHG, EVC, EVG, GHV

### Calibration Parameters (9)

| Parameter | Description                      |
| --------- | -------------------------------- |
| VCMX25    | Maximum carboxylation rate       |
| HVT       | Canopy top height                |
| HVB       | Canopy bottom height             |
| CWPVT    | Canopy wind parameter            |
| Z0MVT    | Momentum roughness length        |
| WLTSMC    | Wilting point soil moisture      |
| REFSMC    | Reference soil moisture          |
| MAXSMC    | Saturated soil moisture          |
| SATDK     | Saturated hydraulic conductivity |

## File Structure

```
project/
├── paths_config.py                       # SITE/MACHINE PATHS - edit this
├── paths_config.sh                       # Auto-generated from paths_config.py
├── paths_config.example.{sh,py}          # Reference (PSO) example
├── run_model_training.sh                 # Model training workflow
├── run_calibration.sh                    # Calibration workflow
├── run_noahmp_batch.sh                   # Parallel Noah-MP driver
├── config_forward_comprehensive.py       # Targets / forcings / training cfg
│
├── 01_data_preprocessing_forward_comprehensive.py
├── 02_train_forward_comprehensive.py
├── 03_emulator_validation.py
├── 04_inference.py
├── 05_calibration_applying_emulator_multiple_runs.py
├── 06_calibration_validation.py / .sh
├── 07_importance_analysis.py / .sh
│
├── docs/
│   └── customization.md                  # Per-site change checklist
├── examples/
│   └── convert_pso_obs.py                # Site-specific obs converter (ref)
├── src/
│   ├── prepare_observation_data.py       # Generic obs converter (use this)
│   ├── parse_noahmp_outputs.py
│   ├── convert_calibrated_params.py
│   ├── constraints.py
│   ├── plot_validation_results.py
│   └── plot_validation_scatter.py
│
└── data/
    ├── raw/forcing/                      # forcing data
    ├── raw/sim_results/                  # Noah-MP outputs
    ├── raw/param/                        # parameter samples + defaults
    │   └── default_param_template.txt    # copy → default_param.txt
    ├── obs/                              # observation data
    ├── calibration_results/              # calibration results
    └── validation_results/               # validation results
```

## FAQ

**Q: I get `cd: No such file or directory` or `FileNotFoundError` on first run.**

You almost certainly haven't edited `paths_config.py` yet. Open it and set
`BASE_DIR`, `SITE_NAME`, `FORCING_SOURCE`, `NOAHMP_LD_LIBRARY_PATH`, and
`HRLDAS_SETUP_FILE` for your machine.

**Q: Timestep mismatch during preprocessing**

Make sure the forcing time span covers the Noah-MP output time span:

```bash
python3 extract_forcing_data.py --daily \
    --start_date 2015-07-30 --end_date 2017-07-30 \
    --output data/raw/forcing/forcing_${SITE_NAME}_daily_full.nc
```

**Q: Out of memory during training**

Reduce `batch_size` in `TRAINING_CONFIG`.

## BibTeX Citation

If you use this code in your research, please cite:

```bibtex
@Article{gmd-19-2197-2026,
AUTHOR = {Cheng, Y. and Wang, Y. and Furtado, K. and He, C. and Chen, F. and Ziegler, A. D. and Chen, S. and Detto, M. and Mao, Y. and Pan, B. and Kosugi, Y. and Lion, M. and Noguchi, S. and Takanashi, S. and Melling, L. and Zhang, B.},
TITLE = {Assessing and enhancing Noah-MP land surface modeling over tropical forests using machine learning techniques},
JOURNAL = {Geoscientific Model Development},
VOLUME = {19},
YEAR = {2026},
NUMBER = {5},
PAGES = {2197--2217},
URL = {https://gmd.copernicus.org/articles/19/2197/2026/},
DOI = {10.5194/gmd-19-2197-2026}
}
```
