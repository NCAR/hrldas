#!/usr/bin/env python3
"""
prepare_observation_data.py
---------------------------

Generic observation-data preparer for the Noah-MP emulator-based calibration
framework.

Input:  any CSV that has a parseable datetime column and one or more
        observation columns (LE, H, SWC, ...).
Output:
  - <output_dir>/<site_name>_obs_30min.csv     (original-cadence data)
  - <output_dir>/<site_name>_obs_daily.csv      (daily aggregate, full range)
  - <output_dir>/<site_name>_obs_<CAL_START>_<CAL_END>.csv   (daily, calibration period)
  - <output_dir>/<site_name>_obs_<VAL_START>_<VAL_END>.csv   (daily, validation period)

All files use ``date`` as the datetime column name for consistency with
downstream scripts.

For a site-specific example see examples/convert_pso_obs.py (Pasoh, Malaysia).
"""

import argparse
import sys
from pathlib import Path
from datetime import timedelta

import pandas as pd


def _parse_columns_arg(items):
    """
    Parse `--columns LH=LE HFX=H SOIL_M=SWC` (list of K=V strings) into a dict
    {csv_column_name -> framework_target_name}.
    """
    mapping = {}
    for item in items:
        if '=' not in item:
            raise ValueError(
                f"--columns entries must be FRAMEWORK_NAME=CSV_COLUMN, got: {item!r}"
            )
        framework_name, csv_col = item.split('=', 1)
        mapping[csv_col.strip()] = framework_name.strip()
    return mapping


def prepare_observations(
    input_csv,
    datetime_col,
    rename_map,
    output_dir,
    site_name,
    time_format=None,
    utc_offset_hours=0.0,
):
    """
    Read an arbitrary observation CSV, normalize column names, convert
    timestamps to UTC, and write the two output CSV files the pipeline
    expects.

    Parameters
    ----------
    input_csv : str | Path
        Path to the input CSV.
    datetime_col : str
        Column name in `input_csv` holding the timestamp.
    rename_map : dict[str, str]
        {csv_column -> framework_target_name}.
    output_dir : str | Path
        Directory to write the two output CSVs to.
    site_name : str
        Site identifier; goes into the output filename.
    time_format : str | None
        Optional strptime format. If None, pandas.to_datetime infers.
    utc_offset_hours : float
        The input column's timezone offset from UTC, in hours. Pipeline
        downstream stores everything in UTC, so the script SUBTRACTS this
        offset to undo the local-time convention.
    """
    input_csv = Path(input_csv)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"[INFO] Reading {input_csv}")
    df = pd.read_csv(input_csv)

    if datetime_col not in df.columns:
        raise KeyError(
            f"--datetime_col {datetime_col!r} not found. "
            f"Available columns: {list(df.columns)}"
        )

    # Parse timestamps
    if time_format is not None:
        df['timestamp'] = pd.to_datetime(df[datetime_col], format=time_format)
    else:
        df['timestamp'] = pd.to_datetime(df[datetime_col])

    # Convert to UTC by subtracting the local offset
    if utc_offset_hours != 0:
        print(f"[INFO] Converting timestamps: local (UTC{utc_offset_hours:+}) -> UTC")
        df['timestamp'] = df['timestamp'] - timedelta(hours=utc_offset_hours)

    # Apply column renaming. Missing rename-sources are an error.
    missing = [c for c in rename_map if c not in df.columns]
    if missing:
        raise KeyError(
            f"--columns references columns not in the CSV: {missing}. "
            f"Available: {list(df.columns)}"
        )
    df = df.rename(columns=rename_map)

    keep_cols = ['timestamp'] + list(rename_map.values())
    df = df[keep_cols].sort_values('timestamp').reset_index(drop=True)

    # Rename 'timestamp' -> 'date' for consistency with downstream scripts
    df = df.rename(columns={'timestamp': 'date'})

    # 30-min file (intended cadence; we just write whatever the input cadence is)
    out_30min = output_dir / f"{site_name}_obs_30min.csv"
    df.to_csv(out_30min, index=False)
    print(f"[SUCCESS] Wrote {out_30min}  ({len(df)} rows)")

    # Daily aggregate (mean for fluxes / soil moisture is the convention)
    daily = (
        df.set_index('date')
          .resample('D')
          .mean(numeric_only=True)
          .reset_index()
    )
    out_daily = output_dir / f"{site_name}_obs_daily.csv"
    daily.to_csv(out_daily, index=False)
    print(f"[SUCCESS] Wrote {out_daily}  ({len(daily)} rows)")

    # Period-split daily files (calibration / validation)
    # These are the files that run_calibration.sh looks for first.
    try:
        import config_forward_comprehensive as _cfg
        for label, tr in [('calibration', _cfg.TIME_RANGE_CALIBRATION),
                          ('validation', _cfg.TIME_RANGE_VALIDATION)]:
            start = pd.to_datetime(tr['start'])
            end = pd.to_datetime(tr['end'])
            period = daily[(daily['date'] >= start) & (daily['date'] <= end)]
            fname = f"{site_name}_obs_{tr['start']}_{tr['end']}.csv"
            out_period = output_dir / fname
            period.to_csv(out_period, index=False)
            print(f"[SUCCESS] Wrote {out_period}  ({len(period)} rows, {label})")
    except ImportError:
        print("[WARNING] config_forward_comprehensive not importable; "
              "skipping period-split files. Run from the project root.")

    return out_30min, out_daily


def main():
    parser = argparse.ArgumentParser(
        description="Generic site-observation preparer for the Noah-MP "
                    "emulator-based calibration framework.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Example:
  python3 src/prepare_observation_data.py \\
      --input  raw_obs/MySite_fluxes_2018.csv \\
      --datetime_col TIMESTAMP_END \\
      --time_format "%Y%m%d%H%M" \\
      --columns LH=LE HFX=H SOIL_M=SWC_1_1_1 \\
      --utc_offset_hours -5 \\
      --site_name MySite \\
      --output_dir data/obs
""",
    )
    parser.add_argument('--input', required=True,
                        help='Input observation CSV.')
    parser.add_argument('--datetime_col', required=True,
                        help='Name of the datetime column in the input CSV.')
    parser.add_argument('--time_format', default=None,
                        help='Optional strptime format. If omitted, pandas '
                             'infers.')
    parser.add_argument('--columns', nargs='+', required=True,
                        help='Renaming list: FRAMEWORK_NAME=CSV_COLUMN '
                             '(e.g. LH=LE HFX=H SOIL_M=SWC).')
    parser.add_argument('--utc_offset_hours', type=float, default=0.0,
                        help='Local-time offset (hours) of the input '
                             'timestamps from UTC. Default 0.')
    parser.add_argument('--site_name', required=True,
                        help='Site identifier, used in output filenames.')
    parser.add_argument('--output_dir', required=True,
                        help='Output directory.')

    args = parser.parse_args()

    try:
        rename_map = _parse_columns_arg(args.columns)
    except ValueError as exc:
        parser.error(str(exc))
        return 2

    try:
        prepare_observations(
            input_csv=args.input,
            datetime_col=args.datetime_col,
            rename_map=rename_map,
            output_dir=args.output_dir,
            site_name=args.site_name,
            time_format=args.time_format,
            utc_offset_hours=args.utc_offset_hours,
        )
    except (KeyError, ValueError) as exc:
        print(f"[ERROR] {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
