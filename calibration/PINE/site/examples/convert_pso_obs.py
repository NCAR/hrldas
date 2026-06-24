#!/usr/bin/env python3
"""
Convert PSO 30-min observation data to standard format.

Features:
  1. Parse PSO observation files with Year, DOY, TIME format
  2. Use gap-filled data (H_gf, LE_gf) from PSO_LH_SH_*.csv for LH and HFX
  3. Use SWC from PSO_otherdata_*.csv for soil moisture
  4. Convert UTC+8 (local time) to UTC+0 (NoahMP time) by shifting -8 hours
  5. Generate 30-min resolution combined file
  6. Generate daily aggregated files for calibration and validation periods
     based on config_forward_comprehensive.py time ranges
  7. Tiered missing value handling (processed SEPARATELY for each variable):
     - >90% missing: deprecate day (set to NaN)
     - 50-90% missing: use adjacent day values to fill, then compute mean
     - <=50% missing: keep the day as complete (compute mean directly)

Input:
  - PSO_LH_SH_*.csv files (gap-filled H_gf and LE_gf)
  - PSO_otherdata_*.csv files (SWC)
Output:
  1. PSO_obs_all_combined_30min.csv - Combined 30-min data in UTC+0
  2. Malaysia_PSO_obs_{CAL_START}_{CAL_END}.csv - Daily calibration data
  3. Malaysia_PSO_obs_{VAL_START}_{VAL_END}.csv - Daily validation data
"""

import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime, timedelta
import sys

# Add project root directory to path for config import
import os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))
import config_forward_comprehensive as config

# Expected number of timesteps per day (30-min resolution)
TIMESTEPS_PER_DAY = 48


def parse_pso_time(year, doy, time_code):
    """Parse PSO time format to datetime (still in UTC+8).
    
    Handles TIME=2400 which represents midnight (00:00 of next day).
    Python datetime doesn't accept hour=24, so we convert it to next day 00:00.
    """
    try:
        time_code = int(time_code)
        hours = time_code // 100
        minutes = time_code % 100
        base_date = datetime(int(year), 1, 1) + timedelta(days=int(doy) - 1)
        
        # Handle TIME=2400 (midnight = 00:00 of next day)
        if hours == 24:
            hours = 0
            base_date = base_date + timedelta(days=1)
        
        full_datetime = base_date.replace(hour=hours, minute=minutes)
        return full_datetime
    except Exception as e:
        return pd.NaT


def convert_utc8_to_utc0(dt):
    """Convert UTC+8 (local time) to UTC+0 by subtracting 8 hours."""
    if pd.isna(dt):
        return pd.NaT
    return dt - timedelta(hours=8)


def process_pso_files_for_year(obs_dir, year):
    """
    Process PSO observation files for a single year.

    Reads:
      - PSO_LH_SH_{year}.csv for gap-filled H_gf and LE_gf
      - PSO_otherdata_{year}.csv for SWC

    Returns:
        DataFrame with datetime, SOIL_M, LH, HFX columns
    """
    lh_sh_file = obs_dir / f'PSO_LH_SH_{year}.csv'
    otherdata_file = obs_dir / f'PSO_otherdata_{year}.csv'

    print(f"Processing year {year}:")

    # Read gap-filled LH/SH data
    if not lh_sh_file.exists():
        print(f"  Warning: {lh_sh_file.name} not found, skipping LH/HFX")
        df_lh_sh = None
    else:
        print(f"  Reading gap-filled data: {lh_sh_file.name}")
        df_lh_sh = pd.read_csv(lh_sh_file, skiprows=[1])
        df_lh_sh['datetime_utc8'] = df_lh_sh.apply(
            lambda row: parse_pso_time(row['Year'], row['DOY'], row['TIME']),
            axis=1
        )
        df_lh_sh = df_lh_sh.dropna(subset=['datetime_utc8'])
        print(f"    Loaded {len(df_lh_sh)} records with H_gf, LE_gf")

    # Read otherdata for SWC
    if not otherdata_file.exists():
        print(f"  Warning: {otherdata_file.name} not found, skipping SWC")
        df_other = None
    else:
        print(f"  Reading SWC data: {otherdata_file.name}")
        df_other = pd.read_csv(otherdata_file, skiprows=[1])
        df_other['datetime_utc8'] = df_other.apply(
            lambda row: parse_pso_time(row['Year'], row['DOY'], row['TIME']),
            axis=1
        )
        df_other = df_other.dropna(subset=['datetime_utc8'])
        print(f"    Loaded {len(df_other)} records with SWC")

    # Merge data
    if df_lh_sh is not None and df_other is not None:
        # Merge on datetime_utc8
        df_merged = pd.merge(
            df_lh_sh[['datetime_utc8', 'H_gf', 'LE_gf']],
            df_other[['datetime_utc8', 'SWC']],
            on='datetime_utc8',
            how='outer'
        )
    elif df_lh_sh is not None:
        df_merged = df_lh_sh[['datetime_utc8', 'H_gf', 'LE_gf']].copy()
        df_merged['SWC'] = np.nan
    elif df_other is not None:
        df_merged = df_other[['datetime_utc8', 'SWC']].copy()
        df_merged['H_gf'] = np.nan
        df_merged['LE_gf'] = np.nan
    else:
        print(f"  Error: No data files found for year {year}")
        return None

    # Convert to UTC+0 for NoahMP compatibility
    df_merged['datetime'] = df_merged['datetime_utc8'].apply(convert_utc8_to_utc0)

    # Create result with standard column names
    # LE_gf -> LH, H_gf -> HFX, SWC -> SOIL_M
    result = df_merged[['datetime']].copy()
    result['SOIL_M'] = df_merged['SWC'].replace(-99999, np.nan)
    result['LH'] = df_merged['LE_gf']  # Gap-filled, no -99999 expected
    result['HFX'] = df_merged['H_gf']  # Gap-filled, no -99999 expected

    print(f"  Combined: {len(result)} records")
    return result


def aggregate_daily_tiered_missing(df, target_cols=['SOIL_M', 'LH', 'HFX']):
    """
    Aggregate 30-min data to daily with tiered missing value handling.
    
    IMPORTANT: Each variable is processed SEPARATELY, so a day can have valid
    SOIL_M but NaN for LH/HFX if their missing ratios differ.

    Tiered logic for handling missing values (per variable):
      - If missing > 90% of timesteps (>43 out of 48): deprecate (set to NaN)
      - If 50% < missing <= 90% (24-43 out of 48): use adjacent day values to fill,
        then compute daily mean. If no adjacent day data available, deprecate.
      - If missing <= 50% (<=24 out of 48): keep (compute mean directly)

    Args:
        df: DataFrame with 'datetime' and target columns
        target_cols: List of columns to process

    Returns:
        DataFrame with daily data (all days, with tiered NaN handling per variable)
    """
    df = df.copy()
    df['date_only'] = df['datetime'].dt.date
    df['time_of_day'] = df['datetime'].dt.time

    # Get the full date range
    min_date = df['date_only'].min()
    max_date = df['date_only'].max()
    all_dates = pd.date_range(start=min_date, end=max_date, freq='D')

    # Build a dictionary of {date: {time_of_day: row_data}} for quick lookup
    date_time_data = {}
    for date in df['date_only'].unique():
        day_data = df[df['date_only'] == date]
        date_time_data[date] = {row['time_of_day']: row for _, row in day_data.iterrows()}

    # Calculate missing counts per day per column
    daily_stats = {}
    for date in df['date_only'].unique():
        day_data = df[df['date_only'] == date]
        total_records = len(day_data)
        
        # Count missing (NaN) values for each target column separately
        col_stats = {}
        for col in target_cols:
            missing_count = day_data[col].isna().sum()
            missing_ratio = missing_count / TIMESTEPS_PER_DAY if TIMESTEPS_PER_DAY > 0 else 1.0
            col_stats[col] = {
                'missing_count': missing_count,
                'missing_ratio': missing_ratio
            }
        
        daily_stats[date] = {
            'total_records': total_records,
            'col_stats': col_stats
        }

    # Process each day with tiered logic - SEPARATELY for each variable
    daily_results = []
    stats_summary = {col: {'deprecated_high_missing': 0, 'filled_from_adjacent': 0, 
                           'kept_complete': 0, 'deprecated_no_adjacent': 0, 'no_data': 0}
                     for col in target_cols}
    
    for date in all_dates.date:
        result_row = {'date_only': date}
        
        # Check if we have any data for this day
        if date not in daily_stats:
            # No data for this day at all
            for col in target_cols:
                result_row[col] = np.nan
                stats_summary[col]['no_data'] += 1
            daily_results.append(result_row)
            continue
        
        stats = daily_stats[date]
        day_data = df[df['date_only'] == date]
        
        # Process each variable SEPARATELY
        for col in target_cols:
            col_stat = stats['col_stats'][col]
            missing_ratio = col_stat['missing_ratio']
            
            # Tier 1: missing > 90% -> deprecate
            if missing_ratio > 0.9:
                result_row[col] = np.nan
                stats_summary[col]['deprecated_high_missing'] += 1
                continue
            
            # Tier 3: missing <= 50% -> keep (compute mean directly)
            if missing_ratio <= 0.5:
                result_row[col] = day_data[col].mean()
                stats_summary[col]['kept_complete'] += 1
                continue
            
            # Tier 2: 50% < missing <= 90% -> try to fill from adjacent days
            prev_date = date - timedelta(days=1)
            next_date = date + timedelta(days=1)
            
            # Check if adjacent days have data
            has_prev = prev_date in date_time_data
            has_next = next_date in date_time_data
            
            if not has_prev and not has_next:
                # No adjacent day data available -> deprecate
                result_row[col] = np.nan
                stats_summary[col]['deprecated_no_adjacent'] += 1
                continue
            
            # Fill missing values from adjacent days for this column
            filled_values = []
            for _, row in day_data.iterrows():
                time_key = row['time_of_day']
                val = row[col]
                
                if pd.isna(val):
                    # Try to get value from previous day same time
                    fill_value = np.nan
                    if has_prev and time_key in date_time_data[prev_date]:
                        prev_val = date_time_data[prev_date][time_key][col]
                        if not pd.isna(prev_val):
                            fill_value = prev_val
                    
                    # If previous day doesn't have it, try next day
                    if pd.isna(fill_value) and has_next and time_key in date_time_data[next_date]:
                        next_val = date_time_data[next_date][time_key][col]
                        if not pd.isna(next_val):
                            fill_value = next_val
                    
                    filled_values.append(fill_value)
                else:
                    filled_values.append(val)
            
            # Calculate daily mean from filled data
            result_row[col] = np.nanmean(filled_values) if filled_values else np.nan
            stats_summary[col]['filled_from_adjacent'] += 1
        
        daily_results.append(result_row)

    # Create result DataFrame
    daily_df = pd.DataFrame(daily_results)
    
    # Print summary for each variable
    print(f"\n  Tiered missing value handling summary (per variable):")
    for col in target_cols:
        s = stats_summary[col]
        total_valid = s['kept_complete'] + s['filled_from_adjacent']
        print(f"\n    {col}:")
        print(f"      Kept complete (<=50% missing): {s['kept_complete']}")
        print(f"      Filled from adjacent (50-90% missing): {s['filled_from_adjacent']}")
        print(f"      Deprecated - high missing (>90%): {s['deprecated_high_missing']}")
        print(f"      Deprecated - no adjacent data: {s['deprecated_no_adjacent']}")
        print(f"      No data for day: {s['no_data']}")
        print(f"      Total valid days: {total_valid}")

    daily_df['date'] = pd.to_datetime(daily_df['date_only']).dt.strftime('%Y-%m-%d')
    daily_df = daily_df.drop(columns=['date_only'])
    daily_df = daily_df[['date', 'SOIL_M', 'LH', 'HFX']]

    return daily_df


def main():
    obs_dir = Path('/home/petrichor/ymwang/snap/Emulator-based_calibration/calibration-PSO/data/obs')

    # Find available years from PSO_LH_SH files (gap-filled data)
    lh_sh_files = sorted(obs_dir.glob('PSO_LH_SH_*.csv'))

    if not lh_sh_files:
        print("Error: No PSO_LH_SH_*.csv files found!")
        return

    # Extract years from filenames
    years = []
    for f in lh_sh_files:
        # Extract year from filename like PSO_LH_SH_2003.csv
        year_str = f.stem.split('_')[-1]
        if year_str.isdigit():
            years.append(int(year_str))

    years = sorted(years)
    print(f"Found gap-filled data for years: {years}")
    print(f"\nNote: Using gap-filled H_gf and LE_gf from PSO_LH_SH files")
    print(f"Note: Using SWC from PSO_otherdata files")
    print(f"Note: Converting UTC+8 (local time) to UTC+0 (NoahMP time)")

    # Process all years
    all_data = []
    for year in years:
        df = process_pso_files_for_year(obs_dir, year)
        if df is not None:
            all_data.append(df)
    
    # Combine all data
    combined_df = pd.concat(all_data, ignore_index=True)
    combined_df = combined_df.sort_values('datetime').drop_duplicates(subset=['datetime'])
    print(f"\nTotal 30-min records: {len(combined_df)}")
    
    # === Save 30-min resolution file (UTC+0 time) ===
    combined_30min = combined_df.copy()
    # Use same format as Panama_BCI file: M/D/YY H:MM
    combined_30min['date'] = combined_30min['datetime'].dt.strftime('%-m/%-d/%Y %-H:%M')
    combined_30min = combined_30min.drop(columns=['datetime'])
    # Reorder columns to match expected format: date, SOIL_M, LH, HFX
    combined_30min = combined_30min[['date', 'SOIL_M', 'LH', 'HFX']]
    
    output_30min = obs_dir / 'PSO_obs_all_combined_30min.csv'
    combined_30min.to_csv(output_30min, index=False)
    print(f"\nSaved 30-min data (UTC+0): {output_30min}")
    print(f"  Date range: {combined_df['datetime'].iloc[0]} to {combined_df['datetime'].iloc[-1]}")
    
    # === Generate daily aggregated files (with tiered missing value handling) ===
    print(f"\nAggregating to daily data (tiered missing value handling, per variable)...")
    daily_df = aggregate_daily_tiered_missing(combined_df)
    
    # Print valid day counts per variable
    print(f"\n  Summary - Valid days per variable:")
    for col in ['SOIL_M', 'LH', 'HFX']:
        valid_count = daily_df[col].notna().sum()
        print(f"    {col}: {valid_count} valid days out of {len(daily_df)} total")
    
    # Get time ranges from config
    cal_start = config.TIME_RANGE_CALIBRATION['start']
    cal_end = config.TIME_RANGE_CALIBRATION['end']
    val_start = config.TIME_RANGE_VALIDATION['start']
    val_end = config.TIME_RANGE_VALIDATION['end']
    full_start = config.TIME_RANGE_FULL['start']
    full_end = config.TIME_RANGE_FULL['end']
    
    print(f"\nTime ranges from config:")
    print(f"  Full:        {full_start} to {full_end}")
    print(f"  Calibration: {cal_start} to {cal_end}")
    print(f"  Validation:  {val_start} to {val_end}")
    
    # Save calibration period file
    cal_df = daily_df[(daily_df['date'] >= cal_start) & (daily_df['date'] <= cal_end)]
    cal_output = obs_dir / f'Malaysia_PSO_obs_{cal_start}_{cal_end}.csv'
    cal_df.to_csv(cal_output, index=False)
    print(f"\nSaved calibration file: {cal_output}")
    print(f"  Total: {len(cal_df)} days")
    for col in ['SOIL_M', 'LH', 'HFX']:
        print(f"    {col}: {cal_df[col].notna().sum()} valid")

    # Save validation period file
    val_df = daily_df[(daily_df['date'] >= val_start) & (daily_df['date'] <= val_end)]
    val_output = obs_dir / f'Malaysia_PSO_obs_{val_start}_{val_end}.csv'
    val_df.to_csv(val_output, index=False)
    print(f"Saved validation file: {val_output}")
    print(f"  Total: {len(val_df)} days")
    for col in ['SOIL_M', 'LH', 'HFX']:
        print(f"    {col}: {val_df[col].notna().sum()} valid")

    # Save full period file (for validation script)
    full_df = daily_df[(daily_df['date'] >= full_start) & (daily_df['date'] <= full_end)]
    full_output = obs_dir / f'Malaysia_PSO_obs_{full_start}_{full_end}.csv'
    full_df.to_csv(full_output, index=False)
    print(f"Saved full period file: {full_output}")
    print(f"  Total: {len(full_df)} days")
    for col in ['SOIL_M', 'LH', 'HFX']:
        print(f"    {col}: {full_df[col].notna().sum()} valid")
    
    # Print sample
    print(f"\n=== Sample daily data (first 5 rows) ===")
    print(daily_df.head(5).to_string())


if __name__ == '__main__':
    main()
