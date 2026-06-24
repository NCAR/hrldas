#!/usr/bin/env python3
"""
Parse Noah-MP NetCDF outputs to CSV format

This script extracts Noah-MP model output from NetCDF format to CSV format
for validation and analysis purposes.

Key Features:
- Preserves original 30-minute temporal resolution by default
- Applies UTC to local time conversion (UTC+8 for Malaysia PSO site)
- Extracts key variables: SOIL_M, LH, HFX, etc.

Timezone Handling:
- Noah-MP outputs timestamps in UTC (UTC+0)
- Malaysia PSO observations are in local time (UTC+8)
- This script converts Noah-MP UTC timestamps to local time by adding 8 hours
  Example: Noah-MP 2005-01-01 04:00 UTC -> 2005-01-01 12:00 local time

Author: Emulator-based calibration project
"""

import sys
import argparse
import xarray as xr
import pandas as pd
import numpy as np
from datetime import timedelta


# =============================================================================
# Timezone Configuration
# =============================================================================
# UTC offset for timestamp conversion is now a CLI flag (--utc_offset_hours).
# Default = 0 (keep UTC). Set per-site, e.g. Malaysia PSO = 8, Panama BCI = -5.


def parse_netcdf_to_csv(nc_file, csv_file, aggregate_daily=False,
                         apply_timezone=True, utc_offset_hours=0):
    """
    Parse Noah-MP NetCDF output to CSV
    
    Parameters:
    - nc_file: Path to NetCDF file
    - csv_file: Path to output CSV file
    - aggregate_daily: If True, aggregate to daily means (default: False, keep 30-min resolution)
    - apply_timezone: If True, convert UTC to local time (default: True)
    - utc_offset_hours: Offset in hours added to UTC (default 0). E.g.
      Malaysia PSO = 8, Panama BCI = -5.

    Processing Steps:
    1. Open NetCDF file and extract timestamps
    2. Apply timezone conversion (UTC -> local time)
    3. Extract target variables (HFX, LH, SOIL_M, etc.)
    4. Handle multi-dimensional variables (e.g., soil layers)
    5. Optionally aggregate to daily means
    6. Save to CSV
    """
    print(f'Parsing {nc_file}...')
    
    # Step 1: Open NetCDF file with explicit engine
    try:
        ds = xr.open_dataset(nc_file, engine='netcdf4')
    except:
        print('  Trying h5netcdf engine...')
        ds = xr.open_dataset(nc_file, engine='h5netcdf')
    
    # Step 2: Extract and convert timestamps
    # Noah-MP uses 'Times' variable with format 'YYYY-MM-DD_HH:MM:SS'
    times_bytes = ds['Times'].values
    times_str = [t.decode('utf-8') if isinstance(t, bytes) else str(t) for t in times_bytes]
    times_utc = pd.to_datetime(times_str, format='%Y-%m-%d_%H:%M:%S')
    
    print(f'  Original time range (UTC): {times_utc.min()} to {times_utc.max()}')
    print(f'  Number of timesteps: {len(times_utc)}')
    
    # Apply timezone conversion: UTC -> local time
    if apply_timezone:
        times_local = times_utc + timedelta(hours=utc_offset_hours)
        print(f'  Applied timezone conversion: UTC -> UTC{utc_offset_hours:+.0f}')
        print(f'  Converted time range (local): {times_local.min()} to {times_local.max()}')
        times = times_local
    else:
        times = times_utc
    
    # Step 3: Create DataFrame with timestamp
    data = {'timestamp': times}
    
    # Step 4: Extract target variables
    # All 29 emulator target variables plus GPP.
    # Multi-layer variables (SOIL_M, SOIL_T) are extracted per-layer with
    # suffixed names that match config_forward_comprehensive.py conventions.
    MULTI_LAYER_VARS = {
        'SOIL_M': {0: 'SOIL_M', 1: 'SOIL_M_L2', 2: 'SOIL_M_L3', 3: 'SOIL_M_L4'},
        'SOIL_T': {0: 'SOIL_T', 1: 'SOIL_T_L2'},
    }

    SCALAR_VARS = [
        'FSA', 'FIRA', 'HFX', 'LH', 'GRDFLX',
        'SAV', 'SAG', 'IRC', 'SHC', 'EVC', 'IRG', 'SHG', 'EVG', 'GHV',
        'ECAN', 'ETRAN', 'EDIR',
        'UGDRNOFF', 'SFCRNOFF',
        'CANLIQ',
        'TG', 'TV', 'TRAD',
        'GPP',
    ]

    # Extract multi-layer variables
    for nc_var, layer_map in MULTI_LAYER_VARS.items():
        if nc_var not in ds.variables:
            continue
        var_data = ds[nc_var].values
        original_shape = var_data.shape
        # Squeeze spatial dims but keep time and layer
        while var_data.ndim > 2:
            axis_to_squeeze = next(
                (ax for ax in range(1, var_data.ndim) if var_data.shape[ax] == 1),
                None
            )
            if axis_to_squeeze is None:
                break
            var_data = np.squeeze(var_data, axis=axis_to_squeeze)
        for layer_idx, col_name in layer_map.items():
            if var_data.ndim == 2 and layer_idx < var_data.shape[1]:
                data[col_name] = var_data[:, layer_idx]
                print(f'  {nc_var} layer {layer_idx} -> {col_name}: shape {original_shape} -> ({var_data.shape[0]},)')
            elif var_data.ndim == 1 and layer_idx == 0:
                data[col_name] = var_data
                print(f'  {nc_var} -> {col_name}: shape {var_data.shape}')

    # Extract scalar variables
    for nc_var in SCALAR_VARS:
        if nc_var not in ds.variables:
            continue
        var_data = ds[nc_var].values
        original_shape = var_data.shape
        var_data = var_data.squeeze()
        if var_data.ndim > 1:
            for dim_idx in range(var_data.ndim - 1, 0, -1):
                var_data = var_data.mean(axis=dim_idx)
            print(f'  {nc_var}: averaged across dimensions, shape {original_shape} -> {var_data.shape}')
        else:
            print(f'  {nc_var}: shape {var_data.shape}')
        data[nc_var] = var_data
    
    # Step 5: Calculate derived variables
    # Total ET from components (ECAN + ETRAN + EDIR, converted from mm/s to mm/30min)
    if all(k in data for k in ['ECAN', 'ETRAN', 'EDIR']):
        # Convert from mm/s to mm per timestep (30 min = 1800 s)
        data['ET'] = (data['ECAN'] + data['ETRAN'] + data['EDIR']) * 1800
        print(f'  Calculated ET from components (mm/30min)')
    
    # Create DataFrame
    df = pd.DataFrame(data)
    print(f'  Raw data shape: {df.shape}')
    
    # Step 6: Optionally aggregate to daily means
    if aggregate_daily:
        df['date'] = df['timestamp'].dt.date
        
        # Aggregate numeric columns
        numeric_cols = [col for col in df.columns if col not in ['timestamp', 'date']]
        agg_dict = {col: 'mean' for col in numeric_cols}
        
        if agg_dict:
            daily_df = df.groupby('date').agg(agg_dict).reset_index()
            daily_df['timestamp'] = pd.to_datetime(daily_df['date'])
            daily_df = daily_df.drop('date', axis=1)
            
            # Reorder columns
            cols = ['timestamp'] + [col for col in daily_df.columns if col != 'timestamp']
            daily_df = daily_df[cols]
            
            print(f'  Aggregated to daily means: {daily_df.shape}')
            df = daily_df
    
    # Reorder columns to have timestamp first
    cols = ['timestamp'] + [col for col in df.columns if col != 'timestamp']
    df = df[cols]
    
    # Save to CSV
    df.to_csv(csv_file, index=False)
    print(f'  Saved to {csv_file}')
    print(f'  Final columns: {list(df.columns)}')
    print(f'  Time range: {df["timestamp"].min()} to {df["timestamp"].max()}')
    print(f'  Sample data (first 3 rows):')
    print(df.head(3).to_string())
    print()
    
    ds.close()
    return df


def main():
    parser = argparse.ArgumentParser(
        description='Parse Noah-MP NetCDF outputs to CSV',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''
Examples:
  # Default: 30-minute resolution with timezone conversion
  python parse_noahmp_outputs.py --input output.nc --output output.csv
  
  # Aggregate to daily means
  python parse_noahmp_outputs.py --input output.nc --output output.csv --daily
  
  # Keep UTC timestamps (no timezone conversion)
  python parse_noahmp_outputs.py --input output.nc --output output.csv --no-timezone

Timezone:
  Noah-MP outputs use UTC. This script converts to local time (UTC+8 for Malaysia PSO)
  by default. Use --no-timezone to keep original UTC timestamps.
        '''
    )
    parser.add_argument('--input', required=True, help='Input NetCDF file')
    parser.add_argument('--output', required=True, help='Output CSV file')
    parser.add_argument('--daily', action='store_true',
                       help='Aggregate to daily means (default: keep 30-min resolution)')
    parser.add_argument('--no-timezone', action='store_true',
                       help='Do not apply timezone conversion (keep UTC)')
    parser.add_argument('--utc_offset_hours', type=float, default=0.0,
                       help='Local-time offset from UTC, hours (default 0). '
                            'Set per site, e.g. Malaysia PSO = 8, Panama BCI = -5.')

    args = parser.parse_args()

    try:
        parse_netcdf_to_csv(
            args.input,
            args.output,
            aggregate_daily=args.daily,
            apply_timezone=not args.no_timezone,
            utc_offset_hours=args.utc_offset_hours,
        )
        print('Success!')
        return 0
    except Exception as e:
        print(f'Error: {e}', file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
