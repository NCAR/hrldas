"""
Extract ALL forcing data from original LDASIN files and save as standalone NetCDF file
This creates forcing files that can be used for:
1. Running Noah-MP (30-min resolution required)
2. LSTM emulator training (30-min or daily aggregated)

Original forcing data variables (LDASIN format):
- T2D: 2m temperature (K)
- Q2D: 2m specific humidity (kg/kg)
- PSFC: Surface pressure (Pa)
- U2D: 2m U-wind (m/s)
- V2D: 2m V-wind (m/s)
- LWDOWN: Downward longwave radiation (W/m2)
- SWDOWN: Downward shortwave radiation (W/m2)
- RAINRATE: Precipitation rate (mm/s)
"""

import xarray as xr
import pandas as pd
import numpy as np
from pathlib import Path
import argparse
from glob import glob


# All forcing variables from LDASIN files with their aggregation methods
FORCING_VARIABLES = [
    {
        'name': 'T2D',
        'aggregation': 'mean',
        'description': '2m temperature (K)'
    },
    {
        'name': 'Q2D',
        'aggregation': 'mean',
        'description': '2m specific humidity (kg/kg)'
    },
    {
        'name': 'PSFC',
        'aggregation': 'mean',
        'description': 'Surface pressure (Pa)'
    },
    {
        'name': 'U2D',
        'aggregation': 'mean',
        'description': '2m U-wind component (m/s)'
    },
    {
        'name': 'V2D',
        'aggregation': 'mean',
        'description': '2m V-wind component (m/s)'
    },
    {
        'name': 'LWDOWN',
        'aggregation': 'mean',
        'description': 'Downward longwave radiation (W/m2)'
    },
    {
        'name': 'SWDOWN',
        'aggregation': 'mean',
        'description': 'Downward shortwave radiation (W/m2)'
    },
    {
        'name': 'RAINRATE',
        'aggregation': 'sum',
        'description': 'Precipitation rate (mm/s, or mm/day when daily aggregated)'
    },
]


def parse_ldasin_filename(filename):
    """
    Parse LDASIN filename to extract datetime
    Format: YYYYMMDDHHMM.LDASIN_DOMAIN1
    """
    basename = Path(filename).name
    datetime_str = basename.split('.')[0]
    return pd.to_datetime(datetime_str, format='%Y%m%d%H%M')


def extract_forcing_data_from_ldasin(forcing_dir, output_file, daily_aggregation=False, start_date=None, end_date=None):
    """
    Extract ALL forcing variables from original LDASIN files and save as NetCDF
    
    Args:
        forcing_dir: Directory containing LDASIN files
        output_file: Output NetCDF file path
        daily_aggregation: If True, aggregate to daily data; if False, keep 30-min data
        start_date: Start date for extraction (inclusive)
        end_date: End date for extraction (inclusive)
    """
    forcing_path = Path(forcing_dir)
    
    # Find all LDASIN files
    ldasin_files = sorted(glob(str(forcing_path / '*.LDASIN_DOMAIN1')))
    
    if not ldasin_files:
        raise FileNotFoundError(f"No LDASIN files found in: {forcing_path}")
    
    print(f"Found {len(ldasin_files)} LDASIN files")
    
    # Parse timestamps from filenames
    timestamps = [parse_ldasin_filename(f) for f in ldasin_files]
    
    # Filter based on date range
    if start_date or end_date:
        print(f"\nFiltering data based on time range:")
        if start_date:
            print(f"  Start date: {start_date}")
        if end_date:
            print(f"  End date: {end_date}")
            
        filtered_files = []
        filtered_timestamps = []
        
        ts_start = pd.to_datetime(start_date) if start_date else None
        ts_end = pd.to_datetime(end_date) if end_date else None
        
        for f, ts in zip(ldasin_files, timestamps):
            if ts_start and ts < ts_start:
                continue
            if ts_end and ts > ts_end:
                continue
            filtered_files.append(f)
            filtered_timestamps.append(ts)
            
        ldasin_files = filtered_files
        timestamps = filtered_timestamps
        
        if not ldasin_files:
            raise ValueError(f"No files found in the specified date range")

    print(f"Processing {len(ldasin_files)} files")
    if ldasin_files:
        print(f"First file: {Path(ldasin_files[0]).name}")
        print(f"Last file: {Path(ldasin_files[-1]).name}")
        print(f"\nTime range: {timestamps[0]} to {timestamps[-1]}")
    
    # Read all files and extract variables
    forcing_data = {var_config['name']: [] for var_config in FORCING_VARIABLES}
    
    print(f"\nExtracting {len(FORCING_VARIABLES)} forcing variables:")
    for var_config in FORCING_VARIABLES:
        print(f"  - {var_config['name']}: {var_config['description']}")
    
    print("\nReading files...")
    for i, (filepath, timestamp) in enumerate(zip(ldasin_files, timestamps)):
        if i % 1000 == 0:
            print(f"  Processing file {i+1}/{len(ldasin_files)}...")
        
        ds = xr.open_dataset(filepath)
        
        for var_config in FORCING_VARIABLES:
            var_name = var_config['name']
            
            if var_name in ds:
                value = ds[var_name].values.squeeze()
                forcing_data[var_name].append(float(value))
            else:
                print(f"  Warning: Variable '{var_name}' not found in {Path(filepath).name}")
                forcing_data[var_name].append(np.nan)
        
        ds.close()
    
    print(f"\nLoaded {len(timestamps)} timesteps (30-min resolution)")
    
    # Create DataFrame with 30-min data
    df = pd.DataFrame(forcing_data, index=timestamps)
    df.index.name = 'time'
    
    if daily_aggregation:
        print("\nAggregating to daily data...")
        
        daily_data = {}
        for var_config in FORCING_VARIABLES:
            var_name = var_config['name']
            agg_method = var_config['aggregation']
            
            if agg_method == 'mean':
                daily_data[var_name] = df[var_name].resample('D').mean()
            elif agg_method == 'sum':
                # Convert rate (mm/s) to daily total (mm/day)
                if var_name == 'RAINRATE':
                    daily_data[var_name] = (df[var_name] * 1800).resample('D').sum()
                else:
                    daily_data[var_name] = df[var_name].resample('D').sum()
            elif agg_method == 'max':
                daily_data[var_name] = df[var_name].resample('D').max()
            elif agg_method == 'min':
                daily_data[var_name] = df[var_name].resample('D').min()
            else:
                daily_data[var_name] = df[var_name].resample('D').mean()
        
        df_out = pd.DataFrame(daily_data)
        times = df_out.index
        temporal_resolution = 'daily'
        print(f"Aggregated to {len(times)} daily timesteps")
    else:
        df_out = df
        times = df.index
        temporal_resolution = '30-minute'
    
    # Create xarray Dataset
    forcing_ds = xr.Dataset(
        data_vars={name: (['time'], df_out[name].values) 
                   for name in df_out.columns},
        coords={'time': times}
    )
    
    # Add metadata
    forcing_ds.attrs['title'] = 'Forcing data for Noah-MP and LSTM emulator'
    forcing_ds.attrs['source'] = f'Original LDASIN files from {forcing_dir}'
    forcing_ds.attrs['created_by'] = 'extract_forcing_data.py'
    forcing_ds.attrs['description'] = 'All atmospheric forcing variables extracted from original NoahMP LDASIN forcing files'
    forcing_ds.attrs['temporal_resolution'] = temporal_resolution
    forcing_ds.attrs['num_variables'] = len(FORCING_VARIABLES)
    forcing_ds.attrs['timestep_seconds'] = 1800 if not daily_aggregation else 86400
    
    # Add variable descriptions
    for var_config in FORCING_VARIABLES:
        var_name = var_config['name']
        if var_name in forcing_ds:
            forcing_ds[var_name].attrs['description'] = var_config['description']
            forcing_ds[var_name].attrs['aggregation'] = var_config['aggregation']
    
    # Save to NetCDF
    output_path = Path(output_file)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    
    forcing_ds.to_netcdf(output_path)
    print(f"\nForcing data saved to: {output_path}")
    
    # Print summary
    print("\nForcing dataset summary:")
    print(forcing_ds)
    
    # Print statistics
    print("\nVariable statistics:")
    for var_config in FORCING_VARIABLES:
        var_name = var_config['name']
        if var_name in forcing_ds:
            data = forcing_ds[var_name].values
            print(f"  {var_name}:")
            print(f"    min: {np.nanmin(data):.6f}, max: {np.nanmax(data):.6f}, mean: {np.nanmean(data):.6f}")
    
    return forcing_ds


def main():
    parser = argparse.ArgumentParser(
        description='Extract ALL forcing data from original LDASIN files'
    )
    # Defaults come from paths_config.py so users only edit one file
    try:
        import paths_config
        default_forcing_dir = paths_config.FORCING_SOURCE
        default_output_30min = f'data/raw/forcing/forcing_{paths_config.SITE_NAME}_30min.nc'
        default_output_daily = f'data/raw/forcing/forcing_{paths_config.SITE_NAME}_daily.nc'
    except ImportError:
        default_forcing_dir = ''
        default_output_30min = 'data/raw/forcing/forcing_30min.nc'
        default_output_daily = 'data/raw/forcing/forcing_daily.nc'

    parser.add_argument('--forcing_dir', type=str,
                       default=default_forcing_dir,
                       help='Directory containing LDASIN forcing files (default from paths_config.py)')
    parser.add_argument('--output', type=str,
                       default=default_output_30min,
                       help='Output NetCDF file path')
    parser.add_argument('--daily', action='store_true',
                       help='Aggregate to daily resolution (default: keep 30-min)')
    parser.add_argument('--start_date', type=str, default=None,
                       help='Start date (YYYY-MM-DD or YYYY-MM-DD HH:MM)')
    parser.add_argument('--end_date', type=str, default=None,
                       help='End date (YYYY-MM-DD or YYYY-MM-DD HH:MM)')
    
    args = parser.parse_args()
    
    # Set output filename based on resolution (only if user accepted the default)
    if args.daily and args.output == default_output_30min:
        args.output = default_output_daily
    
    extract_forcing_data_from_ldasin(
        args.forcing_dir, 
        args.output, 
        daily_aggregation=args.daily,
        start_date=args.start_date,
        end_date=args.end_date
    )


if __name__ == '__main__':
    main()
