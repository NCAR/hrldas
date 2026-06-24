"""
Data preprocessing script for COMPREHENSIVE FORWARD LSTM model
Prepares data for predicting ALL energy/water cycle variables.

The emulator operates exclusively on DAILY aggregates. (Noah-MP itself still
runs at 30-min cadence; only the emulator I/O is daily.)
"""

import xarray as xr
import pandas as pd
import numpy as np
from pathlib import Path
import pickle
from tqdm import tqdm
import argparse
import config_forward_comprehensive as config


def load_forcing_data(forcing_file=None, temporal_resolution=None):
    """
    Load forcing data from extracted daily NetCDF file.

    Args:
        forcing_file: Path to forcing NetCDF file (defaults to
                      config.FORCING_FILE_DAILY).
        temporal_resolution: Accepted for backward compatibility but ignored;
                             always 'daily'.

    Returns:
        DataFrame with forcing data
    """
    if forcing_file is None:
        forcing_file = config.FORCING_FILE_DAILY

    print(f"Loading forcing data from: {forcing_file}")
    print(f"Temporal resolution: daily")
    
    ds = xr.open_dataset(forcing_file)
    
    # Convert to DataFrame
    forcing_df = ds.to_dataframe().reset_index()
    
    # Handle time column
    if 'time' in forcing_df.columns:
        forcing_df['datetime'] = pd.to_datetime(forcing_df['time'])
        forcing_df['date'] = forcing_df['datetime'].dt.date
    
    # Add temporal features
    if config.ADD_TIME_FEATURES:
        forcing_df['doy'] = forcing_df['datetime'].dt.dayofyear / 365.0
    
    if config.ADD_MONTH_FEATURE:
        months = forcing_df['datetime'].dt.month
        for month in range(1, 13):
            forcing_df[f'month_{month}'] = (months == month).astype(float)
    
    ds.close()
    
    print(f"  Loaded {len(forcing_df)} timesteps")
    print(f"  Variables: {[v['name'] for v in config.FORCING_VARIABLES]}")
    
    return forcing_df


def load_target_data(sample_idx, data_dir='data/raw/sim_results', temporal_resolution=None):
    """
    Load target variables from a Noah-MP simulation output. Always aggregated
    to daily.

    Args:
        sample_idx: Sample index (1-based)
        data_dir: Directory containing simulation results
        temporal_resolution: Accepted for backward compatibility but ignored.

    Returns:
        DataFrame with target data, or None if failed
    """
    # Daily-only emulator pipeline; legacy '30min' branch removed.
    
    output_dir = Path(data_dir) / f'sample_{sample_idx}' / 'output'
    
    if not output_dir.exists():
        return None
    
    # Find the LDASOUT file that matches the configured time range
    ldasout_files = list(output_dir.glob('*.LDASOUT_DOMAIN1'))
    if not ldasout_files:
        return None
    
    # Select the LDASOUT file that matches the start year from config
    start_year = config.TIME_RANGE_FULL['start'][:4]  # e.g., '2005'
    matching_files = [f for f in ldasout_files if f.name.startswith(start_year)]
    
    if matching_files:
        sim_path = matching_files[0]
    else:
        # Fall back to the newest file (largest filename = latest date)
        sim_path = sorted(ldasout_files, key=lambda x: x.name, reverse=True)[0]
    
    try:
        ds = xr.open_dataset(sim_path)
    except Exception as e:
        print(f"Error loading sample {sample_idx}: {e}")
        return None
    
    # Get time information
    time_strings = [s.decode() if isinstance(s, bytes) else s for s in ds['Times'].values]
    time_strings = [s.replace('_', ' ') for s in time_strings]
    times = pd.to_datetime(time_strings)
    
    # Extract target variables
    target_data = {'time': times}
    
    for var_config in config.TARGET_VARIABLES:
        var_name = var_config['name']
        
        if var_config.get('convert_accumulated_to_rate', False):
            output_name = var_name
        else:
            output_name = var_config.get('output_name', var_name)
        
        if var_name not in ds:
            ds.close()
            return None
        
        var_data = ds[var_name]
        
        # Handle multi-layer variables
        if 'layer' in var_config:
            layer_dim = None
            for dim in ['soil_layers_stag', 'snow_layers']:
                if dim in var_data.dims:
                    layer_dim = dim
                    break
            
            if layer_dim:
                var_data = var_data.isel({layer_dim: var_config['layer']})
        
        # Squeeze spatial dimensions
        for dim in ['south_north', 'west_east']:
            if dim in var_data.dims:
                var_data = var_data.isel({dim: 0})
        
        target_data[output_name] = var_data.values
    
    ds.close()
    
    # Create DataFrame
    target_df = pd.DataFrame(target_data)
    target_df['datetime'] = target_df['time']
    target_df['date'] = target_df['datetime'].dt.date
    
    # Aggregate to daily (the only supported emulator resolution)
    target_agg_dict = {}
    for var_config in config.TARGET_VARIABLES:
        if var_config.get('convert_accumulated_to_rate', False):
            col_name = var_config['name']
        else:
            col_name = var_config.get('output_name', var_config['name'])
        target_agg_dict[col_name] = var_config['aggregation']

    target_daily = target_df.groupby('date').agg(target_agg_dict).reset_index()

    # Convert accumulated runoff to daily rates
    if 'UGDRNOFF' in target_daily.columns:
        ugdrnoff = target_daily['UGDRNOFF'].values
        ugdrnoff_rate = np.diff(ugdrnoff, prepend=ugdrnoff[0])
        ugdrnoff_rate[0] = ugdrnoff[0]
        target_daily['UGDRNOFF_RATE'] = ugdrnoff_rate
        target_daily.drop('UGDRNOFF', axis=1, inplace=True)

    if 'SFCRNOFF' in target_daily.columns:
        sfcrnoff = target_daily['SFCRNOFF'].values
        sfcrnoff_rate = np.diff(sfcrnoff, prepend=sfcrnoff[0])
        sfcrnoff_rate[0] = sfcrnoff[0]
        target_daily['SFCRNOFF_RATE'] = sfcrnoff_rate
        target_daily.drop('SFCRNOFF', axis=1, inplace=True)

    return target_daily


def load_parameters(param_file=None):
    """Load parameter sets"""
    if param_file is None:
        param_file = config.PARAMETER_FILE
    params = pd.read_csv(param_file, sep=r'\s+')
    return params


def normalize_data(data, method='z-score', fit_stats=None):
    """Normalize data using specified method"""
    if method == 'z-score':
        if fit_stats is None:
            mean = np.nanmean(data, axis=tuple(range(data.ndim - 1)), keepdims=True)
            std = np.nanstd(data, axis=tuple(range(data.ndim - 1)), keepdims=True)
            stats = {'mean': mean, 'std': std, 'method': 'z-score'}
        else:
            mean = fit_stats['mean']
            std = fit_stats['std']
            stats = fit_stats
        
        normalized = (data - mean) / (std + 1e-8)
    
    elif method == 'min-max':
        if fit_stats is None:
            min_val = np.nanmin(data, axis=tuple(range(data.ndim - 1)), keepdims=True)
            max_val = np.nanmax(data, axis=tuple(range(data.ndim - 1)), keepdims=True)
            stats = {'min': min_val, 'max': max_val, 'method': 'min-max'}
        else:
            min_val = fit_stats['min']
            max_val = fit_stats['max']
            stats = fit_stats
        
        normalized = (data - min_val) / (max_val - min_val + 1e-8)
    
    else:
        raise ValueError(f"Unknown normalization method: {method}")
    
    return normalized, stats


def denormalize_data(normalized_data, stats):
    """Reverse normalization"""
    method = stats['method']
    
    if method == 'z-score':
        return normalized_data * stats['std'] + stats['mean']
    elif method == 'min-max':
        return normalized_data * (stats['max'] - stats['min']) + stats['min']
    else:
        raise ValueError(f"Unknown method: {method}")


def preprocess_all_data(max_samples=None, output_file=None, time_range=None,
                        temporal_resolution=None):
    """
    Process all samples for comprehensive forward modeling. Daily-only.

    Args:
        max_samples: Maximum samples to process
        output_file: Output pickle file path
        time_range: Optional dict with 'start' and 'end' dates
        temporal_resolution: Accepted for backward compatibility but ignored.

    Returns:
        dict with processed data
    """
    if max_samples is None:
        max_samples = config.MAX_SAMPLES
    if output_file is None:
        output_file = config.OUTPUT_FILE

    # Print configuration
    config.print_config()

    # Load shared forcing data
    print("\nLoading forcing data...")
    forcing_df = load_forcing_data()
    
    # Apply time range filter if specified
    if time_range is not None:
        start_date = pd.to_datetime(time_range['start'])
        end_date = pd.to_datetime(time_range['end'])
        forcing_df = forcing_df[
            (forcing_df['datetime'] >= start_date) & 
            (forcing_df['datetime'] <= end_date)
        ].reset_index(drop=True)
        print(f"  Filtered to time range: {start_date.date()} to {end_date.date()}")
        print(f"  Remaining timesteps: {len(forcing_df)}")
    
    # Load parameters
    print("\nLoading parameters...")
    params = load_parameters()
    
    # Process simulation results
    print(f"\nProcessing simulation results for up to {max_samples} samples...")
    print(f"Extracting {config.get_num_target_variables()} target variables...")
    
    all_targets = []
    valid_indices = []
    failed_samples = []
    
    for idx in tqdm(range(1, max_samples + 1)):
        target_data = load_target_data(idx, data_dir=config.SIMULATION_DIR)
        
        if target_data is not None:
            # Apply same time filter
            if time_range is not None:
                if 'datetime' in target_data.columns:
                    target_data = target_data[
                        (target_data['datetime'] >= start_date) & 
                        (target_data['datetime'] <= end_date)
                    ].reset_index(drop=True)
                elif 'date' in target_data.columns:
                    target_data = target_data[
                        (pd.to_datetime(target_data['date']) >= start_date) & 
                        (pd.to_datetime(target_data['date']) <= end_date)
                    ].reset_index(drop=True)
            
            # Check dimensions match
            if len(target_data) != len(forcing_df):
                print(f"Warning: Sample {idx} has mismatched timesteps ({len(target_data)} vs {len(forcing_df)}), skipping")
                failed_samples.append(idx)
                continue
            
            # Check for NaN values
            target_var_names = config.get_target_variable_names()
            if target_data[target_var_names].isnull().any().any():
                print(f"Warning: Sample {idx} contains NaN values, skipping")
                failed_samples.append(idx)
                continue
            
            all_targets.append(target_data)
            valid_indices.append(idx - 1)
        else:
            failed_samples.append(idx)
    
    print(f"\nSuccessfully loaded {len(all_targets)} samples")
    if failed_samples:
        print(f"Failed/skipped samples: {len(failed_samples)}")
    
    if len(all_targets) == 0:
        raise ValueError("No valid samples found!")
    
    # Get dimensions
    n_samples = len(all_targets)
    n_timesteps = len(forcing_df)
    n_forcing_vars = config.get_num_forcing_variables()
    n_target_vars = config.get_num_target_variables()
    n_params = params.shape[1]
    
    forcing_var_names = config.get_forcing_variable_names()
    target_var_names = config.get_target_variable_names()
    target_categories = config.get_target_variable_categories()
    param_names = params.columns.tolist()
    
    print(f"\nData dimensions:")
    print(f"  Samples: {n_samples}")
    print(f"  Timesteps: {n_timesteps}")
    print(f"  Forcing variables: {n_forcing_vars}")
    print(f"  Target variables: {n_target_vars}")
    print(f"  Parameters: {n_params}")
    
    # Create forcing array (same for all samples)
    X_forcing_single = np.zeros((n_timesteps, n_forcing_vars))
    for j, var_name in enumerate(forcing_var_names):
        X_forcing_single[:, j] = forcing_df[var_name].values
    
    # Replicate for all samples
    X_forcing = np.tile(X_forcing_single, (n_samples, 1, 1))
    
    # Create target array
    y = np.zeros((n_samples, n_timesteps, n_target_vars))
    for i, target_data in enumerate(all_targets):
        for j, var_name in enumerate(target_var_names):
            y[i, :, j] = target_data[var_name].values
    
    # Create parameter array
    X_params = params.iloc[valid_indices].values
    
    # Apply log transformation
    log_transformed_params = []
    if config.LOG_TRANSFORM_PARAMS:
        print(f"\nApplying log10 transformation to: {config.LOG_TRANSFORM_PARAMS}")
        for param_name in config.LOG_TRANSFORM_PARAMS:
            if param_name in param_names:
                param_idx = param_names.index(param_name)
                orig_range = (X_params[:, param_idx].min(), X_params[:, param_idx].max())
                X_params[:, param_idx] = np.log10(X_params[:, param_idx])
                log_range = (X_params[:, param_idx].min(), X_params[:, param_idx].max())
                print(f"  {param_name}: [{orig_range[0]:.2e}, {orig_range[1]:.2e}] -> [{log_range[0]:.2f}, {log_range[1]:.2f}]")
                log_transformed_params.append(param_name)
    
    # Check data quality
    print("\nChecking data quality...")
    for name, arr in [('X_forcing', X_forcing), ('X_params', X_params), ('y', y)]:
        if np.any(np.isnan(arr)) or np.any(np.isinf(arr)):
            print(f"  Warning: {name} contains NaN or Inf values")
        else:
            print(f"  {name}: OK")
    
    # Normalize data
    norm_method = config.NORMALIZATION_METHOD
    print(f"\nNormalizing data using method: {norm_method}")
    
    X_forcing_normalized, forcing_norm_stats = normalize_data(X_forcing, method=norm_method)
    X_params_normalized, params_norm_stats = normalize_data(X_params, method=norm_method)
    y_normalized, targets_norm_stats = normalize_data(y, method=norm_method)
    
    # Save processed data
    data_dict = {
        'X_forcing': X_forcing_normalized,
        'X_params': X_params_normalized,
        'y': y_normalized,
        # Normalization statistics
        'forcing_norm_stats': forcing_norm_stats,
        'params_norm_stats': params_norm_stats,
        'targets_norm_stats': targets_norm_stats,
        'normalization_method': norm_method,
        # Backward compatibility
        'X_forcing_mean': forcing_norm_stats.get('mean', None),
        'X_forcing_std': forcing_norm_stats.get('std', None),
        'X_params_mean': params_norm_stats.get('mean', None),
        'X_params_std': params_norm_stats.get('std', None),
        'y_mean': targets_norm_stats.get('mean', None),
        'y_std': targets_norm_stats.get('std', None),
        # Metadata
        'forcing_var_names': forcing_var_names,
        'target_var_names': target_var_names,
        'target_categories': target_categories,
        'param_names': param_names,
        'log_transformed_params': log_transformed_params,
        'n_timesteps': n_timesteps,
        'valid_indices': valid_indices,
        'failed_indices': failed_samples,
        'n_forcing_vars': n_forcing_vars,
        'n_target_vars': n_target_vars,
        'n_params': n_params,
        'time_range': time_range,
        'temporal_resolution': 'daily',
        'timestep_seconds': config.get_timestep_seconds(),
    }
    
    # Save
    Path(output_file).parent.mkdir(parents=True, exist_ok=True)
    with open(output_file, 'wb') as f:
        pickle.dump(data_dict, f)
    
    print(f"\nData preprocessing complete!")
    print(f"Forcing input shape: {X_forcing_normalized.shape}")
    print(f"Parameter input shape: {X_params_normalized.shape}")
    print(f"Target output shape: {y_normalized.shape}")
    print(f"Temporal resolution: daily")
    print(f"Saved to: {output_file}")
    
    return data_dict


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Preprocess data for LSTM emulator')
    parser.add_argument('--max_samples', type=int, default=None,
                       help='Maximum samples to process')
    parser.add_argument('--output', type=str, default=None,
                       help='Output file path')
    parser.add_argument('--calibration', action='store_true',
                       help='Use calibration time range (first year only)')

    args = parser.parse_args()

    time_range = None
    if args.calibration:
        time_range = config.TIME_RANGE_CALIBRATION
        print(f"Using calibration time range: {time_range}")

    data = preprocess_all_data(
        max_samples=args.max_samples,
        output_file=args.output,
        time_range=time_range,
    )
