"""
Practical inference script for FORWARD LSTM model
Standardized input/output format for real-world usage

Input:
  1. Forcing data: NetCDF file with LWFORC, SWFORC, RAINRATE, T2MV
  2. Parameters: Text file (same format as noahmp_param_sets.txt)

Output:
  - CSV file with predictions (SOIL_M, LH, HFX columns)
  - Optional PNG plot
"""

import torch
import numpy as np
import pandas as pd
import xarray as xr
import pickle
import json
import matplotlib.pyplot as plt
from pathlib import Path
import argparse

from lstm_model_forward import LSTMForwardPredictor, BiLSTMForwardPredictor, AttentionLSTMForwardPredictor
import config_forward_comprehensive as config

# Main target variables for observation comparison
# Single source of truth: config_forward_comprehensive.MAIN_TARGETS
MAIN_TARGET_VARS = list(config.MAIN_TARGETS)


def load_model(model_dir, device='cpu'):
    """
    Load trained model from directory

    Args:
        model_dir: Directory containing model files
        device: Device to load model on

    Returns:
        model, data_dict, config_dict
    """
    model_dir = Path(model_dir)

    # Load config
    with open(model_dir / 'config.json', 'r') as f:
        config_dict = json.load(f)

    # Load data statistics (for normalization)
    data_file = config.OUTPUT_FILE
    with open(data_file, 'rb') as f:
        data_dict = pickle.load(f)

    # Create model
    model_type = config_dict['model_type']
    model_config = config_dict['model_config']

    model_kwargs = {
        'n_params': config_dict['n_params'],
        'n_forcing_vars': config_dict['n_forcing_vars'],
        'n_target_vars': config_dict['n_target_vars'],
        'hidden_dim': model_config['hidden_dim'],
        'num_layers': model_config['num_layers'],
        'param_embedding_dim': model_config['param_embedding_dim'],
        'dropout': model_config['dropout']
    }

    if model_type == 'LSTM':
        model = LSTMForwardPredictor(**model_kwargs)
    elif model_type == 'BiLSTM':
        model = BiLSTMForwardPredictor(**model_kwargs)
    elif model_type == 'AttentionLSTM':
        model = AttentionLSTMForwardPredictor(**model_kwargs)
    else:
        raise ValueError(f"Unknown model type: {model_type}")

    # Load weights
    model.load_state_dict(torch.load(model_dir / 'best_model.pth', map_location=device))
    model = model.to(device)
    model.eval()

    return model, data_dict, config_dict


def load_forcing_from_netcdf(forcing_file, forcing_var_names):
    """
    Load forcing data from NetCDF file

    Args:
        forcing_file: Path to NetCDF file
        forcing_var_names: List of forcing variable names to extract

    Returns:
        forcing_df: DataFrame with daily aggregated forcing data
    """
    print(f"\nLoading forcing data from: {forcing_file}")

    # Load NetCDF
    ds = xr.open_dataset(forcing_file)

    # Extract time
    times = pd.to_datetime(ds['time'].values)

    # Extract forcing variables
    forcing_data = {'time': times}

    for var_name in forcing_var_names:
        if var_name == 'doy':
            continue  # Will be added later

        if var_name not in ds:
            raise ValueError(f"Variable '{var_name}' not found in forcing file")

        forcing_data[var_name] = ds[var_name].values

    ds.close()

    # Create DataFrame
    forcing_df = pd.DataFrame(forcing_data)

    # Aggregate to daily based on configuration
    forcing_df['date'] = forcing_df['time'].dt.date
    agg_dict = {}

    for var_config in config.FORCING_VARIABLES:
        var_name = var_config['name']
        if var_name in forcing_df.columns:
            agg_dict[var_name] = var_config['aggregation']

    forcing_daily = forcing_df.groupby('date').agg(agg_dict).reset_index()

    # Add temporal features
    if config.ADD_TIME_FEATURES:
        forcing_daily['doy'] = pd.to_datetime(forcing_daily['date']).dt.dayofyear / 365.0

    if config.ADD_MONTH_FEATURE:
        months = pd.to_datetime(forcing_daily['date']).dt.month
        for month in range(1, 13):
            forcing_daily[f'month_{month}'] = (months == month).astype(float)

    print(f"  ✓ Loaded forcing data: {forcing_daily.shape[0]} days")
    print(f"  Variables: {list(agg_dict.keys())}")

    return forcing_daily


def load_parameters_from_txt(param_file, data_dict):
    """
    Load parameters from text file

    Args:
        param_file: Path to parameter file (same format as noahmp_param_sets.txt)
        data_dict: Dictionary with parameter names and statistics

    Returns:
        params_df: DataFrame with parameters
    """
    print(f"\nLoading parameters from: {param_file}")

    # Read parameter file
    params_df = pd.read_csv(param_file, sep=r'\s+')

    print(f"  ✓ Loaded {len(params_df)} parameter set(s)")

    # Check if all required parameters are present
    expected_params = data_dict['param_names']
    missing_params = set(expected_params) - set(params_df.columns)
    if missing_params:
        raise ValueError(f"Missing parameters in file: {missing_params}")

    # Reorder columns to match expected order
    params_df = params_df[expected_params]

    print(f"  Parameters: {list(params_df.columns)}")

    return params_df


def predict(model, params, forcing, data_dict, device='cpu'):
    """
    Predict time series from parameters and forcing

    Args:
        model: Trained model
        params: Parameters array (n_params,) or (n_samples, n_params)
        forcing: Forcing array (n_timesteps, n_forcing_vars) or (n_samples, n_timesteps, n_forcing_vars)
        data_dict: Dictionary with normalization statistics
        device: Device to run prediction on

    Returns:
        Predicted time series (denormalized)
    """
    # Handle single sample case
    if params.ndim == 1:
        params = params.reshape(1, -1)
    if forcing.ndim == 2:
        forcing = forcing.reshape(1, forcing.shape[0], forcing.shape[1])

    # Normalize inputs using the appropriate method
    # Normalize parameters
    params_stats = data_dict['params_norm_stats']
    if params_stats['method'] == 'z-score':
        params_normalized = (params - params_stats['mean']) / (params_stats['std'] + 1e-8)
    elif params_stats['method'] == 'min-max':
        params_normalized = (params - params_stats['min']) / (params_stats['max'] - params_stats['min'] + 1e-8)
    else:
        raise ValueError(f"Unknown normalization method: {params_stats['method']}")

    # Normalize forcing
    forcing_stats = data_dict['forcing_norm_stats']
    if forcing_stats['method'] == 'z-score':
        forcing_normalized = (forcing - forcing_stats['mean']) / (forcing_stats['std'] + 1e-8)
    elif forcing_stats['method'] == 'min-max':
        forcing_normalized = (forcing - forcing_stats['min']) / (forcing_stats['max'] - forcing_stats['min'] + 1e-8)
    else:
        raise ValueError(f"Unknown normalization method: {forcing_stats['method']}")

    # Convert to tensors
    params_tensor = torch.FloatTensor(params_normalized).to(device)
    forcing_tensor = torch.FloatTensor(forcing_normalized).to(device)

    # Predict
    with torch.no_grad():
        predictions_normalized = model(params_tensor, forcing_tensor)
        predictions_normalized = predictions_normalized.cpu().numpy()

    # Denormalize predictions using the appropriate method
    targets_stats = data_dict['targets_norm_stats']
    if targets_stats['method'] == 'z-score':
        predictions = predictions_normalized * targets_stats['std'] + targets_stats['mean']
    elif targets_stats['method'] == 'min-max':
        predictions = predictions_normalized * (targets_stats['max'] - targets_stats['min']) + targets_stats['min']
    else:
        raise ValueError(f"Unknown normalization method: {targets_stats['method']}")

    return predictions


def load_observations_from_csv(obs_file, dates, target_var_names):
    """
    Load observation data from CSV file and align with prediction dates
    Only loads main target variables (SOIL_M, LH, HFX) for comparison

    Args:
        obs_file: Path to observation CSV file
        dates: Array of prediction dates
        target_var_names: List of all target variable names (for indexing)

    Returns:
        obs_array: Array of observations (n_timesteps, n_main_vars)
        valid_mask: Boolean mask indicating which timesteps have valid observations
        obs_var_names: List of variable names that have observations
        obs_indices: Indices in target_var_names for variables with observations
    """
    print(f"\nLoading observations from: {obs_file}")

    # Read observation file
    obs_df = pd.read_csv(obs_file)

    # Convert date column to datetime
    obs_df['date'] = pd.to_datetime(obs_df['date']).dt.date

    # Identify which main variables are available in the CSV
    available_vars = [col for col in obs_df.columns if col != 'date']
    obs_var_names = [var for var in MAIN_TARGET_VARS if var in available_vars]

    if not obs_var_names:
        raise ValueError(
            f"No main target variables found in observation file.\n"
            f"Expected at least one of: {MAIN_TARGET_VARS}\n"
            f"Found columns: {available_vars}"
        )

    # Get indices in target_var_names for the observed variables
    obs_indices = [target_var_names.index(var) for var in obs_var_names]

    print(f"  Main variables with observations: {obs_var_names}")
    if len(obs_var_names) < len(MAIN_TARGET_VARS):
        missing = set(MAIN_TARGET_VARS) - set(obs_var_names)
        print(f"  Note: Missing observations for: {missing}")

    # Create prediction dates dataframe
    pred_dates_df = pd.DataFrame({'date': dates})

    # Merge observations with prediction dates
    merged = pred_dates_df.merge(obs_df, on='date', how='left')

    # Extract observation arrays for available variables
    obs_array = merged[obs_var_names].values  # (n_timesteps, n_obs_vars)

    # Create valid mask (True where we have observations for all available variables)
    valid_mask = ~np.isnan(obs_array).any(axis=1)

    n_valid = valid_mask.sum()
    n_total = len(dates)

    print(f"  ✓ Loaded observations: {n_valid}/{n_total} days with valid data")

    return obs_array, valid_mask, obs_var_names, obs_indices


def compute_metrics(obs, pred, obs_var_names, valid_mask=None):
    """
    Compute metrics comparing observations and predictions
    Only computes metrics for variables that have observations

    Args:
        obs: Observations array (n_timesteps, n_obs_vars)
        pred: Predictions array (n_timesteps, n_obs_vars)
        obs_var_names: List of variable names with observations
        valid_mask: Boolean mask for valid observations (optional)

    Returns:
        Dictionary of metrics per variable
    """
    if valid_mask is not None:
        obs = obs[valid_mask]
        pred = pred[valid_mask]

    metrics = {}

    for i, var_name in enumerate(obs_var_names):
        obs_var = obs[:, i]
        pred_var = pred[:, i]

        # Remove any remaining NaN values
        valid = ~(np.isnan(obs_var) | np.isnan(pred_var))
        obs_var = obs_var[valid]
        pred_var = pred_var[valid]

        if len(obs_var) == 0:
            metrics[var_name] = {
                'R2': np.nan,
                'RMSE': np.nan,
                'PBIAS': np.nan,
                'n_valid': 0
            }
            continue

        # Compute R²
        ss_res = np.sum((obs_var - pred_var) ** 2)
        ss_tot = np.sum((obs_var - obs_var.mean()) ** 2)
        r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else np.nan

        # Compute RMSE
        rmse = np.sqrt(np.mean((obs_var - pred_var) ** 2))

        # Compute PBIAS (Percent Bias)
        pbias = 100 * np.sum(pred_var - obs_var) / np.sum(obs_var) if np.sum(obs_var) != 0 else np.nan

        metrics[var_name] = {
            'R2': float(r2),
            'RMSE': float(rmse),
            'PBIAS': float(pbias),
            'n_valid': int(len(obs_var))
        }

    return metrics


def save_predictions_csv(predictions, dates, target_var_names, output_file):
    """
    Save predictions to CSV file

    Args:
        predictions: Predictions array (n_samples, n_timesteps, n_target_vars)
        dates: List of dates
        target_var_names: List of target variable names
        output_file: Output CSV file path
    """
    n_samples = predictions.shape[0]

    if n_samples == 1:
        # Single sample: simple format
        df_data = {'date': dates}
        for i, var_name in enumerate(target_var_names):
            df_data[var_name] = predictions[0, :, i]

        df = pd.DataFrame(df_data)
        df.to_csv(output_file, index=False)
        print(f"  ✓ Saved predictions to: {output_file}")

    else:
        # Multiple samples: save each sample separately
        output_path = Path(output_file)
        output_dir = output_path.parent
        output_stem = output_path.stem

        for sample_idx in range(n_samples):
            sample_file = output_dir / f"{output_stem}_sample_{sample_idx+1}.csv"

            df_data = {'date': dates}
            for i, var_name in enumerate(target_var_names):
                df_data[var_name] = predictions[sample_idx, :, i]

            df = pd.DataFrame(df_data)
            df.to_csv(sample_file, index=False)

        print(f"  ✓ Saved {n_samples} prediction files to: {output_dir}")


def plot_predictions(predictions, dates, target_var_names, observations=None,
                     valid_mask=None, obs_var_names=None, obs_indices=None,
                     param_set_idx=1, save_path=None):
    """
    Plot predicted time series with optional observations overlay
    Only plots variables with observations when observations are provided

    Args:
        predictions: Predictions array (n_timesteps, n_target_vars) or (1, n_timesteps, n_target_vars)
        dates: List of dates
        target_var_names: List of all target variable names
        observations: Observations array (n_timesteps, n_obs_vars), optional
        valid_mask: Boolean mask for valid observations, optional
        obs_var_names: List of variable names with observations, optional
        obs_indices: Indices in target_var_names for variables with observations, optional
        param_set_idx: Parameter set index (for title)
        save_path: Path to save plot
    """
    # Handle batch dimension
    if predictions.ndim == 3:
        predictions = predictions[0]

    # If observations provided, only plot those variables
    if observations is not None and obs_var_names is not None and obs_indices is not None:
        vars_to_plot = obs_var_names
        pred_indices = obs_indices
    else:
        # Plot all variables
        vars_to_plot = target_var_names
        pred_indices = list(range(len(target_var_names)))

    n_vars_to_plot = len(vars_to_plot)

    fig, axes = plt.subplots(n_vars_to_plot, 1, figsize=(12, 3 * n_vars_to_plot))

    if n_vars_to_plot == 1:
        axes = [axes]

    for plot_idx, (var_name, pred_idx) in enumerate(zip(vars_to_plot, pred_indices)):
        ax = axes[plot_idx]

        # Plot predictions
        ax.plot(dates, predictions[:, pred_idx], linewidth=2, alpha=0.8, color='blue',
                label='Predicted', zorder=2)

        # Plot observations if available
        if observations is not None:
            obs_to_plot = observations[:, plot_idx].copy()
            if valid_mask is not None:
                # Set invalid observations to NaN so they don't plot
                obs_to_plot[~valid_mask] = np.nan
            ax.plot(dates, obs_to_plot, linewidth=2, alpha=0.7, color='red',
                    label='Observed', linestyle='--', zorder=3)

        ax.set_xlabel('Date', fontsize=11)
        ax.set_ylabel(var_name, fontsize=11)

        if observations is not None:
            ax.set_title(f'{var_name} Comparison (Parameter Set {param_set_idx})', fontsize=12)
            ax.legend(fontsize=10, loc='best')
        else:
            ax.set_title(f'Predicted {var_name} (Parameter Set {param_set_idx})', fontsize=12)

        ax.grid(True, alpha=0.3)

        # Rotate date labels
        plt.setp(ax.xaxis.get_majorticklabels(), rotation=45, ha='right')

    title = 'LSTM Forward Model: Predictions vs Observations' if observations is not None else 'LSTM Forward Model Predictions'
    plt.suptitle(title, fontsize=14, fontweight='bold')
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"  ✓ Saved plot to: {save_path}")
    else:
        plt.show()

    plt.close()


def plot_scatter(predictions, observations, obs_var_names, obs_indices, metrics,
                 valid_mask=None, param_set_idx=1, save_path=None):
    """
    Plot scatter plots of predictions vs observations
    Only plots variables with observations

    Args:
        predictions: Predictions array (n_timesteps, n_target_vars) or (1, n_timesteps, n_target_vars)
        observations: Observations array (n_timesteps, n_obs_vars)
        obs_var_names: List of variable names with observations
        obs_indices: Indices in predictions for variables with observations
        metrics: Dictionary of metrics per variable
        valid_mask: Boolean mask for valid observations, optional
        param_set_idx: Parameter set index (for title)
        save_path: Path to save plot
    """
    # Handle batch dimension
    if predictions.ndim == 3:
        predictions = predictions[0]

    n_obs_vars = len(obs_var_names)

    # Calculate subplot layout
    ncols = min(3, n_obs_vars)
    nrows = (n_obs_vars + ncols - 1) // ncols

    fig, axes = plt.subplots(nrows, ncols, figsize=(5 * ncols, 5 * nrows))

    if n_obs_vars == 1:
        axes = np.array([axes])
    axes = axes.flatten() if n_obs_vars > 1 else axes

    for plot_idx, (var_name, pred_idx) in enumerate(zip(obs_var_names, obs_indices)):
        ax = axes[plot_idx]

        # Get observations and predictions for this variable
        obs_var = observations[:, plot_idx].copy()
        pred_var = predictions[:, pred_idx].copy()

        # Apply valid mask if provided
        if valid_mask is not None:
            obs_var = obs_var[valid_mask]
            pred_var = pred_var[valid_mask]

        # Remove NaN values
        valid = ~(np.isnan(obs_var) | np.isnan(pred_var))
        obs_var = obs_var[valid]
        pred_var = pred_var[valid]

        if len(obs_var) > 0:
            # Scatter plot
            ax.scatter(obs_var, pred_var, alpha=0.5, s=30, edgecolors='black', linewidth=0.5)

            # 1:1 line
            min_val = min(obs_var.min(), pred_var.min())
            max_val = max(obs_var.max(), pred_var.max())
            ax.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, label='1:1 Line')

            # Get metrics for this variable
            var_metrics = metrics.get(var_name, {})
            r2 = var_metrics.get('R2', np.nan)
            rmse = var_metrics.get('RMSE', np.nan)
            pbias = var_metrics.get('PBIAS', np.nan)

            # Add metrics text box
            textstr = f'R² = {r2:.3f}\nRMSE = {rmse:.3f}\nPBIAS = {pbias:.2f}%'
            props = dict(boxstyle='round', facecolor='wheat', alpha=0.8)
            ax.text(0.05, 0.95, textstr, transform=ax.transAxes, fontsize=10,
                    verticalalignment='top', bbox=props)

        ax.set_xlabel(f'Observed {var_name}', fontsize=11)
        ax.set_ylabel(f'Predicted {var_name}', fontsize=11)
        ax.set_title(f'{var_name} (Parameter Set {param_set_idx})', fontsize=12)
        ax.grid(True, alpha=0.3)
        ax.legend(fontsize=9, loc='lower right')

        # Equal aspect ratio
        ax.set_aspect('equal', adjustable='box')

    # Hide unused subplots
    for j in range(plot_idx + 1, len(axes)):
        axes[j].axis('off')

    plt.suptitle('Predictions vs Observations (Main Variables)', fontsize=14, fontweight='bold')
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"  ✓ Saved scatter plot to: {save_path}")
    else:
        plt.show()

    plt.close()


def main():
    parser = argparse.ArgumentParser(
        description='Practical inference for LSTM forward model',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Single parameter set (no observations)
  python 04_inference.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_143022_dim-512_layer-2 \\
    --forcing data/raw/forcing/forcing_sample_1.nc \\
    --params data/raw/param/test_params.txt \\
    --output predictions.csv \\
    --plot

  # Single parameter set with observations comparison
  # Note: Observation file only needs main variables (SOIL_M, LH, HFX)
  python 04_inference.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_143022_dim-512_layer-2 \\
    --forcing data/raw/forcing/forcing_sample_1.nc \\
    --params data/raw/param/test_params.txt \\
    --obs data/obs/observations.csv \\
    --output predictions.csv \\
    --plot

  # Multiple parameter sets with observations
  python 04_inference.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_143022_dim-512_layer-2 \\
    --forcing data/raw/forcing/forcing_sample_1.nc \\
    --params data/raw/param/noahmp_param_sets.txt \\
    --obs data/obs/observations.csv \\
    --output predictions.csv \\
    --max_samples 5 \\
    --plot

  # Observation CSV format (only main variables needed):
  # date,SOIL_M,LH,HFX
  # 2015-07-30,0.25,120.5,45.2
  # 2015-07-31,0.24,118.3,43.1
        """
    )
    parser.add_argument('--model_dir', type=str, required=True,
                       help='Directory containing trained model')
    parser.add_argument('--forcing', type=str, required=True,
                       help='NetCDF file with forcing data')
    parser.add_argument('--params', type=str, required=True,
                       help='Text file with parameters (same format as noahmp_param_sets.txt)')
    parser.add_argument('--output', type=str, required=True,
                       help='Output CSV file path')
    parser.add_argument('--obs', type=str, default=None,
                       help='CSV file with observations for main target variables (SOIL_M, LH, HFX). '
                            'Format: date,SOIL_M,LH,HFX (can include subset of main variables)')
    parser.add_argument('--plot', action='store_true',
                       help='Generate PNG plot (time series and scatter if --obs provided)')
    parser.add_argument('--max_samples', type=int, default=None,
                       help='Maximum number of parameter sets to process (default: all)')

    args = parser.parse_args()

    # Device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")

    # Load model
    print(f"\n{'='*60}")
    print("LOADING MODEL")
    print('='*60)
    model, data_dict, config_dict = load_model(args.model_dir, device)
    print(f"  Model type: {config_dict['model_type']}")
    print(f"  Parameters: {config_dict['total_parameters']:,}")

    # Load forcing data
    print(f"\n{'='*60}")
    print("LOADING FORCING DATA")
    print('='*60)
    forcing_var_names = data_dict['forcing_var_names']
    forcing_df = load_forcing_from_netcdf(args.forcing, forcing_var_names)

    # Extract forcing array
    forcing_array = forcing_df[forcing_var_names].values  # (n_timesteps, n_forcing_vars)
    dates = forcing_df['date'].values

    print(f"  Forcing shape: {forcing_array.shape}")

    # Load parameters
    print(f"\n{'='*60}")
    print("LOADING PARAMETERS")
    print('='*60)
    params_df = load_parameters_from_txt(args.params, data_dict)

    # Limit number of samples if specified
    if args.max_samples is not None:
        n_samples = min(args.max_samples, len(params_df))
        params_df = params_df.head(n_samples)
        print(f"  Processing first {n_samples} parameter set(s)")

    params_array = params_df.values  # (n_samples, n_params)

    print(f"  Parameters shape: {params_array.shape}")

    # Predict
    print(f"\n{'='*60}")
    print("RUNNING PREDICTIONS")
    print('='*60)

    # Replicate forcing for all parameter sets
    n_samples = params_array.shape[0]
    forcing_replicated = np.tile(forcing_array[np.newaxis, :, :],
                                 (n_samples, 1, 1))  # (n_samples, n_timesteps, n_forcing_vars)

    print(f"  Input shapes:")
    print(f"    Parameters: {params_array.shape}")
    print(f"    Forcing: {forcing_replicated.shape}")

    predictions = predict(model, params_array, forcing_replicated, data_dict, device)

    print(f"  Output shape: {predictions.shape}")

    target_var_names = data_dict['target_var_names']

    # Save predictions
    print(f"\n{'='*60}")
    print("SAVING RESULTS")
    print('='*60)

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    save_predictions_csv(predictions, dates, target_var_names, args.output)

    # Load and compare with observations if provided
    observations = None
    valid_mask = None
    obs_var_names = None
    obs_indices = None
    metrics_all = []

    if args.obs:
        print(f"\n{'='*60}")
        print("LOADING OBSERVATIONS AND COMPUTING METRICS")
        print('='*60)

        observations, valid_mask, obs_var_names, obs_indices = load_observations_from_csv(
            args.obs, dates, target_var_names
        )

        # Compute metrics for each sample
        for sample_idx in range(n_samples):
            pred_sample = predictions[sample_idx]  # (n_timesteps, n_target_vars)
            # Extract only the predicted values for variables with observations
            pred_sample_obs = pred_sample[:, obs_indices]  # (n_timesteps, n_obs_vars)

            metrics = compute_metrics(observations, pred_sample_obs, obs_var_names, valid_mask)
            metrics_all.append(metrics)

            print(f"\nMetrics for Parameter Set {sample_idx + 1}:")
            for var_name, var_metrics in metrics.items():
                print(f"  {var_name}:")
                print(f"    R²    = {var_metrics['R2']:.4f}")
                print(f"    RMSE  = {var_metrics['RMSE']:.4f}")
                print(f"    PBIAS = {var_metrics['PBIAS']:.2f}%")
                print(f"    N     = {var_metrics['n_valid']}")

        # Save metrics to JSON file(s)
        if n_samples == 1:
            metrics_file = output_path.parent / f"{output_path.stem}_metrics.json"
            with open(metrics_file, 'w') as f:
                json.dump(metrics_all[0], f, indent=2)
            print(f"\n  ✓ Saved metrics to: {metrics_file}")
        else:
            for sample_idx in range(n_samples):
                metrics_file = output_path.parent / f"{output_path.stem}_sample_{sample_idx+1}_metrics.json"
                with open(metrics_file, 'w') as f:
                    json.dump(metrics_all[sample_idx], f, indent=2)
            print(f"\n  ✓ Saved {n_samples} metrics files to: {output_path.parent}")

    # Generate plots if requested
    if args.plot:
        print(f"\n{'='*60}")
        print("GENERATING PLOTS")
        print('='*60)

        if n_samples == 1:
            # Time series plot
            plot_path = output_path.with_suffix('.png')
            plot_predictions(predictions, dates, target_var_names,
                           observations=observations, valid_mask=valid_mask,
                           obs_var_names=obs_var_names, obs_indices=obs_indices,
                           param_set_idx=1, save_path=plot_path)

            # Scatter plot if observations available
            if observations is not None:
                scatter_path = output_path.parent / f"{output_path.stem}_scatter.png"
                plot_scatter(predictions, observations, obs_var_names, obs_indices,
                           metrics_all[0], valid_mask=valid_mask,
                           param_set_idx=1, save_path=scatter_path)
        else:
            # Plot all samples
            for i in range(n_samples):
                # Time series plot
                plot_path = output_path.parent / f"{output_path.stem}_sample_{i+1}.png"
                plot_predictions(predictions[i:i+1], dates, target_var_names,
                               observations=observations, valid_mask=valid_mask,
                               obs_var_names=obs_var_names, obs_indices=obs_indices,
                               param_set_idx=i+1, save_path=plot_path)

                # Scatter plot if observations available
                if observations is not None:
                    scatter_path = output_path.parent / f"{output_path.stem}_sample_{i+1}_scatter.png"
                    plot_scatter(predictions[i:i+1], observations, obs_var_names, obs_indices,
                               metrics_all[i], valid_mask=valid_mask,
                               param_set_idx=i+1, save_path=scatter_path)

    print(f"\n{'='*60}")
    print("✓ INFERENCE COMPLETE")
    print('='*60)
    print(f"\nResults saved to: {output_path.parent}")
    if args.obs:
        print(f"  - Predictions CSV: {output_path.name}")
        print(f"  - Metrics JSON: *_metrics.json")
        if args.plot:
            print(f"  - Time series plots: *.png")
            print(f"  - Scatter plots: *_scatter.png")
    else:
        print(f"  - Predictions CSV: {output_path.name}")
        if args.plot:
            print(f"  - Time series plots: *.png")


if __name__ == '__main__':
    main()
