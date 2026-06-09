"""
Parameter Calibration using LSTM Emulator
Calibrates NoahMP parameters by minimizing error between emulator predictions and observations

This script integrates calibration functionality into the main LSTM_Emulator_4_Noahmp project.
It supports both gradient-based (Adam) and derivative-free (Differential Evolution) optimization.

Key Features:
  - Multiple calibration results (num_calibration) for ensemble predictions and sensitivity analysis
  - Uses observation data from data/obs/<SITE_NAME>_obs_30min.csv
  - Supports both gradient-based and derivative-free optimization methods
  - Saves top N calibration results for uncertainty quantification

Input:
  1. Trained emulator model
  2. Forcing data (NetCDF)
  3. Observation data (CSV with daily resolution)
  4. Parameter bounds

Output:
  - Top N calibrated parameter sets
  - Optimization history
  - Comparison plots for each calibration result
  - Ensemble statistics
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
from scipy.optimize import differential_evolution
from datetime import datetime

from lstm_model_forward import LSTMForwardPredictor, BiLSTMForwardPredictor, AttentionLSTMForwardPredictor
import config_forward_comprehensive as config

# Import HVT > HVB constraint functions
from src.constraints import (
    find_hvt_hvb_pairs,
    check_hvt_hvb_constraint,
    apply_hvt_hvb_constraint_numpy,
    validate_final_params
)

# Import constraint configuration
try:
    from config_forward_comprehensive import HVT_HVB_MARGIN, ENFORCE_HVT_HVB
except ImportError:
    HVT_HVB_MARGIN = 0.5
    ENFORCE_HVT_HVB = True


def set_dropout_to_eval(model):
    """Set all dropout layers to eval mode while keeping model in train mode"""
    for module in model.modules():
        if isinstance(module, torch.nn.Dropout):
            module.eval()


def load_model(model_dir, device='cpu'):
    """Load trained emulator model"""
    model_dir = Path(model_dir)

    # Load config
    with open(model_dir / 'config.json', 'r') as f:
        config_dict = json.load(f)

    # Load data statistics (for normalization)
    data_file = Path(config.OUTPUT_FILE)
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

    model.load_state_dict(torch.load(model_dir / 'best_model.pth', map_location=device))
    model = model.to(device)
    model.eval()

    return model, data_dict, config_dict


def load_forcing_data(forcing_file):
    """Load and process forcing data from NetCDF"""
    print(f"\nLoading forcing data from: {forcing_file}")

    ds = xr.open_dataset(forcing_file)
    times = pd.to_datetime(ds['time'].values)

    # Extract forcing variables
    forcing_data = {'time': times}
    for var_config in config.FORCING_VARIABLES:
        var_name = var_config['name']
        if var_name in ds:
            forcing_data[var_name] = ds[var_name].values

    ds.close()

    # Create DataFrame and aggregate to daily
    forcing_df = pd.DataFrame(forcing_data)
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

    print(f"  Loaded {len(forcing_daily)} days of forcing data")
    print(f"  Date range: {forcing_daily['date'].iloc[0]} to {forcing_daily['date'].iloc[-1]}")

    return forcing_daily


def load_observation_data(obs_file, start_date, end_date):
    """
    Load and process observation data

    Args:
        obs_file: Path to observation CSV file
        start_date: Start date (matching forcing data)
        end_date: End date (matching forcing data)

    Returns:
        DataFrame with daily aggregated observations
    """
    print(f"\nLoading observation data from: {obs_file}")

    # Read observation data
    obs_df = pd.read_csv(obs_file)

    # Parse date - the file already has daily data
    obs_df['date'] = pd.to_datetime(obs_df['date']).dt.date

    # Filter by date range
    start_date_obj = pd.to_datetime(start_date).date()
    end_date_obj = pd.to_datetime(end_date).date()
    obs_df = obs_df[(obs_df['date'] >= start_date_obj) & (obs_df['date'] <= end_date_obj)]

    target_cols = config.MAIN_TARGETS
    available_cols = [col for col in target_cols if col in obs_df.columns]

    if not available_cols:
        raise ValueError(f"No target variables found in observation data. Looking for: {target_cols}")

    print(f"  Loaded {len(obs_df)} days of observations")
    print(f"  Available variables: {available_cols}")
    print(f"  Date range: {obs_df['date'].iloc[0]} to {obs_df['date'].iloc[-1]}")

    return obs_df, available_cols


def predict_with_emulator(model, params, forcing, data_dict, device='cpu'):
    """
    Run emulator prediction

    Args:
        model: Trained emulator model
        params: Parameter array (n_params,) or tensor
        forcing: Forcing array (n_timesteps, n_forcing_vars) or tensor
        data_dict: Normalization statistics
        device: Computing device

    Returns:
        Predictions array (n_timesteps, n_target_vars)
    """
    # Get normalization statistics
    params_stats = data_dict['params_norm_stats']
    forcing_stats = data_dict['forcing_norm_stats']
    targets_stats = data_dict['targets_norm_stats']

    # Convert to tensors if needed
    if isinstance(params, np.ndarray):
        params = params.reshape(1, -1)
        # Normalize parameters using the appropriate method
        if params_stats['method'] == 'z-score':
            params_normalized = (params - params_stats['mean']) / (params_stats['std'] + 1e-8)
        elif params_stats['method'] == 'min-max':
            params_normalized = (params - params_stats['min']) / (params_stats['max'] - params_stats['min'] + 1e-8)
        else:
            raise ValueError(f"Unknown normalization method: {params_stats['method']}")
        params_tensor = torch.FloatTensor(params_normalized).to(device)
    else:
        # Already a tensor (for gradient-based optimization)
        if params.dim() == 1:
            params = params.unsqueeze(0)
        if params_stats['method'] == 'z-score':
            params_mean = torch.FloatTensor(params_stats['mean']).to(device)
            params_std = torch.FloatTensor(params_stats['std']).to(device)
            params_tensor = (params - params_mean) / (params_std + 1e-8)
        elif params_stats['method'] == 'min-max':
            params_min = torch.FloatTensor(params_stats['min']).to(device)
            params_max = torch.FloatTensor(params_stats['max']).to(device)
            params_tensor = (params - params_min) / (params_max - params_min + 1e-8)
        else:
            raise ValueError(f"Unknown normalization method: {params_stats['method']}")

    if isinstance(forcing, np.ndarray):
        forcing = forcing.reshape(1, forcing.shape[0], forcing.shape[1])
        # Normalize forcing using the appropriate method
        if forcing_stats['method'] == 'z-score':
            forcing_normalized = (forcing - forcing_stats['mean']) / (forcing_stats['std'] + 1e-8)
        elif forcing_stats['method'] == 'min-max':
            forcing_normalized = (forcing - forcing_stats['min']) / (forcing_stats['max'] - forcing_stats['min'] + 1e-8)
        else:
            raise ValueError(f"Unknown normalization method: {forcing_stats['method']}")
        forcing_tensor = torch.FloatTensor(forcing_normalized).to(device)
    else:
        # Already a tensor
        if forcing.dim() == 2:
            forcing = forcing.unsqueeze(0)
        if forcing_stats['method'] == 'z-score':
            forcing_mean = torch.FloatTensor(forcing_stats['mean']).to(device)
            forcing_std = torch.FloatTensor(forcing_stats['std']).to(device)
            forcing_tensor = (forcing - forcing_mean) / (forcing_std + 1e-8)
        elif forcing_stats['method'] == 'min-max':
            forcing_min = torch.FloatTensor(forcing_stats['min']).to(device)
            forcing_max = torch.FloatTensor(forcing_stats['max']).to(device)
            forcing_tensor = (forcing - forcing_min) / (forcing_max - forcing_min + 1e-8)
        else:
            raise ValueError(f"Unknown normalization method: {forcing_stats['method']}")

    # Predict
    with torch.no_grad():
        predictions_normalized = model(params_tensor, forcing_tensor)
        predictions_normalized = predictions_normalized.cpu().numpy()

    # Denormalize predictions using the appropriate method
    if targets_stats['method'] == 'z-score':
        predictions = predictions_normalized * targets_stats['std'] + targets_stats['mean']
    elif targets_stats['method'] == 'min-max':
        predictions = predictions_normalized * (targets_stats['max'] - targets_stats['min']) + targets_stats['min']
    else:
        raise ValueError(f"Unknown normalization method: {targets_stats['method']}")

    return predictions[0]  # Remove batch dimension


def calculate_loss(predictions, observations, variable_names, weights=None, loss_type='nrmse'):
    """
    Calculate weighted loss across multiple variables
    
    Supports multiple loss types:
    - 'nrmse': Normalized Root Mean Square Error (default)
    - 'bias': Normalized Absolute Bias (|mean(pred-obs)| / obs_range)
    - 'pbias': Absolute Percent Bias (|100 * sum(pred-obs) / sum(obs)|)
    - 'combined': NRMSE + |NBIAS| (penalizes both variance and systematic bias)

    Args:
        predictions: Array (n_timesteps, n_vars)
        observations: DataFrame with date and variable columns
        variable_names: List of variable names to evaluate
        weights: Optional dict mapping variable names to weights (e.g., {'LH': 1.0, 'HFX': 1.0, 'SOIL_M': 1.0})
                 If None, all variables are weighted equally.
                 Weights are automatically normalized to sum to 1.
        loss_type: Type of loss function ('nrmse', 'bias', 'pbias', 'combined')

    Returns:
        Weighted loss value, var_errors dict
    """
    loss_list = []
    weight_list = []
    var_errors = {}

    for i, var_name in enumerate(variable_names):
        if var_name not in observations.columns:
            continue

        obs_values = observations[var_name].values
        pred_values = predictions[:len(obs_values), i]

        # Remove NaN values
        valid_mask = ~np.isnan(obs_values) & ~np.isnan(pred_values)
        obs_clean = obs_values[valid_mask]
        pred_clean = pred_values[valid_mask]

        if len(obs_clean) == 0:
            continue

        # Calculate basic statistics
        rmse = np.sqrt(np.mean((pred_clean - obs_clean) ** 2))
        bias = np.mean(pred_clean - obs_clean)
        obs_range = np.max(obs_clean) - np.min(obs_clean)
        obs_sum = np.sum(obs_clean)
        
        # Normalized metrics
        nrmse = rmse / (obs_range + 1e-8)
        nbias = np.abs(bias) / (obs_range + 1e-8)  # Normalized absolute bias
        pbias = np.abs(100 * np.sum(pred_clean - obs_clean) / (obs_sum + 1e-8))  # Absolute percent bias
        
        # Store all error metrics for reporting
        var_errors[var_name] = {
            'rmse': rmse, 
            'nrmse': nrmse, 
            'bias': bias,
            'nbias': nbias,
            'pbias': pbias
        }
        
        # Select loss based on loss_type.
        # NOTE: bias / pbias are computed above for diagnostic reporting in
        # var_errors but are NOT permitted as the optimization target because
        # minimizing |mean(pred) - mean(obs)| rewards mean-matching degeneracy
        # (the optimizer can ignore variance / seasonality / dynamics).
        if loss_type == 'nrmse':
            loss_value = nrmse
        elif loss_type == 'combined':
            # Combined loss: NRMSE + NBIAS (equal weighting of both components)
            loss_value = nrmse + nbias
        else:
            raise ValueError(
                f"Unknown loss type: {loss_type}. Supported: 'nrmse', 'combined'. "
                f"'bias' and 'pbias' are diagnostic-only metrics, not valid "
                f"optimization targets."
            )

        loss_list.append(loss_value)
        
        # Get weight for this variable (default to 1.0 if not specified)
        if weights is not None and var_name in weights:
            weight_list.append(weights[var_name])
        else:
            weight_list.append(1.0)

    # Calculate weighted average
    if len(loss_list) == 0:
        return 0.0, var_errors
    
    loss_array = np.array(loss_list)
    weight_array = np.array(weight_list)
    
    # Normalize weights to sum to 1
    weight_array = weight_array / np.sum(weight_array)
    
    weighted_loss = np.sum(loss_array * weight_array)
    
    return weighted_loss, var_errors


# Keep backward compatibility alias
def calculate_normalized_rmse(predictions, observations, variable_names, weights=None):
    """Backward compatibility wrapper for calculate_loss with NRMSE"""
    return calculate_loss(predictions, observations, variable_names, weights, loss_type='nrmse')


def load_parameter_bounds(bounds_file, param_names):
    """
    Load parameter bounds from CSV file
    
    Note: For SATDK parameters, uses 'SATDK(log)' bounds since the emulator
    works with log-transformed values.

    Args:
        bounds_file: Path to bounds CSV
        param_names: List of parameter names

    Returns:
        bounds array (n_params, 2)
    """
    bounds_df = pd.read_csv(bounds_file)

    # Parameters that need log bounds (emulator uses log-transformed values)
    LOG_BOUND_PARAMS = ['SATDK']

    bounds = []
    for param_name in param_names:
        # Remove suffix (_EBF, _CL, _SCL) to match bounds file
        base_name = param_name.split('_')[0]
        
        # For log-transformed parameters, use the (log) bounds
        if base_name in LOG_BOUND_PARAMS:
            log_name = f"{base_name}(log)"
            if log_name in bounds_df['variable'].values:
                row = bounds_df[bounds_df['variable'] == log_name].iloc[0]
                bounds.append([row['Lower bound'], row['Upper bound']])
                print(f"  {param_name}: using log bounds [{row['Lower bound']}, {row['Upper bound']}]")
            else:
                print(f"  Warning: No log bounds for {param_name}, using original bounds")
                if base_name in bounds_df['variable'].values:
                    row = bounds_df[bounds_df['variable'] == base_name].iloc[0]
                    bounds.append([row['Lower bound'], row['Upper bound']])
                else:
                    bounds.append([-6.0, -2.0])  # Default log bounds for SATDK
        elif base_name in bounds_df['variable'].values:
            row = bounds_df[bounds_df['variable'] == base_name].iloc[0]
            bounds.append([row['Lower bound'], row['Upper bound']])
        else:
            # Use wide bounds if not specified
            print(f"  Warning: No bounds for {param_name}, using wide range")
            bounds.append([0.01, 10.0])

    return np.array(bounds)


def load_default_parameters(default_param_file, param_names):
    """
    Load default Noah-MP parameters from file

    Args:
        default_param_file: Path to default parameter file
        param_names: List of parameter names to extract

    Returns:
        Array of default parameter values matching param_names order
    """
    try:
        with open(default_param_file, 'r') as f:
            lines = f.readlines()

        # Parse parameter names and values
        names_line = lines[0].strip().split()
        values_line = lines[1].strip().split()

        # Create dictionary mapping parameter names to values
        default_dict = {}
        for name, value in zip(names_line, values_line):
            try:
                default_dict[name] = float(value)
            except ValueError:
                # Handle scientific notation like 9.74E-7
                default_dict[name] = float(value.replace('E', 'e'))

        # Extract values in the order of param_names
        default_params = []
        for param_name in param_names:
            if param_name in default_dict:
                default_params.append(default_dict[param_name])
            else:
                # Parameter not found, will be handled by caller
                default_params.append(None)

        return np.array(default_params)

    except Exception as e:
        print(f"  Warning: Could not load default parameters from {default_param_file}: {e}")
        print(f"  HINT: Copy data/raw/param/default_param_template.txt to "
              f"data/raw/param/default_param.txt and edit values for your "
              f"site's vegetation/soil class. Until then, calibration will fall "
              f"back to the training-data mean (z-score only) or bounds midpoint.")
        return None


def run_calibration_de(model_dir, forcing_file, obs_file, bounds_file,
                      calibrate_params=None, output_dir='calibration_results',
                      max_iterations=100, popsize=15, num_calibration=5, 
                      weights=None, loss_type='nrmse',
                      enforce_hvt_hvb=True, hvt_hvb_margin=0.5, device='cpu'):
    """
    Run multiple independent parameter calibrations using Differential Evolution
    Each calibration uses a different random seed for diverse parameter exploration

    This provides proper uncertainty quantification through multiple independent runs,
    not just different evaluations from the same optimization run.

    Args:
        model_dir: Directory with trained model
        forcing_file: NetCDF file with forcing data
        obs_file: CSV file with observations
        bounds_file: CSV file with parameter bounds
        calibrate_params: List of parameter names to calibrate (None = all)
        output_dir: Output directory
        max_iterations: Maximum optimization iterations
        popsize: Population size for differential evolution
        num_calibration: Number of independent calibration runs to perform
        weights: Optional dict mapping target variable names to weights 
                 (e.g., {'LH': 1.0, 'HFX': 2.0, 'SOIL_M': 1.5})
                 If None, all variables are weighted equally.
        loss_type: Loss function type ('nrmse' or 'combined')
        enforce_hvt_hvb: Whether to enforce HVT > HVB constraint
        hvt_hvb_margin: Minimum margin for HVT > HVB constraint (meters)
        device: Computing device
    """
    # Loss type display names
    loss_type_names = {
        'nrmse': 'Normalized RMSE',
        'combined': 'NRMSE + NBIAS (Combined)'
    }
    
    print("="*80)
    print("PARAMETER CALIBRATION WITH DIFFERENTIAL EVOLUTION")
    print(f"Running {num_calibration} INDEPENDENT calibration runs for uncertainty quantification")
    print(f"Loss function: {loss_type_names.get(loss_type, loss_type)}")
    if enforce_hvt_hvb:
        print(f"HVT > HVB constraint: ENABLED (margin={hvt_hvb_margin}m)")
    else:
        print("HVT > HVB constraint: DISABLED")
    print("="*80)

    # Load model
    print("\n[1/6] Loading emulator model...")
    model, data_dict, config_dict = load_model(model_dir, device)
    print(f"  Model: {config_dict['model_type']}")

    # Load forcing data
    print("\n[2/6] Loading forcing data...")
    forcing_df = load_forcing_data(forcing_file)
    forcing_array = forcing_df[data_dict['forcing_var_names']].values

    # Get date range from forcing
    start_date = forcing_df['date'].iloc[0]
    end_date = forcing_df['date'].iloc[-1]

    # Load observation data
    print("\n[3/6] Loading observation data...")
    obs_data, available_vars = load_observation_data(obs_file, start_date, end_date)

    # Setup parameters to calibrate
    print("\n[4/6] Setting up calibration parameters...")
    param_names = data_dict['param_names']

    # Get fixed parameters from config
    fixed_params = config.get_fixed_parameters()
    
    if calibrate_params is None:
        # Start with all parameters, then exclude fixed ones
        calibrate_params = config.get_calibration_param_names(param_names)
        calibrate_indices = [i for i, name in enumerate(param_names) if name in calibrate_params]
    else:
        # User specified params, but still exclude fixed ones
        calibrate_params = [p for p in calibrate_params if p not in fixed_params]
        calibrate_indices = [i for i, name in enumerate(param_names) if name in calibrate_params]
        calibrate_params = [param_names[i] for i in calibrate_indices]

    print(f"  Total parameters: {len(param_names)}")
    print(f"  Calibrating: {len(calibrate_params)} parameters")

    # Display HVT/HVB pairs if constraint is enabled
    if enforce_hvt_hvb:
        pairs = find_hvt_hvb_pairs(param_names)
        if pairs:
            print(f"  Found {len(pairs)} HVT/HVB parameter pairs for constraint enforcement")
        else:
            print("  Warning: No HVT/HVB parameter pairs found")
    if fixed_params:
        print(f"  Fixed parameters (excluded from calibration):")
        for param_name, value in fixed_params.items():
            if param_name in param_names:
                print(f"    - {param_name} = {value}")

    # Display weight configuration (only for variables present in observations)
    target_var_names = data_dict['target_var_names']
    obs_vars = [v for v in target_var_names if v in available_vars]
    if weights is not None:
        print(f"\n  Calibration loss weights (variables matched with observations):")
        total_weight = sum(weights.get(var, 1.0) for var in obs_vars)
        for var in obs_vars:
            w = weights.get(var, 1.0)
            normalized_w = w / total_weight
            print(f"    {var}: {w:.2f} (normalized: {normalized_w:.2%})")
        unmatched = [v for v in target_var_names if v not in available_vars]
        if unmatched:
            print(f"  ({len(unmatched)} emulated variables not in observations, excluded from calibration loss)")
    else:
        print(f"\n  Calibration loss weights: Equal weighting (1.0 each)")
        print(f"  Variables matched with observations: {obs_vars}")

    # Load parameter bounds
    bounds = load_parameter_bounds(bounds_file, param_names)
    calibrate_bounds = bounds[calibrate_indices]

    # Get baseline parameters
    # Priority: 1) Default Noah-MP params, 2) Training data mean (z-score only), 3) Midpoint of bounds
    default_param_file = Path('data/raw/param/default_param.txt')
    default_params = load_default_parameters(default_param_file, param_names)

    params_stats = data_dict['params_norm_stats']

    if default_params is not None and not np.any(default_params == None):
        # First priority: Use scientifically validated default Noah-MP parameters
        baseline_params = default_params
        print(f"  Using Noah-MP default parameters as baseline")
    elif params_stats['method'] == 'z-score' and 'mean' in params_stats:
        # Second priority: Use mean from training data (only valid for z-score normalization)
        baseline_params = np.array(params_stats['mean']).flatten().copy()
        print(f"  Using training data mean as baseline (z-score normalization)")
    else:
        # Fallback: Use midpoint of bounds (appropriate for min-max and when mean not available)
        baseline_params = np.array([(lower + upper) / 2 for lower, upper in bounds])
        print(f"  Using midpoint of bounds as baseline")

    # Apply fixed parameter values to baseline
    if fixed_params:
        baseline_params = config.apply_fixed_parameters(baseline_params, param_names)
        print(f"  Applied {len(fixed_params)} fixed parameter value(s) to baseline")

    # Run multiple independent calibrations
    print(f"\n[5/6] Running {num_calibration} independent calibration runs...")
    print(f"  Max iterations per run: {max_iterations}")
    print(f"  Population size: {popsize}")

    all_calibration_results = []

    for run_idx in range(num_calibration):
        print(f"\n{'='*60}")
        print(f"Calibration Run {run_idx + 1}/{num_calibration} (seed={42 + run_idx})")
        print(f"{'='*60}")

        n_evals = [0]
        
        # Loss type display for progress messages
        loss_display = loss_type.upper()

        def objective(calibrate_param_values):
            """Objective function for optimization"""
            n_evals[0] += 1

            # Create full parameter set
            full_params = baseline_params.copy()
            full_params[calibrate_indices] = calibrate_param_values

            # Apply HVT > HVB constraint if enabled
            if enforce_hvt_hvb:
                full_params, _ = apply_hvt_hvb_constraint_numpy(
                    full_params, param_names,
                    bounds[:, 0], bounds[:, 1],
                    margin=hvt_hvb_margin, verbose=False
                )

            # Run emulator
            predictions = predict_with_emulator(model, full_params, forcing_array, data_dict, device)

            # Calculate weighted loss
            error, var_errors = calculate_loss(predictions, obs_data, data_dict['target_var_names'], weights, loss_type)

            if n_evals[0] % 20 == 0:
                print(f"  Eval {n_evals[0]}: Weighted {loss_display} = {error:.6f}")

            return error

        # Run optimization with different seed for each run
        result = differential_evolution(
            objective,
            calibrate_bounds,
            maxiter=max_iterations,
            popsize=popsize,
            strategy='best1bin',
            seed=42 + run_idx,  # Different seed for each run
            disp=False,
            polish=True
        )

        # Store result from this independent run
        full_params = baseline_params.copy()
        full_params[calibrate_indices] = result.x

        # Apply HVT > HVB constraint to final result
        if enforce_hvt_hvb:
            full_params, n_corrections = apply_hvt_hvb_constraint_numpy(
                full_params, param_names,
                bounds[:, 0], bounds[:, 1],
                margin=hvt_hvb_margin, verbose=False
            )
            if n_corrections > 0:
                print(f"  Applied {n_corrections} HVT > HVB correction(s) to final result")
            # Validate final params
            if not validate_final_params(full_params, param_names, hvt_hvb_margin):
                print(f"  WARNING: Final params still violate HVT > HVB constraint!")

        # Get final predictions
        predictions = predict_with_emulator(model, full_params, forcing_array, data_dict, device)
        error, var_errors = calculate_loss(predictions, obs_data, data_dict['target_var_names'], weights, loss_type)

        all_calibration_results.append({
            'run_id': run_idx + 1,
            'seed': 42 + run_idx,
            'params': full_params.copy(),
            'calibrate_params': result.x.copy(),
            'error': error,
            'var_errors': var_errors,
            'n_iterations': result.nit,
            'n_evaluations': result.nfev,
            'success': result.success,
            'loss_type': loss_type
        })

        print(f"\nRun {run_idx + 1} Complete:")
        print(f"  Final {loss_display}: {error:.6f}")
        print(f"  Iterations: {result.nit}")
        print(f"  Evaluations: {result.nfev}")
        print(f"  Success: {result.success}")

    # Sort results by error
    print(f"\n[6/6] Analyzing {num_calibration} independent calibration results...")
    top_results = sorted(all_calibration_results, key=lambda x: x['error'])

    print(f"\nCalibration results summary (Loss: {loss_type.upper()}):")
    errors = [r['error'] for r in top_results]
    print(f"  Best:  {min(errors):.6f}")
    print(f"  Worst: {max(errors):.6f}")
    print(f"  Mean:  {np.mean(errors):.6f}")
    print(f"  Std NRMSE:   {np.std(errors):.6f}")

    print(f"\nIndividual results:")
    for i, res in enumerate(top_results, 1):
        print(f"  #{i} (Run {res['run_id']}): NRMSE = {res['error']:.6f}")

    # Save results
    print("\n" + "="*80)
    print("CALIBRATION COMPLETE")
    print("="*80)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # Save all top calibration results
    for i, res in enumerate(top_results):
        result_dir = output_path / f'calibration_{i+1}'
        result_dir.mkdir(exist_ok=True)

        # Save calibrated parameters
        calib_params_df = pd.DataFrame({
            'parameter': param_names,
            'baseline': baseline_params,
            'calibrated': res['params'],
            'is_calibrated': [
                'Yes' if j in calibrate_indices 
                else ('Fixed' if param_names[j] in fixed_params else 'No') 
                for j in range(len(param_names))
            ]
        })
        calib_params_df.to_csv(result_dir / 'calibrated_parameters.csv', index=False)

        # Save detailed result
        result_dict = {
            'rank': i + 1,
            'final_error': float(res['error']),
            'variable_errors': {k: {'rmse': float(v['rmse']), 'nrmse': float(v['nrmse'])}
                              for k, v in res['var_errors'].items()},
            'calibrated_params': calibrate_params,
            'calibrated_values': res['calibrate_params'].tolist()
        }

        with open(result_dir / 'calibration_result.json', 'w') as f:
            json.dump(result_dict, f, indent=2)

        # Generate predictions and save
        final_predictions = predict_with_emulator(
            model, res['params'], forcing_array, data_dict, device
        )

        pred_df = pd.DataFrame({'date': forcing_df['date'].values})
        for j, var_name in enumerate(data_dict['target_var_names']):
            pred_df[f'{var_name}_pred'] = final_predictions[:, j]
            if var_name in obs_data.columns:
                pred_df[f'{var_name}_obs'] = obs_data[var_name].values

        pred_df.to_csv(result_dir / 'predictions_comparison.csv', index=False)

        # Plot results
        plot_calibration_results(
            pred_df, data_dict['target_var_names'], available_vars,
            result_dir / 'calibration_results.png', rank=i+1, error=res['error']
        )

    # Save ensemble summary
    ensemble_summary = {
        'num_calibrations': num_calibration,
        'optimization_method': 'differential_evolution_multiple_runs',
        'max_iterations': max_iterations,
        'popsize': popsize,
        'statistics': {
            'nrmse_mean': float(np.mean(errors)),
            'nrmse_std': float(np.std(errors)),
            'nrmse_min': float(np.min(errors)),
            'nrmse_max': float(np.max(errors))
        },
        'results': [
            {
                'rank': i+1,
                'run_id': res['run_id'],
                'seed': res['seed'],
                'nrmse': float(res['error']),
                'n_iterations': res['n_iterations'],
                'n_evaluations': res['n_evaluations'],
                'success': res['success'],
                'variable_errors': {k: float(v['nrmse']) for k, v in res['var_errors'].items()}
            }
            for i, res in enumerate(top_results)
        ]
    }

    with open(output_path / 'ensemble_summary.json', 'w') as f:
        json.dump(ensemble_summary, f, indent=2)

    # Calculate and save parameter statistics
    param_statistics = calculate_parameter_statistics(
        top_results, calibrate_indices, param_names, baseline_params
    )

    with open(output_path / 'parameter_statistics.json', 'w') as f:
        json.dump(param_statistics, f, indent=2)

    # Create parameter distribution plots
    plot_parameter_distributions(
        top_results, calibrate_indices, param_names,
        output_path / 'parameter_distributions.png'
    )

    plot_parameter_correlation(
        top_results, calibrate_indices, param_names,
        output_path / 'parameter_correlations.png'
    )

    print(f"\nResults saved to: {output_path}")
    print(f"Generated {num_calibration} independent calibration results")
    print(f"Parameter distribution plots created")

    return top_results, ensemble_summary


def calculate_parameter_statistics(results, calibrate_indices, param_names, baseline_params):
    """
    Calculate statistics for calibrated parameters across multiple runs

    Args:
        results: List of calibration results
        calibrate_indices: Indices of calibrated parameters
        param_names: All parameter names
        baseline_params: Baseline parameter values

    Returns:
        Dictionary with parameter statistics
    """
    # Extract calibrated parameters from all runs
    n_runs = len(results)
    n_params = len(calibrate_indices)

    param_values = np.zeros((n_runs, n_params))
    for i, res in enumerate(results):
        param_values[i, :] = res['calibrate_params']

    # Calculate statistics
    statistics = {}
    for i, param_idx in enumerate(calibrate_indices):
        param_name = param_names[param_idx]
        values = param_values[:, i]

        statistics[param_name] = {
            'baseline': float(baseline_params[param_idx]),
            'mean': float(np.mean(values)),
            'std': float(np.std(values)),
            'min': float(np.min(values)),
            'max': float(np.max(values)),
            'median': float(np.median(values)),
            'cv': float(np.std(values) / np.mean(values)) if np.mean(values) != 0 else 0.0,
            'all_values': values.tolist()
        }

    return {
        'num_calibrations': n_runs,
        'num_parameters': n_params,
        'parameter_names': [param_names[i] for i in calibrate_indices],
        'parameters': statistics
    }


def plot_parameter_distributions(results, calibrate_indices, param_names, save_path):
    """
    Plot distributions of calibrated parameters across multiple runs

    Creates violin plots and histograms showing parameter uncertainty
    """
    import matplotlib.pyplot as plt
    from matplotlib.gridspec import GridSpec

    # Extract parameter values
    n_params = len(calibrate_indices)
    param_data = []
    param_labels = []

    for i, param_idx in enumerate(calibrate_indices):
        param_name = param_names[param_idx]
        values = [res['calibrate_params'][i] for res in results]
        param_data.append(values)
        param_labels.append(param_name)

    # Create figure with subplots
    n_cols = min(3, n_params)
    n_rows = (n_params + n_cols - 1) // n_cols

    fig = plt.figure(figsize=(6 * n_cols, 4 * n_rows))
    gs = GridSpec(n_rows, n_cols, figure=fig, hspace=0.4, wspace=0.3)

    for i in range(n_params):
        row = i // n_cols
        col = i % n_cols
        ax = fig.add_subplot(gs[row, col])

        values = param_data[i]
        param_name = param_labels[i]

        # Create histogram with KDE overlay
        ax.hist(values, bins=min(20, len(values)), density=True, alpha=0.6, color='skyblue', edgecolor='black')

        # Add vertical lines for mean and median
        mean_val = np.mean(values)
        median_val = np.median(values)
        ax.axvline(mean_val, color='red', linestyle='--', linewidth=2, label=f'Mean: {mean_val:.4f}')
        ax.axvline(median_val, color='green', linestyle=':', linewidth=2, label=f'Median: {median_val:.4f}')

        # Calculate and display statistics
        std_val = np.std(values)
        cv_val = std_val / mean_val if mean_val != 0 else 0

        ax.set_xlabel('Parameter Value', fontsize=10)
        ax.set_ylabel('Density', fontsize=10)
        ax.set_title(f'{param_name}\n(CV={cv_val:.2%}, σ={std_val:.4f})', fontsize=11, fontweight='bold')
        ax.legend(fontsize=8)
        ax.grid(True, alpha=0.3)

    plt.suptitle(f'Parameter Uncertainty Distribution ({len(results)} calibration runs)',
                fontsize=14, fontweight='bold', y=0.995)

    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    print(f"Parameter distribution plot saved to: {save_path}")
    plt.close()


def plot_parameter_correlation(results, calibrate_indices, param_names, save_path):
    """
    Plot correlation matrix of calibrated parameters

    Shows which parameters tend to vary together across calibration runs
    """
    import matplotlib.pyplot as plt

    # Extract parameter values
    n_params = len(calibrate_indices)
    n_runs = len(results)

    param_matrix = np.zeros((n_runs, n_params))
    for i, res in enumerate(results):
        param_matrix[i, :] = res['calibrate_params']

    # Calculate correlation matrix
    corr_matrix = np.corrcoef(param_matrix.T)

    # Create plot
    fig, ax = plt.subplots(figsize=(10, 8))

    im = ax.imshow(corr_matrix, cmap='RdBu_r', vmin=-1, vmax=1, aspect='auto')

    # Set ticks and labels
    param_labels = [param_names[i] for i in calibrate_indices]
    ax.set_xticks(np.arange(n_params))
    ax.set_yticks(np.arange(n_params))
    ax.set_xticklabels(param_labels, rotation=45, ha='right', fontsize=9)
    ax.set_yticklabels(param_labels, fontsize=9)

    # Add correlation values as text
    for i in range(n_params):
        for j in range(n_params):
            text = ax.text(j, i, f'{corr_matrix[i, j]:.2f}',
                          ha="center", va="center", color="black" if abs(corr_matrix[i, j]) < 0.5 else "white",
                          fontsize=8)

    # Add colorbar
    cbar = plt.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label('Correlation Coefficient', fontsize=10)

    ax.set_title(f'Parameter Correlation Matrix ({len(results)} calibration runs)',
                fontsize=12, fontweight='bold', pad=20)

    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    print(f"Parameter correlation plot saved to: {save_path}")
    plt.close()


def plot_calibration_results(pred_df, target_var_names, available_vars, save_path, rank=1, error=0.0):
    """Plot comparison of predictions vs observations"""
    # Only plot available observed variables
    plot_vars = [v for v in available_vars if v in target_var_names]

    if not plot_vars:
        print(f"  Warning: No observed variables to plot")
        return

    n_vars = len(plot_vars)
    fig, axes = plt.subplots(n_vars, 1, figsize=(14, 4 * n_vars))

    if n_vars == 1:
        axes = [axes]

    for i, var_name in enumerate(plot_vars):
        ax = axes[i]

        dates = pd.to_datetime(pred_df['date'])

        # Plot predictions
        ax.plot(dates, pred_df[f'{var_name}_pred'],
               label='Calibrated Emulator', linewidth=2, alpha=0.8)

        # Plot observations if available
        if f'{var_name}_obs' in pred_df.columns:
            obs_values = pred_df[f'{var_name}_obs'].values
            valid_mask = ~np.isnan(obs_values)
            ax.scatter(dates[valid_mask], obs_values[valid_mask],
                      label='Observations', alpha=0.6, s=20, color='red')

            # Calculate R2
            pred_values = pred_df[f'{var_name}_pred'].values[valid_mask]
            obs_clean = obs_values[valid_mask]
            ss_res = np.sum((obs_clean - pred_values) ** 2)
            ss_tot = np.sum((obs_clean - np.mean(obs_clean)) ** 2)
            r2 = 1 - (ss_res / ss_tot)

            # Calculate RMSE
            rmse = np.sqrt(np.mean((obs_clean - pred_values) ** 2))

            ax.text(0.02, 0.98, f'R² = {r2:.3f}\nRMSE = {rmse:.3f}',
                   transform=ax.transAxes, fontsize=11,
                   verticalalignment='top', bbox=dict(boxstyle='round',
                   facecolor='white', alpha=0.8))

        ax.set_xlabel('Date', fontsize=11)
        ax.set_ylabel(var_name, fontsize=11)
        ax.set_title(f'{var_name} - Calibrated vs Observed', fontsize=12)
        ax.legend()
        ax.grid(True, alpha=0.3)
        plt.setp(ax.xaxis.get_majorticklabels(), rotation=45, ha='right')

    plt.suptitle(f'Calibration Result #{rank} (NRMSE = {error:.4f})',
                fontsize=14, fontweight='bold')
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close()


def main():
    parser = argparse.ArgumentParser(
        description='Calibrate NoahMP parameters using LSTM emulator',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Example usage:

  # Run 5 independent calibrations for uncertainty quantification
  python 05_calibration_applying_emulator_multiple_runs.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_171820_dim-512_layer-2 \\
    --forcing data/raw/forcing/forcing_sample_1.nc \\
    --obs data/obs/MySite_obs_30min.csv \\
    --bounds value_bounds.csv \\
    --num_calibration 5 \\
    --output calibration_results

  # Run with custom weights for target variables (LH=1.0, HFX=2.0, SOIL_M=1.5)
  # This gives HFX twice the importance of LH in the loss function
  python 05_calibration_applying_emulator_multiple_runs.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_171820_dim-512_layer-2 \\
    --forcing data/raw/forcing/forcing_sample_1.nc \\
    --obs data/obs/MySite_obs_30min.csv \\
    --bounds value_bounds.csv \\
    --weights 1.0 2.0 1.5 --weight_vars LH HFX SOIL_M \\
    --num_calibration 5

  # Use combined loss (NRMSE + NBIAS) with custom weights
  python 05_calibration_applying_emulator_multiple_runs.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_171820_dim-512_layer-2 \\
    --forcing data/raw/forcing/forcing_sample_1.nc \\
    --obs data/obs/MySite_obs_30min.csv \\
    --bounds value_bounds.csv \\
    --loss_type combined \\
    --weights 1.0 2.0 1.5 --weight_vars LH HFX SOIL_M \\
    --num_calibration 5

  # Run 20 independent calibrations for robust parameter distribution
  python 05_calibration_applying_emulator_multiple_runs.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_171820_dim-512_layer-2 \\
    --forcing data/raw/forcing/forcing_sample_1.nc \\
    --obs data/obs/MySite_obs_30min.csv \\
    --bounds value_bounds.csv \\
    --num_calibration 20 \\
    --max_iter 100

  # Calibrate specific parameters with 10 independent runs
  python 05_calibration_applying_emulator_multiple_runs.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_171820_dim-512_layer-2 \\
    --forcing data/raw/forcing/forcing_sample_1.nc \\
    --obs data/obs/MySite_obs_30min.csv \\
    --bounds value_bounds.csv \\
    --calibrate_params VCMX25 HVT SATDK \\
    --num_calibration 10 \\
    --max_iter 50
        """
    )

    parser.add_argument('--model_dir', type=str, required=True,
                       help='Directory containing trained emulator model')
    parser.add_argument('--forcing', type=str, required=True,
                       help='NetCDF file with forcing data')
    parser.add_argument('--obs', type=str, required=True,
                       help='CSV file with observation data')
    parser.add_argument('--bounds', type=str, required=True,
                       help='CSV file with parameter bounds')
    parser.add_argument('--calibrate_params', type=str, nargs='+', default=None,
                       help='List of parameter names to calibrate (default: all)')
    parser.add_argument('--num_calibration', type=int, default=5,
                       help='Number of independent calibration runs (each with different random seed) for uncertainty quantification (default: 5)')
    parser.add_argument('--output', type=str, default='calibration_results',
                       help='Output directory')
    parser.add_argument('--max_iter', type=int, default=100,
                       help='Maximum optimization iterations')
    parser.add_argument('--popsize', type=int, default=15,
                       help='Population size for differential evolution')
    parser.add_argument('--weights', type=float, nargs='+', default=None,
                       help='Weights for target variables in loss calculation, '
                            'paired 1:1 with --weight_vars. '
                            'Example: --weights 1.0 2.0 1.5 --weight_vars LH HFX SOIL_M. '
                            'Weights are automatically normalized to sum to 1. '
                            'Default: equal weights (1.0) for every variable in '
                            'config.MAIN_TARGETS.')
    parser.add_argument('--weight_vars', type=str, nargs='+', default=None,
                       help='Variable names that --weights apply to. Must have '
                            'the same length as --weights.')
    parser.add_argument('--loss_type', type=str, default='nrmse',
                       choices=['nrmse', 'combined'],
                       help='Loss function type for optimization. '
                            'nrmse: Normalized RMSE (default), '
                            'combined: NRMSE + NBIAS '
                            '(bias/pbias are reported as diagnostics but cannot '
                            'be selected as the optimization target -- they '
                            'reward mean-matching degeneracy).')

    # HVT > HVB constraint arguments
    parser.add_argument('--no-hvt-hvb-constraint', action='store_true',
                       help='Disable HVT > HVB physical constraint enforcement')
    parser.add_argument('--hvt-hvb-margin', type=float, default=HVT_HVB_MARGIN,
                       help=f'Minimum margin for HVT > HVB constraint in meters (default: {HVT_HVB_MARGIN})')

    args = parser.parse_args()

    # Device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}\n")

    # Parse weights into dictionary, pairing --weights with --weight_vars
    weights_dict = None
    if args.weights is not None or args.weight_vars is not None:
        if args.weights is None or args.weight_vars is None:
            parser.error("--weights and --weight_vars must be provided together")
        if len(args.weights) != len(args.weight_vars):
            parser.error(
                f"--weights ({len(args.weights)} values) and --weight_vars "
                f"({len(args.weight_vars)} names) must have the same length"
            )
        weights_dict = dict(zip(args.weight_vars, args.weights))
        print(f"Using custom loss weights: {weights_dict}")
    else:
        # Default: 1.0 for every MAIN_TARGETS variable
        weights_dict = {v: 1.0 for v in config.MAIN_TARGETS}
        print(f"Using default loss weights (1.0 for each main target): {weights_dict}")

    # Display loss type (bias / pbias kept for diagnostic reporting only)
    loss_type_names = {
        'nrmse': 'Normalized RMSE',
        'combined': 'NRMSE + NBIAS (Combined)'
    }
    print(f"Using loss function: {loss_type_names.get(args.loss_type, args.loss_type)}")

    # Determine constraint settings
    enforce_hvt_hvb = ENFORCE_HVT_HVB and not args.no_hvt_hvb_constraint
    if enforce_hvt_hvb:
        print(f"HVT > HVB constraint: ENABLED (margin={args.hvt_hvb_margin}m)")
    else:
        print("HVT > HVB constraint: DISABLED")
    print()

    # Run calibration
    run_calibration_de(
        model_dir=args.model_dir,
        forcing_file=args.forcing,
        obs_file=args.obs,
        bounds_file=args.bounds,
        calibrate_params=args.calibrate_params,
        output_dir=args.output,
        max_iterations=args.max_iter,
        popsize=args.popsize,
        num_calibration=args.num_calibration,
        weights=weights_dict,
        loss_type=args.loss_type,
        enforce_hvt_hvb=enforce_hvt_hvb,
        hvt_hvb_margin=args.hvt_hvb_margin,
        device=device
    )


if __name__ == '__main__':
    main()
