"""
Validation script for comprehensive FORWARD LSTM model
Generate time series comparison plots for specific test samples
"""

import torch
import numpy as np
import pickle
import json
import matplotlib.pyplot as plt
from pathlib import Path
import argparse

from lstm_model_forward import LSTMForwardPredictor, BiLSTMForwardPredictor, AttentionLSTMForwardPredictor
import config_forward_comprehensive as config


def load_model(model_dir, device='cpu'):
    """
    Load trained model from directory

    Args:
        model_dir: Directory containing model files
        device: Device to load model on

    Returns:
        model, config_dict
    """
    model_dir = Path(model_dir)

    # Load config
    with open(model_dir / 'config.json', 'r') as f:
        config_dict = json.load(f)

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

    return model, config_dict


def load_data_and_indices(model_dir, data_file):
    """
    Load dataset and train/val split indices

    Args:
        model_dir: Directory containing train_val_indices.npz
        data_file: Path to processed data pickle file

    Returns:
        data_dict, train_indices, val_indices
    """
    # Load data
    with open(data_file, 'rb') as f:
        data_dict = pickle.load(f)

    # Load indices
    model_dir = Path(model_dir)
    indices_file = model_dir / 'train_val_test_indices.npz'

    if not indices_file.exists():
        raise FileNotFoundError(
            f"Train/val indices file not found: {indices_file}\n"
            f"This file should be created during training. "
            f"Please retrain the model with the updated training script."
        )

    indices_data = np.load(indices_file)
    train_indices = indices_data['train_indices']
    val_indices = indices_data['val_indices']

    return data_dict, train_indices, val_indices


def predict_samples(model, data_dict, sample_indices, device='cpu'):
    """
    Generate predictions for specific samples

    Args:
        model: Trained model
        data_dict: Dictionary with data
        sample_indices: List of sample indices in the full dataset
        device: Device to run prediction on

    Returns:
        predictions, true_values (both denormalized)
    """
    # Get sample data
    forcing = data_dict['X_forcing'][sample_indices]  # (n_samples, n_timesteps, n_forcing_vars)
    params = data_dict['X_params'][sample_indices]    # (n_samples, n_params)
    true_values = data_dict['y'][sample_indices]      # (n_samples, n_timesteps, n_target_vars)

    # Convert to tensors
    params_tensor = torch.FloatTensor(params).to(device)
    forcing_tensor = torch.FloatTensor(forcing).to(device)

    # Predict
    with torch.no_grad():
        predictions_normalized = model(params_tensor, forcing_tensor)
        predictions_normalized = predictions_normalized.cpu().numpy()

    # Denormalize predictions using the appropriate method
    targets_stats = data_dict['targets_norm_stats']
    if targets_stats['method'] == 'z-score':
        y_mean = targets_stats['mean']
        y_std = targets_stats['std']
        predictions = predictions_normalized * y_std + y_mean
        true_values_denorm = true_values * y_std + y_mean
    elif targets_stats['method'] == 'min-max':
        y_min = targets_stats['min']
        y_max = targets_stats['max']
        predictions = predictions_normalized * (y_max - y_min) + y_min
        true_values_denorm = true_values * (y_max - y_min) + y_min
    else:
        raise ValueError(f"Unknown normalization method: {targets_stats['method']}")

    return predictions, true_values_denorm


def plot_time_series_for_sample(predictions, true_values, var_names, sample_idx,
                                  dataset_idx, save_dir, main_vars=['SOIL_M', 'LH', 'HFX']):
    """
    Plot time series comparison for all variables for a single sample

    Args:
        predictions: Predictions for this sample (n_timesteps, n_vars)
        true_values: True values for this sample (n_timesteps, n_vars)
        var_names: List of variable names
        sample_idx: Sample index in validation set (0-based)
        dataset_idx: Sample index in full dataset (for labeling)
        save_dir: Directory to save plots
        main_vars: Main target variables to highlight
    """
    n_vars = len(var_names)

    # Compute metrics for each variable
    metrics = {}
    for i, var_name in enumerate(var_names):
        pred_var = predictions[:, i]
        true_var = true_values[:, i]

        ss_res = np.sum((true_var - pred_var) ** 2)
        ss_tot = np.sum((true_var - true_var.mean()) ** 2)
        r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0.0
        rmse = np.sqrt(np.mean((true_var - pred_var) ** 2))

        metrics[var_name] = {'R2': r2, 'RMSE': rmse}

    # Plot main variables (larger)
    main_vars_present = [v for v in main_vars if v in var_names]
    if main_vars_present:
        n_main = len(main_vars_present)
        fig, axes = plt.subplots(n_main, 1, figsize=(12, 4*n_main))
        if n_main == 1:
            axes = [axes]

        for idx, var_name in enumerate(main_vars_present):
            i = var_names.index(var_name)
            ax = axes[idx]

            timesteps = np.arange(predictions.shape[0])
            ax.plot(timesteps, true_values[:, i], label='True', linewidth=2, alpha=0.7, color='blue')
            ax.plot(timesteps, predictions[:, i], label='Predicted', linewidth=2, alpha=0.7,
                   color='red', linestyle='--')

            m = metrics[var_name]
            ax.set_xlabel('Time Step (days)', fontsize=11)
            ax.set_ylabel(var_name, fontsize=11)
            ax.set_title(f'{var_name} - Sample {sample_idx} (Dataset ID: {dataset_idx}) | R²={m["R2"]:.3f}, RMSE={m["RMSE"]:.3f}',
                        fontsize=12, fontweight='bold')
            ax.legend(fontsize=10, loc='best')
            ax.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(save_dir / f'timeseries_sample_{sample_idx}_main.png', dpi=300, bbox_inches='tight')
        plt.close()

    # Plot all variables (smaller subplots)
    ncols = 4
    nrows = (n_vars + ncols - 1) // ncols

    fig, axes = plt.subplots(nrows, ncols, figsize=(5*ncols, 3*nrows))
    axes = axes.flatten() if n_vars > 1 else [axes]

    for i, var_name in enumerate(var_names):
        ax = axes[i]

        timesteps = np.arange(predictions.shape[0])
        ax.plot(timesteps, true_values[:, i], label='True', linewidth=1.5, alpha=0.7)
        ax.plot(timesteps, predictions[:, i], label='Pred', linewidth=1.5, alpha=0.7, linestyle='--')

        m = metrics[var_name]
        ax.set_xlabel('Time (days)', fontsize=9)
        ax.set_ylabel(var_name, fontsize=9)
        ax.set_title(f'{var_name} | R²={m["R2"]:.2f}', fontsize=10, fontweight='bold')
        ax.legend(fontsize=8, loc='best')
        ax.grid(True, alpha=0.3)
        ax.tick_params(labelsize=8)

    # Hide unused subplots
    for j in range(i + 1, len(axes)):
        axes[j].axis('off')

    plt.suptitle(f'All Variables - Sample {sample_idx} (Dataset ID: {dataset_idx})',
                 fontsize=14, fontweight='bold')
    plt.tight_layout()
    plt.savefig(save_dir / f'timeseries_sample_{sample_idx}_all.png', dpi=200, bbox_inches='tight')
    plt.close()

    return metrics


def main():
    parser = argparse.ArgumentParser(
        description='Validate comprehensive LSTM model on specific test samples',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Validate samples 0-4 from validation set
  python 03_validation.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_143022_dim-512_layer-2 \\
    --sample 0 5

  # Validate single sample
  python 03_validation.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_143022_dim-512_layer-2 \\
    --sample 0 1

  # Validate all test samples
  python 03_validation.py \\
    --model_dir results_forward_comprehensive/AttentionLSTM_20251116_143022_dim-512_layer-2 \\
    --sample 0 -1
        """
    )
    parser.add_argument('--model_dir', type=str, required=True,
                       help='Directory containing trained model')
    parser.add_argument('--sample', type=int, nargs=2, required=True,
                       metavar=('START', 'END'),
                       help='Sample range to validate (START END). Use -1 for END to validate all remaining samples.')
    parser.add_argument('--output_dir', type=str, default=None,
                       help='Output directory for plots (default: model_dir/validation_timeseries)')

    args = parser.parse_args()

    # Device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")

    # Load model
    print(f"\n{'='*60}")
    print("LOADING MODEL")
    print('='*60)
    model, config_dict = load_model(args.model_dir, device)
    print(f"  Model type: {config_dict['model_type']}")
    print(f"  Parameters: {config_dict['total_parameters']:,}")

    # Load data and indices
    print(f"\n{'='*60}")
    print("LOADING DATA AND INDICES")
    print('='*60)
    data_file = config.OUTPUT_FILE
    data_dict, train_indices, val_indices = load_data_and_indices(args.model_dir, data_file)

    print(f"  Total samples: {len(data_dict['X_forcing'])}")
    print(f"  Training samples: {len(train_indices)}")
    print(f"  Validation samples: {len(val_indices)}")
    print(f"  Target variables: {len(data_dict['target_var_names'])}")

    # Parse sample range
    start_idx = args.sample[0]
    end_idx = args.sample[1]

    if end_idx == -1:
        end_idx = len(val_indices)

    if start_idx < 0 or start_idx >= len(val_indices):
        raise ValueError(f"Start index {start_idx} out of range [0, {len(val_indices)-1}]")

    if end_idx <= start_idx or end_idx > len(val_indices):
        raise ValueError(f"End index {end_idx} out of range ({start_idx}, {len(val_indices)}]")

    # Get validation sample indices
    val_sample_indices = list(range(start_idx, end_idx))
    dataset_sample_indices = val_indices[val_sample_indices]

    print(f"\n{'='*60}")
    print("VALIDATION SAMPLE SELECTION")
    print('='*60)
    print(f"  Validation set indices: {start_idx} to {end_idx-1} ({len(val_sample_indices)} samples)")
    print(f"  Dataset indices: {dataset_sample_indices.tolist()}")

    # Create output directory
    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        output_dir = Path(args.model_dir) / 'validation_timeseries'

    output_dir.mkdir(parents=True, exist_ok=True)
    print(f"  Output directory: {output_dir}")

    # Run predictions
    print(f"\n{'='*60}")
    print("GENERATING PREDICTIONS")
    print('='*60)

    predictions, true_values = predict_samples(model, data_dict, dataset_sample_indices, device)
    print(f"  Predictions shape: {predictions.shape}")

    # Generate plots for each sample
    print(f"\n{'='*60}")
    print("GENERATING TIME SERIES PLOTS")
    print('='*60)

    all_metrics = []
    for i, (val_idx, dataset_idx) in enumerate(zip(val_sample_indices, dataset_sample_indices)):
        print(f"\n  Processing validation sample {val_idx} (dataset ID: {dataset_idx})...")

        sample_pred = predictions[i]
        sample_true = true_values[i]

        metrics = plot_time_series_for_sample(
            sample_pred, sample_true,
            data_dict['target_var_names'],
            val_idx, dataset_idx,
            output_dir
        )
        all_metrics.append({'val_idx': int(val_idx), 'dataset_idx': int(dataset_idx), 'metrics': metrics})

        # Print main variable metrics
        print(f"    Main variables:")
        for var_name in ['SOIL_M', 'LH', 'HFX']:
            if var_name in metrics:
                m = metrics[var_name]
                print(f"      {var_name}: R²={m['R2']:.4f}, RMSE={m['RMSE']:.4f}")

    # Save metrics summary
    metrics_file = output_dir / 'validation_metrics_summary.json'
    with open(metrics_file, 'w') as f:
        json.dump(all_metrics, f, indent=2)

    print(f"\n{'='*60}")
    print("✓ VALIDATION COMPLETE")
    print('='*60)
    print(f"\nResults saved to: {output_dir}")
    print(f"  - Time series plots (main variables): timeseries_sample_*_main.png")
    print(f"  - Time series plots (all variables): timeseries_sample_*_all.png")
    print(f"  - Metrics summary: validation_metrics_summary.json")
    print(f"\nValidated {len(val_sample_indices)} samples:")
    print(f"  Validation indices: {val_sample_indices}")
    print(f"  Dataset indices: {dataset_sample_indices.tolist()}")


if __name__ == '__main__':
    main()
