"""
Generate validation plots for forward LSTM model:
1. Time series plots from validation/test dataset
2. Scatter plots of predicted vs true mean values
"""

import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import argparse
import json


def plot_time_series_samples(val_targets, val_preds, target_var_names,
                             save_path, n_samples=6):
    """
    Plot time series predictions vs true values for validation samples

    Args:
        val_targets: True values (n_val_samples, n_timesteps, n_target_vars)
        val_preds: Predicted values (n_val_samples, n_timesteps, n_target_vars)
        target_var_names: List of target variable names
        save_path: Path to save plot
        n_samples: Number of samples to plot
    """
    n_target_vars = len(target_var_names)
    n_samples = min(n_samples, val_targets.shape[0])

    fig, axes = plt.subplots(n_samples, n_target_vars,
                            figsize=(15, 2.5 * n_samples))

    if n_samples == 1:
        axes = axes.reshape(1, -1)

    for i in range(n_samples):
        for j, var_name in enumerate(target_var_names):
            ax = axes[i, j]

            # Plot true and predicted time series
            ax.plot(val_targets[i, :, j], label='True',
                   linewidth=2, alpha=0.8, color='blue')
            ax.plot(val_preds[i, :, j], label='Predicted',
                   linewidth=2, alpha=0.8, color='red', linestyle='--')

            # Compute R² for this sample
            y_true = val_targets[i, :, j]
            y_pred = val_preds[i, :, j]
            ss_res = np.sum((y_true - y_pred) ** 2)
            ss_tot = np.sum((y_true - y_true.mean()) ** 2)
            r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

            # Compute RMSE for this sample
            rmse = np.sqrt(np.mean((y_true - y_pred) ** 2))

            ax.set_xlabel('Time (days)', fontsize=10)
            ax.set_ylabel(var_name, fontsize=10)
            ax.set_title(f'Val Sample {i+1} - {var_name}\n(R²={r2:.3f}, RMSE={rmse:.3f})',
                        fontsize=11)
            ax.legend(fontsize=9, loc='upper right')
            ax.grid(True, alpha=0.3)

    plt.suptitle('Validation Dataset: Time Series Predictions',
                fontsize=14, fontweight='bold', y=1.0)
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    print(f"Saved time series plot to {save_path}")
    plt.close()


def plot_mean_scatter(val_targets, val_preds, target_var_names, save_path):
    """
    Plot scatter plots of predicted vs true mean values for each sample

    Args:
        val_targets: True values (n_val_samples, n_timesteps, n_target_vars)
        val_preds: Predicted values (n_val_samples, n_timesteps, n_target_vars)
        target_var_names: List of target variable names
        save_path: Path to save plot
    """
    n_target_vars = len(target_var_names)

    fig, axes = plt.subplots(1, n_target_vars, figsize=(6 * n_target_vars, 5))

    if n_target_vars == 1:
        axes = [axes]

    for j, var_name in enumerate(target_var_names):
        ax = axes[j]

        # Compute mean values for each sample
        true_means = val_targets[:, :, j].mean(axis=1)  # (n_val_samples,)
        pred_means = val_preds[:, :, j].mean(axis=1)    # (n_val_samples,)

        # Scatter plot
        ax.scatter(true_means, pred_means, alpha=0.6, s=100,
                  edgecolors='black', linewidths=1.5)

        # Compute overall R² for means
        ss_res = np.sum((true_means - pred_means) ** 2)
        ss_tot = np.sum((true_means - true_means.mean()) ** 2)
        r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

        # Compute RMSE
        rmse = np.sqrt(np.mean((true_means - pred_means) ** 2))

        # Compute correlation
        corr = np.corrcoef(true_means, pred_means)[0, 1]

        # Plot 1:1 line
        min_val = min(true_means.min(), pred_means.min())
        max_val = max(true_means.max(), pred_means.max())
        ax.plot([min_val, max_val], [min_val, max_val],
               'k--', linewidth=2, alpha=0.5, label='1:1 line')

        # Add statistics text
        stats_text = f'R² = {r2:.4f}\nRMSE = {rmse:.4f}\nCorr = {corr:.4f}\nn = {len(true_means)}'
        ax.text(0.05, 0.95, stats_text, transform=ax.transAxes,
               fontsize=11, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

        ax.set_xlabel(f'True Mean {var_name}', fontsize=12)
        ax.set_ylabel(f'Predicted Mean {var_name}', fontsize=12)
        ax.set_title(f'{var_name} - Sample Mean Values', fontsize=13, fontweight='bold')
        ax.legend(fontsize=10)
        ax.grid(True, alpha=0.3)

        # Equal aspect ratio for better visualization
        ax.set_aspect('equal', adjustable='box')

    plt.suptitle('Validation Dataset: Predicted vs True Mean Values (Per Sample)',
                fontsize=14, fontweight='bold', y=1.02)
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    print(f"Saved scatter plot to {save_path}")
    plt.close()


def plot_combined_statistics(val_targets, val_preds, target_var_names, save_path):
    """
    Plot additional statistics: box plots and distribution comparisons

    Args:
        val_targets: True values (n_val_samples, n_timesteps, n_target_vars)
        val_preds: Predicted values (n_val_samples, n_timesteps, n_target_vars)
        target_var_names: List of target variable names
        save_path: Path to save plot
    """
    n_target_vars = len(target_var_names)

    fig, axes = plt.subplots(2, n_target_vars, figsize=(6 * n_target_vars, 10))

    if n_target_vars == 1:
        axes = axes.reshape(-1, 1)

    for j, var_name in enumerate(target_var_names):
        # Compute per-sample R²
        n_samples = val_targets.shape[0]
        r2_per_sample = []

        for i in range(n_samples):
            y_true = val_targets[i, :, j]
            y_pred = val_preds[i, :, j]
            ss_res = np.sum((y_true - y_pred) ** 2)
            ss_tot = np.sum((y_true - y_true.mean()) ** 2)
            r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
            r2_per_sample.append(r2)

        r2_per_sample = np.array(r2_per_sample)

        # Top plot: Box plot of R² per sample
        ax1 = axes[0, j]
        bp = ax1.boxplot([r2_per_sample], vert=True, patch_artist=True,
                         labels=[var_name])
        bp['boxes'][0].set_facecolor('lightblue')
        bp['boxes'][0].set_alpha(0.7)

        ax1.set_ylabel('R² Score', fontsize=12)
        ax1.set_title(f'{var_name} - R² Distribution\n(mean={r2_per_sample.mean():.3f}, std={r2_per_sample.std():.3f})',
                     fontsize=12, fontweight='bold')
        ax1.grid(True, alpha=0.3, axis='y')
        ax1.axhline(y=0.9, color='green', linestyle='--', alpha=0.5, label='R²=0.9')
        ax1.axhline(y=0.7, color='orange', linestyle='--', alpha=0.5, label='R²=0.7')
        ax1.legend(fontsize=9)

        # Bottom plot: Histogram of errors
        ax2 = axes[1, j]
        errors = (val_preds[:, :, j] - val_targets[:, :, j]).flatten()

        ax2.hist(errors, bins=50, alpha=0.7, color='steelblue', edgecolor='black')
        ax2.axvline(x=0, color='red', linestyle='--', linewidth=2, label='Zero error')
        ax2.axvline(x=errors.mean(), color='orange', linestyle='--', linewidth=2,
                   label=f'Mean error = {errors.mean():.4f}')

        ax2.set_xlabel('Prediction Error', fontsize=12)
        ax2.set_ylabel('Frequency', fontsize=12)
        ax2.set_title(f'{var_name} - Error Distribution\n(std={errors.std():.4f})',
                     fontsize=12, fontweight='bold')
        ax2.legend(fontsize=10)
        ax2.grid(True, alpha=0.3, axis='y')

    plt.suptitle('Validation Dataset: Statistical Analysis',
                fontsize=14, fontweight='bold', y=1.0)
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    print(f"Saved statistics plot to {save_path}")
    plt.close()


def main():
    parser = argparse.ArgumentParser(description='Plot validation results')
    parser.add_argument('--model_dir', type=str, required=True,
                       help='Directory containing model results')
    parser.add_argument('--n_samples', type=int, default=6,
                       help='Number of samples to plot in time series (default: 6)')

    args = parser.parse_args()

    model_dir = Path(args.model_dir)

    # Load validation predictions
    print(f"Loading predictions from {model_dir}...")
    val_targets = np.load(model_dir / 'val_targets.npy')
    val_preds = np.load(model_dir / 'val_predictions.npy')

    # Load config to get variable names
    with open(model_dir / 'config.json', 'r') as f:
        config = json.load(f)

    # Load metrics
    with open(model_dir / 'metrics.json', 'r') as f:
        metrics = json.load(f)

    target_var_names = list(metrics.keys())

    print(f"\nValidation data shape:")
    print(f"  Targets: {val_targets.shape}")
    print(f"  Predictions: {val_preds.shape}")
    print(f"  Variables: {target_var_names}")

    print("\nOverall Metrics:")
    for var_name, var_metrics in metrics.items():
        print(f"\n{var_name}:")
        for metric_name, value in var_metrics.items():
            print(f"  {metric_name}: {value:.4f}")

    # Generate plots
    print(f"\nGenerating plots...")

    # Plot 1: Time series samples
    plot_time_series_samples(
        val_targets, val_preds, target_var_names,
        model_dir / 'validation_time_series.png',
        n_samples=args.n_samples
    )

    # Plot 2: Mean scatter plots
    plot_mean_scatter(
        val_targets, val_preds, target_var_names,
        model_dir / 'validation_mean_scatter.png'
    )

    # Plot 3: Additional statistics
    plot_combined_statistics(
        val_targets, val_preds, target_var_names,
        model_dir / 'validation_statistics.png'
    )

    print("\n✓ All validation plots generated successfully!")
    print(f"\nPlots saved to {model_dir}:")
    print("  - validation_time_series.png")
    print("  - validation_mean_scatter.png")
    print("  - validation_statistics.png")


if __name__ == '__main__':
    main()
