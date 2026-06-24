#!/usr/bin/env python3
"""
Standalone script for generating validation and test scatter plots from a trained model.

Usage:
    # Activate conda environment first:
    source /opt/anaconda3/etc/profile.d/conda.sh && conda activate dfm
    
    # Run the script:
    python plot_validation_scatter.py <model_folder>
    
Example:
    python plot_validation_scatter.py results_forward_comprehensive/AttentionLSTM_20260127_221411_dim-1536_layer-4

Options:
    --data-file     Path to processed data file (default: from config)
    --output-dir    Output directory for plots (default: model_folder)
    --focus-vars    Focus variables for main scatter plot (default: SOIL_M LH HFX)
    --batch-size    Batch size for inference (default: 16)
"""

import argparse
import json
import pickle
import numpy as np
import torch
from torch.utils.data import TensorDataset, DataLoader
from pathlib import Path
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator, ScalarFormatter

import sys as _sys
import os as _os
_sys.path.insert(0, _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '..'))
from lstm_model_forward import LSTMForwardPredictor, BiLSTMForwardPredictor, AttentionLSTMForwardPredictor
import config_forward_comprehensive as config


def denormalize_data(normalized_data, stats):
    """Reverse normalization"""
    method = stats['method']
    
    if method == 'z-score':
        return normalized_data * stats['std'] + stats['mean']
    elif method == 'min-max':
        return normalized_data * (stats['max'] - stats['min']) + stats['min']
    else:
        raise ValueError(f"Unknown method: {method}")


def load_data(data_file):
    """Load preprocessed data"""
    with open(data_file, 'rb') as f:
        data = pickle.load(f)
    return data


def create_dataloader(X_forcing, X_params, y, indices, batch_size=16):
    """Create dataloader from indices"""
    X_forcing_subset = torch.FloatTensor(X_forcing[indices])
    X_params_subset = torch.FloatTensor(X_params[indices])
    y_subset = torch.FloatTensor(y[indices])
    
    dataset = TensorDataset(X_forcing_subset, X_params_subset, y_subset)
    loader = DataLoader(dataset, batch_size=batch_size, shuffle=False)
    
    return loader


def load_model(model_folder, device):
    """Load trained model from folder"""
    model_folder = Path(model_folder)
    
    # Load config
    with open(model_folder / 'config.json', 'r') as f:
        model_config = json.load(f)
    
    # Get model parameters
    model_type = model_config['model_type']
    n_params = model_config['n_params']
    n_forcing_vars = model_config['n_forcing_vars']
    n_target_vars = model_config['n_target_vars']
    hidden_dim = model_config['model_config']['hidden_dim']
    num_layers = model_config['model_config']['num_layers']
    dropout = model_config['model_config']['dropout']
    param_embedding_dim = model_config['model_config']['param_embedding_dim']
    
    # Create model
    model_kwargs = {
        'n_params': n_params,
        'n_forcing_vars': n_forcing_vars,
        'n_target_vars': n_target_vars,
        'hidden_dim': hidden_dim,
        'num_layers': num_layers,
        'param_embedding_dim': param_embedding_dim,
        'dropout': dropout
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
    model.load_state_dict(torch.load(model_folder / 'best_model.pth', map_location=device, weights_only=False))
    model = model.to(device)
    model.eval()
    
    return model, model_config


def validate_on_set(model, dataloader, data_dict, device):
    """
    Run validation and return predictions and targets (denormalized)
    """
    model.eval()
    all_preds = []
    all_targets = []
    
    with torch.no_grad():
        for forcing, params, target in dataloader:
            forcing = forcing.to(device)
            params = params.to(device)
            
            output = model(params, forcing)
            
            all_preds.append(output.cpu().numpy())
            all_targets.append(target.numpy())
    
    # Concatenate
    predictions = np.concatenate(all_preds, axis=0)
    targets = np.concatenate(all_targets, axis=0)
    
    # Denormalize
    predictions = denormalize_data(predictions, data_dict['targets_norm_stats'])
    targets = denormalize_data(targets, data_dict['targets_norm_stats'])
    
    return predictions, targets


def compute_metrics(predictions, targets, var_names):
    """
    Compute R², RMSE, and PBIAS metrics for each variable
    """
    # Flatten time dimension
    pred_flat = predictions.reshape(-1, predictions.shape[-1])
    targ_flat = targets.reshape(-1, targets.shape[-1])
    
    metrics = {}
    for i, var_name in enumerate(var_names):
        pred_var = pred_flat[:, i]
        targ_var = targ_flat[:, i]
        
        # R²
        ss_res = np.sum((targ_var - pred_var) ** 2)
        ss_tot = np.sum((targ_var - targ_var.mean()) ** 2)
        r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0.0
        
        # RMSE
        rmse = np.sqrt(np.mean((targ_var - pred_var) ** 2))
        
        # PBIAS
        pbias = 100 * np.sum(pred_var - targ_var) / np.sum(targ_var) if np.sum(targ_var) != 0 else 0.0
        
        metrics[var_name] = {
            'R2': float(r2),
            'RMSE': float(rmse),
            'PBIAS': float(pbias)
        }
    
    return metrics


def format_axis_compact(ax, fontsize):
    """
    Format axis ticks to be compact:
    - Reduce number of ticks
    - Move scientific notation exponent to title suffix
    - Return exponent string if present
    """
    # Reduce number of ticks
    ax.xaxis.set_major_locator(MaxNLocator(nbins=3, prune='both'))
    ax.yaxis.set_major_locator(MaxNLocator(nbins=3, prune='both'))
    
    # Use ScalarFormatter and get the offset/exponent
    for axis in [ax.xaxis, ax.yaxis]:
        formatter = ScalarFormatter(useMathText=True)
        formatter.set_scientific(True)
        formatter.set_powerlimits((-2, 3))  # Use scientific notation outside this range
        axis.set_major_formatter(formatter)
    
    # Draw to compute the offset text
    ax.figure.canvas.draw()
    
    # Get and hide offset text, return for title
    x_offset = ax.xaxis.get_offset_text().get_text()
    y_offset = ax.yaxis.get_offset_text().get_text()
    ax.xaxis.get_offset_text().set_visible(False)
    ax.yaxis.get_offset_text().set_visible(False)
    
    # Return the common scale if both axes have the same
    if x_offset and x_offset == y_offset:
        return x_offset
    elif x_offset or y_offset:
        # If different, return combined
        parts = []
        if x_offset:
            parts.append(f"x:{x_offset}")
        if y_offset:
            parts.append(f"y:{y_offset}")
        return " ".join(parts)
    return ""


def compute_density_2d(x, y, bins=120, x_range=None, y_range=None):
    """
    Compute point density (counts per unit area) on a 2D grid using histogram2d.

    Parameters
    ----------
    x, y : 1D arrays
        Coordinates of points (e.g., observation, prediction).
    bins : int or (int, int)
        Number of bins in x and y directions.
    x_range, y_range : (min, max) or None
        Ranges for histogram. If None, inferred from data.

    Returns
    -------
    density : 2D array (nx, ny)
        Density per unit area (counts / (dx*dy)) in each bin.
    x_edges, y_edges : 1D arrays
        Bin edges for x and y.
    """
    if x_range is None:
        x_range = (np.nanmin(x), np.nanmax(x))
    if y_range is None:
        y_range = (np.nanmin(y), np.nanmax(y))

    # 2D histogram counts
    counts, x_edges, y_edges = np.histogram2d(
        x, y, bins=bins, range=[x_range, y_range]
    )

    # Bin width (assuming uniform bins)
    dx = (x_edges[-1] - x_edges[0]) / (len(x_edges) - 1)
    dy = (y_edges[-1] - y_edges[0]) / (len(y_edges) - 1)
    area = dx * dy

    # Density = counts per unit area
    density = counts / area
    return density, x_edges, y_edges


# def plot_scatter(predictions, targets, var_names, metrics, save_dir, 
#                  dataset_name='validation', focus_vars=['SOIL_M', 'LH', 'HFX']):
#     """
#     Create scatter plots for all variables (with focus on main target variables)

#     Args:
#         predictions: Predicted values (n_samples, n_timesteps, n_vars)
#         targets: True values (n_samples, n_timesteps, n_vars)
#         var_names: List of variable names
#         metrics: Dictionary with metrics per variable
#         save_dir: Directory to save plots
#         dataset_name: Name of the dataset ('validation' or 'test') for file naming
#         focus_vars: Main target variables to highlight
#     """
#     save_dir = Path(save_dir)
    
#     # A4 page settings (portrait orientation)
#     A4_WIDTH_INCHES = 8.27
#     MARGIN_INCHES = 0.75  # Margins on each side
#     USABLE_WIDTH = A4_WIDTH_INCHES - 2 * MARGIN_INCHES  # ~6.77 inches
    
#     # Display name mapping: internal name -> display name
#     DISPLAY_NAMES = {
#         'SOIL_M': 'SM',
#         'HFX': 'SH',
#         'LH': 'LH',
#     }
    
#     # Units for RMSE
#     UNITS = {
#         'SOIL_M': 'm³/m³',
#         'HFX': 'W/m²',
#         'LH': 'W/m²',
#     }
    
#     def get_display_name(var_name):
#         """Get display name for a variable"""
#         return DISPLAY_NAMES.get(var_name, var_name)
    
#     def get_rmse_str(var_name, rmse_value):
#         """Get RMSE string with unit if available"""
#         unit = UNITS.get(var_name, '')
#         if unit:
#             return f"RMSE = {rmse_value:.3f} {unit}"
#         else:
#             return f"RMSE = {rmse_value:.3f}"
    
#     # Flatten time dimension
#     pred_flat = predictions.reshape(-1, predictions.shape[-1])
#     targ_flat = targets.reshape(-1, targets.shape[-1])

#     # Plot for focus variables (main scatter plot)
#     n_focus = len([v for v in focus_vars if v in var_names])
#     if n_focus > 0:
#         # Figure size:
#         # - Width = USABLE_WIDTH (A4 usable width)
#         # - Height = USABLE_WIDTH / n_focus * height_ratio (for square subplots + padding)
#         # - height_ratio accounts for axis labels and title
#         height_ratio = 1.15 / n_focus  # Each subplot is square, plus 15% padding
#         fig_width = USABLE_WIDTH
#         fig_height = USABLE_WIDTH * height_ratio
        
#         fig, axes = plt.subplots(1, n_focus, figsize=(fig_width, fig_height))
#         if n_focus == 1:
#             axes = [axes]

#         # Scale font sizes based on subplot size
#         base_fontsize = 8
#         title_fontsize = base_fontsize
#         label_fontsize = base_fontsize
#         tick_fontsize = base_fontsize - 1
#         text_fontsize = base_fontsize - 1

#         ax_idx = 0
#         for var_name in focus_vars:
#             if var_name not in var_names:
#                 continue

#             i = var_names.index(var_name)
#             ax = axes[ax_idx]
#             display_name = get_display_name(var_name)

#             # Scatter plot
#             ax.scatter(targ_flat[:, i], pred_flat[:, i], alpha=0.3, s=5, edgecolors='none')

#             # 1:1 line
#             min_val = min(targ_flat[:, i].min(), pred_flat[:, i].min())
#             max_val = max(targ_flat[:, i].max(), pred_flat[:, i].max())
#             ax.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=1.5, label='1:1 Line')

#             # Add metrics with units
#             m = metrics[var_name]
#             rmse_str = get_rmse_str(var_name, m['RMSE'])
#             textstr = f"R² = {m['R2']:.3f}\n{rmse_str}\nPBIAS = {m['PBIAS']:.2f}%"
#             ax.text(0.05, 0.95, textstr, transform=ax.transAxes, fontsize=text_fontsize,
#                    verticalalignment='top', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

#             ax.set_xlabel('Observation', fontsize=label_fontsize)
#             ax.set_ylabel('Prediction', fontsize=label_fontsize)
#             ax.set_title(f'{display_name}', fontsize=title_fontsize, fontweight='bold')
#             ax.grid(True, alpha=0.3)
#             ax.tick_params(labelsize=tick_fontsize)
#             ax.set_aspect('equal', adjustable='box')

#             ax_idx += 1

#         plt.tight_layout()
#         plt.savefig(save_dir / f'{dataset_name}_scatter_main.png', dpi=300, bbox_inches='tight')
#         plt.close()
#         print(f"Saved: {save_dir / f'{dataset_name}_scatter_main.png'}")

#     # Plot for all variables (smaller subplots)
#     n_vars = len(var_names)
#     ncols = 5
#     nrows = (n_vars + ncols - 1) // ncols
    
#     # For all variables plot:
#     # - Width = USABLE_WIDTH
#     # - Height = USABLE_WIDTH * (nrows/ncols) (square subplots)
#     # - Reduce height ratio since we removed individual labels
#     all_height_ratio = nrows / ncols
#     all_fig_width = USABLE_WIDTH
#     all_fig_height = USABLE_WIDTH * all_height_ratio

#     fig, axes = plt.subplots(nrows, ncols, figsize=(all_fig_width, all_fig_height))
#     axes = axes.flatten() if n_vars > 1 else [axes]
    
#     # Font sizes for smaller subplots
#     all_base_fontsize = 8
#     all_title_fontsize = all_base_fontsize
#     all_label_fontsize = all_base_fontsize
#     all_tick_fontsize = 6
#     all_text_fontsize = 7

#     for i, var_name in enumerate(var_names):
#         ax = axes[i]
#         display_name = get_display_name(var_name)

#         # Scatter plot
#         ax.scatter(targ_flat[:, i], pred_flat[:, i], alpha=0.2, s=2, edgecolors='none')

#         # 1:1 line
#         min_val = min(targ_flat[:, i].min(), pred_flat[:, i].min())
#         max_val = max(targ_flat[:, i].max(), pred_flat[:, i].max())
#         ax.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=1)

#         # Add R² only
#         m = metrics[var_name]
#         ax.text(0.05, 0.95, f"R²={m['R2']:.3f}", transform=ax.transAxes, fontsize=all_text_fontsize,
#                verticalalignment='top', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.7, pad=0.2))

#         # Format axis: reduce ticks and move exponent to title
#         scale_str = format_axis_compact(ax, all_tick_fontsize)
        
#         # Title with scale factor if present
#         if scale_str:
#             ax.set_title(f"{display_name} ({scale_str})", fontsize=all_title_fontsize, fontweight='bold', pad=2)
#         else:
#             ax.set_title(display_name, fontsize=all_title_fontsize, fontweight='bold', pad=2)
        
#         ax.grid(True, alpha=0.3)
#         ax.tick_params(labelsize=all_tick_fontsize, pad=1)

#     # Hide unused subplots
#     for j in range(i + 1, len(axes)):
#         axes[j].axis('off')

#     # Add shared x and y labels using fig.text
#     fig.text(0.5, 0.01, 'Observation', ha='center', va='bottom', fontsize=all_label_fontsize)
#     fig.text(0.01, 0.5, 'Prediction', ha='left', va='center', rotation='vertical', fontsize=all_label_fontsize)

#     # Adjust subplot spacing - key for reducing whitespace
#     plt.subplots_adjust(left=0.06, right=0.98, bottom=0.06, top=0.94, wspace=0.25, hspace=0.4)
    
#     # Capitalize dataset name for title
#     dataset_title = dataset_name.capitalize()
#     plt.suptitle(f'{dataset_title}: All Variables', fontsize=all_title_fontsize + 2, fontweight='bold', y=0.98)
#     plt.savefig(save_dir / f'{dataset_name}_scatter_all.png', dpi=200, bbox_inches='tight')
#     plt.close()
#     print(f"Saved: {save_dir / f'{dataset_name}_scatter_all.png'}")

def plot_scatter(predictions, targets, var_names, metrics, save_dir,
                 dataset_name='validation', focus_vars=['SOIL_M', 'LH', 'HFX']):
    """
    Create density-heatmap plots (counts per unit area) for all variables,
    replacing scatter points with a 2D density heatmap.

    Key changes:
    - Compute density via np.histogram2d: density = counts / (dx*dy)
    - Plot heatmap via pcolormesh (log normalization for readability)
    """
    from pathlib import Path
    from matplotlib.ticker import MaxNLocator, ScalarFormatter
    from matplotlib.colors import LogNorm

    save_dir = Path(save_dir)

    # A4 page settings (portrait orientation)
    A4_WIDTH_INCHES = 8.27
    MARGIN_INCHES = 0.75
    USABLE_WIDTH = A4_WIDTH_INCHES - 2 * MARGIN_INCHES  # ~6.77 inches

    DISPLAY_NAMES = {'SOIL_M': 'SM', 'HFX': 'SH', 'LH': 'LH'}
    UNITS = {'SOIL_M': 'm³/m³', 'HFX': 'W/m²', 'LH': 'W/m²'}

    def get_display_name(var_name):
        return DISPLAY_NAMES.get(var_name, var_name)

    def get_rmse_str(var_name, rmse_value):
        unit = UNITS.get(var_name, '')
        return f"RMSE = {rmse_value:.3f} {unit}" if unit else f"RMSE = {rmse_value:.3f}"

    def format_axis_compact(ax, fontsize):
        ax.xaxis.set_major_locator(MaxNLocator(nbins=3, prune='both'))
        ax.yaxis.set_major_locator(MaxNLocator(nbins=3, prune='both'))
        for axis in [ax.xaxis, ax.yaxis]:
            formatter = ScalarFormatter(useMathText=True)
            formatter.set_scientific(True)
            formatter.set_powerlimits((-2, 3))
            axis.set_major_formatter(formatter)

        ax.figure.canvas.draw()
        x_offset = ax.xaxis.get_offset_text().get_text()
        y_offset = ax.yaxis.get_offset_text().get_text()
        ax.xaxis.get_offset_text().set_visible(False)
        ax.yaxis.get_offset_text().set_visible(False)

        if x_offset and x_offset == y_offset:
            return x_offset
        elif x_offset or y_offset:
            parts = []
            if x_offset:
                parts.append(f"x:{x_offset}")
            if y_offset:
                parts.append(f"y:{y_offset}")
            return " ".join(parts)
        return ""

    # Flatten time dimension
    pred_flat = predictions.reshape(-1, predictions.shape[-1])
    targ_flat = targets.reshape(-1, targets.shape[-1])

    # -----------------------------
    # (A) Focus variables (main)
    # -----------------------------
    focus_list = [v for v in focus_vars if v in var_names]
    n_focus = len(focus_list)
    if n_focus > 0:
        height_ratio = 1.15 / n_focus
        fig_width = USABLE_WIDTH
        fig_height = USABLE_WIDTH * height_ratio

        fig, axes = plt.subplots(1, n_focus, figsize=(fig_width, fig_height))
        if n_focus == 1:
            axes = [axes]

        base_fontsize = 8
        title_fontsize = base_fontsize
        label_fontsize = base_fontsize
        tick_fontsize = base_fontsize - 1
        text_fontsize = base_fontsize - 1

        for ax_idx, var_name in enumerate(focus_list):
            i = var_names.index(var_name)
            ax = axes[ax_idx]
            display_name = get_display_name(var_name)

            x = targ_flat[:, i]
            y = pred_flat[:, i]

            # Define a common range to keep square domain and consistent 1:1 line
            min_val = np.nanmin([np.nanmin(x), np.nanmin(y)])
            max_val = np.nanmax([np.nanmax(x), np.nanmax(y)])
            xy_range = (min_val, max_val)

            # Compute density (counts per unit area)
            density, x_edges, y_edges = compute_density_2d(
                x, y, bins=80, x_range=xy_range, y_range=xy_range
            )

            # Plot density heatmap (log scale improves visibility)
            # Avoid LogNorm error if density is all zeros
            positive = density[density > 0]
            if positive.size > 0:
                norm = LogNorm(vmin=positive.min(), vmax=positive.max())
            else:
                norm = None

            # # add smooth process before plotting heatmap (optional)
            # from scipy.ndimage import gaussian_filter
            # density_smooth = gaussian_filter(np.nan_to_num(density, nan=0.0), sigma=0.8)
            # density_smooth[density == 0] = np.nan
            # density = density_smooth


            mesh = ax.pcolormesh(
                x_edges, y_edges, density.T,
                shading='flat', norm=norm, cmap="Blues"
            )

            # 1:1 line
            ax.plot([min_val, max_val], [min_val, max_val],
                    'r--', linewidth=1.5, label='1:1 Line')

            # Metrics box
            m = metrics[var_name]
            rmse_str = get_rmse_str(var_name, m['RMSE'])
            textstr = f"R² = {m['R2']:.3f}\n{rmse_str}\nPBIAS = {m['PBIAS']:.2f}%"
            ax.text(0.05, 0.95, textstr, transform=ax.transAxes, fontsize=text_fontsize,
                    verticalalignment='top',
                    bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

            ax.set_xlabel('Observation', fontsize=label_fontsize)
            ax.set_ylabel('Prediction', fontsize=label_fontsize)
            ax.set_title(f'{display_name}', fontsize=title_fontsize, fontweight='bold')
            ax.grid(True, alpha=0.3)
            ax.tick_params(labelsize=tick_fontsize)
            ax.set_aspect('equal', adjustable='box')

            # Optional colorbar for each subplot (compact)
            cbar = fig.colorbar(mesh, ax=ax, fraction=0.046, pad=0.02)
            cbar.ax.tick_params(labelsize=tick_fontsize)
            cbar.set_label('Density (count / unit area)', fontsize=tick_fontsize)

        plt.tight_layout()
        plt.savefig(save_dir / f'{dataset_name}_scatter_main.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Saved: {save_dir / f'{dataset_name}_scatter_main.png'}")

    # -----------------------------
    # (B) All variables (grid)
    # -----------------------------
    n_vars = len(var_names)
    ncols = 5
    nrows = (n_vars + ncols - 1) // ncols

    all_height_ratio = nrows / ncols
    all_fig_width = USABLE_WIDTH
    all_fig_height = USABLE_WIDTH * all_height_ratio

    fig, axes = plt.subplots(nrows, ncols, figsize=(all_fig_width, all_fig_height))
    axes = axes.flatten() if n_vars > 1 else [axes]

    all_base_fontsize = 8
    all_title_fontsize = all_base_fontsize
    all_label_fontsize = all_base_fontsize
    all_tick_fontsize = 6
    all_text_fontsize = 7

    for i, var_name in enumerate(var_names):
        ax = axes[i]
        display_name = get_display_name(var_name)

        x = targ_flat[:, i]
        y = pred_flat[:, i]

        min_val = np.nanmin([np.nanmin(x), np.nanmin(y)])
        max_val = np.nanmax([np.nanmax(x), np.nanmax(y)])
        xy_range = (min_val, max_val)

        # Compute density (use fewer bins for speed on many subplots)
        density, x_edges, y_edges = compute_density_2d(
            x, y, bins=70, x_range=xy_range, y_range=xy_range
        )

        positive = density[density > 0]
        if positive.size > 0:
            norm = LogNorm(vmin=positive.min(), vmax=positive.max())
        else:
            norm = None

        ax.pcolormesh(x_edges, y_edges, density.T, shading='auto', norm=norm, cmap="Blues")

        # 1:1 line
        ax.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=1)

        # R² only
        m = metrics[var_name]
        ax.text(0.05, 0.95, f"R²={m['R2']:.3f}", transform=ax.transAxes, fontsize=all_text_fontsize,
                verticalalignment='top',
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.7, pad=0.2))

        # Compact axis formatting
        scale_str = format_axis_compact(ax, all_tick_fontsize)
        if scale_str:
            ax.set_title(f"{display_name} ({scale_str})", fontsize=all_title_fontsize,
                         fontweight='bold', pad=2)
        else:
            ax.set_title(display_name, fontsize=all_title_fontsize,
                         fontweight='bold', pad=2)

        ax.grid(True, alpha=0.3)
        ax.tick_params(labelsize=all_tick_fontsize, pad=1)

    # Hide unused subplots
    for j in range(i + 1, len(axes)):
        axes[j].axis('off')

    fig.text(0.5, 0.01, 'Observation', ha='center', va='bottom', fontsize=all_label_fontsize)
    fig.text(0.01, 0.5, 'Prediction', ha='left', va='center', rotation='vertical', fontsize=all_label_fontsize)

    plt.subplots_adjust(left=0.06, right=0.98, bottom=0.06, top=0.94, wspace=0.25, hspace=0.4)
    dataset_title = dataset_name.capitalize()
    plt.suptitle(f'{dataset_title}: All Variables', fontsize=all_title_fontsize + 2,
                 fontweight='bold', y=0.98)

    plt.savefig(save_dir / f'{dataset_name}_scatter_all.png', dpi=200, bbox_inches='tight')
    plt.close()
    print(f"Saved: {save_dir / f'{dataset_name}_scatter_all.png'}")





def print_metrics_summary(metrics, var_names, dataset_name, focus_vars=['SOIL_M', 'LH', 'HFX']):
    """Print formatted metrics summary for a dataset"""
    print(f"\n{dataset_name} - Main Target Variables Metrics:")
    for var_name in focus_vars:
        if var_name in metrics:
            m = metrics[var_name]
            print(f"  {var_name}:")
            print(f"    R²    = {m['R2']:.4f}")
            print(f"    RMSE  = {m['RMSE']:.4f}")
            print(f"    PBIAS = {m['PBIAS']:.2f}%")

    avg_r2 = np.mean([m['R2'] for m in metrics.values()])
    print(f"\n{dataset_name} - All Variables Summary:")
    print(f"  Average R²: {avg_r2:.4f}")
    
    return avg_r2


def main():
    parser = argparse.ArgumentParser(
        description='Generate validation and test scatter plots from a trained model',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Example:
    # Activate conda environment first:
    source /opt/anaconda3/etc/profile.d/conda.sh && conda activate dfm
    
    # Run the script:
    python plot_validation_scatter.py results_forward_comprehensive/AttentionLSTM_20260127_221411_dim-1536_layer-4
        """
    )
    parser.add_argument('model_folder', type=str, help='Path to the model folder')
    parser.add_argument('--data-file', type=str, default=None, 
                        help='Path to processed data file (default: from config)')
    parser.add_argument('--output-dir', type=str, default=None,
                        help='Output directory for plots (default: model_folder)')
    parser.add_argument('--focus-vars', type=str, nargs='+', default=['SOIL_M', 'LH', 'HFX'],
                        help='Focus variables for main scatter plot')
    parser.add_argument('--batch-size', type=int, default=16, help='Batch size for inference')
    
    args = parser.parse_args()
    
    # Setup paths
    model_folder = Path(args.model_folder)
    if not model_folder.exists():
        raise FileNotFoundError(f"Model folder not found: {model_folder}")
    
    data_file = args.data_file if args.data_file else config.OUTPUT_FILE
    output_dir = Path(args.output_dir) if args.output_dir else model_folder
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")
    
    # Load model
    print(f"\nLoading model from: {model_folder}")
    model, model_config = load_model(model_folder, device)
    print(f"  Model type: {model_config['model_type']}")
    print(f"  Hidden dim: {model_config['model_config']['hidden_dim']}")
    print(f"  Num layers: {model_config['model_config']['num_layers']}")
    
    # Load data
    print(f"\nLoading data from: {data_file}")
    data = load_data(data_file)
    print(f"  Forcing shape: {data['X_forcing'].shape}")
    print(f"  Params shape: {data['X_params'].shape}")
    print(f"  Target shape: {data['y'].shape}")
    
    # Load train/val/test indices
    indices_file = model_folder / 'train_val_test_indices.npz'
    if indices_file.exists():
        print(f"\nLoading indices from: {indices_file}")
        indices = np.load(indices_file)
        val_indices = indices['val_indices']
        test_indices = indices['test_indices']
        print(f"  Validation samples: {len(val_indices)}")
        print(f"  Test samples: {len(test_indices)}")
    else:
        # Fallback: recreate split based on config
        print(f"\nWarning: train_val_test_indices.npz not found, recreating split...")
        np.random.seed(42)
        n_samples = data['X_forcing'].shape[0]
        train_ratio = model_config.get('train_ratio', 0.64)
        val_ratio = model_config.get('val_ratio', 0.16)
        n_train = int(n_samples * train_ratio)
        n_val = int(n_samples * val_ratio)
        indices = np.random.permutation(n_samples)
        val_indices = indices[n_train:n_train + n_val]
        test_indices = indices[n_train + n_val:]
        print(f"  Validation samples: {len(val_indices)}")
        print(f"  Test samples: {len(test_indices)}")
    
    var_names = data['target_var_names']
    
    # ========================================
    # VALIDATION SET EVALUATION
    # ========================================
    print(f"\n{'='*60}")
    print("VALIDATION SET EVALUATION")
    print("(Used for model selection - may have optimistic bias)")
    print('='*60)
    
    val_loader = create_dataloader(
        data['X_forcing'], data['X_params'], data['y'],
        val_indices, batch_size=args.batch_size
    )
    
    val_predictions, val_targets = validate_on_set(model, val_loader, data, device)
    print(f"Validation predictions shape: {val_predictions.shape}")
    
    # Compute validation metrics
    val_metrics = compute_metrics(val_predictions, val_targets, var_names)
    val_avg_r2 = print_metrics_summary(val_metrics, var_names, "Validation", args.focus_vars)
    
    # Generate validation scatter plots
    print(f"\nGenerating validation scatter plots...")
    plot_scatter(val_predictions, val_targets, var_names, val_metrics, 
                 output_dir, dataset_name='validation', focus_vars=args.focus_vars)
    
    # ========================================
    # TEST SET EVALUATION (UNBIASED)
    # ========================================
    print(f"\n{'='*60}")
    print("TEST SET EVALUATION")
    print("(Held out - unbiased estimate of model performance)")
    print('='*60)
    
    test_loader = create_dataloader(
        data['X_forcing'], data['X_params'], data['y'],
        test_indices, batch_size=args.batch_size
    )
    
    test_predictions, test_targets = validate_on_set(model, test_loader, data, device)
    print(f"Test predictions shape: {test_predictions.shape}")
    
    # Compute test metrics
    test_metrics = compute_metrics(test_predictions, test_targets, var_names)
    test_avg_r2 = print_metrics_summary(test_metrics, var_names, "Test", args.focus_vars)
    
    # Generate test scatter plots
    print(f"\nGenerating test scatter plots...")
    plot_scatter(test_predictions, test_targets, var_names, test_metrics, 
                 output_dir, dataset_name='test', focus_vars=args.focus_vars)
    
    # Save metrics for both validation and test
    all_metrics = {
        'validation': val_metrics,
        'test': test_metrics
    }
    metrics_file = output_dir / 'evaluation_metrics_regenerated.json'
    with open(metrics_file, 'w') as f:
        json.dump(all_metrics, f, indent=2)
    print(f"\nMetrics saved to: {metrics_file}")
    
    print(f"\n{'='*60}")
    print("SCATTER PLOTS GENERATED SUCCESSFULLY")
    print('='*60)
    print(f"\nOutput directory: {output_dir}")
    print(f"  - validation_scatter_main.png, validation_scatter_all.png")
    print(f"  - test_scatter_main.png, test_scatter_all.png")
    print(f"  - evaluation_metrics_regenerated.json")


if __name__ == '__main__':
    main()
