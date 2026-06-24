"""
Training script for COMPREHENSIVE FORWARD LSTM model
Predicts ALL energy/water cycle variables for full conservation checking

UPDATED:
- Changed data split from 80:20 (train:val) to 64:16:20 (train:val:test)
- Validation is used for early stopping and model selection
- Test set is held out for unbiased final evaluation
- Improved scatter plot function based on plot_validation_scatter.py
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import TensorDataset, DataLoader
import numpy as np
import pickle
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator, ScalarFormatter
from pathlib import Path
import json
from datetime import datetime
import importlib.util

from lstm_model_forward import LSTMForwardPredictor, BiLSTMForwardPredictor, AttentionLSTMForwardPredictor
import config_forward_comprehensive as config

# Import denormalize_data function from preprocessing module
spec = importlib.util.spec_from_file_location(
    "preprocessing",
    "01_data_preprocessing_forward_comprehensive.py"
)
preprocessing = importlib.util.module_from_spec(spec)
spec.loader.exec_module(preprocessing)
denormalize_data = preprocessing.denormalize_data

def set_seed(seed=42):
    """Set random seeds for reproducibility"""
    torch.manual_seed(seed)
    np.random.seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)

def load_data(data_file):
    """Load preprocessed data"""
    with open(data_file, 'rb') as f:
        data = pickle.load(f)
    return data

def create_dataloaders(X_forcing, X_params, y, train_ratio=0.64, val_ratio=0.16, batch_size=16):
    """
    Create train, validation, and test dataloaders
    
    Split ratios:
    - Training (64%): Used for gradient updates
    - Validation (16%): Used for early stopping, LR scheduling, and model selection
    - Test (20%): Held out for unbiased final evaluation only
    
    This ensures the test set is never used during training or model selection,
    providing an unbiased estimate of model performance.
    """
    n_samples = X_forcing.shape[0]
    n_train = int(n_samples * train_ratio)
    n_val = int(n_samples * val_ratio)
    # Remaining samples go to test set

    # Random shuffle
    indices = np.random.permutation(n_samples)
    train_indices = indices[:n_train]
    val_indices = indices[n_train:n_train + n_val]
    test_indices = indices[n_train + n_val:]

    # Create training dataset
    X_forcing_train = torch.FloatTensor(X_forcing[train_indices])
    X_params_train = torch.FloatTensor(X_params[train_indices])
    y_train = torch.FloatTensor(y[train_indices])

    # Create validation dataset
    X_forcing_val = torch.FloatTensor(X_forcing[val_indices])
    X_params_val = torch.FloatTensor(X_params[val_indices])
    y_val = torch.FloatTensor(y[val_indices])

    # Create test dataset
    X_forcing_test = torch.FloatTensor(X_forcing[test_indices])
    X_params_test = torch.FloatTensor(X_params[test_indices])
    y_test = torch.FloatTensor(y[test_indices])

    train_dataset = TensorDataset(X_forcing_train, X_params_train, y_train)
    val_dataset = TensorDataset(X_forcing_val, X_params_val, y_val)
    test_dataset = TensorDataset(X_forcing_test, X_params_test, y_test)

    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=batch_size, shuffle=False)
    test_loader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False)

    return train_loader, val_loader, test_loader, train_indices, val_indices, test_indices

class WeightedMSELoss(nn.Module):
    """Weighted Mean Squared Error Loss for different variable importance"""
    def __init__(self, weights=None):
        super(WeightedMSELoss, self).__init__()
        self.register_buffer('weights', weights)

    def forward(self, pred, target):
        squared_errors = (pred - target) ** 2
        if self.weights is not None:
            weighted_errors = squared_errors * self.weights.view(1, 1, -1)
            return weighted_errors.mean()
        else:
            return squared_errors.mean()

def train_epoch(model, train_loader, criterion, optimizer, device):
    """Train for one epoch"""
    model.train()
    total_loss = 0
    n_batches = 0

    for forcing, params, target in train_loader:
        forcing = forcing.to(device)
        params = params.to(device)
        target = target.to(device)

        optimizer.zero_grad()
        output = model(params, forcing)
        loss = criterion(output, target)
        loss.backward()

        # Gradient clipping
        torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=1.0)

        optimizer.step()

        total_loss += loss.item()
        n_batches += 1

    return total_loss / n_batches

def validate(model, val_loader, criterion, device):
    """Validate the model"""
    model.eval()
    total_loss = 0
    n_batches = 0

    with torch.no_grad():
        for forcing, params, target in val_loader:
            forcing = forcing.to(device)
            params = params.to(device)
            target = target.to(device)

            output = model(params, forcing)
            loss = criterion(output, target)

            total_loss += loss.item()
            n_batches += 1

    if n_batches == 0:
        # Empty val loader (e.g. fewer than 5 samples and val_ratio rounds to 0):
        # treat as worst-possible loss so early stopping does not crash.
        return float('inf')
    return total_loss / n_batches

def plot_training_history(train_losses, val_losses, save_path):
    """Plot training and validation losses"""
    plt.figure(figsize=(10, 6))
    plt.plot(train_losses, label='Train Loss', alpha=0.7)
    plt.plot(val_losses, label='Validation Loss', alpha=0.7)
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.title('Training History - Comprehensive Model')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.yscale('log')
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close()

def compute_metrics(predictions, targets, var_names):
    """
    Compute R², RMSE, and PBIAS metrics for each variable

    Args:
        predictions: Predicted values (n_samples, n_timesteps, n_vars)
        targets: True values (n_samples, n_timesteps, n_vars)
        var_names: List of variable names

    Returns:
        Dictionary with metrics per variable
    """
    # Flatten time dimension
    pred_flat = predictions.reshape(-1, predictions.shape[-1])  # (n_samples*n_timesteps, n_vars)
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


def plot_scatter(predictions, targets, var_names, metrics, save_dir,
                 dataset_name='validation', focus_vars=None):
    """
    Create scatter plots for all variables (with focus on main target variables).
    `focus_vars` defaults to config.MAIN_TARGETS.

    Args:
        predictions: Predicted values (n_samples, n_timesteps, n_vars)
        targets: True values (n_samples, n_timesteps, n_vars)
        var_names: List of variable names
        metrics: Dictionary with metrics per variable
        save_dir: Directory to save plots
        dataset_name: Name of the dataset ('validation' or 'test') for file naming
        focus_vars: Main target variables to highlight
    """
    save_dir = Path(save_dir)
    if focus_vars is None:
        focus_vars = list(config.MAIN_TARGETS)

    # A4 page settings (portrait orientation)
    A4_WIDTH_INCHES = 8.27
    MARGIN_INCHES = 0.75  # Margins on each side
    USABLE_WIDTH = A4_WIDTH_INCHES - 2 * MARGIN_INCHES  # ~6.77 inches
    
    # Display name mapping: internal name -> display name
    DISPLAY_NAMES = {
        'SOIL_M': 'SM',
        'HFX': 'SH',
        'LH': 'LH',
    }
    
    # Units for RMSE
    UNITS = {
        'SOIL_M': 'm³/m³',
        'HFX': 'W/m²',
        'LH': 'W/m²',
    }
    
    def get_display_name(var_name):
        """Get display name for a variable"""
        return DISPLAY_NAMES.get(var_name, var_name)
    
    def get_rmse_str(var_name, rmse_value):
        """Get RMSE string with unit if available"""
        unit = UNITS.get(var_name, '')
        if unit:
            return f"RMSE = {rmse_value:.3f} {unit}"
        else:
            return f"RMSE = {rmse_value:.3f}"
    
    # Flatten time dimension
    pred_flat = predictions.reshape(-1, predictions.shape[-1])
    targ_flat = targets.reshape(-1, targets.shape[-1])

    # Plot for focus variables (main scatter plot)
    n_focus = len([v for v in focus_vars if v in var_names])
    if n_focus > 0:
        # Figure size:
        # - Width = USABLE_WIDTH (A4 usable width)
        # - Height = USABLE_WIDTH / n_focus * height_ratio (for square subplots + padding)
        height_ratio = 1.15 / n_focus  # Each subplot is square, plus 15% padding
        fig_width = USABLE_WIDTH
        fig_height = USABLE_WIDTH * height_ratio
        
        fig, axes = plt.subplots(1, n_focus, figsize=(fig_width, fig_height))
        if n_focus == 1:
            axes = [axes]

        # Scale font sizes based on subplot size
        base_fontsize = 8
        title_fontsize = base_fontsize
        label_fontsize = base_fontsize
        tick_fontsize = base_fontsize - 1
        text_fontsize = base_fontsize - 1

        ax_idx = 0
        for var_name in focus_vars:
            if var_name not in var_names:
                continue

            i = var_names.index(var_name)
            ax = axes[ax_idx]
            display_name = get_display_name(var_name)

            # Scatter plot
            ax.scatter(targ_flat[:, i], pred_flat[:, i], alpha=0.3, s=5, edgecolors='none')

            # 1:1 line
            min_val = min(targ_flat[:, i].min(), pred_flat[:, i].min())
            max_val = max(targ_flat[:, i].max(), pred_flat[:, i].max())
            ax.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=1.5, label='1:1 Line')

            # Add metrics with units
            m = metrics[var_name]
            rmse_str = get_rmse_str(var_name, m['RMSE'])
            textstr = f"R² = {m['R2']:.3f}\n{rmse_str}\nPBIAS = {m['PBIAS']:.2f}%"
            ax.text(0.05, 0.95, textstr, transform=ax.transAxes, fontsize=text_fontsize,
                   verticalalignment='top', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

            ax.set_xlabel('Observation', fontsize=label_fontsize)
            ax.set_ylabel('Prediction', fontsize=label_fontsize)
            ax.set_title(f'{display_name}', fontsize=title_fontsize, fontweight='bold')
            ax.grid(True, alpha=0.3)
            ax.tick_params(labelsize=tick_fontsize)
            ax.set_aspect('equal', adjustable='box')

            ax_idx += 1

        plt.tight_layout()
        plt.savefig(save_dir / f'{dataset_name}_scatter_main.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Saved: {save_dir / f'{dataset_name}_scatter_main.png'}")

    # Plot for all variables (smaller subplots)
    n_vars = len(var_names)
    ncols = 5
    nrows = (n_vars + ncols - 1) // ncols
    
    # For all variables plot:
    # - Width = USABLE_WIDTH
    # - Height = USABLE_WIDTH * (nrows/ncols) (square subplots)
    all_height_ratio = nrows / ncols
    all_fig_width = USABLE_WIDTH
    all_fig_height = USABLE_WIDTH * all_height_ratio

    fig, axes = plt.subplots(nrows, ncols, figsize=(all_fig_width, all_fig_height))
    axes = axes.flatten() if n_vars > 1 else [axes]
    
    # Font sizes for smaller subplots
    all_base_fontsize = 8
    all_title_fontsize = all_base_fontsize
    all_tick_fontsize = 6
    all_text_fontsize = 7
    all_label_fontsize = all_base_fontsize

    for i, var_name in enumerate(var_names):
        ax = axes[i]
        display_name = get_display_name(var_name)

        # Scatter plot
        ax.scatter(targ_flat[:, i], pred_flat[:, i], alpha=0.2, s=2, edgecolors='none')

        # 1:1 line
        min_val = min(targ_flat[:, i].min(), pred_flat[:, i].min())
        max_val = max(targ_flat[:, i].max(), pred_flat[:, i].max())
        ax.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=1)

        # Add R² only
        m = metrics[var_name]
        ax.text(0.05, 0.95, f"R²={m['R2']:.3f}", transform=ax.transAxes, fontsize=all_text_fontsize,
               verticalalignment='top', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.7, pad=0.2))

        # Format axis: reduce ticks and move exponent to title
        scale_str = format_axis_compact(ax, all_tick_fontsize)
        
        # Title with scale factor if present
        if scale_str:
            ax.set_title(f"{display_name} ({scale_str})", fontsize=all_title_fontsize, fontweight='bold', pad=2)
        else:
            ax.set_title(display_name, fontsize=all_title_fontsize, fontweight='bold', pad=2)
        
        ax.grid(True, alpha=0.3)
        ax.tick_params(labelsize=all_tick_fontsize, pad=1)

    # Hide unused subplots
    for j in range(i + 1, len(axes)):
        axes[j].axis('off')

    # Add shared x and y labels using fig.text
    fig.text(0.5, 0.01, 'Observation', ha='center', va='bottom', fontsize=all_label_fontsize)
    fig.text(0.01, 0.5, 'Prediction', ha='left', va='center', rotation='vertical', fontsize=all_label_fontsize)

    # Adjust subplot spacing - key for reducing whitespace
    plt.subplots_adjust(left=0.06, right=0.98, bottom=0.06, top=0.94, wspace=0.25, hspace=0.4)
    
    # Capitalize dataset name for title
    dataset_title = dataset_name.capitalize()
    plt.suptitle(f'{dataset_title}: All Variables', fontsize=all_title_fontsize + 2, fontweight='bold', y=0.98)
    plt.savefig(save_dir / f'{dataset_name}_scatter_all.png', dpi=200, bbox_inches='tight')
    plt.close()
    print(f"Saved: {save_dir / f'{dataset_name}_scatter_all.png'}")


def validate_on_set(model, dataloader, data_dict, device):
    """
    Run validation and return predictions and targets (denormalized)

    Args:
        model: Trained model
        dataloader: DataLoader for validation set
        data_dict: Dictionary with normalization statistics
        device: Device to run on

    Returns:
        predictions, targets (both denormalized)
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
    predictions = np.concatenate(all_preds, axis=0)  # (n_samples, n_timesteps, n_vars)
    targets = np.concatenate(all_targets, axis=0)

    # Denormalize using the new API (supports both z-score and min-max)
    predictions = denormalize_data(predictions, data_dict['targets_norm_stats'])
    targets = denormalize_data(targets, data_dict['targets_norm_stats'])

    return predictions, targets


def print_metrics_summary(metrics, var_names, dataset_name, focus_vars=None):
    """Print formatted metrics summary for a dataset.

    `focus_vars` defaults to config.MAIN_TARGETS.
    """
    if focus_vars is None:
        focus_vars = list(config.MAIN_TARGETS)
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
    """Main training function"""
    set_seed(42)

    # Device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")

    # Load data
    print(f"\nLoading data from {config.OUTPUT_FILE}...")
    data = load_data(config.OUTPUT_FILE)

    print(f"Data loaded:")
    print(f"  Forcing shape: {data['X_forcing'].shape}")
    print(f"  Params shape: {data['X_params'].shape}")
    print(f"  Target shape: {data['y'].shape}")
    print(f"  Number of target variables: {data['n_target_vars']}")

    # Create dataloaders with train/val/test split
    train_r = config.TRAINING_CONFIG.get('train_ratio', 0.64)
    val_r = config.TRAINING_CONFIG.get('val_ratio', 0.16)
    test_r = round(1.0 - train_r - val_r, 2)
    print(f"\nCreating dataloaders with train/val/test split ({int(train_r*100)}:{int(val_r*100)}:{int(test_r*100)})...")
    train_loader, val_loader, test_loader, train_indices, val_indices, test_indices = create_dataloaders(
        data['X_forcing'],
        data['X_params'],
        data['y'],
        train_ratio=train_r,
        val_ratio=val_r,
        batch_size=config.TRAINING_CONFIG['batch_size']
    )

    print(f"Train samples: {len(train_indices)}, Val samples: {len(val_indices)}, Test samples: {len(test_indices)}")
    print(f"  - Training: Used for gradient updates")
    print(f"  - Validation: Used for early stopping, LR scheduling, and model selection")
    print(f"  - Test: Held out for unbiased final evaluation only")

    # Create model
    print(f"\nCreating {config.MODEL_CONFIG['model_type']} model...")
    model_kwargs = {
        'n_params': data['n_params'],
        'n_forcing_vars': data['n_forcing_vars'],
        'n_target_vars': data['n_target_vars'],
        'hidden_dim': config.MODEL_CONFIG['hidden_dim'],
        'num_layers': config.MODEL_CONFIG['num_layers'],
        'param_embedding_dim': config.MODEL_CONFIG['param_embedding_dim'],
        'dropout': config.MODEL_CONFIG['dropout']
    }

    if config.MODEL_CONFIG['model_type'] == 'LSTM':
        model = LSTMForwardPredictor(**model_kwargs)
    elif config.MODEL_CONFIG['model_type'] == 'BiLSTM':
        model = BiLSTMForwardPredictor(**model_kwargs)
    elif config.MODEL_CONFIG['model_type'] == 'AttentionLSTM':
        model = AttentionLSTMForwardPredictor(**model_kwargs)
    else:
        raise ValueError(f"Unknown model type: {config.MODEL_CONFIG['model_type']}")

    model = model.to(device)

    # Count parameters
    total_params = sum(p.numel() for p in model.parameters())
    trainable_params = sum(p.numel() for p in model.parameters() if p.requires_grad)
    print(f"\nModel parameters:")
    print(f"  Total: {total_params:,}")
    print(f"  Trainable: {trainable_params:,}")

    # Create loss function with weights
    weights = config.get_output_weights(data['target_var_names'])
    weights_tensor = torch.FloatTensor(weights).to(device)
    criterion = WeightedMSELoss(weights=weights_tensor)

    print(f"\nLoss weights summary:")
    print(f"  Min weight: {weights.min():.2f}")
    print(f"  Max weight: {weights.max():.2f}")
    print(f"  Mean weight: {weights.mean():.2f}")

    # Optimizer
    optimizer = optim.Adam(model.parameters(), lr=config.TRAINING_CONFIG['learning_rate'])

    # Learning rate scheduler
    scheduler = optim.lr_scheduler.ReduceLROnPlateau(
        optimizer, mode='min', factor=0.5, patience=30, verbose=True
    )

    # Training loop
    print(f"\nStarting training...")
    print(f"  Epochs: {config.TRAINING_CONFIG['num_epochs']}")
    print(f"  Patience: {config.TRAINING_CONFIG['patience']}")
    print(f"  Learning rate: {config.TRAINING_CONFIG['learning_rate']}")

    best_val_loss = float('inf')
    patience_counter = 0
    train_losses = []
    val_losses = []

    # Create results directory with timestamp
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    model_name = f"{config.MODEL_CONFIG['model_type']}_{timestamp}_dim-{config.MODEL_CONFIG['hidden_dim']}_layer-{config.MODEL_CONFIG['num_layers']}"
    results_dir = Path(config.RESULTS_DIR) / model_name
    results_dir.mkdir(parents=True, exist_ok=True)

    print(f"\nResults directory: {results_dir}")

    for epoch in range(config.TRAINING_CONFIG['num_epochs']):
        train_loss = train_epoch(model, train_loader, criterion, optimizer, device)
        val_loss = validate(model, val_loader, criterion, device)

        train_losses.append(train_loss)
        val_losses.append(val_loss)

        # Learning rate scheduling
        scheduler.step(val_loss)

        # Print progress
        if (epoch + 1) % 10 == 0:
            print(f"Epoch [{epoch+1}/{config.TRAINING_CONFIG['num_epochs']}] "
                  f"Train Loss: {train_loss:.6f}, Val Loss: {val_loss:.6f}")

        # Early stopping
        if val_loss < best_val_loss:
            best_val_loss = val_loss
            patience_counter = 0

            # Save best model
            torch.save(model.state_dict(), results_dir / 'best_model.pth')
        else:
            patience_counter += 1

        if patience_counter >= config.TRAINING_CONFIG['patience']:
            print(f"\nEarly stopping at epoch {epoch+1}")
            break

    print(f"\nTraining complete!")
    print(f"Best validation loss: {best_val_loss:.6f}")

    # Load best model for final validation
    model.load_state_dict(torch.load(results_dir / 'best_model.pth'))
    model.eval()

    # Plot training history
    plot_training_history(train_losses, val_losses, results_dir / 'training_history.png')
    print(f"Training history plot saved to {results_dir / 'training_history.png'}")

    # ========================================
    # VALIDATION SET EVALUATION
    # ========================================
    print(f"\n{'='*60}")
    print("VALIDATION SET EVALUATION")
    print("(Used for model selection - may have optimistic bias)")
    print('='*60)

    val_predictions, val_targets = validate_on_set(model, val_loader, data, device)
    print(f"Validation predictions shape: {val_predictions.shape}")

    # Compute validation metrics
    val_metrics = compute_metrics(val_predictions, val_targets, data['target_var_names'])
    val_avg_r2 = print_metrics_summary(val_metrics, data['target_var_names'], "Validation")

    # Generate validation scatter plots
    print(f"\nGenerating validation scatter plots...")
    plot_scatter(val_predictions, val_targets, data['target_var_names'], val_metrics, 
                 results_dir, dataset_name='validation')

    # ========================================
    # TEST SET EVALUATION (UNBIASED)
    # ========================================
    print(f"\n{'='*60}")
    print("TEST SET EVALUATION")
    print("(Held out - unbiased estimate of model performance)")
    print('='*60)

    test_predictions, test_targets = validate_on_set(model, test_loader, data, device)
    print(f"Test predictions shape: {test_predictions.shape}")

    # Compute test metrics
    test_metrics = compute_metrics(test_predictions, test_targets, data['target_var_names'])
    test_avg_r2 = print_metrics_summary(test_metrics, data['target_var_names'], "Test")

    # Generate test scatter plots
    print(f"\nGenerating test scatter plots...")
    plot_scatter(test_predictions, test_targets, data['target_var_names'], test_metrics, 
                 results_dir, dataset_name='test')

    # Save metrics for both validation and test
    all_metrics = {
        'validation': val_metrics,
        'test': test_metrics
    }
    with open(results_dir / 'evaluation_metrics.json', 'w') as f:
        json.dump(all_metrics, f, indent=2)
    print(f"\nEvaluation metrics saved to {results_dir / 'evaluation_metrics.json'}")

    # Save configuration
    config_dict = {
        'model_type': config.MODEL_CONFIG['model_type'],
        'model_config': config.MODEL_CONFIG,
        'training_config': config.TRAINING_CONFIG,
        'n_params': data['n_params'],
        'n_forcing_vars': data['n_forcing_vars'],
        'n_target_vars': data['n_target_vars'],
        'forcing_var_names': data['forcing_var_names'],
        'target_var_names': data['target_var_names'],
        'target_categories': data['target_categories'],
        'param_names': data['param_names'],
        'total_parameters': total_params,
        'trainable_parameters': trainable_params,
        'best_val_loss': best_val_loss,
        'final_epoch': epoch + 1,
        'train_samples': len(train_indices),
        'val_samples': len(val_indices),
        'test_samples': len(test_indices),
        'train_ratio': train_r,
        'val_ratio': val_r,
        'test_ratio': test_r,
        'validation_metrics': {
            'main_targets': {k: v for k, v in val_metrics.items() if k in config.MAIN_TARGETS},
            'average_r2': float(val_avg_r2)
        },
        'test_metrics': {
            'main_targets': {k: v for k, v in test_metrics.items() if k in config.MAIN_TARGETS},
            'average_r2': float(test_avg_r2)
        },
        'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
        'results_dir': str(results_dir)
    }

    with open(results_dir / 'config.json', 'w') as f:
        json.dump(config_dict, f, indent=2)

    print(f"\nConfiguration saved to {results_dir / 'config.json'}")
    print(f"Model saved to {results_dir / 'best_model.pth'}")

    # Save loss history
    np.savez(results_dir / 'loss_history.npz',
             train_losses=train_losses,
             val_losses=val_losses,
             best_val_loss=best_val_loss,
             final_epoch=epoch + 1)

    # Save train/val/test split indices for reproducibility and validation
    np.savez(results_dir / 'train_val_test_indices.npz',
             train_indices=train_indices,
             val_indices=val_indices,
             test_indices=test_indices)

    print(f"\n{'='*60}")
    print(f"TRAINING AND EVALUATION COMPLETE")
    print('='*60)
    print(f"\nAll results saved to: {results_dir}")
    print(f"  - Model: best_model.pth")
    print(f"  - Config: config.json")
    print(f"  - Metrics: evaluation_metrics.json")
    print(f"  - Plots: training_history.png")
    print(f"           validation_scatter_main.png, validation_scatter_all.png")
    print(f"           test_scatter_main.png, test_scatter_all.png")
    print(f"  - Loss history: loss_history.npz")
    print(f"  - Data split indices: train_val_test_indices.npz")

if __name__ == '__main__':
    main()
