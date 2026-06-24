"""
Publication-quality plotting settings for A4 print output.

This module provides consistent plotting configurations that produce
figures suitable for academic publication and A4 printing.

Key considerations:
- DPI: 300 for print quality
- Figure size: Based on A4 page dimensions with margins
- Font sizes: Calculated for actual printed size
- Line widths: Visible at print resolution
"""

import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np

# =============================================================================
# A4 Page Dimensions (in inches, accounting for margins)
# =============================================================================
A4_WIDTH_INCHES = 8.27  # 210mm
A4_HEIGHT_INCHES = 11.69  # 297mm

# Typical margins for publications
MARGIN_INCHES = 1.0  # 1 inch margins on each side

# Usable width for figures
USABLE_WIDTH = A4_WIDTH_INCHES - 2 * MARGIN_INCHES  # ~6.27 inches
USABLE_HEIGHT = A4_HEIGHT_INCHES - 2 * MARGIN_INCHES  # ~9.69 inches

# Common figure sizes (width, height in inches)
FIGURE_SIZES = {
    'full_width': (USABLE_WIDTH, USABLE_WIDTH * 0.75),  # 4:3 aspect
    'half_width': (USABLE_WIDTH / 2, USABLE_WIDTH / 2 * 0.75),
    'square': (USABLE_WIDTH * 0.8, USABLE_WIDTH * 0.8),
    'wide': (USABLE_WIDTH, USABLE_WIDTH * 0.5),
    'tall': (USABLE_WIDTH * 0.6, USABLE_WIDTH),
    'multi_panel_2x2': (USABLE_WIDTH, USABLE_WIDTH),
    'multi_panel_3x2': (USABLE_WIDTH, USABLE_WIDTH * 1.2),
    'multi_panel_4x2': (USABLE_WIDTH, USABLE_WIDTH * 1.5),
}

# =============================================================================
# Font Sizes (in points, for actual print size)
# =============================================================================
# These are the ACTUAL sizes that will appear in print
# For A4 printing at 300 DPI, these translate correctly

FONT_SIZES = {
    'title': 12,       # Main figure title
    'subtitle': 11,    # Subplot titles
    'axis_label': 10,  # X and Y axis labels
    'tick_label': 9,   # Tick mark labels
    'legend': 9,       # Legend text
    'annotation': 8,   # Annotations and notes
    'small': 7,        # Small text (e.g., panel labels)
}

# =============================================================================
# Line and Marker Settings
# =============================================================================
LINE_WIDTHS = {
    'data': 1.5,       # Main data lines
    'reference': 1.0,  # Reference/baseline lines
    'thin': 0.75,      # Grid lines, minor elements
    'thick': 2.0,      # Emphasis lines
    'border': 0.5,     # Axis borders
}

MARKER_SIZES = {
    'default': 4,
    'small': 2,
    'large': 6,
    'emphasis': 8,
}

# =============================================================================
# Color Palettes
# =============================================================================
# Colorblind-friendly palette
COLORS = {
    'primary': '#0077BB',    # Blue
    'secondary': '#EE7733',  # Orange
    'tertiary': '#009988',   # Teal
    'quaternary': '#CC3311', # Red
    'quinary': '#33BBEE',    # Cyan
    'senary': '#EE3377',     # Magenta
    'black': '#000000',
    'gray': '#666666',
    'light_gray': '#BBBBBB',
}

# Categorical color palette (up to 8 colors)
COLOR_PALETTE = [
    '#0077BB', '#33BBEE', '#009988', '#EE7733',
    '#CC3311', '#EE3377', '#BBBBBB', '#000000'
]

# Sequential color palette for continuous data
COLOR_SEQUENTIAL = plt.cm.viridis

# =============================================================================
# Style Configuration Function
# =============================================================================
def setup_publication_style():
    """
    Configure matplotlib for publication-quality figures.
    Call this at the start of your plotting script.
    """
    # Use a clean style base
    plt.style.use('seaborn-v0_8-whitegrid')
    
    # Override with publication settings
    mpl.rcParams.update({
        # Figure
        'figure.dpi': 100,  # Screen display DPI
        'savefig.dpi': 300,  # Print quality DPI
        'figure.figsize': FIGURE_SIZES['full_width'],
        'figure.facecolor': 'white',
        'figure.edgecolor': 'white',
        'figure.autolayout': False,
        'figure.constrained_layout.use': True,
        
        # Font
        'font.family': 'sans-serif',
        'font.sans-serif': ['Arial', 'DejaVu Sans', 'Helvetica'],
        'font.size': FONT_SIZES['axis_label'],
        
        # Axes
        'axes.titlesize': FONT_SIZES['subtitle'],
        'axes.labelsize': FONT_SIZES['axis_label'],
        'axes.linewidth': LINE_WIDTHS['border'],
        'axes.grid': True,
        'axes.axisbelow': True,
        'axes.facecolor': 'white',
        'axes.edgecolor': 'black',
        'axes.labelcolor': 'black',
        'axes.prop_cycle': mpl.cycler(color=COLOR_PALETTE),
        
        # Ticks
        'xtick.labelsize': FONT_SIZES['tick_label'],
        'ytick.labelsize': FONT_SIZES['tick_label'],
        'xtick.major.width': LINE_WIDTHS['border'],
        'ytick.major.width': LINE_WIDTHS['border'],
        'xtick.direction': 'out',
        'ytick.direction': 'out',
        
        # Grid
        'grid.linewidth': LINE_WIDTHS['thin'],
        'grid.alpha': 0.5,
        'grid.color': COLORS['light_gray'],
        
        # Legend
        'legend.fontsize': FONT_SIZES['legend'],
        'legend.frameon': True,
        'legend.framealpha': 0.9,
        'legend.edgecolor': 'black',
        'legend.fancybox': False,
        
        # Lines
        'lines.linewidth': LINE_WIDTHS['data'],
        'lines.markersize': MARKER_SIZES['default'],
        
        # Scatter
        'scatter.marker': 'o',
        
        # Save settings
        'savefig.format': 'png',
        'savefig.bbox': 'tight',
        'savefig.pad_inches': 0.1,
        'savefig.transparent': False,
    })

def get_figure_size(size_name='full_width', aspect_ratio=None):
    """
    Get figure size tuple.
    
    Args:
        size_name: One of FIGURE_SIZES keys or tuple (width, height)
        aspect_ratio: If provided, calculate height from width using this ratio
        
    Returns:
        tuple: (width, height) in inches
    """
    if isinstance(size_name, tuple):
        return size_name
    
    base_size = FIGURE_SIZES.get(size_name, FIGURE_SIZES['full_width'])
    
    if aspect_ratio is not None:
        return (base_size[0], base_size[0] / aspect_ratio)
    
    return base_size

def create_figure(size='full_width', nrows=1, ncols=1, aspect_ratio=None, **kwargs):
    """
    Create a figure with publication-quality settings.
    
    Args:
        size: Figure size name or tuple
        nrows: Number of subplot rows
        ncols: Number of subplot columns
        aspect_ratio: Width/height ratio
        **kwargs: Additional arguments for plt.subplots
        
    Returns:
        tuple: (fig, axes)
    """
    figsize = get_figure_size(size, aspect_ratio)
    
    # Adjust figure size for multiple panels
    if nrows > 1 or ncols > 1:
        base_panel_height = figsize[1] / max(nrows, 1)
        figsize = (figsize[0], base_panel_height * nrows * 1.1)
    
    fig, axes = plt.subplots(nrows, ncols, figsize=figsize, **kwargs)
    
    return fig, axes

def save_figure(fig, filename, formats=['png', 'pdf'], dpi=300):
    """
    Save figure in multiple formats.
    
    Args:
        fig: matplotlib figure
        filename: Base filename (without extension)
        formats: List of formats to save
        dpi: Resolution for raster formats
    """
    from pathlib import Path
    
    base_path = Path(filename)
    base_path.parent.mkdir(parents=True, exist_ok=True)
    
    for fmt in formats:
        output_path = base_path.with_suffix(f'.{fmt}')
        fig.savefig(
            output_path,
            format=fmt,
            dpi=dpi if fmt in ['png', 'jpg', 'jpeg'] else None,
            bbox_inches='tight',
            facecolor='white',
            edgecolor='none'
        )
        print(f"Saved: {output_path}")

def add_panel_labels(axes, labels=None, fontsize=None, loc='upper left', offset=(0.02, 0.98)):
    """
    Add panel labels (a), (b), (c), etc. to subplots.
    
    Args:
        axes: Array of axes
        labels: Custom labels (default: a, b, c, ...)
        fontsize: Font size for labels
        loc: Location string or offset tuple
        offset: (x, y) offset from corner (normalized coordinates)
    """
    if fontsize is None:
        fontsize = FONT_SIZES['subtitle']
    
    if not hasattr(axes, '__iter__'):
        axes = [axes]
    else:
        axes = axes.flatten()
    
    if labels is None:
        labels = [f'({chr(97 + i)})' for i in range(len(axes))]
    
    for ax, label in zip(axes, labels):
        ax.text(
            offset[0], offset[1], label,
            transform=ax.transAxes,
            fontsize=fontsize,
            fontweight='bold',
            va='top', ha='left'
        )

def format_axis_scientific(ax, axis='y', scilimits=(-3, 3)):
    """
    Format axis labels using scientific notation.
    
    Args:
        ax: matplotlib axis
        axis: 'x', 'y', or 'both'
        scilimits: Range for using scientific notation
    """
    formatter = mpl.ticker.ScalarFormatter(useMathText=True)
    formatter.set_scientific(True)
    formatter.set_powerlimits(scilimits)
    
    if axis in ['x', 'both']:
        ax.xaxis.set_major_formatter(formatter)
    if axis in ['y', 'both']:
        ax.yaxis.set_major_formatter(formatter)


# =============================================================================
# Convenience plotting functions
# =============================================================================
def plot_timeseries_comparison(ax, times, observed, predicted, 
                               obs_label='Observed', pred_label='Predicted',
                               show_legend=True, title=None):
    """
    Plot observed vs predicted time series with consistent styling.
    """
    ax.plot(times, observed, color=COLORS['black'], linewidth=LINE_WIDTHS['data'],
            label=obs_label, linestyle='-', marker='o', markersize=MARKER_SIZES['small'])
    ax.plot(times, predicted, color=COLORS['primary'], linewidth=LINE_WIDTHS['data'],
            label=pred_label, linestyle='--')
    
    if title:
        ax.set_title(title, fontsize=FONT_SIZES['subtitle'])
    
    if show_legend:
        ax.legend(loc='upper right', fontsize=FONT_SIZES['legend'])
    
    ax.set_xlabel('Date', fontsize=FONT_SIZES['axis_label'])

def plot_scatter_comparison(ax, observed, predicted, title=None, 
                           show_stats=True, show_identity=True):
    """
    Plot scatter comparison with 1:1 line and statistics.
    """
    # Scatter plot
    ax.scatter(observed, predicted, alpha=0.5, s=MARKER_SIZES['default']**2,
               color=COLORS['primary'], edgecolors='none')
    
    # 1:1 line
    if show_identity:
        lims = [
            min(np.nanmin(observed), np.nanmin(predicted)),
            max(np.nanmax(observed), np.nanmax(predicted))
        ]
        margin = (lims[1] - lims[0]) * 0.05
        lims = [lims[0] - margin, lims[1] + margin]
        ax.plot(lims, lims, 'k--', linewidth=LINE_WIDTHS['reference'], label='1:1 line')
        ax.set_xlim(lims)
        ax.set_ylim(lims)
    
    # Statistics
    if show_stats:
        mask = ~(np.isnan(observed) | np.isnan(predicted))
        if np.sum(mask) > 1:
            r2 = np.corrcoef(observed[mask], predicted[mask])[0, 1]**2
            rmse = np.sqrt(np.mean((predicted[mask] - observed[mask])**2))
            bias = np.mean(predicted[mask] - observed[mask])
            
            stats_text = f'R\u00b2 = {r2:.3f}\nRMSE = {rmse:.3f}\nBias = {bias:.3f}'
            ax.text(0.05, 0.95, stats_text, transform=ax.transAxes,
                   fontsize=FONT_SIZES['annotation'], va='top',
                   bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    if title:
        ax.set_title(title, fontsize=FONT_SIZES['subtitle'])
    
    ax.set_xlabel('Observed', fontsize=FONT_SIZES['axis_label'])
    ax.set_ylabel('Predicted', fontsize=FONT_SIZES['axis_label'])
    ax.set_aspect('equal')


# Initialize publication style when module is imported
setup_publication_style()

if __name__ == '__main__':
    # Demo plot
    setup_publication_style()
    
    # Create sample figure
    fig, axes = create_figure('multi_panel_2x2', nrows=2, ncols=2)
    
    # Generate sample data
    x = np.linspace(0, 10, 100)
    y1 = np.sin(x) + np.random.normal(0, 0.1, 100)
    y2 = np.sin(x)
    
    # Time series comparison
    plot_timeseries_comparison(axes[0, 0], x, y1, y2, title='Time Series')
    
    # Scatter comparison
    plot_scatter_comparison(axes[0, 1], y1, y2, title='Scatter Plot')
    
    # Additional panels
    axes[1, 0].bar(range(5), [1, 2, 3, 2, 1], color=COLOR_PALETTE[:5])
    axes[1, 0].set_title('Bar Chart', fontsize=FONT_SIZES['subtitle'])
    
    axes[1, 1].hist(np.random.randn(1000), bins=30, color=COLORS['primary'], alpha=0.7)
    axes[1, 1].set_title('Histogram', fontsize=FONT_SIZES['subtitle'])
    
    # Add panel labels
    add_panel_labels(axes)
    
    save_figure(fig, 'demo_publication_figure', formats=['png'])
    plt.close()
    
    print("Publication plotting demo completed.")
