#!/usr/bin/env python3
"""
Enhanced Calibration Validation Script with Period Markers

This script compares Noah-MP model outputs with different parameter sets,
distinguishing between calibration and validation time periods in both
analysis and plotting.

Features:
- Separate metrics calculation for calibration and validation periods (RMSE, BIAS, MAE)
- Time series plots with period labels drawn directly on the boundary
- Observation (truth) curve plotted prominently in black
- Column name mapping between observation and simulation data
- Publication-quality figures sized for A4 paper
- Comprehensive comparison of default and emulator-calibrated parameters
- Metrics summary output file
- Improved daily averaging: requires minimum data coverage to avoid biased means
- Line breaking for disconnected dates to avoid misleading visual connections
"""

import os
import sys
import argparse
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.patches import Patch
from pathlib import Path
from datetime import datetime, timedelta

# Add project directory to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Import publication plotting settings
HAS_PLOT_SETTINGS = False
try:
    import plot_settings
    HAS_PLOT_SETTINGS = True
except (ImportError, OSError):
    pass

# Set default style
try:
    if 'seaborn-v0_8-whitegrid' in plt.style.available:
        plt.style.use('seaborn-v0_8-whitegrid')
    elif 'seaborn-whitegrid' in plt.style.available:
        plt.style.use('seaborn-whitegrid')
    else:
        plt.style.use('ggplot')
except:
    pass

import config_forward_comprehensive as config

# Get full time range for plotting
TIME_RANGE_FULL = getattr(config, 'TIME_RANGE_FULL', None)

# =============================================================================
# Timezone Configuration
# =============================================================================
# Local UTC offset for the site (hours). Malaysia is UTC+8.
# Observation data was converted to UTC in convert_pso_obs.py, so we need to
# convert it back to local time for diurnal cycle plots.
# Simulation data from NoahMP is already in local time.
LOCAL_UTC_OFFSET = 8  # hours (UTC+8 for Malaysia)

# =============================================================================
# Daily Aggregation Configuration
# =============================================================================
# Minimum fraction of timesteps required to compute a valid daily average
# This prevents biased averages when only partial day data is available
# (e.g., only 11am-1pm would give unrealistically high heat flux values)
MIN_DAILY_COVERAGE = 0.5  # At least 50% of expected timesteps

# Expected timesteps per day (for 30-min data)
TIMESTEPS_PER_DAY = 48

# =============================================================================
# Column Name Mapping: Observation -> Simulation
# =============================================================================
OBS_TO_SIM_MAPPING = {
    # Fluxtower observation names -> Noah-MP simulation names
    'LE': 'LH',           # Latent heat flux
    'H': 'HFX',           # Sensible heat flux
    'SWC': 'SOIL_M',      # Soil water content / soil moisture
    'Rnet': 'RNET',       # Net radiation
    'Rs': 'SWDOWN',       # Shortwave radiation
    'tair': 'T2M',        # Air temperature
}

# Reverse mapping: Simulation -> Observation
SIM_TO_OBS_MAPPING = {v: k for k, v in OBS_TO_SIM_MAPPING.items()}


# =============================================================================
# A4 Paper Configuration for Publication-Quality Figures
# =============================================================================
A4_WIDTH_INCHES = 8.27  # 210mm
MARGIN_INCHES = 0.75
USABLE_WIDTH = A4_WIDTH_INCHES - 2 * MARGIN_INCHES  # ~6.77 inches

# Font sizes for actual print size
FONT_SIZES = {
    'title': 10,
    'subtitle': 10,
    'axis_label': 9,
    'tick_label': 8,
    'legend': 8,
    'annotation': 7,
    'period_label': 9,
}

# Colors for different model types (matching reference figure style)
COLORS = {
    'observed': '#000000',      # Black
    'emulator': '#0066CC',      # Blue (Calibrated)
    'default': '#CC0000',       # Red
}


def setup_matplotlib():
    """Configure matplotlib for publication-quality output"""
    plt.rcParams.update({
        'font.size': FONT_SIZES['tick_label'],
        'axes.titlesize': FONT_SIZES['subtitle'],
        'axes.labelsize': FONT_SIZES['axis_label'],
        'xtick.labelsize': FONT_SIZES['tick_label'],
        'ytick.labelsize': FONT_SIZES['tick_label'],
        'legend.fontsize': FONT_SIZES['legend'],
        'figure.titlesize': FONT_SIZES['title'],
        'figure.dpi': 300,
        'savefig.dpi': 300,
        'savefig.bbox': 'tight',
        'axes.grid': True,
        'grid.alpha': 0.3,
    })


def parse_noahmp_output(output_file):
    """Parse Noah-MP output CSV file and return DataFrame"""
    if not os.path.exists(output_file):
        return None
    try:
        df = pd.read_csv(output_file)
        if 'timestamp' in df.columns:
            df['timestamp'] = pd.to_datetime(df['timestamp'])
        elif 'date' in df.columns:
            df['timestamp'] = pd.to_datetime(df['date'])
        return df
    except Exception as e:
        print(f"Error reading {output_file}: {e}")
        return None


def read_observation_data(obs_file, time_range=None):
    """Read observation data from CSV file and apply column name mapping"""
    if not os.path.exists(obs_file):
        print(f"Warning: Observation file not found: {obs_file}")
        return None
    
    obs_df = pd.read_csv(obs_file)
    
    # Handle timestamp column
    if 'timestamp' in obs_df.columns:
        obs_df['timestamp'] = pd.to_datetime(obs_df['timestamp'])
    elif 'date' in obs_df.columns:
        obs_df['timestamp'] = pd.to_datetime(obs_df['date'])
    
    # Apply column name mapping (observation -> simulation names)
    rename_map = {}
    for obs_name, sim_name in OBS_TO_SIM_MAPPING.items():
        if obs_name in obs_df.columns and sim_name not in obs_df.columns:
            rename_map[obs_name] = sim_name
    
    if rename_map:
        obs_df = obs_df.rename(columns=rename_map)
        print(f"  Applied column mapping: {rename_map}")
    
    # Filter by time range if specified
    if time_range is not None:
        start_date = pd.to_datetime(time_range['start'])
        end_date = pd.to_datetime(time_range['end'])
        obs_df = obs_df[
            (obs_df['timestamp'] >= start_date) & 
            (obs_df['timestamp'] <= end_date)
        ].reset_index(drop=True)
    
    return obs_df


def calculate_metrics(obs, sim):
    """Calculate performance metrics between observations and simulations"""
    mask = ~(np.isnan(obs) | np.isnan(sim))
    obs_clean = obs[mask]
    sim_clean = sim[mask]
    
    if len(obs_clean) == 0:
        return {'RMSE': np.nan, 'PBIAS': np.nan, 'MAE': np.nan,
                'BIAS': np.nan, 'R2': np.nan, 'NSE': np.nan, 'N': 0}
    
    bias = np.mean(sim_clean - obs_clean)
    mae = np.mean(np.abs(sim_clean - obs_clean))
    rmse = np.sqrt(np.mean((sim_clean - obs_clean)**2))
    
    obs_mean = np.mean(obs_clean)
    obs_sum = np.sum(obs_clean)
    if abs(obs_sum) > 1e-10:
        pbias = 100 * np.sum(sim_clean - obs_clean) / obs_sum
    else:
        pbias = np.nan
    
    ss_res = np.sum((obs_clean - sim_clean)**2)
    ss_tot = np.sum((obs_clean - obs_mean)**2)
    if ss_tot > 1e-10:
        r2 = 1 - (ss_res / ss_tot)
        nse = 1 - (ss_res / ss_tot)
    else:
        r2 = np.nan
        nse = np.nan
    
    return {'RMSE': rmse, 'PBIAS': pbias, 'MAE': mae,
            'BIAS': bias, 'R2': r2, 'NSE': nse, 'N': len(obs_clean)}


def calculate_metrics_by_period(obs_df, sim_df, variable, cal_start, cal_end, val_start, val_end):
    """
    Calculate metrics separately for calibration and validation periods
    """
    results = {}
    
    # Check if variable exists in both dataframes
    if variable not in obs_df.columns:
        print(f"    Warning: Variable '{variable}' not found in observation data")
        return None
    if variable not in sim_df.columns:
        print(f"    Warning: Variable '{variable}' not found in simulation data")
        return None
    
    # Merge dataframes on timestamp
    merged = pd.merge(
        obs_df[['timestamp', variable]].rename(columns={variable: 'obs'}),
        sim_df[['timestamp', variable]].rename(columns={variable: 'sim'}),
        on='timestamp',
        how='inner'
    )
    
    if merged.empty:
        print(f"    Warning: No matching timestamps for variable '{variable}'")
        return None
    
    # Calibration period
    cal_mask = (merged['timestamp'] >= pd.to_datetime(cal_start)) & \
               (merged['timestamp'] <= pd.to_datetime(cal_end))
    cal_data = merged[cal_mask]
    results['calibration'] = calculate_metrics(cal_data['obs'].values, cal_data['sim'].values)
    
    # Validation period
    val_mask = (merged['timestamp'] >= pd.to_datetime(val_start)) & \
               (merged['timestamp'] <= pd.to_datetime(val_end))
    val_data = merged[val_mask]
    results['validation'] = calculate_metrics(val_data['obs'].values, val_data['sim'].values)
    
    # Full period
    results['full'] = calculate_metrics(merged['obs'].values, merged['sim'].values)
    
    return results


def aggregate_to_daily(df, variable, min_coverage=MIN_DAILY_COVERAGE):
    """
    Aggregate data to daily means for cleaner time series plotting.
    
    Only computes daily average if sufficient data coverage exists for that day.
    This prevents biased averages when only partial day data is available
    (e.g., if only 11am-1pm has data, the daily heat flux would be unrealistically high).
    
    Args:
        df: DataFrame with 'timestamp' and variable columns
        variable: Name of the variable column
        min_coverage: Minimum fraction of expected timesteps required (default 0.5)
    
    Returns:
        DataFrame with daily data, NaN for days with insufficient coverage
    """
    if df is None or "timestamp" not in df.columns or variable not in df.columns:
        return None
    
    df_copy = df[["timestamp", variable]].copy()
    df_copy["date"] = df_copy["timestamp"].dt.date
    
    # Count valid (non-NaN) records per day
    valid_counts = df_copy.groupby("date")[variable].apply(lambda x: x.notna().sum())
    
    # Calculate daily means
    daily_means = df_copy.groupby("date")[variable].mean()
    
    # Set to NaN if insufficient coverage
    min_records = int(TIMESTEPS_PER_DAY * min_coverage)
    insufficient_days = valid_counts[valid_counts < min_records].index
    daily_means.loc[insufficient_days] = np.nan
    
    # Create result DataFrame
    daily = daily_means.reset_index()
    daily["timestamp"] = pd.to_datetime(daily["date"])
    daily = daily.drop(columns=["date"])
    
    return daily


def plot_line_segments(ax, dates, values, color, linewidth, label, linestyle='-', alpha=1.0, zorder=5):
    """
    Plot line segments, breaking where dates are not consecutive.
    
    This prevents misleading visual connections across gaps in the data
    (e.g., drawing a straight line across a month of missing data).
    
    Args:
        ax: Matplotlib axis
        dates: Array of datetime values
        values: Array of corresponding values
        color: Line color
        linewidth: Line width
        label: Label for legend (only applied to first segment)
        linestyle: Line style
        alpha: Line transparency
        zorder: Z-order for layering
    """
    if len(dates) == 0:
        return
    
    # Convert to arrays and remove NaN values
    dates = np.array(dates)
    values = np.array(values)
    valid_mask = ~np.isnan(values)
    dates = dates[valid_mask]
    values = values[valid_mask]
    
    if len(dates) == 0:
        return
    
    # Find gaps in dates (more than 1 day apart)
    date_diffs = np.diff(dates).astype('timedelta64[D]').astype(int)
    gap_indices = np.where(date_diffs > 1)[0] + 1
    
    # Split into segments
    segment_starts = np.concatenate([[0], gap_indices])
    segment_ends = np.concatenate([gap_indices, [len(dates)]])
    
    # Plot each segment
    first_segment = True
    for start, end in zip(segment_starts, segment_ends):
        if end - start < 2:
            # Single point - plot as a small marker instead of line
            ax.plot(dates[start:end], values[start:end], 
                   color=color, marker='.', markersize=2, linestyle='none',
                   alpha=alpha, zorder=zorder)
        else:
            # Multiple points - plot as line
            seg_label = label if first_segment else None
            ax.plot(dates[start:end], values[start:end],
                   color=color, linewidth=linewidth, label=seg_label,
                   linestyle=linestyle, alpha=alpha, zorder=zorder)
            first_segment = False


# def plot_timeseries_with_periods(obs_df, sim_dfs, variables, output_file,
#                                   cal_start, cal_end, val_start, val_end,
#                                   labels=None):
#     """
#     Plot time series comparison with calibration/validation period labels

#     Features:
#     - 3 rows (LH, HFX, SOIL_M) x 2 columns (Default, Emulator-calibrated)
#     - Each subplot contains observation (black) and one simulation (colored)
#     - Subplots labeled (a), (b), (c), (d), (e), (f)
#     """
#     setup_matplotlib()

#     # Define variable order and ensure we have exactly these variables
#     var_order = ['LH', 'HFX', 'SOIL_M']
#     variables = [v for v in var_order if v in variables]

#     # Filter out None sim_dfs and get ordered list
#     valid_sim_names = [k for k in ['default', 'emulator'] if k in sim_dfs and sim_dfs[k] is not None]
#     n_rows = len(variables)
#     n_cols = len(valid_sim_names)

#     if n_rows == 0 or n_cols == 0:
#         print(f"Warning: No valid data for time series plot")
#         return

#     # Create figure with subplots (3 rows x 2 columns)
#     fig, axes = plt.subplots(
#         n_rows, n_cols,
#         figsize=(USABLE_WIDTH, USABLE_WIDTH * 0.28 * n_rows),
#         sharex=True
#     )

#     # Handle edge cases for axes shape
#     if n_rows == 1 and n_cols == 1:
#         axes = np.array([[axes]])
#     elif n_rows == 1:
#         axes = axes.reshape(1, -1)
#     elif n_cols == 1:
#         axes = axes.reshape(-1, 1)

#     # Convert dates
#     cal_start_dt = pd.to_datetime(cal_start)
#     cal_end_dt = pd.to_datetime(cal_end)
#     val_start_dt = pd.to_datetime(val_start)
#     val_end_dt = pd.to_datetime(val_end)
#     boundary_date = cal_end_dt + (val_start_dt - cal_end_dt) / 2

#     # Subplot labels
#     subplot_labels = ['(a)', '(b)', '(c)', '(d)', '(e)', '(f)']
#     label_idx = 0

#     # Variable display names
#     var_display = {'LH': 'Latent heat', 'HFX': 'Sensible heat', 'SOIL_M': 'Soil moisture'}
#     var_units = {'LH': 'W/m²', 'HFX': 'W/m²', 'SOIL_M': 'm³/m³'}

#     for row_idx, variable in enumerate(variables):
#         # Prepare observation data for this variable
#         obs_daily = None
#         if obs_df is not None and variable in obs_df.columns:
#             obs_plot = obs_df[['timestamp', variable]].dropna()
#             if TIME_RANGE_FULL is not None:
#                 full_start = pd.to_datetime(TIME_RANGE_FULL['start'])
#                 full_end = pd.to_datetime(TIME_RANGE_FULL['end'])
#                 obs_plot = obs_plot[
#                     (obs_plot['timestamp'] >= full_start) &
#                     (obs_plot['timestamp'] <= full_end)
#                 ]
#             obs_daily = aggregate_to_daily(obs_plot.copy(), variable)

#         for col_idx, sim_name in enumerate(valid_sim_names):
#             ax = axes[row_idx, col_idx]
#             sim_df = sim_dfs[sim_name]

#             # --- CHANGE: Only add shading/period boundary for NON-first column ---
#             if col_idx != 0:
#                 ax.axvspan(cal_start_dt, cal_end_dt, alpha=0.15, color='blue', zorder=0)
#                 ax.axvspan(val_start_dt, val_end_dt, alpha=0.15, color='orange', zorder=0)
#                 ax.axvline(x=boundary_date, color='gray', linestyle='--', linewidth=1.0, zorder=1)

#             # Plot observation (black line)
#             if obs_daily is not None and not obs_daily.empty:
#                 ax.plot(
#                     obs_daily['timestamp'], obs_daily[variable],
#                     color=COLORS['observed'], linewidth=0.8,
#                     label='Observation', linestyle='-', zorder=5
#                 )

#             # Plot simulation (colored line)
#             if sim_df is not None and variable in sim_df.columns:
#                 sim_label = labels.get(sim_name, sim_name.capitalize()) if labels else sim_name.capitalize()
#                 if sim_name == 'emulator':
#                     sim_label = 'Calibrated'
#                 color = COLORS.get(sim_name, 'C0')
#                 sim_daily = aggregate_to_daily(sim_df[['timestamp', variable]].copy(), variable)
#                 if sim_daily is not None and not sim_daily.empty:
#                     ax.plot(
#                         sim_daily['timestamp'], sim_daily[variable],
#                         color=color, linewidth=0.8,
#                         label=sim_label, linestyle='-',
#                         alpha=0.85, zorder=10
#                     )

#             # Subplot label and title
#             var_title = var_display.get(variable, variable)
#             ax.set_title(
#                 f'{subplot_labels[label_idx]} {var_title}',
#                 fontsize=FONT_SIZES['subtitle'], fontweight='bold', loc='center'
#             )
#             label_idx += 1

#             # Y-axis label
#             ax.set_ylabel(var_units.get(variable, variable), fontsize=FONT_SIZES['axis_label'])

#             # Legend (only in last row)
#             if row_idx == n_rows - 1:
#                 ax.legend(loc='lower center', fontsize=FONT_SIZES['legend'], framealpha=0.9)

#             # Grid
#             ax.grid(True, alpha=0.3, zorder=0)

#             # Format x-axis dates
#             ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
#             ax.xaxis.set_major_locator(mdates.MonthLocator(interval=12))

#     # --- CHANGE: Add period labels only to NON-first column ---
#     for col_idx in range(n_cols):
#         if col_idx == 0:
#             continue
#         ax = axes[0, col_idx]
#         y_min, y_max = ax.get_ylim()
#         y_pos = y_max - (y_max - y_min) * 0.08
#         # ax.text(
#         #     boundary_date, y_pos, 'Calibration  ',
#         #     ha='right', va='top', fontsize=FONT_SIZES['period_label'] - 1,
#         #     fontweight='bold', color='#444444', alpha=0.9
#         # )
#         # ax.text(
#         #     boundary_date, y_pos, '  Validation',
#         #     ha='left', va='top', fontsize=FONT_SIZES['period_label'] - 1,
#         #     fontweight='bold', color='#444444', alpha=0.9
#         # )

#     # X-axis label only on bottom row
#     for col_idx in range(n_cols):
#         axes[-1, col_idx].set_xlabel('Date', fontsize=FONT_SIZES['axis_label'])
#         plt.setp(axes[-1, col_idx].xaxis.get_majorticklabels(), rotation=45, ha='right')

#     plt.tight_layout()

#     # Save in multiple formats
#     fig.savefig(f"{output_file}.png", dpi=300, bbox_inches='tight')
#     fig.savefig(f"{output_file}.pdf", dpi=300, bbox_inches='tight')
#     print(f"  Saved: {output_file}.png, {output_file}.pdf")

#     plt.close()


def plot_timeseries_with_periods(obs_df, sim_dfs, variables, output_file,
                                 cal_start, cal_end, val_start, val_end,
                                 labels=None):
    """
    Plot time series comparison with calibration/validation period labels

    Added:
    - 5-day moving average smoothing on DAILY time series (obs & sim)
    """
    setup_matplotlib()

    # ----------------------------
    # Helper: 5-day rolling mean
    # ----------------------------
    def smooth_rolling_mean(daily_df, var, window_days=5, center=True):
        """
        Apply rolling mean on daily time series.

        Parameters
        ----------
        daily_df : pd.DataFrame with columns ['timestamp', var]
        var      : str, variable name
        window_days : int, rolling window length in days (on daily data => window size)
        center   : bool, if True uses centered window to reduce phase lag

        Returns
        -------
        pd.DataFrame with same columns, but var replaced by smoothed values
        """
        if daily_df is None or daily_df.empty or var not in daily_df.columns:
            return daily_df

        df = daily_df[['timestamp', var]].copy()
        df = df.sort_values('timestamp')
        # Rolling mean on daily samples; min_periods=1 keeps endpoints
        df[var] = df[var].rolling(window=window_days, min_periods=1, center=center).mean()
        return df

    # Filter out None sim_dfs and get ordered list
    valid_sim_names = [k for k in ['default', 'emulator'] if k in sim_dfs and sim_dfs[k] is not None]
    n_rows = len(variables)
    n_cols = len(valid_sim_names)

    if n_rows == 0 or n_cols == 0:
        print(f"Warning: No valid data for time series plot")
        return

    # Create figure with subplots (N rows x 2 columns)
    fig, axes = plt.subplots(
        n_rows, n_cols,
        figsize=(USABLE_WIDTH, USABLE_WIDTH * 0.28 * n_rows),
        sharex=True
    )

    # Handle edge cases for axes shape
    if n_rows == 1 and n_cols == 1:
        axes = np.array([[axes]])
    elif n_rows == 1:
        axes = axes.reshape(1, -1)
    elif n_cols == 1:
        axes = axes.reshape(-1, 1)

    # Convert dates
    cal_start_dt = pd.to_datetime(cal_start)
    cal_end_dt = pd.to_datetime(cal_end)
    val_start_dt = pd.to_datetime(val_start)
    val_end_dt = pd.to_datetime(val_end)
    boundary_date = cal_end_dt + (val_start_dt - cal_end_dt) / 2

    # Subplot labels
    subplot_labels = ['(a)', '(b)', '(c)', '(d)', '(e)', '(f)']
    label_idx = 0

    # Variable display names
    var_display = {'LH': 'Latent heat', 'HFX': 'Sensible heat', 'SOIL_M': 'Soil moisture'}
    var_units = {'LH': 'W/m²', 'HFX': 'W/m²', 'SOIL_M': 'm³/m³'}

    # Smoothing config (edit if needed)
    SMOOTH_WINDOW_DAYS = 10
    SMOOTH_CENTER = True  # set False if you want causal smoothing (past-only)

    for row_idx, variable in enumerate(variables):
        # Prepare observation data for this variable
        obs_daily = None
        if obs_df is not None and variable in obs_df.columns:
            obs_plot = obs_df[['timestamp', variable]].dropna()
            if TIME_RANGE_FULL is not None:
                full_start = pd.to_datetime(TIME_RANGE_FULL['start'])
                full_end = pd.to_datetime(TIME_RANGE_FULL['end'])
                obs_plot = obs_plot[
                    (obs_plot['timestamp'] >= full_start) &
                    (obs_plot['timestamp'] <= full_end)
                ]
            obs_daily = aggregate_to_daily(obs_plot.copy(), variable)

            # --- NEW: 5-day smoothing (obs) ---
            obs_daily = smooth_rolling_mean(
                obs_daily, variable,
                window_days=SMOOTH_WINDOW_DAYS,
                center=SMOOTH_CENTER
            )

        for col_idx, sim_name in enumerate(valid_sim_names):
            ax = axes[row_idx, col_idx]
            sim_df = sim_dfs[sim_name]

            # Only add shading/period boundary for NON-first column
            if col_idx != 0:
                ax.axvspan(cal_start_dt, cal_end_dt, alpha=0.15, color='blue', zorder=0)
                ax.axvspan(val_start_dt, val_end_dt, alpha=0.15, color='orange', zorder=0)
                ax.axvline(x=boundary_date, color='gray', linestyle='--', linewidth=1.0, zorder=1)

            # Plot observation (black line)
            if obs_daily is not None and not obs_daily.empty:
                ax.plot(
                    obs_daily['timestamp'], obs_daily[variable],
                    color=COLORS['observed'], linewidth=0.8,
                    label=f'Observation ({SMOOTH_WINDOW_DAYS}d mean)',
                    linestyle='-', zorder=5
                )

            # Plot simulation (colored line)
            if sim_df is not None and variable in sim_df.columns:
                sim_label = labels.get(sim_name, sim_name.capitalize()) if labels else sim_name.capitalize()
                if sim_name == 'emulator':
                    sim_label = 'Calibrated'
                color = COLORS.get(sim_name, 'C0')

                sim_daily = aggregate_to_daily(sim_df[['timestamp', variable]].copy(), variable)

                # --- NEW: 5-day smoothing (sim) ---
                sim_daily = smooth_rolling_mean(
                    sim_daily, variable,
                    window_days=SMOOTH_WINDOW_DAYS,
                    center=SMOOTH_CENTER
                )

                if sim_daily is not None and not sim_daily.empty:
                    ax.plot(
                        sim_daily['timestamp'], sim_daily[variable],
                        color=color, linewidth=0.8,
                        label=f'{sim_label} ({SMOOTH_WINDOW_DAYS}d mean)',
                        linestyle='-',
                        alpha=0.85, zorder=10
                    )

            # Subplot label and title
            var_title = var_display.get(variable, variable)
            ax.set_title(
                f'{subplot_labels[label_idx]} {var_title}',
                fontsize=FONT_SIZES['subtitle'], fontweight='bold', loc='center'
            )
            label_idx += 1

            # Y-axis label
            ax.set_ylabel(var_units.get(variable, variable), fontsize=FONT_SIZES['axis_label'])

            # Legend (only in last row)
            if row_idx == n_rows - 1:
                ax.legend(loc='lower center', fontsize=FONT_SIZES['legend'], framealpha=0.9)

            ax.grid(True, alpha=0.3, zorder=0)
            ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
            ax.xaxis.set_major_locator(mdates.MonthLocator(interval=12))

    # Add period labels only to NON-first column (kept as your version)
    for col_idx in range(n_cols):
        if col_idx == 0:
            continue
        ax = axes[0, col_idx]
        y_min, y_max = ax.get_ylim()
        _ = y_max - (y_max - y_min) * 0.08  # kept for future use

    # X-axis label only on bottom row
    for col_idx in range(n_cols):
        axes[-1, col_idx].set_xlabel('Date', fontsize=FONT_SIZES['axis_label'])
        plt.setp(axes[-1, col_idx].xaxis.get_majorticklabels(), rotation=45, ha='right')

    plt.tight_layout()

    fig.savefig(f"{output_file}.png", dpi=300, bbox_inches='tight')
    fig.savefig(f"{output_file}.pdf", dpi=300, bbox_inches='tight')
    print(f"  Saved: {output_file}.png, {output_file}.pdf")

    plt.close()



def plot_metrics_comparison(all_metrics, variable, output_file):
    """
    Plot metrics comparison bar chart for calibration vs validation periods
    
    Shows performance metrics (RMSE, BIAS, MAE) for each model and period
    """
    setup_matplotlib()
    
    # Prepare data
    models = ['default', 'emulator']
    metrics_to_plot = ['RMSE', 'BIAS', 'MAE']
    
    # Create figure with subplots for each metric
    fig, axes = plt.subplots(1, 3, figsize=(USABLE_WIDTH, USABLE_WIDTH * 0.35))
    
    x = np.arange(len(models))
    width = 0.35
    
    for idx, metric in enumerate(metrics_to_plot):
        ax = axes[idx]
        
        # Calibration period values
        cal_values = []
        for model in models:
            if model in all_metrics and all_metrics[model] is not None and 'calibration' in all_metrics[model]:
                cal_values.append(all_metrics[model]['calibration'].get(metric, np.nan))
            else:
                cal_values.append(np.nan)
        
        # Validation period values
        val_values = []
        for model in models:
            if model in all_metrics and all_metrics[model] is not None and 'validation' in all_metrics[model]:
                val_values.append(all_metrics[model]['validation'].get(metric, np.nan))
            else:
                val_values.append(np.nan)
        
        # Plot bars
        bars1 = ax.bar(x - width/2, cal_values, width, label='Calibration', 
                       color='steelblue', alpha=0.8)
        bars2 = ax.bar(x + width/2, val_values, width, label='Validation',
                       color='darkorange', alpha=0.8)
        
        # Add value labels on bars
        for bars in [bars1, bars2]:
            for bar in bars:
                height = bar.get_height()
                if not np.isnan(height):
                    if metric == 'BIAS':
                        label_text = f'{height:.4f}'
                    else:
                        label_text = f'{height:.3f}'
                    ax.annotate(label_text,
                              xy=(bar.get_x() + bar.get_width() / 2, height),
                              xytext=(0, 3), textcoords="offset points",
                              ha='center', va='bottom', fontsize=6)
        
        metric_labels = {
            'RMSE': 'RMSE',
            'BIAS': 'BIAS',
            'MAE': 'MAE'
        }
        ax.set_ylabel(metric_labels.get(metric, metric), fontsize=FONT_SIZES['axis_label'])
        ax.set_title(metric_labels.get(metric, metric), fontsize=FONT_SIZES['subtitle'], fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(['Default', 'Emulator'], fontsize=FONT_SIZES['tick_label'])
        
        if idx == 0:
            ax.legend(fontsize=FONT_SIZES['legend'], loc='upper right')
        
        ax.grid(True, alpha=0.3, axis='y')
        ax.axhline(y=0, color='black', linestyle='-', linewidth=0.5, alpha=0.5)
    
    fig.suptitle(f'{variable} - Performance Metrics by Period', 
                fontsize=FONT_SIZES['title'], fontweight='bold', y=1.02)
    
    plt.tight_layout()
    
    fig.savefig(f"{output_file}.png", dpi=300, bbox_inches='tight')
    fig.savefig(f"{output_file}.pdf", dpi=300, bbox_inches='tight')
    print(f"  Saved: {output_file}.png, {output_file}.pdf")
    
    plt.close()


def plot_scatter_by_period(obs_df, sim_df, variable, output_file, 
                           cal_start, cal_end, val_start, val_end, 
                           model_name='Simulated', display_name=None):
    """
    Plot scatter comparison with points colored by period
    """
    setup_matplotlib()
    
    if obs_df is None or sim_df is None:
        return
    
    if variable not in obs_df.columns or variable not in sim_df.columns:
        return
    
    # Merge data
    merged = pd.merge(
        obs_df[['timestamp', variable]].rename(columns={variable: 'obs'}),
        sim_df[['timestamp', variable]].rename(columns={variable: 'sim'}),
        on='timestamp',
        how='inner'
    )
    
    if merged.empty:
        return
    
    # Remove NaN values
    merged = merged.dropna()
    
    # Create masks for periods
    cal_mask = (merged['timestamp'] >= pd.to_datetime(cal_start)) & \
               (merged['timestamp'] <= pd.to_datetime(cal_end))
    val_mask = (merged['timestamp'] >= pd.to_datetime(val_start)) & \
               (merged['timestamp'] <= pd.to_datetime(val_end))
    
    cal_data = merged[cal_mask]
    val_data = merged[val_mask]
    
    # Create figure
    fig, ax = plt.subplots(figsize=(USABLE_WIDTH * 0.6, USABLE_WIDTH * 0.6))
    
    # Plot calibration period
    if not cal_data.empty:
        ax.scatter(cal_data['obs'], cal_data['sim'], 
                   alpha=0.6, s=15, c='steelblue', label='Calibration', edgecolors='none')
    
    # Plot validation period
    if not val_data.empty:
        ax.scatter(val_data['obs'], val_data['sim'],
                   alpha=0.6, s=15, c='darkorange', label='Validation', edgecolors='none')
    
    # 1:1 line
    all_values = np.concatenate([merged['obs'].values, merged['sim'].values])
    if len(all_values) > 0:
        lims = [np.nanmin(all_values), np.nanmax(all_values)]
        margin = (lims[1] - lims[0]) * 0.05
        lims = [lims[0] - margin, lims[1] + margin]
        ax.plot(lims, lims, 'k--', linewidth=1, label='1:1 line', zorder=0)
        ax.set_xlim(lims)
        ax.set_ylim(lims)
    
    # Calculate and display metrics for each period (RMSE, BIAS, MAE)
    cal_metrics = calculate_metrics(cal_data['obs'].values, cal_data['sim'].values) if not cal_data.empty else {'RMSE': np.nan, 'BIAS': np.nan, 'MAE': np.nan}
    val_metrics = calculate_metrics(val_data['obs'].values, val_data['sim'].values) if not val_data.empty else {'RMSE': np.nan, 'BIAS': np.nan, 'MAE': np.nan}
    
    stats_text = f"Calibration: RMSE={cal_metrics['RMSE']:.3f}, BIAS={cal_metrics['BIAS']:.3f}, MAE={cal_metrics['MAE']:.3f}\n"
    stats_text += f"Validation:  RMSE={val_metrics['RMSE']:.3f}, BIAS={val_metrics['BIAS']:.3f}, MAE={val_metrics['MAE']:.3f}"
    ax.text(0.05, 0.95, stats_text, transform=ax.transAxes, fontsize=FONT_SIZES['annotation'],
            va='top', family='monospace',
            bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    # Use display_name if provided, otherwise use variable name
    var_display = display_name if display_name else variable
    ax.set_xlabel('Observation', fontsize=FONT_SIZES['axis_label'])
    ax.set_ylabel('Simulation', fontsize=FONT_SIZES['axis_label'])
    ax.set_title(f'{var_display} - {model_name}', fontsize=FONT_SIZES['subtitle'], fontweight='bold')
    ax.legend(fontsize=FONT_SIZES['legend'], loc='lower right')
    ax.set_aspect('equal', adjustable='box')
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    
    fig.savefig(f"{output_file}.png", dpi=300, bbox_inches='tight')
    fig.savefig(f"{output_file}.pdf", dpi=300, bbox_inches='tight')
    print(f"  Saved: {output_file}.png, {output_file}.pdf")
    
    plt.close()


def calculate_seasonal_cycle(df, variable, utc_offset=0):
    """
    Calculate multi-year monthly climatology (seasonal cycle)

    Process (enhanced to handle systematic missing data):
    1. First calculate diurnal cycle (hourly means) from available data
    2. Fill missing values with corresponding hourly means
       - This prevents bias from systematic gaps (e.g., missing nighttime data)
    3. For each year, calculate monthly averages
    4. For each month, calculate the average across all years

    Args:
        df: DataFrame with timestamp and variable columns
        variable: Name of variable column to analyze
        utc_offset: Hours to add to convert UTC to local time (default: 0)

    Returns DataFrame with columns: month, value, std
    """
    if df is None or "timestamp" not in df.columns or variable not in df.columns:
        return None

    # Keep all records (including NaN) for proper filling
    df_copy = df[["timestamp", variable]].copy()

    # Convert to local time if offset specified
    if utc_offset != 0:
        df_copy["timestamp"] = df_copy["timestamp"] + pd.Timedelta(hours=utc_offset)

    df_copy["hour"] = df_copy["timestamp"].dt.hour
    df_copy["year"] = df_copy["timestamp"].dt.year
    df_copy["month"] = df_copy["timestamp"].dt.month

    # Step 1: Calculate hourly means (diurnal climatology) from valid data
    hourly_means = df_copy.groupby("hour")[variable].mean()
    
    # Step 2: Fill missing values with corresponding hourly means
    missing_mask = df_copy[variable].isna()
    n_missing_before = missing_mask.sum()
    
    if n_missing_before > 0:
        df_copy.loc[missing_mask, variable] = df_copy.loc[missing_mask, "hour"].map(hourly_means)


    # Drop any remaining NaN (shouldn't happen if hourly_means covers all hours)
    df_copy = df_copy.dropna(subset=[variable])

    # Step 3: Monthly average for each year
    monthly_by_year = df_copy.groupby(["year", "month"])[variable].mean().reset_index()

    # Step 4: Average across years for each month
    seasonal = monthly_by_year.groupby("month")[variable].agg(["mean", "std"]).reset_index()
    seasonal.columns = ["month", "value", "std"]

    return seasonal


def calculate_diurnal_cycle(df, variable, utc_offset=0):
    """
    Calculate multi-day hourly climatology (diurnal cycle)

    Process:
    1. For each day, calculate hourly averages (if sub-hourly data)
    2. For each hour, calculate the average across all days

    Args:
        df: DataFrame with timestamp and variable columns
        variable: Name of variable column to analyze
        utc_offset: Hours to add to convert UTC to local time (default: 0)

    Returns DataFrame with columns: hour, value, std
    """
    if df is None or "timestamp" not in df.columns or variable not in df.columns:
        return None

    df_copy = df[["timestamp", variable]].dropna().copy()

    # Convert to local time if offset specified
    if utc_offset != 0:
        df_copy["timestamp"] = df_copy["timestamp"] + pd.Timedelta(hours=utc_offset)

    df_copy["date"] = df_copy["timestamp"].dt.date
    df_copy["hour"] = df_copy["timestamp"].dt.hour

    # Step 1: Hourly average for each day (handles sub-hourly data like 30-min)
    hourly_by_day = df_copy.groupby(["date", "hour"])[variable].mean().reset_index()

    # Step 2: Average across days for each hour
    diurnal = hourly_by_day.groupby("hour")[variable].agg(["mean", "std"]).reset_index()
    diurnal.columns = ["hour", "value", "std"]

    return diurnal


def plot_seasonal_diurnal_combined(obs_df, sim_dfs, variables, output_file, labels=None):
    """
    Plot combined seasonal and diurnal cycles comparison
    
    Features:
    - 3 rows (LH, HFX, SOIL_M) x 2 columns (Seasonal, Diurnal)
    - Each subplot contains 3 lines: Observation (black), Default (red), Calibrated (blue)
    - Subplots labeled (a), (b), (c), (d), (e), (f)
    - No markers on lines
    - Colored simulation lines drawn IN FRONT of black observation line
    """
    setup_matplotlib()

    n_rows = len(variables)
    n_cols = 2  # Seasonal, Diurnal

    if n_rows == 0:
        print(f"Warning: No valid data for seasonal/diurnal cycle plot")
        return

    # Create figure with subplots (N rows x 2 columns)
    fig, axes = plt.subplots(n_rows, n_cols, figsize=(USABLE_WIDTH, USABLE_WIDTH * 0.3 * n_rows))
    
    # Handle edge cases for axes shape
    if n_rows == 1:
        axes = axes.reshape(1, -1)
    
    month_names = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    
    # Subplot labels
    subplot_labels = ['(a)', '(b)', '(c)', '(d)', '(e)', '(f)']
    
    # Variable display names
    var_display = {'LH': 'Latent heat', 'HFX': 'Sensible heat', 'SOIL_M': 'Soil moisture'}
    var_units = {'LH': 'W/m²', 'HFX': 'W/m²', 'SOIL_M': 'm³/m³'}
    
    # Colors matching reference image: Observation=black, Default=red, Calibrated=blue
    line_colors = {
        'observed': '#000000',   # Black
        'default': '#CC0000',    # Red
        'emulator': '#0066CC'    # Blue
    }
    
    for row_idx, variable in enumerate(variables):
        # Calculate cycles for this variable
        # Note: Both observation and simulation data are in UTC.
        # Apply LOCAL_UTC_OFFSET to convert all data to local time for plotting.
        obs_seasonal = None
        obs_diurnal = None
        if obs_df is not None and variable in obs_df.columns:
            obs_seasonal = calculate_seasonal_cycle(obs_df, variable, utc_offset=LOCAL_UTC_OFFSET)
            obs_diurnal = calculate_diurnal_cycle(obs_df, variable, utc_offset=LOCAL_UTC_OFFSET)

        # ============ Column 0: Seasonal Cycle ============
        ax_seasonal = axes[row_idx, 0]
        label_idx = row_idx * 2

        # Plot observation (black line) - BEHIND colored lines (zorder=5)
        if obs_seasonal is not None and not obs_seasonal.empty:
            ax_seasonal.plot(obs_seasonal["month"], obs_seasonal["value"],
                    color=line_colors["observed"], linewidth=1.5,
                    label="Observation", linestyle="-", zorder=5)

        # Plot simulations (also convert to local time) - IN FRONT (zorder=10)
        for sim_name in ['default', 'emulator']:
            if sim_name in sim_dfs and sim_dfs[sim_name] is not None:
                sim_df = sim_dfs[sim_name]
                if variable in sim_df.columns:
                    sim_seasonal = calculate_seasonal_cycle(sim_df, variable, utc_offset=LOCAL_UTC_OFFSET)
                    if sim_seasonal is not None and not sim_seasonal.empty:
                        sim_label = labels.get(sim_name, sim_name.capitalize()) if labels else sim_name.capitalize()
                        # Shorten label for legend
                        if sim_name == 'emulator':
                            sim_label = 'Calibrated'
                        color = line_colors.get(sim_name, "C0")
                        ax_seasonal.plot(sim_seasonal["month"], sim_seasonal["value"],
                               color=color, linewidth=1.5,
                               label=sim_label, linestyle="-", zorder=10)
        
        # Subplot formatting - Seasonal
        var_title = var_display.get(variable, variable)
        ax_seasonal.set_title(f'{subplot_labels[label_idx]} {var_title}', 
                    fontsize=FONT_SIZES['subtitle'], fontweight='bold', loc='center')
        ax_seasonal.set_xticks(range(1, 13))
        ax_seasonal.set_xticklabels(month_names, fontsize=FONT_SIZES["tick_label"]-1, rotation=45, ha='right')
        ax_seasonal.set_ylabel(var_units.get(variable, variable), fontsize=FONT_SIZES["axis_label"])
        ax_seasonal.grid(True, alpha=0.3)
        
        # No legend in seasonal (col 0)
        pass  # Legend only in row 0, col 1 (diurnal)
        
        # ============ Column 1: Diurnal Cycle ============
        ax_diurnal = axes[row_idx, 1]
        label_idx = row_idx * 2 + 1
        
        # Plot observation (black line) - BEHIND colored lines (zorder=5)
        if obs_diurnal is not None and not obs_diurnal.empty:
            ax_diurnal.plot(obs_diurnal["hour"], obs_diurnal["value"],
                    color=line_colors["observed"], linewidth=1.5,
                    label="Observation", linestyle="-", zorder=5)
        
        # Plot simulations (also convert to local time) - IN FRONT (zorder=10)
        for sim_name in ['default', 'emulator']:
            if sim_name in sim_dfs and sim_dfs[sim_name] is not None:
                sim_df = sim_dfs[sim_name]
                if variable in sim_df.columns:
                    sim_diurnal = calculate_diurnal_cycle(sim_df, variable, utc_offset=LOCAL_UTC_OFFSET)
                    if sim_diurnal is not None and not sim_diurnal.empty:
                        sim_label = labels.get(sim_name, sim_name.capitalize()) if labels else sim_name.capitalize()
                        if sim_name == 'emulator':
                            sim_label = 'Calibrated'
                        color = line_colors.get(sim_name, "C0")
                        ax_diurnal.plot(sim_diurnal["hour"], sim_diurnal["value"],
                               color=color, linewidth=1.5,
                               label=sim_label, linestyle="-", zorder=10)
        
        # Subplot formatting - Diurnal
        ax_diurnal.set_title(f'{subplot_labels[label_idx]} {var_title}', 
                    fontsize=FONT_SIZES['subtitle'], fontweight='bold', loc='center')
        ax_diurnal.set_xticks(range(0, 24, 2))
        ax_diurnal.set_xlim(-0.5, 23.5)
        ax_diurnal.set_ylabel(var_units.get(variable, variable), fontsize=FONT_SIZES["axis_label"])
        ax_diurnal.grid(True, alpha=0.3)
        
        # Legend only in row 0, col 1 (diurnal)
        if row_idx == 0:
            ax_diurnal.legend(loc="upper left", fontsize=FONT_SIZES["legend"], framealpha=0.9)
    
    # X-axis labels only on bottom row
    axes[-1, 0].set_xlabel("Month", fontsize=FONT_SIZES["axis_label"])
    axes[-1, 1].set_xlabel("Hour (Local Time)", fontsize=FONT_SIZES["axis_label"])
    
    plt.tight_layout()
    
    fig.savefig(f"{output_file}.png", dpi=300, bbox_inches="tight")
    fig.savefig(f"{output_file}.pdf", dpi=300, bbox_inches="tight")
    print(f"  Saved: {output_file}.png, {output_file}.pdf")
    
    plt.close()

    # =====================================================================
    # Export seasonal and diurnal cycle data to CSV
    # =====================================================================
    export_seasonal_diurnal_csv(obs_df, sim_dfs, variables, output_file)


def export_seasonal_diurnal_csv(obs_df, sim_dfs, variables, output_file):
    """
    Export seasonal and diurnal cycle numerical results to CSV files.

    For each variable, creates two CSV files:
      - seasonal: monthly average values for Obs, Default, Calibrated + biases
      - diurnal:  hourly  average values for Obs, Default, Calibrated + biases

    Args:
        obs_df: Observation DataFrame
        sim_dfs: Dict of simulation DataFrames (keys: 'default', 'emulator', ...)
        variables: List of variable names to export
        output_file: Base path string (same as the plot output_file)
    """
    output_dir = os.path.dirname(output_file)

    month_names = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

    for variable in variables:
        # ---------- Seasonal ----------
        obs_seasonal = None
        if obs_df is not None and variable in obs_df.columns:
            obs_seasonal = calculate_seasonal_cycle(obs_df, variable, utc_offset=LOCAL_UTC_OFFSET)

        default_seasonal = None
        if 'default' in sim_dfs and sim_dfs['default'] is not None and variable in sim_dfs['default'].columns:
            default_seasonal = calculate_seasonal_cycle(sim_dfs['default'], variable, utc_offset=LOCAL_UTC_OFFSET)

        emulator_seasonal = None
        if 'emulator' in sim_dfs and sim_dfs['emulator'] is not None and variable in sim_dfs['emulator'].columns:
            emulator_seasonal = calculate_seasonal_cycle(sim_dfs['emulator'], variable, utc_offset=LOCAL_UTC_OFFSET)

        # Build seasonal CSV
        seasonal_rows = []
        for m in range(1, 13):
            row = {'Month': month_names[m - 1]}
            obs_val = np.nan
            def_val = np.nan
            emu_val = np.nan

            if obs_seasonal is not None and m in obs_seasonal['month'].values:
                obs_val = obs_seasonal.loc[obs_seasonal['month'] == m, 'value'].values[0]
            if default_seasonal is not None and m in default_seasonal['month'].values:
                def_val = default_seasonal.loc[default_seasonal['month'] == m, 'value'].values[0]
            if emulator_seasonal is not None and m in emulator_seasonal['month'].values:
                emu_val = emulator_seasonal.loc[emulator_seasonal['month'] == m, 'value'].values[0]

            row['Observation'] = obs_val
            row['Default'] = def_val
            row['Calibrated'] = emu_val
            row['Default_Bias'] = def_val - obs_val
            row['Calibrated_Bias'] = emu_val - obs_val
            seasonal_rows.append(row)

        seasonal_csv = pd.DataFrame(seasonal_rows)
        seasonal_path = os.path.join(output_dir, f'seasonal_cycle_{variable}.csv')
        seasonal_csv.to_csv(seasonal_path, index=False, float_format='%.6f')
        print(f"  Saved seasonal CSV: {seasonal_path}")

        # ---------- Diurnal ----------
        obs_diurnal = None
        if obs_df is not None and variable in obs_df.columns:
            obs_diurnal = calculate_diurnal_cycle(obs_df, variable, utc_offset=LOCAL_UTC_OFFSET)

        default_diurnal = None
        if 'default' in sim_dfs and sim_dfs['default'] is not None and variable in sim_dfs['default'].columns:
            default_diurnal = calculate_diurnal_cycle(sim_dfs['default'], variable, utc_offset=LOCAL_UTC_OFFSET)

        emulator_diurnal = None
        if 'emulator' in sim_dfs and sim_dfs['emulator'] is not None and variable in sim_dfs['emulator'].columns:
            emulator_diurnal = calculate_diurnal_cycle(sim_dfs['emulator'], variable, utc_offset=LOCAL_UTC_OFFSET)

        # Build diurnal CSV
        diurnal_rows = []
        for h in range(0, 24):
            row = {'Hour': h}
            obs_val = np.nan
            def_val = np.nan
            emu_val = np.nan

            if obs_diurnal is not None and h in obs_diurnal['hour'].values:
                obs_val = obs_diurnal.loc[obs_diurnal['hour'] == h, 'value'].values[0]
            if default_diurnal is not None and h in default_diurnal['hour'].values:
                def_val = default_diurnal.loc[default_diurnal['hour'] == h, 'value'].values[0]
            if emulator_diurnal is not None and h in emulator_diurnal['hour'].values:
                emu_val = emulator_diurnal.loc[emulator_diurnal['hour'] == h, 'value'].values[0]

            row['Observation'] = obs_val
            row['Default'] = def_val
            row['Calibrated'] = emu_val
            row['Default_Bias'] = def_val - obs_val
            row['Calibrated_Bias'] = emu_val - obs_val
            diurnal_rows.append(row)

        diurnal_csv = pd.DataFrame(diurnal_rows)
        diurnal_path = os.path.join(output_dir, f'diurnal_cycle_{variable}.csv')
        diurnal_csv.to_csv(diurnal_path, index=False, float_format='%.6f')
        print(f"  Saved diurnal CSV: {diurnal_path}")


# Keep old functions for backward compatibility but they are no longer called
def plot_seasonal_cycle(obs_df, sim_dfs, variables, output_file, labels=None):
    """Deprecated: Use plot_seasonal_diurnal_combined instead"""
    print("  Note: plot_seasonal_cycle is deprecated, use plot_seasonal_diurnal_combined")
    pass


def plot_diurnal_cycle(obs_df, sim_dfs, variables, output_file, labels=None):
    """Deprecated: Use plot_seasonal_diurnal_combined instead"""
    print("  Note: plot_diurnal_cycle is deprecated, use plot_seasonal_diurnal_combined")
    pass




def save_metrics_summary(all_results, output_dir, cal_start, cal_end, val_start, val_end):
    """
    Save comprehensive metrics summary to text file
    """
    summary_file = output_dir / 'metrics_summary.txt'
    
    with open(summary_file, 'w') as f:
        f.write("=" * 80 + "\n")
        f.write("CALIBRATION/VALIDATION METRICS SUMMARY\n")
        f.write("=" * 80 + "\n\n")
        f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        
        f.write("Time Ranges:\n")
        f.write(f"  Calibration: {cal_start} to {cal_end}\n")
        f.write(f"  Validation:  {val_start} to {val_end}\n\n")
        
        # Convert to DataFrame for easier processing
        results_df = pd.DataFrame(all_results)
        
        for variable in results_df['variable'].unique():
            f.write(f"\n{'='*80}\n")
            f.write(f"Variable: {variable}\n")
            f.write(f"{'='*80}\n")
            
            var_df = results_df[results_df['variable'] == variable]
            
            for period in ['calibration', 'validation']:
                period_df = var_df[var_df['period'] == period]
                if len(period_df) == 0:
                    continue
                
                f.write(f"\n--- {period.capitalize()} Period ---\n")
                f.write(f"{'Model':<20} {'RMSE':>10} {'BIAS':>10} {'MAE':>10} {'R2':>10} {'N':>8}\n")
                f.write("-" * 70 + "\n")
                
                for _, row in period_df.iterrows():
                    model_name = row['model'].capitalize()
                    f.write(f"{model_name:<20} {row['RMSE']:>10.4f} {row['BIAS']:>10.4f} {row['MAE']:>10.4f} {row['R2']:>10.4f} {int(row['N']):>8}\n")
        
        f.write(f"\n{'='*80}\n")
        f.write("END OF SUMMARY\n")
        f.write(f"{'='*80}\n")
    
    print(f"  Saved: {summary_file}")
    return summary_file




def plot_scatter_combined(obs_df, sim_dfs, variables, output_file,
                          cal_start, cal_end, val_start, val_end):
    """
    Plot combined scatter plots: 3 rows (variables) x 2 columns (Default, Calibrated)

    Features:
    - Different colors for calibration (steelblue) and validation (darkorange) periods
    - Same color scheme for both Default and Calibrated simulations
    - Each subplot shows R2 and RMSE for each period
    - 1:1 line (dashed)
    - Subplot labels (a), (b), (c), (d), (e), (f)
    """
    setup_matplotlib()

    # Variable display names (fallback to variable name itself for unknowns)
    var_display = {"LH": "LH", "HFX": "SH", "SOIL_M": "SM"}
    var_units = {"LH": "W m$^{-2}$", "HFX": "W m$^{-2}$", "SOIL_M": "m$^3$ m$^{-3}$"}

    n_rows = len(variables)
    n_cols = 2  # Default, Calibrated

    if n_rows == 0:
        print("Warning: No valid variables for combined scatter plot")
        return

    # Model order: default, emulator (calibrated)
    model_order = ["default", "emulator"]
    model_labels = {"default": "Default", "emulator": "Calibrated"}

    # Period colors (same for all models)
    period_colors = {"calibration": "steelblue", "validation": "darkorange"}

    # Convert period dates
    cal_start_dt = pd.to_datetime(cal_start)
    cal_end_dt = pd.to_datetime(cal_end)
    val_start_dt = pd.to_datetime(val_start)
    val_end_dt = pd.to_datetime(val_end)

    # Create figure
    fig, axes = plt.subplots(n_rows, n_cols, figsize=(USABLE_WIDTH, USABLE_WIDTH * 0.5 * n_rows))

    # Handle edge cases
    if n_rows == 1:
        axes = axes.reshape(1, -1)

    # Subplot labels
    subplot_labels = ["(a)", "(b)", "(c)", "(d)", "(e)", "(f)"]
    label_idx = 0

    for row_idx, variable in enumerate(variables):
        var_disp = var_display.get(variable, variable)
        var_unit = var_units.get(variable, "")

        for col_idx, model_name in enumerate(model_order):
            ax = axes[row_idx, col_idx]
            sim_df = sim_dfs.get(model_name)

            if sim_df is None or variable not in sim_df.columns or variable not in obs_df.columns:
                ax.text(0.5, 0.5, "No data", ha="center", va="center", transform=ax.transAxes)
                label_idx += 1
                continue

            # Merge data
            merged = pd.merge(
                obs_df[["timestamp", variable]].rename(columns={variable: "obs"}),
                sim_df[["timestamp", variable]].rename(columns={variable: "sim"}),
                on="timestamp",
                how="inner"
            ).dropna()

            if merged.empty:
                ax.text(0.5, 0.5, "No data", ha="center", va="center", transform=ax.transAxes)
                label_idx += 1
                continue

            # Create masks for periods
            cal_mask = (merged["timestamp"] >= cal_start_dt) & (merged["timestamp"] <= cal_end_dt)
            val_mask = (merged["timestamp"] >= val_start_dt) & (merged["timestamp"] <= val_end_dt)

            cal_data = merged[cal_mask]
            val_data = merged[val_mask]

            # Plot calibration period
            if not cal_data.empty:
                ax.scatter(cal_data["obs"], cal_data["sim"],
                           alpha=0.6, s=8, c=period_colors["calibration"],
                           label="Calibration", edgecolors="none")

            # Plot validation period
            if not val_data.empty:
                ax.scatter(val_data["obs"], val_data["sim"],
                           alpha=0.6, s=8, c=period_colors["validation"],
                           label="Validation", edgecolors="none")

            # 1:1 line
            all_vals = np.concatenate([merged["obs"].values, merged["sim"].values])
            lims = [np.nanmin(all_vals), np.nanmax(all_vals)]
            margin = (lims[1] - lims[0]) * 0.05
            lims = [lims[0] - margin, lims[1] + margin]
            ax.plot(lims, lims, "k--", linewidth=1, zorder=0, label="1:1 line")
            ax.set_xlim(lims)
            ax.set_ylim(lims)

            # Calculate metrics for each period
            cal_metrics = calculate_metrics(cal_data["obs"].values, cal_data["sim"].values) if not cal_data.empty else {"RMSE": np.nan, "R2": np.nan}
            val_metrics = calculate_metrics(val_data["obs"].values, val_data["sim"].values) if not val_data.empty else {"RMSE": np.nan, "R2": np.nan}

            # Add metrics text for both periods
            stats_text = f"Cal: R$^2$={cal_metrics['R2']:.2f}, RMSE={cal_metrics['RMSE']:.2f}\nVal: R$^2$={val_metrics['R2']:.2f}, RMSE={val_metrics['RMSE']:.2f}"
            ax.text(0.05, 0.95, stats_text, transform=ax.transAxes,
                    fontsize=FONT_SIZES["annotation"], va="top",
                    bbox=dict(boxstyle="round", facecolor="white", alpha=0.8))

            # Title with subplot label and model name
            model_label = model_labels.get(model_name, model_name)
            ax.set_title(f"{subplot_labels[label_idx]} {var_disp} ({model_label})",
                        fontsize=FONT_SIZES["subtitle"], fontweight="bold", loc="left")
            label_idx += 1

            # Axis labels
            ax.set_xlabel(f"Observation ({var_unit})", fontsize=FONT_SIZES["axis_label"])
            ax.set_ylabel(f"Simulation ({var_unit})", fontsize=FONT_SIZES["axis_label"])

            # Legend (only for first subplot)
            if row_idx == 0 and col_idx == 0:
                ax.legend(fontsize=FONT_SIZES["legend"] - 1, loc="lower right")

            # Grid and aspect
            ax.grid(True, alpha=0.3)
            ax.set_aspect("equal", adjustable="box")

    plt.tight_layout()

    # Save
    fig.savefig(f"{output_file}.png", dpi=300, bbox_inches="tight")
    fig.savefig(f"{output_file}.pdf", dpi=300, bbox_inches="tight")
    print(f"  Saved: {output_file}.png, {output_file}.pdf")

    plt.close()

def main():
    parser = argparse.ArgumentParser(
        description='Enhanced calibration validation with period markers',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument('--obs', type=str, required=True,
                       help='Observation CSV file')
    parser.add_argument('--emulator_output', type=str, required=True,
                       help='Emulator-calibrated Noah-MP output CSV')
    parser.add_argument('--default_output', type=str, default=None,
                       help='Default parameter Noah-MP output CSV (optional; '
                            'if omitted, default-parameter curves are skipped)')
    parser.add_argument('--output_dir', type=str, required=True,
                       help='Output directory for results')
    parser.add_argument('--variables', type=str, nargs='+', default=None,
                       help='Variables to validate (default: MAIN_TARGETS from '
                            'config_forward_comprehensive.py)')
    
    args = parser.parse_args()

    # Default variables to MAIN_TARGETS from config (single source of truth)
    if args.variables is None:
        args.variables = list(config.MAIN_TARGETS)

    # Read time ranges from config (no command line override needed)
    cal_start = config.TIME_RANGE_CALIBRATION['start']
    cal_end = config.TIME_RANGE_CALIBRATION['end']
    val_start = config.TIME_RANGE_VALIDATION['start']
    val_end = config.TIME_RANGE_VALIDATION['end']
    
    print("="*70)
    print("Enhanced Calibration Validation")
    print("="*70)
    print(f"Calibration period: {cal_start} to {cal_end}")
    print(f"Validation period:  {val_start} to {val_end}")
    print(f"Variables: {args.variables}")
    print("="*70)
    
    # Create output directory
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Load data
    print("\nLoading data...")
    obs_df = read_observation_data(args.obs)
    emulator_df = parse_noahmp_output(args.emulator_output)
    default_df = parse_noahmp_output(args.default_output) if args.default_output else None
    if args.default_output is None:
        print("  No --default_output supplied; default-parameter curves will be skipped.")
    
    if obs_df is None:
        print("Error: Cannot load observation data")
        return 1
    
    print(f"  Observation data loaded: {len(obs_df)} records")
    print(f"    Available columns: {list(obs_df.columns)}")
    if 'timestamp' in obs_df.columns:
        print(f"    Time range: {obs_df['timestamp'].min()} to {obs_df['timestamp'].max()}")
    
    sim_dfs = {
        'emulator': emulator_df,
        'default': default_df,
    }

    labels = {
        'emulator': 'Emulator-calibrated',
        'default': 'Default',
    }
    
    # Process each variable
    all_results = []
    
    for variable in args.variables:
        print(f"\nProcessing {variable}...")
        
        # Check if variable exists in observation data
        if variable not in obs_df.columns:
            print(f"  WARNING: Variable '{variable}' not found in observation data. Skipping...")
            continue
        
        # Calculate metrics by period for each model
        all_metrics = {}
        for model_name, sim_df in sim_dfs.items():
            if sim_df is not None and variable in sim_df.columns:
                metrics_result = calculate_metrics_by_period(
                    obs_df, sim_df, variable,
                    cal_start, cal_end, val_start, val_end
                )
                
                if metrics_result is not None:
                    all_metrics[model_name] = metrics_result
                    
                    # Store results
                    for period in ['calibration', 'validation', 'full']:
                        metrics = metrics_result[period]
                        all_results.append({
                            'variable': variable,
                            'model': model_name,
                            'period': period,
                            **metrics
                        })
                    
                    # Print summary for this model
                    cal_m = metrics_result['calibration']
                    val_m = metrics_result['validation']
                    print(f"  {model_name.capitalize()}:")
                    print(f"    Calibration - RMSE: {cal_m['RMSE']:.4f}, BIAS: {cal_m['BIAS']:.4f}, MAE: {cal_m['MAE']:.4f}, N: {cal_m['N']}")
                    print(f"    Validation  - RMSE: {val_m['RMSE']:.4f}, BIAS: {val_m['BIAS']:.4f}, MAE: {val_m['MAE']:.4f}, N: {val_m['N']}")
    
    # Generate combined plots (all variables in one figure)
    print("\nGenerating combined plots...")
    
    # 1. Time series with period labels (aggregated to daily) - 3x2 grid
    plot_timeseries_with_periods(
        obs_df, sim_dfs, args.variables,
        str(output_dir / 'timeseries_combined'),
        cal_start, cal_end, val_start, val_end,
        labels=labels
    )
    
    # 2. Combined Seasonal and Diurnal cycle plot - 3x2 grid
    # Each row is a variable, column 1 is seasonal, column 2 is diurnal
    plot_seasonal_diurnal_combined(
        obs_df, sim_dfs, args.variables,
        str(output_dir / 'seasonal_diurnal_combined'),
        labels=labels
    )
    
    # 3. Combined scatter plots - 3x2 grid (Default vs Calibrated)
    plot_scatter_combined(
        obs_df, sim_dfs, args.variables,
        str(output_dir / "scatter_combined"),
        cal_start, cal_end, val_start, val_end
    )
    
    # 4. Individual scatter plots by period for each variable and model
    scatter_display_names = {'LH': 'LH', 'HFX': 'SH', 'SOIL_M': 'SM'}
    
    for var in args.variables:
        if var not in obs_df.columns:
            continue
        var_display = scatter_display_names.get(var, var)
        for model_name, sim_df in sim_dfs.items():
            if sim_df is not None and var in sim_df.columns:
                plot_scatter_by_period(
                    obs_df, sim_df, var,
                    str(output_dir / f'scatter_{var_display}_{model_name}'),
                    cal_start, cal_end, val_start, val_end,
                    model_name=labels.get(model_name, model_name),
                    display_name=var_display
                )
    
    # Save metrics to CSV
    if all_results:
        results_df = pd.DataFrame(all_results)
        
        # Reorder columns for clarity
        col_order = ['variable', 'model', 'period', 'RMSE', 'MAE', 'BIAS', 'R2', 'NSE', 'N']
        available_cols = [c for c in col_order if c in results_df.columns]
        results_df = results_df[available_cols]
        
        csv_file = output_dir / 'validation_metrics_by_period.csv'
        results_df.to_csv(csv_file, index=False)
        print(f"\nMetrics saved to: {csv_file}")
        
        # Save text summary
        save_metrics_summary(all_results, output_dir, cal_start, cal_end, val_start, val_end)
        
        # Print summary
        print("\n" + "="*70)
        print("VALIDATION RESULTS SUMMARY")
        print("="*70)
        
        for variable in args.variables:
            if variable not in results_df['variable'].values:
                continue
            print(f"\n{variable}:")
            var_df = results_df[results_df['variable'] == variable]
            for model in ['default', 'emulator']:
                model_df = var_df[var_df['model'] == model]
                if not model_df.empty:
                    cal_row = model_df[model_df['period'] == 'calibration'].iloc[0] if len(model_df[model_df['period'] == 'calibration']) > 0 else None
                    val_row = model_df[model_df['period'] == 'validation'].iloc[0] if len(model_df[model_df['period'] == 'validation']) > 0 else None
                    
                    print(f"  {model.capitalize():20s}")
                    if cal_row is not None:
                        print(f"    Calibration:  RMSE={cal_row['RMSE']:.4f}, BIAS={cal_row['BIAS']:.4f}, MAE={cal_row['MAE']:.4f}")
                    if val_row is not None:
                        print(f"    Validation:   RMSE={val_row['RMSE']:.4f}, BIAS={val_row['BIAS']:.4f}, MAE={val_row['MAE']:.4f}")
    else:
        print("\nWARNING: No metrics were calculated. Check variable names and data availability.")
    
    print("\n" + "="*70)
    print("Validation completed!")
    print(f"Results saved to: {output_dir}")
    print("="*70)
    
    return 0


if __name__ == '__main__':
    sys.exit(main())


