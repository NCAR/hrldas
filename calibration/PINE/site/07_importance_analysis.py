#!/usr/bin/env python3
"""
Parameter Importance Analysis for Noah-MP Calibration

This script analyzes the contribution of each calibrated parameter to the
improvement in model performance (relative to default parameters).

Two methods are implemented:
1. OAT (One-at-a-Time): Simple sensitivity analysis
   - Change one parameter at a time from default to calibrated value
   - Fast but ignores parameter interactions
   - Contributions may not sum to total improvement

2. Shapley Value: Game-theoretic attribution
   - Considers all possible parameter combinations
   - Accounts for parameter interactions
   - Contributions sum exactly to total improvement
   - Based on marginal contributions across all coalition orderings

Usage:
    # Step 1: Generate parameter combinations and prepare run directories
    python 07_importance_analysis.py --mode prepare --method shapley

    # Step 2: (Manual) Run Noah-MP for all combinations
    bash 07_importance_analysis.sh

    # Step 3: Analyze results and compute importance
    python 07_importance_analysis.py --mode analyze --method shapley

Author: Generated for ymwang
Date: 2026-01-23
Updated: 2026-01-28 - Added LAI parameter, improved waterfall plot
"""

import os
import sys
import argparse
import shutil
import itertools
import numpy as np
import pandas as pd
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from collections import defaultdict
import json
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

# Import plot settings for consistent styling
try:
    from plot_settings import (
        setup_publication_style,
        FONT_SIZES,
        LINE_WIDTHS,
        FIGURE_SIZES,
        COLORS as PLOT_COLORS,
        COLOR_PALETTE,
        save_figure,
        USABLE_WIDTH
    )
    setup_publication_style()
    HAS_PLOT_SETTINGS = True
except ImportError:
    HAS_PLOT_SETTINGS = False
    print("Warning: plot_settings.py not found, using default matplotlib settings")

# =============================================================================
# CONFIGURATION
# =============================================================================

# All site/machine paths come from paths_config.py (edit there)
import paths_config

BASE_DIR = Path(paths_config.BASE_DIR)
TBL_GENERATOR_DIR = Path(paths_config.TBL_GENERATOR_DIR)
BASE_TBL_FILE = TBL_GENERATOR_DIR / "NoahmpTable.TBL"
VAR_INFO_FILE = TBL_GENERATOR_DIR / "var_info_matrix.csv"

# Default calibration result directory (override with --calib-dir)
DEFAULT_CALIB_DIR = "validation_results/latest/calibration_1"

# Observation data (default: 30-min combined obs file from paths_config.py)
OBS_FILE = Path(paths_config.OBS_FILE_30MIN)

# Parameter groups (HVT and HVB are treated as one group)
# Each group is defined as a list of parameter column names
# Updated 2026-01-28: Added LAI parameter
PARAM_GROUPS = {
    'VCMX25': ['VCMX25_EBF'],
    'HVT_HVB': ['HVT_EBF', 'HVB_EBF'],  # Coupled parameters
    'CWPVT': ['CWPVT_EBF'],
    'Z0MVT': ['Z0MVT_EBF'],
    'WLTSMC': ['WLTSMC_SCL'],
    'REFSMC': ['REFSMC_SCL'],
    'MAXSMC': ['MAXSMC_SCL'],
    'SATDK': ['SATDK_SCL'],
    'LAI': ['LAI_EBF'],  # Added LAI parameter
}

# Target variables for importance analysis
TARGET_VARIABLES = ['LH', 'SOIL_M', 'HFX']  # HFX is sensible heat (SH)

# Metrics to compute
METRICS = ['RMSE', 'BIAS', 'MAE', 'R2']

# Time periods (from config)
TIME_RANGE_CALIBRATION = {'start': '2005-01-01', 'end': '2006-12-31'}
TIME_RANGE_VALIDATION = {'start': '2007-01-01', 'end': '2009-12-30'}
TIME_RANGE_FULL = {'start': '2005-01-01', 'end': '2009-12-31'}

# Column mapping: observation -> simulation
OBS_TO_SIM_MAPPING = {
    'LE': 'LH',
    'H': 'HFX',
    'SWC': 'SOIL_M',
    'HFX': 'HFX',
    'LH': 'LH',
    'SOIL_M': 'SOIL_M',
}

# =============================================================================
# TBL GENERATION FUNCTIONS (Adapted from noahmp_apply_samples.py)
# =============================================================================

import re

# LAI month names for evergreen vegetation (same value for all months)
LAI_MONTHS = ['LAI_JAN', 'LAI_FEB', 'LAI_MAR', 'LAI_APR', 'LAI_MAY', 'LAI_JUN',
              'LAI_JUL', 'LAI_AUG', 'LAI_SEP', 'LAI_OCT', 'LAI_NOV', 'LAI_DEC']

def find_section_span(text: str, section_name: str) -> Tuple[int, int]:
    """Return (start_idx, end_idx) of a section delimited by '&...\n' to next section start or EOF."""
    start = text.find(section_name)
    if start == -1:
        raise ValueError(f"Section '{section_name}' not found in table.")
    next_match = re.search(r"\n&", text[start+1:])
    if next_match:
        end = start + 1 + next_match.start()
    else:
        end = len(text)
    return start, end


def replace_var_value_in_section(section_text: str, var_name: str, type_idx_1based: int, new_value: float) -> str:
    """Replace the comma-separated {type_idx}-th value on the var assignment line within section_text."""
    pattern = re.compile(rf"(^[ \t]*{re.escape(var_name)}[ \t]*=[^\n]*$)", flags=re.MULTILINE)
    m = pattern.search(section_text)
    if not m:
        raise ValueError(f"Variable '{var_name}' not found as single-line assignment within section.")

    full_line = m.group(1)
    lhs, rhs = full_line.split("=", 1)

    comment_part = ""
    if "!" in rhs:
        rhs, comment_part = rhs.split("!", 1)
        comment_part = "!" + comment_part

    parts = [p.strip() for p in rhs.strip().split(",")]
    idx = type_idx_1based - 1
    if not (0 <= idx < len(parts)):
        raise IndexError(f"Type index {type_idx_1based} is out of range for variable '{var_name}' with {len(parts)} values.")

    # Format value appropriately
    if abs(new_value) < 1e-3 and new_value != 0:
        parts[idx] = f"{float(new_value):.6e}"
    else:
        parts[idx] = f"{float(new_value):.8g}"

    new_rhs = ", ".join(parts)
    new_line = f"{lhs.strip()} = {new_rhs}{comment_part}".rstrip()

    indent = re.match(r"^[ \t]*", full_line).group(0)
    new_line = indent + new_line + "\n"

    start, end = m.span(1)
    section_text = section_text[:start] + new_line + section_text[end:]
    return section_text


def build_var_info(var_info_csv: str = None) -> pd.DataFrame:
    """Load variable info mapping from CSV file."""
    if var_info_csv is None:
        var_info_csv = VAR_INFO_FILE

    if not os.path.exists(var_info_csv):
        raise FileNotFoundError(f"var_info_matrix.csv not found at {var_info_csv}")

    return pd.read_csv(var_info_csv)


def apply_params_to_tbl(base_text: str, params: Dict[str, float], var_info: pd.DataFrame) -> str:
    """
    Apply parameter values to TBL text and return updated text.

    Special handling for LAI_MONTHLY: applies the same LAI value to all 12 months
    (LAI_JAN through LAI_DEC) for evergreen vegetation.
    """
    text = base_text

    for _, row in var_info.iterrows():
        col_name = row['column_name']
        if col_name not in params:
            continue

        section = row['section']
        var = row['variable']
        typ = int(row['type'])
        value = params[col_name]

        try:
            start, end = find_section_span(text, section)
            section_text = text[start:end]

            # Special handling for LAI_MONTHLY: apply same value to all 12 months
            if var == 'LAI_MONTHLY':
                for month_var in LAI_MONTHS:
                    try:
                        section_text = replace_var_value_in_section(section_text, month_var, typ, value)
                    except ValueError:
                        # Month variable might not exist, skip silently
                        pass
            else:
                section_text = replace_var_value_in_section(section_text, var, typ, value)

            text = text[:start] + section_text + text[end:]
        except (ValueError, IndexError) as e:
            print(f"Warning: Could not update {var} in {section}: {e}")

    return text


# =============================================================================
# PARAMETER LOADING
# =============================================================================

def load_calibrated_params(calib_dir: Path) -> Dict[str, float]:
    """Load emulator-calibrated parameters from calibration results."""
    param_file = calib_dir / "emulator_calibrated_params.txt"

    if not param_file.exists():
        raise FileNotFoundError(f"Calibrated parameters not found: {param_file}")

    with open(param_file, 'r') as f:
        lines = f.readlines()

    # First line: parameter names
    # Second line: parameter values
    param_names = lines[0].strip().split()
    param_values = [float(v) for v in lines[1].strip().split()]

    return dict(zip(param_names, param_values))


def load_default_params(base_tbl_file: Path, var_info: pd.DataFrame) -> Dict[str, float]:
    """
    Extract default parameter values from base TBL file.

    Special handling for LAI_MONTHLY: reads from LAI_JAN to get the default value.
    """
    with open(base_tbl_file, 'r') as f:
        base_text = f.read()

    default_params = {}

    for _, row in var_info.iterrows():
        col_name = row['column_name']
        section = row['section']
        var = row['variable']
        typ = int(row['type'])

        try:
            start, end = find_section_span(base_text, section)
            section_text = base_text[start:end]

            # Special handling for LAI_MONTHLY: read from LAI_JAN
            search_var = 'LAI_JAN' if var == 'LAI_MONTHLY' else var

            # Extract current value
            pattern = re.compile(rf"^[ \t]*{re.escape(search_var)}[ \t]*=([^\n]*)", flags=re.MULTILINE)
            m = pattern.search(section_text)
            if m:
                rhs = m.group(1)
                if "!" in rhs:
                    rhs = rhs.split("!")[0]
                parts = [p.strip() for p in rhs.strip().split(",")]
                idx = typ - 1
                if 0 <= idx < len(parts):
                    default_params[col_name] = float(parts[idx])
        except Exception as e:
            print(f"Warning: Could not extract default value for {col_name}: {e}")

    return default_params


# =============================================================================
# COMBINATION GENERATION
# =============================================================================

def generate_oat_combinations(
    default_params: Dict[str, float],
    calibrated_params: Dict[str, float],
    param_groups: Dict[str, List[str]]
) -> List[Tuple[str, Dict[str, float]]]:
    """
    Generate One-at-a-Time parameter combinations.

    Each combination changes one parameter group from default to calibrated,
    keeping all other parameters at their default values.

    Returns:
        List of (combination_name, params_dict) tuples
    """
    combinations = []

    # Baseline: all default
    combinations.append(('baseline_default', default_params.copy()))

    # For each parameter group, change only that group to calibrated
    for group_name, param_list in param_groups.items():
        params = default_params.copy()
        for param in param_list:
            if param in calibrated_params:
                params[param] = calibrated_params[param]
        combinations.append((f'oat_{group_name}', params))

    # Full calibrated (for reference)
    combinations.append(('full_calibrated', calibrated_params.copy()))

    return combinations


def generate_shapley_combinations(
    default_params: Dict[str, float],
    calibrated_params: Dict[str, float],
    param_groups: Dict[str, List[str]]
) -> List[Tuple[str, Dict[str, float], frozenset]]:
    """
    Generate all 2^n parameter combinations for Shapley value calculation.

    Each combination is a subset of parameter groups set to calibrated values,
    with the remaining groups at default values.

    Returns:
        List of (combination_name, params_dict, active_groups_set) tuples
    """
    combinations = []
    group_names = list(param_groups.keys())
    n_groups = len(group_names)

    # Generate all 2^n subsets
    for i in range(2 ** n_groups):
        # Determine which groups are "active" (calibrated) in this combination
        active_groups = frozenset(
            group_names[j] for j in range(n_groups) if (i >> j) & 1
        )

        # Build parameter dictionary
        params = default_params.copy()
        for group_name in active_groups:
            for param in param_groups[group_name]:
                if param in calibrated_params:
                    params[param] = calibrated_params[param]

        # Create readable combination name
        if len(active_groups) == 0:
            combo_name = 'shapley_empty'
        elif len(active_groups) == n_groups:
            combo_name = 'shapley_full'
        else:
            combo_name = 'shapley_' + '_'.join(sorted(active_groups))

        combinations.append((combo_name, params, active_groups))

    return combinations


# =============================================================================
# RUN DIRECTORY PREPARATION
# =============================================================================

def prepare_run_directory(
    template_dir: Path,
    output_dir: Path,
    tbl_text: str,
    combo_name: str
) -> Path:
    """
    Prepare a run directory for a parameter combination.

    Copies template directory structure and replaces NoahmpTable.TBL.
    """
    run_dir = output_dir / f"run_{combo_name}"

    if run_dir.exists():
        shutil.rmtree(run_dir)

    # Copy template directory
    shutil.copytree(template_dir, run_dir, symlinks=True)

    # Write new TBL file
    tbl_path = run_dir / "NoahmpTable.TBL"
    with open(tbl_path, 'w') as f:
        f.write(tbl_text)

    # Create output directory if needed
    output_subdir = run_dir / "output"
    output_subdir.mkdir(exist_ok=True)

    return run_dir


# =============================================================================
# METRICS CALCULATION
# =============================================================================

def calculate_metrics(obs: np.ndarray, sim: np.ndarray) -> Dict[str, float]:
    """Calculate performance metrics between observations and simulations."""
    mask = ~(np.isnan(obs) | np.isnan(sim))
    obs_clean = obs[mask]
    sim_clean = sim[mask]

    if len(obs_clean) == 0:
        return {m: np.nan for m in METRICS + ['N']}

    bias = np.mean(sim_clean - obs_clean)
    mae = np.mean(np.abs(sim_clean - obs_clean))
    rmse = np.sqrt(np.mean((sim_clean - obs_clean)**2))

    obs_mean = np.mean(obs_clean)
    ss_res = np.sum((obs_clean - sim_clean)**2)
    ss_tot = np.sum((obs_clean - obs_mean)**2)

    if ss_tot > 1e-10:
        r2 = 1 - (ss_res / ss_tot)
    else:
        r2 = np.nan

    return {
        'RMSE': rmse,
        'BIAS': bias,
        'MAE': mae,
        'R2': r2,
        'N': len(obs_clean)
    }


def load_simulation_output(output_dir: Path) -> Optional[pd.DataFrame]:
    """Load and parse Noah-MP simulation output."""
    # Look for output CSV files
    csv_files = list(output_dir.glob("*.csv"))

    if not csv_files:
        # Try to find NetCDF files and convert them
        nc_files = list(output_dir.glob("*.nc"))
        if nc_files:
            print(f"  Found {len(nc_files)} NetCDF files, conversion needed")
            return None
        return None

    # Read the CSV file
    try:
        df = pd.read_csv(csv_files[0])
        if 'timestamp' in df.columns:
            df['timestamp'] = pd.to_datetime(df['timestamp'])
        elif 'date' in df.columns:
            df['timestamp'] = pd.to_datetime(df['date'])
        return df
    except Exception as e:
        print(f"  Error reading output: {e}")
        return None


def load_observation_data(time_range: Dict = None) -> Optional[pd.DataFrame]:
    """Load observation data."""
    if not OBS_FILE.exists():
        print(f"Warning: Observation file not found: {OBS_FILE}")
        return None

    obs_df = pd.read_csv(OBS_FILE)

    # Handle timestamp column
    if 'date' in obs_df.columns:
        obs_df['timestamp'] = pd.to_datetime(obs_df['date'])
    elif 'timestamp' in obs_df.columns:
        obs_df['timestamp'] = pd.to_datetime(obs_df['timestamp'])

    # Apply column mapping
    rename_map = {}
    for obs_name, sim_name in OBS_TO_SIM_MAPPING.items():
        if obs_name in obs_df.columns and sim_name not in obs_df.columns:
            rename_map[obs_name] = sim_name
    if rename_map:
        obs_df = obs_df.rename(columns=rename_map)

    # Filter by time range
    if time_range is not None:
        start = pd.to_datetime(time_range['start'])
        end = pd.to_datetime(time_range['end'])
        obs_df = obs_df[(obs_df['timestamp'] >= start) & (obs_df['timestamp'] <= end)]

    return obs_df


def compute_metrics_for_combination(
    sim_df: pd.DataFrame,
    obs_df: pd.DataFrame,
    variables: List[str],
    period: str = 'full'
) -> Dict[str, Dict[str, float]]:
    """Compute metrics for all target variables for a given combination."""

    # Determine time range based on period
    if period == 'calibration':
        time_range = TIME_RANGE_CALIBRATION
    elif period == 'validation':
        time_range = TIME_RANGE_VALIDATION
    else:
        time_range = TIME_RANGE_FULL

    start = pd.to_datetime(time_range['start'])
    end = pd.to_datetime(time_range['end'])

    results = {}

    for var in variables:
        if var not in sim_df.columns:
            print(f"    Warning: {var} not in simulation output")
            results[var] = {m: np.nan for m in METRICS}
            continue

        if var not in obs_df.columns:
            print(f"    Warning: {var} not in observation data")
            results[var] = {m: np.nan for m in METRICS}
            continue

        # Merge on timestamp
        merged = pd.merge(
            obs_df[['timestamp', var]].rename(columns={var: 'obs'}),
            sim_df[['timestamp', var]].rename(columns={var: 'sim'}),
            on='timestamp',
            how='inner'
        )

        # Filter by time period
        merged = merged[(merged['timestamp'] >= start) & (merged['timestamp'] <= end)]

        if merged.empty:
            results[var] = {m: np.nan for m in METRICS}
            continue

        results[var] = calculate_metrics(merged['obs'].values, merged['sim'].values)

    return results


# =============================================================================
# SHAPLEY VALUE CALCULATION
# =============================================================================

def compute_shapley_values(
    metrics_by_combo: Dict[str, Dict[str, Dict[str, float]]],
    param_groups: Dict[str, List[str]],
    variables: List[str],
    metric_name: str = 'RMSE'
) -> Dict[str, Dict[str, float]]:
    """
    Compute Shapley values for each parameter group.

    Shapley value formula:
    phi_i = sum_{S in N\{i}} [|S|! (n-|S|-1)! / n!] * [v(S u {i}) - v(S)]

    where:
    - N is the set of all parameter groups
    - S is a subset not containing i
    - v(S) is the "value" (performance improvement) of coalition S
    - n = |N| is the total number of parameter groups

    For RMSE/MAE: lower is better, so improvement = baseline - current
    For R2: higher is better, so improvement = current - baseline

    Returns:
        Dict[variable][param_group] -> Shapley value (contribution to improvement)
    """
    group_names = list(param_groups.keys())
    n = len(group_names)

    # Get baseline (empty set = all default) performance
    baseline_key = 'shapley_empty'

    shapley_values = {var: {} for var in variables}

    for var in variables:
        if baseline_key not in metrics_by_combo:
            print(f"  Warning: baseline not found for {var}")
            continue

        baseline_metric = metrics_by_combo[baseline_key].get(var, {}).get(metric_name, np.nan)

        if np.isnan(baseline_metric):
            print(f"  Warning: baseline metric is NaN for {var}")
            continue

        for i, group_i in enumerate(group_names):
            phi_i = 0.0

            # Iterate over all subsets S that don't contain i
            other_groups = [g for g in group_names if g != group_i]

            for k in range(len(other_groups) + 1):
                for S in itertools.combinations(other_groups, k):
                    S_set = frozenset(S)
                    S_with_i = S_set | {group_i}

                    # Build combo names
                    if len(S_set) == 0:
                        combo_S = 'shapley_empty'
                    elif len(S_set) == n - 1:
                        # All except one
                        combo_S = 'shapley_' + '_'.join(sorted(S_set))
                    else:
                        combo_S = 'shapley_' + '_'.join(sorted(S_set))

                    if len(S_with_i) == n:
                        combo_S_i = 'shapley_full'
                    else:
                        combo_S_i = 'shapley_' + '_'.join(sorted(S_with_i))

                    # Get metric values
                    v_S = metrics_by_combo.get(combo_S, {}).get(var, {}).get(metric_name, np.nan)
                    v_S_i = metrics_by_combo.get(combo_S_i, {}).get(var, {}).get(metric_name, np.nan)

                    if np.isnan(v_S) or np.isnan(v_S_i):
                        continue

                    # Compute marginal contribution
                    # For RMSE/MAE/BIAS: improvement = baseline - current (lower is better)
                    # For R2: improvement = current - baseline (higher is better)
                    if metric_name in ['R2']:
                        # Higher is better: v_S_i - v_S represents improvement from adding i
                        marginal = v_S_i - v_S
                    else:
                        # Lower is better: v_S - v_S_i represents improvement from adding i
                        # (if adding i reduces RMSE, that's positive improvement)
                        marginal = v_S - v_S_i

                    # Compute weight: |S|! * (n-|S|-1)! / n!
                    s_size = len(S_set)
                    import math
                    weight = (math.factorial(s_size) * math.factorial(n - s_size - 1)) / math.factorial(n)

                    phi_i += weight * marginal

            shapley_values[var][group_i] = phi_i

    return shapley_values

def compute_oat_contributions(
    metrics_by_combo: Dict[str, Dict[str, Dict[str, float]]],
    param_groups: Dict[str, List[str]],
    variables: List[str],
    metric_name: str = 'RMSE'
) -> Dict[str, Dict[str, float]]:
    """
    Compute OAT (One-at-a-Time) contributions for each parameter group.

    Contribution = performance(with_param_calibrated) - performance(baseline)

    Returns:
        Dict[variable][param_group] -> OAT contribution
    """
    baseline_key = 'baseline_default'

    oat_contributions = {var: {} for var in variables}

    for var in variables:
        baseline_metric = metrics_by_combo.get(baseline_key, {}).get(var, {}).get(metric_name, np.nan)

        if np.isnan(baseline_metric):
            print(f"  Warning: baseline metric is NaN for {var}")
            continue

        for group_name in param_groups.keys():
            combo_key = f'oat_{group_name}'
            combo_metric = metrics_by_combo.get(combo_key, {}).get(var, {}).get(metric_name, np.nan)

            if np.isnan(combo_metric):
                oat_contributions[var][group_name] = np.nan
                continue

            # Compute contribution
            if metric_name in ['R2']:
                # Higher is better
                contribution = combo_metric - baseline_metric
            else:
                # Lower is better
                contribution = baseline_metric - combo_metric

            oat_contributions[var][group_name] = contribution

    return oat_contributions


# =============================================================================
# WATERFALL PLOT - Improved version with relative scaling
# =============================================================================

def create_waterfall_plot(
    shapley_values: Dict[str, float],
    baseline_value: float,
    final_value: float,
    metric_name: str = 'RMSE',
    variable_name: str = 'LH',
    normalize: bool = True,
    figsize: Tuple[float, float] = None,
    colors: Dict[str, str] = None,
    save_path: Optional[Path] = None,
    show_plot: bool = True
) -> plt.Figure:
    """
    Create improved Waterfall plot showing parameter contributions to performance improvement.

    Improvements:
    1. Relative scaling: Y-axis is scaled based on actual data range, not 0-100%
    2. Labels outside bars: Change values are placed above/below bars for readability
    3. A4-compatible styling: Uses plot_settings.py for consistent publication-quality output

    Parameters
    ----------
    shapley_values : Dict[str, float]
        Parameter group -> Shapley contribution dictionary
    baseline_value : float
        Performance metric value with default parameters
    final_value : float
        Performance metric value with calibrated parameters
    metric_name : str
        Metric name (e.g., 'RMSE', 'R2')
    variable_name : str
        Target variable name (e.g., 'LH', 'HFX')
    normalize : bool
        True: Show as percentage of baseline (with relative scaling)
        False: Show absolute values
    figsize : tuple
        Figure size (width, height). If None, uses A4-compatible default.
    colors : dict
        Custom colors: 'positive', 'negative', 'baseline', 'final'
    save_path : Path
        Save path for the figure
    show_plot : bool
        Whether to display the figure

    Returns
    -------
    matplotlib.Figure
    """

    # Set A4-compatible figure size if not specified
    if figsize is None:
        if HAS_PLOT_SETTINGS:
            figsize = (USABLE_WIDTH, USABLE_WIDTH * 0.6)  # ~6.27 x 3.76 inches
        else:
            figsize = (8, 5)

    # Font sizes for A4 printing
    if HAS_PLOT_SETTINGS:
        title_size = FONT_SIZES['subtitle']
        label_size = FONT_SIZES['axis_label']
        tick_size = FONT_SIZES['tick_label']
        annotation_size = FONT_SIZES['annotation']
        legend_size = FONT_SIZES['legend']
    else:
        title_size = 11
        label_size = 10
        tick_size = 9
        annotation_size = 8
        legend_size = 9

    # Default color scheme
    if colors is None:
        colors = {
            'positive': '#2ecc71',      # Green - improvement
            'negative': '#e74c3c',      # Red - degradation
            'baseline': '#3498db',      # Blue - baseline
            'final': '#9b59b6',         # Purple - final
            'connector': '#7f8c8d'      # Gray - connector lines
        }

    # Sort by absolute contribution (descending)
    sorted_items = sorted(shapley_values.items(), key=lambda x: abs(x[1]), reverse=True)
    param_names = [item[0] for item in sorted_items]
    contributions = [item[1] for item in sorted_items]

    # Determine metric direction: lower is better or higher is better
    lower_is_better = metric_name.upper() in ['RMSE', 'MAE', 'BIAS', 'MSE']

    # Normalize and scale
    if normalize:
        scale_factor = 100.0 / baseline_value
        baseline_display = 100.0
        contributions_display = [c * scale_factor for c in contributions]
        if lower_is_better:
            final_display = baseline_display - sum(contributions_display)
        else:
            final_display = baseline_display + sum(contributions_display)
        unit_label = '% of Default'
    else:
        baseline_display = baseline_value
        contributions_display = contributions
        if lower_is_better:
            final_display = baseline_display - sum(contributions_display)
        else:
            final_display = baseline_display + sum(contributions_display)
        unit_label = f'{metric_name}'

    # Build chart data
    labels = ['Default'] + param_names + ['Calibrated']
    n_bars = len(labels)

    bar_bottoms = np.zeros(n_bars)
    bar_heights = np.zeros(n_bars)
    bar_colors = []

    # First bar: baseline value
    bar_heights[0] = baseline_display
    bar_bottoms[0] = 0
    bar_colors.append(colors['baseline'])

    # Middle bars: parameter contributions
    running_value = baseline_display
    cumulative_values = [baseline_display]

    for i, contrib in enumerate(contributions_display):
        if lower_is_better:
            if contrib > 0:  # Positive contribution = RMSE decrease = improvement
                bar_heights[i + 1] = contrib
                bar_bottoms[i + 1] = running_value - contrib
                bar_colors.append(colors['positive'])
                running_value -= contrib
            else:  # Negative contribution = RMSE increase = degradation
                bar_heights[i + 1] = abs(contrib)
                bar_bottoms[i + 1] = running_value
                bar_colors.append(colors['negative'])
                running_value += abs(contrib)
        else:  # Higher is better (e.g., R2)
            if contrib > 0:
                bar_heights[i + 1] = contrib
                bar_bottoms[i + 1] = running_value
                bar_colors.append(colors['positive'])
                running_value += contrib
            else:
                bar_heights[i + 1] = abs(contrib)
                bar_bottoms[i + 1] = running_value - abs(contrib)
                bar_colors.append(colors['negative'])
                running_value -= abs(contrib)
        cumulative_values.append(running_value)

    # Last bar: calibrated result
    bar_heights[-1] = final_display
    bar_bottoms[-1] = 0
    bar_colors.append(colors['final'])

    # =========================================================================
    # IMPROVED: Calculate relative y-axis range
    # =========================================================================
    all_bar_tops = bar_bottoms + bar_heights
    all_bar_bottoms = bar_bottoms.copy()

    # Find the actual data range (excluding the full baseline/final bars)
    if len(cumulative_values) > 2:
        mid_values = cumulative_values[1:-1] + [cumulative_values[0], cumulative_values[-1]]
        data_min = min(mid_values)
        data_max = max(mid_values)
    else:
        data_min = min(baseline_display, final_display)
        data_max = max(baseline_display, final_display)

    # Add padding
    data_range = data_max - data_min
    if data_range < 1e-6:
        data_range = max(baseline_display, final_display) * 0.1

    padding = data_range * 0.3
    y_min = max(0, data_min - padding)
    y_max = data_max + padding

    # Ensure we show enough context (at least show from 0 to max if range is small)
    if normalize and (y_max - y_min) < 30:
        # If range is too narrow, expand it
        y_center = (baseline_display + final_display) / 2
        y_min = max(0, y_center - 20)
        y_max = y_center + 20

    # Create figure
    fig, ax = plt.subplots(figsize=figsize)

    x_positions = np.arange(n_bars)
    bar_width = 0.6

    # Draw bars
    bars = ax.bar(x_positions, bar_heights, bar_width, bottom=bar_bottoms,
                  color=bar_colors, edgecolor='white', linewidth=1.2)

    # Draw connector lines
    for i in range(len(cumulative_values) - 1):
        ax.hlines(y=cumulative_values[i],
                  xmin=x_positions[i] + bar_width/2 - 0.02,
                  xmax=x_positions[i + 1] - bar_width/2 + 0.02,
                  colors=colors['connector'], linestyles='--', linewidth=1.0, alpha=0.6)

    # =========================================================================
    # IMPROVED: Add value labels OUTSIDE bars
    # =========================================================================
    for i, (height, bottom) in enumerate(zip(bar_heights, bar_bottoms)):
        bar_top = bottom + height

        if i == 0 or i == n_bars - 1:
            # Baseline and final: show absolute value above bar
            if bar_top > y_max * 0.9:
                # If bar is near top, place label inside
                y_pos = bar_top - height * 0.1
                va = 'top'
            else:
                y_pos = bar_top + (y_max - y_min) * 0.02
                va = 'bottom'
            value_text = f'{bar_top:.1f}'
            color = 'black'
        else:
            # Contribution bars: show change value outside bar
            contrib_val = contributions_display[i - 1]
            if lower_is_better:
                # For RMSE: positive contribution = decrease (improvement)
                value_text = f'{contrib_val:+.1f}' if contrib_val >= 0 else f'{contrib_val:.1f}'
            else:
                value_text = f'{contrib_val:+.1f}'

            # Determine label position: above or below the bar
            if contrib_val > 0:
                if lower_is_better:
                    # Improvement bar goes down, label below
                    y_pos = bottom - (y_max - y_min) * 0.02
                    va = 'top'
                else:
                    # Improvement bar goes up, label above
                    y_pos = bar_top + (y_max - y_min) * 0.02
                    va = 'bottom'
            else:
                if lower_is_better:
                    # Degradation bar goes up, label above
                    y_pos = bar_top + (y_max - y_min) * 0.02
                    va = 'bottom'
                else:
                    # Degradation bar goes down, label below
                    y_pos = bottom - (y_max - y_min) * 0.02
                    va = 'top'

            color = 'black'

        ax.text(x_positions[i], y_pos, value_text,
                ha='center', va=va, fontsize=annotation_size, fontweight='bold', color=color)

    # Set axis properties
    ax.set_xticks(x_positions)
    ax.set_xticklabels(labels, rotation=45, ha='right', fontsize=tick_size)
    ax.set_ylabel(unit_label, fontsize=label_size)
    ax.set_ylim(y_min, y_max)

    # Title with improvement summary
    improvement_pct = (1 - final_value / baseline_value) * 100 if lower_is_better else (final_value / baseline_value - 1) * 100
    title = f'Parameter Contribution: {variable_name} {metric_name}\n'
    title += f'(Default: {baseline_value:.3f} -> Calibrated: {final_value:.3f}, '
    title += f'{"Improvement" if improvement_pct > 0 else "Change"}: {abs(improvement_pct):.1f}%)'
    ax.set_title(title, fontsize=title_size, fontweight='bold', pad=10)

    # Legend
    legend_elements = [
        mpatches.Patch(facecolor=colors['baseline'], edgecolor='white', label='Default'),
        mpatches.Patch(facecolor=colors['positive'], edgecolor='white', label='Improvement'),
        mpatches.Patch(facecolor=colors['negative'], edgecolor='white', label='Degradation'),
        mpatches.Patch(facecolor=colors['final'], edgecolor='white', label='Calibrated'),
    ]
    ax.legend(handles=legend_elements, loc='upper right', framealpha=0.9, fontsize=legend_size)

    # Grid and borders
    ax.yaxis.grid(True, linestyle='--', alpha=0.3)
    ax.set_axisbelow(True)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    plt.tight_layout()

    if save_path:
        # Save in both PNG and PDF formats
        fig.savefig(save_path, dpi=300, bbox_inches='tight', facecolor='white')
        pdf_path = str(save_path).replace('.png', '.pdf')
        fig.savefig(pdf_path, format='pdf', bbox_inches='tight', facecolor='white')
        print(f"  Saved: {save_path}")
        print(f"  Saved: {pdf_path}")

    if show_plot:
        plt.show()

    return fig


# =============================================================================
# Generate all waterfall plots
# =============================================================================

def create_waterfall_plots_all_variables(
    results: Dict,
    metrics_by_combo: Dict,
    output_dir: Path,
    method: str = 'shapley',
    period: str = 'full',
    variables: List[str] = None,
    metrics: List[str] = None
):
    """
    Generate waterfall plots for all variables and metrics.
    """

    if variables is None:
        variables = ['LH', 'SOIL_M', 'HFX']
    if metrics is None:
        metrics = ['RMSE', 'R2']

    plots_dir = output_dir / 'plots'
    plots_dir.mkdir(exist_ok=True)

    # Determine baseline and calibrated combination names
    if method == 'shapley':
        baseline_key = 'shapley_empty'
        calibrated_key = 'shapley_full'
    else:
        baseline_key = 'baseline_default'
        calibrated_key = 'full_calibrated'

    for var in variables:
        for metric in metrics:
            print(f"Creating waterfall plot: {var} - {metric}")

            # Get Shapley values
            shapley_vals = results.get('importance', {}).get(metric, {}).get(var, {})

            if not shapley_vals:
                print(f"  No Shapley values found, skipping...")
                continue

            # Get baseline and final values
            baseline_val = metrics_by_combo.get(baseline_key, {}).get(var, {}).get(metric)
            final_val = metrics_by_combo.get(calibrated_key, {}).get(var, {}).get(metric)

            if baseline_val is None or final_val is None:
                print(f"  Missing baseline or final values, skipping...")
                continue

            save_path = plots_dir / f'waterfall_{var}_{metric}_{method}_{period}.png'

            try:
                create_waterfall_plot(
                    shapley_values=shapley_vals,
                    baseline_value=baseline_val,
                    final_value=final_val,
                    metric_name=metric,
                    variable_name=var,
                    normalize=True,
                    save_path=save_path,
                    show_plot=False
                )
            except Exception as e:
                print(f"  Error: {e}")

    # Create summary plot
    create_summary_waterfall(results, metrics_by_combo, output_dir, method, period, variables)

    plt.close('all')


# =============================================================================
# Summary waterfall: multiple variables side by side
# =============================================================================

def create_summary_waterfall(
    results: Dict,
    metrics_by_combo: Dict,
    output_dir: Path,
    method: str = 'shapley',
    period: str = 'full',
    variables: List[str] = None,
    metric: str = 'RMSE'
):
    """
    Create summary plot with multiple variables side by side.
    Uses A4-compatible sizing and improved relative scaling.
    """

    if variables is None:
        variables = ['LH', 'SOIL_M', 'HFX']

    if method == 'shapley':
        baseline_key = 'shapley_empty'
        calibrated_key = 'shapley_full'
    else:
        baseline_key = 'baseline_default'
        calibrated_key = 'full_calibrated'

    n_vars = len(variables)

    # A4-compatible figure size
    if HAS_PLOT_SETTINGS:
        fig_width = USABLE_WIDTH
        fig_height = USABLE_WIDTH * 0.5
        title_size = FONT_SIZES['title']
        subtitle_size = FONT_SIZES['subtitle']
        label_size = FONT_SIZES['axis_label']
        tick_size = FONT_SIZES['tick_label']
        annotation_size = FONT_SIZES['small']
        legend_size = FONT_SIZES['legend']
    else:
        fig_width = 10
        fig_height = 5
        title_size = 12
        subtitle_size = 11
        label_size = 10
        tick_size = 9
        annotation_size = 7
        legend_size = 9

    fig, axes = plt.subplots(1, n_vars, figsize=(fig_width, fig_height))

    if n_vars == 1:
        axes = [axes]

    colors = {
        'positive': '#2ecc71',
        'negative': '#e74c3c',
        'baseline': '#3498db',
        'final': '#9b59b6',
        'connector': '#7f8c8d'
    }

    lower_is_better = metric.upper() in ['RMSE', 'MAE', 'BIAS', 'MSE']

    for idx, (var, ax) in enumerate(zip(variables, axes)):
        shapley_vals = results.get('importance', {}).get(metric, {}).get(var, {})
        baseline_val = metrics_by_combo.get(baseline_key, {}).get(var, {}).get(metric)
        final_val = metrics_by_combo.get(calibrated_key, {}).get(var, {}).get(metric)

        if not shapley_vals or baseline_val is None or final_val is None:
            ax.text(0.5, 0.5, 'No data', ha='center', va='center', transform=ax.transAxes)
            ax.set_title(f'{var}', fontsize=subtitle_size)
            continue

        sorted_items = sorted(shapley_vals.items(), key=lambda x: abs(x[1]), reverse=True)
        param_names = [item[0] for item in sorted_items]
        contributions = [item[1] for item in sorted_items]

        # Normalize
        scale_factor = 100.0 / baseline_val
        baseline_display = 100.0
        contributions_display = [c * scale_factor for c in contributions]
        if lower_is_better:
            final_display = baseline_display - sum(contributions_display)
        else:
            final_display = baseline_display + sum(contributions_display)

        # Build bar data
        labels = ['Def'] + [p[:6] for p in param_names] + ['Cal']  # Shortened labels
        n_bars = len(labels)
        bar_bottoms = np.zeros(n_bars)
        bar_heights = np.zeros(n_bars)
        bar_colors = []

        bar_heights[0] = baseline_display
        bar_colors.append(colors['baseline'])

        running_value = baseline_display
        cumulative_values = [baseline_display]

        for i, contrib in enumerate(contributions_display):
            if lower_is_better:
                if contrib > 0:
                    bar_heights[i + 1] = contrib
                    bar_bottoms[i + 1] = running_value - contrib
                    bar_colors.append(colors['positive'])
                    running_value -= contrib
                else:
                    bar_heights[i + 1] = abs(contrib)
                    bar_bottoms[i + 1] = running_value
                    bar_colors.append(colors['negative'])
                    running_value += abs(contrib)
            else:
                if contrib > 0:
                    bar_heights[i + 1] = contrib
                    bar_bottoms[i + 1] = running_value
                    bar_colors.append(colors['positive'])
                    running_value += contrib
                else:
                    bar_heights[i + 1] = abs(contrib)
                    bar_bottoms[i + 1] = running_value - abs(contrib)
                    bar_colors.append(colors['negative'])
                    running_value -= abs(contrib)
            cumulative_values.append(running_value)

        bar_heights[-1] = final_display
        bar_colors.append(colors['final'])

        # Calculate relative y-axis range
        if len(cumulative_values) > 2:
            mid_values = cumulative_values[1:-1] + [cumulative_values[0], cumulative_values[-1]]
            data_min = min(mid_values)
            data_max = max(mid_values)
        else:
            data_min = min(baseline_display, final_display)
            data_max = max(baseline_display, final_display)

        data_range = data_max - data_min
        if data_range < 1e-6:
            data_range = max(baseline_display, final_display) * 0.1

        padding = data_range * 0.35
        y_min = max(0, data_min - padding)
        y_max = data_max + padding

        # Draw bars
        x_positions = np.arange(n_bars)
        bar_width = 0.65

        ax.bar(x_positions, bar_heights, bar_width, bottom=bar_bottoms,
               color=bar_colors, edgecolor='white', linewidth=0.8)

        # Add value labels outside bars
        for i, (height, bottom) in enumerate(zip(bar_heights, bar_bottoms)):
            bar_top = bottom + height

            if i == 0 or i == n_bars - 1:
                y_pos = bar_top + (y_max - y_min) * 0.02
                va = 'bottom'
                value_text = f'{bar_top:.0f}'
            else:
                contrib_val = contributions_display[i - 1]
                value_text = f'{contrib_val:+.1f}'

                if contrib_val > 0:
                    if lower_is_better:
                        y_pos = bottom - (y_max - y_min) * 0.02
                        va = 'top'
                    else:
                        y_pos = bar_top + (y_max - y_min) * 0.02
                        va = 'bottom'
                else:
                    if lower_is_better:
                        y_pos = bar_top + (y_max - y_min) * 0.02
                        va = 'bottom'
                    else:
                        y_pos = bottom - (y_max - y_min) * 0.02
                        va = 'top'

            ax.text(x_positions[i], y_pos, value_text,
                    ha='center', va=va, fontsize=annotation_size, fontweight='bold')

        ax.set_xticks(x_positions)
        ax.set_xticklabels(labels, rotation=55, ha='right', fontsize=tick_size - 1)
        ax.set_ylabel('% of Default' if idx == 0 else '', fontsize=label_size)
        ax.set_ylim(y_min, y_max)

        if lower_is_better:
            improvement = (1 - final_val / baseline_val) * 100
        else:
            improvement = (final_val / baseline_val - 1) * 100
        ax.set_title(f'{var}\n({baseline_val:.2f}->{final_val:.2f}, {improvement:+.1f}%)',
                     fontsize=subtitle_size, fontweight='bold')

        ax.yaxis.grid(True, linestyle='--', alpha=0.3)
        ax.set_axisbelow(True)
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)

    # Legend
    legend_elements = [
        mpatches.Patch(facecolor=colors['baseline'], edgecolor='white', label='Default'),
        mpatches.Patch(facecolor=colors['positive'], edgecolor='white', label='Improvement'),
        mpatches.Patch(facecolor=colors['negative'], edgecolor='white', label='Degradation'),
        mpatches.Patch(facecolor=colors['final'], edgecolor='white', label='Calibrated'),
    ]
    fig.legend(handles=legend_elements, loc='upper center', ncol=4,
               bbox_to_anchor=(0.5, 1.02), framealpha=0.9, fontsize=legend_size)

    plt.suptitle(f'Parameter Contribution Analysis ({metric}, {period.capitalize()} Period)',
                 fontsize=title_size, fontweight='bold', y=1.08)

    plt.tight_layout()

    plots_dir = output_dir / 'plots'
    plots_dir.mkdir(exist_ok=True)

    # Save in both formats
    save_path = plots_dir / f'waterfall_summary_{metric}_{method}_{period}.png'
    fig.savefig(save_path, dpi=300, bbox_inches='tight', facecolor='white')

    pdf_path = plots_dir / f'waterfall_summary_{metric}_{method}_{period}.pdf'
    fig.savefig(pdf_path, format='pdf', bbox_inches='tight', facecolor='white')

    print(f"\nSummary figure saved: {save_path}")
    print(f"Summary figure saved: {pdf_path}")

    plt.close(fig)
    return fig


# =============================================================================
# MAIN WORKFLOW
# =============================================================================

def prepare_combinations(
    calib_dir: Path,
    output_dir: Path,
    method: str = 'shapley'
) -> Tuple[List, Path]:
    """
    Prepare parameter combinations and run directories.

    Returns:
        List of combination info and path to combination metadata file
    """
    print(f"\n{'='*60}")
    print(f"PARAMETER IMPORTANCE ANALYSIS - PREPARATION")
    print(f"Method: {method.upper()}")
    print(f"{'='*60}\n")

    # Load parameters
    print("Loading parameters...")
    var_info = build_var_info()
    default_params = load_default_params(BASE_TBL_FILE, var_info)
    calibrated_params = load_calibrated_params(calib_dir)

    print(f"  Default parameters: {len(default_params)}")
    print(f"  Calibrated parameters: {len(calibrated_params)}")

    # Print parameter comparison
    print("\nParameter comparison (default -> calibrated):")
    for group_name, param_list in PARAM_GROUPS.items():
        print(f"  {group_name}:")
        for param in param_list:
            def_val = default_params.get(param, 'N/A')
            cal_val = calibrated_params.get(param, 'N/A')
            if isinstance(def_val, float) and isinstance(cal_val, float):
                if abs(def_val) < 1e-3:
                    print(f"    {param}: {def_val:.6e} -> {cal_val:.6e}")
                else:
                    print(f"    {param}: {def_val:.4f} -> {cal_val:.4f}")

    # Generate combinations
    print(f"\nGenerating {method} combinations...")

    if method == 'oat':
        combinations = generate_oat_combinations(default_params, calibrated_params, PARAM_GROUPS)
        combo_info = [(name, params) for name, params in combinations]
    else:  # shapley
        combinations = generate_shapley_combinations(default_params, calibrated_params, PARAM_GROUPS)
        combo_info = [(name, params, list(active)) for name, params, active in combinations]

    print(f"  Generated {len(combo_info)} combinations")

    # Load base TBL
    with open(BASE_TBL_FILE, 'r') as f:
        base_tbl = f.read()

    # Get template run directory
    template_dir = calib_dir / "run_default"
    if not template_dir.exists():
        template_dir = calib_dir / "run_emulator"

    if not template_dir.exists():
        raise FileNotFoundError(f"No template run directory found in {calib_dir}")

    # Create output directory
    importance_dir = output_dir / "importance_analysis"
    importance_dir.mkdir(parents=True, exist_ok=True)

    # Prepare run directories
    print(f"\nPreparing run directories in {importance_dir}...")
    run_dirs = []

    for combo_data in combo_info:
        if method == 'oat':
            name, params = combo_data
            active_groups = None
        else:
            name, params, active_groups = combo_data

        # Generate TBL text
        tbl_text = apply_params_to_tbl(base_tbl, params, var_info)

        # Prepare run directory
        run_dir = prepare_run_directory(template_dir, importance_dir, tbl_text, name)
        run_dirs.append(str(run_dir))

        print(f"  Created: {run_dir.name}")

    # Save combination metadata
    metadata = {
        'method': method,
        'param_groups': {k: v for k, v in PARAM_GROUPS.items()},
        'default_params': default_params,
        'calibrated_params': calibrated_params,
        'combinations': [
            {
                'name': combo_data[0],
                'active_groups': list(combo_data[2]) if method == 'shapley' else None,
                'run_dir': str(importance_dir / f"run_{combo_data[0]}")
            }
            for combo_data in combo_info
        ],
        'target_variables': TARGET_VARIABLES,
        'metrics': METRICS,
    }

    metadata_file = importance_dir / "combination_metadata.json"
    with open(metadata_file, 'w') as f:
        json.dump(metadata, f, indent=2)

    print(f"\nMetadata saved to: {metadata_file}")
    print(f"Total run directories: {len(run_dirs)}")

    return combo_info, metadata_file


def analyze_results(
    calib_dir: Path,
    output_dir: Path,
    method: str = 'shapley',
    period: str = 'full'
) -> Dict:
    """
    Analyze model outputs and compute parameter importance.

    Returns:
        Dict containing Shapley values or OAT contributions
    """
    print(f"\n{'='*60}")
    print(f"PARAMETER IMPORTANCE ANALYSIS - RESULTS")
    print(f"Method: {method.upper()}, Period: {period}")
    print(f"{'='*60}\n")

    importance_dir = output_dir / "importance_analysis"
    metadata_file = importance_dir / "combination_metadata.json"

    if not metadata_file.exists():
        raise FileNotFoundError(f"Metadata file not found: {metadata_file}\nRun with --mode prepare first.")

    # Load metadata
    with open(metadata_file, 'r') as f:
        metadata = json.load(f)

    # Load observation data
    print("Loading observation data...")
    obs_df = load_observation_data()
    if obs_df is None:
        raise RuntimeError("Could not load observation data")
    print(f"  Observation records: {len(obs_df)}")

    # Compute metrics for each combination
    print("\nComputing metrics for each combination...")
    metrics_by_combo = {}

    for combo_info in metadata['combinations']:
        combo_name = combo_info['name']
        run_dir = Path(combo_info['run_dir'])

        # Try to load pre-computed output CSV (if available from validation script)
        output_csv = run_dir / "output" / "combined_output.csv"
        if not output_csv.exists():
            # Try other common output file patterns
            output_files = list((run_dir / "output").glob("*.csv"))
            if output_files:
                output_csv = output_files[0]
            else:
                print(f"  {combo_name}: No output CSV found (model may not have run yet)")
                continue

        sim_df = pd.read_csv(output_csv)
        if 'timestamp' in sim_df.columns:
            sim_df['timestamp'] = pd.to_datetime(sim_df['timestamp'])
        elif 'date' in sim_df.columns:
            sim_df['timestamp'] = pd.to_datetime(sim_df['date'])

        # Compute metrics
        metrics = compute_metrics_for_combination(sim_df, obs_df, TARGET_VARIABLES, period)
        metrics_by_combo[combo_name] = metrics

        # Print summary
        summary_parts = []
        for var in TARGET_VARIABLES[:2]:  # Just show first two variables
            rmse = metrics.get(var, {}).get('RMSE', np.nan)
            if not np.isnan(rmse):
                summary_parts.append(f"{var} RMSE={rmse:.3f}")
        print(f"  {combo_name}: {', '.join(summary_parts)}")

    if len(metrics_by_combo) == 0:
        print("\nNo simulation outputs found. Please run the models first.")
        return None

    # Compute importance values
    print(f"\nComputing {method} importance values...")

    results = {
        'method': method,
        'period': period,
        'param_groups': list(PARAM_GROUPS.keys()),
        'variables': TARGET_VARIABLES,
        'metrics': METRICS,
        'importance': {},
        'metrics_by_combo': metrics_by_combo,
    }

    if method == 'shapley':
        for metric in METRICS:
            shapley = compute_shapley_values(metrics_by_combo, PARAM_GROUPS, TARGET_VARIABLES, metric)
            results['importance'][metric] = shapley
    else:
        for metric in METRICS:
            oat = compute_oat_contributions(metrics_by_combo, PARAM_GROUPS, TARGET_VARIABLES, metric)
            results['importance'][metric] = oat

    # Print results
    print(f"\n{'='*60}")
    print(f"IMPORTANCE ANALYSIS RESULTS ({method.upper()})")
    print(f"{'='*60}")

    for metric in METRICS:
        print(f"\n{metric}:")
        importance_data = results['importance'][metric]

        for var in TARGET_VARIABLES:
            var_importance = importance_data.get(var, {})
            if not var_importance:
                continue

            print(f"\n  {var}:")

            # Sort by absolute importance
            sorted_groups = sorted(var_importance.items(), key=lambda x: abs(x[1]) if not np.isnan(x[1]) else 0, reverse=True)

            total = sum(v for _, v in sorted_groups if not np.isnan(v))

            for group, value in sorted_groups:
                if np.isnan(value):
                    print(f"    {group:15s}: N/A")
                else:
                    pct = (value / total * 100) if total != 0 else 0
                    sign = "+" if value > 0 else ""
                    print(f"    {group:15s}: {sign}{value:8.4f} ({pct:5.1f}%)")

            print(f"    {'Total':15s}: {total:8.4f}")

    # Save results
    results_file = importance_dir / f"importance_results_{method}_{period}.json"

    # Convert numpy types to Python types for JSON serialization
    def convert_to_python_types(obj):
        if isinstance(obj, dict):
            return {k: convert_to_python_types(v) for k, v in obj.items()}
        elif isinstance(obj, (np.floating, np.integer)):
            return float(obj)
        elif isinstance(obj, np.ndarray):
            return obj.tolist()
        else:
            return obj

    with open(results_file, 'w') as f:
        json.dump(convert_to_python_types(results), f, indent=2)

    print(f"\nResults saved to: {results_file}")

    # Also save as CSV for easy viewing
    for metric in METRICS:
        csv_file = importance_dir / f"importance_{metric}_{method}_{period}.csv"
        importance_data = results['importance'][metric]

        rows = []
        for var in TARGET_VARIABLES:
            row = {'variable': var}
            row.update(importance_data.get(var, {}))
            rows.append(row)

        df = pd.DataFrame(rows)
        df.to_csv(csv_file, index=False)

    print(f"CSV files saved to: {importance_dir}/importance_*.csv")

    print("\n" + "="*60)
    print("GENERATING WATERFALL PLOTS")
    print("="*60 + "\n")

    create_waterfall_plots_all_variables(
        results=results,
        metrics_by_combo=metrics_by_combo,
        output_dir=importance_dir,
        method=method,
        period=period,
        variables=TARGET_VARIABLES,
        metrics=['RMSE', 'R2']
    )

    print(f"\nPlots saved to: {importance_dir / 'plots'}")

    return results


# =============================================================================
# SHELL SCRIPT GENERATION
# =============================================================================

def generate_shell_script(
    calib_dir: Path,
    output_dir: Path,
    method: str = 'shapley'
) -> Path:
    """Generate shell script to run all parameter combinations."""

    importance_dir = output_dir / "importance_analysis"
    metadata_file = importance_dir / "combination_metadata.json"

    if not metadata_file.exists():
        raise FileNotFoundError(f"Metadata file not found: {metadata_file}\nRun with --mode prepare first.")

    with open(metadata_file, 'r') as f:
        metadata = json.load(f)

    script_file = output_dir / "07_importance_analysis.sh"

    with open(script_file, 'w') as f:
        f.write("#!/bin/bash\n")
        f.write("#\n")
        f.write("# Parameter Importance Analysis - Model Run Script\n")
        f.write(f"# Method: {method.upper()}\n")
        f.write(f"# Generated: $(date)\n")
        f.write("#\n")
        f.write("# This script runs Noah-MP for all parameter combinations.\n")
        f.write("# Each run may take several minutes depending on the simulation period.\n")
        f.write("#\n")
        f.write("# Usage:\n")
        f.write("#   bash 07_importance_analysis.sh          # Run all combinations\n")
        f.write("#   bash 07_importance_analysis.sh -n 4     # Run with 4 parallel jobs\n")
        f.write("#\n\n")

        f.write("set -e\n\n")

        f.write("# Parse arguments\n")
        f.write("PARALLEL_JOBS=1\n")
        f.write("while getopts 'n:' opt; do\n")
        f.write("    case $opt in\n")
        f.write("        n) PARALLEL_JOBS=$OPTARG ;;\n")
        f.write("    esac\n")
        f.write("done\n\n")

        f.write(f"IMPORTANCE_DIR=\"{importance_dir}\"\n")
        f.write(f"BASE_DIR=\"{BASE_DIR}\"\n\n")

        f.write("# List of run directories\n")
        f.write("RUN_DIRS=(\n")
        for combo_info in metadata['combinations']:
            f.write(f"    \"{combo_info['run_dir']}\"\n")
        f.write(")\n\n")

        f.write("# Function to run a single combination\n")
        f.write("run_combination() {\n")
        f.write("    local run_dir=$1\n")
        f.write("    local combo_name=$(basename $run_dir)\n")
        f.write("    \n")
        f.write("    echo \"Running: $combo_name\"\n")
        f.write("    cd \"$run_dir\"\n")
        f.write("    \n")
        f.write("    # Run Noah-MP\n")
        f.write("    ./hrldas.exe > run.log 2>&1\n")
        f.write("    \n")
        f.write("    if [ $? -eq 0 ]; then\n")
        f.write("        echo \"  $combo_name: SUCCESS\"\n")
        f.write("    else\n")
        f.write("        echo \"  $combo_name: FAILED (see $run_dir/run.log)\"\n")
        f.write("    fi\n")
        f.write("}\n\n")

        f.write("# Export function for parallel\n")
        f.write("export -f run_combination\n\n")

        f.write("echo \"Starting importance analysis model runs...\"\n")
        f.write(f"echo \"Total combinations: {len(metadata['combinations'])}\"\n")
        f.write("echo \"Parallel jobs: $PARALLEL_JOBS\"\n")
        f.write("echo \"\"\n\n")

        f.write("# Run combinations\n")
        f.write("if [ $PARALLEL_JOBS -gt 1 ]; then\n")
        f.write("    # Parallel execution\n")
        f.write("    printf '%s\\n' \"${RUN_DIRS[@]}\" | xargs -P $PARALLEL_JOBS -I {} bash -c 'run_combination \"$@\"' _ {}\n")
        f.write("else\n")
        f.write("    # Sequential execution\n")
        f.write("    for run_dir in \"${RUN_DIRS[@]}\"; do\n")
        f.write("        run_combination \"$run_dir\"\n")
        f.write("    done\n")
        f.write("fi\n\n")

        f.write("echo \"\"\n")
        f.write("echo \"All model runs completed.\"\n")
        f.write("echo \"Next step: Run analysis to compute importance values:\"\n")
        f.write(f"echo \"  python 07_importance_analysis.py --mode analyze --method {method}\"\n")

    # Make script executable
    os.chmod(script_file, 0o755)

    print(f"\nShell script generated: {script_file}")
    return script_file


# =============================================================================
# MAIN
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Parameter Importance Analysis for Noah-MP Calibration",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Prepare OAT analysis (10 combinations with LAI)
  python 07_importance_analysis.py --mode prepare --method oat

  # Prepare Shapley analysis (512 combinations with LAI)
  python 07_importance_analysis.py --mode prepare --method shapley

  # Analyze results after running models
  python 07_importance_analysis.py --mode analyze --method shapley --period full

  # Generate shell script only
  python 07_importance_analysis.py --mode script --method shapley
        """
    )

    parser.add_argument(
        '--mode',
        choices=['prepare', 'analyze', 'script', 'all'],
        default='prepare',
        help='Operation mode: prepare (generate combinations), analyze (compute importance), script (generate shell script), all (prepare + script)'
    )

    parser.add_argument(
        '--method',
        choices=['oat', 'shapley'],
        default='shapley',
        help='Analysis method: oat (One-at-a-Time) or shapley (Shapley values)'
    )

    parser.add_argument(
        '--calib-dir',
        type=str,
        default=DEFAULT_CALIB_DIR,
        help='Calibration results directory (relative to BASE_DIR or absolute)'
    )

    parser.add_argument(
        '--output-dir',
        type=str,
        default=None,
        help='Output directory (default: same as calib-dir)'
    )

    parser.add_argument(
        '--period',
        choices=['calibration', 'validation', 'full'],
        default='full',
        help='Time period for analysis (used in analyze mode)'
    )

    args = parser.parse_args()

    # Resolve paths
    if os.path.isabs(args.calib_dir):
        calib_dir = Path(args.calib_dir)
    else:
        calib_dir = BASE_DIR / args.calib_dir

    if args.output_dir is None:
        output_dir = calib_dir
    elif os.path.isabs(args.output_dir):
        output_dir = Path(args.output_dir)
    else:
        output_dir = BASE_DIR / args.output_dir

    # Validate paths
    if not calib_dir.exists():
        print(f"Error: Calibration directory not found: {calib_dir}")
        sys.exit(1)

    # Execute requested mode
    if args.mode in ['prepare', 'all']:
        prepare_combinations(calib_dir, output_dir, args.method)
        generate_shell_script(calib_dir, output_dir, args.method)

    if args.mode == 'script':
        generate_shell_script(calib_dir, output_dir, args.method)

    if args.mode == 'analyze':
        analyze_results(calib_dir, output_dir, args.method, args.period)


if __name__ == "__main__":
    main()
