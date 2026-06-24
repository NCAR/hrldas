"""
Physical constraints for CLM parameter calibration.

This module provides constraint functions to ensure physically valid parameter
combinations during optimization, particularly for canopy height parameters.
"""

import re
import warnings
from typing import Dict, List, Optional, Tuple
import numpy as np

# Vegetation type suffixes used in parameter naming
VEG_SUFFIXES = [
    '_NET', '_NDT', '_BET', '_BDT', '_SET', '_SDT',  # Trees
    '_GA', '_C3G', '_C4G',  # Grasses
    '_C3C', '_C3I',  # Crops
    '_EBF', '_DBF', '_ENF', '_DNF',  # Additional forest types
    '_SHR', '_CL'  # Shrubs and other
]


def find_hvt_hvb_pairs(param_names: List[str]) -> List[Tuple[int, int, str]]:
    """
    Find matching HVT/HVB parameter pairs by vegetation type suffix.

    HVT (canopy top height) and HVB (canopy bottom height) parameters are named
    with vegetation type suffixes (e.g., HVT_EBF, HVB_EBF).

    Args:
        param_names: List of parameter names

    Returns:
        List of tuples (hvt_index, hvb_index, suffix) for matched pairs
    """
    pairs = []

    # Build dictionaries mapping suffix to index
    hvt_params = {}
    hvb_params = {}

    for idx, name in enumerate(param_names):
        upper_name = name.upper()

        # Check for HVT parameters
        if upper_name.startswith('HVT'):
            # Extract suffix (everything after HVT)
            suffix = upper_name[3:]  # e.g., '_EBF' from 'HVT_EBF'
            if not suffix:
                suffix = ''  # Handle bare 'HVT' parameter
            hvt_params[suffix] = idx

        # Check for HVB parameters
        elif upper_name.startswith('HVB'):
            suffix = upper_name[3:]
            if not suffix:
                suffix = ''
            hvb_params[suffix] = idx

    # Match pairs by suffix
    for suffix, hvt_idx in hvt_params.items():
        if suffix in hvb_params:
            hvb_idx = hvb_params[suffix]
            pairs.append((hvt_idx, hvb_idx, suffix))
        else:
            warnings.warn(f"HVT parameter with suffix '{suffix}' has no matching HVB parameter")

    # Check for unmatched HVB parameters
    for suffix in hvb_params:
        if suffix not in hvt_params:
            warnings.warn(f"HVB parameter with suffix '{suffix}' has no matching HVT parameter")

    return pairs


def check_hvt_hvb_constraint(
    params: np.ndarray,
    param_names: List[str],
    margin: float = 0.5
) -> Tuple[bool, List[str]]:
    """
    Check if HVT > HVB + margin constraint is satisfied for all parameter pairs.

    Args:
        params: Parameter values (1D array)
        param_names: List of parameter names
        margin: Minimum required difference between HVT and HVB (meters)

    Returns:
        Tuple of (all_satisfied, list of violation messages)
    """
    pairs = find_hvt_hvb_pairs(param_names)
    violations = []

    for hvt_idx, hvb_idx, suffix in pairs:
        hvt_val = params[hvt_idx]
        hvb_val = params[hvb_idx]

        if hvt_val <= hvb_val + margin:
            violation_msg = (
                f"HVT{suffix}={hvt_val:.2f} <= HVB{suffix}={hvb_val:.2f} + margin={margin}"
            )
            violations.append(violation_msg)

    return len(violations) == 0, violations


def project_hvt_hvb_constraint(
    params,  # torch.Tensor (modified in place)
    param_names: List[str],
    lower_bounds,  # torch.Tensor
    upper_bounds,  # torch.Tensor
    margin: float = 0.5,
    verbose: bool = False
) -> int:
    """
    Project parameters to satisfy HVT > HVB + margin constraint.

    This function modifies the parameter tensor in-place to ensure the constraint
    is satisfied. The projection strategy:
    1. Detect violation: HVT <= HVB + margin
    2. Calculate violation amount
    3. Split correction: move HVT up and HVB down equally
    4. Respect individual parameter bounds
    5. If bounds conflict, prioritize moving HVT up

    Args:
        params: Parameter tensor (modified in place)
        param_names: List of parameter names
        lower_bounds: Lower bounds tensor
        upper_bounds: Upper bounds tensor
        margin: Minimum required difference between HVT and HVB (meters)
        verbose: If True, print warnings when corrections are made

    Returns:
        Number of corrections made
    """
    import torch

    pairs = find_hvt_hvb_pairs(param_names)
    corrections = 0

    for hvt_idx, hvb_idx, suffix in pairs:
        hvt_val = params[hvt_idx].item()
        hvb_val = params[hvb_idx].item()

        # Check constraint violation
        if hvt_val <= hvb_val + margin:
            corrections += 1

            # Calculate required correction
            violation = (hvb_val + margin) - hvt_val
            half_correction = violation / 2.0

            # Get bounds
            hvt_lower = lower_bounds[hvt_idx].item()
            hvt_upper = upper_bounds[hvt_idx].item()
            hvb_lower = lower_bounds[hvb_idx].item()
            hvb_upper = upper_bounds[hvb_idx].item()

            # Try to split correction equally
            new_hvt = hvt_val + half_correction + margin / 2
            new_hvb = hvb_val - half_correction - margin / 2

            # Apply bounds to HVT
            if new_hvt > hvt_upper:
                # Can't move HVT up enough, need to move HVB down more
                new_hvt = hvt_upper
                new_hvb = new_hvt - margin
            elif new_hvt < hvt_lower:
                new_hvt = hvt_lower

            # Apply bounds to HVB
            if new_hvb < hvb_lower:
                # Can't move HVB down enough, need to move HVT up more
                new_hvb = hvb_lower
                new_hvt = new_hvb + margin
                # Re-check HVT upper bound
                if new_hvt > hvt_upper:
                    new_hvt = hvt_upper
                    # Constraint cannot be fully satisfied
                    if verbose:
                        warnings.warn(
                            f"Cannot fully satisfy HVT{suffix} > HVB{suffix} + {margin} "
                            f"due to bounds conflict. Setting to bounds: "
                            f"HVT={new_hvt:.2f}, HVB={new_hvb:.2f}"
                        )
            elif new_hvb > hvb_upper:
                new_hvb = hvb_upper

            # Update parameters
            params[hvt_idx] = new_hvt
            params[hvb_idx] = new_hvb

            if verbose:
                print(
                    f"  Corrected HVT{suffix}: {hvt_val:.2f} -> {new_hvt:.2f}, "
                    f"HVB{suffix}: {hvb_val:.2f} -> {new_hvb:.2f}"
                )

    return corrections


def apply_hvt_hvb_constraint_numpy(
    params: np.ndarray,
    param_names: List[str],
    lower_bounds: np.ndarray,
    upper_bounds: np.ndarray,
    margin: float = 0.5,
    verbose: bool = False
) -> Tuple[np.ndarray, int]:
    """
    Apply HVT > HVB + margin constraint using NumPy arrays.

    This is a NumPy version of project_hvt_hvb_constraint for use in
    post-processing and validation.

    Args:
        params: Parameter values (1D array, will be copied)
        param_names: List of parameter names
        lower_bounds: Lower bounds array
        upper_bounds: Upper bounds array
        margin: Minimum required difference between HVT and HVB (meters)
        verbose: If True, print warnings when corrections are made

    Returns:
        Tuple of (corrected params array, number of corrections)
    """
    params = params.copy()
    pairs = find_hvt_hvb_pairs(param_names)
    corrections = 0

    for hvt_idx, hvb_idx, suffix in pairs:
        hvt_val = params[hvt_idx]
        hvb_val = params[hvb_idx]

        # Check constraint violation
        if hvt_val <= hvb_val + margin:
            corrections += 1

            # Calculate required correction
            violation = (hvb_val + margin) - hvt_val
            half_correction = violation / 2.0

            # Get bounds
            hvt_lower = lower_bounds[hvt_idx]
            hvt_upper = upper_bounds[hvt_idx]
            hvb_lower = lower_bounds[hvb_idx]
            hvb_upper = upper_bounds[hvb_idx]

            # Try to split correction equally
            new_hvt = hvt_val + half_correction + margin / 2
            new_hvb = hvb_val - half_correction - margin / 2

            # Apply bounds to HVT
            if new_hvt > hvt_upper:
                new_hvt = hvt_upper
                new_hvb = new_hvt - margin
            elif new_hvt < hvt_lower:
                new_hvt = hvt_lower

            # Apply bounds to HVB
            if new_hvb < hvb_lower:
                new_hvb = hvb_lower
                new_hvt = new_hvb + margin
                if new_hvt > hvt_upper:
                    new_hvt = hvt_upper
                    if verbose:
                        warnings.warn(
                            f"Cannot fully satisfy HVT{suffix} > HVB{suffix} + {margin} "
                            f"due to bounds conflict. Setting to bounds: "
                            f"HVT={new_hvt:.2f}, HVB={new_hvb:.2f}"
                        )
            elif new_hvb > hvb_upper:
                new_hvb = hvb_upper

            # Update parameters
            params[hvt_idx] = new_hvt
            params[hvb_idx] = new_hvb

            if verbose:
                print(
                    f"  Corrected HVT{suffix}: {hvt_val:.2f} -> {new_hvt:.2f}, "
                    f"HVB{suffix}: {hvb_val:.2f} -> {new_hvb:.2f}"
                )

    return params, corrections


def validate_final_params(
    params: np.ndarray,
    param_names: List[str],
    margin: float = 0.5
) -> bool:
    """
    Validate that final calibrated parameters satisfy all constraints.

    Args:
        params: Parameter values (1D array)
        param_names: List of parameter names
        margin: Minimum required difference between HVT and HVB (meters)

    Returns:
        True if all constraints are satisfied, False otherwise
    """
    satisfied, violations = check_hvt_hvb_constraint(params, param_names, margin)

    if not satisfied:
        warnings.warn(
            f"Final parameters violate HVT > HVB + {margin} constraint:\n" +
            "\n".join(f"  - {v}" for v in violations)
        )

    return satisfied
