#!/usr/bin/env python3
"""
Convert calibrated parameters from CSV to TBL generator format

Updated: Now includes fixed parameters (marked as is_calibrated == "Fixed") 
in the output, in addition to calibrated parameters.
"""

import sys
import argparse
import pandas as pd
import numpy as np


# Define minimum values for critical parameters that cannot be zero or negative
# to prevent numerical instabilities (division by zero, log of zero, etc.)
PARAMETER_CONSTRAINTS = {
    "Z0MVT": 0.001,   # Momentum roughness length - cannot be zero (causes log(0))
    "HVT": 0.01,      # Canopy top height - should be positive
    "HVB": 0.001,     # Canopy bottom height - should be positive
    "DLEAF": 0.001,   # Leaf dimension - should be positive
    "RC": 0.001,      # Stomatal resistance - should be positive
    "CWPVT": 0.001,   # Canopy wind parameter - should be positive
}


def apply_constraints(param_name, value):
    """
    Apply minimum value constraints to parameters
    
    Parameters:
    - param_name: Parameter name (e.g., "Z0MVT_EBF")
    - value: Calibrated value
    
    Returns:
    - Constrained value
    """
    # Extract base parameter name (remove suffix like _EBF, _CL, etc.)
    base_param = param_name.split("_")[0]
    
    if base_param in PARAMETER_CONSTRAINTS:
        min_val = PARAMETER_CONSTRAINTS[base_param]
        if value < min_val:
            print(f"  WARNING: {param_name} = {value:.6f} is below minimum {min_val:.6f}")
            print(f"           Constraining to {min_val:.6f} to avoid numerical issues")
            return min_val
    
    return value


def convert_params(input_csv, output_txt):
    """
    Convert calibrated_parameters.csv to space-delimited format
    
    Handles log-transformed parameters (SATDK) by applying inverse transformation.
    Also applies minimum value constraints for critical parameters.
    
    IMPORTANT: Now includes BOTH calibrated parameters (is_calibrated == "Yes")
    AND fixed parameters (is_calibrated == "Fixed") in the output.
    This ensures that user-specified fixed values (like LAI) are used in validation
    instead of falling back to default values.
    
    Parameters:
    - input_csv: Path to calibrated_parameters.csv
    - output_txt: Path to output text file
    """
    try:
        df = pd.read_csv(input_csv)
        
        # Include both calibrated AND fixed parameters
        # "Yes" = actively calibrated, "Fixed" = user-specified fixed values
        include_mask = df["is_calibrated"].isin(["Yes", "Fixed"])
        params_to_include = df[include_mask]
        
        if len(params_to_include) == 0:
            # Fallback: if no "Fixed" markers, just use "Yes"
            print("  Note: No Fixed parameters found, using only Yes parameters")
            params_to_include = df[df["is_calibrated"] == "Yes"]
        
        # Create output in space-delimited format with header
        params = params_to_include["parameter"].tolist()
        values = params_to_include["calibrated"].tolist()
        statuses = params_to_include["is_calibrated"].tolist()
        
        # Apply inverse transformation for log-transformed parameters
        # and apply constraints
        transformed_values = []
        for param, value, status in zip(params, values, statuses):
            # SATDK parameters are log10-transformed, need to convert back
            if "SATDK" in param:
                actual_value = 10 ** value
                transformed_values.append(actual_value)
                print(f"  Transformed {param}: log({value:.6f}) -> {actual_value:.6e} [{status}]")
            else:
                # Apply minimum value constraints
                constrained_value = apply_constraints(param, value)
                transformed_values.append(constrained_value)
                if constrained_value != value:
                    print(f"  Constrained {param}: {value:.6f} -> {constrained_value:.6f} [{status}]")
                else:
                    if status == "Fixed":
                        print(f"  Fixed {param}: {value:.6f} (user-specified value)")
        
        # Write header and values (with proper newlines)
        with open(output_txt, "w") as f:
            # Write parameter names on first line
            f.write(" ".join(params))
            f.write("\n")
            # Write values on second line
            value_strs = []
            for i, v in enumerate(transformed_values):
                if "SATDK" in params[i]:
                    value_strs.append(f"{v:.6e}")
                elif isinstance(v, float):
                    value_strs.append(f"{v:.6f}")
                else:
                    value_strs.append(str(v))
            f.write(" ".join(value_strs))
            f.write("\n")
        
        n_calibrated = sum(1 for s in statuses if s == "Yes")
        n_fixed = sum(1 for s in statuses if s == "Fixed")
        print(f"Converted {len(params)} parameters ({n_calibrated} calibrated, {n_fixed} fixed)")
        return 0
        
    except Exception as e:
        print(f"Error converting parameters: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 1


def main():
    parser = argparse.ArgumentParser(description="Convert calibrated parameters CSV to TBL format")
    parser.add_argument("--input", required=True, help="Input calibrated_parameters.csv file")
    parser.add_argument("--output", required=True, help="Output text file")
    
    args = parser.parse_args()
    
    return convert_params(args.input, args.output)


if __name__ == "__main__":
    sys.exit(main())
