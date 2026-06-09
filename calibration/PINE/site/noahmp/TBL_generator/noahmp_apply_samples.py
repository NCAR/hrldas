import os
import re
import argparse
import pandas as pd
import numpy as np
from pathlib import Path
from typing import List, Tuple

# ----------------------------
# CONSTANTS
# ----------------------------
# LAI month names for evergreen vegetation (same value for all months)
LAI_MONTHS = ['LAI_JAN', 'LAI_FEB', 'LAI_MAR', 'LAI_APR', 'LAI_MAY', 'LAI_JUN',
              'LAI_JUL', 'LAI_AUG', 'LAI_SEP', 'LAI_OCT', 'LAI_NOV', 'LAI_DEC']

# ----------------------------
# HELPERS
# ----------------------------
def build_var_info(columns: List[str], var_info_csv: str = "var_info_matrix.csv") -> pd.DataFrame:
    """
    Build the (section, variable, type) mapping for each samples column name.
    Now reads from var_info_matrix.csv instead of using hardcoded mappings.
    """
    # Read the var_info_matrix.csv file
    if os.path.exists(var_info_csv):
        var_info_df = pd.read_csv(var_info_csv)
        # Create a lookup dictionary
        var_info_dict = {}
        for _, row in var_info_df.iterrows():
            var_info_dict[row['column_name']] = {
                'section': row['section'],
                'variable': row['variable'],
                'type': int(row['type'])
            }

        # Build rows for each column in samples
        rows = []
        for col in columns:
            if col in var_info_dict:
                info = var_info_dict[col]
                rows.append((col, info['section'], info['variable'], info['type']))
            else:
                # Column not found in var_info_matrix.csv
                rows.append((col, "UNKNOWN", col, None))

        return pd.DataFrame(rows, columns=["column_name", "section", "variable", "type"])
    else:
        raise FileNotFoundError(f"var_info_matrix.csv not found at {var_info_csv}")


def find_section_span(text: str, section_name: str) -> Tuple[int, int]:
    """Return (start_idx, end_idx) of a section delimited by '&...\n' to next section start or EOF."""
    start = text.find(section_name)
    if start == -1:
        raise ValueError(f"Section '{section_name}' not found in table.")
    # Find the next section start ('\n&') after 'start'
    next_match = re.search(r"\n&", text[start+1:])
    if next_match:
        end = start + 1 + next_match.start()
    else:
        end = len(text)
    return start, end


def replace_var_value_in_section(section_text: str, var_name: str, type_idx_1based: int, new_value: float) -> str:
    """
    Replace the comma-separated {type_idx}-th value on the var assignment line within section_text.
    Assumes assignments are single-line like: 'VAR = v1, v2, v3, ...'
    Preserves anything after an inline comment marker '!' by keeping it as-is.
    """
    # Regex for the assignment line
    pattern = re.compile(rf"(^[ \t]*{re.escape(var_name)}[ \t]*=[^\n]*$)", flags=re.MULTILINE)
    m = pattern.search(section_text)
    if not m:
        raise ValueError(f"Variable '{var_name}' not found as single-line assignment within section.")

    full_line = m.group(1)
    # Split into lhs, rhs
    lhs, rhs = full_line.split("=", 1)
    # Separate inline comment if any
    comment_part = ""
    if "!" in rhs:
        rhs, comment_part = rhs.split("!", 1)
        comment_part = "!" + comment_part  # keep marker

    # Tokenize comma-separated values
    parts = [p.strip() for p in rhs.strip().split(",")]
    idx = type_idx_1based - 1
    if not (0 <= idx < len(parts)):
        raise IndexError(f"Type index {type_idx_1based} is out of range for variable '{var_name}' with {len(parts)} values.")

    # Replace target value (format minimally)
    # Use scientific notation for very small values (like SATDK)
    if abs(new_value) < 1e-3 and new_value != 0:
        parts[idx] = f"{float(new_value):.6e}"
    else:
        parts[idx] = f"{float(new_value):.8g}"

    new_rhs = ", ".join(parts)
    new_line = f"{lhs.strip()} = {new_rhs}{comment_part}".rstrip()

    # Put back original indentation from lhs
    indent = re.match(r"^[ \t]*", full_line).group(0)
    new_line = indent + new_line + "\n"

    # Replace in section_text
    start, end = m.span(1)
    section_text = section_text[:start] + new_line + section_text[end:]
    return section_text


def apply_row_to_table(base_text: str, var_info: pd.DataFrame, row: pd.Series, verbose: bool = False) -> str:
    """Apply one samples row to the full table text and return updated text.
    
    Note: Parameter values are used directly without any transformation.
    SATDK should be stored in original (non-log) form in the samples file.
    
    Special handling for LAI_MONTHLY: applies the same LAI value to all 12 months.
    """
    text = base_text  # work on a copy

    # Get unique sections from var_info (dynamically determined)
    sections = var_info["section"].unique()
    sections = [s for s in sections if s != "UNKNOWN"]

    # We'll update per section to avoid shifting indices too much.
    for section_name in sections:
        # Slice the section span fresh each time (indices shift after replacements)
        try:
            start, end = find_section_span(text, section_name)
        except ValueError as e:
            print(f"Warning: {e}")
            continue

        section_text = text[start:end]

        # Extract the variables that live in this section, in the order they appear in var_info
        idxs = var_info.index[var_info["section"] == section_name].tolist()
        for j in idxs:
            var = var_info.loc[j, "variable"]
            typ = int(var_info.loc[j, "type"])
            col_name = var_info.loc[j, "column_name"]
            if pd.isna(row[col_name]):
                # Skip NaNs (no replacement)
                continue
            
            # Use value directly (no transformation)
            value = row[col_name]
            
            # Special handling for LAI_MONTHLY: apply same value to all 12 months
            if var == 'LAI_MONTHLY':
                if verbose:
                    print(f"  LAI (all months): {value:.4f}")
                for month_var in LAI_MONTHS:
                    try:
                        section_text = replace_var_value_in_section(section_text, month_var, typ, value)
                    except ValueError as e:
                        if verbose:
                            print(f"    Warning: {e}")
            else:
                if verbose and var == 'SATDK':
                    print(f"  {var}: {value:.6e}")
                section_text = replace_var_value_in_section(section_text, var, typ, value)

        # Put the updated section back
        text = text[:start] + section_text + text[end:]
    return text


def main():
    parser = argparse.ArgumentParser(description="Apply NoahMP samples to NoahmpTable.TBL to generate emulated tables.")
    parser.add_argument("--samples", required=True, help="Path to noahmp_1000samples.txt (space-delimited).")
    parser.add_argument("--base_table", required=True, help="Path to the base NoahmpTable.TBL to modify.")
    parser.add_argument("--out_root", default=".", help="Output root directory.")
    parser.add_argument("--n_rows", type=int, default=10, help="How many rows to process (default: 10).")
    parser.add_argument("--verbose", action="store_true", help="Print transformation details.")
    args = parser.parse_args()

    # Load samples (skip comment lines starting with #)
    df = pd.read_csv(args.samples, delim_whitespace=True, comment='#')
    var_info = build_var_info(df.columns.tolist())

    # Sanity check: any unknowns?
    unknowns = var_info[var_info["section"] == "UNKNOWN"]
    if not unknowns.empty:
        raise RuntimeError(f"Found columns with unknown section/type:\n{unknowns}")

    # Load base table once
    with open(args.base_table, "r", encoding="utf-8") as f:
        base_text = f.read()

    print(f"Processing {min(args.n_rows, len(df))} samples...")
    print(f"Note: SATDK values are used directly (should be in original form, not log)")
    print(f"Note: LAI values will be applied to all 12 months (evergreen vegetation)")
    
    n = min(args.n_rows, len(df))
    for i in range(n):
        if args.verbose or i == 0:
            print(f"\nSample {i+1}:")
        updated_text = apply_row_to_table(base_text, var_info, df.iloc[i], verbose=(args.verbose or i == 0))

        out_dir = Path(args.out_root) / f"NoahmpTable_4emu_{i+1}"
        out_dir.mkdir(parents=True, exist_ok=True)
        out_path = out_dir / "NoahmpTable.TBL"
        with open(out_path, "w", encoding="utf-8") as f:
            f.write(updated_text)
        
        if (i + 1) % 100 == 0:
            print(f"  Processed {i+1}/{n} samples...")
    
    print(f"\nWrote {n} TBL files to {args.out_root}")

if __name__ == "__main__":
    main()
