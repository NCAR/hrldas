########################################################################
"""
NLDAS Initial Condition Extractor
=================================

This script extracts initial condition variables from NLDAS (North American Land Data
Assimilation System) NetCDF files and converts them to GRIB format for use NoahMP 5.0

Purpose:
- Converts NLDAS NetCDF data to GRIB format
- Handles proper GRIB parameter codes and level types
- Fixes soil layer information for subsurface variables
- Creates files compatible with the requirements of create_forcing script

Key Challenge:
NLDAS NetCDF files store soil variables as single-level data, but GRIB format
requires explicit top/bottom layer definitions for subsurface soil data. This
script manually fixes the GRIB headers to include proper soil layer boundaries.

Dependencies:
- CDO 
- wgrib 

By Mahmoud Mbarak
Date: June 16 2025
"""
#############################################################################
import os
import subprocess

# Configuration
data_dir = "" # set input directory
results_dir = "" # set saving directory
wgrib = "" # set path to wgrib

date = "20170101"   # Manually set the date
hh = "01"          # Manually set the hour
#############################################################################



# Variable mapping from original GRIB to NetCDF names
vars = ["SWE","CanopInt","AvgSurfT","SoilM_0_10cm","SoilM_10_40cm","SoilM_40_100cm","SoilM_100_200cm",
        "SoilT_0_10cm","SoilT_10_40cm","SoilT_40_100cm","SoilT_100_200cm"]



# =============================================================================
# VARIABLE MAPPING DICTIONARIES
# =============================================================================
# The following dictionaries handle the translation between NLDAS variable
# names and GRIB format requirements. Each variable needs:
# 1. Output name (for filename compatibility)
# 2. GRIB parameter code (WMO standard)
# 3. Level type (1=surface, 112=soil depth)
# 4. Level values (depth ranges for soil variables)
# Output filename mapping to match namelist expectations
output_name_map = {
    "SWE":            "WEASD",
    "CanopInt":       "CNWAT",
    "AvgSurfT":       "AVSFT",
    "SoilM_0_10cm":   "SOILM_000-010",
    "SoilM_10_40cm":  "SOILM_010-040",
    "SoilM_40_100cm": "SOILM_040-100",
    "SoilM_100_200cm": "SOILM_100-200",
    "SoilT_0_10cm":   "TSOIL_000-010",
    "SoilT_10_40cm":  "TSOIL_010-040",
    "SoilT_40_100cm": "TSOIL_040-100",
    "SoilT_100_200cm": "TSOIL_100-200"
}

# GRIB parameter codes mapping
grib_param_map = {
    "SWE":            "65",    # SNOW
    "CanopInt":       "223",   # CANWAT
    "AvgSurfT":       "148",   # TSK
    "SoilM_0_10cm":   "86",    # SMOIS
    "SoilM_10_40cm":  "86",    # SMOIS
    "SoilM_40_100cm": "86",    # SMOIS
    "SoilM_100_200cm": "86",   # SMOIS
    "SoilT_0_10cm":   "85",    # STEMP
    "SoilT_10_40cm":  "85",    # STEMP
    "SoilT_40_100cm": "85",    # STEMP
    "SoilT_100_200cm": "85"    # STEMP
}

# Level type mapping
level_type_map = {
    "SWE":            "1",     # Surface
    "CanopInt":       "1",     # Surface
    "AvgSurfT":       "1",     # Surface
    "SoilM_0_10cm":   "112",   # Depth below land surface
    "SoilM_10_40cm":  "112",   # Depth below land surface
    "SoilM_40_100cm": "112",   # Depth below land surface
    "SoilM_100_200cm": "112",  # Depth below land surface
    "SoilT_0_10cm":   "112",   # Depth below land surface
    "SoilT_10_40cm":  "112",   # Depth below land surface
    "SoilT_40_100cm": "112",   # Depth below land surface
    "SoilT_100_200cm": "112"   # Depth below land surface
}

# Level values - for soil layers, we need BOTH top and bottom
level_value_map = {
    "SWE":            {"top": "0", "bottom": None},      # Surface
    "CanopInt":       {"top": "0", "bottom": None},      # Surface
    "AvgSurfT":       {"top": "0", "bottom": None},      # Surface
    "SoilM_0_10cm":   {"top": "0", "bottom": "10"},      # 0-10 cm layer
    "SoilM_10_40cm":  {"top": "10", "bottom": "40"},     # 10-40 cm layer
    "SoilM_40_100cm": {"top": "40", "bottom": "100"},    # 40-100 cm layer
    "SoilM_100_200cm": {"top": "100", "bottom": "200"},  # 100-200 cm layer
    "SoilT_0_10cm":   {"top": "0", "bottom": "10"},      # 0-10 cm layer
    "SoilT_10_40cm":  {"top": "10", "bottom": "40"},     # 10-40 cm layer
    "SoilT_40_100cm": {"top": "40", "bottom": "100"},    # 40-100 cm layer
    "SoilT_100_200cm": {"top": "100", "bottom": "200"}   # 100-200 cm layer
}
# ============================================================================
# ============================================================================

# Create results directory if it doesn't exist
os.makedirs(results_dir, exist_ok=True)

def fix_soil_layer_grib(grib_file, top_level, bottom_level):
    """
    Fix GRIB file for soil layers to have both Level1 and Level2
    For level type 112 (depth below land surface), we need to set both levels
    """
    # Read the file
    with open(grib_file, 'rb') as f:
        data = bytearray(f.read())
    
    # Find GRIB sections
    grib_start = data.find(b'GRIB')
    if grib_start == -1:
        print(f"Error: GRIB marker not found in {grib_file}")
        return False
    
    # PDS starts after IS (8 bytes)
    pds_start = grib_start + 8
    
    # In PDS for soil layers:
    # Byte 11 (index 10): Level1 (top of layer)
    # Byte 12 (index 11): Level2 (bottom of layer)
    
    # Set Level1 and Level2
    data[pds_start + 10] = int(top_level)
    data[pds_start + 11] = int(bottom_level)
    
    print(f"  Set layer: {top_level}-{bottom_level} cm")
    
    # Write the modified file
    with open(grib_file, 'wb') as f:
        f.write(data)
    
    return True

# Process each variable
for var in vars:
    file = f"{data_dir}/NLDAS_NOAH0125_H.A{date}.{hh}00.020.nc" ## ensure you have downloaded the right files
    
    if os.path.exists(file):
        # Get parameters
        grib_param = grib_param_map[var]
        level_type = level_type_map[var]
        level_values = level_value_map[var]
        output_name = output_name_map[var]
        
        output_file = f"{results_dir}/NLDAS_{output_name}.{date}{hh}.grb"
        
        print(f"Processing: {date} {hh}:00 - Variable: {var} -> {output_name}")
        
        # For surface variables (level type 1), use simple conversion
        if level_type == "1":
            cmd = f"cdo -s -f grb -setparam,{grib_param} -setltype,{level_type} -setlevel,{level_values['top']} -selvar,{var} {file} {output_file}"
            result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
            
            if result.returncode == 0:
                print(f"  Created surface variable")
            else:
                print(f"  CDO failed: {result.stderr}")
        
        # For soil variables (level type 112), need special handling
        else:
            # First use CDO with top level
            cmd = f"cdo -s -f grb -setparam,{grib_param} -setltype,{level_type} -setlevel,{level_values['top']} -selvar,{var} {file} {output_file}"
            result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
            
            if result.returncode == 0:
                # Fix the GRIB file to include both levels
                if fix_soil_layer_grib(output_file, level_values['top'], level_values['bottom']):
                    # Verify with wgrib
                    cmd = f"{wgrib} -V {output_file} 2>&1 | grep -E 'kpds5|kpds6|kpds7|levels'"
                    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
                    print(f"  Verification: {result.stdout.strip()}")
            else:
                print(f"  CDO failed: {result.stderr}")
    else:
        print(f"File not found: {file}")

print("\nDone processing initial condition files.")
