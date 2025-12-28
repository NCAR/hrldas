# =====================================================================
"""
APCP Processor
==========================================

This script requires separate handling because:
1. Precipitation represents ACCUMULATED values over time periods
2. GRIB format requires specific time range indicators for accumulation
3. Standard CDO conversion doesn't set proper accumulation metadata

The fix_apcp_grib() function manually adjusts GRIB headers to indicate
1-hour accumulation periods, which is needed by the create_forcing script.

By Mahmoud Mbarak (mbarak@utexas.edu)
Date: June 16 2025
Revised by Tzu-Shun Lin for leap years, Date: August 22 2025

"""
# =======================================================================
import os
import subprocess

# Configuration
nums = ["00","01","02","03","04","05","06","07","08","09","10",
        "11","12","13","14","15","16","17","18","19","20","21",
        "22","23","24","25","26","27","28","29","30","31"]
yrs = ["00"]
day_start = 1
day_end = 10
cc = "20"
noleap_days = [0,31,59,90,120,151,181,212,243,273,304,334,365]
leap_days = [0,31,60,91,121,152,182,213,244,274,305,335,366]

# APCP settings
var = "APCP"
netcdf_var = "Rainf"
grib_param = "61"
level_type = "1"
level_value = "0"

data_dir = "/glade/campaign/ral/hap/tslin2/data/NLDAS2/FORA0125" # set input directory
results_dir = "./extracted" # set input directory
wgrib = "/glade/u/home/tslin2/wgrib/wgrib" # set path to wgrib
# ======================================================================== 

def fix_apcp_grib(grib_file):
    """
    Fix GRIB file for APCP:
    - Change timeRangeIndicator from 10 to 4 (at byte 21)
    - Change P2 from 0 to 1 (at byte 20)
    """
    # Read the file
    with open(grib_file, 'rb') as f:
        data = bytearray(f.read())
    
    # Find GRIB sections
    # Look for 'GRIB' marker
    grib_start = data.find(b'GRIB')
    if grib_start == -1:
        print(f"Error: GRIB marker not found in {grib_file}")
        return False
    
    # PDS (Product Definition Section) starts after IS (Indicator Section)
    # IS is 8 bytes, so PDS starts at grib_start + 8
    pds_start = grib_start + 8
    
    # In PDS:
    # Byte 19 (index 18): P1 - should stay 0
    # Byte 20 (index 19): P2 - change from 0 to 1
    # Byte 21 (index 20): timeRangeIndicator - change from 10 to 4
    
    # Change timeRangeIndicator from 10 to 4
    if data[pds_start + 20] == 10:
        data[pds_start + 20] = 4
        print(f"  Changed timeRangeIndicator: 10 -> 4")
    
    # Change P2 from 0 to 1
    if data[pds_start + 19] == 0:
        data[pds_start + 19] = 1
        print(f"  Changed P2: 0 -> 1")
    
    # Write the modified file
    with open(grib_file, 'wb') as f:
        f.write(data)
    
    return True

# Process APCP files
for yy in yrs:
    # Set month days array based on leap year

    year='{0}{1}'.format(cc,yy)
    year=int(year)
    if(year%4 == 0 and year%100 !=0) or (year%400 == 0):
        modays = leap_days.copy()
    else:
        modays = noleap_days.copy()
    
    # Adjust day_end for leap years
    local_day_end = day_end
    if day_end == 365 and (year%4 == 0 and year%100 !=0):
        local_day_end = 366
    if day_end == 365 and (year%400 == 0):
        local_day_end = 366

    for julday in range(day_start, local_day_end + 1):
        # Find month and day
        for mo in range(1, 13):
            if julday > modays[mo-1] and julday <= modays[mo]:
                mon = mo
                day = julday - modays[mo-1]
                break
        
        for hr in range(24):
            # NetCDF file path
            file = f"{data_dir}/NLDAS_FORA0125_H.A{cc}{yy}{nums[mon]}{nums[day]}.{nums[hr]}00.020.nc"
            
            if os.path.exists(file):
                # Output file path
                output_file = f"{results_dir}/{var}/NLDAS_{var}.{cc}{yy}{nums[mon]}{nums[day]}{nums[hr]}.grb"
                
                print(f"Processing: {cc}{yy}-{nums[mon]}-{nums[day]} {nums[hr]}:00")
                
                # Step 1: Convert with CDO
                cmd = f"cdo -s -f grb -setparam,{grib_param} -setltype,{level_type} -setlevel,{level_value} -selvar,{netcdf_var} {file} {output_file}"
                result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
                
                if result.returncode == 0:
                    # Step 2: Fix the GRIB file
                    if fix_apcp_grib(output_file):
                        # Step 3: Verify with wgrib
                        cmd = f"{wgrib} -V {output_file} 2>&1 | grep -E 'timerange|P1|P2'"
                        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
                        print(f"  Verification: {result.stdout.strip()}")
                else:
                    print(f"  CDO failed: {result.stderr}")
            else:
                print(f"File not found: {file}")

print("\nDone processing APCP files.")
