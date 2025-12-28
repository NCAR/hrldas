#!/usr/bin/perl
##==============================================================================
# NLDAS Meteorological Variables Extractor
#==============================================================================
#
# Purpose:
#   Extracts meteorological forcing variables from NLDAS NetCDF files and
#   converts them to GRIB format.
#
# Variables Processed:
#   - DLWRF  : Downward longwave radiation flux (W/m²)
#   - DSWRF  : Downward shortwave radiation flux (W/m²)
#   - PRES   : Surface pressure (Pa)
#   - TMP    : Air temperature at 2m (K)
#   - SPFH   : Specific humidity at 2m (kg/kg)
#   - UGRD   : U-component wind at 10m (m/s)
#   - VGRD   : V-component wind at 10m (m/s)
#
# Input Data:
#   NLDAS forcing files: NLDAS_FORA0125_H.A[YYYYMMDD].[HH]00.020.nc
#
# Output Format:
#   Extracted NLDAS GRIB files 
#
# Dependencies:
#   - CDO (Climate Data Operators) must be installed and in PATH
#
# By Mahmoud Mbarak
# Date: June 16 2025
# Revised by Tzu-Shun Lin for leap years, Date: August 22 2025
#==============================================================================


#==============================================================================
# CONFIGURATION SECTION
#==============================================================================
@nums = ("00","01","02","03","04","05","06","07","08","09","10",
         "11","12","13","14","15","16","17","18","19","20","21",
         "22","23","24","25","26","27","28","29","30","31");
@yrs = ("02");
$day_start = 1;
$day_end   = 90;
$cc = "20";
@noleap_days = (0,31,59,90,120,151,181,212,243,273,304,334,365);
@leap_days   = (0,31,60,91,121,152,182,213,244,274,305,335,366);
@vars = ("DLWRF","DSWRF","PRES","TMP","SPFH","UGRD","VGRD");


$data_dir = "/glade/campaign/ral/hap/tslin2/data/NLDAS2/FORA0125"; # set input directory
$results_dir = "./extracted"; # set input directory

# Variable mapping from GRIB to NetCDF names
%netcdf_var_map = (
    "DLWRF" => "LWdown",
    "DSWRF" => "SWdown",
    "PRES"  => "PSurf",
    "TMP"   => "Tair",
    "SPFH"  => "Qair",
    "UGRD"  => "Wind_E",
    "VGRD"  => "Wind_N"
);

# GRIB parameter codes mapping (based on Vtable)
%grib_param_map = (
    "TMP"   => "11",     # Temperature (T2D in Vtable)
    "SPFH"  => "51",     # Specific Humidity (Q2D in Vtable) 
    "UGRD"  => "33",     # U-wind (U2D in Vtable)
    "VGRD"  => "34",     # V-wind (V2D in Vtable)
    "PRES"  => "1",      # Surface Pressure (PSFC in Vtable)
    "DSWRF" => "204",    # Shortwave radiation (SWDOWN in Vtable)
    "DLWRF" => "205"     # Longwave radiation (LWDOWN in Vtable)
);

# Level type mapping (based on Vtable)
%level_type_map = (
    "TMP"   => "105",    # Height above ground
    "SPFH"  => "105",    # Height above ground  
    "UGRD"  => "105",    # Height above ground
    "VGRD"  => "105",    # Height above ground
    "PRES"  => "1",      # Surface
    "DSWRF" => "1",      # Surface
    "DLWRF" => "1"       # Surface
);

# Level value mapping (based on Vtable)
%level_value_map = (
    "TMP"   => "2",      # 2 meters
    "SPFH"  => "2",      # 2 meters
    "UGRD"  => "10",     # 10 meters
    "VGRD"  => "10",     # 10 meters
    "PRES"  => "0",      # Surface level
    "DSWRF" => "0",      # Surface level
    "DLWRF" => "0"       # Surface level
);

for $var (@vars) {
    for $yy (@yrs) {
        # Set month days array based on leap year
        @modays = @noleap_days;
	#if($yy == "20") {@modays = @leap_days}
	
	my $year="$cc$yy";
	if($year%4 == 0 and $year%100 !=0) {@modays = @leap_days}
	if($year%400 == 0) {@modays = @leap_days}

        # Only modify day_end if it's set to 365 AND it's a leap year
	# local variable to avoid affecting subsequent years
	my $local_day_end = $day_end;
	if($day_end == 365 && $year%4 == 0 and $year%100 !=0) {
	    $local_day_end = 366;
	}
        if($day_end == 365 && $year%400 == 0) {
            $local_day_end = 366;
        }

        for($julday=$day_start;$julday<=$local_day_end;$julday++) {
            # Find month and day
            for($mo=1;$mo<=12;$mo++) {
                if($julday>$modays[$mo-1] && $julday<=$modays[$mo]) {
                    $mon = $mo;
                    $day = $julday - $modays[$mo-1];
                    last;  # Exit loop once found
                }
            }
            
            for($hr=0;$hr<=23;$hr++) {
                # NetCDF file path
                $file = "$data_dir/NLDAS_FORA0125_H.A$cc$yy$nums[$mon]$nums[$day].$nums[$hr]00.020.nc";
                
                # Check if file exists
                if(-e $file) {
                    # Get NetCDF variable name and GRIB codes
                    $netcdf_var = $netcdf_var_map{$var};
                    $grib_param = $grib_param_map{$var};
                    $level_type = $level_type_map{$var};
                    $level_value = $level_value_map{$var};
                    
                    # Print progress message
                    print "Processing: $cc$yy-$nums[$mon]-$nums[$day] $nums[$hr]:00 - Variable: $var (NetCDF: $netcdf_var, GRIB: $grib_param|$level_type|$level_value)\n";
                    
                    # Use CDO to convert NetCDF to GRIB with proper parameter code, level type, and level value
                    system("cdo -s -f grb -setparam,$grib_param -setltype,$level_type -setlevel,$level_value -selvar,$netcdf_var $file $results_dir/$var/NLDAS_$var.$cc$yy$nums[$mon]$nums[$day]$nums[$hr].grb");
                    
                } else {
                    print "File not found: $file\n";
                }
            }
        }
    }
}
