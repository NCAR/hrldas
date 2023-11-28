#!/usr/bin/python
### Zhe Zhang
### 2020-04-21
### python script to extract gldas variables
### origional perl code from Mike's directory
### migrate from perl to python 

import glob
import numpy as np
import sys,os

### create date arrays
year  = "2023"
month = "11"
day   = "01"

vars_name = ["SKT","SD","STL1","STL2","STL3","STL4","SWVL1","SWVL2","SWVL3","SWVL4"]
vars_time = ["hr fcst:","hr fcst:","hr fcst:","hr fcst:","hr fcst:","hr acc:","hr acc:","hr acc:"]

data_dir = "../../../ERA5_forcing/"
infile  = data_dir+"ERA5-Land-Noah-MP-2023_11_01_INIT.grib"
results_dir = "./OUTPUT/INIT/"

os.system("mkdir -p "+results_dir)

for var in range(10):
    print("working on time: ",year+month+day+"00")
    dcommand = "d="+year+month+day+"00:"+vars_name[var]
    outfile= results_dir+"/ERA5_"+vars_name[var]+"_"+year+month+day+"00"
    if not os.path.exists(outfile):
        os.system("wgrib -s -4yr "+infile+" | grep '"+dcommand+"' | wgrib -i -grib "+infile+" -o "+outfile)
    else:
        print("file exist, move to next one")

print("Successfully extract necessary variables!")

