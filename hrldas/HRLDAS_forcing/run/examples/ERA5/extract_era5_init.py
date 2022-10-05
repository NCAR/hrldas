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
yr   = "01"
date = "1001"
cc   = "20"   # Manually set the century

vars_name = ["SKT","SD","STL1","STL2","STL3","STL4","SWVL1","SWVL2","SWVL3","SWVL4"]
vars_time = ["hr fcst:","hr fcst:","hr fcst:","hr fcst:","hr fcst:","hr acc:","hr acc:","hr acc:"]

data_dir = "/glade/scratch/zhezhang/ERA5/land/"
infile  = data_dir+"ERA5_global_2001-10-01_init.grib"
results_dir = "/glade/scratch/zhezhang/ERA5/land/extract/INIT/"

for var in range(10):
    print "working on time: ",cc+yr+date+"00"
    dcommand = "d="+cc+yr+date+"00:"+vars_name[var]
    outfile= results_dir+"/ERA5_"+vars_name[var]+"_"+cc+yr+date+"00"
    if not os.path.exists(outfile):
        os.system("wgrib -s -4yr "+infile+" | grep '"+dcommand+"' | wgrib -i -grib "+infile+" -o "+outfile)
    else:
        print "file exist, move to next one"

print "Successfully extract necessary variables!"
