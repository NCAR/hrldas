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
nums = ["00","01","02","03","04","05","06","07","08","09",
        "10","11","12","13","14","15","16","17","18","19",
        "20","21","22","23","24","25","26","27","28","29",
        "30","31"]
yrs  = ["01"]
day_start = 274 
day_end   = 275
cc  = ["20"]   # Manually set the century

noleap_days = [0,31,59,90,120,151,181,212,243,273,304,334,365]
leap_days   = [0,31,60,91,121,152,182,213,244,274,305,335,366]
vars_name = ["10U","10V","2T","2D","SP","SSRD","STRD","TP"]
vars_out  = ["10U","10V","2T","2D","SP","ACSSRD","ACSTRD","ACTP"]
vars_time = ["hr fcst:","hr fcst:","hr fcst:","hr fcst:","hr fcst:","hr acc:","hr acc:","hr acc:"]
hours     = np.linspace(0,23,24)
houre     = np.linspace(1,24,24)
hh        = ["00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"]

data_dir = "/glade/scratch/zhezhang/ERA5/land/"
infile  = data_dir+"ERA5_global_2001-10-01.grib"
results_dir = "/glade/scratch/zhezhang/ERA5/land/extract/"

for var in range(8):
    if not os.path.exists(results_dir+"/"+vars_name[var]):
        os.system("mkdir "+results_dir+"/"+vars_name[var])
for yy in range(len(yrs)):
    modays = noleap_days
    if (yrs[yy] == "92" or "96"):
        modays = leap_days
    if (yrs[yy] == "00" or "04" or "08" or "12" or "16"):
        modays = leap_days
    for julday in range(day_start+1,day_end+2,1):
        for mo in range(0,12,1):
            if (julday>modays[mo] and julday<=modays[mo+1]):
                day = (julday - modays[mo])
                if (mo<9):
                    mon = "0"+str(mo+1)
                else:
                    mon = str(mo+1)
                if (day<10):
                    day = "0"+str(day)
                else:
                    day = str(day)
	        inday    = cc[0]+yrs[yy]+(mon)+(day) 
        for hr in range(24):
            for var in range(8):
                print "working on time: ",inday+str(int(hours[hr]))+"-"+str(int(houre[hr]))
                dforecast = "d="+inday+"00:"+vars_name[var]+":sfc:"+str(int(houre[hr]))+vars_time[var]
                danalysis = "d="+inday+"00:"+vars_name[var]+":sfc:"+str(int(hours[hr]))+"-"+str(int(houre[hr]))+vars_time[var]
                if (var <= 4):
                    dcommand = dforecast
                else:
                    dcommand = danalysis 
		outfile= results_dir+"/"+vars_name[var]+"/ERA5_"+vars_out[var]+"_"+inday+hh[hr]
		if not os.path.exists(outfile):
                    os.system("wgrib -s -4yr "+infile+" | grep '"+dcommand+"' | wgrib -i -grib "+infile+" -o "+outfile) 
	        else:
		    print "file exist, move to next one"

print "Successfully extract necessary variables!"
