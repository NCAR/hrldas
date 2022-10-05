#!/usr/bin/python
### Zhe Zhang
### 2020-05-31
### There are three accumulated fields in ERA5
### shortwave radiation (SSRD), longwave radiation (STRD)
### and total precipitation (TP)
### start accumulate by 00 hour, leave the first time step
### start subtracting the previous time step from the rest 01-23 time steps

import glob
import numpy as np
import sys,os

### module load cdo
os.system("module load cdo ")

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
vars_name = ["SSRD","STRD","TP"]
vars_out  = ["ACSSRD","ACSTRD","ACTP"]
hours     = np.linspace(0,23,24)
houre     = np.linspace(1,24,24)
hh        = ["00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"]

data_dir = "/glade/scratch/zhezhang/ERA5/land/"
results_dir = "/glade/scratch/zhezhang/ERA5/land/extract/"

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

        for var in range(3):
            for hr in range(1,24):
                print "working on time: ",inday+str(int(hours[hr]))+"-"+str(int(houre[hr]))
                infile1= results_dir+"/"+vars_name[var]+"/ERA5_"+vars_out[var]+"_"+inday+hh[hr]
                infile2= results_dir+"/"+vars_name[var]+"/ERA5_"+vars_out[var]+"_"+inday+hh[hr-1]
		outfile= results_dir+"/"+vars_name[var]+"/ERA5_"+vars_name[var]+"_"+inday+hh[hr]
                commandline = "cdo sub "+infile1+" "+infile2+" "+outfile # subtract the time step before
                print (commandline)
		if not os.path.exists(outfile):
                    os.system(commandline) 
	        else:
		    print "file exist, move to next one"
            hr = 0
            print "working on time: ",inday+str(int(hours[hr]))+"-"+str(int(houre[hr]))
            infile  = results_dir+"/"+vars_name[var]+"/ERA5_"+vars_out[var]+"_"+inday+hh[hr]
            outfile = results_dir+"/"+vars_name[var]+"/ERA5_"+vars_name[var]+"_"+inday+hh[hr]
            commandline = "mv "+infile+" "+outfile # leave the 00 time step out
            print (commandline)
            if not os.path.exists(outfile):
                os.system(commandline)
            else:
                print "file exist, move to next one"

print "Successfully deaccumulate SSRD, STRD, TP!"
