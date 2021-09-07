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
yrs  = ["17"]
day_start = 114 
day_end   = 116
hrs = ["00","03","06","09","12","15","18","21"]
cc  = ["20"]   # Manually set the century

noleap_days = [0,31,59,90,120,151,181,212,243,273,304,334,365]
leap_days   = [0,31,60,91,121,152,182,213,244,274,305,335,366]
vars_name = ["Rainf_tavg","Snowf_tavg","Wind_f_inst","Tair_f_inst",
             "Qair_f_inst","Psurf_f_inst","SWdown_f_tavg","LWdown_f_tavg"]
vars_short= ["Rainf","Snowf","Wind","Tair",
             "Qair","Psurf","SWdown","LWdown"]
data_dir = "/glade/work/zhezhang/GLDAS/raw"
results_dir = "/glade/work/zhezhang/GLDAS/extracted"

for var in range(len(vars_name)):
    if not os.path.exists(results_dir+"/"+vars_short[var]):
        os.system("mkdir "+results_dir+"/"+vars_short[var])
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
		    infiles = sorted(glob.glob(data_dir+"/GLDAS_NOAH025_3H.A"+cc[0]+yrs[yy]+(mon)+(day)+"*"))
		    if (len(infiles)>0):
			intime      = cc[0]+yrs[yy]+(mon)+(day) 
			infile_list = infiles
            for hr in range(0,8):
	        infile = infile_list[hr]
		print "working on date: ",intime
		outfile= results_dir+"/"+vars_short[var]+"/GLDAS_"+vars_name[var]+"_"+intime+hrs[hr]
		if not os.path.exists(outfile):
		    print "working on file: ",infile
	            os.system("ncks -v "+vars_name[var]+" "+infile+" "+outfile)
	        else:
		    print "file exist, move to next one"

print "Successfully extract necessary variables!"
