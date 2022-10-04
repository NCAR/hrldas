#!/usr/bin/python
### Zhe Zhang
### 2020-05-31
### Calculate specific humidity
### from dew point temperature and surface pressure
### according to Bolton 1980
### e = 6.112*exp((17.67*Td)/(Td + 243.5));
### q = (0.622 * e)/(p - (0.378 * e));
###     where:
###       e = vapor pressure in mb;
###       Td = dew point in deg C;
###       p = surface pressure in mb;
###       q = specific humidity in kg/kg.
###       (Note the final specific humidity units are in g/kg = (kg/kg)*1000.0)
### https://archive.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html

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
vars_name = ["2D","SP"]
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

# Four steps are taken for calculating specific humidity
# (1) calculate saturated water pressure e from Td 
# (2) combine e and SP
# (3) calculate q from e and SP
# (4) set the output parameter id  
        for hr in range(24):
            print "working on time: ",inday+str(int(hours[hr]))+"-"+str(int(houre[hr]))
            infile1=  results_dir+"2D/ERA5_2D_"+inday+hh[hr]
            outfile1= results_dir+"2D/ERA5_E_"+inday+hh[hr]
            command1= 'cdo expr,"var1=6.112*exp(17.67*(var168-273.15)/(var168-273.15+243.5))" '+infile1+" "+outfile1
            os.system(command1)            

            infile2 = results_dir+"/SP/ERA5_SP_"+inday+hh[hr]
            outfile2= results_dir+"2D/ERA5_combine_"+inday+hh[hr]
            command2= 'cdo merge '+outfile1+" "+infile2+" "+outfile2
	    os.system(command2)

	    outfile3= results_dir+"/Q/ERA5_Q1_"+inday+hh[hr]
            command3= 'cdo expr,"var133=(0.622*var1)/(var134/100.-(0.378*var1))/1000." '+" "+outfile2+" "+outfile3
	    os.system(command3)
            
	    outfile4= results_dir+"/Q/ERA5_Q_"+inday+hh[hr]
            command4= "cdo setparam,133.128 "+outfile3+" "+outfile4
	    os.system(command4)

print "Successfully calculate specific humidity!"
