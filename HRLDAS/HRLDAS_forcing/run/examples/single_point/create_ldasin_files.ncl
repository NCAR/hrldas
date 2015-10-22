;;;
;;;  This script created by Barlage to provide some guidance on how to create
;;;   HRLDAS forcing files for single_point simulations from 2D input data
;;;
;;;  This script is hardwired to work with NLDAS forcing; possibly significant 
;;;   modifications will be required to make it work with generic data
;;;

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"  

begin

 latitude =   40.00
longitude = -105.25

yyyy_start = 2012     ; time you want to start creating forcing
 leap_year = True
  mm_start = 6
  dd_start = 1
  hh_start = 0

hh_spacing = 1        ; number of hours between forcing files

total_timesteps = 49  ; set manually
                      ; - suggest doing each year separately

elevation_adjustment = 0.0   ; if you know that the input data elevation is different than the location of interest,
                             ;  put the elevation different here and the temperature will be adjusted as
			     ;  T_out = T_in - 0.0065 * elevation_adjustment		      

 indir = "/d1/barlage/data/NLDAS/raw/"
outdir = "/d1/barlage/data/NLDAS/single_point/LDASIN/"

nums = (/"00","01","02","03","04","05","06","07","08","09", \
         "10","11","12","13","14","15","16","17","18","19", \
	 "20","21","22","23","24","25","26","27","28","29", \
	 "30","31" /)

;;;;;;;;;;;;
; READ YOUR FORCING DATA
;
; YOU WILL NEED THE FOLLOWING:
;    PRECIPITATION RATE [mm/s]
;    DOWNWARD SOLAR [W/m^2]
;    DOWNWARD LONGWAVE [W/m^2]
;    TEMPERATURE [K]
;    SPECIFIC HUMIDITY [kg/kg]
;    U/V WIND COMPONENTS [m/s] (the model doesn't care about direction)
;    PRESSURE [Pa]
;    
;    ***********Note: you may need to adjust units*************
;;;;;;;;;;;;

do istep = 0, total_timesteps - 1

 inname = indir+"NLDAS_FORA0125_H.A"+yyyy_start+nums(mm_start)+nums(dd_start)+"."+nums(hh_start)+"00.002.grb"
 infile = addfile(inname,"r")
; print(getfilevarnames(infile))

 outname = outdir+yyyy_start+nums(mm_start)+nums(dd_start)+nums(hh_start)+".LDASIN_DOMAIN1"
 print( "Starting creation of: "+ outname )
 system("if [ -e "+outname+".nc ]; then rm -f "+outname+ ".nc;fi")
 
 outfile = addfile(outname+".nc","c")
 filedimdef(outfile,(/"Time","south_north","west_east"/),(/1,1,1/),(/True,False,False/))
 
 vartmp = new((/1,1,1/),"float")
 vartmp!0 = "Time"
 vartmp!1 = "south_north"
 vartmp!2 = "west_east"
 
 ; read in the temperature data from external forcing file
 
 indata = infile->TMP_110_HTGL
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) 
 vartmp = vartmp - 0.0065 * elevation_adjustment
 vartmp@units = "K"
 outfile->T2D = vartmp
 
 indata = infile->SPFH_110_HTGL
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) 
 vartmp@units = "kg/kg"
 outfile->Q2D = vartmp
 
 indata = infile->UGRD_110_HTGL
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) 
 vartmp@units = "m/s"
 outfile->U2D = vartmp

 indata = infile->VGRD_110_HTGL
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) 
 vartmp@units = "m/s"
 outfile->V2D = vartmp

 indata = infile->PRES_110_SFC
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) 
 vartmp@units = "Pa"
 outfile->PSFC = vartmp

 indata = infile->APCP_110_SFC_acc1h
 indata = indata / 3600.0   ; convert units
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) 
 vartmp@units = "mm/s"
 outfile->RAINRATE = vartmp
 
 indata = infile->DSWRF_110_SFC
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) 
 vartmp@units = "W/m^2"
 outfile->SWDOWN = vartmp
 
 indata = infile->DLWRF_110_SFC
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) 
 vartmp@units = "W/m^2"
 outfile->LWDOWN = vartmp
 
 delete(vartmp)
 
 system("mv "+outname+".nc "+outname)  ; get rid of the .nc
 
 ; now increment the time
 
 ddinmm = (/31,28,31,30,31,30,31,31,30,31,30,31/)
 if(mod(yyyy_start,4).eq.0) then  ; take care of leap years, not exhaustive, add more if necessary
   ddinmm(1) = 29
 end if

 hh_start = hh_start + hh_spacing
 if(hh_start .gt. 23) then
   dd_start = dd_start + 1
   hh_start = hh_start - 24
 end if
 
 if(dd_start .gt. ddinmm(mm_start-1)) then
   mm_start = mm_start + 1
   dd_start = 1
 end if
 
 if(mm_start .gt. 12) then
   yyyy_start = yyyy_start + 1
   mm_start = 1
 end if
 
end do

end

 
