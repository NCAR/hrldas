;;; manually create a SETUP file from observations
;;;    file will contain both initial conditions and location information
;;;
;;;  This script is hardwired to work with NLDAS forcing; possibly significant 
;;;   modifications will be required to make it work with generic data
;;;

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"  

begin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modify the initial time in the following three lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    timestring = "2012-06-01_00:00:00"

        inname = "/d1/barlage/data/NLDAS/raw/NLDAS_NOAH0125_H.A20120601.0000.002.grb"

setup_filename = "/d1/barlage/data/NLDAS/single_point/LDASIN/HRLDAS_setup_2012060100_d1"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up the location-specific information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               latitude =   40.00
              longitude = -105.25
              elevation = 1655.0
annual_mean_temperature =   10.9   ; will be used as deep soil temperature
    vegetation_fraction =  100.0   ; % vegetation cover
        leaf_area_index =    2.0   ; will be used to initialize dynamic vegetation if active
	vegetation_type =     10   ; vegetation class; confirm consistency with global attributes
	      soil_type =      9   ; soil texture class

system("if [ -e "+setup_filename+".nc ]; then rm -f "+setup_filename+ ".nc;fi")

outfile = addfile(setup_filename+".nc","c")

 filedimdef(outfile,(/"Time","south_north",    "west_east","soil_layers_stag"/), \
                    (/     1,            1,              1,                 4/), \
		    (/  True,        False,          False,             False/))

; Define some temporary variables

 vartmp = new((/1,1,1/),"float")
 vartmp!0 = "Time"
 vartmp!1 = "south_north"
 vartmp!2 = "west_east"
 
 var3tmp = new((/1,4,1,1/),"float")
 var3tmp!0 = "Time"
 var3tmp!1 = "soil_layers_stag"
 var3tmp!2 = "south_north"
 var3tmp!3 = "west_east"
 
 ivartmp = new((/1,1,1/),"integer")
 ivartmp!0 = "Time"
 ivartmp!1 = "south_north"
 ivartmp!2 = "west_east"

 varztmp = new((/1,4/),"float")
 varztmp!0 = "Time"
 varztmp!1 = "soil_layers_stag"
 
; Set up the time

 timetemp = stringtochar(timestring)
 timechar = new((/1,19/),"character")
 timechar(0,:) = timetemp(0:18)
 timechar!0 = "Time"
 timechar!1 = "DateStrLen"
 
   outfile->Times = timechar

 vartmp(0,0,0) = latitude                  ;  set the latitude
   vartmp@units = "degrees_north"
 
   outfile->XLAT = vartmp

 vartmp(0,0,0) = longitude                 ;  set the longitude
   vartmp@units = "degrees_east"
  
   outfile->XLONG = vartmp

 vartmp(0,0,0) = annual_mean_temperature   ;  set the deep soil temperature
   vartmp@units = "K"
 
   outfile->TMN = vartmp

 vartmp(0,0,0) = elevation                 ;  set the elevation
   vartmp@units = "m"
 
   outfile->HGT = vartmp

 vartmp(0,0,0) = 0.0                       ;  set the seaice (shouldn't be used since the points
   vartmp@units = ""                       ;   should be land
                                           
   outfile->SEAICE = vartmp

 vartmp(0,0,0) = 0.0                       ;  for future use
   vartmp@units = ""
 
   outfile->MAPFAC_MX = vartmp

 vartmp(0,0,0) = 0.0                       ;  for future use
   vartmp@units = ""
 
   outfile->MAPFAC_MY = vartmp

 vartmp(0,0,0) = vegetation_fraction       ;  set the maximum annual vegetation fraction
   vartmp@units = "%"
 
   outfile->SHDMAX = vartmp

 vartmp(0,0,0) = vegetation_fraction       ;  set the minimum annual vegetation fraction
   vartmp@units = "%"
 
   outfile->SHDMIN = vartmp

 vartmp(0,0,0) = leaf_area_index           ;  set the LAI (will initialized dynamic vegetation)
   vartmp@units = "m^2/m^2"
 
   outfile->LAI = vartmp

 ivartmp(0,0,0) = 1                        ;  set the landmask (1 = land)
   ivartmp@units = ""
 
 outfile->XLAND = ivartmp

 ivartmp(0,0,0) = vegetation_type          ;  set the land class types of each point
   ivartmp@units = ""                      ;  needs to be based on either MODIS or USGS class scheme
					   ;  make sure you set these below for consistency:
					   ;      outfile@MMINLU
					   ;      outfile@ISWATER
					   ;      outfile@ISURBAN
					   ;      outfile@ISICE
  
   outfile->IVGTYP = ivartmp

 ivartmp(0,0,:) = soil_type                ;  set the soil texture class of each point
   ivartmp@units = ""
 
 outfile->ISLTYP = ivartmp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  State initialization here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 infile = addfile(inname,"r")
; print(getfilevarnames(infile))

 indata = infile->WEASD_110_SFC
 vartmp(0,0,0) = (/ indata({latitude},{longitude}) /) ;  set the initial SWE
   vartmp@units = "kg/m^2"
 
 outfile->SNOW = vartmp

 vartmp(0,0,0) = 0.0                                  ;  set the initial canopy water content
   vartmp@units = "kg/m^2"                            ;  assuming zero without much effect
 
 outfile->CANWAT = vartmp

 varztmp(0,:) = (/0.1,0.3,0.6,1.0/)                   ;  set the soil layer thicknesses
   varztmp@units = "m"
 
 outfile->DZS = varztmp
 
 varztmp(0,:) = (/0.05,0.25,0.7,1.5/)                 ;  set the soil layer nodes
   varztmp@units = "m"
 
 outfile->ZS = varztmp
 
 delete(indata)
 indata = infile->TSOIL_110_DBLY
 var3tmp(0,:,0,0) = (/ indata(:,{latitude},{longitude}) /)  ;  set the initial soil temperature: all layers
   var3tmp@units = "K"
 
 outfile->TSLB = var3tmp

 vartmp(0,0,0) = (/ var3tmp(0,0,0,0) /)                     ;  set the initial surface temperature
   vartmp@units = "K"
 
 outfile->TSK = vartmp
 
 delete(indata)
 indata = infile->SOILM_110_DBLY                            ; NCL lumps six layers together: 0-10,0-100,0-200,
; print(indata(:,{latitude},{longitude}))                   ;  10-40,40-100,100-200 so you should probably check to 
                                                            ;  confirm they are being set properly
 var3tmp(0,0,0,0) = (/ indata(0,{latitude},{longitude}) /)  ;  set the initial soil moisture: layer 1
 var3tmp(0,1,0,0) = (/ indata(3,{latitude},{longitude}) /)  ;  set the initial soil moisture: layer 2
 var3tmp(0,2,0,0) = (/ indata(4,{latitude},{longitude}) /)  ;  set the initial soil moisture: layer 3
 var3tmp(0,3,0,0) = (/ indata(5,{latitude},{longitude}) /)  ;  set the initial soil moisture: layer 4

 var3tmp(0,0,0,0) = var3tmp(0,0,0,0) /  100.0               ; convert units from mm -> m^3/m^3
 var3tmp(0,1,0,0) = var3tmp(0,1,0,0) /  300.0
 var3tmp(0,2,0,0) = var3tmp(0,2,0,0) /  600.0
 var3tmp(0,3,0,0) = var3tmp(0,3,0,0) / 1000.0
   var3tmp@units = "m^3/m^3"
 
 outfile->SMOIS = var3tmp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Global attributes, many are not used and should be removed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


outfile@TITLE = "OUTPUT FROM VECTOR CREATION SCRIPTS: m.barlage v20150608" ;
outfile@DX        = 1000.0  ; not used
outfile@DY        = 1000.0  ; not used
outfile@TRUELAT1  = 45.0    ; not used
outfile@TRUELAT2  = 45.0    ; not used
outfile@STAND_LON = 45.0    ; not used
outfile@MAP_PROJ  = 1       ; not used
outfile@GRID_ID   = 1       ; used for grid labeling
outfile@ISWATER   = 17      ; water type in land classification       (17 for MODIS; 16 for USGS)
outfile@ISURBAN   = 13      ; urban type in land classification       (13 for MODIS;  1 for USGS)
outfile@ISICE     = 15      ; snow/ice type in land classification    (15 for MODIS; 24 for USGS)
outfile@MMINLU    = "MODIFIED_IGBP_MODIS_NOAH"  ; land classification (USGS or MODIFIED_IGBP_MODIS_NOAH)


 system("mv "+setup_filename+".nc "+setup_filename)  ; get rid of the .nc

end
