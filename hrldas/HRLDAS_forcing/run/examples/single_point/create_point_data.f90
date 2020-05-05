
use netcdf
implicit none

! Location Information

real    :: latitude                  ! latitude [degrees]
real    :: longitude                 ! longitude [degrees]
integer :: vegetation_category       ! category based on land_cover_source
integer :: soil_category             ! soil texture class
real    :: deep_soil_temperature     ! deep soil temperature [K]
real    :: elevation                 ! elevation [m]
real    :: sea_ice                   ! sea ice fraction [-] (should be 0.0 for a land point)
real    :: maximum_vegetation_pct    ! maximum annual vegetation percentage [0-100]
real    :: minimum_vegetation_pct    ! minimum annual vegetation percentage [0-100]
integer :: land_mask                 ! land mask [-] (should be 1 for a land point)
integer :: nsoil                     ! number of soil layers

! Initial States

real                            :: snow_depth              ! snow depth [m]
real                            :: snow_water_equivalent   ! snow water equivalent [m]
real                            :: canopy_water            ! canopy water [mm]
real                            :: skin_temperature        ! skin temperature [K]
real                            :: leaf_area_index         ! leaf area index [m2/m2] 
real, allocatable, dimension(:) :: soil_layer_thickness    ! layer thicknesses [m]
real, allocatable, dimension(:) :: soil_layer_nodes        ! layer centroids from surface [m]
real, allocatable, dimension(:) :: soil_temperature        ! layer soil temperature [K]
real, allocatable, dimension(:) :: soil_moisture           ! layer volumetric total water content [m3/m3]
real, allocatable, dimension(:) :: soil_liquid             ! layer volumetric liquid content [m3/m3]

! Metadata

integer       :: grid_id                ! used for grid labeling
integer       :: water_classification   ! water type in land classification
integer       :: urban_classification   ! urban type in land classification
integer       :: ice_classification     ! snow/ice type in land classification
character*100 :: land_cover_source      ! land classification

! Conversion

logical :: have_relative_humidity       ! set to true if you need to convert relative to specific
real    :: temperature_offset           ! 273.15 to convert C to K
real    :: temperature_scale            ! can be used to convert F to C
real    :: pressure_scale               ! 100.0 to convert mb to Pa
real    :: precipitation_scale          ! 0.014111 to convert inches/30min to mm/s

! Text forcing variables

integer       :: yyyy
integer       :: mm
integer       :: dd
integer       :: hh
integer       :: nn
real          :: uwind_speed
real          :: vwind_speed = 0.0
real          :: temperature
real          :: humidity
real          :: pressure
real          :: solar_radiation
real          :: longwave_radiation
real          :: precipitation

! NETCDF variables

integer :: ncid
integer :: iret
integer :: dim_id_time, dim_id_sn, dim_id_we, dim_id_soil
integer :: var_id_xlat   , var_id_xlong , var_id_hgt    , var_id_tmn    ,  &
           var_id_veg    , var_id_soil  , var_id_seaice , var_id_vegmax ,  &
	   var_id_vegmin , var_id_mask
integer :: var_id_depth  , var_id_swe   , var_id_canwat , var_id_tsk    ,  &
           var_id_thick  , var_id_nodes , var_id_stemp  , var_id_smois  ,  &
	   var_id_sliq   , var_id_lai
integer :: var_id_grid   , var_id_water , var_id_urban  , var_id_ice    ,  &
           var_id_source    
integer :: var_id_force_temp  , var_id_force_humid , var_id_force_pres   , &
           var_id_force_uwind , var_id_force_vwind ,                       &
	   var_id_force_sw    , var_id_force_lw    , var_id_force_precip    

! Misc variables

integer           :: itime
character(len=27) :: filename
real              :: e, svp     ! for relative humidity conversion

namelist / location / latitude, longitude, vegetation_category, soil_category, &
  deep_soil_temperature, elevation, sea_ice, maximum_vegetation_pct, &
  minimum_vegetation_pct, land_mask, nsoil
  
namelist / initial / snow_depth, snow_water_equivalent, canopy_water, &
  skin_temperature, soil_layer_thickness, soil_layer_nodes, &
  soil_temperature, soil_moisture, soil_liquid, leaf_area_index

namelist / metadata / grid_id, water_classification, urban_classification, & 
  ice_classification, land_cover_source
  
namelist / conversion / have_relative_humidity, temperature_offset, &
  temperature_scale, pressure_scale, precipitation_scale

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Start creation of hrldas setup file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

open(30, file="bondville.dat", form="FORMATTED")
read(30, location)

allocate(soil_layer_thickness(nsoil))
allocate(soil_layer_nodes(nsoil))
allocate(soil_temperature(nsoil))
allocate(soil_moisture(nsoil))
allocate(soil_liquid(nsoil))

read(30, initial)
read(30, metadata)
read(30, conversion)
close(30)

! Create the NetCDF file mimicing WRF input files.

  iret = nf90_create("hrldas_setup_single_point.nc", NF90_CLOBBER, ncid)

! Define dimensions in the file.

  iret = nf90_def_dim(ncid, "west_east"       , 1             , dim_id_we)
  iret = nf90_def_dim(ncid, "south_north"     , 1             , dim_id_sn)
  iret = nf90_def_dim(ncid, "soil_layers_stag", nsoil         , dim_id_soil)
  iret = nf90_def_dim(ncid, "Time"            , NF90_UNLIMITED, dim_id_time)


  iret = nf90_def_var(ncid,   "XLAT", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_xlat)
    iret = nf90_put_att(ncid, var_id_xlat, "description", "latitude")
    iret = nf90_put_att(ncid, var_id_xlat, "units", "degrees_north")

  iret = nf90_def_var(ncid,  "XLONG", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_xlong)
    iret = nf90_put_att(ncid, var_id_xlong, "description", "longitude")
    iret = nf90_put_att(ncid, var_id_xlong, "units", "degrees_east")

  iret = nf90_def_var(ncid,    "TMN", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_tmn)
    iret = nf90_put_att(ncid, var_id_tmn, "description", "soil temperature lower boundary")
    iret = nf90_put_att(ncid, var_id_tmn, "units", "K")

  iret = nf90_def_var(ncid,    "HGT", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_hgt)
    iret = nf90_put_att(ncid, var_id_hgt, "description", "elevation")
    iret = nf90_put_att(ncid, var_id_hgt, "units", "m")

  iret = nf90_def_var(ncid, "SEAICE", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_seaice)
    iret = nf90_put_att(ncid, var_id_seaice, "description", "sea ice fraction")
    iret = nf90_put_att(ncid, var_id_seaice, "units", "-")

  iret = nf90_def_var(ncid, "SHDMAX", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_vegmax)
    iret = nf90_put_att(ncid, var_id_vegmax, "description", "maximum annual vegetation cover")
    iret = nf90_put_att(ncid, var_id_vegmax, "units", "%")

  iret = nf90_def_var(ncid, "SHDMIN", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_vegmin)
    iret = nf90_put_att(ncid, var_id_vegmin, "description", "minimum annual vegetation cover")
    iret = nf90_put_att(ncid, var_id_vegmin, "units", "%")

  iret = nf90_def_var(ncid,    "LAI", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_lai)
    iret = nf90_put_att(ncid, var_id_lai, "description", "leaf area index")
    iret = nf90_put_att(ncid, var_id_lai, "units", "m2/m2")

  iret = nf90_def_var(ncid,  "XLAND", NF90_INT,   (/dim_id_we,dim_id_sn,dim_id_time/), var_id_mask)
    iret = nf90_put_att(ncid, var_id_mask, "description", "land mask: 1=land/ice")
    iret = nf90_put_att(ncid, var_id_mask, "units", "-")

  iret = nf90_def_var(ncid, "IVGTYP", NF90_INT,   (/dim_id_we,dim_id_sn,dim_id_time/), var_id_veg)
    iret = nf90_put_att(ncid, var_id_veg, "description", "vegetation type")
    iret = nf90_put_att(ncid, var_id_veg, "units", "-")

  iret = nf90_def_var(ncid, "ISLTYP", NF90_INT,   (/dim_id_we,dim_id_sn,dim_id_time/), var_id_soil)
    iret = nf90_put_att(ncid, var_id_soil, "description", "soil texture type")
    iret = nf90_put_att(ncid, var_id_soil, "units", "-")

  iret = nf90_def_var(ncid,   "SNOW", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_swe)
    iret = nf90_put_att(ncid, var_id_swe, "description", "snow water equivalent")
    iret = nf90_put_att(ncid, var_id_swe, "units", "mm")

  iret = nf90_def_var(ncid, "SNODEP",  NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_depth)
    iret = nf90_put_att(ncid, var_id_depth, "description", "snow depth")
    iret = nf90_put_att(ncid, var_id_depth, "units", "m")

  iret = nf90_def_var(ncid, "CANWAT", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_canwat)
    iret = nf90_put_att(ncid, var_id_canwat, "description", "canopy surface water")
    iret = nf90_put_att(ncid, var_id_canwat, "units", "mm")

  iret = nf90_def_var(ncid,    "TSK", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_tsk)
    iret = nf90_put_att(ncid, var_id_tsk, "description", "surface skin temperature")
    iret = nf90_put_att(ncid, var_id_tsk, "units", "K")

  iret = nf90_def_var(ncid,    "DZS", NF90_FLOAT, (/dim_id_soil,dim_id_time/)        , var_id_thick)
    iret = nf90_put_att(ncid, var_id_thick, "description", "soil layer thicknesses")
    iret = nf90_put_att(ncid, var_id_thick, "units", "m")

  iret = nf90_def_var(ncid,     "ZS", NF90_FLOAT, (/dim_id_soil,dim_id_time/)        , var_id_nodes)
    iret = nf90_put_att(ncid, var_id_nodes, "description", "soil node depths")
    iret = nf90_put_att(ncid, var_id_nodes, "units", "m")

  iret = nf90_def_var(ncid,   "TSLB", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_soil,dim_id_time/), var_id_stemp)
    iret = nf90_put_att(ncid, var_id_stemp, "description", "soil temperature")
    iret = nf90_put_att(ncid, var_id_stemp, "units", "K")

  iret = nf90_def_var(ncid,  "SMOIS", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_soil,dim_id_time/), var_id_smois)
    iret = nf90_put_att(ncid, var_id_smois, "description", "volumetric soil moisture")
    iret = nf90_put_att(ncid, var_id_smois, "units", "m3/m3")

  ! File metadata:

  iret = nf90_put_att(ncid, NF90_GLOBAL, "GRID_ID"  , grid_id)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "ISWATER"  , water_classification)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "ISURBAN"  , urban_classification)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "ISICE"    , ice_classification)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "MMINLU"   , land_cover_source)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "DX"       , 0.0)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "DY"       , 0.0)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT1" , 0.0)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT2" , 0.0)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "STAND_LON", 0.0)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ" , 0)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "TITLE"    , "Created from HRLDAS create_point_data")

  iret = nf90_enddef(ncid)

  iret = nf90_put_var(ncid, var_id_xlat  , latitude              )
  iret = nf90_put_var(ncid, var_id_xlong , longitude             )
  iret = nf90_put_var(ncid, var_id_tmn   , deep_soil_temperature )
  iret = nf90_put_var(ncid, var_id_hgt   , elevation             )
  iret = nf90_put_var(ncid, var_id_seaice, sea_ice               )
  iret = nf90_put_var(ncid, var_id_vegmax, maximum_vegetation_pct)
  iret = nf90_put_var(ncid, var_id_vegmin, minimum_vegetation_pct)
  iret = nf90_put_var(ncid, var_id_lai   , leaf_area_index       )
  iret = nf90_put_var(ncid, var_id_mask  , land_mask             )
  iret = nf90_put_var(ncid, var_id_veg   , vegetation_category   )
  iret = nf90_put_var(ncid, var_id_soil  , soil_category         )
  iret = nf90_put_var(ncid, var_id_swe   , snow_water_equivalent )
  iret = nf90_put_var(ncid, var_id_depth , snow_depth            )
  iret = nf90_put_var(ncid, var_id_canwat, canopy_water          )
  iret = nf90_put_var(ncid, var_id_tsk   , skin_temperature      )
  iret = nf90_put_var(ncid, var_id_thick , soil_layer_thickness  )
  iret = nf90_put_var(ncid, var_id_nodes , soil_layer_nodes      )
  iret = nf90_put_var(ncid, var_id_stemp , soil_temperature      , count = (/1,1,4,1/))
  iret = nf90_put_var(ncid, var_id_smois , soil_moisture         , count = (/1,1,4,1/))
  
  iret = nf90_close(ncid)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Start creation of forcing data files
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


open(30, file="bondville.dat", form="FORMATTED")

do itime = 1, 54
  read(30,*)
end do

forcing: do 

read(30,*,end=1000) yyyy, mm, dd, hh, nn, uwind_speed, &
           temperature, humidity, pressure, &
	   solar_radiation, longwave_radiation, precipitation

! Do some conversions

temperature = temperature_scale * temperature + temperature_offset
pressure = pressure_scale * pressure
precipitation = precipitation_scale * precipitation

if(have_relative_humidity) then
  svp = 611.2*exp(17.67*(temperature-273.15)/(temperature-29.65)) ! [Pa]
  e   = humidity/100.0 * svp                                      ! [Pa]
  humidity = (0.622*e)/(pressure-(1.0-0.622)*e)                   ! now it is specific humidity
end if

! Write to NetCDF file

write(filename,'(i4,4i2.2,a15)') yyyy, mm, dd, hh, nn, ".LDASIN_DOMAIN1"
write(*,*) "Creating: "//filename

  iret = nf90_create(filename, NF90_CLOBBER, ncid)

! Define dimensions in the file.

  iret = nf90_def_dim(ncid, "west_east"       , 1             , dim_id_we)
  iret = nf90_def_dim(ncid, "south_north"     , 1             , dim_id_sn)
  iret = nf90_def_dim(ncid, "Time"            , NF90_UNLIMITED, dim_id_time)

! Define variables in the file.

  iret = nf90_def_var(ncid,   "T2D", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_force_temp)
    iret = nf90_put_att(ncid, var_id_force_temp, "description", "temperature")
    iret = nf90_put_att(ncid, var_id_force_temp, "units", "K")

  iret = nf90_def_var(ncid,   "Q2D", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_force_humid)
    iret = nf90_put_att(ncid, var_id_force_humid, "description", "specific humidity")
    iret = nf90_put_att(ncid, var_id_force_humid, "units", "kg/kg")

  iret = nf90_def_var(ncid,   "PSFC", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_force_pres)
    iret = nf90_put_att(ncid, var_id_force_pres, "description", "surface pressure")
    iret = nf90_put_att(ncid, var_id_force_pres, "units", "Pa")

  iret = nf90_def_var(ncid,   "U2D", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_force_uwind)
    iret = nf90_put_att(ncid, var_id_force_uwind, "description", "zonal wind")
    iret = nf90_put_att(ncid, var_id_force_uwind, "units", "m/s")

  iret = nf90_def_var(ncid,   "V2D", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_force_vwind)
    iret = nf90_put_att(ncid, var_id_force_vwind, "description", "meridional wind")
    iret = nf90_put_att(ncid, var_id_force_vwind, "units", "m/s")

  iret = nf90_def_var(ncid,   "LWDOWN", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_force_lw)
    iret = nf90_put_att(ncid, var_id_force_lw, "description", "downward longwave radiation")
    iret = nf90_put_att(ncid, var_id_force_lw, "units", "W/m2")

  iret = nf90_def_var(ncid,   "SWDOWN", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_force_sw)
    iret = nf90_put_att(ncid, var_id_force_sw, "description", "downward shortwave radiation")
    iret = nf90_put_att(ncid, var_id_force_sw, "units", "W/m2")

  iret = nf90_def_var(ncid,   "RAINRATE", NF90_FLOAT, (/dim_id_we,dim_id_sn,dim_id_time/), var_id_force_precip)
    iret = nf90_put_att(ncid, var_id_force_precip, "description", "temperature")
    iret = nf90_put_att(ncid, var_id_force_precip, "units", "mm/s")

  iret = nf90_put_att(ncid, NF90_GLOBAL, "TITLE"  , "Created from HRLDAS create_point_data")

  iret = nf90_enddef(ncid)

  iret = nf90_put_var(ncid, var_id_force_temp    , temperature        )
  iret = nf90_put_var(ncid, var_id_force_humid   , humidity           )
  iret = nf90_put_var(ncid, var_id_force_pres    , pressure           )
  iret = nf90_put_var(ncid, var_id_force_uwind   , uwind_speed        )
  iret = nf90_put_var(ncid, var_id_force_vwind   , vwind_speed        )
  iret = nf90_put_var(ncid, var_id_force_sw      , solar_radiation    )
  iret = nf90_put_var(ncid, var_id_force_lw      , longwave_radiation )
  iret = nf90_put_var(ncid, var_id_force_precip  , precipitation      )

  iret = nf90_close(ncid)

end do forcing

1000 continue

end
