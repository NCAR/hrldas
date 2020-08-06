use netcdf
implicit none
integer            :: ncid,ierr
integer, parameter :: NDIMS = 3, NRECS = 1
integer, parameter :: NLATS = 600, NLONS = 1440
character (len = *), parameter :: LAT_NAME = "latitude"
character (len = *), parameter :: LON_NAME = "longitude"
character (len = *), parameter :: REC_NAME = "time"
integer :: lon_dimid, lat_dimid, rec_dimid

integer :: start(NDIMS), count(NDIMS)
real :: lats(NLATS), lons(NLONS)
integer :: lon_varid, lat_varid

character (len = *), parameter :: RAIN_NAME = "Rainf_tavg" 
character (len = *), parameter :: SNOW_NAME = "Snowf_tavg"
character (len = *), parameter :: PREC_NAME = "Precf_tavg"
integer :: rain_varid,snow_varid,prec_varid
integer :: dimids(NDIMS)

! We recommend that each variable carry a "units" attribute.
character (len = *), parameter :: UNITS = "units"
character (len = *), parameter :: RAIN_UNITS = "kg m-2 s-1"
character (len = *), parameter :: SNOW_UNITS = "kg m-2 s-1"
character (len = *), parameter :: PREC_UNITS = "kg m-2 s-1"
character (len = *), parameter :: LAT_UNITS = "degrees_north"
character (len = *), parameter :: LON_UNITS = "degrees_east"
real    :: rain_in(NLONS, NLATS), snow_in(NLONS, NLATS), prec_out(NLONS, NLATS)

! Loop indices
integer :: lat, lon, rec, i
character*100   :: infile1, infile2, outfile  

call getarg(1,infile1)
call getarg(2,infile2)
call getarg(3,outfile)

! READ IN RAIN VARIABLE
print *,"Reading in infile: ",infile1
ierr = nf90_open(infile1,nf90_nowrite, ncid)
if (ierr/=0) then
print *, "can't find Rain file: ",infile1
call abort
else
ierr = nf90_inq_varid(ncid,RAIN_NAME,rain_varid)
ierr = nf90_get_var(ncid,rain_varid,rain_in)
ierr = nf90_inq_varid(ncid,"lat",lat_varid)
ierr = nf90_inq_varid(ncid,"lon",lon_varid)
ierr = nf90_get_var(ncid,lat_varid,lats)
ierr = nf90_get_var(ncid,lon_varid,lons)
ierr = nf90_close(ncid)
! READ IN SNOW VARIABLE

print *,"Reading in infile: ",infile2
ierr = nf90_open(infile2,nf90_nowrite, ncid)
if (ierr/=0) then
print *, "can't find Snow file: ",infile2
call abort
else
ierr = nf90_inq_varid(ncid,SNOW_NAME,snow_varid)
ierr = nf90_get_var(ncid,snow_varid,snow_in)
ierr = nf90_get_var(ncid,lat_varid,lats)
ierr = nf90_get_var(ncid,lon_varid,lons)
ierr = nf90_close(ncid)
! WRITE WIND SPEED to U component
prec_out = rain_in + snow_in
where (prec_out<0) prec_out = -9999 
! Create the file. 
ierr = nf90_create(outfile, nf90_clobber, ncid) 
! Define the dimensions. The record dimension is defined to have
! unlimited length - it can grow as needed. In this example it is
! the time dimension.
ierr = nf90_def_dim(ncid, LAT_NAME, NLATS, lat_dimid) 
ierr = nf90_def_dim(ncid, LON_NAME, NLONS, lon_dimid)
ierr = nf90_def_dim(ncid, REC_NAME, NF90_UNLIMITED, rec_dimid)
! Define the coordinate variables. We will only define coordinate
! variables for lat and lon.  Ordinarily we would need to provide
! an array of dimension IDs for each variable's dimensions, but
! since coordinate variables only have one dimension, we can
! simply provide the address of that dimension ID (lat_dimid) and
! similarly for (lon_dimid).
ierr = nf90_def_var(ncid, LAT_NAME, NF90_REAL, lat_dimid, lat_varid) 
ierr = nf90_def_var(ncid, LON_NAME, NF90_REAL, lon_dimid, lon_varid)
! Assign units attributes to coordinate variables.
ierr = nf90_put_att(ncid,lat_varid,UNITS,LAT_UNITS)
ierr = nf90_put_att(ncid,lon_varid,UNITS,LON_UNITS)
! The dimids array is used to pass the dimids of the dimensions of
! the netCDF variables. Both of the netCDF variables we are creating
! share the same four dimensions. In Fortran, the unlimited
! dimension must come last on the list of dimids.
dimids = (/ lon_dimid, lat_dimid, rec_dimid /)
! Define the netCDF variables for the u variable
ierr = nf90_def_var(ncid, PREC_NAME, NF90_REAL, dimids, prec_varid)
! Assign units attributes to the netCDF variables.
ierr = nf90_put_att(ncid, prec_varid, UNITS, PREC_UNITS) 
ierr = nf90_put_att(ncid, prec_varid, "missing_value",-9999)
! End define mode
ierr = nf90_enddef(ncid)
! Write the coordinate variable data. This will put the latitudes
! and longitudes of our data grid into the netCDF file.
ierr = nf90_put_var(ncid, lat_varid, lats) 
ierr = nf90_put_var(ncid, lon_varid, lons) 
! These settings tell netcdf to write one timestep of data. (The
! setting of start(4) inside the loop below tells netCDF which
! timestep to write.)
count = (/ NLONS, NLATS, 1 /)
start = (/ 1, 1, 1 /)
do rec = 1, NRECS
   start(3) = rec
   ierr =  nf90_put_var(ncid, prec_varid, prec_out, start = start, &
                            count = count) 
end do
ierr = nf90_close(ncid)
endif 
endif

print *, "*** SUCCESS combining precipitation file ", outfile
end
