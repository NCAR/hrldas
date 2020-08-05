
include ../../../../user_build_options

all:	
	$(COMPILERF90) $(NETCDFMOD) -o combine_precips_netcdf.exe $(F90FLAGS) combine_precips_netcdf.f90 $(NETCDFLIB)
	$(COMPILERF90) $(NETCDFMOD) -o create_UV_netcdf.exe $(F90FLAGS) create_UV_netcdf.f90 $(NETCDFLIB)
clean:
	$(RM) *.o *~ *.exe 
#





