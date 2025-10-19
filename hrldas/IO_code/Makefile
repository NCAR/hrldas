# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F .F90

include ../user_build_options

OBJS_NoahMP = module_NoahMP_hrldas_driver.o

OBJS = \
	main_hrldas_driver.o \
	module_hrldas_netcdf_io.o\
	MosaicAverageMod.o\
	WriteNoahmpMosaicGridAverageOutputMod.o\
	WriteNoahmpMosaicSubgridOutputMod.o\
	WriteNoahmpOutputMod.o\
	WriteNoahmpMosaicRestartMod.o\
	WriteNoahmpRestartMod.o\
	ReadNoahmpMosaicRestartMod.o\
	ReadNoahmpRestartMod.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_ $(MOD_OPT)

all:	$(OBJS_NoahMP) $(OBJS)

NoahMP : $(OBJS_NoahMP) $(OBJS)

module_NoahMP_hrldas_driver.o: module_NoahMP_hrldas_driver.F
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f90
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(MODFLAG) -I. \
	-I../../noahmp/src -I../../noahmp/utility -I../../noahmp/drivers/hrldas -I../../urban/wrf -I../Utility_routines $(NETCDFMOD) $(*).f90
	@echo ""

main_hrldas_driver.o: main_hrldas_driver.F
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f90
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) -I../MPP -I. \
	-I../../noahmp/src -I../../noahmp/utility -I../../noahmp/drivers/hrldas -I../../urban/wrf -I../Utility_routines $(NETCDFMOD) $(*).f90
	@echo ""

module_hrldas_netcdf_io.o: module_hrldas_netcdf_io.F
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(NETCDFMOD) $(*).F > $(*).f90
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) -I../MPP -I../Utility_routines -I../../noahmp/utility $(NETCDFMOD)  $(*).f90
	@echo ""

.F.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f90
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(*).f90
	@echo ""

.F90.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F90 > $(*).f90
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) -I. -I../../noahmp/utility -I../../noahmp/drivers/hrldas $(*).f90
	@echo ""

#
# Dependencies:
#

main_hrldas_driver.o:	$(OBJS_NoahMP)
$(OBJS_NoahMP):	module_hrldas_netcdf_io.o
MosaicAverageMod.o: ../../noahmp/utility/Machine.o\
                    ../../noahmp/drivers/hrldas/NoahmpIOVarType.o                
WriteNoahmpMosaicGridAverageOutputMod.o:  ../../noahmp/utility/Machine.o\
                                          ../../noahmp/drivers/hrldas/NoahmpIOVarType.o\
										  module_hrldas_netcdf_io\
										  MosaicAverageMod.o   
WriteNoahmpMosaicSubgridOutputMod.o:  ../../noahmp/utility/Machine.o\
                                      ../../noahmp/drivers/hrldas/NoahmpIOVarType.o\
									  module_hrldas_netcdf_io
WriteNoahmpOutputMod.o: ../../noahmp/utility/Machine.o\
                        ../../noahmp/drivers/hrldas/NoahmpIOVarType.o\
						module_hrldas_netcdf_io
WriteNoahmpMosaicRestartMod.o: ../../noahmp/utility/Machine.o\
                               ../../noahmp/drivers/hrldas/NoahmpIOVarType.o\
						       module_hrldas_netcdf_io
WriteNoahmpRestartMod.o: ../../noahmp/utility/Machine.o\
                         ../../noahmp/drivers/hrldas/NoahmpIOVarType.o\
						 module_hrldas_netcdf_io
ReadNoahmpMosaicRestartMod.o: ../../noahmp/utility/Machine.o\
                              ../../noahmp/drivers/hrldas/NoahmpIOVarType.o\
						      module_hrldas_netcdf_io
ReadNoahmpRestartMod.o: ../../noahmp/utility/Machine.o\
                        ../../noahmp/drivers/hrldas/NoahmpIOVarType.o\
						module_hrldas_netcdf_io

# This command cleans up object files, etc.
clean:
	$(RM) *.o *.mod *.stb *~ *.f90
