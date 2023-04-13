# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../../hrldas/user_build_options

OBJS = \
        module_bep_bem_helper.o \
	module_sf_urban.o \
	module_sf_bep.o \
	module_sf_bem.o \
	module_sf_bep_bem.o \
	NoahmpUrbanDriverMainMod.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

NoahmpUrbanDriverMainMod.o: NoahmpUrbanDriverMainMod.F
	@echo ""
	$(RM) NoahmpUrbanDriverMainMod.f90
	$(CPP) $(CPPFLAGS) $(*).F > $(*).f90
	$(COMPILERF90) -c -I../../noahmp/drivers/hrldas -I. -I../../hrldas/Utility_routines \
        -I../../noahmp/utility $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
	@echo ""

.F.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f90
	$(COMPILERF90) -c  -I. -I../../hrldas/Utility_routines \
        $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
	@echo ""

#
# Dependencies:
#

NoahmpUrbanDriverMainMod.o: ../../noahmp/utility/Machine.o \
                            ../../noahmp/drivers/hrldas/NoahmpIOVarType.o \
			    ../../hrldas/Utility_routines/module_ra_gfdleta.o \
			    ../../hrldas/Utility_routines/module_model_constants.o \
			    module_sf_urban.o module_sf_bep.o module_sf_bep_bem.o
module_sf_bep.o:	module_sf_urban.o module_bep_bem_helper.o 
module_sf_bep_bem.o:	module_sf_urban.o module_sf_bem.o module_bep_bem_helper.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~ *.f90

