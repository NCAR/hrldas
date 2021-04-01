# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../../hrldas/user_build_options

OBJS = \
        module_bep_bem_helper.o \
	module_sf_noahmpdrv.o \
	module_sf_noahmplsm.o \
	module_sf_noahmp_glacier.o \
	module_sf_noahmp_groundwater.o \
	module_sf_gecros.o \
	module_sf_urban.o \
	module_sf_bep.o \
	module_sf_bem.o \
	module_sf_bep_bem.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

module_sf_noahmplsm.o: ../../noahmp/src/module_sf_noahmplsm.F
	@echo ""
	$(RM) module_sf_noahmplsm.f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) ../../noahmp/src/module_sf_noahmplsm.F > module_sf_noahmplsm.f90
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) module_sf_noahmplsm.f90
	@echo ""

.F.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f90
	$(COMPILERF90) -o $(@) -c -I../../hrldas/Utility_routines $(F90FLAGS) $(FREESOURCE) $(*).f90
	@echo ""

#
# Dependencies:
#
module_sf_noahmplsm.o:	module_sf_gecros.o
module_sf_noahmpdrv.o:	module_sf_noahmplsm.o module_sf_noahmp_glacier.o module_sf_noahmp_groundwater.o \
                        module_sf_urban.o module_sf_bep.o module_sf_bep_bem.o
module_sf_noahmp_groundwater.o: module_sf_noahmplsm.o
module_sf_bep.o:	module_sf_urban.o module_bep_bem_helper.o 
module_sf_bep_bem.o:	module_sf_urban.o module_sf_bem.o module_bep_bem_helper.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~ *.f90

