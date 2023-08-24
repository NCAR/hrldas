.SUFFIXES:
.SUFFIXES: .o .f

ifeq ($(WRF_HYDRO_RAPID),1)
include ${TAO_DIR}/bmake/tao_common
endif

include ../user_build_options

ifeq ($(WRF_HYDRO_RAPID),1)
PHDF5_INC=-I ${TACC_HDF5_INC}
RAPID_MACRO = ${TAO_FORTRAN_LIB} ${TAO_LIB} ${PETSC_LIB} ${PHDF5_INC} \
        -Wl,-rpath,${TACC_HDF5_LIB} -L${TACC_HDF5_LIB} -lhdf5 -lz 
RAPID_LIB =  -lrapid
else
RAPID_MACRO = 
RAPID_LIB =  
endif

OBJS_NoahMP = ../IO_code/module_NoahMP_hrldas_driver.o 

OBJS = \
	../IO_code/main_hrldas_driver.o \
	../IO_code/module_hrldas_netcdf_io.o \
        ../../urban/wrf/module_bep_bem_helper.o \
	../../urban/wrf/module_sf_urban.o \
	../../urban/wrf/module_sf_bep.o \
	../../urban/wrf/module_sf_bem.o \
	../../urban/wrf/module_sf_bep_bem.o \
        ../../urban/wrf/NoahmpUrbanDriverMainMod.o \
	../Utility_routines/module_wrf_utilities.o \
	../Utility_routines/module_model_constants.o \
	../Utility_routines/module_date_utilities.o \
	../Utility_routines/kwm_string_utilities.o \
        ../Utility_routines/module_wrf_error.o \
	../Utility_routines/module_ra_gfdleta.o \
        ../Utility_routines/module_domain.o\
        ../../noahmp/drivers/hrldas/NoahmpDriverMainMod.o \
        ../../noahmp/drivers/hrldas/NoahmpInitMainMod.o \
        ../../noahmp/drivers/hrldas/NoahmpGroundwaterInitMod.o \
        ../../noahmp/drivers/hrldas/NoahmpSnowInitMod.o \
        ../../noahmp/drivers/hrldas/NoahmpIOVarType.o \
        ../../noahmp/drivers/hrldas/NoahmpIOVarInitMod.o \
        ../../noahmp/drivers/hrldas/NoahmpReadNamelistMod.o \
        ../../noahmp/drivers/hrldas/NoahmpReadTableMod.o \
        ../../noahmp/drivers/hrldas/ConfigVarOutTransferMod.o \
        ../../noahmp/drivers/hrldas/ForcingVarOutTransferMod.o \
        ../../noahmp/drivers/hrldas/EnergyVarOutTransferMod.o \
        ../../noahmp/drivers/hrldas/WaterVarOutTransferMod.o \
        ../../noahmp/drivers/hrldas/BiochemVarOutTransferMod.o \
        ../../noahmp/drivers/hrldas/ConfigVarInTransferMod.o \
        ../../noahmp/drivers/hrldas/ForcingVarInTransferMod.o \
        ../../noahmp/drivers/hrldas/EnergyVarInTransferMod.o \
        ../../noahmp/drivers/hrldas/WaterVarInTransferMod.o \
        ../../noahmp/drivers/hrldas/BiochemVarInTransferMod.o \
        ../../noahmp/drivers/hrldas/PedoTransferSR2006Mod.o \
        ../../noahmp/utility/Machine.o \
        ../../noahmp/utility/CheckNanMod.o \
        ../../noahmp/src/ConstantDefineMod.o \
        ../../noahmp/src/ConfigVarType.o \
        ../../noahmp/src/ForcingVarType.o \
        ../../noahmp/src/EnergyVarType.o \
        ../../noahmp/src/WaterVarType.o \
        ../../noahmp/src/BiochemVarType.o \
        ../../noahmp/src/NoahmpVarType.o \
        ../../noahmp/src/ConfigVarInitMod.o \
        ../../noahmp/src/ForcingVarInitMod.o \
        ../../noahmp/src/EnergyVarInitMod.o \
        ../../noahmp/src/WaterVarInitMod.o \
        ../../noahmp/src/BiochemVarInitMod.o \
        ../../noahmp/src/CanopyHydrologyMod.o \
        ../../noahmp/src/GroundWaterTopModelMod.o \
        ../../noahmp/src/IrrigationFloodMod.o \
        ../../noahmp/src/IrrigationInfilPhilipMod.o \
        ../../noahmp/src/IrrigationMicroMod.o \
        ../../noahmp/src/MatrixSolverTriDiagonalMod.o \
        ../../noahmp/src/RunoffSubSurfaceDrainageMod.o \
        ../../noahmp/src/RunoffSubSurfaceEquiWaterTableMod.o \
        ../../noahmp/src/RunoffSubSurfaceGroundWaterMod.o \
        ../../noahmp/src/RunoffSubSurfaceShallowMmfMod.o \
        ../../noahmp/src/RunoffSurfaceBatsMod.o \
        ../../noahmp/src/RunoffSurfaceDynamicVicMod.o \
        ../../noahmp/src/RunoffSurfaceExcessDynamicVicMod.o \
        ../../noahmp/src/RunoffSurfaceFreeDrainMod.o \
        ../../noahmp/src/RunoffSurfaceTopModelEquiMod.o \
        ../../noahmp/src/RunoffSurfaceTopModelGrdMod.o \
        ../../noahmp/src/RunoffSurfaceTopModelMmfMod.o \
        ../../noahmp/src/RunoffSurfaceVicMod.o \
        ../../noahmp/src/RunoffSurfaceXinAnJiangMod.o \
        ../../noahmp/src/ShallowWaterTableMmfMod.o \
        ../../noahmp/src/SnowfallBelowCanopyMod.o \
        ../../noahmp/src/SnowLayerCombineMod.o \
        ../../noahmp/src/SnowLayerDivideMod.o \
        ../../noahmp/src/SnowLayerWaterComboMod.o \
        ../../noahmp/src/SnowpackCompactionMod.o \
        ../../noahmp/src/SnowpackHydrologyMod.o \
        ../../noahmp/src/SnowWaterMainMod.o \
        ../../noahmp/src/SoilHydraulicPropertyMod.o \
        ../../noahmp/src/SoilMoistureSolverMod.o \
        ../../noahmp/src/SoilWaterDiffusionRichardsMod.o \
        ../../noahmp/src/SoilWaterInfilGreenAmptMod.o \
        ../../noahmp/src/SoilWaterInfilPhilipMod.o \
        ../../noahmp/src/SoilWaterInfilSmithParlangeMod.o \
        ../../noahmp/src/SoilWaterMainMod.o \
        ../../noahmp/src/TileDrainageEquiDepthMod.o \
        ../../noahmp/src/TileDrainageHooghoudtMod.o \
        ../../noahmp/src/TileDrainageSimpleMod.o \
        ../../noahmp/src/WaterMainMod.o \
        ../../noahmp/src/WaterTableDepthSearchMod.o \
        ../../noahmp/src/WaterTableEquilibriumMod.o \
        ../../noahmp/src/IrrigationTriggerMod.o \
        ../../noahmp/src/IrrigationSprinklerMod.o \
        ../../noahmp/src/CanopyWaterInterceptMod.o \
        ../../noahmp/src/PrecipitationHeatAdvectMod.o \
        ../../noahmp/src/SnowThermalPropertyMod.o \
        ../../noahmp/src/SoilThermalPropertyMod.o \
        ../../noahmp/src/GroundThermalPropertyMod.o \
        ../../noahmp/src/EnergyMainMod.o \
        ../../noahmp/src/NoahmpMainMod.o \
        ../../noahmp/src/CanopyRadiationTwoStreamMod.o \
        ../../noahmp/src/GroundAlbedoMod.o \
        ../../noahmp/src/SnowAgingBatsMod.o \
        ../../noahmp/src/SnowAlbedoBatsMod.o \
        ../../noahmp/src/SnowAlbedoClassMod.o \
        ../../noahmp/src/SurfaceAlbedoMod.o \
        ../../noahmp/src/SurfaceRadiationMod.o \
        ../../noahmp/src/HumiditySaturationMod.o \
        ../../noahmp/src/ResistanceAboveCanopyChen97Mod.o \
        ../../noahmp/src/ResistanceAboveCanopyMostMod.o \
        ../../noahmp/src/ResistanceCanopyStomataBallBerryMod.o \
        ../../noahmp/src/ResistanceCanopyStomataJarvisMod.o \
        ../../noahmp/src/ResistanceLeafToGroundMod.o \
        ../../noahmp/src/VaporPressureSaturationMod.o \
        ../../noahmp/src/SurfaceEnergyFluxVegetatedMod.o \
        ../../noahmp/src/ResistanceBareGroundChen97Mod.o \
        ../../noahmp/src/ResistanceBareGroundMostMod.o \
        ../../noahmp/src/SurfaceEnergyFluxBareGroundMod.o \
        ../../noahmp/src/SoilSnowTemperatureMainMod.o \
        ../../noahmp/src/SoilSnowTemperatureSolverMod.o \
        ../../noahmp/src/SoilSnowThermalDiffusionMod.o \
        ../../noahmp/src/SoilSnowWaterPhaseChangeMod.o \
        ../../noahmp/src/SoilWaterSupercoolKoren99Mod.o \
        ../../noahmp/src/SoilWaterSupercoolNiu06Mod.o \
        ../../noahmp/src/SnowCoverGroundNiu07Mod.o \
        ../../noahmp/src/GroundRoughnessPropertyMod.o \
        ../../noahmp/src/SurfaceEmissivityMod.o \
        ../../noahmp/src/PsychrometricVariableMod.o \
        ../../noahmp/src/ResistanceGroundEvaporationMod.o \
        ../../noahmp/src/SoilWaterTranspirationMod.o \
        ../../noahmp/src/AtmosForcingMod.o \
        ../../noahmp/src/PhenologyMainMod.o \
        ../../noahmp/src/BiochemCropMainMod.o \
        ../../noahmp/src/BiochemNatureVegMainMod.o \
        ../../noahmp/src/CarbonFluxCropMod.o \
        ../../noahmp/src/CarbonFluxNatureVegMod.o \
        ../../noahmp/src/CropGrowDegreeDayMod.o \
        ../../noahmp/src/CropPhotosynthesisMod.o \
        ../../noahmp/src/IrrigationPrepareMod.o \
        ../../noahmp/src/BalanceErrorCheckMod.o \
        ../../noahmp/src/GeneralInitMod.o \
        ../../noahmp/src/GroundWaterMmfMod.o \
        ../../noahmp/src/BalanceErrorCheckGlacierMod.o \
        ../../noahmp/src/EnergyMainGlacierMod.o \
        ../../noahmp/src/GeneralInitGlacierMod.o \
        ../../noahmp/src/GlacierIceThermalPropertyMod.o \
        ../../noahmp/src/GlacierPhaseChangeMod.o \
        ../../noahmp/src/GlacierTemperatureMainMod.o \
        ../../noahmp/src/GlacierTemperatureSolverMod.o \
        ../../noahmp/src/GlacierThermalDiffusionMod.o \
        ../../noahmp/src/GroundAlbedoGlacierMod.o \
        ../../noahmp/src/GroundRoughnessPropertyGlacierMod.o \
        ../../noahmp/src/GroundThermalPropertyGlacierMod.o \
        ../../noahmp/src/NoahmpMainGlacierMod.o \
        ../../noahmp/src/PrecipitationHeatAdvectGlacierMod.o \
        ../../noahmp/src/PsychrometricVariableGlacierMod.o \
        ../../noahmp/src/ResistanceGroundEvaporationGlacierMod.o \
        ../../noahmp/src/SnowCoverGlacierMod.o \
        ../../noahmp/src/SnowWaterMainGlacierMod.o \
        ../../noahmp/src/SnowpackHydrologyGlacierMod.o \
        ../../noahmp/src/SurfaceAlbedoGlacierMod.o \
        ../../noahmp/src/SurfaceEmissivityGlacierMod.o \
        ../../noahmp/src/SurfaceEnergyFluxGlacierMod.o \
        ../../noahmp/src/SurfaceRadiationGlacierMod.o \
        ../../noahmp/src/RunoffSurfaceWetlandMod.o \
        ../../noahmp/src/WaterWetlandMod.o \
        ../../noahmp/src/WaterMainGlacierMod.o

CMD = hrldas.exe
all:	$(CMD)

### default we create the exe based on NoahMP
hrldas.exe: $(OBJS)
	@echo ""
	echo "${TAO_FORTRAN_LIB} ${TAO_LIB} ${PETSC_LIB} ${PHDF5_INC} -Wl,-rpath,${TACC_HDF5_LIB} -L${TACC_HDF5_LIB} -lhdf5 -lz"
# We have to include the modules built in ../IO_code 
	$(COMPILERF90) -o $(@) -I../IO_code -I../../urban/wrf -I../../noahmp/src -I../../noahmp/drivers/hrldas \
        -I../../noahmp/utility -I../Utility_routines $(OBJS) $(OBJS_NoahMP) $(HYDRO_LIB) $(RAPID_LIB) $(NETCDFLIB) $(RAPID_MACRO)

	@echo ""

# Template to create the exe file based on different land model. Such as NoahMP
NoahMP: $(OBJS)
	@echo ""
	$(COMPILERF90) -o hrldas.exe  -I../IO_code -I../../urban/wrf -I../../noahmp/src -I../../noahmp/drivers/hrldas \
        -I../../noahmp/utility -I../Utility_routines $(OBJS) $(OBJS_NoahMP)  $(HYDRO_LIB) $(NETCDFLIB)
	@echo ""

# This command cleans up
clean:
	$(RM) *~ $(CMD)

