module WriteNoahmpMosaicGridAverageOutputMod

!!! This module part of NoahMP Mosaic/Subgrid Tiling Scheme
!!! Purpose: To write grid average values of Noah-MP Mosaic. 

! ------------------------ Code history -----------------------------------
! Original code : Prasanth Valayamkunnath (IISER Thiruvananthapuram)
! Date          : September 12, 2025
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use module_hrldas_netcdf_io
  use MosaicAverageMod

  implicit none

contains

!=== sort landuse/soiltype/hydrotype index based on area fraction and identify most dominant types

  subroutine WriteNoahmpMosaicGridAverageOutput (NoahmpIO)
 
    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO


    ! For 3D arrays, we need to know whether the Z dimension is snow layers, or soil layers.

    ! Properties - Assigned or predicted
    call add_to_output( NoahmpIO%IVGTYP                    , "IVGTYP"  , "Dominant vegetation category"         , "category"              )
    call add_to_output( NoahmpIO%ISLTYP                    , "ISLTYP"  , "Dominant soil category"               , "category"              )
    call add_to_output( MosaicAverage(NoahmpIO%FVEGXY, NoahmpIO)     , "FVEG"    , "Green Vegetation Fraction"            , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%LAI, NoahmpIO)        , "LAI"     , "Leaf area index"                      , "m2/m2"                 )
    call add_to_output( MosaicAverage(NoahmpIO%XSAIXY, NoahmpIO)     , "SAI"     , "Stem area index"                      , "m2/m2"                 )
    ! Forcing
    call add_to_output( NoahmpIO%SWDOWN     , "SWFORC"  , "Shortwave forcing"                    , "W/m2"                  )
    call add_to_output( NoahmpIO%COSZEN     , "COSZ"    , "Cosine of zenith angle"               , "-"                     )
    call add_to_output( NoahmpIO%GLW        , "LWFORC"  , "Longwave forcing"                     , "W/m2"                  )
    call add_to_output( NoahmpIO%RAINBL     , "RAINRATE", "Precipitation rate"                   , "mm/timestep"           )
    ! Grid energy budget terms
    call add_to_output( MosaicAverage(NoahmpIO%EMISS, NoahmpIO)      , "EMISS"   , "Grid emissivity"                      , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%FSAXY, NoahmpIO)      , "FSA"     , "Total absorbed SW radiation"          , "W/m2"                  )         
    call add_to_output( MosaicAverage(NoahmpIO%FIRAXY, NoahmpIO)     , "FIRA"    , "Total net LW radiation to atmosphere" , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GRDFLX, NoahmpIO)     , "GRDFLX"  , "Heat flux into the soil"              , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%HFX, NoahmpIO)        , "HFX"     , "Total sensible heat to atmosphere"    , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%LH, NoahmpIO)         , "LH"      , "Total latent heat to atmosphere"      , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%ECANXY, NoahmpIO)     , "ECAN"    , "Canopy water evaporation rate"        , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%ETRANXY, NoahmpIO)    , "ETRAN"   , "Transpiration rate"                   , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%EDIRXY, NoahmpIO)     , "EDIR"    , "Direct from soil evaporation rate"    , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%ALBEDO, NoahmpIO)     , "ALBEDO"  , "Surface albedo"                       , "-"                     )
    ! Grid water budget terms - in addition to above
    call add_to_output( MosaicAverage(NoahmpIO%UDRUNOFF, NoahmpIO)   , "UGDRNOFF", "Accumulated underground runoff"       , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%SFCRUNOFF, NoahmpIO)  , "SFCRNOFF", "Accumulatetd surface runoff"          , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%CANLIQXY, NoahmpIO)   , "CANLIQ"  , "Canopy liquid water content"          , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%CANICEXY, NoahmpIO)   , "CANICE"  , "Canopy ice water content"             , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%ZWTXY, NoahmpIO)      , "ZWT"     , "Depth to water table"                 , "m"                     )
    call add_to_output( MosaicAverage(NoahmpIO%WAXY, NoahmpIO)       , "WA"      , "Water in aquifer"                     , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%WTXY, NoahmpIO)       , "WT"      , "Water in aquifer and saturated soil"  , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%QTDRAIN, NoahmpIO)    , "QTDRAIN" , "Accumulated tile drainage"            , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%PONDINGXY, NoahmpIO)  , "PONDING" , "total surface ponding per time step"            , "mm/s"        )
    ! Additional needed to close the canopy energy budget
    call add_to_output( MosaicAverage(NoahmpIO%SAVXY, NoahmpIO)      , "SAV"     , "Solar radiation absorbed by canopy"   , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%TRXY, NoahmpIO)       , "TR"      , "Transpiration heat"                   , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%EVCXY, NoahmpIO)      , "EVC"     , "Canopy evap heat"                     , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%IRCXY, NoahmpIO)      , "IRC"     , "Canopy net LW rad"                    , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%SHCXY, NoahmpIO)      , "SHC"     , "Canopy sensible heat"                 , "W/m2"                  )
    ! Additional needed to close the under canopy ground energy budget
    call add_to_output( MosaicAverage(NoahmpIO%IRGXY, NoahmpIO)      , "IRG"     , "below-canopy ground net LW rad"       , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%SHGXY, NoahmpIO)      , "SHG"     , "below-canopy ground sensible heat"    , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%EVGXY, NoahmpIO)      , "EVG"     , "below-canopy ground evap heat"        , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GHVXY, NoahmpIO)      , "GHV"     , "below-canopy ground heat to soil"     , "W/m2"                  )
    ! Needed to close the bare ground energy budget
    call add_to_output( MosaicAverage(NoahmpIO%SAGXY, NoahmpIO)      , "SAG"     , "Solar radiation absorbed by ground"   , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%IRBXY, NoahmpIO)      , "IRB"     , "Net LW rad to atm bare ground"        , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%SHBXY, NoahmpIO)      , "SHB"     , "Sensible heat to atm bare ground"     , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%EVBXY, NoahmpIO)      , "EVB"     , "Evaporation heat to atm bare ground"  , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GHBXY, NoahmpIO)      , "GHB"     , "Ground heat flux to soil bare ground" , "W/m2"                  )
    ! Above-soil temperatures
    call add_to_output( MosaicAverage(NoahmpIO%TRADXY, NoahmpIO)     , "TRAD"    , "Surface radiative temperature"        , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TGXY, NoahmpIO)       , "TG"      , "Ground temperature"                   , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TVXY, NoahmpIO)       , "TV"      , "Vegetation temperature"               , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TAHXY, NoahmpIO)      , "TAH"     , "Canopy air temperature"               , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TGVXY, NoahmpIO)      , "TGV"     , "Ground surface Temp vegetated"        , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TGBXY, NoahmpIO)      , "TGB"     , "Ground surface Temp bare"             , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%T2MVXY, NoahmpIO)     , "T2MV"    , "2m Air Temp vegetated"                , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%T2MBXY, NoahmpIO)     , "T2MB"    , "2m Air Temp bare"                     , "K"                     )
    ! Above-soil moisture
    call add_to_output( MosaicAverage(NoahmpIO%Q2MVXY, NoahmpIO)     , "Q2MV"    , "2m mixing ratio vegetated"            , "kg/kg"                 )
    call add_to_output( MosaicAverage(NoahmpIO%Q2MBXY, NoahmpIO)     , "Q2MB"    , "2m mixing ratio bare"                 , "kg/kg"                 )
    call add_to_output( MosaicAverage(NoahmpIO%EAHXY, NoahmpIO)      , "EAH"     , "Canopy air vapor pressure"            , "Pa"                    )
    call add_to_output( MosaicAverage(NoahmpIO%FWETXY, NoahmpIO)     , "FWET"    , "Wetted fraction of canopy"            , "fraction"              )
    ! Snow and soil - 3D terms
    call add_to_output( MosaicAverage(NoahmpIO%ZSNSOXY(:,-NoahmpIO%nsnow+1:0,:,:), NoahmpIO),"ZSNSO_SN","Snow layer depth from snow surface","m","SNOW")
    call add_to_output( MosaicAverage(NoahmpIO%SNICEXY, NoahmpIO)    , "SNICE"   , "Snow layer ice"                       , "mm"             , "SNOW")
    call add_to_output( MosaicAverage(NoahmpIO%SNLIQXY, NoahmpIO)    , "SNLIQ"   , "Snow layer liquid water"              , "mm"             , "SNOW")
    call add_to_output( MosaicAverage(NoahmpIO%TSLB, NoahmpIO)       , "SOIL_T"  , "soil temperature"                     , "K"              , "SOIL")
    call add_to_output( MosaicAverage(NoahmpIO%SMOIS, NoahmpIO)      , "SOIL_M"  , "volumetric soil moisture"             , "m3/m3"          , "SOIL")
    call add_to_output( MosaicAverage(NoahmpIO%SH2O, NoahmpIO)       , "SOIL_W"  , "liquid volumetric soil moisture"      , "m3/m3"          , "SOIL")
    call add_to_output( MosaicAverage(NoahmpIO%TSNOXY, NoahmpIO)     , "SNOW_T"  , "snow temperature"                     , "K"              , "SNOW")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSNOWDIRXY, NoahmpIO), "ALBSNOWDIR" , "Snow albedo (direct)"             , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSNOWDIFXY, NoahmpIO), "ALBSNOWDIF" , "Snow albedo (diffuse)"            , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSFCDIRXY, NoahmpIO) , "ALBSFCDIR"  , "Surface albedo (direct)"          , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSFCDIFXY, NoahmpIO) , "ALBSFCDIF"  , "Surface albedo (diffuse)"         , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSOILDIRXY, NoahmpIO), "ALBSOILDIR"  , "Soil albedo (direct)"            , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSOILDIFXY, NoahmpIO), "ALBSOILDIF"  , "Soil albedo (diffuse)"           , "-"              , "RADN")
    ! Snow - 2D terms
    call add_to_output( MosaicAverage(NoahmpIO%SNOWH, NoahmpIO)      , "SNOWH"   , "Snow depth"                           , "m"                     )
    call add_to_output( MosaicAverage(NoahmpIO%SNOW, NoahmpIO)       , "SNEQV"   , "Snow water equivalent"                , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%QSNOWXY, NoahmpIO)    , "QSNOW"   , "Snowfall rate on the ground"          , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%QRAINXY, NoahmpIO)    , "QRAIN"   , "Rainfall rate on the ground"          , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%ISNOWXY, NoahmpIO)    , "ISNOW"   , "Number of snow layers"                , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%SNOWC, NoahmpIO)      , "FSNO"    , "Snow-cover fraction on the ground"    , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%ACSNOW, NoahmpIO)     , "ACSNOW"  , "accumulated snow fall"                , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%ACSNOM, NoahmpIO)     , "ACSNOM"  , "accumulated snow melt water"          , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%QSNBOTXY, NoahmpIO)   , "QSNBOT"  , "water (melt+rain through) out of snow bottom"   , "mm/s"        )
    call add_to_output( MosaicAverage(NoahmpIO%QMELTXY, NoahmpIO)    , "QMELT"   , "snow melt due to phase change"                  , "mm/s"        )
    ! SNICAR snow albedo scheme
    if (NoahmpIO%IOPT_ALB == 3)then
      call add_to_output( MosaicAverage(NoahmpIO%SNRDSXY, NoahmpIO)    , "SNRDS"   , "Snow layer effective grain radius"    , "m-6"           , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%SNFRXY, NoahmpIO)     , "SNFR"    , "Snow layer rate of freezing"          , "mm/s"          , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%BCPHIXY, NoahmpIO)    , "BCPHI_Mass","hydrophilic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%BCPHOXY, NoahmpIO)    , "BCPHO_Mass","hydrophobic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%OCPHIXY, NoahmpIO)    , "OCPHI_Mass","hydrophilic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%OCPHOXY, NoahmpIO)    , "OCPHO_Mass","hydrophobic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST1XY, NoahmpIO)    , "DUST1_Mass","dust size bin 1 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST2XY, NoahmpIO)    , "DUST2_Mass","dust size bin 2 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST3XY, NoahmpIO)    , "DUST3_Mass","dust size bin 3 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST4XY, NoahmpIO)    , "DUST4_Mass","dust size bin 4 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST5XY, NoahmpIO)    , "DUST5_Mass","dust size bin 5 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcBCPHIXY, NoahmpIO), "BCPHI_MassConc","hydrophilic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcBCPHOXY, NoahmpIO), "BCPHO_MassConc","hydrophobic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcOCPHIXY, NoahmpIO), "OCPHI_MassConc","hydrophilic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcOCPHOXY, NoahmpIO), "OCPHO_MassConc","hydrophobic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST1XY, NoahmpIO), "DUST1_MassConc","dust size bin 1 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST2XY, NoahmpIO), "DUST2_MassConc","dust size bin 2 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST3XY, NoahmpIO), "DUST3_MassConc","dust size bin 3 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST4XY, NoahmpIO), "DUST4_MassConc","dust size bin 4 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST5XY, NoahmpIO), "DUST5_MassConc","dust size bin 5 mass concentration in snow", "kg/kg", "SNOW")
    endif
    ! Exchange coefficients
    call add_to_output( MosaicAverage(NoahmpIO%CMXY, NoahmpIO)       , "CM"      , "Momentum drag coefficient"            , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHXY, NoahmpIO)       , "CH"      , "Sensible heat exchange coefficient"   , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHVXY, NoahmpIO)      , "CHV"     , "Exchange coefficient vegetated"       , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHBXY, NoahmpIO)      , "CHB"     , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHLEAFXY, NoahmpIO)   , "CHLEAF"  , "Exchange coefficient leaf"            , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHUCXY, NoahmpIO)     , "CHUC"    , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHV2XY, NoahmpIO)     , "CHV2"    , "Exchange coefficient 2-m vegetated"   , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHB2XY, NoahmpIO)     , "CHB2"    , "Exchange coefficient 2-m bare"        , "m/s"                   )
    ! Carbon allocation model
    call add_to_output( MosaicAverage(NoahmpIO%LFMASSXY, NoahmpIO)   , "LFMASS"  , "Leaf mass"                            , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%RTMASSXY, NoahmpIO)   , "RTMASS"  , "Mass of fine roots"                   , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%STMASSXY, NoahmpIO)   , "STMASS"  , "Stem mass"                            , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%WOODXY, NoahmpIO)     , "WOOD"    , "Mass of wood and woody roots"         , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GRAINXY, NoahmpIO)    , "GRAIN"   , "Mass of grain"                        , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GDDXY, NoahmpIO)      , "GDD"     , "Growing degree days "                 , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%STBLCPXY, NoahmpIO)   , "STBLCP"  , "Stable carbon in deep soil"           , "gC/m2"                 )
    call add_to_output( MosaicAverage(NoahmpIO%FASTCPXY, NoahmpIO)   , "FASTCP"  , "Short-lived carbon in shallow soil"   , "gC/m2"                 )
    call add_to_output( MosaicAverage(NoahmpIO%NEEXY, NoahmpIO)      , "NEE"     , "Net ecosystem exchange"               , "gCO2/m2/s"             )
    call add_to_output( MosaicAverage(NoahmpIO%GPPXY, NoahmpIO)      , "GPP"     , "Net instantaneous assimilation"       , "gC/m2/s"               )
    call add_to_output( MosaicAverage(NoahmpIO%NPPXY, NoahmpIO)      , "NPP"     , "Net primary productivity"             , "gC/m2/s"               )
    call add_to_output( MosaicAverage(NoahmpIO%PSNXY, NoahmpIO)      , "PSN"     , "Total photosynthesis"                 , "umol CO2/m2/s"         )
    call add_to_output( MosaicAverage(NoahmpIO%APARXY, NoahmpIO)     , "APAR"    , "Photosynthesis active energy by canopy", "W/m2"                 )
    ! additional NoahMP output
    if (NoahmpIO%noahmp_output > 0) then
    ! additional water budget terms
      call add_to_output( MosaicAverage(NoahmpIO%QINTSXY, NoahmpIO)    , "QINTS"   , "canopy interception (loading) rate for snowfall", "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QINTRXY, NoahmpIO)    , "QINTR"   , "canopy interception rate for rain"              , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QDRIPSXY, NoahmpIO)   , "QDRIPS"  , "drip (unloading) rate for intercepted snow"     , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QDRIPRXY, NoahmpIO)   , "QDRIPR"  , "drip rate for canopy intercepted rain"          , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QTHROSXY, NoahmpIO)   , "QTHROS"  , "throughfall of snowfall"                        , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QTHRORXY, NoahmpIO)   , "QTHROR"  , "throughfall for rain"                           , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QSNSUBXY, NoahmpIO)   , "QSNSUB"  , "snow surface sublimation rate"                  , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QSNFROXY, NoahmpIO)   , "QSNFRO"  , "snow surface frost rate"                        , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QSUBCXY, NoahmpIO)    , "QSUBC"   , "canopy snow sublimation rate"                   , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QFROCXY, NoahmpIO)    , "QFROC"   , "canopy snow frost rate"                         , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QEVACXY, NoahmpIO)    , "QEVAC"   , "canopy snow evaporation rate"                   , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QDEWCXY, NoahmpIO)    , "QDEWC"   , "canopy snow dew rate"                           , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QFRZCXY, NoahmpIO)    , "QFRZC"   , "refreezing rate of canopy liquid water"         , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QMELTCXY, NoahmpIO)   , "QMELTC"  , "melting rate of canopy snow"                    , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%FPICEXY, NoahmpIO)    , "FPICE"   , "snow fraction in precipitation"                 , "-"           )
      call add_to_output( MosaicAverage(NoahmpIO%ACC_QINSURXY, NoahmpIO),"ACC_QINSUR", "accumuated water flux to soil within soil timestep"     , "m/s*dt_soil/dt_main")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_QSEVAXY, NoahmpIO) ,"ACC_QSEVA" , "accumulated soil surface evap rate within soil timestep", "m/s*dt_soil/dt_main")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_ETRANIXY, NoahmpIO),"ACC_ETRANI", "accumualted transpiration rate within soil timestep"    , "m/s*dt_soil/dt_main","SOIL")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_DWATERXY, NoahmpIO),"ACC_DWATER", "accumulated water storage change within soil timestep"  , "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_PRCPXY, NoahmpIO)  ,"ACC_PRCP"  , "accumulated precipitation within soil timestep"         , "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_ECANXY, NoahmpIO) ,"ACC_ECAN"  , "accumulated net canopy evaporation within soil timestep", "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_ETRANXY, NoahmpIO) ,"ACC_ETRAN" , "accumulated transpiration within soil timestep"         , "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_EDIRXY, NoahmpIO)  ,"ACC_EDIR"  , "accumulated net ground evaporation within soil timestep", "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_GLAFLWXY, NoahmpIO),"ACC_GLAFLW", "accumuated glacier excessive flow per soil timestep"    , "mm")
      ! additional energy terms
      call add_to_output( MosaicAverage(NoahmpIO%PAHXY, NoahmpIO)      , "PAH"     , "Precipitation advected heat flux"                         , "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%PAHGXY, NoahmpIO)    , "PAHG"    , "Precipitation advected heat flux to below-canopy ground"  , "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%PAHBXY, NoahmpIO)     , "PAHB"    , "Precipitation advected heat flux to bare ground"          , "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%PAHVXY, NoahmpIO)     , "PAHV"    , "Precipitation advected heat flux to canopy"               , "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%ACC_SSOILXY, NoahmpIO), "ACC_SSOIL","accumulated heat flux into snow/soil within soil timestep", "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%EFLXBXY, NoahmpIO)    , "EFLXB"   , "accumulated heat flux through soil bottom"                , "J/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%SOILENERGY, NoahmpIO) , "SOILENERGY","energy content in soil relative to 273.16"               , "KJ/m2"   )
      call add_to_output( MosaicAverage(NoahmpIO%SNOWENERGY, NoahmpIO) , "SNOWENERGY","energy content in snow relative to 273.16"               , "KJ/m2"   )
      call add_to_output( MosaicAverage(NoahmpIO%CANHSXY, NoahmpIO)    , "CANHS"   , "canopy heat storage change"                               , "W/m2"    )
      ! additional forcing terms
      call add_to_output( MosaicAverage(NoahmpIO%RAINLSM, NoahmpIO)    , "RAINLSM" , "lowest model liquid precipitation into LSM"               , "mm/s"    )
      call add_to_output( MosaicAverage(NoahmpIO%SNOWLSM, NoahmpIO)    , "SNOWLSM" , "lowest model snowfall into LSM"                           , "mm/s"    )
      call add_to_output( MosaicAverage(NoahmpIO%FORCTLSM, NoahmpIO)   , "FORCTLSM", "lowest model temperature into LSM"                        , "K"       ) 
      call add_to_output( MosaicAverage(NoahmpIO%FORCQLSM, NoahmpIO)   , "FORCQLSM", "lowest model specific humidty into LSM"                   , "kg/kg"   )
      call add_to_output( MosaicAverage(NoahmpIO%FORCPLSM, NoahmpIO)   , "FORCPLSM", "lowest model pressure into LSM"                           , "Pa"      )
      call add_to_output( MosaicAverage(NoahmpIO%FORCZLSM, NoahmpIO)   , "FORCZLSM", "lowest model forcing height into LSM"                     , "m"       )
      call add_to_output( MosaicAverage(NoahmpIO%FORCWLSM, NoahmpIO)   , "FORCWLSM", "lowest model wind speed into LSM"                         , "m/s"     )
      call add_to_output( MosaicAverage(NoahmpIO%RadSwVisFrac, NoahmpIO) , "SWVISFRAC", "Fraction of visible band downward solar radiation", "-"      )
      call add_to_output( MosaicAverage(NoahmpIO%RadSwDirFrac, NoahmpIO) , "SWDIRFRAC", "Fraction of downward solar direct radiation",   "-"          )
    endif

    ! Irrigation
    if ( NoahmpIO%IOPT_IRR > 0 ) then
      call add_to_output( MosaicAverage(NoahmpIO%IRNUMSI, NoahmpIO)    , "IRNUMSI" , "Sprinkler irrigation count"                               , "-"       )
      call add_to_output( MosaicAverage(NoahmpIO%IRNUMMI, NoahmpIO)    , "IRNUMMI" , "Micro irrigation count"                                   , "-"       )
      call add_to_output( MosaicAverage(NoahmpIO%IRNUMFI, NoahmpIO)    , "IRNUMFI" , "Flood irrigation count"                                   , "-"       )
      call add_to_output( MosaicAverage(NoahmpIO%IRELOSS, NoahmpIO)    , "IRELOSS" , "Accumulated sprinkler Evaporation"                        , "mm"      )
      call add_to_output( MosaicAverage(NoahmpIO%IRSIVOL, NoahmpIO)    , "IRSIVOL" , "Sprinkler irrigation amount"                              , "mm"      )
      call add_to_output( MosaicAverage(NoahmpIO%IRMIVOL, NoahmpIO)    , "IRMIVOL" , "Micro irrigation amount"                                  , "mm"      )
      call add_to_output( MosaicAverage(NoahmpIO%IRFIVOL, NoahmpIO)    , "IRFIVOL" , "Flood irrigation amount"                                  , "mm"      )
      call add_to_output( MosaicAverage(NoahmpIO%IRRSPLH, NoahmpIO)    , "IRRSPLH" , "Accumulated latent heating due to sprinkler"              , "J/m2"    )
    endif
    ! MMF groundwater  model
    if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
      call add_to_output( MosaicAverage(NoahmpIO%SMCWTDXY, NoahmpIO)   , "SMCWTD"   , "soil water content between bottom of the soil and water table", "m3/m3"  )
      call add_to_output( MosaicAverage(NoahmpIO%RECHXY, NoahmpIO)     , "RECH"     , "recharge to or from the water table when shallow"             , "m"      )
      call add_to_output( MosaicAverage(NoahmpIO%DEEPRECHXY, NoahmpIO) , "DEEPRECH" , "recharge to or from the water table when deep"                , "m"      )
      call add_to_output( NoahmpIO%QRFSXY                    , "QRFS"     , "accumulated groundwater baselow"                              , "mm"     )
      call add_to_output( NoahmpIO%QRFXY                     , "QRF"      , "groundwater baseflow"                                         , "m"      )
      call add_to_output( NoahmpIO%QSPRINGSXY                , "QSPRINGS" , "accumulated seeping water"                                    , "mm"     )
      call add_to_output( NoahmpIO%QSPRINGXY                 , "QSPRING"  , "instantaneous seeping water"                                  , "m"      )
      call add_to_output( NoahmpIO%QSLATXY                   , "QSLAT"    , "accumulated lateral flow"                                     , "mm"     )
      call add_to_output( NoahmpIO%QLATXY                    , "QLAT"     , "instantaneous lateral flow"                                   , "m"      )
    endif
    ! Wetland model
    if ( NoahmpIO%IOPT_WETLAND > 0 ) then
      call add_to_output( MosaicAverage(NoahmpIO%FSATXY, NoahmpIO)     , "FSAT"    , "saturated fraction of the grid"                           , "-" )
      call add_to_output( MosaicAverage(NoahmpIO%WSURFXY, NoahmpIO)    , "WSURF"   , "Wetland Water Storage"                                    , "mm")
    endif

  end subroutine WriteNoahmpMosaicGridAverageOutput

end module WriteNoahmpMosaicGridAverageOutputMod