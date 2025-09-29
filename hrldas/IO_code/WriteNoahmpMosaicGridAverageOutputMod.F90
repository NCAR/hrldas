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
    call add_to_output( NoahmpIO%FVEGXY                    , "FVEG"    , "Green Vegetation Fraction"            , "-"                     )
    call add_to_output( NoahmpIO%LAI                       , "LAI"     , "Leaf area index"                      , "m2/m2"                 )
    call add_to_output( NoahmpIO%XSAIXY                    , "SAI"     , "Stem area index"                      , "m2/m2"                 )
    ! Forcing
    call add_to_output( MosaicAverage(NoahmpIO%SWDOWN)     , "SWFORC"  , "Shortwave forcing"                    , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%COSZEN)     , "COSZ"    , "Cosine of zenith angle"               , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%GLW)        , "LWFORC"  , "Longwave forcing"                     , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%RAINBL)     , "RAINRATE", "Precipitation rate"                   , "mm/timestep"           )
    ! Grid energy budget terms
    call add_to_output( MosaicAverage(NoahmpIO%EMISS)      , "EMISS"   , "Grid emissivity"                      , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%FSAXY)      , "FSA"     , "Total absorbed SW radiation"          , "W/m2"                  )         
    call add_to_output( MosaicAverage(NoahmpIO%FIRAXY)     , "FIRA"    , "Total net LW radiation to atmosphere" , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GRDFLX)     , "GRDFLX"  , "Heat flux into the soil"              , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%HFX)        , "HFX"     , "Total sensible heat to atmosphere"    , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%LH)         , "LH"      , "Total latent heat to atmosphere"      , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%ECANXY)     , "ECAN"    , "Canopy water evaporation rate"        , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%ETRANXY)    , "ETRAN"   , "Transpiration rate"                   , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%EDIRXY)     , "EDIR"    , "Direct from soil evaporation rate"    , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%ALBEDO)     , "ALBEDO"  , "Surface albedo"                       , "-"                     )
    ! Grid water budget terms - in addition to above
    call add_to_output( MosaicAverage(NoahmpIO%UDRUNOFF)   , "UGDRNOFF", "Accumulated underground runoff"       , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%SFCRUNOFF)  , "SFCRNOFF", "Accumulatetd surface runoff"          , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%CANLIQXY)   , "CANLIQ"  , "Canopy liquid water content"          , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%CANICEXY)   , "CANICE"  , "Canopy ice water content"             , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%ZWTXY)      , "ZWT"     , "Depth to water table"                 , "m"                     )
    call add_to_output( MosaicAverage(NoahmpIO%WAXY)       , "WA"      , "Water in aquifer"                     , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%WTXY)       , "WT"      , "Water in aquifer and saturated soil"  , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%QTDRAIN)    , "QTDRAIN" , "Accumulated tile drainage"            , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%PONDINGXY)  , "PONDING" , "total surface ponding per time step"            , "mm/s"        )
    ! Additional needed to close the canopy energy budget
    call add_to_output( MosaicAverage(NoahmpIO%SAVXY)      , "SAV"     , "Solar radiation absorbed by canopy"   , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%TRXY)       , "TR"      , "Transpiration heat"                   , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%EVCXY)      , "EVC"     , "Canopy evap heat"                     , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%IRCXY)      , "IRC"     , "Canopy net LW rad"                    , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%SHCXY)      , "SHC"     , "Canopy sensible heat"                 , "W/m2"                  )
    ! Additional needed to close the under canopy ground energy budget
    call add_to_output( MosaicAverage(NoahmpIO%IRGXY)      , "IRG"     , "below-canopy ground net LW rad"       , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%SHGXY)      , "SHG"     , "below-canopy ground sensible heat"    , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%EVGXY)      , "EVG"     , "below-canopy ground evap heat"        , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GHVXY)      , "GHV"     , "below-canopy ground heat to soil"     , "W/m2"                  )
    ! Needed to close the bare ground energy budget
    call add_to_output( MosaicAverage(NoahmpIO%SAGXY)      , "SAG"     , "Solar radiation absorbed by ground"   , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%IRBXY)      , "IRB"     , "Net LW rad to atm bare ground"        , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%SHBXY)      , "SHB"     , "Sensible heat to atm bare ground"     , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%EVBXY)      , "EVB"     , "Evaporation heat to atm bare ground"  , "W/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GHBXY)      , "GHB"     , "Ground heat flux to soil bare ground" , "W/m2"                  )
    ! Above-soil temperatures
    call add_to_output( MosaicAverage(NoahmpIO%TRADXY)     , "TRAD"    , "Surface radiative temperature"        , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TGXY)       , "TG"      , "Ground temperature"                   , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TVXY)       , "TV"      , "Vegetation temperature"               , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TAHXY)      , "TAH"     , "Canopy air temperature"               , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TGVXY)      , "TGV"     , "Ground surface Temp vegetated"        , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%TGBXY)      , "TGB"     , "Ground surface Temp bare"             , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%T2MVXY)     , "T2MV"    , "2m Air Temp vegetated"                , "K"                     )
    call add_to_output( MosaicAverage(NoahmpIO%T2MBXY)     , "T2MB"    , "2m Air Temp bare"                     , "K"                     )
    ! Above-soil moisture
    call add_to_output( MosaicAverage(NoahmpIO%Q2MVXY)     , "Q2MV"    , "2m mixing ratio vegetated"            , "kg/kg"                 )
    call add_to_output( MosaicAverage(NoahmpIO%Q2MBXY)     , "Q2MB"    , "2m mixing ratio bare"                 , "kg/kg"                 )
    call add_to_output( MosaicAverage(NoahmpIO%EAHXY)      , "EAH"     , "Canopy air vapor pressure"            , "Pa"                    )
    call add_to_output( MosaicAverage(NoahmpIO%FWETXY)     , "FWET"    , "Wetted fraction of canopy"            , "fraction"              )
    ! Snow and soil - 3D terms
    call add_to_output( MosaicAverage(NoahmpIO%ZSNSOXY(:,-NoahmpIO%nsnow+1:0,:,:)),"ZSNSO_SN","Snow layer depth from snow surface","m","SNOW")
    call add_to_output( MosaicAverage(NoahmpIO%SNICEXY)    , "SNICE"   , "Snow layer ice"                       , "mm"             , "SNOW")
    call add_to_output( MosaicAverage(NoahmpIO%SNLIQXY)    , "SNLIQ"   , "Snow layer liquid water"              , "mm"             , "SNOW")
    call add_to_output( MosaicAverage(NoahmpIO%TSLB)       , "SOIL_T"  , "soil temperature"                     , "K"              , "SOIL")
    call add_to_output( MosaicAverage(NoahmpIO%SMOIS)      , "SOIL_M"  , "volumetric soil moisture"             , "m3/m3"          , "SOIL")
    call add_to_output( MosaicAverage(NoahmpIO%SH2O)       , "SOIL_W"  , "liquid volumetric soil moisture"      , "m3/m3"          , "SOIL")
    call add_to_output( MosaicAverage(NoahmpIO%TSNOXY)     , "SNOW_T"  , "snow temperature"                     , "K"              , "SNOW")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSNOWDIRXY), "ALBSNOWDIR" , "Snow albedo (direct)"             , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSNOWDIFXY), "ALBSNOWDIF" , "Snow albedo (diffuse)"            , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSFCDIRXY) , "ALBSFCDIR"  , "Surface albedo (direct)"          , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSFCDIFXY) , "ALBSFCDIF"  , "Surface albedo (diffuse)"         , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSOILDIRXY), "ALBSOILDIR"  , "Soil albedo (direct)"            , "-"              , "RADN")
    call add_to_output( MosaicAverage(NoahmpIO%ALBSOILDIFXY), "ALBSOILDIF"  , "Soil albedo (diffuse)"           , "-"              , "RADN")
    ! Snow - 2D terms
    call add_to_output( MosaicAverage(NoahmpIO%SNOWH)      , "SNOWH"   , "Snow depth"                           , "m"                     )
    call add_to_output( MosaicAverage(NoahmpIO%SNOW)       , "SNEQV"   , "Snow water equivalent"                , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%QSNOWXY)    , "QSNOW"   , "Snowfall rate on the ground"          , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%QRAINXY)    , "QRAIN"   , "Rainfall rate on the ground"          , "mm/s"                  )
    call add_to_output( MosaicAverage(NoahmpIO%ISNOWXY)    , "ISNOW"   , "Number of snow layers"                , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%SNOWC)      , "FSNO"    , "Snow-cover fraction on the ground"    , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%ACSNOW)     , "ACSNOW"  , "accumulated snow fall"                , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%ACSNOM)     , "ACSNOM"  , "accumulated snow melt water"          , "mm"                    )
    call add_to_output( MosaicAverage(NoahmpIO%QSNBOTXY)   , "QSNBOT"  , "water (melt+rain through) out of snow bottom"   , "mm/s"        )
    call add_to_output( MosaicAverage(NoahmpIO%QMELTXY)    , "QMELT"   , "snow melt due to phase change"                  , "mm/s"        )
    ! SNICAR snow albedo scheme
    if (NoahmpIO%IOPT_ALB == 3)then
      call add_to_output( MosaicAverage(NoahmpIO%SNRDSXY)    , "SNRDS"   , "Snow layer effective grain radius"    , "m-6"           , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%SNFRXY)     , "SNFR"    , "Snow layer rate of freezing"          , "mm/s"          , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%BCPHIXY)    , "BCPHI_Mass","hydrophilic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%BCPHOXY)    , "BCPHO_Mass","hydrophobic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%OCPHIXY)    , "OCPHI_Mass","hydrophilic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%OCPHOXY)    , "OCPHO_Mass","hydrophobic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST1XY)    , "DUST1_Mass","dust size bin 1 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST2XY)    , "DUST2_Mass","dust size bin 2 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST3XY)    , "DUST3_Mass","dust size bin 3 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST4XY)    , "DUST4_Mass","dust size bin 4 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%DUST5XY)    , "DUST5_Mass","dust size bin 5 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcBCPHIXY), "BCPHI_MassConc","hydrophilic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcBCPHOXY), "BCPHO_MassConc","hydrophobic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcOCPHIXY), "OCPHI_MassConc","hydrophilic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcOCPHOXY), "OCPHO_MassConc","hydrophobic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST1XY), "DUST1_MassConc","dust size bin 1 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST2XY), "DUST2_MassConc","dust size bin 2 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST3XY), "DUST3_MassConc","dust size bin 3 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST4XY), "DUST4_MassConc","dust size bin 4 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output( MosaicAverage(NoahmpIO%MassConcDUST5XY), "DUST5_MassConc","dust size bin 5 mass concentration in snow", "kg/kg", "SNOW")
    endif
    ! Exchange coefficients
    call add_to_output( MosaicAverage(NoahmpIO%CMXY)       , "CM"      , "Momentum drag coefficient"            , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHXY)       , "CH"      , "Sensible heat exchange coefficient"   , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHVXY)      , "CHV"     , "Exchange coefficient vegetated"       , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHBXY)      , "CHB"     , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHLEAFXY)   , "CHLEAF"  , "Exchange coefficient leaf"            , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHUCXY)     , "CHUC"    , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHV2XY)     , "CHV2"    , "Exchange coefficient 2-m vegetated"   , "m/s"                   )
    call add_to_output( MosaicAverage(NoahmpIO%CHB2XY)     , "CHB2"    , "Exchange coefficient 2-m bare"        , "m/s"                   )
    ! Carbon allocation model
    call add_to_output( MosaicAverage(NoahmpIO%LFMASSXY)   , "LFMASS"  , "Leaf mass"                            , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%RTMASSXY)   , "RTMASS"  , "Mass of fine roots"                   , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%STMASSXY)   , "STMASS"  , "Stem mass"                            , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%WOODXY)     , "WOOD"    , "Mass of wood and woody roots"         , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GRAINXY)    , "GRAIN"   , "Mass of grain"                        , "g/m2"                  )
    call add_to_output( MosaicAverage(NoahmpIO%GDDXY)      , "GDD"     , "Growing degree days "                 , "-"                     )
    call add_to_output( MosaicAverage(NoahmpIO%STBLCPXY)   , "STBLCP"  , "Stable carbon in deep soil"           , "gC/m2"                 )
    call add_to_output( MosaicAverage(NoahmpIO%FASTCPXY)   , "FASTCP"  , "Short-lived carbon in shallow soil"   , "gC/m2"                 )
    call add_to_output( MosaicAverage(NoahmpIO%NEEXY)      , "NEE"     , "Net ecosystem exchange"               , "gCO2/m2/s"             )
    call add_to_output( MosaicAverage(NoahmpIO%GPPXY)      , "GPP"     , "Net instantaneous assimilation"       , "gC/m2/s"               )
    call add_to_output( MosaicAverage(NoahmpIO%NPPXY)      , "NPP"     , "Net primary productivity"             , "gC/m2/s"               )
    call add_to_output( MosaicAverage(NoahmpIO%PSNXY)      , "PSN"     , "Total photosynthesis"                 , "umol CO2/m2/s"         )
    call add_to_output( MosaicAverage(NoahmpIO%APARXY)     , "APAR"    , "Photosynthesis active energy by canopy", "W/m2"                 )
    ! additional NoahMP output
    if (NoahmpIO%noahmp_output > 0) then
    ! additional water budget terms
      call add_to_output( MosaicAverage(NoahmpIO%QINTSXY)    , "QINTS"   , "canopy interception (loading) rate for snowfall", "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QINTRXY)    , "QINTR"   , "canopy interception rate for rain"              , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QDRIPSXY)   , "QDRIPS"  , "drip (unloading) rate for intercepted snow"     , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QDRIPRXY)   , "QDRIPR"  , "drip rate for canopy intercepted rain"          , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QTHROSXY)   , "QTHROS"  , "throughfall of snowfall"                        , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QTHRORXY)   , "QTHROR"  , "throughfall for rain"                           , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QSNSUBXY)   , "QSNSUB"  , "snow surface sublimation rate"                  , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QSNFROXY)   , "QSNFRO"  , "snow surface frost rate"                        , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QSUBCXY)    , "QSUBC"   , "canopy snow sublimation rate"                   , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QFROCXY)    , "QFROC"   , "canopy snow frost rate"                         , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QEVACXY)    , "QEVAC"   , "canopy snow evaporation rate"                   , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QDEWCXY)    , "QDEWC"   , "canopy snow dew rate"                           , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QFRZCXY)    , "QFRZC"   , "refreezing rate of canopy liquid water"         , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%QMELTCXY)   , "QMELTC"  , "melting rate of canopy snow"                    , "mm/s"        )
      call add_to_output( MosaicAverage(NoahmpIO%FPICEXY)    , "FPICE"   , "snow fraction in precipitation"                 , "-"           )
      call add_to_output( MosaicAverage(NoahmpIO%ACC_QINSURXY),"ACC_QINSUR", "accumuated water flux to soil within soil timestep"     , "m/s*dt_soil/dt_main")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_QSEVAXY) ,"ACC_QSEVA" , "accumulated soil surface evap rate within soil timestep", "m/s*dt_soil/dt_main")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_ETRANIXY),"ACC_ETRANI", "accumualted transpiration rate within soil timestep"    , "m/s*dt_soil/dt_main","SOIL")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_DWATERXY),"ACC_DWATER", "accumulated water storage change within soil timestep"  , "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_PRCPXY)  ,"ACC_PRCP"  , "accumulated precipitation within soil timestep"         , "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_ECANXY) ,"ACC_ECAN"  , "accumulated net canopy evaporation within soil timestep", "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_ETRANXY) ,"ACC_ETRAN" , "accumulated transpiration within soil timestep"         , "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_EDIRXY)  ,"ACC_EDIR"  , "accumulated net ground evaporation within soil timestep", "mm")
      call add_to_output( MosaicAverage(NoahmpIO%ACC_GLAFLWXY),"ACC_GLAFLW", "accumuated glacier excessive flow per soil timestep"    , "mm")
      ! additional energy terms
      call add_to_output( MosaicAverage(NoahmpIO%PAHXY)      , "PAH"     , "Precipitation advected heat flux"                         , "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%PAHGXY)    , "PAHG"    , "Precipitation advected heat flux to below-canopy ground"  , "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%PAHBXY)     , "PAHB"    , "Precipitation advected heat flux to bare ground"          , "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%PAHVXY)     , "PAHV"    , "Precipitation advected heat flux to canopy"               , "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%ACC_SSOILXY), "ACC_SSOIL","accumulated heat flux into snow/soil within soil timestep", "W/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%EFLXBXY)    , "EFLXB"   , "accumulated heat flux through soil bottom"                , "J/m2"    )
      call add_to_output( MosaicAverage(NoahmpIO%SOILENERGY) , "SOILENERGY","energy content in soil relative to 273.16"               , "KJ/m2"   )
      call add_to_output( MosaicAverage(NoahmpIO%SNOWENERGY) , "SNOWENERGY","energy content in snow relative to 273.16"               , "KJ/m2"   )
      call add_to_output( MosaicAverage(NoahmpIO%CANHSXY)    , "CANHS"   , "canopy heat storage change"                               , "W/m2"    )
      ! additional forcing terms
      call add_to_output( MosaicAverage(NoahmpIO%RAINLSM)    , "RAINLSM" , "lowest model liquid precipitation into LSM"               , "mm/s"    )
      call add_to_output( MosaicAverage(NoahmpIO%SNOWLSM)    , "SNOWLSM" , "lowest model snowfall into LSM"                           , "mm/s"    )
      call add_to_output( MosaicAverage(NoahmpIO%FORCTLSM)   , "FORCTLSM", "lowest model temperature into LSM"                        , "K"       ) 
      call add_to_output( MosaicAverage(NoahmpIO%FORCQLSM)   , "FORCQLSM", "lowest model specific humidty into LSM"                   , "kg/kg"   )
      call add_to_output( MosaicAverage(NoahmpIO%FORCPLSM)   , "FORCPLSM", "lowest model pressure into LSM"                           , "Pa"      )
      call add_to_output( MosaicAverage(NoahmpIO%FORCZLSM)   , "FORCZLSM", "lowest model forcing height into LSM"                     , "m"       )
      call add_to_output( MosaicAverage(NoahmpIO%FORCWLSM)   , "FORCWLSM", "lowest model wind speed into LSM"                         , "m/s"     )
      call add_to_output( MosaicAverage(NoahmpIO%RadSwVisFrac) , "SWVISFRAC", "Fraction of visible band downward solar radiation", "-"      )
      call add_to_output( MosaicAverage(NoahmpIO%RadSwDirFrac) , "SWDIRFRAC", "Fraction of downward solar direct radiation",   "-"          )
    endif

    ! Irrigation
    if ( NoahmpIO%IOPT_IRR > 0 ) then
      call add_to_output( MosaicAverage(NoahmpIO%IRNUMSI)    , "IRNUMSI" , "Sprinkler irrigation count"                               , "-"       )
      call add_to_output( MosaicAverage(NoahmpIO%IRNUMMI)    , "IRNUMMI" , "Micro irrigation count"                                   , "-"       )
      call add_to_output( MosaicAverage(NoahmpIO%IRNUMFI)    , "IRNUMFI" , "Flood irrigation count"                                   , "-"       )
      call add_to_output( MosaicAverage(NoahmpIO%IRELOSS)    , "IRELOSS" , "Accumulated sprinkler Evaporation"                        , "mm"      )
      call add_to_output( MosaicAverage(NoahmpIO%IRSIVOL)    , "IRSIVOL" , "Sprinkler irrigation amount"                              , "mm"      )
      call add_to_output( MosaicAverage(NoahmpIO%IRMIVOL)    , "IRMIVOL" , "Micro irrigation amount"                                  , "mm"      )
      call add_to_output( MosaicAverage(NoahmpIO%IRFIVOL)    , "IRFIVOL" , "Flood irrigation amount"                                  , "mm"      )
      call add_to_output( MosaicAverage(NoahmpIO%IRRSPLH)    , "IRRSPLH" , "Accumulated latent heating due to sprinkler"              , "J/m2"    )
    endif
    ! MMF groundwater  model
    if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
      call add_to_output( MosaicAverage(NoahmpIO%SMCWTDXY)   , "SMCWTD"   , "soil water content between bottom of the soil and water table", "m3/m3"  )
      call add_to_output( MosaicAverage(NoahmpIO%RECHXY)     , "RECH"     , "recharge to or from the water table when shallow"             , "m"      )
      call add_to_output( MosaicAverage(NoahmpIO%DEEPRECHXY) , "DEEPRECH" , "recharge to or from the water table when deep"                , "m"      )
      call add_to_output( MosaicAverage(NoahmpIO%QRFSXY)     , "QRFS"     , "accumulated groundwater baselow"                              , "mm"     )
      call add_to_output( MosaicAverage(NoahmpIO%QRFXY)      , "QRF"      , "groundwater baseflow"                                         , "m"      )
      call add_to_output( MosaicAverage(NoahmpIO%QSPRINGSXY) , "QSPRINGS" , "accumulated seeping water"                                    , "mm"     )
      call add_to_output( MosaicAverage(NoahmpIO%QSPRINGXY)  , "QSPRING"  , "instantaneous seeping water"                                  , "m"      )
      call add_to_output( MosaicAverage(NoahmpIO%QSLATXY)    , "QSLAT"    , "accumulated lateral flow"                                     , "mm"     )
      call add_to_output( MosaicAverage(NoahmpIO%QLATXY)     , "QLAT"     , "instantaneous lateral flow"                                   , "m"      )
    endif
    ! Wetland model
    if ( NoahmpIO%IOPT_WETLAND > 0 ) then
      call add_to_output( MosaicAverage(NoahmpIO%FSATXY)     , "FSAT"    , "saturated fraction of the grid"                           , "-" )
      call add_to_output( MosaicAverage(NoahmpIO%WSURFXY)    , "WSURF"   , "Wetland Water Storage"                                    , "mm")
    endif

  end subroutine WriteNoahmpMosaicGridAverageOutput

end module WriteNoahmpMosaicGridAverageOutputMod