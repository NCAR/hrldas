module WriteNoahmpMosaicSubgridOutputMod

!!! This module part of NoahMP Mosaic/Subgrid Tiling Scheme
!!! Purpose: To write grid average values of Noah-MP Mosaic. 

! ------------------------ Code history -----------------------------------
! Original code : Prasanth Valayamkunnath (IISER Thiruvananthapuram)
! Date          : September 12, 2025
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use module_hrldas_netcdf_io

  implicit none

contains

!=== sort landuse/soiltype/hydrotype index based on area fraction and identify most dominant types

  subroutine WriteNoahmpMosaicSubgridOutput (NoahmpIO)
 
    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO

    ! For 3D arrays, we need to know whether the Z dimension is snow layers, or soil layers.

    ! Properties - Assigned or predicted
    call add_to_output_mosaic(NoahmpIO%IVGTYP     , "IVGTYP"  , "Dominant vegetation category"         , "category"              )
    call add_to_output_mosaic(NoahmpIO%ISLTYP     , "ISLTYP"  , "Dominant soil category"               , "category"              )
    call add_to_output_mosaic(NoahmpIO%FVEGXY     , "FVEG"    , "Green Vegetation Fraction"            , "-"                     )
    call add_to_output_mosaic(NoahmpIO%LAI        , "LAI"     , "Leaf area index"                      , "m2/m2"                 )
    call add_to_output_mosaic(NoahmpIO%XSAIXY     , "SAI"     , "Stem area index"                      , "m2/m2"                 )
    ! Forcing
    call add_to_output_mosaic(NoahmpIO%SWDOWN     , "SWFORC"  , "Shortwave forcing"                    , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%COSZEN     , "COSZ"    , "Cosine of zenith angle"               , "-"                     )
    call add_to_output_mosaic(NoahmpIO%GLW        , "LWFORC"  , "Longwave forcing"                     , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%RAINBL     , "RAINRATE", "Precipitation rate"                   , "mm/timestep"           )
    ! Grid energy budget terms
    call add_to_output_mosaic(NoahmpIO%EMISS      , "EMISS"   , "Grid emissivity"                      , "-"                     )
    call add_to_output_mosaic(NoahmpIO%FSAXY      , "FSA"     , "Total absorbed SW radiation"          , "W/m2"                  )         
    call add_to_output_mosaic(NoahmpIO%FIRAXY     , "FIRA"    , "Total net LW radiation to atmosphere" , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GRDFLX     , "GRDFLX"  , "Heat flux into the soil"              , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%HFX        , "HFX"     , "Total sensible heat to atmosphere"    , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%LH         , "LH"      , "Total latent heat to atmosphere"      , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%ECANXY     , "ECAN"    , "Canopy water evaporation rate"        , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%ETRANXY    , "ETRAN"   , "Transpiration rate"                   , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%EDIRXY     , "EDIR"    , "Direct from soil evaporation rate"    , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%ALBEDO     , "ALBEDO"  , "Surface albedo"                       , "-"                     )
    ! Grid water budget terms - in addition to above
    call add_to_output_mosaic(NoahmpIO%UDRUNOFF   , "UGDRNOFF", "Accumulated underground runoff"       , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%SFCRUNOFF  , "SFCRNOFF", "Accumulatetd surface runoff"          , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%CANLIQXY   , "CANLIQ"  , "Canopy liquid water content"          , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%CANICEXY   , "CANICE"  , "Canopy ice water content"             , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%ZWTXY      , "ZWT"     , "Depth to water table"                 , "m"                     )
    call add_to_output_mosaic(NoahmpIO%WAXY       , "WA"      , "Water in aquifer"                     , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%WTXY       , "WT"      , "Water in aquifer and saturated soil"  , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%QTDRAIN    , "QTDRAIN" , "Accumulated tile drainage"            , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%PONDINGXY  , "PONDING" , "total surface ponding per time step"            , "mm/s"        )
    ! Additional needed to close the canopy energy budget
    call add_to_output_mosaic(NoahmpIO%SAVXY      , "SAV"     , "Solar radiation absorbed by canopy"   , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%TRXY       , "TR"      , "Transpiration heat"                   , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%EVCXY      , "EVC"     , "Canopy evap heat"                     , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%IRCXY      , "IRC"     , "Canopy net LW rad"                    , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%SHCXY      , "SHC"     , "Canopy sensible heat"                 , "W/m2"                  )
    ! Additional needed to close the under canopy ground energy budget
    call add_to_output_mosaic(NoahmpIO%IRGXY      , "IRG"     , "below-canopy ground net LW rad"       , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%SHGXY      , "SHG"     , "below-canopy ground sensible heat"    , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%EVGXY      , "EVG"     , "below-canopy ground evap heat"        , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GHVXY      , "GHV"     , "below-canopy ground heat to soil"     , "W/m2"                  )
    ! Needed to close the bare ground energy budget
    call add_to_output_mosaic(NoahmpIO%SAGXY      , "SAG"     , "Solar radiation absorbed by ground"   , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%IRBXY      , "IRB"     , "Net LW rad to atm bare ground"        , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%SHBXY      , "SHB"     , "Sensible heat to atm bare ground"     , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%EVBXY      , "EVB"     , "Evaporation heat to atm bare ground"  , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GHBXY      , "GHB"     , "Ground heat flux to soil bare ground" , "W/m2"                  )
    ! Above-soil temperatures
    call add_to_output_mosaic(NoahmpIO%TRADXY     , "TRAD"    , "Surface radiative temperature"        , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TGXY       , "TG"      , "Ground temperature"                   , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TVXY       , "TV"      , "Vegetation temperature"               , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TAHXY      , "TAH"     , "Canopy air temperature"               , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TGVXY      , "TGV"     , "Ground surface Temp vegetated"        , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TGBXY      , "TGB"     , "Ground surface Temp bare"             , "K"                     )
    call add_to_output_mosaic(NoahmpIO%T2MVXY     , "T2MV"    , "2m Air Temp vegetated"                , "K"                     )
    call add_to_output_mosaic(NoahmpIO%T2MBXY     , "T2MB"    , "2m Air Temp bare"                     , "K"                     )
    ! Above-soil moisture
    call add_to_output_mosaic(NoahmpIO%Q2MVXY     , "Q2MV"    , "2m mixing ratio vegetated"            , "kg/kg"                 )
    call add_to_output_mosaic(NoahmpIO%Q2MBXY     , "Q2MB"    , "2m mixing ratio bare"                 , "kg/kg"                 )
    call add_to_output_mosaic(NoahmpIO%EAHXY      , "EAH"     , "Canopy air vapor pressure"            , "Pa"                    )
    call add_to_output_mosaic(NoahmpIO%FWETXY     , "FWET"    , "Wetted fraction of canopy"            , "fraction"              )
    ! Snow and soil - 3D terms
    call add_to_output_mosaic(NoahmpIO%ZSNSOXY(:,-NoahmpIO%nsnow+1:0,:),"ZSNSO_SN","Snow layer depth from snow surface","m","SNOW")
    call add_to_output_mosaic(NoahmpIO%SNICEXY    , "SNICE"   , "Snow layer ice"                       , "mm"             , "SNOW")
    call add_to_output_mosaic(NoahmpIO%SNLIQXY    , "SNLIQ"   , "Snow layer liquid water"              , "mm"             , "SNOW")
    call add_to_output_mosaic(NoahmpIO%TSLB       , "SOIL_T"  , "soil temperature"                     , "K"              , "SOIL")
    call add_to_output_mosaic(NoahmpIO%SMOIS      , "SOIL_M"  , "volumetric soil moisture"             , "m3/m3"          , "SOIL")
    call add_to_output_mosaic(NoahmpIO%SH2O       , "SOIL_W"  , "liquid volumetric soil moisture"      , "m3/m3"          , "SOIL")
    call add_to_output_mosaic(NoahmpIO%TSNOXY     , "SNOW_T"  , "snow temperature"                     , "K"              , "SNOW")
    call add_to_output_mosaic(NoahmpIO%ALBSNOWDIRXY, "ALBSNOWDIR" , "Snow albedo (direct)"             , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSNOWDIFXY, "ALBSNOWDIF" , "Snow albedo (diffuse)"            , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSFCDIRXY , "ALBSFCDIR"  , "Surface albedo (direct)"          , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSFCDIFXY , "ALBSFCDIF"  , "Surface albedo (diffuse)"         , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSOILDIRXY, "ALBSOILDIR"  , "Soil albedo (direct)"            , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSOILDIFXY, "ALBSOILDIF"  , "Soil albedo (diffuse)"           , "-"              , "RADN")
    ! Snow - 2D terms
    call add_to_output_mosaic(NoahmpIO%SNOWH      , "SNOWH"   , "Snow depth"                           , "m"                     )
    call add_to_output_mosaic(NoahmpIO%SNOW       , "SNEQV"   , "Snow water equivalent"                , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%QSNOWXY    , "QSNOW"   , "Snowfall rate on the ground"          , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%QRAINXY    , "QRAIN"   , "Rainfall rate on the ground"          , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%ISNOWXY    , "ISNOW"   , "Number of snow layers"                , "-"                     )
    call add_to_output_mosaic(NoahmpIO%SNOWC      , "FSNO"    , "Snow-cover fraction on the ground"    , "-"                     )
    call add_to_output_mosaic(NoahmpIO%ACSNOW     , "ACSNOW"  , "accumulated snow fall"                , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%ACSNOM     , "ACSNOM"  , "accumulated snow melt water"          , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%QSNBOTXY   , "QSNBOT"  , "water (melt+rain through) out of snow bottom"   , "mm/s"        )
    call add_to_output_mosaic(NoahmpIO%QMELTXY    , "QMELT"   , "snow melt due to phase change"                  , "mm/s"        )
    ! SNICAR snow albedo scheme
    if (NoahmpIO%IOPT_ALB == 3)then
      call add_to_output_mosaic(NoahmpIO%SNRDSXY    , "SNRDS"   , "Snow layer effective grain radius"    , "m-6"           , "SNOW")
      call add_to_output_mosaic(NoahmpIO%SNFRXY     , "SNFR"    , "Snow layer rate of freezing"          , "mm/s"          , "SNOW")
      call add_to_output_mosaic(NoahmpIO%BCPHIXY    , "BCPHI_Mass","hydrophilic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%BCPHOXY    , "BCPHO_Mass","hydrophobic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%OCPHIXY    , "OCPHI_Mass","hydrophilic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%OCPHOXY    , "OCPHO_Mass","hydrophobic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST1XY    , "DUST1_Mass","dust size bin 1 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST2XY    , "DUST2_Mass","dust size bin 2 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST3XY    , "DUST3_Mass","dust size bin 3 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST4XY    , "DUST4_Mass","dust size bin 4 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST5XY    , "DUST5_Mass","dust size bin 5 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcBCPHIXY, "BCPHI_MassConc","hydrophilic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcBCPHOXY, "BCPHO_MassConc","hydrophobic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcOCPHIXY, "OCPHI_MassConc","hydrophilic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcOCPHOXY, "OCPHO_MassConc","hydrophobic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST1XY, "DUST1_MassConc","dust size bin 1 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST2XY, "DUST2_MassConc","dust size bin 2 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST3XY, "DUST3_MassConc","dust size bin 3 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST4XY, "DUST4_MassConc","dust size bin 4 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST5XY, "DUST5_MassConc","dust size bin 5 mass concentration in snow", "kg/kg", "SNOW")
    endif
    ! Exchange coefficients
    call add_to_output_mosaic(NoahmpIO%CMXY       , "CM"      , "Momentum drag coefficient"            , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHXY       , "CH"      , "Sensible heat exchange coefficient"   , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHVXY      , "CHV"     , "Exchange coefficient vegetated"       , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHBXY      , "CHB"     , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHLEAFXY   , "CHLEAF"  , "Exchange coefficient leaf"            , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHUCXY     , "CHUC"    , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHV2XY     , "CHV2"    , "Exchange coefficient 2-m vegetated"   , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHB2XY     , "CHB2"    , "Exchange coefficient 2-m bare"        , "m/s"                   )
    ! Carbon allocation model
    call add_to_output_mosaic(NoahmpIO%LFMASSXY   , "LFMASS"  , "Leaf mass"                            , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%RTMASSXY   , "RTMASS"  , "Mass of fine roots"                   , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%STMASSXY   , "STMASS"  , "Stem mass"                            , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%WOODXY     , "WOOD"    , "Mass of wood and woody roots"         , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GRAINXY    , "GRAIN"   , "Mass of grain"                        , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GDDXY      , "GDD"     , "Growing degree days "                 , "-"                     )
    call add_to_output_mosaic(NoahmpIO%STBLCPXY   , "STBLCP"  , "Stable carbon in deep soil"           , "gC/m2"                 )
    call add_to_output_mosaic(NoahmpIO%FASTCPXY   , "FASTCP"  , "Short-lived carbon in shallow soil"   , "gC/m2"                 )
    call add_to_output_mosaic(NoahmpIO%NEEXY      , "NEE"     , "Net ecosystem exchange"               , "gCO2/m2/s"             )
    call add_to_output_mosaic(NoahmpIO%GPPXY      , "GPP"     , "Net instantaneous assimilation"       , "gC/m2/s"               )
    call add_to_output_mosaic(NoahmpIO%NPPXY      , "NPP"     , "Net primary productivity"             , "gC/m2/s"               )
    call add_to_output_mosaic(NoahmpIO%PSNXY      , "PSN"     , "Total photosynthesis"                 , "umol CO2/m2/s"         )
    call add_to_output_mosaic(NoahmpIO%APARXY     , "APAR"    , "Photosynthesis active energy by canopy", "W/m2"                 )
    ! additional NoahMP output
    if (NoahmpIO%noahmp_output > 0) then
    ! additional water budget terms
      call add_to_output_mosaic(NoahmpIO%QINTSXY    , "QINTS"   , "canopy interception (loading) rate for snowfall", "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QINTRXY    , "QINTR"   , "canopy interception rate for rain"              , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QDRIPSXY   , "QDRIPS"  , "drip (unloading) rate for intercepted snow"     , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QDRIPRXY   , "QDRIPR"  , "drip rate for canopy intercepted rain"          , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QTHROSXY   , "QTHROS"  , "throughfall of snowfall"                        , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QTHRORXY   , "QTHROR"  , "throughfall for rain"                           , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QSNSUBXY   , "QSNSUB"  , "snow surface sublimation rate"                  , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QSNFROXY   , "QSNFRO"  , "snow surface frost rate"                        , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QSUBCXY    , "QSUBC"   , "canopy snow sublimation rate"                   , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QFROCXY    , "QFROC"   , "canopy snow frost rate"                         , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QEVACXY    , "QEVAC"   , "canopy snow evaporation rate"                   , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QDEWCXY    , "QDEWC"   , "canopy snow dew rate"                           , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QFRZCXY    , "QFRZC"   , "refreezing rate of canopy liquid water"         , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QMELTCXY   , "QMELTC"  , "melting rate of canopy snow"                    , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%FPICEXY    , "FPICE"   , "snow fraction in precipitation"                 , "-"           )
      call add_to_output_mosaic(NoahmpIO%ACC_QINSURXY,"ACC_QINSUR", "accumuated water flux to soil within soil timestep"     , "m/s*dt_soil/dt_main")
      call add_to_output_mosaic(NoahmpIO%ACC_QSEVAXY ,"ACC_QSEVA" , "accumulated soil surface evap rate within soil timestep", "m/s*dt_soil/dt_main")
      call add_to_output_mosaic(NoahmpIO%ACC_ETRANIXY,"ACC_ETRANI", "accumualted transpiration rate within soil timestep"    , "m/s*dt_soil/dt_main","SOIL")
      call add_to_output_mosaic(NoahmpIO%ACC_DWATERXY,"ACC_DWATER", "accumulated water storage change within soil timestep"  , "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_PRCPXY  ,"ACC_PRCP"  , "accumulated precipitation within soil timestep"         , "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_ECANXY  ,"ACC_ECAN"  , "accumulated net canopy evaporation within soil timestep", "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_ETRANXY ,"ACC_ETRAN" , "accumulated transpiration within soil timestep"         , "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_EDIRXY  ,"ACC_EDIR"  , "accumulated net ground evaporation within soil timestep", "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_GLAFLWXY,"ACC_GLAFLW", "accumuated glacier excessive flow per soil timestep"    , "mm")
      ! additional energy terms
      call add_to_output_mosaic(NoahmpIO%PAHXY      , "PAH"     , "Precipitation advected heat flux"                         , "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%PAHGXY     , "PAHG"    , "Precipitation advected heat flux to below-canopy ground"  , "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%PAHBXY     , "PAHB"    , "Precipitation advected heat flux to bare ground"          , "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%PAHVXY     , "PAHV"    , "Precipitation advected heat flux to canopy"               , "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%ACC_SSOILXY, "ACC_SSOIL","accumulated heat flux into snow/soil within soil timestep", "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%EFLXBXY    , "EFLXB"   , "accumulated heat flux through soil bottom"                , "J/m2"    )
      call add_to_output_mosaic(NoahmpIO%SOILENERGY , "SOILENERGY","energy content in soil relative to 273.16"               , "KJ/m2"   )
      call add_to_output_mosaic(NoahmpIO%SNOWENERGY , "SNOWENERGY","energy content in snow relative to 273.16"               , "KJ/m2"   )
      call add_to_output_mosaic(NoahmpIO%CANHSXY    , "CANHS"   , "canopy heat storage change"                               , "W/m2"    )
      ! additional forcing terms
      call add_to_output_mosaic(NoahmpIO%RAINLSM    , "RAINLSM" , "lowest model liquid precipitation into LSM"               , "mm/s"    )
      call add_to_output_mosaic(NoahmpIO%SNOWLSM    , "SNOWLSM" , "lowest model snowfall into LSM"                           , "mm/s"    )
      call add_to_output_mosaic(NoahmpIO%FORCTLSM   , "FORCTLSM", "lowest model temperature into LSM"                        , "K"       ) 
      call add_to_output_mosaic(NoahmpIO%FORCQLSM   , "FORCQLSM", "lowest model specific humidty into LSM"                   , "kg/kg"   )
      call add_to_output_mosaic(NoahmpIO%FORCPLSM   , "FORCPLSM", "lowest model pressure into LSM"                           , "Pa"      )
      call add_to_output_mosaic(NoahmpIO%FORCZLSM   , "FORCZLSM", "lowest model forcing height into LSM"                     , "m"       )
      call add_to_output_mosaic(NoahmpIO%FORCWLSM   , "FORCWLSM", "lowest model wind speed into LSM"                         , "m/s"     )
      call add_to_output_mosaic(NoahmpIO%RadSwVisFrac , "SWVISFRAC", "Fraction of visible band downward solar radiation", "-"      )
      call add_to_output_mosaic(NoahmpIO%RadSwDirFrac , "SWDIRFRAC", "Fraction of downward solar direct radiation",   "-"          )
    endif

    ! Irrigation
    if ( NoahmpIO%IOPT_IRR > 0 ) then
      call add_to_output_mosaic(NoahmpIO%IRNUMSI    , "IRNUMSI" , "Sprinkler irrigation count"                               , "-"       )
      call add_to_output_mosaic(NoahmpIO%IRNUMMI    , "IRNUMMI" , "Micro irrigation count"                                   , "-"       )
      call add_to_output_mosaic(NoahmpIO%IRNUMFI    , "IRNUMFI" , "Flood irrigation count"                                   , "-"       )
      call add_to_output_mosaic(NoahmpIO%IRELOSS    , "IRELOSS" , "Accumulated sprinkler Evaporation"                        , "mm"      )
      call add_to_output_mosaic(NoahmpIO%IRSIVOL    , "IRSIVOL" , "Sprinkler irrigation amount"                              , "mm"      )
      call add_to_output_mosaic(NoahmpIO%IRMIVOL    , "IRMIVOL" , "Micro irrigation amount"                                  , "mm"      )
      call add_to_output_mosaic(NoahmpIO%IRFIVOL    , "IRFIVOL" , "Flood irrigation amount"                                  , "mm"      )
      call add_to_output_mosaic(NoahmpIO%IRRSPLH    , "IRRSPLH" , "Accumulated latent heating due to sprinkler"              , "J/m2"    )
    endif
    ! MMF groundwater  model
    if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
      call add_to_output_mosaic(NoahmpIO%SMCWTDXY   , "SMCWTD"   , "soil water content between bottom of the soil and water table", "m3/m3"  )
      call add_to_output_mosaic(NoahmpIO%RECHXY     , "RECH"     , "recharge to or from the water table when shallow"             , "m"      )
      call add_to_output_mosaic(NoahmpIO%DEEPRECHXY , "DEEPRECH" , "recharge to or from the water table when deep"                , "m"      )
      call add_to_output_mosaic(NoahmpIO%QRFSXY     , "QRFS"     , "accumulated groundwater baselow"                              , "mm"     )
      call add_to_output_mosaic(NoahmpIO%QRFXY      , "QRF"      , "groundwater baseflow"                                         , "m"      )
      call add_to_output_mosaic(NoahmpIO%QSPRINGSXY , "QSPRINGS" , "accumulated seeping water"                                    , "mm"     )
      call add_to_output_mosaic(NoahmpIO%QSPRINGXY  , "QSPRING"  , "instantaneous seeping water"                                  , "m"      )
      call add_to_output_mosaic(NoahmpIO%QSLATXY    , "QSLAT"    , "accumulated lateral flow"                                     , "mm"     )
      call add_to_output_mosaic(NoahmpIO%QLATXY     , "QLAT"     , "instantaneous lateral flow"                                   , "m"      )
    endif
    ! Wetland model
    if ( NoahmpIO%IOPT_WETLAND > 0 ) then
      call add_to_output_mosaic(NoahmpIO%FSATXY     , "FSAT"    , "saturated fraction of the grid"                           , "-" )
      call add_to_output_mosaic(NoahmpIO%WSURFXY    , "WSURF"   , "Wetland Water Storage"                                    , "mm")
    endif

  end subroutine WriteNoahmpMosaicSubgridOutput

end module WriteNoahmpMosaicSubgridOutputMod