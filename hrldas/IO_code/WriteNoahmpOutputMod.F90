module WriteNoahmpOutputMod

!!! This module part of NoahMP/HRLDAS output writing workflow
!!! Purpose: To write NoahMP grid values to model output file. 

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

  subroutine WriteNoahmpOutput (NoahmpIO)
 
    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO

    ! For 3D arrays, we need to know whether the Z dimension is snow layers, or soil layers.

    ! Properties - Assigned or predicted
    call add_to_output(NoahmpIO%IVGTYP     , "IVGTYP"  , "Dominant vegetation category"         , "category"              )
    call add_to_output(NoahmpIO%ISLTYP     , "ISLTYP"  , "Dominant soil category"               , "category"              )
    call add_to_output(NoahmpIO%FVEGXY     , "FVEG"    , "Green Vegetation Fraction"            , "-"                     )
    call add_to_output(NoahmpIO%LAI        , "LAI"     , "Leaf area index"                      , "m2/m2"                 )
    call add_to_output(NoahmpIO%XSAIXY     , "SAI"     , "Stem area index"                      , "m2/m2"                 )
    ! Forcing
    call add_to_output(NoahmpIO%SWDOWN     , "SWFORC"  , "Shortwave forcing"                    , "W/m2"                  )
    call add_to_output(NoahmpIO%COSZEN     , "COSZ"    , "Cosine of zenith angle"               , "-"                     )
    call add_to_output(NoahmpIO%GLW        , "LWFORC"  , "Longwave forcing"                     , "W/m2"                  )
    call add_to_output(NoahmpIO%RAINBL     , "RAINRATE", "Precipitation rate"                   , "mm/timestep"           )
    ! Grid energy budget terms
    call add_to_output(NoahmpIO%EMISS      , "EMISS"   , "Grid emissivity"                      , "-"                     )
    call add_to_output(NoahmpIO%FSAXY      , "FSA"     , "Total absorbed SW radiation"          , "W/m2"                  )         
    call add_to_output(NoahmpIO%FIRAXY     , "FIRA"    , "Total net LW radiation to atmosphere" , "W/m2"                  )
    call add_to_output(NoahmpIO%GRDFLX     , "GRDFLX"  , "Heat flux into the soil"              , "W/m2"                  )
    call add_to_output(NoahmpIO%HFX        , "HFX"     , "Total sensible heat to atmosphere"    , "W/m2"                  )
    call add_to_output(NoahmpIO%LH         , "LH"      , "Total latent heat to atmosphere"      , "W/m2"                  )
    call add_to_output(NoahmpIO%ECANXY     , "ECAN"    , "Canopy water evaporation rate"        , "mm/s"                  )
    call add_to_output(NoahmpIO%ETRANXY    , "ETRAN"   , "Transpiration rate"                   , "mm/s"                  )
    call add_to_output(NoahmpIO%EDIRXY     , "EDIR"    , "Direct from soil evaporation rate"    , "mm/s"                  )
    call add_to_output(NoahmpIO%ALBEDO     , "ALBEDO"  , "Surface albedo"                       , "-"                     )
    ! Grid water budget terms - in addition to above
    call add_to_output(NoahmpIO%UDRUNOFF   , "UGDRNOFF", "Accumulated underground runoff"       , "mm"                    )
    call add_to_output(NoahmpIO%SFCRUNOFF  , "SFCRNOFF", "Accumulatetd surface runoff"          , "mm"                    )
    call add_to_output(NoahmpIO%CANLIQXY   , "CANLIQ"  , "Canopy liquid water content"          , "mm"                    )
    call add_to_output(NoahmpIO%CANICEXY   , "CANICE"  , "Canopy ice water content"             , "mm"                    )
    call add_to_output(NoahmpIO%ZWTXY      , "ZWT"     , "Depth to water table"                 , "m"                     )
    call add_to_output(NoahmpIO%WAXY       , "WA"      , "Water in aquifer"                     , "mm"                    )
    call add_to_output(NoahmpIO%WTXY       , "WT"      , "Water in aquifer and saturated soil"  , "mm"                    )
    call add_to_output(NoahmpIO%QTDRAIN    , "QTDRAIN" , "Accumulated tile drainage"            , "mm"                    )
    call add_to_output(NoahmpIO%PONDINGXY  , "PONDING" , "total surface ponding per time step"            , "mm/s"        )
    ! Additional needed to close the canopy energy budget
    call add_to_output(NoahmpIO%SAVXY      , "SAV"     , "Solar radiation absorbed by canopy"   , "W/m2"                  )
    call add_to_output(NoahmpIO%TRXY       , "TR"      , "Transpiration heat"                   , "W/m2"                  )
    call add_to_output(NoahmpIO%EVCXY      , "EVC"     , "Canopy evap heat"                     , "W/m2"                  )
    call add_to_output(NoahmpIO%IRCXY      , "IRC"     , "Canopy net LW rad"                    , "W/m2"                  )
    call add_to_output(NoahmpIO%SHCXY      , "SHC"     , "Canopy sensible heat"                 , "W/m2"                  )
    ! Additional needed to close the under canopy ground energy budget
    call add_to_output(NoahmpIO%IRGXY      , "IRG"     , "below-canopy ground net LW rad"       , "W/m2"                  )
    call add_to_output(NoahmpIO%SHGXY      , "SHG"     , "below-canopy ground sensible heat"    , "W/m2"                  )
    call add_to_output(NoahmpIO%EVGXY      , "EVG"     , "below-canopy ground evap heat"        , "W/m2"                  )
    call add_to_output(NoahmpIO%GHVXY      , "GHV"     , "below-canopy ground heat to soil"     , "W/m2"                  )
    ! Needed to close the bare ground energy budget
    call add_to_output(NoahmpIO%SAGXY      , "SAG"     , "Solar radiation absorbed by ground"   , "W/m2"                  )
    call add_to_output(NoahmpIO%IRBXY      , "IRB"     , "Net LW rad to atm bare ground"        , "W/m2"                  )
    call add_to_output(NoahmpIO%SHBXY      , "SHB"     , "Sensible heat to atm bare ground"     , "W/m2"                  )
    call add_to_output(NoahmpIO%EVBXY      , "EVB"     , "Evaporation heat to atm bare ground"  , "W/m2"                  )
    call add_to_output(NoahmpIO%GHBXY      , "GHB"     , "Ground heat flux to soil bare ground" , "W/m2"                  )
    ! Above-soil temperatures
    call add_to_output(NoahmpIO%TRADXY     , "TRAD"    , "Surface radiative temperature"        , "K"                     )
    call add_to_output(NoahmpIO%TGXY       , "TG"      , "Ground temperature"                   , "K"                     )
    call add_to_output(NoahmpIO%TVXY       , "TV"      , "Vegetation temperature"               , "K"                     )
    call add_to_output(NoahmpIO%TAHXY      , "TAH"     , "Canopy air temperature"               , "K"                     )
    call add_to_output(NoahmpIO%TGVXY      , "TGV"     , "Ground surface Temp vegetated"        , "K"                     )
    call add_to_output(NoahmpIO%TGBXY      , "TGB"     , "Ground surface Temp bare"             , "K"                     )
    call add_to_output(NoahmpIO%T2MVXY     , "T2MV"    , "2m Air Temp vegetated"                , "K"                     )
    call add_to_output(NoahmpIO%T2MBXY     , "T2MB"    , "2m Air Temp bare"                     , "K"                     )
    ! Above-soil moisture
    call add_to_output(NoahmpIO%Q2MVXY     , "Q2MV"    , "2m mixing ratio vegetated"            , "kg/kg"                 )
    call add_to_output(NoahmpIO%Q2MBXY     , "Q2MB"    , "2m mixing ratio bare"                 , "kg/kg"                 )
    call add_to_output(NoahmpIO%EAHXY      , "EAH"     , "Canopy air vapor pressure"            , "Pa"                    )
    call add_to_output(NoahmpIO%FWETXY     , "FWET"    , "Wetted fraction of canopy"            , "fraction"              )
    ! Snow and soil - 3D terms
    call add_to_output(NoahmpIO%ZSNSOXY(:,-NoahmpIO%nsnow+1:0,:),"ZSNSO_SN","Snow layer depth from snow surface","m","SNOW")
    call add_to_output(NoahmpIO%SNICEXY    , "SNICE"   , "Snow layer ice"                       , "mm"             , "SNOW")
    call add_to_output(NoahmpIO%SNLIQXY    , "SNLIQ"   , "Snow layer liquid water"              , "mm"             , "SNOW")
    call add_to_output(NoahmpIO%TSLB       , "SOIL_T"  , "soil temperature"                     , "K"              , "SOIL")
    call add_to_output(NoahmpIO%SMOIS      , "SOIL_M"  , "volumetric soil moisture"             , "m3/m3"          , "SOIL")
    call add_to_output(NoahmpIO%SH2O       , "SOIL_W"  , "liquid volumetric soil moisture"      , "m3/m3"          , "SOIL")
    call add_to_output(NoahmpIO%TSNOXY     , "SNOW_T"  , "snow temperature"                     , "K"              , "SNOW")
    call add_to_output(NoahmpIO%ALBSNOWDIRXY, "ALBSNOWDIR" , "Snow albedo (direct)"             , "-"              , "RADN")
    call add_to_output(NoahmpIO%ALBSNOWDIFXY, "ALBSNOWDIF" , "Snow albedo (diffuse)"            , "-"              , "RADN")
    call add_to_output(NoahmpIO%ALBSFCDIRXY , "ALBSFCDIR"  , "Surface albedo (direct)"          , "-"              , "RADN")
    call add_to_output(NoahmpIO%ALBSFCDIFXY , "ALBSFCDIF"  , "Surface albedo (diffuse)"         , "-"              , "RADN")
    call add_to_output(NoahmpIO%ALBSOILDIRXY, "ALBSOILDIR"  , "Soil albedo (direct)"            , "-"              , "RADN")
    call add_to_output(NoahmpIO%ALBSOILDIFXY, "ALBSOILDIF"  , "Soil albedo (diffuse)"           , "-"              , "RADN")
    ! Snow - 2D terms
    call add_to_output(NoahmpIO%SNOWH      , "SNOWH"   , "Snow depth"                           , "m"                     )
    call add_to_output(NoahmpIO%SNOW       , "SNEQV"   , "Snow water equivalent"                , "mm"                    )
    call add_to_output(NoahmpIO%QSNOWXY    , "QSNOW"   , "Snowfall rate on the ground"          , "mm/s"                  )
    call add_to_output(NoahmpIO%QRAINXY    , "QRAIN"   , "Rainfall rate on the ground"          , "mm/s"                  )
    call add_to_output(NoahmpIO%ISNOWXY    , "ISNOW"   , "Number of snow layers"                , "-"                     )
    call add_to_output(NoahmpIO%SNOWC      , "FSNO"    , "Snow-cover fraction on the ground"    , "-"                     )
    call add_to_output(NoahmpIO%ACSNOW     , "ACSNOW"  , "accumulated snow fall"                , "mm"                    )
    call add_to_output(NoahmpIO%ACSNOM     , "ACSNOM"  , "accumulated snow melt water"          , "mm"                    )
    call add_to_output(NoahmpIO%QSNBOTXY   , "QSNBOT"  , "water (melt+rain through) out of snow bottom"   , "mm/s"        )
    call add_to_output(NoahmpIO%QMELTXY    , "QMELT"   , "snow melt due to phase change"                  , "mm/s"        )
    ! SNICAR snow albedo scheme
    if (NoahmpIO%IOPT_ALB == 3)then
      call add_to_output(NoahmpIO%SNRDSXY    , "SNRDS"   , "Snow layer effective grain radius"    , "m-6"           , "SNOW")
      call add_to_output(NoahmpIO%SNFRXY     , "SNFR"    , "Snow layer rate of freezing"          , "mm/s"          , "SNOW")
      call add_to_output(NoahmpIO%BCPHIXY    , "BCPHI_Mass","hydrophilic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%BCPHOXY    , "BCPHO_Mass","hydrophobic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%OCPHIXY    , "OCPHI_Mass","hydrophilic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%OCPHOXY    , "OCPHO_Mass","hydrophobic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%DUST1XY    , "DUST1_Mass","dust size bin 1 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%DUST2XY    , "DUST2_Mass","dust size bin 2 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%DUST3XY    , "DUST3_Mass","dust size bin 3 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%DUST4XY    , "DUST4_Mass","dust size bin 4 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%DUST5XY    , "DUST5_Mass","dust size bin 5 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output(NoahmpIO%MassConcBCPHIXY, "BCPHI_MassConc","hydrophilic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output(NoahmpIO%MassConcBCPHOXY, "BCPHO_MassConc","hydrophobic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output(NoahmpIO%MassConcOCPHIXY, "OCPHI_MassConc","hydrophilic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output(NoahmpIO%MassConcOCPHOXY, "OCPHO_MassConc","hydrophobic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output(NoahmpIO%MassConcDUST1XY, "DUST1_MassConc","dust size bin 1 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output(NoahmpIO%MassConcDUST2XY, "DUST2_MassConc","dust size bin 2 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output(NoahmpIO%MassConcDUST3XY, "DUST3_MassConc","dust size bin 3 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output(NoahmpIO%MassConcDUST4XY, "DUST4_MassConc","dust size bin 4 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output(NoahmpIO%MassConcDUST5XY, "DUST5_MassConc","dust size bin 5 mass concentration in snow", "kg/kg", "SNOW")
    endif
    ! Exchange coefficients
    call add_to_output(NoahmpIO%CMXY       , "CM"      , "Momentum drag coefficient"            , "m/s"                   )
    call add_to_output(NoahmpIO%CHXY       , "CH"      , "Sensible heat exchange coefficient"   , "m/s"                   )
    call add_to_output(NoahmpIO%CHVXY      , "CHV"     , "Exchange coefficient vegetated"       , "m/s"                   )
    call add_to_output(NoahmpIO%CHBXY      , "CHB"     , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output(NoahmpIO%CHLEAFXY   , "CHLEAF"  , "Exchange coefficient leaf"            , "m/s"                   )
    call add_to_output(NoahmpIO%CHUCXY     , "CHUC"    , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output(NoahmpIO%CHV2XY     , "CHV2"    , "Exchange coefficient 2-m vegetated"   , "m/s"                   )
    call add_to_output(NoahmpIO%CHB2XY     , "CHB2"    , "Exchange coefficient 2-m bare"        , "m/s"                   )
    ! Carbon allocation model
    call add_to_output(NoahmpIO%LFMASSXY   , "LFMASS"  , "Leaf mass"                            , "g/m2"                  )
    call add_to_output(NoahmpIO%RTMASSXY   , "RTMASS"  , "Mass of fine roots"                   , "g/m2"                  )
    call add_to_output(NoahmpIO%STMASSXY   , "STMASS"  , "Stem mass"                            , "g/m2"                  )
    call add_to_output(NoahmpIO%WOODXY     , "WOOD"    , "Mass of wood and woody roots"         , "g/m2"                  )
    call add_to_output(NoahmpIO%GRAINXY    , "GRAIN"   , "Mass of grain"                        , "g/m2"                  )
    call add_to_output(NoahmpIO%GDDXY      , "GDD"     , "Growing degree days "                 , "-"                     )
    call add_to_output(NoahmpIO%STBLCPXY   , "STBLCP"  , "Stable carbon in deep soil"           , "gC/m2"                 )
    call add_to_output(NoahmpIO%FASTCPXY   , "FASTCP"  , "Short-lived carbon in shallow soil"   , "gC/m2"                 )
    call add_to_output(NoahmpIO%NEEXY      , "NEE"     , "Net ecosystem exchange"               , "gCO2/m2/s"             )
    call add_to_output(NoahmpIO%GPPXY      , "GPP"     , "Net instantaneous assimilation"       , "gC/m2/s"               )
    call add_to_output(NoahmpIO%NPPXY      , "NPP"     , "Net primary productivity"             , "gC/m2/s"               )
    call add_to_output(NoahmpIO%PSNXY      , "PSN"     , "Total photosynthesis"                 , "umol CO2/m2/s"         )
    call add_to_output(NoahmpIO%APARXY     , "APAR"    , "Photosynthesis active energy by canopy", "W/m2"                 )
    ! additional NoahMP output
    if (NoahmpIO%noahmp_output > 0) then
    ! additional water budget terms
      call add_to_output(NoahmpIO%QINTSXY    , "QINTS"   , "canopy interception (loading) rate for snowfall", "mm/s"        )
      call add_to_output(NoahmpIO%QINTRXY    , "QINTR"   , "canopy interception rate for rain"              , "mm/s"        )
      call add_to_output(NoahmpIO%QDRIPSXY   , "QDRIPS"  , "drip (unloading) rate for intercepted snow"     , "mm/s"        )
      call add_to_output(NoahmpIO%QDRIPRXY   , "QDRIPR"  , "drip rate for canopy intercepted rain"          , "mm/s"        )
      call add_to_output(NoahmpIO%QTHROSXY   , "QTHROS"  , "throughfall of snowfall"                        , "mm/s"        )
      call add_to_output(NoahmpIO%QTHRORXY   , "QTHROR"  , "throughfall for rain"                           , "mm/s"        )
      call add_to_output(NoahmpIO%QSNSUBXY   , "QSNSUB"  , "snow surface sublimation rate"                  , "mm/s"        )
      call add_to_output(NoahmpIO%QSNFROXY   , "QSNFRO"  , "snow surface frost rate"                        , "mm/s"        )
      call add_to_output(NoahmpIO%QSUBCXY    , "QSUBC"   , "canopy snow sublimation rate"                   , "mm/s"        )
      call add_to_output(NoahmpIO%QFROCXY    , "QFROC"   , "canopy snow frost rate"                         , "mm/s"        )
      call add_to_output(NoahmpIO%QEVACXY    , "QEVAC"   , "canopy snow evaporation rate"                   , "mm/s"        )
      call add_to_output(NoahmpIO%QDEWCXY    , "QDEWC"   , "canopy snow dew rate"                           , "mm/s"        )
      call add_to_output(NoahmpIO%QFRZCXY    , "QFRZC"   , "refreezing rate of canopy liquid water"         , "mm/s"        )
      call add_to_output(NoahmpIO%QMELTCXY   , "QMELTC"  , "melting rate of canopy snow"                    , "mm/s"        )
      call add_to_output(NoahmpIO%FPICEXY    , "FPICE"   , "snow fraction in precipitation"                 , "-"           )
      call add_to_output(NoahmpIO%ACC_QINSURXY,"ACC_QINSUR", "accumuated water flux to soil within soil timestep"     , "m/s*dt_soil/dt_main")
      call add_to_output(NoahmpIO%ACC_QSEVAXY ,"ACC_QSEVA" , "accumulated soil surface evap rate within soil timestep", "m/s*dt_soil/dt_main")
      call add_to_output(NoahmpIO%ACC_ETRANIXY,"ACC_ETRANI", "accumualted transpiration rate within soil timestep"    , "m/s*dt_soil/dt_main","SOIL")
      call add_to_output(NoahmpIO%ACC_DWATERXY,"ACC_DWATER", "accumulated water storage change within soil timestep"  , "mm")
      call add_to_output(NoahmpIO%ACC_PRCPXY  ,"ACC_PRCP"  , "accumulated precipitation within soil timestep"         , "mm")
      call add_to_output(NoahmpIO%ACC_ECANXY  ,"ACC_ECAN"  , "accumulated net canopy evaporation within soil timestep", "mm")
      call add_to_output(NoahmpIO%ACC_ETRANXY ,"ACC_ETRAN" , "accumulated transpiration within soil timestep"         , "mm")
      call add_to_output(NoahmpIO%ACC_EDIRXY  ,"ACC_EDIR"  , "accumulated net ground evaporation within soil timestep", "mm")
      call add_to_output(NoahmpIO%ACC_GLAFLWXY,"ACC_GLAFLW", "accumuated glacier excessive flow per soil timestep"    , "mm")
      ! additional energy terms
      call add_to_output(NoahmpIO%PAHXY      , "PAH"     , "Precipitation advected heat flux"                         , "W/m2"    )
      call add_to_output(NoahmpIO%PAHGXY     , "PAHG"    , "Precipitation advected heat flux to below-canopy ground"  , "W/m2"    )
      call add_to_output(NoahmpIO%PAHBXY     , "PAHB"    , "Precipitation advected heat flux to bare ground"          , "W/m2"    )
      call add_to_output(NoahmpIO%PAHVXY     , "PAHV"    , "Precipitation advected heat flux to canopy"               , "W/m2"    )
      call add_to_output(NoahmpIO%ACC_SSOILXY, "ACC_SSOIL","accumulated heat flux into snow/soil within soil timestep", "W/m2"    )
      call add_to_output(NoahmpIO%EFLXBXY    , "EFLXB"   , "accumulated heat flux through soil bottom"                , "J/m2"    )
      call add_to_output(NoahmpIO%SOILENERGY , "SOILENERGY","energy content in soil relative to 273.16"               , "KJ/m2"   )
      call add_to_output(NoahmpIO%SNOWENERGY , "SNOWENERGY","energy content in snow relative to 273.16"               , "KJ/m2"   )
      call add_to_output(NoahmpIO%CANHSXY    , "CANHS"   , "canopy heat storage change"                               , "W/m2"    )
      ! additional forcing terms
      call add_to_output(NoahmpIO%RAINLSM    , "RAINLSM" , "lowest model liquid precipitation into LSM"               , "mm/s"    )
      call add_to_output(NoahmpIO%SNOWLSM    , "SNOWLSM" , "lowest model snowfall into LSM"                           , "mm/s"    )
      call add_to_output(NoahmpIO%FORCTLSM   , "FORCTLSM", "lowest model temperature into LSM"                        , "K"       ) 
      call add_to_output(NoahmpIO%FORCQLSM   , "FORCQLSM", "lowest model specific humidty into LSM"                   , "kg/kg"   )
      call add_to_output(NoahmpIO%FORCPLSM   , "FORCPLSM", "lowest model pressure into LSM"                           , "Pa"      )
      call add_to_output(NoahmpIO%FORCZLSM   , "FORCZLSM", "lowest model forcing height into LSM"                     , "m"       )
      call add_to_output(NoahmpIO%FORCWLSM   , "FORCWLSM", "lowest model wind speed into LSM"                         , "m/s"     )
      call add_to_output(NoahmpIO%RadSwVisFrac , "SWVISFRAC", "Fraction of visible band downward solar radiation", "-"      )
      call add_to_output(NoahmpIO%RadSwDirFrac , "SWDIRFRAC", "Fraction of downward solar direct radiation",   "-"          )
    endif

    ! Irrigation
    if ( NoahmpIO%IOPT_IRR > 0 ) then
      call add_to_output(NoahmpIO%IRNUMSI    , "IRNUMSI" , "Sprinkler irrigation count"                               , "-"       )
      call add_to_output(NoahmpIO%IRNUMMI    , "IRNUMMI" , "Micro irrigation count"                                   , "-"       )
      call add_to_output(NoahmpIO%IRNUMFI    , "IRNUMFI" , "Flood irrigation count"                                   , "-"       )
      call add_to_output(NoahmpIO%IRELOSS    , "IRELOSS" , "Accumulated sprinkler Evaporation"                        , "mm"      )
      call add_to_output(NoahmpIO%IRSIVOL    , "IRSIVOL" , "Sprinkler irrigation amount"                              , "mm"      )
      call add_to_output(NoahmpIO%IRMIVOL    , "IRMIVOL" , "Micro irrigation amount"                                  , "mm"      )
      call add_to_output(NoahmpIO%IRFIVOL    , "IRFIVOL" , "Flood irrigation amount"                                  , "mm"      )
      call add_to_output(NoahmpIO%IRRSPLH    , "IRRSPLH" , "Accumulated latent heating due to sprinkler"              , "J/m2"    )
    endif
    ! MMF groundwater  model
    if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
      call add_to_output(NoahmpIO%SMCWTDXY   , "SMCWTD"   , "soil water content between bottom of the soil and water table", "m3/m3"  )
      call add_to_output(NoahmpIO%RECHXY     , "RECH"     , "recharge to or from the water table when shallow"             , "m"      )
      call add_to_output(NoahmpIO%DEEPRECHXY , "DEEPRECH" , "recharge to or from the water table when deep"                , "m"      )
      call add_to_output(NoahmpIO%QRFSXY     , "QRFS"     , "accumulated groundwater baselow"                              , "mm"     )
      call add_to_output(NoahmpIO%QRFXY      , "QRF"      , "groundwater baseflow"                                         , "m"      )
      call add_to_output(NoahmpIO%QSPRINGSXY , "QSPRINGS" , "accumulated seeping water"                                    , "mm"     )
      call add_to_output(NoahmpIO%QSPRINGXY  , "QSPRING"  , "instantaneous seeping water"                                  , "m"      )
      call add_to_output(NoahmpIO%QSLATXY    , "QSLAT"    , "accumulated lateral flow"                                     , "mm"     )
      call add_to_output(NoahmpIO%QLATXY     , "QLAT"     , "instantaneous lateral flow"                                   , "m"      )
    endif
    ! Wetland model
    if ( NoahmpIO%IOPT_WETLAND > 0 ) then
      call add_to_output(NoahmpIO%FSATXY     , "FSAT"    , "saturated fraction of the grid"                           , "-" )
      call add_to_output(NoahmpIO%WSURFXY    , "WSURF"   , "Wetland Water Storage"                                    , "mm")
    endif

  end subroutine WriteNoahmpOutput

end module WriteNoahmpOutputMod