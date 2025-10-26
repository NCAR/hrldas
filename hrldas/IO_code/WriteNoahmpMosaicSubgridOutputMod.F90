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
    call add_to_output(NoahmpIO%IVGTYP     , "IVGTYP"  , "Dominant vegetation category"         , "category"              )
    call add_to_output(NoahmpIO%ISLTYP     , "ISLTYP"  , "Dominant soil category"               , "category"              )
    call add_to_output_mosaic(NoahmpIO%FVEGXY     , "FVEG"    ,NoahmpIO%NTilesMax , "Green Vegetation Fraction"            , "-"                     )
    call add_to_output_mosaic(NoahmpIO%LAI        , "LAI"     ,NoahmpIO%NTilesMax , "Leaf area index"                      , "m2/m2"                 )
    call add_to_output_mosaic(NoahmpIO%XSAIXY     , "SAI"     ,NoahmpIO%NTilesMax , "Stem area index"                      , "m2/m2"                 )
    ! Forcing
    call add_to_output(NoahmpIO%SWDOWN     , "SWFORC"  , "Shortwave forcing"                    , "W/m2"                  )
    call add_to_output(NoahmpIO%COSZEN     , "COSZ"    , "Cosine of zenith angle"               , "-"                     )
    call add_to_output(NoahmpIO%GLW        , "LWFORC"  , "Longwave forcing"                     , "W/m2"                  )
    call add_to_output(NoahmpIO%RAINBL     , "RAINRATE", "Precipitation rate"                   , "mm/timestep"           )
    ! Grid energy budget terms
    call add_to_output_mosaic(NoahmpIO%EMISS      , "EMISS"   ,NoahmpIO%NTilesMax , "Grid emissivity"                      , "-"                     )
    call add_to_output_mosaic(NoahmpIO%FSAXY      , "FSA"     ,NoahmpIO%NTilesMax , "Total absorbed SW radiation"          , "W/m2"                  )         
    call add_to_output_mosaic(NoahmpIO%FIRAXY     , "FIRA"    ,NoahmpIO%NTilesMax , "Total net LW radiation to atmosphere" , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GRDFLX     , "GRDFLX"  ,NoahmpIO%NTilesMax , "Heat flux into the soil"              , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%HFX        , "HFX"     ,NoahmpIO%NTilesMax , "Total sensible heat to atmosphere"    , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%LH         , "LH"      ,NoahmpIO%NTilesMax , "Total latent heat to atmosphere"      , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%ECANXY     , "ECAN"    ,NoahmpIO%NTilesMax , "Canopy water evaporation rate"        , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%ETRANXY    , "ETRAN"   ,NoahmpIO%NTilesMax , "Transpiration rate"                   , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%EDIRXY     , "EDIR"    ,NoahmpIO%NTilesMax , "Direct from soil evaporation rate"    , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%ALBEDO     , "ALBEDO"  ,NoahmpIO%NTilesMax , "Surface albedo"                       , "-"                     )
    ! Grid water budget terms - in addition to above
    call add_to_output_mosaic(NoahmpIO%UDRUNOFF   , "UGDRNOFF",NoahmpIO%NTilesMax , "Accumulated underground runoff"       , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%SFCRUNOFF  , "SFCRNOFF",NoahmpIO%NTilesMax , "Accumulatetd surface runoff"          , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%CANLIQXY   , "CANLIQ"  ,NoahmpIO%NTilesMax , "Canopy liquid water content"          , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%CANICEXY   , "CANICE"  ,NoahmpIO%NTilesMax , "Canopy ice water content"             , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%ZWTXY      , "ZWT"     ,NoahmpIO%NTilesMax , "Depth to water table"                 , "m"                     )
    call add_to_output_mosaic(NoahmpIO%WAXY       , "WA"      ,NoahmpIO%NTilesMax , "Water in aquifer"                     , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%WTXY       , "WT"      ,NoahmpIO%NTilesMax , "Water in aquifer and saturated soil"  , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%QTDRAIN    , "QTDRAIN" ,NoahmpIO%NTilesMax , "Accumulated tile drainage"            , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%PONDINGXY  , "PONDING" ,NoahmpIO%NTilesMax , "total surface ponding per time step"            , "mm/s"        )
    ! Additional needed to close the canopy energy budget
    call add_to_output_mosaic(NoahmpIO%SAVXY      , "SAV"     ,NoahmpIO%NTilesMax , "Solar radiation absorbed by canopy"   , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%TRXY       , "TR"      ,NoahmpIO%NTilesMax , "Transpiration heat"                   , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%EVCXY      , "EVC"     ,NoahmpIO%NTilesMax , "Canopy evap heat"                     , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%IRCXY      , "IRC"     ,NoahmpIO%NTilesMax , "Canopy net LW rad"                    , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%SHCXY      , "SHC"     ,NoahmpIO%NTilesMax , "Canopy sensible heat"                 , "W/m2"                  )
    ! Additional needed to close the under canopy ground energy budget
    call add_to_output_mosaic(NoahmpIO%IRGXY      , "IRG"     ,NoahmpIO%NTilesMax , "below-canopy ground net LW rad"       , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%SHGXY      , "SHG"     ,NoahmpIO%NTilesMax , "below-canopy ground sensible heat"    , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%EVGXY      , "EVG"     ,NoahmpIO%NTilesMax , "below-canopy ground evap heat"        , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GHVXY      , "GHV"     ,NoahmpIO%NTilesMax , "below-canopy ground heat to soil"     , "W/m2"                  )
    ! Needed to close the bare ground energy budget
    call add_to_output_mosaic(NoahmpIO%SAGXY      , "SAG"     ,NoahmpIO%NTilesMax , "Solar radiation absorbed by ground"   , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%IRBXY      , "IRB"     ,NoahmpIO%NTilesMax , "Net LW rad to atm bare ground"        , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%SHBXY      , "SHB"     ,NoahmpIO%NTilesMax , "Sensible heat to atm bare ground"     , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%EVBXY      , "EVB"     ,NoahmpIO%NTilesMax , "Evaporation heat to atm bare ground"  , "W/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GHBXY      , "GHB"     ,NoahmpIO%NTilesMax , "Ground heat flux to soil bare ground" , "W/m2"                  )
    ! Above-soil temperatures
    call add_to_output_mosaic(NoahmpIO%TRADXY     , "TRAD"    ,NoahmpIO%NTilesMax , "Surface radiative temperature"        , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TGXY       , "TG"      ,NoahmpIO%NTilesMax , "Ground temperature"                   , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TVXY       , "TV"      ,NoahmpIO%NTilesMax , "Vegetation temperature"               , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TAHXY      , "TAH"     ,NoahmpIO%NTilesMax , "Canopy air temperature"               , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TGVXY      , "TGV"     ,NoahmpIO%NTilesMax , "Ground surface Temp vegetated"        , "K"                     )
    call add_to_output_mosaic(NoahmpIO%TGBXY      , "TGB"     ,NoahmpIO%NTilesMax , "Ground surface Temp bare"             , "K"                     )
    call add_to_output_mosaic(NoahmpIO%T2MVXY     , "T2MV"    ,NoahmpIO%NTilesMax , "2m Air Temp vegetated"                , "K"                     )
    call add_to_output_mosaic(NoahmpIO%T2MBXY     , "T2MB"    ,NoahmpIO%NTilesMax , "2m Air Temp bare"                     , "K"                     )
    ! Above-soil moisture
    call add_to_output_mosaic(NoahmpIO%Q2MVXY     , "Q2MV"    ,NoahmpIO%NTilesMax , "2m mixing ratio vegetated"            , "kg/kg"                 )
    call add_to_output_mosaic(NoahmpIO%Q2MBXY     , "Q2MB"    ,NoahmpIO%NTilesMax , "2m mixing ratio bare"                 , "kg/kg"                 )
    call add_to_output_mosaic(NoahmpIO%EAHXY      , "EAH"     ,NoahmpIO%NTilesMax , "Canopy air vapor pressure"            , "Pa"                    )
    call add_to_output_mosaic(NoahmpIO%FWETXY     , "FWET"    ,NoahmpIO%NTilesMax , "Wetted fraction of canopy"            , "fraction"              )
    ! Snow and soil - 3D terms
    call add_to_output_mosaic(NoahmpIO%ZSNSOXY(:,-NoahmpIO%nsnow+1:0,:,:),"ZSNSO_SN",NoahmpIO%NTilesMax ,"Snow layer depth from snow surface","m","SNOW")
    call add_to_output_mosaic(NoahmpIO%SNICEXY    , "SNICE"   ,NoahmpIO%NTilesMax , "Snow layer ice"                       , "mm"             , "SNOW")
    call add_to_output_mosaic(NoahmpIO%SNLIQXY    , "SNLIQ"   ,NoahmpIO%NTilesMax , "Snow layer liquid water"              , "mm"             , "SNOW")
    call add_to_output_mosaic(NoahmpIO%TSLB       , "SOIL_T"  ,NoahmpIO%NTilesMax , "soil temperature"                     , "K"              , "SOIL")
    call add_to_output_mosaic(NoahmpIO%SMOIS      , "SOIL_M"  ,NoahmpIO%NTilesMax , "volumetric soil moisture"             , "m3/m3"          , "SOIL")
    call add_to_output_mosaic(NoahmpIO%SH2O       , "SOIL_W"  ,NoahmpIO%NTilesMax , "liquid volumetric soil moisture"      , "m3/m3"          , "SOIL")
    call add_to_output_mosaic(NoahmpIO%TSNOXY     , "SNOW_T"  ,NoahmpIO%NTilesMax , "snow temperature"                     , "K"              , "SNOW")
    call add_to_output_mosaic(NoahmpIO%ALBSNOWDIRXY, "ALBSNOWDIR" ,NoahmpIO%NTilesMax , "Snow albedo (direct)"             , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSNOWDIFXY, "ALBSNOWDIF" ,NoahmpIO%NTilesMax , "Snow albedo (diffuse)"            , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSFCDIRXY , "ALBSFCDIR"  ,NoahmpIO%NTilesMax , "Surface albedo (direct)"          , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSFCDIFXY , "ALBSFCDIF"  ,NoahmpIO%NTilesMax , "Surface albedo (diffuse)"         , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSOILDIRXY, "ALBSOILDIR"  ,NoahmpIO%NTilesMax , "Soil albedo (direct)"            , "-"              , "RADN")
    call add_to_output_mosaic(NoahmpIO%ALBSOILDIFXY, "ALBSOILDIF"  ,NoahmpIO%NTilesMax , "Soil albedo (diffuse)"           , "-"              , "RADN")
    ! Snow - 2D terms
    call add_to_output_mosaic(NoahmpIO%SNOWH      , "SNOWH"   ,NoahmpIO%NTilesMax , "Snow depth"                           , "m"                     )
    call add_to_output_mosaic(NoahmpIO%SNOW       , "SNEQV"   ,NoahmpIO%NTilesMax , "Snow water equivalent"                , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%QSNOWXY    , "QSNOW"   ,NoahmpIO%NTilesMax , "Snowfall rate on the ground"          , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%QRAINXY    , "QRAIN"   ,NoahmpIO%NTilesMax , "Rainfall rate on the ground"          , "mm/s"                  )
    call add_to_output_mosaic(NoahmpIO%ISNOWXY    , "ISNOW"   ,NoahmpIO%NTilesMax , "Number of snow layers"                , "-"                     )
    call add_to_output_mosaic(NoahmpIO%SNOWC      , "FSNO"    ,NoahmpIO%NTilesMax , "Snow-cover fraction on the ground"    , "-"                     )
    call add_to_output_mosaic(NoahmpIO%ACSNOW     , "ACSNOW"  ,NoahmpIO%NTilesMax , "accumulated snow fall"                , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%ACSNOM     , "ACSNOM"  ,NoahmpIO%NTilesMax , "accumulated snow melt water"          , "mm"                    )
    call add_to_output_mosaic(NoahmpIO%QSNBOTXY   , "QSNBOT"  ,NoahmpIO%NTilesMax , "water (melt+rain through) out of snow bottom"   , "mm/s"        )
    call add_to_output_mosaic(NoahmpIO%QMELTXY    , "QMELT"   ,NoahmpIO%NTilesMax , "snow melt due to phase change"                  , "mm/s"        )
    ! SNICAR snow albedo scheme
    if (NoahmpIO%IOPT_ALB == 3)then
      call add_to_output_mosaic(NoahmpIO%SNRDSXY    , "SNRDS"   ,NoahmpIO%NTilesMax , "Snow layer effective grain radius"    , "m-6"           , "SNOW")
      call add_to_output_mosaic(NoahmpIO%SNFRXY     , "SNFR"    ,NoahmpIO%NTilesMax , "Snow layer rate of freezing"          , "mm/s"          , "SNOW")
      call add_to_output_mosaic(NoahmpIO%BCPHIXY    , "BCPHI_Mass",NoahmpIO%NTilesMax ,"hydrophilic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%BCPHOXY    , "BCPHO_Mass",NoahmpIO%NTilesMax ,"hydrophobic BC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%OCPHIXY    , "OCPHI_Mass",NoahmpIO%NTilesMax ,"hydrophilic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%OCPHOXY    , "OCPHO_Mass",NoahmpIO%NTilesMax ,"hydrophobic OC mass in snow"         , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST1XY    , "DUST1_Mass",NoahmpIO%NTilesMax ,"dust size bin 1 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST2XY    , "DUST2_Mass",NoahmpIO%NTilesMax ,"dust size bin 2 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST3XY    , "DUST3_Mass",NoahmpIO%NTilesMax ,"dust size bin 3 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST4XY    , "DUST4_Mass",NoahmpIO%NTilesMax ,"dust size bin 4 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%DUST5XY    , "DUST5_Mass",NoahmpIO%NTilesMax ,"dust size bin 5 mass in snow"        , "kg/m2"         , "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcBCPHIXY, "BCPHI_MassConc",NoahmpIO%NTilesMax ,"hydrophilic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcBCPHOXY, "BCPHO_MassConc",NoahmpIO%NTilesMax ,"hydrophobic BC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcOCPHIXY, "OCPHI_MassConc",NoahmpIO%NTilesMax ,"hydrophilic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcOCPHOXY, "OCPHO_MassConc",NoahmpIO%NTilesMax ,"hydrophobic OC mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST1XY, "DUST1_MassConc",NoahmpIO%NTilesMax ,"dust size bin 1 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST2XY, "DUST2_MassConc",NoahmpIO%NTilesMax ,"dust size bin 2 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST3XY, "DUST3_MassConc",NoahmpIO%NTilesMax ,"dust size bin 3 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST4XY, "DUST4_MassConc",NoahmpIO%NTilesMax ,"dust size bin 4 mass concentration in snow", "kg/kg", "SNOW")
      call add_to_output_mosaic(NoahmpIO%MassConcDUST5XY, "DUST5_MassConc",NoahmpIO%NTilesMax ,"dust size bin 5 mass concentration in snow", "kg/kg", "SNOW")
    endif
    ! Exchange coefficients
    call add_to_output_mosaic(NoahmpIO%CMXY       , "CM"      ,NoahmpIO%NTilesMax , "Momentum drag coefficient"            , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHXY       , "CH"      ,NoahmpIO%NTilesMax , "Sensible heat exchange coefficient"   , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHVXY      , "CHV"     ,NoahmpIO%NTilesMax , "Exchange coefficient vegetated"       , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHBXY      , "CHB"     ,NoahmpIO%NTilesMax , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHLEAFXY   , "CHLEAF"  ,NoahmpIO%NTilesMax , "Exchange coefficient leaf"            , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHUCXY     , "CHUC"    ,NoahmpIO%NTilesMax , "Exchange coefficient bare"            , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHV2XY     , "CHV2"    ,NoahmpIO%NTilesMax , "Exchange coefficient 2-m vegetated"   , "m/s"                   )
    call add_to_output_mosaic(NoahmpIO%CHB2XY     , "CHB2"    ,NoahmpIO%NTilesMax , "Exchange coefficient 2-m bare"        , "m/s"                   )
    ! Carbon allocation model
    call add_to_output_mosaic(NoahmpIO%LFMASSXY   , "LFMASS"  ,NoahmpIO%NTilesMax , "Leaf mass"                            , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%RTMASSXY   , "RTMASS"  ,NoahmpIO%NTilesMax , "Mass of fine roots"                   , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%STMASSXY   , "STMASS"  ,NoahmpIO%NTilesMax , "Stem mass"                            , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%WOODXY     , "WOOD"    ,NoahmpIO%NTilesMax , "Mass of wood and woody roots"         , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GRAINXY    , "GRAIN"   ,NoahmpIO%NTilesMax , "Mass of grain"                        , "g/m2"                  )
    call add_to_output_mosaic(NoahmpIO%GDDXY      , "GDD"     ,NoahmpIO%NTilesMax , "Growing degree days "                 , "-"                     )
    call add_to_output_mosaic(NoahmpIO%STBLCPXY   , "STBLCP"  ,NoahmpIO%NTilesMax , "Stable carbon in deep soil"           , "gC/m2"                 )
    call add_to_output_mosaic(NoahmpIO%FASTCPXY   , "FASTCP"  ,NoahmpIO%NTilesMax , "Short-lived carbon in shallow soil"   , "gC/m2"                 )
    call add_to_output_mosaic(NoahmpIO%NEEXY      , "NEE"     ,NoahmpIO%NTilesMax , "Net ecosystem exchange"               , "gCO2/m2/s"             )
    call add_to_output_mosaic(NoahmpIO%GPPXY      , "GPP"     ,NoahmpIO%NTilesMax , "Net instantaneous assimilation"       , "gC/m2/s"               )
    call add_to_output_mosaic(NoahmpIO%NPPXY      , "NPP"     ,NoahmpIO%NTilesMax , "Net primary productivity"             , "gC/m2/s"               )
    call add_to_output_mosaic(NoahmpIO%PSNXY      , "PSN"     ,NoahmpIO%NTilesMax , "Total photosynthesis"                 , "umol CO2/m2/s"         )
    call add_to_output_mosaic(NoahmpIO%APARXY     , "APAR"    ,NoahmpIO%NTilesMax , "Photosynthesis active energy by canopy", "W/m2"                 )
    ! additional NoahMP output
    if (NoahmpIO%noahmp_output > 0) then
    ! additional water budget terms
      call add_to_output_mosaic(NoahmpIO%QINTSXY    , "QINTS"   ,NoahmpIO%NTilesMax , "canopy interception (loading) rate for snowfall", "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QINTRXY    , "QINTR"   ,NoahmpIO%NTilesMax , "canopy interception rate for rain"              , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QDRIPSXY   , "QDRIPS"  ,NoahmpIO%NTilesMax , "drip (unloading) rate for intercepted snow"     , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QDRIPRXY   , "QDRIPR"  ,NoahmpIO%NTilesMax , "drip rate for canopy intercepted rain"          , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QTHROSXY   , "QTHROS"  ,NoahmpIO%NTilesMax , "throughfall of snowfall"                        , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QTHRORXY   , "QTHROR"  ,NoahmpIO%NTilesMax , "throughfall for rain"                           , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QSNSUBXY   , "QSNSUB"  ,NoahmpIO%NTilesMax , "snow surface sublimation rate"                  , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QSNFROXY   , "QSNFRO"  ,NoahmpIO%NTilesMax , "snow surface frost rate"                        , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QSUBCXY    , "QSUBC"   ,NoahmpIO%NTilesMax , "canopy snow sublimation rate"                   , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QFROCXY    , "QFROC"   ,NoahmpIO%NTilesMax , "canopy snow frost rate"                         , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QEVACXY    , "QEVAC"   ,NoahmpIO%NTilesMax , "canopy snow evaporation rate"                   , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QDEWCXY    , "QDEWC"   ,NoahmpIO%NTilesMax , "canopy snow dew rate"                           , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QFRZCXY    , "QFRZC"   ,NoahmpIO%NTilesMax , "refreezing rate of canopy liquid water"         , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%QMELTCXY   , "QMELTC"  ,NoahmpIO%NTilesMax , "melting rate of canopy snow"                    , "mm/s"        )
      call add_to_output_mosaic(NoahmpIO%FPICEXY    , "FPICE"   ,NoahmpIO%NTilesMax , "snow fraction in precipitation"                 , "-"           )
      call add_to_output_mosaic(NoahmpIO%ACC_QINSURXY,"ACC_QINSUR",NoahmpIO%NTilesMax , "accumuated water flux to soil within soil timestep"     , "m/s*dt_soil/dt_main")
      call add_to_output_mosaic(NoahmpIO%ACC_QSEVAXY ,"ACC_QSEVA" ,NoahmpIO%NTilesMax , "accumulated soil surface evap rate within soil timestep", "m/s*dt_soil/dt_main")
      call add_to_output_mosaic(NoahmpIO%ACC_ETRANIXY,"ACC_ETRANI",NoahmpIO%NTilesMax , "accumualted transpiration rate within soil timestep"    , "m/s*dt_soil/dt_main","SOIL")
      call add_to_output_mosaic(NoahmpIO%ACC_DWATERXY,"ACC_DWATER",NoahmpIO%NTilesMax , "accumulated water storage change within soil timestep"  , "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_PRCPXY  ,"ACC_PRCP"  ,NoahmpIO%NTilesMax , "accumulated precipitation within soil timestep"         , "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_ECANXY  ,"ACC_ECAN"  ,NoahmpIO%NTilesMax , "accumulated net canopy evaporation within soil timestep", "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_ETRANXY ,"ACC_ETRAN" ,NoahmpIO%NTilesMax , "accumulated transpiration within soil timestep"         , "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_EDIRXY  ,"ACC_EDIR"  ,NoahmpIO%NTilesMax , "accumulated net ground evaporation within soil timestep", "mm")
      call add_to_output_mosaic(NoahmpIO%ACC_GLAFLWXY,"ACC_GLAFLW",NoahmpIO%NTilesMax , "accumuated glacier excessive flow per soil timestep"    , "mm")
      ! additional energy terms
      call add_to_output_mosaic(NoahmpIO%PAHXY      , "PAH"     ,NoahmpIO%NTilesMax , "Precipitation advected heat flux"                         , "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%PAHGXY     , "PAHG"    ,NoahmpIO%NTilesMax , "Precipitation advected heat flux to below-canopy ground"  , "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%PAHBXY     , "PAHB"    ,NoahmpIO%NTilesMax , "Precipitation advected heat flux to bare ground"          , "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%PAHVXY     , "PAHV"    ,NoahmpIO%NTilesMax , "Precipitation advected heat flux to canopy"               , "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%ACC_SSOILXY, "ACC_SSOIL",NoahmpIO%NTilesMax ,"accumulated heat flux into snow/soil within soil timestep", "W/m2"    )
      call add_to_output_mosaic(NoahmpIO%EFLXBXY    , "EFLXB"   ,NoahmpIO%NTilesMax , "accumulated heat flux through soil bottom"                , "J/m2"    )
      call add_to_output_mosaic(NoahmpIO%SOILENERGY , "SOILENERGY",NoahmpIO%NTilesMax ,"energy content in soil relative to 273.16"               , "KJ/m2"   )
      call add_to_output_mosaic(NoahmpIO%SNOWENERGY , "SNOWENERGY",NoahmpIO%NTilesMax ,"energy content in snow relative to 273.16"               , "KJ/m2"   )
      call add_to_output_mosaic(NoahmpIO%CANHSXY    , "CANHS"   ,NoahmpIO%NTilesMax , "canopy heat storage change"                               , "W/m2"    )
      ! additional forcing terms
      call add_to_output_mosaic(NoahmpIO%RAINLSM    , "RAINLSM" ,NoahmpIO%NTilesMax , "lowest model liquid precipitation into LSM"               , "mm/s"    )
      call add_to_output_mosaic(NoahmpIO%SNOWLSM    , "SNOWLSM" ,NoahmpIO%NTilesMax , "lowest model snowfall into LSM"                           , "mm/s"    )
      call add_to_output_mosaic(NoahmpIO%FORCTLSM   , "FORCTLSM",NoahmpIO%NTilesMax , "lowest model temperature into LSM"                        , "K"       ) 
      call add_to_output_mosaic(NoahmpIO%FORCQLSM   , "FORCQLSM",NoahmpIO%NTilesMax , "lowest model specific humidty into LSM"                   , "kg/kg"   )
      call add_to_output_mosaic(NoahmpIO%FORCPLSM   , "FORCPLSM",NoahmpIO%NTilesMax , "lowest model pressure into LSM"                           , "Pa"      )
      call add_to_output_mosaic(NoahmpIO%FORCZLSM   , "FORCZLSM",NoahmpIO%NTilesMax , "lowest model forcing height into LSM"                     , "m"       )
      call add_to_output_mosaic(NoahmpIO%FORCWLSM   , "FORCWLSM",NoahmpIO%NTilesMax , "lowest model wind speed into LSM"                         , "m/s"     )
      call add_to_output_mosaic(NoahmpIO%RadSwVisFrac , "SWVISFRAC",NoahmpIO%NTilesMax , "Fraction of visible band downward solar radiation", "-"      )
      call add_to_output_mosaic(NoahmpIO%RadSwDirFrac , "SWDIRFRAC",NoahmpIO%NTilesMax , "Fraction of downward solar direct radiation",   "-"          )
    endif

    ! Irrigation
    if ( NoahmpIO%IOPT_IRR > 0 ) then
      call add_to_output_mosaic(NoahmpIO%IRNUMSI    , "IRNUMSI" ,NoahmpIO%NTilesMax , "Sprinkler irrigation count"                               , "-"       )
      call add_to_output_mosaic(NoahmpIO%IRNUMMI    , "IRNUMMI" ,NoahmpIO%NTilesMax , "Micro irrigation count"                                   , "-"       )
      call add_to_output_mosaic(NoahmpIO%IRNUMFI    , "IRNUMFI" ,NoahmpIO%NTilesMax , "Flood irrigation count"                                   , "-"       )
      call add_to_output_mosaic(NoahmpIO%IRELOSS    , "IRELOSS" ,NoahmpIO%NTilesMax , "Accumulated sprinkler Evaporation"                        , "mm"      )
      call add_to_output_mosaic(NoahmpIO%IRSIVOL    , "IRSIVOL" ,NoahmpIO%NTilesMax , "Sprinkler irrigation amount"                              , "mm"      )
      call add_to_output_mosaic(NoahmpIO%IRMIVOL    , "IRMIVOL" ,NoahmpIO%NTilesMax , "Micro irrigation amount"                                  , "mm"      )
      call add_to_output_mosaic(NoahmpIO%IRFIVOL    , "IRFIVOL" ,NoahmpIO%NTilesMax , "Flood irrigation amount"                                  , "mm"      )
      call add_to_output_mosaic(NoahmpIO%IRRSPLH    , "IRRSPLH" ,NoahmpIO%NTilesMax , "Accumulated latent heating due to sprinkler"              , "J/m2"    )
    endif
    ! MMF groundwater  model
    if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
      call add_to_output_mosaic(NoahmpIO%SMCWTDXY   , "SMCWTD"   ,NoahmpIO%NTilesMax , "soil water content between bottom of the soil and water table", "m3/m3"  )
      call add_to_output_mosaic(NoahmpIO%RECHXY     , "RECH"     ,NoahmpIO%NTilesMax , "recharge to or from the water table when shallow"             , "m"      )
      call add_to_output_mosaic(NoahmpIO%DEEPRECHXY , "DEEPRECH" ,NoahmpIO%NTilesMax , "recharge to or from the water table when deep"                , "m"      )
      call add_to_output(NoahmpIO%QRFSXY     , "QRFS"     , "accumulated groundwater baselow"                              , "mm"     )
      call add_to_output(NoahmpIO%QRFXY      , "QRF"      , "groundwater baseflow"                                         , "m"      )
      call add_to_output(NoahmpIO%QSPRINGSXY , "QSPRINGS" , "accumulated seeping water"                                    , "mm"     )
      call add_to_output(NoahmpIO%QSPRINGXY  , "QSPRING"  , "instantaneous seeping water"                                  , "m"      )
      call add_to_output(NoahmpIO%QSLATXY    , "QSLAT"    , "accumulated lateral flow"                                     , "mm"     )
      call add_to_output(NoahmpIO%QLATXY     , "QLAT"     , "instantaneous lateral flow"                                   , "m"      )
    endif
    ! Wetland model
    if ( NoahmpIO%IOPT_WETLAND > 0 ) then
      call add_to_output_mosaic(NoahmpIO%FSATXY     , "FSAT"    ,NoahmpIO%NTilesMax , "saturated fraction of the grid"                           , "-" )
      call add_to_output_mosaic(NoahmpIO%WSURFXY    , "WSURF"   ,NoahmpIO%NTilesMax , "Wetland Water Storage"                                    , "mm")
    endif

  end subroutine WriteNoahmpMosaicSubgridOutput

end module WriteNoahmpMosaicSubgridOutputMod