module ReadNoahmpRestartMod

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

  subroutine ReadNoahmpRestart (NoahmpIO)
 
    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO

    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SOIL_T"  , NoahmpIO%TSLB     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNOW_T"  , NoahmpIO%TSNOXY   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMC"     , NoahmpIO%SMOIS    )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SH2O"    , NoahmpIO%SH2O     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ZSNSO"   , NoahmpIO%ZSNSOXY  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNICE"   , NoahmpIO%SNICEXY  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNLIQ"   , NoahmpIO%SNLIQXY  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FWET"    , NoahmpIO%FWETXY   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNEQVO"  , NoahmpIO%SNEQVOXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "EAH"     , NoahmpIO%EAHXY    )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TAH"     , NoahmpIO%TAHXY    )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ALBOLD"  , NoahmpIO%ALBOLDXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CM"      , NoahmpIO%CMXY     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CH"      , NoahmpIO%CHXY     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ISNOW"   , NoahmpIO%ISNOWXY  ) 
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CANLIQ"  , NoahmpIO%CANLIQXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CANICE"  , NoahmpIO%CANICEXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNEQV"   , NoahmpIO%SNOW     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNOWH"   , NoahmpIO%SNOWH    )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TV"      , NoahmpIO%TVXY     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TG"      , NoahmpIO%TGXY     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ZWT"     , NoahmpIO%ZWTXY    )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WA"      , NoahmpIO%WAXY     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WT"      , NoahmpIO%WTXY     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WSLAKE"  , NoahmpIO%WSLAKEXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "LFMASS"  , NoahmpIO%LFMASSXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RTMASS"  , NoahmpIO%RTMASSXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "STMASS"  , NoahmpIO%STMASSXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CROPCAT" , NoahmpIO%CROPCAT  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WOOD"    , NoahmpIO%WOODXY   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "GRAIN"   , NoahmpIO%GRAINXY  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "GDD"     , NoahmpIO%GDDXY    )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "STBLCP"  , NoahmpIO%STBLCPXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FASTCP"  , NoahmpIO%FASTCPXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "LAI"     , NoahmpIO%LAI      )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SAI"     , NoahmpIO%XSAIXY   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "VEGFRA"  , NoahmpIO%VEGFRA   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "GVFMIN"  , NoahmpIO%GVFMIN   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "GVFMAX"  , NoahmpIO%GVFMAX   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACMELT"  , NoahmpIO%ACSNOM   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACSNOW"  , NoahmpIO%ACSNOW   )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TAUSS"   , NoahmpIO%TAUSSXY  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSFC"    , NoahmpIO%QSFC     )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SFCRUNOFF",NoahmpIO%SFCRUNOFF)
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "UDRUNOFF" ,NoahmpIO%UDRUNOFF )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QTDRAIN"  ,NoahmpIO%QTDRAIN  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_SSOIL" , NoahmpIO%ACC_SSOILXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_QINSUR", NoahmpIO%ACC_QINSURXY)
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_QSEVA" , NoahmpIO%ACC_QSEVAXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_ETRANI", NoahmpIO%ACC_ETRANIXY)
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_DWATER", NoahmpIO%ACC_DWATERXY)
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_PRCP"  , NoahmpIO%ACC_PRCPXY  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_ECAN"  , NoahmpIO%ACC_ECANXY  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_ETRAN" , NoahmpIO%ACC_ETRANXY )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_EDIR"  , NoahmpIO%ACC_EDIRXY  )
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACC_GLAFLW", NoahmpIO%ACC_GLAFLWXY)
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ALBSOILDIR", NoahmpIO%ALBSOILDIRXY)
    call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ALBSOILDIF", NoahmpIO%ALBSOILDIFXY)

    ! below for SNICAR snow albedo scheme
    if (NoahmpIO%IOPT_ALB == 3)then
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNRDS"   , NoahmpIO%SNRDSXY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNFR"    , NoahmpIO%SNFRXY   )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "BCPHI"   , NoahmpIO%BCPHIXY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "BCPHO"   , NoahmpIO%BCPHOXY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "OCPHI"   , NoahmpIO%OCPHIXY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "OCPHO"   , NoahmpIO%OCPHOXY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DUST1"   , NoahmpIO%DUST1XY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DUST2"   , NoahmpIO%DUST2XY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DUST3"   , NoahmpIO%DUST3XY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DUST4"   , NoahmpIO%DUST4XY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DUST5"   , NoahmpIO%DUST5XY  )
    endif

    ! below for irrigation scheme
    if ( NoahmpIO%IOPT_IRR > 0 ) then
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRNUMSI" , NoahmpIO%IRNUMSI  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRNUMMI" , NoahmpIO%IRNUMMI  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRNUMFI" , NoahmpIO%IRNUMFI  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRWATSI" , NoahmpIO%IRWATSI  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRWATMI" , NoahmpIO%IRWATMI  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRWATFI" , NoahmpIO%IRWATFI  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRSIVOL" , NoahmpIO%IRSIVOL  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRMIVOL" , NoahmpIO%IRMIVOL  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRFIVOL" , NoahmpIO%IRFIVOL  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRELOSS" , NoahmpIO%IRELOSS  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "IRRSPLH" , NoahmpIO%IRRSPLH  )
    endif

    ! below for MMF groundwater scheme
    if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMOISEQ"   , NoahmpIO%SMOISEQ    )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "AREAXY"    , NoahmpIO%AREAXY     )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMCWTDXY"  , NoahmpIO%SMCWTDXY   )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QRFXY"     , NoahmpIO%QRFXY      )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DEEPRECHXY", NoahmpIO%DEEPRECHXY )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSPRINGXY" , NoahmpIO%QSPRINGXY  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSLATXY"   , NoahmpIO%QSLATXY    )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QRFSXY"    , NoahmpIO%QRFSXY     )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSPRINGSXY", NoahmpIO%QSPRINGSXY )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RECHXY"    , NoahmpIO%RECHXY     )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FDEPTHXY"   ,NoahmpIO%FDEPTHXY   )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RIVERCONDXY",NoahmpIO%RIVERCONDXY)
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RIVERBEDXY" ,NoahmpIO%RIVERBEDXY )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "EQZWT"      ,NoahmpIO%EQZWT      )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PEXPXY"     ,NoahmpIO%PEXPXY     )
    endif

    ! for wetland scheme
    if ( NoahmpIO%IOPT_WETLAND > 0 ) then
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FSATXY"     , NoahmpIO%FSATXY    )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WSURFXY"    , NoahmpIO%WSURFXY   )
    endif

    ! below for urban model
    if ( NoahmpIO%SF_URBAN_PHYSICS > 0 ) then
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "SH_URB2D"  ,     NoahmpIO%SH_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "LH_URB2D"  ,     NoahmpIO%LH_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,       "G_URB2D"  ,      NoahmpIO%G_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "RN_URB2D"  ,     NoahmpIO%RN_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "TS_URB2D"  ,     NoahmpIO%TS_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "FRC_URB2D"  ,    NoahmpIO%FRC_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "UTYPE_URB2D"  ,  NoahmpIO%UTYPE_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "LP_URB2D"  ,     NoahmpIO%LP_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "LB_URB2D"  ,     NoahmpIO%LB_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "HGT_URB2D"  ,    NoahmpIO%HGT_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "MH_URB2D"  ,     NoahmpIO%MH_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "STDH_URB2D"  ,   NoahmpIO%STDH_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "HI_URB2D"  ,     NoahmpIO%HI_URB2D  )
      call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "LF_URB2D"  ,     NoahmpIO%LF_URB2D  )

      if ( NoahmpIO%SF_URBAN_PHYSICS == 1 ) then  ! single layer urban model
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "CMR_SFCDIF" ,    NoahmpIO%CMR_SFCDIF )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "CHR_SFCDIF" ,    NoahmpIO%CHR_SFCDIF )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "CMC_SFCDIF" ,    NoahmpIO%CMC_SFCDIF )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "CHC_SFCDIF" ,    NoahmpIO%CHC_SFCDIF )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "CMGR_SFCDIF" ,   NoahmpIO%CMGR_SFCDIF )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "CHGR_SFCDIF" ,   NoahmpIO%CHGR_SFCDIF )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "TR_URB2D"  ,     NoahmpIO%TR_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "TB_URB2D"  ,     NoahmpIO%TB_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "TG_URB2D"  ,     NoahmpIO%TG_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "TC_URB2D"  ,     NoahmpIO%TC_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "QC_URB2D"  ,     NoahmpIO%QC_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "UC_URB2D"  ,     NoahmpIO%UC_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "XXXR_URB2D"  ,   NoahmpIO%XXXR_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "XXXB_URB2D"  ,   NoahmpIO%XXXB_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "XXXG_URB2D"  ,   NoahmpIO%XXXG_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "XXXC_URB2D"  ,   NoahmpIO%XXXC_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TRL_URB3D"  ,    NoahmpIO%TRL_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TBL_URB3D"  ,    NoahmpIO%TBL_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TGL_URB3D"  ,    NoahmpIO%TGL_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "CMCR_URB2D"  ,   NoahmpIO%CMCR_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TGR_URB2D"  ,    NoahmpIO%TGR_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "TGRL_URB3D"  ,   NoahmpIO%TGRL_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "SMR_URB3D"  ,    NoahmpIO%SMR_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "DRELR_URB2D"  ,  NoahmpIO%DRELR_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "DRELB_URB2D"  ,  NoahmpIO%DRELB_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "DRELG_URB2D"  ,  NoahmpIO%DRELG_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FLXHUMR_URB2D"  ,NoahmpIO%FLXHUMR_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FLXHUMB_URB2D"  ,NoahmpIO%FLXHUMB_URB2D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FLXHUMG_URB2D"  ,NoahmpIO%FLXHUMG_URB2D  )
      endif

      if ( (NoahmpIO%SF_URBAN_PHYSICS == 2) .or. (NoahmpIO%SF_URBAN_PHYSICS == 3) ) then  ! BEP or BEM urban models
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TRB_URB4D"  ,    NoahmpIO%TRB_URB4D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TW1_URB4D"  ,    NoahmpIO%TW1_URB4D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TW2_URB4D"  ,    NoahmpIO%TW2_URB4D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TGB_URB4D"  ,    NoahmpIO%TGB_URB4D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "SFW1_URB3D"  ,   NoahmpIO%SFW1_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "SFW2_URB3D"  ,   NoahmpIO%SFW2_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "SFR_URB3D"  ,    NoahmpIO%SFR_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "SFG_URB3D"  ,    NoahmpIO%SFG_URB3D  )
      endif

      if ( NoahmpIO%SF_URBAN_PHYSICS == 3 ) then  ! BEM urban model        
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "TLEV_URB3D"  ,   NoahmpIO%TLEV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "QLEV_URB3D"  ,   NoahmpIO%QLEV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,  "TW1LEV_URB3D"  , NoahmpIO%TW1LEV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,  "TW2LEV_URB3D"  , NoahmpIO%TW2LEV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "TGLEV_URB3D"  ,  NoahmpIO%TGLEV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "TFLEV_URB3D"  ,  NoahmpIO%TFLEV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "SF_AC_URB3D"  ,  NoahmpIO%SF_AC_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "LF_AC_URB3D"  ,  NoahmpIO%LF_AC_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "CM_AC_URB3D"  ,  NoahmpIO%CM_AC_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,  "SFVENT_URB3D"  , NoahmpIO%SFVENT_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,  "LFVENT_URB3D"  , NoahmpIO%LFVENT_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,  "SFWIN1_URB3D"  , NoahmpIO%SFWIN1_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,  "SFWIN2_URB3D"  , NoahmpIO%SFWIN2_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "EP_PV_URB3D"  ,  NoahmpIO%EP_PV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "T_PV_URB3D"  ,   NoahmpIO%T_PV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TRV_URB4D"  ,    NoahmpIO%TRV_URB4D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "QR_URB4D"  ,     NoahmpIO%QR_URB4D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "QGR_URB3D"  ,    NoahmpIO%QGR_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "TGR_URB3D"  ,    NoahmpIO%TGR_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,   "DRAIN_URB4D"  ,  NoahmpIO%DRAIN_URB4D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DRAINGR_URB3D"  ,NoahmpIO%DRAINGR_URB3D  ) 
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "SFRV_URB3D"  ,   NoahmpIO%SFRV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,    "LFRV_URB3D"  ,   NoahmpIO%LFRV_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "DGR_URB3D"  ,    NoahmpIO%DGR_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,      "DG_URB3D"  ,     NoahmpIO%DG_URB3D  )
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "LFR_URB3D"  ,    NoahmpIO%LFR_URB3D  ) 
          call get_from_restart(xstart, xend, xstart, ixfull, jxfull,     "LFG_URB3D"  ,    NoahmpIO%LFG_URB3D  )
      endif
    endif

  end subroutine ReadNoahmpRestart

end module ReadNoahmpRestartMod