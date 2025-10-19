module WriteNoahmpMosaicRestartMod

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

  subroutine WriteNoahmpMosaicRestart (NoahmpIO)
 
    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO

    call add_to_restart_mosaic(NoahmpIO%TSLB      , "SOIL_T", NoahmpIO%NTilesMax, layers="SOIL")
    call add_to_restart_mosaic(NoahmpIO%TSNOXY    , "SNOW_T", NoahmpIO%NTilesMax, layers="SNOW")
    call add_to_restart_mosaic(NoahmpIO%SMOIS     , "SMC"   , NoahmpIO%NTilesMax, layers="SOIL")
    call add_to_restart_mosaic(NoahmpIO%SH2O      , "SH2O"  , NoahmpIO%NTilesMax, layers="SOIL")
    call add_to_restart_mosaic(NoahmpIO%ZSNSOXY   , "ZSNSO" , NoahmpIO%NTilesMax, layers="SOSN")
    call add_to_restart_mosaic(NoahmpIO%SNICEXY   , "SNICE" , NoahmpIO%NTilesMax, layers="SNOW")
    call add_to_restart_mosaic(NoahmpIO%SNLIQXY   , "SNLIQ" , NoahmpIO%NTilesMax, layers="SNOW")  
    call add_to_restart_mosaic(NoahmpIO%FWETXY    , "FWET"  , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%SNEQVOXY  , "SNEQVO", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%EAHXY     , "EAH"   , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%TAHXY     , "TAH"   , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ALBOLDXY  , "ALBOLD", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%CMXY      , "CM"    , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%CHXY      , "CH"    , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ISNOWXY   , "ISNOW" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%CANLIQXY  , "CANLIQ", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%CANICEXY  , "CANICE", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%SNOW      , "SNEQV" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%SNOWH     , "SNOWH" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%TVXY      , "TV"    , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%TGXY      , "TG"    , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ZWTXY     , "ZWT"   , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%WAXY      , "WA"    , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%WTXY      , "WT"    , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%WSLAKEXY  , "WSLAKE", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%LFMASSXY  , "LFMASS", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%RTMASSXY  , "RTMASS", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%STMASSXY  , "STMASS", NoahmpIO%NTilesMax)

    call add_to_restart(NoahmpIO%CROPCAT   , "CROPCAT" )

    call add_to_restart_mosaic(NoahmpIO%WOODXY    , "WOOD"  , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%GRAINXY   , "GRAIN" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%GDDXY     , "GDD"   , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%STBLCPXY  , "STBLCP", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%FASTCPXY  , "FASTCP", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%LAI       , "LAI"   , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%XSAIXY    , "SAI"   , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%VEGFRA    , "VEGFRA", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%GVFMIN    , "GVFMIN", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%GVFMAX    , "GVFMAX", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACSNOM    , "ACMELT", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACSNOW    , "ACSNOW", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%TAUSSXY   , "TAUSS" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%QSFC      , "QSFC"  , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%SFCRUNOFF , "SFCRUNOFF", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%UDRUNOFF  , "UDRUNOFF" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%QTDRAIN   , "QTDRAIN"  , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_SSOILXY ,"ACC_SSOIL" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_QINSURXY,"ACC_QINSUR", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_QSEVAXY ,"ACC_QSEVA" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_ETRANIXY,"ACC_ETRANI", NoahmpIO%NTilesMax, layers="SOIL")
    call add_to_restart_mosaic(NoahmpIO%ACC_DWATERXY,"ACC_DWATER", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_PRCPXY  ,"ACC_PRCP"  , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_ECANXY  ,"ACC_ECAN"  , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_ETRANXY ,"ACC_ETRAN" , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_EDIRXY  ,"ACC_EDIR"  , NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ACC_GLAFLWXY,"ACC_GLAFLW", NoahmpIO%NTilesMax)
    call add_to_restart_mosaic(NoahmpIO%ALBSOILDIRXY, "ALBSOILDIR", NoahmpIO%NTilesMax, layers="RADN")
    call add_to_restart_mosaic(NoahmpIO%ALBSOILDIFXY, "ALBSOILDIF", NoahmpIO%NTilesMax, layers="RADN")

    ! SNICAR snow albedo scheme
    if (NoahmpIO%IOPT_ALB == 3)then
      call add_to_restart_mosaic(NoahmpIO%SNFRXY    , "SNFR"  , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%SNRDSXY   , "SNRDS" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%BCPHIXY   , "BCPHI" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%BCPHOXY   , "BCPHO" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%OCPHIXY   , "OCPHI" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%OCPHOXY   , "OCPHO" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%DUST1XY   , "DUST1" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%DUST2XY   , "DUST2" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%DUST3XY   , "DUST3" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%DUST4XY   , "DUST4" , NoahmpIO%NTilesMax, layers="SNOW")
      call add_to_restart_mosaic(NoahmpIO%DUST5XY   , "DUST5" , NoahmpIO%NTilesMax, layers="SNOW")
    endif

  ! irrigation scheme
    if ( NoahmpIO%IOPT_IRR > 0 ) then
      call add_to_restart_mosaic(NoahmpIO%IRNUMSI   , "IRNUMSI", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRNUMMI   , "IRNUMMI", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRNUMFI   , "IRNUMFI", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRWATSI   , "IRWATSI", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRWATMI   , "IRWATMI", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRWATFI   , "IRWATFI", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRSIVOL   , "IRSIVOL", NoahmpIO%NTilesMax) 
      call add_to_restart_mosaic(NoahmpIO%IRMIVOL   , "IRMIVOL", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRFIVOL   , "IRFIVOL", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRELOSS   , "IRELOSS", NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%IRRSPLH   , "IRRSPLH", NoahmpIO%NTilesMax)
    endif

    ! below for MMF groundwater scheme
    if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
      call add_to_restart_mosaic(NoahmpIO%SMOISEQ     , "SMOISEQ"    , NoahmpIO%NTilesMax, layers="SOIL"  )
      call add_to_restart_mosaic(NoahmpIO%SMCWTDXY    , "SMCWTDXY"   , NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%DEEPRECHXY  , "DEEPRECHXY" , NoahmpIO%NTilesMax)

      ! following are grid values only from input. So NO mosaic for now
      call add_to_restart(NoahmpIO%AREAXY      , "AREAXY"     )
      call add_to_restart(NoahmpIO%QSLATXY     , "QSLATXY"    )
      call add_to_restart(NoahmpIO%QRFSXY      , "QRFSXY"     )
      call add_to_restart(NoahmpIO%QSPRINGSXY  , "QSPRINGSXY" )
      call add_to_restart(NoahmpIO%RECHXY      , "RECHXY"     )
      call add_to_restart(NoahmpIO%QRFXY       , "QRFXY"      )
      call add_to_restart(NoahmpIO%QSPRINGXY   , "QSPRINGXY"  )
      call add_to_restart(NoahmpIO%FDEPTHXY    , "FDEPTHXY"   )
      call add_to_restart(NoahmpIO%RIVERCONDXY , "RIVERCONDXY")
      call add_to_restart(NoahmpIO%RIVERBEDXY  , "RIVERBEDXY" )
      call add_to_restart(NoahmpIO%EQZWT       , "EQZWT"      )
      call add_to_restart(NoahmpIO%PEXPXY      , "PEXPXY"     )
    endif

    ! for wetland scheme
    if ( NoahmpIO%IOPT_WETLAND > 0 ) then
      call add_to_restart_mosaic(NoahmpIO%FSATXY      , "FSATXY"     , NoahmpIO%NTilesMax)
      call add_to_restart_mosaic(NoahmpIO%WSURFXY     , "WSURFXY"    , NoahmpIO%NTilesMax)
    endif

    ! below for urban model
    if ( NoahmpIO%SF_URBAN_PHYSICS > 0 ) then
        call add_to_restart(     NoahmpIO%SH_URB2D  ,      "SH_URB2D" )
        call add_to_restart(     NoahmpIO%LH_URB2D  ,      "LH_URB2D" )
        call add_to_restart(      NoahmpIO%G_URB2D  ,       "G_URB2D" )
        call add_to_restart(     NoahmpIO%RN_URB2D  ,      "RN_URB2D" )
        call add_to_restart(     NoahmpIO%TS_URB2D  ,      "TS_URB2D" )
        call add_to_restart(    NoahmpIO%FRC_URB2D  ,     "FRC_URB2D" )
        call add_to_restart(  NoahmpIO%UTYPE_URB2D  ,   "UTYPE_URB2D" )
        call add_to_restart(     NoahmpIO%LP_URB2D  ,      "LP_URB2D" )
        call add_to_restart(     NoahmpIO%LB_URB2D  ,      "LB_URB2D" )
        call add_to_restart(    NoahmpIO%HGT_URB2D  ,     "HGT_URB2D" ) 
        call add_to_restart(     NoahmpIO%MH_URB2D  ,      "MH_URB2D" )
        call add_to_restart(   NoahmpIO%STDH_URB2D  ,    "STDH_URB2D" )
        call add_to_restart(     NoahmpIO%HI_URB2D  ,      "HI_URB2D", layers="URBN")
        call add_to_restart(     NoahmpIO%LF_URB2D  ,      "LF_URB2D", layers="URBN")
          
        if ( NoahmpIO%SF_URBAN_PHYSICS == 1 ) then  ! single layer urban model  
          call add_to_restart(    NoahmpIO%CMR_SFCDIF ,     "CMR_SFCDIF" )
          call add_to_restart(    NoahmpIO%CHR_SFCDIF ,     "CHR_SFCDIF" )
          call add_to_restart(    NoahmpIO%CMC_SFCDIF ,     "CMC_SFCDIF" )
          call add_to_restart(    NoahmpIO%CHC_SFCDIF ,     "CHC_SFCDIF" )
          call add_to_restart(   NoahmpIO%CMGR_SFCDIF ,    "CMGR_SFCDIF" )
          call add_to_restart(   NoahmpIO%CHGR_SFCDIF ,    "CHGR_SFCDIF" )
          call add_to_restart(     NoahmpIO%TR_URB2D  ,      "TR_URB2D" )
          call add_to_restart(     NoahmpIO%TB_URB2D  ,      "TB_URB2D" )
          call add_to_restart(     NoahmpIO%TG_URB2D  ,      "TG_URB2D" )
          call add_to_restart(     NoahmpIO%TC_URB2D  ,      "TC_URB2D" )
          call add_to_restart(     NoahmpIO%QC_URB2D  ,      "QC_URB2D" )
          call add_to_restart(     NoahmpIO%UC_URB2D  ,      "UC_URB2D" )
          call add_to_restart(   NoahmpIO%XXXR_URB2D  ,    "XXXR_URB2D" )
          call add_to_restart(   NoahmpIO%XXXB_URB2D  ,    "XXXB_URB2D" )
          call add_to_restart(   NoahmpIO%XXXG_URB2D  ,    "XXXG_URB2D" )
          call add_to_restart(   NoahmpIO%XXXC_URB2D  ,    "XXXC_URB2D" )
          call add_to_restart(    NoahmpIO%TRL_URB3D  ,     "TRL_URB3D", layers="SOIL" )
          call add_to_restart(    NoahmpIO%TBL_URB3D  ,     "TBL_URB3D", layers="SOIL" )
          call add_to_restart(    NoahmpIO%TGL_URB3D  ,     "TGL_URB3D", layers="SOIL" )
          call add_to_restart(   NoahmpIO%CMCR_URB2D  ,    "CMCR_URB2D" )
          call add_to_restart(    NoahmpIO%TGR_URB2D  ,     "TGR_URB2D" )
          call add_to_restart(   NoahmpIO%TGRL_URB3D  ,    "TGRL_URB3D", layers="SOIL" )
          call add_to_restart(    NoahmpIO%SMR_URB3D  ,     "SMR_URB3D", layers="SOIL" )
          call add_to_restart(  NoahmpIO%DRELR_URB2D  ,   "DRELR_URB2D" )
          call add_to_restart(  NoahmpIO%DRELB_URB2D  ,   "DRELB_URB2D" )
          call add_to_restart(  NoahmpIO%DRELG_URB2D  ,   "DRELG_URB2D" )
          call add_to_restart(NoahmpIO%FLXHUMR_URB2D  , "FLXHUMR_URB2D" )
          call add_to_restart(NoahmpIO%FLXHUMB_URB2D  , "FLXHUMB_URB2D" )
          call add_to_restart(NoahmpIO%FLXHUMG_URB2D  , "FLXHUMG_URB2D" )
        endif

        if ( (NoahmpIO%SF_URBAN_PHYSICS == 2) .or. (NoahmpIO%SF_URBAN_PHYSICS == 3) ) then ! BEP or BEM urban models  
          call add_to_restart(    NoahmpIO%TRB_URB4D  ,     "TRB_URB4D", layers="URBN" )
          call add_to_restart(    NoahmpIO%TW1_URB4D  ,     "TW1_URB4D", layers="URBN" )
          call add_to_restart(    NoahmpIO%TW2_URB4D  ,     "TW2_URB4D", layers="URBN" )
          call add_to_restart(    NoahmpIO%TGB_URB4D  ,     "TGB_URB4D", layers="URBN" )
          call add_to_restart(   NoahmpIO%SFW1_URB3D  ,    "SFW1_URB3D", layers="URBN" )
          call add_to_restart(   NoahmpIO%SFW2_URB3D  ,    "SFW2_URB3D", layers="URBN" )
          call add_to_restart(    NoahmpIO%SFR_URB3D  ,     "SFR_URB3D", layers="URBN" )
          call add_to_restart(    NoahmpIO%SFG_URB3D  ,     "SFG_URB3D", layers="URBN" )
        endif

        if ( NoahmpIO%SF_URBAN_PHYSICS == 3 ) then  ! BEM urban model  
          call add_to_restart(   NoahmpIO%TLEV_URB3D  ,    "TLEV_URB3D", layers="URBN" )
          call add_to_restart(   NoahmpIO%QLEV_URB3D  ,    "QLEV_URB3D", layers="URBN" )
          call add_to_restart( NoahmpIO%TW1LEV_URB3D  ,  "TW1LEV_URB3D", layers="URBN" )
          call add_to_restart( NoahmpIO%TW2LEV_URB3D  ,  "TW2LEV_URB3D", layers="URBN" )
          call add_to_restart(  NoahmpIO%TGLEV_URB3D  ,   "TGLEV_URB3D", layers="URBN" )
          call add_to_restart(  NoahmpIO%TFLEV_URB3D  ,   "TFLEV_URB3D", layers="URBN" )
          call add_to_restart(  NoahmpIO%SF_AC_URB3D  ,   "SF_AC_URB3D" )
          call add_to_restart(  NoahmpIO%LF_AC_URB3D  ,   "LF_AC_URB3D" )
          call add_to_restart(  NoahmpIO%CM_AC_URB3D  ,   "CM_AC_URB3D" )
          call add_to_restart( NoahmpIO%SFVENT_URB3D  ,  "SFVENT_URB3D" )
          call add_to_restart( NoahmpIO%LFVENT_URB3D  ,  "LFVENT_URB3D" )
          call add_to_restart( NoahmpIO%SFWIN1_URB3D  ,  "SFWIN1_URB3D", layers="URBN" )
          call add_to_restart( NoahmpIO%SFWIN2_URB3D  ,  "SFWIN2_URB3D", layers="URBN" )
          call add_to_restart(  NoahmpIO%EP_PV_URB3D  ,   "EP_PV_URB3D" )
          call add_to_restart(   NoahmpIO%T_PV_URB3D  ,    "T_PV_URB3D", layers="URBN" )
          call add_to_restart(    NoahmpIO%TRV_URB4D  ,    "TRV_URB4D" , layers="URBN" )
          call add_to_restart(    NoahmpIO%QR_URB4D   ,     "QR_URB4D" , layers="URBN" )
          call add_to_restart(   NoahmpIO%QGR_URB3D   ,    "QGR_URB3D"  )
          call add_to_restart(   NoahmpIO%TGR_URB3D   ,    "TGR_URB3D"  )
          call add_to_restart(   NoahmpIO%DRAIN_URB4D ,   "DRAIN_URB4D", layers="URBN" )
          call add_to_restart( NoahmpIO%DRAINGR_URB3D , "DRAINGR_URB3D" )
          call add_to_restart(    NoahmpIO%SFRV_URB3D ,    "SFRV_URB3D", layers="URBN" )
          call add_to_restart(    NoahmpIO%LFRV_URB3D ,    "LFRV_URB3D", layers="URBN" )
          call add_to_restart(     NoahmpIO%DGR_URB3D ,     "DGR_URB3D", layers="URBN" )
          call add_to_restart(      NoahmpIO%DG_URB3D ,      "DG_URB3D", layers="URBN" )
          call add_to_restart(     NoahmpIO%LFR_URB3D ,     "LFR_URB3D", layers="URBN" )
          call add_to_restart(     NoahmpIO%LFG_URB3D ,     "LFG_URB3D", layers="URBN" )
        endif
    endif
 

  end subroutine WriteNoahmpMosaicRestart

end module WriteNoahmpMosaicRestartMod