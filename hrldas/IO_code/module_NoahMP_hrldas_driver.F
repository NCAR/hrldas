module module_NoahMP_hrldas_driver

  use Machine
  use NoahmpIOVarType
  use NoahmpIOVarInitMod
  use NoahmpReadNamelistMod
  use NoahmpReadTableMod
  use module_hrldas_netcdf_io
  use NoahmpInitMainMod
  use NoahmpGroundwaterInitMod
  use NoahmpDriverMainMod
  use GroundWaterMmfMod,        only : WTABLE_mmf_noahmp, LATERALFLOW, UPDATEWTD
  use NoahmpUrbanDriverMainMod, only : noahmp_urban
  use module_sf_urban,          only : urban_param_init, urban_var_init
  use module_date_utilities

#ifdef MPP_LAND
  use module_mpp_land, only: MPP_LAND_PAR_INI, mpp_land_init, getLocalXY, mpp_land_bcast_char
  use module_mpp_land, only: check_land, my_id , node_info
  use module_cpl_land, only: cpl_land_init
#endif

  implicit none

#ifdef MPP_LAND
  include "mpif.h"
#endif

#ifdef WRF_HYDRO
   integer :: forc_typ, snow_assim, HRLDAS_ini_typ
   real(kind=kind_noahmp), allocatable, dimension(:,:)   :: etpnd, greenfrac, prcp0
   real(kind=kind_noahmp) :: etpnd1
   character(len=19) :: forcDate
   character(len = 256):: GEO_STATIC_FLNM
   real(kind=kind_noahmp), allocatable, dimension(:) :: zsoil    
   integer :: kk
   integer  :: finemesh, finemesh_factor
#endif

  character(len=9), parameter :: version = "v20220701"
  integer :: LDASIN_VERSION

#ifdef MPP_LAND
  integer ix_tmp, jx_tmp
#endif

  type(NoahmpIO_type)                                    :: NoahmpIO

  integer                                                :: I
  integer                                                :: J
  integer                                                :: imode

contains

  subroutine land_driver_ini(NTIME_out,wrfits,wrfite,wrfjts,wrfjte)

    use module_bep_bem_helper, only: nurbm ! C.He 03/31/2021: add for bep_bem treatment in WRFv4.3
    use module_domain,         only : domain ! for groundwater_init WRF

    implicit none

    integer      :: NTIME_out
    real(kind=kind_noahmp) :: max_landtype2d
    type(domain) :: grid
    ! initilization for stand alone parallel code.
    integer, optional, intent(in) :: wrfits,wrfite,wrfjts,wrfjte
    
    !local
    integer ::   xstart, xend, ystart, yend, ixfull, jxfull
    
#ifdef MPP_LAND
    call  MPP_LAND_INIT()
#endif

!---------------------------------------------------------------------
!  read in input data from table and initial file
!---------------------------------------------------------------------
   
    call NoahmpReadNamelist(NoahmpIO)


!C.He: add for urban update in WRFv4.3
!-----------------------------------------------------------------------
! Urban physics set up. If the run-time option for use_wudapt_lcz = 0,
! then the number of urban classes is 3. Else, if the use_wudapt_lcz = 1, 
! then the number increases to 11. The seemingly local variable 
! assignment, "nurbm", is actually USE associated from the BEP BEM 
! helper module.
!-----------------------------------------------------------------------
    if ( NoahmpIO%use_wudapt_lcz == 0 ) then
      nurbm = 3
    elseif ( NoahmpIO%use_wudapt_lcz == 1 ) then
      nurbm = 11
    endif
 
  ! derived urban dimensions 
    if (NoahmpIO%sf_urban_physics > 0 ) then
       NoahmpIO%urban_map_zrd  = NoahmpIO%num_urban_ndm * NoahmpIO%num_urban_nwr * NoahmpIO%num_urban_nz
       NoahmpIO%urban_map_zwd  = NoahmpIO%num_urban_ndm * NoahmpIO%num_urban_nwr * NoahmpIO%num_urban_nz * &
                                 NoahmpIO%num_urban_nbui
       NoahmpIO%urban_map_gd   = NoahmpIO%num_urban_ndm * NoahmpIO%num_urban_ng
       NoahmpIO%urban_map_zd   = NoahmpIO%num_urban_ndm * NoahmpIO%num_urban_nz  * NoahmpIO%num_urban_nbui
       NoahmpIO%urban_map_zdf  = NoahmpIO%num_urban_ndm * NoahmpIO%num_urban_nz 
       NoahmpIO%urban_map_bd   = NoahmpIO%num_urban_nz  * NoahmpIO%num_urban_nbui
       NoahmpIO%urban_map_wd   = NoahmpIO%num_urban_ndm * NoahmpIO%num_urban_nz  * NoahmpIO%num_urban_nbui
       NoahmpIO%urban_map_gbd  = NoahmpIO%num_urban_ndm * NoahmpIO%num_urban_ngb * NoahmpIO%num_urban_nbui
       NoahmpIO%urban_map_fbd  = NoahmpIO%num_urban_ndm * (NoahmpIO%num_urban_nz - 1)  * &
                                 NoahmpIO%num_urban_nf  * NoahmpIO%num_urban_nbui
       NoahmpIO%urban_map_zgrd = NoahmpIO%num_urban_ndm * NoahmpIO%num_urban_ngr * NoahmpIO%num_urban_nz  ! for green roof
       ! C. He: compute maximum urban dimension
       NoahmpIO%max_urban_dim  = max(NoahmpIO%num_urban_hi, NoahmpIO%urban_map_zrd, NoahmpIO%urban_map_zwd, &
                                     NoahmpIO%urban_map_gd, NoahmpIO%urban_map_zd,  NoahmpIO%urban_map_zdf, &
                                     NoahmpIO%urban_map_bd, NoahmpIO%urban_map_wd,  NoahmpIO%urban_map_gbd, &
                                     NoahmpIO%urban_map_fbd,NoahmpIO%urban_map_zgrd)
    endif     
      
!----------------------------------------------------------------------
! Initialize gridded domain
!----------------------------------------------------------------------

#ifdef MPP_LAND
#ifdef WRF_HYDRO
    if ( finemesh /= 0 ) then
         NoahmpIO%xstart = (wrfits-1)*finemesh_factor + 1
         NoahmpIO%xend   = (wrfite-1)*finemesh_factor
         NoahmpIO%ystart = (wrfjts-1)*finemesh_factor + 1
         NoahmpIO%yend   = (wrfjte-1)*finemesh_factor
         call CPL_LAND_INIT(xstart,xend, ystart,yend)
         ix_tmp = NoahmpIO%xend - NoahmpIO%xstart + 1
         jx_tmp = NoahmpIO%yend - NoahmpIO%ystart + 1
    else
#endif
       call read_dim(NoahmpIO%hrldas_setup_file,ix_tmp,jx_tmp)
       call MPP_LAND_PAR_INI(1,ix_tmp,jx_tmp,1)
       call getLocalXY(ix_tmp,jx_tmp,NoahmpIO%xstart,NoahmpIO%ystart,NoahmpIO%xend,NoahmpIO%yend)
#ifdef WRF_HYDRO
    endif
#endif
#endif

    call read_hrldas_hdrinfo(NoahmpIO%hrldas_setup_file, NoahmpIO%ix, NoahmpIO%jx,           &
                             NoahmpIO%xstart, NoahmpIO%xend, NoahmpIO%ystart, NoahmpIO%yend, &
                             NoahmpIO%iswater, NoahmpIO%islake, NoahmpIO%isurban,            &
                             NoahmpIO%isice, NoahmpIO%llanduse, NoahmpIO%dx, NoahmpIO%dy,    &
                             NoahmpIO%truelat1, NoahmpIO%truelat2, NoahmpIO%cen_lon,         &
                             NoahmpIO%lat1, NoahmpIO%lon1, NoahmpIO%igrid, NoahmpIO%mapproj  &
                            )
                          
    write (NoahmpIO%hgrid,'(I1)') NoahmpIO%igrid
    write (NoahmpIO%olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
           NoahmpIO%start_year, NoahmpIO%start_month, NoahmpIO%start_day, NoahmpIO%start_hour, NoahmpIO%start_min, 0

    NoahmpIO%startdate = NoahmpIO%olddate

#ifdef MPP_LAND
    NoahmpIO%ix = ix_tmp
    NoahmpIO%jx = jx_tmp
#endif

#ifdef WRF_HYDRO
    forcdate = NoahmpIO%olddate
#endif
  
    NoahmpIO%ids = NoahmpIO%xstart
    NoahmpIO%ide = NoahmpIO%xend
    NoahmpIO%jds = NoahmpIO%ystart
    NoahmpIO%jde = NoahmpIO%yend
    NoahmpIO%kds = 1
    NoahmpIO%kde = 2
    NoahmpIO%its = NoahmpIO%xstart
    NoahmpIO%ite = NoahmpIO%xend
    NoahmpIO%jts = NoahmpIO%ystart
    NoahmpIO%jte = NoahmpIO%yend
    NoahmpIO%kts = 1
    NoahmpIO%kte = 2
    NoahmpIO%ims = NoahmpIO%xstart
    NoahmpIO%ime = NoahmpIO%xend
    NoahmpIO%jms = NoahmpIO%ystart
    NoahmpIO%jme = NoahmpIO%yend
    NoahmpIO%kms = 1
    NoahmpIO%kme = 2
  
    if ( (NoahmpIO%sf_urban_physics == 2) .or. (NoahmpIO%sf_urban_physics == 3) ) then
       NoahmpIO%kde = NoahmpIO%num_urban_atmosphere + 1  ! to be consistent with kme
       NoahmpIO%kme = NoahmpIO%num_urban_atmosphere + 1  ! bug fix for dimension of urban cell interfaces
       NoahmpIO%kte = NoahmpIO%num_urban_atmosphere
    endif

!---------------------------------------------------------------------
!  Allocate multi-dimension fields for subwindow calculation
!---------------------------------------------------------------------

    NoahmpIO%ixfull    = NoahmpIO%xend - NoahmpIO%xstart+1
    NoahmpIO%jxfull    = NoahmpIO%yend - NoahmpIO%ystart+1
    NoahmpIO%ixpar     = NoahmpIO%ixfull
    NoahmpIO%jxpar     = NoahmpIO%jxfull
    NoahmpIO%xstartpar = 1
    NoahmpIO%ystartpar = 1

!---------------------------------------------------------------------
!  initialize NoahmpIOVarType data type and variables
!---------------------------------------------------------------------

    call NoahmpIOVarInitDefault(NoahmpIO)

!---------------------------------------------------------------------
!  read in Noah-MP Parameters from MPTABLE
!---------------------------------------------------------------------

    call NoahmpReadTable(NoahmpIO)

#ifdef WRF_HYDRO
    allocate( greenfrac   (NoahmpIO%XSTART:NoahmpIO%XEND,NoahmpIO%YSTART:NoahmpIO%YEND))
    greenfrac = 0
#endif

!----------------------------------------------------------------------
! Read Landuse Type and Soil Texture and Other Information
!----------------------------------------------------------------------
 
    call READLAND_HRLDAS(NoahmpIO%HRLDAS_SETUP_FILE, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND,          &
                         NoahmpIO%ISWATER, NoahmpIO%ISLAKE, NoahmpIO%IVGTYP, NoahmpIO%ISLTYP, NoahmpIO%TERRAIN, NoahmpIO%TMN, &
                         NoahmpIO%XLAT, NoahmpIO%XLONG, NoahmpIO%XLAND, NoahmpIO%SEAICE, NoahmpIO%MSFTX, NoahmpIO%MSFTY)
  
    where (NoahmpIO%SEAICE > 0.0) NoahmpIO%XICE = 1.0

!------------------------------------------------------------------------
! For spatially-varying soil parameters, read in necessary extra fields
!------------------------------------------------------------------------

    if ( NoahmpIO%IOPT_SOIL == 2 ) then
       call READ_SOIL_TEXTURE(NoahmpIO%HRLDAS_SETUP_FILE, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND, &
                              NoahmpIO%NSOIL, NoahmpIO%IVGTYP, NoahmpIO%SOILCL1, NoahmpIO%SOILCL2, NoahmpIO%SOILCL3,      &
                              NoahmpIO%SOILCL4, NoahmpIO%ISICE, NoahmpIO%ISWATER)
       NoahmpIO%ISLTYP = nint(NoahmpIO%SOILCL1(:,:))
    endif
  
    if ( NoahmpIO%IOPT_SOIL == 3 ) then
       call READ_SOIL_COMPOSITION(NoahmpIO%HRLDAS_SETUP_FILE, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND, &
                                  NoahmpIO%NSOIL, NoahmpIO%IVGTYP, NoahmpIO%ISICE, NoahmpIO%ISWATER, NoahmpIO%SOILCOMP)
    endif
  
    if ( NoahmpIO%IOPT_SOIL == 4 ) then
       call READ_3D_SOIL(NoahmpIO%SPATIAL_FILENAME, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND,       &
                        NoahmpIO%NSOIL, NoahmpIO%BEXP_3D, NoahmpIO%SMCDRY_3D, NoahmpIO%SMCWLT_3D, NoahmpIO%SMCREF_3D,     &
                        NoahmpIO%SMCMAX_3D, NoahmpIO%DKSAT_3D, NoahmpIO%DWSAT_3D, NoahmpIO%PSISAT_3D, NoahmpIO%QUARTZ_3D, &
                        NoahmpIO%REFDK_2D, NoahmpIO%REFKDT_2D, NoahmpIO%IRR_FRAC_2D, NoahmpIO%IRR_HAR_2D,                 &
                        NoahmpIO%IRR_LAI_2D, NoahmpIO%IRR_MAD_2D, NoahmpIO%FILOSS_2D, NoahmpIO%SPRIR_RATE_2D,             &
                        NoahmpIO%MICIR_RATE_2D, NoahmpIO%FIRTFAC_2D, NoahmpIO%IR_RAIN_2D, NoahmpIO%BVIC_2D,               &
                        NoahmpIO%AXAJ_2D, NoahmpIO%BXAJ_2D, NoahmpIO%XXAJ_2D, NoahmpIO%BDVIC_2D, NoahmpIO%GDVIC_2D,       &
                        NoahmpIO%BBVIC_2D, NoahmpIO%KLAT_FAC, NoahmpIO%TDSMC_FAC, NoahmpIO%TD_DC, NoahmpIO%TD_DCOEF,      &
                        NoahmpIO%TD_DDRAIN, NoahmpIO%TD_RADI, NoahmpIO%TD_SPAC)
    endif

!----------------------------------------------------------------------
! For spatially-varying irrigation parameters, read in necessary extra fields
! only if IOPT_IRR >= 1
!----------------------------------------------------------------------
    if ( NoahmpIO%IOPT_IRR >= 1 ) then
       call READ_AGRICULTURE_DATA(NoahmpIO%AGDATA_FLNM, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND, &
                                  NoahmpIO%IRFRACT, NoahmpIO%SIFRACT, NoahmpIO%MIFRACT, NoahmpIO%FIFRACT)
    endif
  
!------------------------------------------------------------------------
! For IOPT_RUNSUB = 5 (MMF groundwater), read in necessary extra fields
! This option is not tested for parallel use in the offline driver
!------------------------------------------------------------------------

    if (NoahmpIO%IOPT_RUNSUB == 5) then
       call READ_MMF_RUNOFF(NoahmpIO%HRLDAS_SETUP_FILE, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND,&
                            NoahmpIO%FDEPTHXY, NoahmpIO%EQZWT, NoahmpIO%RECHCLIM, NoahmpIO%RIVERBEDXY)
    endif

!------------------------------------------------------------------------
! For OPT_CROP=1 (crop model), read in necessary extra fields
!------------------------------------------------------------------------

    NoahmpIO%CROPTYPE   = 0       ! make default 0% crops everywhere
    NoahmpIO%PLANTING   = 126     ! default planting date
    NoahmpIO%HARVEST    = 290     ! default harvest date
    NoahmpIO%SEASON_GDD = 1605    ! default total seasonal growing degree days
    if (NoahmpIO%IOPT_CROP == 1) then ! use crop_option=1 for reading crop extra fields
       call READ_CROP_INPUT(NoahmpIO%HRLDAS_SETUP_FILE, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND,&
                            NoahmpIO%CROPTYPE, NoahmpIO%PLANTING, NoahmpIO%HARVEST, NoahmpIO%SEASON_GDD)
    endif

!------------------------------------------------------------------------
! For IOPT_TDRN = 1 or 2 READ TILE DRAIN MAP
!------------------------------------------------------------------------

    NoahmpIO%TD_FRACTION = 0.0
    if (NoahmpIO%IOPT_TDRN > 0) then
       call READ_TILE_DRAIN_MAP(NoahmpIO%TDINPUT_FLNM, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND, NoahmpIO%TD_FRACTION)
    endif

!------------------------------------------------------------------------
! For SF_URBAN_PHYSICS > 0 read 2D map of urban parameters
!------------------------------------------------------------------------

    NoahmpIO%FRC_URB2D = 0.0
    if (NoahmpIO%SF_URBAN_PHYSICS > 0) then
       call READ_URBAN_MAP(NoahmpIO%HRLDAS_SETUP_FILE, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND, NoahmpIO%FRC_URB2D)
    endif

!----------------------------------------------------------------------
! Initialize Model State
!----------------------------------------------------------------------

    NoahmpIO%SLOPETYP  =  1 ! it was 2 here and 1 in the noahmpdrv- pvk
    NoahmpIO%DZS       =  NoahmpIO%SOIL_THICK_INPUT(1:NoahmpIO%NSOIL)
    NoahmpIO%ITIMESTEP = 1
    xstart = NoahmpIO%xstart
    xend   = NoahmpIO%xend
    ystart = NoahmpIO%ystart
    yend   = NoahmpIO%yend
    ixfull = NoahmpIO%ixfull
    jxfull = NoahmpIO%jxfull 

    if (NoahmpIO%restart_filename_requested /= " ") then
       NoahmpIO%restart_flag = .true.

       call find_restart_file(NoahmpIO%rank, trim(NoahmpIO%restart_filename_requested), NoahmpIO%startdate, &
                              NoahmpIO%khour, NoahmpIO%olddate, NoahmpIO%restart_flnm)

       call read_restart(trim(NoahmpIO%restart_flnm), xstart, xend, xstart, ixfull, jxfull, NoahmpIO%NSOIL, NoahmpIO%olddate)

#ifdef MPP_LAND
       call mpp_land_bcast_char(19,NoahmpIO%OLDDATE(1:19))
#endif

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
     
       NoahmpIO%STEPWTD = nint(NoahmpIO%WTDDT*60.0/NoahmpIO%DTBL)
       NoahmpIO%STEPWTD = max(NoahmpIO%STEPWTD,1)

! Must still call NoahmpInitMain even in restart to set up parameter arrays (also done in WRF)

!------------------------------------------------------------------------------------------
!    Initialize Noah-MP states and variables
!------------------------------------------------------------------------------------------
       call NoahmpInitMain(NoahmpIO)

       if ( NoahmpIO%SF_URBAN_PHYSICS > 0 ) then  !urban
        
          call urban_param_init(NoahmpIO%DZR, NoahmpIO%DZB, NoahmpIO%DZG, NoahmpIO%NSOIL, &
                                NoahmpIO%sf_urban_physics, NoahmpIO%use_wudapt_lcz, NoahmpIO%IRI_URBAN) ! cenlin added

          call urban_var_init(NoahmpIO%ISURBAN, NoahmpIO%TSK, NoahmpIO%TSLB, NoahmpIO%TMN,                                  & !urban
                              NoahmpIO%IVGTYP, NoahmpIO%ims,NoahmpIO%ime,NoahmpIO%jms,NoahmpIO%jme,                         & !urban
                              NoahmpIO%kms,NoahmpIO%kme,NoahmpIO%NSOIL, NoahmpIO%LCZ_1_TABLE,                               & !urban  
                              NoahmpIO%LCZ_2_TABLE,NoahmpIO%LCZ_3_TABLE,NoahmpIO%LCZ_4_TABLE,NoahmpIO%LCZ_5_TABLE,          & !urban
                              NoahmpIO%LCZ_6_TABLE,NoahmpIO%LCZ_7_TABLE,NoahmpIO%LCZ_8_TABLE,NoahmpIO%LCZ_9_TABLE,          & !urban
                              NoahmpIO%LCZ_10_TABLE, NoahmpIO%LCZ_11_TABLE,NoahmpIO%restart_flag,NoahmpIO%sf_urban_physics, & !urban
                              NoahmpIO%XXXR_URB2D, NoahmpIO%XXXB_URB2D, NoahmpIO%XXXG_URB2D, NoahmpIO%XXXC_URB2D,           & !urban
                              NoahmpIO%TR_URB2D, NoahmpIO%TB_URB2D, NoahmpIO%TG_URB2D, NoahmpIO%TC_URB2D,NoahmpIO%QC_URB2D, & !urban
                              NoahmpIO%TRL_URB3D, NoahmpIO%TBL_URB3D, NoahmpIO%TGL_URB3D, NoahmpIO%SH_URB2D,                & !urban
                              NoahmpIO%LH_URB2D, NoahmpIO%G_URB2D, NoahmpIO%RN_URB2D, NoahmpIO%TS_URB2D,                    & !urban
                              NoahmpIO%num_urban_ndm,  NoahmpIO%urban_map_zrd,  NoahmpIO%urban_map_zwd,                     & !urban
                              NoahmpIO%urban_map_gd,                                                                        & !I multi-layer urban
                              NoahmpIO%urban_map_zd,  NoahmpIO%urban_map_zdf, NoahmpIO%urban_map_bd,                        & !I multi-layer urban   
                              NoahmpIO%urban_map_wd, NoahmpIO%urban_map_gbd,  NoahmpIO%urban_map_fbd,                       & !I multi-layer urban
                              NoahmpIO%urban_map_zgrd,                                                                      & !I multi-layer urban
                              NoahmpIO%num_urban_hi,                                                                        & !urban
                              NoahmpIO%TRB_URB4D, NoahmpIO%TW1_URB4D, NoahmpIO%TW2_URB4D, NoahmpIO%TGB_URB4D,               & !urban
                              NoahmpIO%TLEV_URB3D, NoahmpIO%QLEV_URB3D,                                                     & !urban
                              NoahmpIO%TW1LEV_URB3D,NoahmpIO%TW2LEV_URB3D,                                                  & !urban
                              NoahmpIO%TGLEV_URB3D, NoahmpIO%TFLEV_URB3D,                                                   & !urban
                              NoahmpIO%SF_AC_URB3D, NoahmpIO%LF_AC_URB3D, NoahmpIO%CM_AC_URB3D,                             & !urban
                              NoahmpIO%SFVENT_URB3D, NoahmpIO%LFVENT_URB3D,                                                 & !urban
                              NoahmpIO%SFWIN1_URB3D, NoahmpIO%SFWIN2_URB3D,                                                 & !urban
                              NoahmpIO%SFW1_URB3D, NoahmpIO%SFW2_URB3D, NoahmpIO%SFR_URB3D, NoahmpIO%SFG_URB3D,             & !urban
                              NoahmpIO%EP_PV_URB3D, NoahmpIO%T_PV_URB3D,                                                    & !GRZ
                              NoahmpIO%TRV_URB4D, NoahmpIO%QR_URB4D, NoahmpIO%QGR_URB3D, NoahmpIO%TGR_URB3D,                & !GRZ
                              NoahmpIO%DRAIN_URB4D, NoahmpIO%DRAINGR_URB3D, NoahmpIO%SFRV_URB3D,                            & !GRZ
                              NoahmpIO%LFRV_URB3D, NoahmpIO%DGR_URB3D, NoahmpIO%DG_URB3D, NoahmpIO%LFR_URB3D,               & !GRZ   
                              NoahmpIO%LFG_URB3D, NoahmpIO%SMOIS,                                                           & !GRZ
                              NoahmpIO%LP_URB2D, NoahmpIO%HI_URB2D, NoahmpIO%LB_URB2D,                                      & !urban
                              NoahmpIO%HGT_URB2D, NoahmpIO%MH_URB2D, NoahmpIO%STDH_URB2D,                                   & !urban
                              NoahmpIO%LF_URB2D,                                                                            & !urban
                              NoahmpIO%CMCR_URB2D, NoahmpIO%TGR_URB2D, NoahmpIO%TGRL_URB3D, NoahmpIO%SMR_URB3D,             & !urban
                              NoahmpIO%DRELR_URB2D, NoahmpIO%DRELB_URB2D, NoahmpIO%DRELG_URB2D,                             & !urban
                              NoahmpIO%FLXHUMR_URB2D, NoahmpIO%FLXHUMB_URB2D, NoahmpIO%FLXHUMG_URB2D,                       & !urban
                              NoahmpIO%A_U_BEP, NoahmpIO%A_V_BEP, NoahmpIO%A_T_BEP, NoahmpIO%A_Q_BEP,                       & !multi-layer urban
                              NoahmpIO%A_E_BEP, NoahmpIO%B_U_BEP, NoahmpIO%B_V_BEP,                                         & !multi-layer urban
                              NoahmpIO%B_T_BEP, NoahmpIO%B_Q_BEP, NoahmpIO%B_E_BEP, NoahmpIO%DLG_BEP,                       & !multi-layer urban
                              NoahmpIO%DL_U_BEP, NoahmpIO%SF_BEP, NoahmpIO%VL_BEP,                                          & !multi-layer urban
                              NoahmpIO%FRC_URB2D, NoahmpIO%UTYPE_URB2D, NoahmpIO%use_wudapt_lcz)                              !urban

          max_landtype2d = maxval(NoahmpIO%IVGTYP)*1.0
          if ( (NoahmpIO%use_wudapt_lcz == 0) .and. (max_landtype2d > 50.0) ) then   ! LCZ number: 51~61
             stop "USING 10 WUDAPT LCZ WITHOUT URBPARM_LCZ.TBL. SET USE_WUDAPT_LCZ=1"
          endif
          if ( (NoahmpIO%use_wudapt_lcz == 1) .and. (max_landtype2d <= 50.0) ) then  ! LCZ number: 51~61
             stop "USING URBPARM_LCZ.TBL WITH OLD 3 URBAN CLASSES. SET USE_WUDAPT_LCZ=0"
          endif

       endif ! NoahmpIO%SF_URBAN_PHYSICS

    else

       NoahmpIO%restart_flag = .false.

#ifdef WRF_HYDRO
       if ( (forc_typ > 2) .and. (forc_typ /= 6) ) HRLDAS_ini_typ = 0

       if (HRLDAS_ini_typ == 1) then
       ! read initial parameters and conditions from the HRLDAS forcing data
          if (forc_typ == 2) then
             NoahmpIO%inflnm = trim(indir)//"/"//&
             NoahmpIO%startdate(1:4)//NoahmpIO%startdate(6:7)//NoahmpIO%startdate(9:10)//NoahmpIO%startdate(12:13)//&
             NoahmpIO%startdate(15:16)//".LDASIN_DOMAIN"//NoahmpIO%hgrid
          else
             NoahmpIO%inflnm = trim(NoahmpIO%indir)//"/"//&
             NoahmpIO%startdate(1:4)//NoahmpIO%startdate(6:7)//NoahmpIO%startdate(9:10)//NoahmpIO%startdate(12:13)//&
             ".LDASIN_DOMAIN"//NoahmpIO%hgrid
          endif
#else
          NoahmpIO%inflnm = trim(NoahmpIO%indir)//"/"//&
          NoahmpIO%startdate(1:4)//NoahmpIO%startdate(6:7)//NoahmpIO%startdate(9:10)//NoahmpIO%startdate(12:13)//&
          ".LDASIN_DOMAIN"//NoahmpIO%hgrid
#endif

          call READINIT_HRLDAS(NoahmpIO%HRLDAS_SETUP_FILE, NoahmpIO%xstart, NoahmpIO%xend,    &
                              NoahmpIO%ystart, NoahmpIO%yend, NoahmpIO%NSOIL, NoahmpIO%DZS,  &
                              NoahmpIO%OLDDATE, LDASIN_VERSION, NoahmpIO%SMOIS,              &
                              NoahmpIO%TSLB, NoahmpIO%CANWAT, NoahmpIO%TSK, NoahmpIO%SNOW,   &
                              NoahmpIO%SNOWH, NoahmpIO%FNDSNOWH)

          NoahmpIO%VEGFRA =  0.0
          call READVEG_HRLDAS(NoahmpIO%HRLDAS_SETUP_FILE, NoahmpIO%xstart, NoahmpIO%xend,        &
                              NoahmpIO%ystart, NoahmpIO%yend, NoahmpIO%OLDDATE, NoahmpIO%IVGTYP, &
                              NoahmpIO%VEGFRA, NoahmpIO%LAI, NoahmpIO%GVFMIN, NoahmpIO%GVFMAX)

#ifdef WRF_HYDRO
       else   !  HRLDAS_ini_typ        

#ifdef MPP_LAND
          call HYDRO_HRLDAS_ini_mpp   &
#else
          call HYDRO_HRLDAS_ini   &
#endif
          (trim(NoahmpIO%hrldas_setup_file), NoahmpIO%xend-NoahmpIO%xstart+1,NoahmpIO%yend-NoahmpIO%ystart+1,                          & 
          NoahmpIO%NSOIL,NoahmpIO%SMOIS(:,1:NoahmpIO%NSOIL,:),NoahmpIO%TSLB(:,1:NoahmpIO%NSOIL,:),NoahmpIO%SH2O(:,1:NoahmpIO%NSOIL,:), &
          NoahmpIO%CANWAT, NoahmpIO%TSK, NoahmpIO%SNOW, NoahmpIO%SNOWH, NoahmpIO%lai, NoahmpIO%VEGFRA, NoahmpIO%IVGTYP,                &
          NoahmpIO%FNDSNOWH)

          NoahmpIO%greenfrac = 0.0
#ifdef MPP_LAND
          call get_greenfrac_mpp &
#else
          call get_greenfrac &
#endif
          (trim(NoahmpIO%GEO_STATIC_FLNM),NoahmpIO%greenfrac, NoahmpIO%ix, NoahmpIO%jx, NoahmpIO%olddate, NoahmpIO%SHDMAX)
          !yw GVFMAX = maxval(greenfrac)
       endif
#endif

       !----------------------------------------------------------------------------------
       !   Initialize Noah-MP variables
       !----------------------------------------------------------------------------------
       call NoahmpInitMain(NoahmpIO)

       !----------------------------------------------------------------------------------
       !   Initialize MMF Groundwater variables
       !----------------------------------------------------------------------------------
       if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
          call NoahmpGroundwaterInitMain(grid, NoahmpIO)
       endif

       if (NoahmpIO%SF_URBAN_PHYSICS > 0 ) then  !urban

          call urban_param_init(NoahmpIO%DZR,NoahmpIO%DZB,NoahmpIO%DZG,NoahmpIO%NSOIL,    &
                                NoahmpIO%sf_urban_physics,NoahmpIO%use_wudapt_lcz,NoahmpIO%IRI_URBAN) ! cenlin added

          call urban_var_init(NoahmpIO%ISURBAN, NoahmpIO%TSK, NoahmpIO%TSLB, NoahmpIO%TMN,                                 & !urban
                              NoahmpIO%IVGTYP, NoahmpIO%ims,NoahmpIO%ime,NoahmpIO%jms,NoahmpIO%jme,                        & !urban
                              NoahmpIO%kms,NoahmpIO%kme,NoahmpIO%NSOIL, NoahmpIO%LCZ_1_TABLE,                              & !urban  
                              NoahmpIO%LCZ_2_TABLE,NoahmpIO%LCZ_3_TABLE,NoahmpIO%LCZ_4_TABLE,NoahmpIO%LCZ_5_TABLE,         & !urban
                              NoahmpIO%LCZ_6_TABLE,NoahmpIO%LCZ_7_TABLE,NoahmpIO%LCZ_8_TABLE,NoahmpIO%LCZ_9_TABLE,         & !urban
                              NoahmpIO%LCZ_10_TABLE, NoahmpIO%LCZ_11_TABLE,NoahmpIO%restart_flag,NoahmpIO%sf_urban_physics,& !urban
                              NoahmpIO%XXXR_URB2D, NoahmpIO%XXXB_URB2D, NoahmpIO%XXXG_URB2D, NoahmpIO%XXXC_URB2D,          & !urban
                              NoahmpIO%TR_URB2D, NoahmpIO%TB_URB2D, NoahmpIO%TG_URB2D, NoahmpIO%TC_URB2D,NoahmpIO%QC_URB2D,& !urban
                              NoahmpIO%TRL_URB3D, NoahmpIO%TBL_URB3D, NoahmpIO%TGL_URB3D, NoahmpIO%SH_URB2D,               & !urban
                              NoahmpIO%LH_URB2D, NoahmpIO%G_URB2D, NoahmpIO%RN_URB2D, NoahmpIO%TS_URB2D,                   & !urban
                              NoahmpIO%num_urban_ndm,  NoahmpIO%urban_map_zrd,  NoahmpIO%urban_map_zwd,                    & !urban
                              NoahmpIO%urban_map_gd,                                                                       & !I multi-layer urban
                              NoahmpIO%urban_map_zd,  NoahmpIO%urban_map_zdf, NoahmpIO%urban_map_bd,                       & !I multi-layer urban   
                              NoahmpIO%urban_map_wd, NoahmpIO%urban_map_gbd,  NoahmpIO%urban_map_fbd,                      & !I multi-layer urban
                              NoahmpIO%urban_map_zgrd,                                                                     & !I multi-layer urban
                              NoahmpIO%num_urban_hi,                                                                       & !urban
                              NoahmpIO%TRB_URB4D, NoahmpIO%TW1_URB4D, NoahmpIO%TW2_URB4D, NoahmpIO%TGB_URB4D,              & !urban
                              NoahmpIO%TLEV_URB3D, NoahmpIO%QLEV_URB3D,                                                    & !urban
                              NoahmpIO%TW1LEV_URB3D,NoahmpIO%TW2LEV_URB3D,                                                 & !urban
                              NoahmpIO%TGLEV_URB3D, NoahmpIO%TFLEV_URB3D,                                                  & !urban
                              NoahmpIO%SF_AC_URB3D, NoahmpIO%LF_AC_URB3D, NoahmpIO%CM_AC_URB3D,                            & !urban
                              NoahmpIO%SFVENT_URB3D, NoahmpIO%LFVENT_URB3D,                                                & !urban
                              NoahmpIO%SFWIN1_URB3D, NoahmpIO%SFWIN2_URB3D,                                                & !urban
                              NoahmpIO%SFW1_URB3D, NoahmpIO%SFW2_URB3D, NoahmpIO%SFR_URB3D, NoahmpIO%SFG_URB3D,            & !urban
                              NoahmpIO%EP_PV_URB3D, NoahmpIO%T_PV_URB3D,                                                   & !GRZ
                              NoahmpIO%TRV_URB4D, NoahmpIO%QR_URB4D, NoahmpIO%QGR_URB3D, NoahmpIO%TGR_URB3D,               & !GRZ
                              NoahmpIO%DRAIN_URB4D, NoahmpIO%DRAINGR_URB3D, NoahmpIO%SFRV_URB3D,                           & !GRZ
                              NoahmpIO%LFRV_URB3D, NoahmpIO%DGR_URB3D, NoahmpIO%DG_URB3D, NoahmpIO%LFR_URB3D,              & !GRZ   
                              NoahmpIO%LFG_URB3D, NoahmpIO%SMOIS,                                                          & !GRZ
                              NoahmpIO%LP_URB2D, NoahmpIO%HI_URB2D, NoahmpIO%LB_URB2D,                                     & !urban
                              NoahmpIO%HGT_URB2D, NoahmpIO%MH_URB2D, NoahmpIO%STDH_URB2D,                                  & !urban
                              NoahmpIO%LF_URB2D,                                                                           & !urban
                              NoahmpIO%CMCR_URB2D, NoahmpIO%TGR_URB2D, NoahmpIO%TGRL_URB3D, NoahmpIO%SMR_URB3D,            & !urban
                              NoahmpIO%DRELR_URB2D, NoahmpIO%DRELB_URB2D, NoahmpIO%DRELG_URB2D,                            & !urban
                              NoahmpIO%FLXHUMR_URB2D, NoahmpIO%FLXHUMB_URB2D, NoahmpIO%FLXHUMG_URB2D,                      & !urban
                              NoahmpIO%A_U_BEP, NoahmpIO%A_V_BEP, NoahmpIO%A_T_BEP, NoahmpIO%A_Q_BEP,                      & !multi-layer urban
                              NoahmpIO%A_E_BEP, NoahmpIO%B_U_BEP, NoahmpIO%B_V_BEP,                                        & !multi-layer urban
                              NoahmpIO%B_T_BEP, NoahmpIO%B_Q_BEP, NoahmpIO%B_E_BEP, NoahmpIO%DLG_BEP,                      & !multi-layer urban
                              NoahmpIO%DL_U_BEP, NoahmpIO%SF_BEP, NoahmpIO%VL_BEP,                                         & !multi-layer urban
                              NoahmpIO%FRC_URB2D, NoahmpIO%UTYPE_URB2D, NoahmpIO%use_wudapt_lcz)                             !urban

          max_landtype2d = maxval(NoahmpIO%IVGTYP)*1.0
          if ( (NoahmpIO%use_wudapt_lcz == 0) .and. (max_landtype2d > 50.0) ) then   ! LCZ number: 51~61
             stop "USING 10 WUDAPT LCZ WITHOUT URBPARM_LCZ.TBL. SET USE_WUDAPT_LCZ=1"
          endif
          if ( (NoahmpIO%use_wudapt_lcz == 1) .and. (max_landtype2d <= 50.0) ) then  ! LCZ number: 51~61
             stop "USING URBPARM_LCZ.TBL WITH OLD 3 URBAN CLASSES. SET USE_WUDAPT_LCZ=0"
          endif

       endif !urban

       if ( NoahmpIO%SF_URBAN_PHYSICS > 1 ) then  !urban
          do i = 1, NoahmpIO%num_urban_atmosphere-1
             NoahmpIO%dz_urban(:,i,:) = NoahmpIO%urban_atmosphere_thickness  ! thickness of full levels
          enddo
          NoahmpIO%dz_urban(:,NoahmpIO%num_urban_atmosphere,:) =                                   &
                       2*(NoahmpIO%zlvl - NoahmpIO%urban_atmosphere_thickness*(NoahmpIO%num_urban_atmosphere-1))
          print*, NoahmpIO%dz_urban(1,:,1)
       endif

    endif  ! restart flag

    NoahmpIO%NTIME       = (NoahmpIO%KHOUR)*3600.0/nint(NoahmpIO%dtbl)*(NoahmpIO%spinup_loops+1)
    NoahmpIO%spinup_loop = 0
    NoahmpIO%reset_spinup_date = .false.

    print*, "NTIME = ", NoahmpIO%NTIME , "KHOUR=",NoahmpIO%KHOUR,"dtbl = ", NoahmpIO%dtbl
  
    call system_clock(count=NoahmpIO%clock_count_1)   ! Start a timer


#ifdef WRF_HYDRO
    allocate( etpnd     (NoahmpIO%ix,NoahmpIO%jx) )
    allocate( prcp0     (NoahmpIO%ix,NoahmpIO%jx) )

    etpnd     = 0.0
    prcp0     = 0

    allocate(zsoil (NoahmpIO%NSOIL))
    zsoil = 0

    zsoil(1) = -1*NoahmpIO%soil_thick_input(1)
    do kk = 2, NoahmpIO%NSOIL
       zsoil(kk) = zsoil(kk-1)-NoahmpIO%soil_thick_input(kk)
    enddo
   
    print*, "zsoil/soil_thick_input = ", NoahmpIO%soil_thick_input(1:NoahmpIO%NSOIL) 

    call hrldas_drv_HYDRO_ini(NoahmpIO%TSLB(:,1:NoahmpIO%NSOIL,:),NoahmpIO%SMOIS(:,1:NoahmpIO%NSOIL,:),     &
                              NoahmpIO%SH2O(:,1:NoahmpIO%NSOIL,:), NoahmpIO%infxsrt,NoahmpIO%sfcheadrt,     &
                              NoahmpIO%soldrain,NoahmpIO%qtiledrain,NoahmpIO%ZWATBLE2D,                     &
                              NoahmpIO%ix,NoahmpIO%jx,NoahmpIO%NSOIL, NoahmpIO%SMOIS,                       &
                              real(NoahmpIO%noah_timestep),NoahmpIO%olddate,zsoil(1:NoahmpIO%NSOIL))

    if ( finemesh /= 0 ) then
       if ( NoahmpIO%restart_flag .eqv. .true. ) then
          NTIME_out =  10
       else
          NTIME_out =  1 
       endif
       return
    endif
#endif

    NTIME_out = NoahmpIO%NTIME 
   
  end subroutine land_driver_ini

!===============================================================================
  subroutine land_driver_exe(itime)
     implicit  none
     integer :: itime          ! timestep loop
     integer :: I, J, K

!---------------------------------------------------------------------------------
! Read the forcing data.
!---------------------------------------------------------------------------------
! For HRLDAS, we're assuming (for now) that each time period is in a 
! separate file.  So we can open a new one right now.

     NoahmpIO%inflnm = trim(NoahmpIO%indir)//"/"//&
          NoahmpIO%olddate(1:4)//NoahmpIO%olddate(6:7)//NoahmpIO%olddate(9:10)//NoahmpIO%olddate(12:13)//&
          ".LDASIN_DOMAIN"//NoahmpIO%hgrid

     ! Build a filename template
     NoahmpIO%inflnm_template = trim(NoahmpIO%indir)//"/<date>.LDASIN_DOMAIN"//NoahmpIO%hgrid

#ifdef MPP_LAND
     call mpp_land_bcast_char(19,NoahmpIO%OLDDATE(1:19))
#endif

#ifdef WRF_HYDRO

#ifdef MPP_LAND
     call mpp_land_bcast_char(19,forcDate(1:19))
#endif
     if(finemesh .ne. 0) goto 991

     if(forc_typ .eq. 0) then
        call READFORC_HRLDAS(NoahmpIO%INFLNM_TEMPLATE, NoahmpIO%FORCING_TIMESTEP,    &
                             NoahmpIO%OLDDATE,NoahmpIO%XSTART, NoahmpIO%XEND,        &
                             NoahmpIO%YSTART, NoahmpIO%YEND,                         &
                             NoahmpIO%forcing_name_T, NoahmpIO%forcing_name_Q,       &
                             NoahmpIO%forcing_name_U, NoahmpIO%forcing_name_V,       &
                             NoahmpIO%forcing_name_P, NoahmpIO%forcing_name_LW,      &
                             NoahmpIO%forcing_name_SW,NoahmpIO%forcing_name_PR,      &
                             NoahmpIO%forcing_name_SN,NoahmpIO%T_PHY(:,1,:),         &
                             NoahmpIO%QV_CURR(:,1,:),NoahmpIO%U_PHY(:,1,:),          &
                             NoahmpIO%V_PHY(:,1,:),NoahmpIO%P8W(:,1,:), NoahmpIO%GLW,&
                             NoahmpIO%SWDOWN, NoahmpIO%RAINBL, NoahmpIO%SNOWBL,      &
                             NoahmpIO%VEGFRA, NoahmpIO%update_veg, NoahmpIO%LAI,     &
                             NoahmpIO%update_lai, NoahmpIO%reset_spinup_date,        &
                             NoahmpIO%startdate)
                             
             NoahmpIO%VEGFRA = NoahmpIO%VEGFRA * 100.0
     else
        if(NoahmpIO%olddate == forcDate) then
           call HYDRO_frocing_drv(trim(NoahmpIO%indir), NoahmpIO%forc_typ,NoahmpIO%snow_assim,NoahmpIO%olddate, &
                                       NoahmpIO%xstart, NoahmpIO%xend, NoahmpIO%ystart, NoahmpIO%yend,          &
                                       NoahmpIO%T_PHY(:,1,:),NoahmpIO%QV_CURR(:,1,:),NoahmpIO%U_PHY(:,1,:),     &
                                       NoahmpIO%V_PHY(:,1,:),NoahmpIO%P8W(:,1,:), NoahmpIO%GLW,                 &   
                                       NoahmpIO%SWDOWN,NoahmpIO%RAINBL,NoahmpIO%LAI,NoahmpIO%VEGFRA,            &
                                       NoahmpIO%SNOWH,ITIME,NoahmpIO%FORCING_TIMESTEP,NoahmpIO%prcp0)

           call geth_newdate(NoahmpIO%newdate, forcDate, NoahmpIO%FORCING_TIMESTEP)
           forcDate = NoahmpIO%newdate
        endif
     endif

#else
     call READFORC_HRLDAS(NoahmpIO%INFLNM_TEMPLATE, NoahmpIO%FORCING_TIMESTEP,                               &
                          NoahmpIO%OLDDATE, NoahmpIO%XSTART, NoahmpIO%XEND, NoahmpIO%YSTART, NoahmpIO%YEND,  &
                          NoahmpIO%forcing_name_T,NoahmpIO%forcing_name_Q,NoahmpIO%forcing_name_U,           &
                          NoahmpIO%forcing_name_V,NoahmpIO%forcing_name_P,NoahmpIO%forcing_name_LW,          &
                          NoahmpIO%forcing_name_SW,NoahmpIO%forcing_name_PR,NoahmpIO%forcing_name_SN,        &
                          NoahmpIO%T_PHY(:,1,:),NoahmpIO%QV_CURR(:,1,:),NoahmpIO%U_PHY(:,1,:),               &
                          NoahmpIO%V_PHY(:,1,:),NoahmpIO%P8W(:,1,:), NoahmpIO%GLW, NoahmpIO%SWDOWN,          &
                          NoahmpIO%RAINBL, NoahmpIO%SNOWBL, NoahmpIO%VEGFRA, NoahmpIO%update_veg,            &
                          NoahmpIO%LAI, NoahmpIO%update_lai, NoahmpIO%reset_spinup_date, NoahmpIO%startdate)
          
       if(maxval(NoahmpIO%VEGFRA) <= 1 ) then
           NoahmpIO%VEGFRA = NoahmpIO%VEGFRA * 100.   ! added for input vegfra as a fraction (0~1)
       endif

#endif

991  continue

     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%T_PHY(:,1,:)   = 0.0 ! Prevent some overflow problems with ifort compiler [MB:20150812]
     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%U_PHY(:,1,:)   = 0.0
     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%V_PHY(:,1,:)   = 0.0
     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%QV_CURR(:,1,:) = 0.0
     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%P8W(:,1,:)     = 0.0
     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%GLW            = 0.0
     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%SWDOWN         = 0.0
     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%RAINBL         = 0.0
     where(NoahmpIO%XLAND > 1.5)   NoahmpIO%SNOWBL         = 0.0

     NoahmpIO%QV_CURR(:,1,:) = NoahmpIO%QV_CURR(:,1,:)/&
                            (1.0 - NoahmpIO%QV_CURR(:,1,:))     ! Assuming input forcing are specific hum.;
                                                                ! WRF wants mixing ratio at driver level
     NoahmpIO%P8W(:,2,:)     = NoahmpIO%P8W(:,1,:)              ! WRF uses lowest two layers
     NoahmpIO%T_PHY(:,2,:)   = NoahmpIO%T_PHY(:,1,:)            ! Only pressure is needed in two layer but fill the rest
     NoahmpIO%U_PHY(:,2,:)   = NoahmpIO%U_PHY(:,1,:)            ! 
     NoahmpIO%V_PHY(:,2,:)   = NoahmpIO%V_PHY(:,1,:)            ! 
     NoahmpIO%QV_CURR(:,2,:) = NoahmpIO%QV_CURR(:,1,:)          ! 
     NoahmpIO%RAINBL         = NoahmpIO%RAINBL * NoahmpIO%DTBL  ! RAINBL in WRF is [mm]
     NoahmpIO%SNOWBL         = NoahmpIO%SNOWBL * NoahmpIO%DTBL  ! 
     NoahmpIO%SR             = 0.0                              ! Will only use component if opt_snf=4
     NoahmpIO%RAINCV         = 0.0
     NoahmpIO%RAINNCV        = NoahmpIO%RAINBL
     NoahmpIO%RAINSHV        = 0.0
     NoahmpIO%SNOWNCV        = NoahmpIO%SNOWBL
     NoahmpIO%GRAUPELNCV     = 0.0
     NoahmpIO%HAILNCV        = 0.0
     NoahmpIO%DZ8W           = 2*NoahmpIO%ZLVL                  ! 2* to be consistent with WRF model level

     NoahmpIO%SWDDIR = NoahmpIO%SWDOWN * 0.7                    ! following noahmplsm ATM 70% direct radiation
     NoahmpIO%SWDDIF = NoahmpIO%SWDOWN * 0.3                    ! following noahmplsm ATM 30% diffuse radiation

     IF(NoahmpIO%SF_URBAN_PHYSICS == 2 .or. NoahmpIO%SF_URBAN_PHYSICS == 3) THEN  ! BEP or BEM urban models
       where(NoahmpIO%XLAND < 1.5) NoahmpIO%p_urban    (:,NoahmpIO%kde,:) = NoahmpIO%p8w(:,1,:)
       where(NoahmpIO%XLAND < 1.5) NoahmpIO%rho_urban  (:,NoahmpIO%kde,:) = NoahmpIO%p8w(:,1,:) /287.04/NoahmpIO%t_phy(:,1,:)
       where(NoahmpIO%XLAND < 1.5) NoahmpIO%theta_urban(:,NoahmpIO%kde,:) = NoahmpIO%t_phy(:,1,:)*&
                                                                            (100000.0/NoahmpIO%p8w(:,1,:))**(0.285714)
       where(NoahmpIO%XLAND < 1.5) NoahmpIO%u_urban    (:,NoahmpIO%kde,:) = NoahmpIO%u_phy(:,1,:)
       where(NoahmpIO%XLAND < 1.5) NoahmpIO%v_urban    (:,NoahmpIO%kde,:) = NoahmpIO%v_phy(:,1,:)

       ! allow one more dummy layer at the top to be consistent with cell interface array dimension
       NoahmpIO%p_urban    (:,NoahmpIO%kde-1,:) = NoahmpIO%p_urban(:,NoahmpIO%kde,:)
       NoahmpIO%rho_urban  (:,NoahmpIO%kde-1,:) = NoahmpIO%rho_urban(:,NoahmpIO%kde,:)
       NoahmpIO%theta_urban(:,NoahmpIO%kde-1,:) = NoahmpIO%theta_urban(:,NoahmpIO%kde,:)
       NoahmpIO%u_urban    (:,NoahmpIO%kde-1,:) = NoahmpIO%u_urban(:,NoahmpIO%kde,:)
       NoahmpIO%v_urban    (:,NoahmpIO%kde-1,:) = NoahmpIO%v_urban(:,NoahmpIO%kde,:)

       NoahmpIO%height_urban = 0.5*(NoahmpIO%dz_urban(1,NoahmpIO%kde-1,1) + &
                               NoahmpIO%dz_urban(1,NoahmpIO%kde-2,1))
       
       do i = 3, NoahmpIO%kde
           where(NoahmpIO%XLAND < 1.5) NoahmpIO%QV_CURR(:,i,:) = NoahmpIO%QV_CURR(:,1,:)
       enddo

       DO I = NoahmpIO%kde-2, NoahmpIO%kds, -1
         where(NoahmpIO%XLAND < 1.5) NoahmpIO%p_urban(:,i,:) = NoahmpIO%p8w(:,1,:)* &
                                                               exp(9.8*NoahmpIO%height_urban/287.04/NoahmpIO%t_phy(:,1,:))
         where(NoahmpIO%XLAND < 1.5) NoahmpIO%rho_urban(:,i,:) = NoahmpIO%p_urban(:,i,:)/287.04/NoahmpIO%t_phy(:,1,:)
         where(NoahmpIO%XLAND < 1.5) NoahmpIO%theta_urban(:,i,:) = NoahmpIO%t_phy(:,1,:)* &
                                                                   (100000.0/NoahmpIO%p_urban(:,i,:))**(0.285714)
         where(NoahmpIO%XLAND < 1.5) NoahmpIO%u_urban(:,i,:) = NoahmpIO%u_urban(:,NoahmpIO%kde,:)* &
                                                               log(max(NoahmpIO%zlvl-NoahmpIO%height_urban,1.001))/log(NoahmpIO%zlvl)
         where(NoahmpIO%XLAND < 1.5) NoahmpIO%v_urban(:,i,:) = NoahmpIO%v_urban(:,NoahmpIO%kde,:)* &
                                                               log(max(NoahmpIO%zlvl-NoahmpIO%height_urban,1.001))/log(NoahmpIO%zlvl)
         NoahmpIO%height_urban = NoahmpIO%height_urban + NoahmpIO%urban_atmosphere_thickness
       END DO    
     ENDIF
  
!------------------------------------------------------------------------
! Noah-MP updates we can do before spatial loop.
!------------------------------------------------------------------------

   ! create a few fields that are IN in WRF - coszen, julian,yr

    DO J = NoahmpIO%YSTART,NoahmpIO%YEND
    DO I = NoahmpIO%XSTART,NoahmpIO%XEND
      IF(NoahmpIO%SF_URBAN_PHYSICS == 0) THEN
        call CALC_DECLIN(NoahmpIO%OLDDATE(1:19),NoahmpIO%XLAT(I,J), NoahmpIO%XLONG(I,J), &
                         NoahmpIO%COSZEN(I,J),NoahmpIO%JULIAN)
      ELSE
        call CALC_DECLIN(NoahmpIO%OLDDATE(1:19),NoahmpIO%XLAT(I,J), NoahmpIO%XLONG(I,J), &
                         NoahmpIO%COSZEN(I,J),NoahmpIO%JULIAN,NoahmpIO%HRANG(I,J),       &
                         NoahmpIO%DECLIN, NoahmpIO%GMT, NoahmpIO%JULDAY)
      ENDIF
      IF(NoahmpIO%IOPT_IRR > 0 ) THEN
         call LOCAL_TIME(NoahmpIO%OLDDATE(1:19), NoahmpIO%XLONG(I,J), NoahmpIO%LOCTIM(I,J)) 
      ENDIF
    END DO
    END DO

    READ(NoahmpIO%OLDDATE(1:4),*)  NoahmpIO%YR
    NoahmpIO%YEARLEN = 365                      ! find length of year for phenology (also S Hemisphere)
    if (mod(NoahmpIO%YR,4) == 0) then
       NoahmpIO%YEARLEN = 366
       if (mod(NoahmpIO%YR,100) == 0) then
          NoahmpIO%YEARLEN = 365
          if (mod(NoahmpIO%YR,400) == 0) then
             NoahmpIO%YEARLEN = 366
          endif
       endif
    endif

    IF (ITIME == 1 .AND. .NOT. NoahmpIO%RESTART_FLAG ) THEN
      NoahmpIO%EAHXY = (NoahmpIO%P8W(:,1,:)*NoahmpIO%QV_CURR(:,1,:))/(0.622+NoahmpIO%QV_CURR(:,1,:)) ! Initial guess only.
      NoahmpIO%TAHXY = NoahmpIO%T_PHY(:,1,:)                                                         ! Initial guess only.
      NoahmpIO%CHXY = 0.1
      NoahmpIO%CMXY = 0.1
    ENDIF

!------------------------------------------------------------------------
! Skip model call at t=1 since initial conditions are at start time; First model time is +1
!------------------------------------------------------------------------

    IF (ITIME > 0) THEN

         NoahmpIO%MP_RAINC  = NoahmpIO%RAINCV 
         NoahmpIO%MP_RAINNC = NoahmpIO%RAINNCV 
         NoahmpIO%MP_SHCV   = NoahmpIO%RAINSHV
         NoahmpIO%MP_SNOW   = NoahmpIO%SNOWNCV
         NoahmpIO%MP_GRAUP  = NoahmpIO%GRAUPELNCV
         NoahmpIO%MP_HAIL   = NoahmpIO%HAILNCV 
         
!------------------------------------------------------------------------
! Call to Noah-MP driver same as surface_driver
!------------------------------------------------------------------------

         NoahmpIO%sflx_count_sum = 0      ! Timing
         NoahmpIO%ITIMESTEP      = ITIME  ! update time step counter

         ! Timing information for SFLX:
    
         call system_clock(count=NoahmpIO%count_before_sflx, count_rate=NoahmpIO%clock_rate)

         call NoahmpDriverMain(NoahmpIO)                                       

         call system_clock(count=NoahmpIO%count_after_sflx, count_rate=NoahmpIO%clock_rate)
         NoahmpIO%sflx_count_sum = NoahmpIO%sflx_count_sum + ( NoahmpIO%count_after_sflx - NoahmpIO%count_before_sflx )

         IF(NoahmpIO%SF_URBAN_PHYSICS == 2 .or. NoahmpIO%SF_URBAN_PHYSICS == 3) THEN  ! BEP or BEM urban models
           NoahmpIO%dz8w  = NoahmpIO%dz_urban
           NoahmpIO%u_phy = NoahmpIO%u_urban
           NoahmpIO%v_phy = NoahmpIO%v_urban
         ENDIF

         IF(NoahmpIO%SF_URBAN_PHYSICS > 0 ) THEN  !urban
         
            call noahmp_urban(NoahmpIO, NoahmpIO%sf_urban_physics, NoahmpIO%NSOIL, NoahmpIO%IVGTYP,             & ! IN : Model configuration 
                            NoahmpIO%ITIMESTEP, NoahmpIO%DTBL, NoahmpIO%COSZEN, NoahmpIO%XLAT,                  & ! IN : Time/Space-related
                            NoahmpIO%T_PHY, NoahmpIO%QV_CURR, NoahmpIO%U_PHY, NoahmpIO%V_PHY, NoahmpIO%SWDOWN,  & ! IN : Forcing
                            NoahmpIO%SWDDIR, NoahmpIO%SWDDIF,                                                   &
                            NoahmpIO%GLW, NoahmpIO%P8W, NoahmpIO%RAINBL, NoahmpIO%DZ8W, NoahmpIO%ZNT,           & ! IN : Forcing
                            NoahmpIO%TSK, NoahmpIO%HFX, NoahmpIO%QFX, NoahmpIO%LH, NoahmpIO%GRDFLX,             & ! IN/OUT : LSM 
                            NoahmpIO%ALBEDO, NoahmpIO%EMISS, NoahmpIO%QSFC,                                     & ! IN/OUT : LSM 
                            NoahmpIO%ids, NoahmpIO%ide, NoahmpIO%jds, NoahmpIO%jde, NoahmpIO%kds, NoahmpIO%kde, &
                            NoahmpIO%ims, NoahmpIO%ime, NoahmpIO%jms, NoahmpIO%jme, NoahmpIO%kms, NoahmpIO%kme, &
                            NoahmpIO%its, NoahmpIO%ite, NoahmpIO%jts, NoahmpIO%jte, NoahmpIO%kts, NoahmpIO%kte, &
                            NoahmpIO%cmr_sfcdif, NoahmpIO%chr_sfcdif, NoahmpIO%cmc_sfcdif,                      &
                            NoahmpIO%chc_sfcdif, NoahmpIO%cmgr_sfcdif, NoahmpIO%chgr_sfcdif,                    &
                            NoahmpIO%tr_urb2d, NoahmpIO%tb_urb2d, NoahmpIO%tg_urb2d,                            & !H urban
                            NoahmpIO%tc_urb2d, NoahmpIO%qc_urb2d, NoahmpIO%uc_urb2d,                            & !H urban
                            NoahmpIO%xxxr_urb2d, NoahmpIO%xxxb_urb2d, NoahmpIO%xxxg_urb2d,                      & !H urban
                            NoahmpIO%xxxc_urb2d, NoahmpIO%trl_urb3d, NoahmpIO%tbl_urb3d, NoahmpIO%tgl_urb3d,    & !H urban
                            NoahmpIO%sh_urb2d, NoahmpIO%lh_urb2d, NoahmpIO%g_urb2d, NoahmpIO%rn_urb2d,          & !H urban
                            NoahmpIO%ts_urb2d, NoahmpIO%psim_urb2d,                                             &
                            NoahmpIO%psih_urb2d, NoahmpIO%u10_urb2d, NoahmpIO%v10_urb2d,                        & !O urban
                            NoahmpIO%GZ1OZ0_urb2d, NoahmpIO%AKMS_URB2D,                                         & !O urban
                            NoahmpIO%th2_urb2d, NoahmpIO%q2_urb2d, NoahmpIO%ust_urb2d,                          & !O urban
                            NoahmpIO%declin, NoahmpIO%hrang,                                                    & !I urban
                            NoahmpIO%num_roof_layers, NoahmpIO%num_wall_layers, NoahmpIO%num_road_layers,       & !I urban
                            NoahmpIO%dzr, NoahmpIO%dzb, NoahmpIO%dzg,                                           & !I urban
                            NoahmpIO%cmcr_urb2d, NoahmpIO%tgr_urb2d, NoahmpIO%tgrl_urb3d, NoahmpIO%smr_urb3d,   & !H urban
                            NoahmpIO%drelr_urb2d, NoahmpIO%drelb_urb2d, NoahmpIO%drelg_urb2d,                   & !H urban
                            NoahmpIO%flxhumr_urb2d, NoahmpIO%flxhumb_urb2d, NoahmpIO%flxhumg_urb2d,             & !H urban
                            NoahmpIO%julday, NoahmpIO%yr,                                                       & !H urban
                            NoahmpIO%FRC_URB2D, NoahmpIO%utype_urb2d,                                           & !I urban
                            NoahmpIO%chs, NoahmpIO%chs2, NoahmpIO%cqs2,                                          & !H
                            NoahmpIO%num_urban_ndm, NoahmpIO%urban_map_zrd, NoahmpIO%urban_map_zwd,             & !I multi-layer urban
                            NoahmpIO%urban_map_gd,                                                              & !I multi-layer urban 
                            NoahmpIO%urban_map_zd, NoahmpIO%urban_map_zdf, NoahmpIO%urban_map_bd,               & !I multi-layer urban
                            NoahmpIO%urban_map_wd, NoahmpIO%urban_map_gbd, NoahmpIO%urban_map_fbd,              & !I multi-layer urban
                            NoahmpIO%urban_map_zgrd, NoahmpIO%num_urban_hi,                                     & !I multi-layer urban
                            NoahmpIO%trb_urb4d, NoahmpIO%tw1_urb4d, NoahmpIO%tw2_urb4d, NoahmpIO%tgb_urb4d,     & !H multi-layer urban
                            NoahmpIO%tlev_urb3d, NoahmpIO%qlev_urb3d,                                           & !H multi-layer urban
                            NoahmpIO%tw1lev_urb3d, NoahmpIO%tw2lev_urb3d,                                       & !H multi-layer urban
                            NoahmpIO%tglev_urb3d, NoahmpIO%tflev_urb3d,                                         & !H multi-layer urban
                            NoahmpIO%sf_ac_urb3d, NoahmpIO%lf_ac_urb3d, NoahmpIO%cm_ac_urb3d,                   & !H multi-layer urban
                            NoahmpIO%sfvent_urb3d, NoahmpIO%lfvent_urb3d,                                       & !H multi-layer urban
                            NoahmpIO%sfwin1_urb3d, NoahmpIO%sfwin2_urb3d,                                       & !H multi-layer urban
                            NoahmpIO%sfw1_urb3d, NoahmpIO%sfw2_urb3d, NoahmpIO%sfr_urb3d, NoahmpIO%sfg_urb3d,   & !H multi-layer urban
                            NoahmpIO%ep_pv_urb3d, NoahmpIO%t_pv_urb3d,                                          & !RMS
                            NoahmpIO%trv_urb4d, NoahmpIO%qr_urb4d, NoahmpIO%qgr_urb3d, NoahmpIO%tgr_urb3d,      & !RMS
                            NoahmpIO%drain_urb4d, NoahmpIO%draingr_urb3d, NoahmpIO%sfrv_urb3d,                  & !RMS 
                            NoahmpIO%lfrv_urb3d,                                                                & !RMS
                            NoahmpIO%dgr_urb3d, NoahmpIO%dg_urb3d, NoahmpIO%lfr_urb3d, NoahmpIO%lfg_urb3d,      & !RMS
                            NoahmpIO%lp_urb2d, NoahmpIO%hi_urb2d, NoahmpIO%lb_urb2d, NoahmpIO%hgt_urb2d,        & !H multi-layer urban
                            NoahmpIO%mh_urb2d, NoahmpIO%stdh_urb2d, NoahmpIO%lf_urb2d,                          & !SLUCM
                            NoahmpIO%theta_urban, NoahmpIO%rho_urban, NoahmpIO%p_urban, NoahmpIO%ust,           & !I multi-layer urban
                            NoahmpIO%gmt, NoahmpIO%julday, NoahmpIO%XLONG, NoahmpIO%XLAT,                       & !I multi-layer urban
                            NoahmpIO%a_u_bep, NoahmpIO%a_v_bep, NoahmpIO%a_t_bep, NoahmpIO%a_q_bep,             & !O multi-layer urban
                            NoahmpIO%a_e_bep, NoahmpIO%b_u_bep, NoahmpIO%b_v_bep,                               & !O multi-layer urban
                            NoahmpIO%b_t_bep, NoahmpIO%b_q_bep, NoahmpIO%b_e_bep, NoahmpIO%dlg_bep,             & !O multi-layer urban
                            NoahmpIO%dl_u_bep, NoahmpIO%sf_bep, NoahmpIO%vl_bep)                                  !O multi-layer urban
                       
         ENDIF 

         if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
            if ( mod(ITIME,NoahmpIO%STEPWTD) == 0 ) then
               call wrf_message('calling WTABLE')

               !gmm update wtable from lateral flow and shed water to rivers
               call WTABLE_MMF_NOAHMP(NoahmpIO,                                             &
                                  NoahmpIO%NSOIL, NoahmpIO%XLAND, NoahmpIO%XICE,            & 
                                  NoahmpIO%XICE_THRESHOLD, NoahmpIO%ISICE,                  &
                                  NoahmpIO%ISLTYP, NoahmpIO%SMOISEQ, NoahmpIO%DZS,          &       
                                  NoahmpIO%WTDDT, NoahmpIO%FDEPTHXY, NoahmpIO%AREAXY,       &
                                  NoahmpIO%TERRAIN, NoahmpIO%ISURBAN, NoahmpIO%IVGTYP,      &
                                  NoahmpIO%RIVERCONDXY, NoahmpIO%RIVERBEDXY, NoahmpIO%EQZWT,& 
                                  NoahmpIO%PEXPXY, NoahmpIO%SMOIS, NoahmpIO%SH2O,           & 
                                  NoahmpIO%SMCWTDXY, NoahmpIO%ZWTXY, NoahmpIO%QLATXY,       & 
                                  NoahmpIO%QRFXY, NoahmpIO%DEEPRECHXY, NoahmpIO%QSPRINGXY,  &
                                  NoahmpIO%QSLATXY, NoahmpIO%QRFSXY, NoahmpIO%QSPRINGSXY,   &  
                                  NoahmpIO%RECHXY,                                          &
                                  NoahmpIO%IDS, NoahmpIO%IDE, NoahmpIO%JDS, NoahmpIO%JDE,   &
                                  NoahmpIO%KDS, NoahmpIO%KDE, NoahmpIO%IMS, NoahmpIO%IME,   &
                                  NoahmpIO%JMS,NoahmpIO%JME, NoahmpIO%KMS,NoahmpIO%KME,     &
                                  NoahmpIO%ITS,NoahmpIO%ITE, NoahmpIO%JTS,NoahmpIO%JTE,     &
                                  NoahmpIO%KTS,NoahmpIO%KTE)
            endif ! MMF timestep
         endif ! MMF groundwater

!------------------------------------------------------------------------
! END of surface_driver consistent code
!------------------------------------------------------------------------

    ENDIF   ! ITIME > 0: SKIP FIRST TIMESTEP

! Output for history
    OUTPUT_FOR_HISTORY: if (NoahmpIO%output_timestep > 0) then
        if (mod(ITIME*NoahmpIO%noah_timestep, NoahmpIO%output_timestep) == 0 &
            .and. .not. NoahmpIO%skip_first_output ) then

           call prepare_output_file (trim(NoahmpIO%outdir), version,                                   &
                                     NoahmpIO%igrid, NoahmpIO%output_timestep, NoahmpIO%llanduse,      &
                                     NoahmpIO%split_output_count, NoahmpIO%hgrid,                      &
                                     NoahmpIO%ixfull, NoahmpIO%jxfull, NoahmpIO%ixpar, NoahmpIO%jxpar, &
                                     NoahmpIO%xstartpar, NoahmpIO%ystartpar,                           &
                                     NoahmpIO%iswater, NoahmpIO%mapproj, NoahmpIO%lat1, NoahmpIO%lon1, &
                                     NoahmpIO%dx, NoahmpIO%dy, NoahmpIO%truelat1, NoahmpIO%truelat2,   &
                                     NoahmpIO%cen_lon, NoahmpIO%NSOIL, NoahmpIO%nsnow, NoahmpIO%dzs,   &
                                     NoahmpIO%startdate, NoahmpIO%olddate, NoahmpIO%spinup_loop,       &
                                     NoahmpIO%spinup_loops, NoahmpIO%IVGTYP, NoahmpIO%ISLTYP)

           DEFINE_MODE_LOOP : do imode = 1, 2

              call set_output_define_mode(imode)

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
            ! Snow - 2D terms
              call add_to_output(NoahmpIO%SNOWH      , "SNOWH"   , "Snow depth"                           , "m"                     )
              call add_to_output(NoahmpIO%SNOW       , "SNEQV"   , "Snow water equivalent"                , "mm"                    )
              call add_to_output(NoahmpIO%QSNOWXY    , "QSNOW"   , "Snowfall rate on the ground"          , "mm/s"                  )
              call add_to_output(NoahmpIO%QRAINXY    , "QRAIN"   , "Rainfall rate on the ground"          , "mm/s"                  )
              call add_to_output(NoahmpIO%ISNOWXY    , "ISNOW"   , "Number of snow layers"                , "-"                     )
              call add_to_output(NoahmpIO%SNOWC      , "FSNO"    , "Snow-cover fraction on the ground"    , "-"                     )
              call add_to_output(NoahmpIO%ACSNOW     , "ACSNOW"  , "accumulated snow fall"                , "mm"                    )
              call add_to_output(NoahmpIO%ACSNOM     , "ACSNOM"  , "accumulated snow melt water"          , "mm"                    )
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
              call add_to_output(NoahmpIO%QSNBOTXY   , "QSNBOT"  , "water (melt+rain through) out of snow bottom"   , "mm/s"        )
              call add_to_output(NoahmpIO%QMELTXY    , "QMELT"   , "snow melt due to phase change"                  , "mm/s"        )
              call add_to_output(NoahmpIO%PONDINGXY  , "PONDING" , "total surface ponding per time step"            , "mm/s"        )
              call add_to_output(NoahmpIO%FPICEXY    , "FPICE"   , "snow fraction in precipitation"                 , "-"           )
              call add_to_output(NoahmpIO%ACC_QINSURXY,"ACC_QINSUR", "accumuated water flux to soil within soil timestep"     , "m/s*dt_soil/dt_main")
              call add_to_output(NoahmpIO%ACC_QSEVAXY ,"ACC_QSEVA" , "accumulated soil surface evap rate within soil timestep", "m/s*dt_soil/dt_main")
              call add_to_output(NoahmpIO%ACC_ETRANIXY,"ACC_ETRANI", "accumualted transpiration rate within soil timestep"    , "m/s*dt_soil/dt_main","SOIL")
              call add_to_output(NoahmpIO%ACC_DWATERXY,"ACC_DWATER", "accumulated water storage change within soil timestep"  , "mm")
              call add_to_output(NoahmpIO%ACC_PRCPXY  ,"ACC_PRCP"  , "accumulated precipitation within soil timestep"         , "mm")
              call add_to_output(NoahmpIO%ACC_ECANXY  ,"ACC_ECAN"  , "accumulated net canopy evaporation within soil timestep", "mm")
              call add_to_output(NoahmpIO%ACC_ETRANXY ,"ACC_ETRAN" , "accumulated transpiration within soil timestep"         , "mm")
              call add_to_output(NoahmpIO%ACC_EDIRXY  ,"ACC_EDIR"  , "accumulated net ground evaporation within soil timestep", "mm")
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
           
           ! For now, no urban output variables included

           enddo DEFINE_MODE_LOOP

           call finalize_output_file(NoahmpIO%split_output_count,itime)
                   
        endif
   
        if(NoahmpIO%skip_first_output) NoahmpIO%skip_first_output = .false.

     endif OUTPUT_FOR_HISTORY
 
     if (NoahmpIO%IVGTYP(NoahmpIO%xstart,NoahmpIO%ystart)==NoahmpIO%ISWATER) then
       write(*,'(" ***DATE=", A19)', advance="NO") NoahmpIO%olddate
     else
       write(*,'(" ***DATE=", A19, 6F10.5)', advance="NO")     &
       NoahmpIO%olddate, NoahmpIO%TSLB(NoahmpIO%xstart,1,NoahmpIO%ystart), &
       NoahmpIO%LAI(NoahmpIO%xstart,NoahmpIO%ystart)
     endif

!------------------------------------------------------------------------
! Write Restart - timestamp equal to output will have same states
!------------------------------------------------------------------------

      if ( (NoahmpIO%restart_frequency_hours .gt. 0) .and. &
           (mod(ITIME, int(NoahmpIO%restart_frequency_hours*3600./nint(NoahmpIO%dtbl))) == 0)) then
       call lsm_restart()

      else

       if (NoahmpIO%restart_frequency_hours <= 0) then
          if ( (NoahmpIO%olddate( 9:10) == "01") .and. (NoahmpIO%olddate(12:13) == "00") .and. &
               (NoahmpIO%olddate(15:16) == "00") .and. (NoahmpIO%olddate(18:19) == "00") ) then
                call lsm_restart()  ! jlm - i moved all the restart code to a subroutine. 
          endif
       endif
      endif

!------------------------------------------------------------------------
! Advance the time 
!------------------------------------------------------------------------

     call geth_newdate(NoahmpIO%newdate, NoahmpIO%olddate, nint(NoahmpIO%dtbl))
     NoahmpIO%olddate = NoahmpIO%newdate

     if(itime > 0 .and. NoahmpIO%spinup_loops > 0 .and. mod(itime,NoahmpIO%ntime/(NoahmpIO%spinup_loops+1)) == 0) then
       NoahmpIO%spinup_loop = NoahmpIO%spinup_loop + 1
       call geth_newdate(NoahmpIO%olddate, NoahmpIO%startdate, nint(NoahmpIO%dtbl))
       NoahmpIO%reset_spinup_date = .true.
     end if

! update the timer
     call system_clock(count=NoahmpIO%clock_count_2, count_rate=NoahmpIO%clock_rate)
     NoahmpIO%timing_sum = NoahmpIO%timing_sum + &
                        float(NoahmpIO%clock_count_2-NoahmpIO%clock_count_1)/float(NoahmpIO%clock_rate)
     write(*,'("    Timing: ",f6.2," Cumulative:  ", f10.2, "  SFLX: ", f6.2 )') &
          float(NoahmpIO%clock_count_2-NoahmpIO%clock_count_1)/float(NoahmpIO%clock_rate), &
          NoahmpIO%timing_sum, real(NoahmpIO%sflx_count_sum) / real(NoahmpIO%clock_rate)
    
     NoahmpIO%clock_count_1 = NoahmpIO%clock_count_2

#ifdef WRF_HYDRO
      call hrldas_drv_HYDRO(NoahmpIO%TSLB(:,1:NoahmpIO%NSOIL,:),NoahmpIO%SMOIS(:,1:NoahmpIO%NSOIL,:), &
                            NoahmpIO%SH2O(:,1:NoahmpIO%NSOIL,:),NoahmpIO%infxsrt,                     &
                            NoahmpIO%sfcheadrt,NoahmpIO%soldrain,NoahmpIO%qtiledrain,                 &
                            NoahmpIO%ZWATBLE2D,NoahmpIO%ix,NoahmpIO%jx,NoahmpIO%NSOIL) 
#endif

end subroutine land_driver_exe

!!===============================================================================
subroutine lsm_restart()
  implicit none 
  
  print*, 'Write restart at '//NoahmpIO%olddate(1:13)

  call prepare_restart_file (trim(NoahmpIO%outdir), version, NoahmpIO%igrid, NoahmpIO%llanduse,       &
                             NoahmpIO%olddate, NoahmpIO%startdate, NoahmpIO%ixfull, NoahmpIO%jxfull,  &
                             NoahmpIO%ixpar, NoahmpIO%jxpar, NoahmpIO%xstartpar, NoahmpIO%ystartpar,  &                               
                             NoahmpIO%NSOIL, NoahmpIO%nsnow, NoahmpIO%max_urban_dim, NoahmpIO%dx,     &
                             NoahmpIO%dy, NoahmpIO%truelat1, NoahmpIO%truelat2, NoahmpIO%mapproj,     &
                             NoahmpIO%lat1, NoahmpIO%lon1, NoahmpIO%cen_lon, NoahmpIO%iswater, NoahmpIO%ivgtyp)
  
  call add_to_restart(NoahmpIO%TSLB      , "SOIL_T", layers="SOIL")
  call add_to_restart(NoahmpIO%TSNOXY    , "SNOW_T", layers="SNOW")
  call add_to_restart(NoahmpIO%SMOIS     , "SMC"   , layers="SOIL")
  call add_to_restart(NoahmpIO%SH2O      , "SH2O"  , layers="SOIL")
  call add_to_restart(NoahmpIO%ZSNSOXY   , "ZSNSO" , layers="SOSN")
  call add_to_restart(NoahmpIO%SNICEXY   , "SNICE" , layers="SNOW")
  call add_to_restart(NoahmpIO%SNLIQXY   , "SNLIQ" , layers="SNOW")
  call add_to_restart(NoahmpIO%FWETXY    , "FWET"  )
  call add_to_restart(NoahmpIO%SNEQVOXY  , "SNEQVO")
  call add_to_restart(NoahmpIO%EAHXY     , "EAH"   )
  call add_to_restart(NoahmpIO%TAHXY     , "TAH"   )
  call add_to_restart(NoahmpIO%ALBOLDXY  , "ALBOLD")
  call add_to_restart(NoahmpIO%CMXY      , "CM"    )
  call add_to_restart(NoahmpIO%CHXY      , "CH"    )
  call add_to_restart(NoahmpIO%ISNOWXY   , "ISNOW" )
  call add_to_restart(NoahmpIO%CANLIQXY  , "CANLIQ")
  call add_to_restart(NoahmpIO%CANICEXY  , "CANICE")
  call add_to_restart(NoahmpIO%SNOW      , "SNEQV" )
  call add_to_restart(NoahmpIO%SNOWH     , "SNOWH" )
  call add_to_restart(NoahmpIO%TVXY      , "TV"    )
  call add_to_restart(NoahmpIO%TGXY      , "TG"    )
  call add_to_restart(NoahmpIO%ZWTXY     , "ZWT"   )
  call add_to_restart(NoahmpIO%WAXY      , "WA"    )
  call add_to_restart(NoahmpIO%WTXY      , "WT"    )
  call add_to_restart(NoahmpIO%WSLAKEXY  , "WSLAKE")
  call add_to_restart(NoahmpIO%LFMASSXY  , "LFMASS")
  call add_to_restart(NoahmpIO%RTMASSXY  , "RTMASS")
  call add_to_restart(NoahmpIO%STMASSXY  , "STMASS")
  call add_to_restart(NoahmpIO%CROPCAT   , "CROPCAT")
  call add_to_restart(NoahmpIO%WOODXY    , "WOOD"  )
  call add_to_restart(NoahmpIO%GRAINXY   , "GRAIN" )
  call add_to_restart(NoahmpIO%GDDXY     , "GDD"   )
  call add_to_restart(NoahmpIO%STBLCPXY  , "STBLCP")
  call add_to_restart(NoahmpIO%FASTCPXY  , "FASTCP")
  call add_to_restart(NoahmpIO%LAI       , "LAI"   )
  call add_to_restart(NoahmpIO%XSAIXY    , "SAI"   )
  call add_to_restart(NoahmpIO%VEGFRA    , "VEGFRA")
  call add_to_restart(NoahmpIO%GVFMIN    , "GVFMIN")
  call add_to_restart(NoahmpIO%GVFMAX    , "GVFMAX")
  call add_to_restart(NoahmpIO%ACSNOM    , "ACMELT")
  call add_to_restart(NoahmpIO%ACSNOW    , "ACSNOW")
  call add_to_restart(NoahmpIO%TAUSSXY   , "TAUSS" )
  call add_to_restart(NoahmpIO%QSFC      , "QSFC"  )
  call add_to_restart(NoahmpIO%SFCRUNOFF , "SFCRUNOFF")
  call add_to_restart(NoahmpIO%UDRUNOFF  , "UDRUNOFF" )
  call add_to_restart(NoahmpIO%QTDRAIN   , "QTDRAIN"  )
  call add_to_restart(NoahmpIO%ACC_SSOILXY ,"ACC_SSOIL" )
  call add_to_restart(NoahmpIO%ACC_QINSURXY,"ACC_QINSUR")
  call add_to_restart(NoahmpIO%ACC_QSEVAXY ,"ACC_QSEVA" )
  call add_to_restart(NoahmpIO%ACC_ETRANIXY,"ACC_ETRANI", layers="SOIL")
  call add_to_restart(NoahmpIO%ACC_DWATERXY,"ACC_DWATER")
  call add_to_restart(NoahmpIO%ACC_PRCPXY  ,"ACC_PRCP"  )
  call add_to_restart(NoahmpIO%ACC_ECANXY  ,"ACC_ECAN"  )
  call add_to_restart(NoahmpIO%ACC_ETRANXY ,"ACC_ETRAN" )
  call add_to_restart(NoahmpIO%ACC_EDIRXY  ,"ACC_EDIR"  )
 ! irrigation scheme
  if ( NoahmpIO%IOPT_IRR > 0 ) then
     call add_to_restart(NoahmpIO%IRNUMSI   , "IRNUMSI")
     call add_to_restart(NoahmpIO%IRNUMMI   , "IRNUMMI")
     call add_to_restart(NoahmpIO%IRNUMFI   , "IRNUMFI")
     call add_to_restart(NoahmpIO%IRWATSI   , "IRWATSI")
     call add_to_restart(NoahmpIO%IRWATMI   , "IRWATMI")
     call add_to_restart(NoahmpIO%IRWATFI   , "IRWATFI")
     call add_to_restart(NoahmpIO%IRSIVOL   , "IRSIVOL") 
     call add_to_restart(NoahmpIO%IRMIVOL   , "IRMIVOL")
     call add_to_restart(NoahmpIO%IRFIVOL   , "IRFIVOL")
     call add_to_restart(NoahmpIO%IRELOSS   , "IRELOSS")
     call add_to_restart(NoahmpIO%IRRSPLH   , "IRRSPLH")
  endif
  ! below for MMF groundwater scheme
  if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
     call add_to_restart(NoahmpIO%SMOISEQ     , "SMOISEQ"  , layers="SOIL"  )
     call add_to_restart(NoahmpIO%AREAXY      , "AREAXY"     )
     call add_to_restart(NoahmpIO%SMCWTDXY    , "SMCWTDXY"   )
     call add_to_restart(NoahmpIO%DEEPRECHXY  , "DEEPRECHXY" )
     call add_to_restart(NoahmpIO%QSLATXY     , "QSLATXY"    )
     call add_to_restart(NoahmpIO%QRFSXY      , "QRFSXY"     )
     call add_to_restart(NoahmpIO%QSPRINGSXY  , "QSPRINGSXY" )
     call add_to_restart(NoahmpIO%RECHXY      , "RECHXY"     )
     call add_to_restart(NoahmpIO%QRFXY       , "QRFXY"      )
     call add_to_restart(NoahmpIO%QSPRINGXY   , "QSPRINGXY"  )
     call add_to_restart(NoahmpIO%FDEPTHXY    , "FDEPTHXY"  )
     call add_to_restart(NoahmpIO%RIVERCONDXY , "RIVERCONDXY"  )
     call add_to_restart(NoahmpIO%RIVERBEDXY  , "RIVERBEDXY"  )
     call add_to_restart(NoahmpIO%EQZWT       , "EQZWT"  )
     call add_to_restart(NoahmpIO%PEXPXY      , "PEXPXY"  )
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

  call finalize_restart_file()

end subroutine lsm_restart

!------------------------------------------------------------------------------------

SUBROUTINE CALC_DECLIN ( NOWDATE, LATITUDE, LONGITUDE, COSZ, JULIAN, &
                         HRANG_OUT, DECLIN_OUT, GMT_OUT, JULDAY_OUT )

  USE MODULE_DATE_UTILITIES
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------

! !ARGUMENTS:
   CHARACTER(LEN=19),                   INTENT(IN)  :: NOWDATE    ! YYYY-MM-DD_HH:MM:SS
   REAL(kind=kind_noahmp),              INTENT(IN)  :: LATITUDE
   REAL(kind=kind_noahmp),              INTENT(IN)  :: LONGITUDE
   REAL(kind=kind_noahmp),              INTENT(OUT) :: COSZ
   REAL(kind=kind_noahmp),              INTENT(OUT) :: JULIAN
   REAL(kind=kind_noahmp),    OPTIONAL, INTENT(OUT) :: HRANG_OUT
   REAL(kind=kind_noahmp),    OPTIONAL, INTENT(OUT) :: DECLIN_OUT
   REAL(kind=kind_noahmp),    OPTIONAL, INTENT(OUT) :: GMT_OUT
   INTEGER,                   OPTIONAL, INTENT(OUT) :: JULDAY_OUT
   REAL(kind=kind_noahmp)                           :: OBECL
   REAL(kind=kind_noahmp)                           :: SINOB
   REAL(kind=kind_noahmp)                           :: SXLONG
   REAL(kind=kind_noahmp)                           :: ARG
   REAL(kind=kind_noahmp)                           :: TLOCTIM
   REAL(kind=kind_noahmp)                           :: HRANG
   REAL(kind=kind_noahmp)                           :: DECLIN
   REAL(kind=kind_noahmp)                           :: GMT
   INTEGER                                          :: JULDAY
   INTEGER                                          :: IDAY
   INTEGER                                          :: IHOUR
   INTEGER                                          :: IMINUTE
   INTEGER                                          :: ISECOND

   REAL(kind=kind_noahmp), PARAMETER                :: DEGRAD = 3.14159265/180.
   REAL(kind=kind_noahmp), PARAMETER                :: DPD    = 360./365.

   call GETH_IDTS(NOWDATE(1:10), NOWDATE(1:4)//"-01-01", IDAY)
   READ(NOWDATE(12:13), *) IHOUR
   READ(NOWDATE(15:16), *) IMINUTE
   READ(NOWDATE(18:19), *) ISECOND
   GMT = REAL(IHOUR) + IMINUTE/60.0 + ISECOND/3600.0
   JULIAN = REAL(IDAY) + GMT/24.
   JULDAY = IDAY

!
! FOR SHORT WAVE RADIATION

   DECLIN=0.

!-----OBECL : OBLIQUITY = 23.5 DEGREE.

   OBECL=23.5*DEGRAD
   SINOB=SIN(OBECL)

!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:

   IF(JULIAN >= 80.0) SXLONG = DPD*(JULIAN-80.0)*DEGRAD
   IF(JULIAN <  80.0) SXLONG = DPD*(JULIAN+285.0)*DEGRAD
   ARG    = SINOB * SIN(SXLONG)
   DECLIN = ASIN(ARG)

   TLOCTIM = REAL(IHOUR) + REAL(IMINUTE)/60.0 + REAL(ISECOND)/3600.0 + LONGITUDE/15.0 ! LOCAL TIME IN HOURS
   TLOCTIM = AMOD(TLOCTIM+24.0, 24.0)
   HRANG=15.*(TLOCTIM-12.)*DEGRAD
   COSZ=SIN(LATITUDE*DEGRAD)*SIN(DECLIN)+COS(LATITUDE*DEGRAD)*COS(DECLIN)*COS(HRANG)
   
   IF (PRESENT(HRANG_OUT ))  HRANG_OUT = HRANG
   IF (PRESENT(DECLIN_OUT)) DECLIN_OUT = DECLIN
   IF (PRESENT(GMT_OUT   ))    GMT_OUT = GMT
   IF (PRESENT(JULDAY_OUT)) JULDAY_OUT = JULDAY

 END SUBROUTINE CALC_DECLIN

!---------------------------------------------------------------------

 SUBROUTINE LOCAL_TIME(NOWDATE, LONGITUDE, TLOCTIM)

   IMPLICIT NONE
!---------------------------------------------------------------------
   CHARACTER(LEN=19),      INTENT(IN)  :: NOWDATE    ! YYYY-MM-DD_HH:MM:SS
   REAL(kind=kind_noahmp), INTENT(IN)  :: LONGITUDE
   REAL(kind=kind_noahmp), INTENT(OUT) :: TLOCTIM
   INTEGER                             :: IHOUR
   INTEGER                             :: IMINUTE
   INTEGER                             :: ISECOND

   READ(NOWDATE(12:13), *) IHOUR
   READ(NOWDATE(15:16), *) IMINUTE
   READ(NOWDATE(18:19), *) ISECOND

   TLOCTIM = REAL(IHOUR) + REAL(IMINUTE)/60.0 + REAL(ISECOND)/3600.0 + LONGITUDE/15.0 ! LOCAL TIME IN HOURS
   TLOCTIM = AMOD(TLOCTIM+24.0, 24.0)

 END SUBROUTINE LOCAL_TIME


end module module_NoahMP_hrldas_driver

