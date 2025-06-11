MODULE module_sf_bep_bem
#if defined(mpas)
use mpas_atmphys_utilities, only: physics_error_fatal
#define FATAL_ERROR(M) call physics_error_fatal( M )
#else
use module_wrf_error
#define FATAL_ERROR(M) call wrf_error_fatal( M )
!USE module_model_constants
#endif
 USE module_sf_urban
 USE module_sf_bem
 USE module_bep_bem_helper, ONLY: nurbm
 use Machine, only : kind_noahmp

! SGClarke 09/11/2008
! Access urban_param.tbl values through calling urban_param_init in module_physics_init
! for CASE (BEPSCHEME) select sf_urban_physics
!
! -----------------------------------------------------------------------
!  Dimension for the array used in the BEP module
! -----------------------------------------------------------------------

      integer nurbmax         ! Maximum number of urban classes    
      parameter (nurbmax=11)

      integer ndm             ! Maximum number of street directions 
      parameter (ndm=2)

      integer nz_um           ! Maximum number of vertical levels in the urban grid
      parameter(nz_um=18)

      integer ng_u            ! Number of grid levels in the ground
      parameter (ng_u=10)
 
      integer ngr_u            ! Number of grid levels in green roof
      parameter (ngr_u=10)

      integer nwr_u            ! Number of grid levels in the walls or roofs
      parameter (nwr_u=10)

      integer nf_u             !Number of grid levels in the floors (BEM)
      parameter (nf_u=10)

      integer ngb_u            !Number of grid levels in the ground below building (BEM)
      parameter (ngb_u=10)

      real(kind=kind_noahmp) dz_u                ! Urban grid resolution
      parameter (dz_u=5.)

      integer nbui_max         !maximum number of types of buildings in an urban class
      parameter (nbui_max=15)   !must be less or equal than nz_um 


      real(kind=kind_noahmp) h_water
      parameter(h_water=0.0009722) !mm of irrigation per hour

!---------------------------------------------------------------------------------
!Parameters of the windows. The glasses of windows are considered without films  -
!Read the paper of J.Karlsson and A.Roos(2000):"modelling the angular behaviour  -
!of the total solar energy transmittance of windows".Solar Energy Vol.69,No.4,   -
!pp. 321-329, for more details.                                                  -
!---------------------------------------------------------------------------------
      integer p_num            !number of panes in the windows (1,2 or 3)
      parameter (p_num=2)
      integer q_num            !category number for the windows (q_num= 4, standard glasses)
      parameter(q_num=4)       !Possible values 1,2,...,10

! The change of ng_u, nwr_u should be done in agreement with the block data
!  in the routine "surf_temp" 
! -----------------------------------------------------------------------
!  Constant used in the BEP module
! -----------------------------------------------------------------------
           
      real(kind=kind_noahmp) vk                 ! von Karman constant
      real(kind=kind_noahmp) g_u                ! Gravity acceleration
      real(kind=kind_noahmp) pi                 !
      real(kind=kind_noahmp) r                  ! Perfect gas constant
      real(kind=kind_noahmp) cp_u               ! Specific heat at constant pressure
      real(kind=kind_noahmp) rcp_u              !
      real(kind=kind_noahmp) sigma              !
      real(kind=kind_noahmp) p0                 ! Reference pressure at the sea level
      real(kind=kind_noahmp) latent             ! Latent heat of vaporization [J/kg] (used in BEM)
      real(kind=kind_noahmp) dgmax              ! Maximum ground water holding capacity (mm)
      real(kind=kind_noahmp) drmax              ! Maximum ground roof holding capacity (mm)

      parameter(vk=0.40,g_u=9.81,pi=3.141592653,r=287.,cp_u=1004.)        
      parameter(rcp_u=r/cp_u,sigma=5.67e-08,p0=1.e+5,latent=2.45e+06,dgmax=1.,drmax=1.)

! -----------------------------------------------------------------------     




   CONTAINS
 
      subroutine BEP_BEM(FRC_URB2D,UTYPE_URB2D,itimestep,dz8w,dt,u_phy,v_phy,      &
                      th_phy,rho,p_phy,swdown,glw,                    &
                      gmt,julday,xlong,xlat,                                       &
                      declin_urb,cosz_urb2d,omg_urb2d,                             &
                      num_urban_ndm,  urban_map_zrd,  urban_map_zwd, urban_map_gd, &
                      urban_map_zd,  urban_map_zdf,   urban_map_bd, urban_map_wd,  &
                      urban_map_gbd,  urban_map_fbd,                               &
                      urban_map_zgrd,  num_urban_hi,                               &
                      trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,                     &
                      tlev_urb3d,qlev_urb3d,tw1lev_urb3d,tw2lev_urb3d,             &
                      tglev_urb3d,tflev_urb3d,sf_ac_urb3d,lf_ac_urb3d,             &        
                      cm_ac_urb3d,                                                 & 
                      sfvent_urb3d,lfvent_urb3d,                                   &
                      sfwin1_urb3d,sfwin2_urb3d,                                   &
                      sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,                   &
		      ep_pv_urb3d,t_pv_urb3d,                                      &
		      trv_urb4d,qr_urb4d,qgr_urb3d,tgr_urb3d,                      &
                      drain_urb4d,draingr_urb3d,                                   &
		      sfrv_urb3d,lfrv_urb3d,                                       &
                      dgr_urb3d,dg_urb3d,                                          &
                      lfr_urb3d,lfg_urb3d,rainbl,swddir,swddif,                    &
                      lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,                        &
                      a_u,a_v,a_t,a_e,b_u,b_v,                                     &
                      b_t,b_e,b_q,dlg,dl_u,sf,vl,                                  &
                      rl_up,rs_abs,emiss,grdflx_urb,qv_phy,                        &
                      ids,ide, jds,jde, kds,kde,                                   &
                      ims,ime, jms,jme, kms,kme,                                   &
                      its,ite, jts,jte, kts,kte)                    

      implicit none

!------------------------------------------------------------------------
!     Input
!------------------------------------------------------------------------
   INTEGER ::                       ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte,  &
                                    itimestep
 

   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   DZ8W
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   P_PHY
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   RHO
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   TH_PHY
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   T_PHY
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   U_PHY
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   V_PHY
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   U
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme )::   V
   REAL(kind=kind_noahmp), DIMENSION( ims:ime , jms:jme )        ::   GLW
   REAL(kind=kind_noahmp), DIMENSION( ims:ime , jms:jme )        ::   swdown
   REAL(kind=kind_noahmp), DIMENSION( ims:ime , jms:jme )        ::   swddir
   REAL(kind=kind_noahmp), DIMENSION( ims:ime , jms:jme )        ::   swddif
    REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme )         ::   UST
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   UTYPE_URB2D
   REAL(kind=kind_noahmp), DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   FRC_URB2D
   REAL(kind=kind_noahmp), INTENT(IN  )   ::                                   GMT 
   INTEGER, INTENT(IN  ) ::                               JULDAY
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                           XLAT, XLONG
   REAL(kind=kind_noahmp), INTENT(IN) :: DECLIN_URB
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZ_URB2D
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: OMG_URB2D
   INTEGER, INTENT(IN  ) :: urban_map_zrd
   INTEGER, INTENT(IN  ) :: urban_map_zwd
   INTEGER, INTENT(IN  ) :: urban_map_gd
   INTEGER, INTENT(IN  ) :: urban_map_zd
   INTEGER, INTENT(IN  ) :: urban_map_zdf
   INTEGER, INTENT(IN  ) :: urban_map_bd
   INTEGER, INTENT(IN  ) :: urban_map_wd
   INTEGER, INTENT(IN  ) :: urban_map_gbd
   INTEGER, INTENT(IN  ) :: urban_map_fbd
   INTEGER, INTENT(IN  ) :: num_urban_ndm
   INTEGER, INTENT(IN) :: num_urban_hi
   INTEGER , INTENT(IN)        ::     urban_map_zgrd
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zrd, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_gd , jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ), INTENT(INOUT) :: trv_urb4d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ), INTENT(INOUT) :: qr_urb4d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: qgr_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: tgr_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: drain_urb4d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: rainbl
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: draingr_urb3d
!New variables used for BEM
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, kms:kme, jms:jme ):: qv_phy
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_bd, jms:jme ), INTENT(INOUT) :: tlev_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: qlev_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_gbd, jms:jme ), INTENT(INOUT) :: tglev_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_fbd, jms:jme ), INTENT(INOUT) :: tflev_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lf_ac_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sf_ac_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: cm_ac_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ep_pv_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: t_pv_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sfvent_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lfvent_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfwin2_urb3d

!End variables
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: sfrv_urb3d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: lfrv_urb3d
   REAL(kind=kind_noahmp), OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: dgr_urb3d !GRZ
   REAL(kind=kind_noahmp), OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: dg_urb3d !GRZ
   REAL(kind=kind_noahmp), OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: lfr_urb3d !GRZ
   REAL(kind=kind_noahmp), OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: lfg_urb3d !G

   REAL(kind=kind_noahmp), DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ), INTENT(IN) :: hi_urb2d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime,jms:jme), INTENT(IN) :: lp_urb2d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime,jms:jme), INTENT(IN) :: lb_urb2d
   REAL(kind=kind_noahmp), DIMENSION( ims:ime,jms:jme), INTENT(IN) :: hgt_urb2d

   real(kind=kind_noahmp) z(ims:ime,kms:kme,jms:jme)            ! Vertical coordinates
   REAL(kind=kind_noahmp), INTENT(IN )::   DT      ! Time step

!------------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------ 
!
!    Implicit and explicit components of the source and sink terms at each levels,
!     the fluxes can be computed as follow: FX = A*X + B   example: t_fluxes = a_t * pt + b_t
      real(kind=kind_noahmp) a_u(ims:ime,kms:kme,jms:jme)         ! Implicit component for the momemtum in X-direction (center)
      real(kind=kind_noahmp) a_v(ims:ime,kms:kme,jms:jme)         ! Implicit component for the momemtum in Y-direction (center)
      real(kind=kind_noahmp) a_t(ims:ime,kms:kme,jms:jme)         ! Implicit component for the temperature
      real(kind=kind_noahmp) a_e(ims:ime,kms:kme,jms:jme)         ! Implicit component for the TKE
      real(kind=kind_noahmp) b_u(ims:ime,kms:kme,jms:jme)         ! Explicit component for the momemtum in X-direction (center)
      real(kind=kind_noahmp) b_v(ims:ime,kms:kme,jms:jme)         ! Explicit component for the momemtum in Y-direction (center)
      real(kind=kind_noahmp) b_t(ims:ime,kms:kme,jms:jme)         ! Explicit component for the temperature
      real(kind=kind_noahmp) b_e(ims:ime,kms:kme,jms:jme)         ! Explicit component for the TKE
      real(kind=kind_noahmp) b_q(ims:ime,kms:kme,jms:jme)         ! Explicit component for the Humidity
      real(kind=kind_noahmp) dlg(ims:ime,kms:kme,jms:jme)         ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real(kind=kind_noahmp) dl_u(ims:ime,kms:kme,jms:jme)        ! Length scale (lb in formula (22) ofthe BLM paper).
! urban surface and volumes        
      real(kind=kind_noahmp) sf(ims:ime,kms:kme,jms:jme)           ! surface of the urban grid cells
      real(kind=kind_noahmp) vl(ims:ime,kms:kme,jms:jme)             ! volume of the urban grid cells
! urban fluxes
      real(kind=kind_noahmp) rl_up(its:ite,jts:jte) ! upward long wave radiation
      real(kind=kind_noahmp) rs_abs(its:ite,jts:jte) ! absorbed short wave radiation
      real(kind=kind_noahmp) emiss(its:ite,jts:jte)  ! emissivity averaged for urban surfaces
      real(kind=kind_noahmp) grdflx_urb(its:ite,jts:jte)  ! ground heat flux for urban areas
!------------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------
      real(kind=kind_noahmp) hi_urb(its:ite,1:nz_um,jts:jte) ! Height histograms of buildings
      real(kind=kind_noahmp) hi_urb1D(nz_um)                 ! Height histograms of buildings
      real(kind=kind_noahmp) ss_urb(nz_um,nurbmax)           ! Probability that a building has an height equal to z
      real(kind=kind_noahmp) pb_urb(nz_um)                   ! Probability that a building has an height greater or equal to z
      real(kind=kind_noahmp) hb_u(nz_um)                     ! Bulding's heights
      integer nz_urb(nurbmax)              ! Number of layer in the urban grid
      integer nzurban(nurbmax)

!    Building parameters      
      real(kind=kind_noahmp) alag_u(nurbmax)                    ! Ground thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) alaw_u(nurbmax)                    ! Wall thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) alar_u(nurbmax)                    ! Roof thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) csg_u(nurbmax)                     ! Specific heat of the ground material [J m^3 K^-1]
      real(kind=kind_noahmp) csw_u(nurbmax)                     ! Specific heat of the wall material [J m^3 K^-1]
      real(kind=kind_noahmp) csr_u(nurbmax)                     ! Specific heat of the roof material [J m^3 K^-1]
      real(kind=kind_noahmp) twini_u(nurbmax)                   ! Initial temperature inside the building's wall [K]
      real(kind=kind_noahmp) trini_u(nurbmax)                   ! Initial temperature inside the building's roof [K]
      real(kind=kind_noahmp) tgini_u(nurbmax)                   ! Initial road temperature

!
!   Building materials
!

      real(kind=kind_noahmp) csg(ng_u)           ! Specific heat of the ground material [J m^3 K^-1]
      real(kind=kind_noahmp) csw(nwr_u)          ! Specific heat of the wall material for the current urban class [J m^3 K^-1]
      real(kind=kind_noahmp) csr(nwr_u)          ! Specific heat of the roof material for the current urban class [J m^3 K^-1]
      real(kind=kind_noahmp) csgb(ngb_u)         ! Specific heat of the ground material below the buildings at each ground levels[J m^3 K^-1]
      real(kind=kind_noahmp) csf(nf_u)           ! Specific heat of the floors materials in the buildings at each levels[J m^3 K^-1]
      real(kind=kind_noahmp) alar(nwr_u+1)       ! Roof thermal diffusivity for the current urban class [W/m K]
      real(kind=kind_noahmp) alaw(nwr_u+1)       ! Walls thermal diffusivity for the current urban class [W/m K]
      real(kind=kind_noahmp) alag(ng_u)          ! Ground thermal diffusivity for the current urban class [m^2 s^-1] 
      real(kind=kind_noahmp) alagb(ngb_u+1)      ! Ground thermal diffusivity below the building at each wall layer [W/m K]
      real(kind=kind_noahmp) alaf(nf_u+1)        ! Floor thermal diffusivity at each wall layers [W/m K]  
      real(kind=kind_noahmp) dzr(nwr_u)          ! Layer sizes in the roofs [m]
      real(kind=kind_noahmp) dzf(nf_u)           ! Layer sizes in the floors[m]
      real(kind=kind_noahmp) dzw(nwr_u)          ! Layer sizes in the walls [m]
      real(kind=kind_noahmp) dzgb(ngb_u)         ! Layer sizes in the ground below the buildings [m]

!
!New street and radiation parameters


      real(kind=kind_noahmp) bs(ndm)              ! Building width for the current urban class
      real(kind=kind_noahmp) ws(ndm)              ! Street widths of the current urban class
      real(kind=kind_noahmp) strd(ndm)            ! Street lengths for the current urban class
      real(kind=kind_noahmp) drst(ndm)            ! street directions for the current urban class
      real(kind=kind_noahmp) ss(nz_um)            ! Probability to have a building with height h
      real(kind=kind_noahmp) pb(nz_um)            ! Probability to have a building with an height equal
      real(kind=kind_noahmp) HFGR_D(nz_um)
!New roughness and buildings parameters
!
      real(kind=kind_noahmp) z0(ndm,nz_um)        ! Roughness lengths "profiles"
      real(kind=kind_noahmp) bs_urb(ndm,nurbmax)  ! Building width
      real(kind=kind_noahmp) ws_urb(ndm,nurbmax)  ! Street width

!
! for twini_u, and trini_u the initial value at the deepest level is kept constant during the simulation
!
!    Radiation paramters
      real(kind=kind_noahmp) albg_u(nurbmax)                    ! Albedo of the ground
      real(kind=kind_noahmp) albw_u(nurbmax)                    ! Albedo of the wall
      real(kind=kind_noahmp) albr_u(nurbmax)                    ! Albedo of the roof
      real(kind=kind_noahmp) albwin_u(nurbmax)                  ! Albedo of the windows
      real(kind=kind_noahmp) emwind_u(nurbmax)                  ! Emissivity of windows
      real(kind=kind_noahmp) emg_u(nurbmax)                     ! Emissivity of ground
      real(kind=kind_noahmp) emw_u(nurbmax)                     ! Emissivity of wall
      real(kind=kind_noahmp) emr_u(nurbmax)                     ! Emissivity of roof
      real(kind=kind_noahmp) gr_frac_roof_u(nurbmax)
      real(kind=kind_noahmp) pv_frac_roof_u(nurbmax)
      integer gr_flag_u
      integer gr_type_u

!   fww_u,fwg_u,fgw_u,fsw_u,fsg_u are the view factors used to compute the long wave
!   and the short wave radiation. 
      real(kind=kind_noahmp) fww_u(nz_um,nz_um,ndm,nurbmax)       !  from wall to wall
      real(kind=kind_noahmp) fwg_u(nz_um,ndm,nurbmax)             !  from wall to ground
      real(kind=kind_noahmp) fgw_u(nz_um,ndm,nurbmax)             !  from ground to wall
      real(kind=kind_noahmp) fsw_u(nz_um,ndm,nurbmax)             !  from sky to wall
      real(kind=kind_noahmp) fws_u(nz_um,ndm,nurbmax)             !  from sky to wall
      real(kind=kind_noahmp) fsg_u(ndm,nurbmax)                   !  from sky to ground

!    Roughness parameters
      real(kind=kind_noahmp) z0g_u(nurbmax)       ! The ground's roughness length
      real(kind=kind_noahmp) z0r_u(nurbmax)       ! The roof's roughness length

!    Street parameters
      integer nd_u(nurbmax)     ! Number of street direction for each urban class 
      real(kind=kind_noahmp) strd_u(ndm,nurbmax)  ! Street length (fix to greater value to the horizontal length of the cells)
      real(kind=kind_noahmp) drst_u(ndm,nurbmax)  ! Street direction
      real(kind=kind_noahmp) ws_u(ndm,nurbmax)    ! Street width
      real(kind=kind_noahmp) bs_u(ndm,nurbmax)    ! Building width
      real(kind=kind_noahmp) h_b(nz_um,nurbmax)   ! Bulding's heights
      real(kind=kind_noahmp) d_b(nz_um,nurbmax)   ! Probability that a building has an height h_b
      real(kind=kind_noahmp) ss_u(nz_um,nurbmax)! Probability that a building has an height equal to z
      real(kind=kind_noahmp) pb_u(nz_um,nurbmax)! Probability that a building has an height greater or equal to z


!    Grid parameters
      integer nz_u(nurbmax)     ! Number of layer in the urban grid
      
      real(kind=kind_noahmp) z_u(nz_um)         ! Height of the urban grid levels
!FS
      real(kind=kind_noahmp) cop_u(nurbmax)
      real(kind=kind_noahmp) bldac_frc_u(nurbmax)
      real(kind=kind_noahmp) cooled_frc_u(nurbmax)
      real(kind=kind_noahmp) pwin_u(nurbmax)
      real(kind=kind_noahmp) beta_u(nurbmax)
      integer sw_cond_u(nurbmax)
      real(kind=kind_noahmp) time_on_u(nurbmax)
      real(kind=kind_noahmp) time_off_u(nurbmax)
      real(kind=kind_noahmp) targtemp_u(nurbmax)
      real(kind=kind_noahmp) gaptemp_u(nurbmax)
      real(kind=kind_noahmp) targhum_u(nurbmax)
      real(kind=kind_noahmp) gaphum_u(nurbmax)
      real(kind=kind_noahmp) perflo_u(nurbmax)
      real(kind=kind_noahmp) hsesf_u(nurbmax)
      real(kind=kind_noahmp) hsequip(24)
      real(kind=kind_noahmp) irho(24)
! 1D array used for the input and output of the routine "urban"

      real(kind=kind_noahmp) z1D(kms:kme)               ! vertical coordinates
      real(kind=kind_noahmp) ua1D(kms:kme)                ! wind speed in the x directions
      real(kind=kind_noahmp) va1D(kms:kme)                ! wind speed in the y directions
      real(kind=kind_noahmp) pt1D(kms:kme)                ! potential temperature
      real(kind=kind_noahmp) da1D(kms:kme)                ! air density
      real(kind=kind_noahmp) pr1D(kms:kme)                ! air pressure
      real(kind=kind_noahmp) pt01D(kms:kme)               ! reference potential temperature
      real(kind=kind_noahmp) zr1D                    ! zenith angle
      real(kind=kind_noahmp) deltar1D                ! declination of the sun
      real(kind=kind_noahmp) ah1D                    ! hour angle (it should come from the radiation routine)
      real(kind=kind_noahmp) rs1D                    ! solar radiation
      real(kind=kind_noahmp) rld1D                   ! downward flux of the longwave radiation
      real(kind=kind_noahmp) swddir1D
      real(kind=kind_noahmp) swddif1D                ! short wave diffuse solar radiation _gl



      real(kind=kind_noahmp) tw1D(2*ndm,nz_um,nwr_u,nbui_max) ! temperature in each layer of the wall
      real(kind=kind_noahmp) tg1D(ndm,ng_u)                   ! temperature in each layer of the ground
      real(kind=kind_noahmp) tr1D(ndm,nz_um,nwr_u)   ! temperature in each layer of the roof
      real(kind=kind_noahmp) trv1D(ndm,nz_um,ngr_u)   ! temperature in each layer of the GREEN roof
      real(kind=kind_noahmp) qr1D(ndm,nz_um,ngr_u)   ! humidity in each layer of the GREEN roof

!
!New variable for BEM
!
      real(kind=kind_noahmp) tlev1D(nz_um,nbui_max)            ! temperature in each floor and in each different type of building
      real(kind=kind_noahmp) qlev1D(nz_um,nbui_max)            ! specific humidity in each floor and in each different type of building
      real(kind=kind_noahmp) twlev1D(2*ndm,nz_um,nbui_max)     ! temperature in each window in each floor in each different type of building
      real(kind=kind_noahmp) tglev1D(ndm,ngb_u,nbui_max)       ! temperature in each layer of the ground below of a type of building
      real(kind=kind_noahmp) tflev1D(ndm,nf_u,nz_um-1,nbui_max)! temperature in each layer of the floors in each building
      real(kind=kind_noahmp) lflev1D(nz_um,nz_um)           ! latent heat flux due to the air conditioning systems
      real(kind=kind_noahmp) sflev1D(nz_um,nz_um)           ! sensible heat flux due to the air conditioning systems
      real(kind=kind_noahmp) lfvlev1D(nz_um,nz_um)          ! latent heat flux due to ventilation
      real(kind=kind_noahmp) sfvlev1D(nz_um,nz_um)          ! sensible heat flux due to ventilation
      real(kind=kind_noahmp) sfwin1D(2*ndm,nz_um,nbui_max)     ! sensible heat flux from windows
      real(kind=kind_noahmp) consumlev1D(nz_um,nz_um)       ! consumption due to the air conditioning systems
      real(kind=kind_noahmp) eppvlev1D(nz_um)               ! electricity production of PV panels 
      real(kind=kind_noahmp) tair1D(nz_um)
      real(kind=kind_noahmp) tpvlev1D(ndm,nz_um)
      real(kind=kind_noahmp) qv1D(kms:kme)                  ! specific humidity
      real(kind=kind_noahmp) meso_urb                       ! constant to link meso and urban scales [m-2]
      real(kind=kind_noahmp) meso_urb_ac
      real(kind=kind_noahmp) roof_frac                       ! Surface fraction occupied by roof 
      real(kind=kind_noahmp) d_urb(nz_um)    
      real(kind=kind_noahmp) sf_ac
      integer ibui,nbui
      integer nlev(nz_um)
 
!
!End new variables
!

      real(kind=kind_noahmp) sfw1D(2*ndm,nz_um,nbui_max)      ! sensible heat flux from walls
      real(kind=kind_noahmp) sfg1D(ndm)              ! sensible heat flux from ground (road)
      real(kind=kind_noahmp) sfr1D(ndm,nz_um)      ! sensible heat flux from roofs
      real(kind=kind_noahmp) sfrpv1D(ndm,nz_um)

      real(kind=kind_noahmp) tpv1D(nbui_max)
      real(kind=kind_noahmp) sfr_indoor1D(nbui_max) 
      real(kind=kind_noahmp) sfrv1D(ndm,nz_um)      ! sensible heat flux from roofs
      real(kind=kind_noahmp) lfrv1D(ndm,nz_um)      ! latent heat flux from roofs
      real(kind=kind_noahmp) dg1D(ndm)              ! water depth from ground
      real(kind=kind_noahmp) dgr1D(ndm,nz_um)      ! water depth from roofs
      real(kind=kind_noahmp) lfg1D(ndm)              ! latent heat flux from ground (road)
      real(kind=kind_noahmp) lfr1D(ndm,nz_um)      ! latent heat flux from roofs
      real(kind=kind_noahmp) drain1D(ndm,nz_um)      ! sensible heat flux from roofs
      real(kind=kind_noahmp) sf1D(kms:kme)              ! surface of the urban grid cells
      real(kind=kind_noahmp) vl1D(kms:kme)                ! volume of the urban grid cells
      real(kind=kind_noahmp) a_u1D(kms:kme)               ! Implicit component of the momentum sources or sinks in the X-direction
      real(kind=kind_noahmp) a_v1D(kms:kme)               ! Implicit component of the momentum sources or sinks in the Y-direction
      real(kind=kind_noahmp) a_t1D(kms:kme)               ! Implicit component of the heat sources or sinks
      real(kind=kind_noahmp) a_e1D(kms:kme)               ! Implicit component of the TKE sources or sinks
      real(kind=kind_noahmp) b_u1D(kms:kme)               ! Explicit component of the momentum sources or sinks in the X-direction
      real(kind=kind_noahmp) b_v1D(kms:kme)               ! Explicit component of the momentum sources or sinks in the Y-direction
      real(kind=kind_noahmp) b_t1D(kms:kme)               ! Explicit component of the heat sources or sinks
      real(kind=kind_noahmp) b_ac1D(kms:kme)
      real(kind=kind_noahmp) b_e1D(kms:kme)               ! Explicit component of the TKE sources or sinks
      real(kind=kind_noahmp) b_q1D(kms:kme)               ! Explicit component of the Humidity sources or sinks
      real(kind=kind_noahmp) dlg1D(kms:kme)               ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real(kind=kind_noahmp) dl_u1D(kms:kme)              ! Length scale (lb in formula (22) ofthe BLM paper)
      real(kind=kind_noahmp) gfr1D(ndm,nz_um)
      real(kind=kind_noahmp) time_bep
! arrays used to collapse indexes
      integer ind_zwd(nbui_max,nz_um,nwr_u,ndm)
      integer ind_gd(ng_u,ndm)
      integer ind_zd(nbui_max,nz_um,ndm)
      integer ind_zdf(nz_um,ndm)
      integer ind_zrd(nz_um,nwr_u,ndm)
      integer ind_grd(nz_um,ngr_u,ndm)
!
      integer ind_bd(nbui_max,nz_um)
      integer ind_wd(nbui_max,nz_um,ndm)
      integer ind_gbd(nbui_max,ngb_u,ndm)  
      integer ind_fbd(nbui_max,nf_u,nz_um-1,ndm)

      integer ix,iy,iz,iurb,id,iz_u,iw,ig,ir,ix1,iy1,k
      integer it, nint
      integer iii
      logical first
      character(len=80) :: text
      data first/.true./
      save first,time_bep
       
      save alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,                       &
           albg_u,albw_u,albr_u,emg_u,emw_u,emr_u,                       &
           z0g_u,z0r_u, nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b,ss_u,pb_u,  &
           nz_u,z_u,albwin_u,emwind_u,cop_u,pwin_u,beta_u,sw_cond_u,     &
           bldac_frc_u,cooled_frc_u,                                     &
           time_on_u,time_off_u,targtemp_u,gaptemp_u,targhum_u,gaphum_u, &
           perflo_u,gr_frac_roof_u,                    &
           pv_frac_roof_u,hsesf_u,hsequip,irho,gr_flag_u,gr_type_u

!------------------------------------------------------------------------
!    Calculation of the momentum, heat and turbulent kinetic fluxes
!     produced by buildings
!
! References:
! Martilli, A., Clappier, A., Rotach, M.W.:2002, 'AN URBAN SURFACE EXCHANGE
! PARAMETERISATION FOR MESOSCALE MODELS', Boundary-Layer Meteorolgy 104:
! 261-304
!
! F. Salamanca and A. Martilli, 2009: 'A new Building Energy Model coupled 
! with an Urban Canopy Parameterization for urban climate simulations - part II. 
! Validation with one dimension off-line simulations'. Theor Appl Climatol
! DOI 10.1007/s00704-009-0143-8 
!------------------------------------------------------------------------
!
!prepare the arrays to collapse indexes


!
     if(urban_map_zwd.lt.nbui_max*nz_um*ndm*max(nwr_u,ng_u))then
        write(*,*)'urban_map_zwd too small, please increase to at least ', nbui_max*nz_um*ndm*max(nwr_u,ng_u)
        stop
      endif
!
!New conditions for BEM
!
      if(urban_map_bd.lt.nbui_max*nz_um)then !limit for indoor temperature and indoor humidity
        write(*,*)'urban_map_bd too small, please increase to at least ', nbui_max*nz_um
        stop
      endif

      if(urban_map_wd.lt.nbui_max*nz_um*ndm)then !limit for window temperature
        write(*,*)'urban_map_wd too small, please increase to at least ', nbui_max*nz_um*ndm
        stop
      endif

      if(urban_map_gbd.lt.nbui_max*ndm*ngb_u)then !limit for ground temperature below a building
        write(*,*)'urban_map_gbd too small, please increase to at least ', nbui_max*ndm*ngb_u
        stop
      endif

      if(urban_map_fbd.lt.(nz_um-1)*nbui_max*ndm*nf_u)then !limit for floor temperature
        write(*,*)'urban_map_fbd too small, please increase to at least ', nbui_max*ndm*nf_u*(nz_um-1)
        stop
      endif

      if (ndm.ne.2)then
         write(*,*) 'number of directions is not correct',ndm
         stop
      endif

!End of new conditions
!
!
!Initialize collapse indexes
!
      ind_zwd=0       
      ind_gd=0
      ind_zd=0
      ind_zdf=0
      ind_zrd=0
      ind_grd=0
      ind_bd=0
      ind_wd=0
      ind_gbd=0
      ind_fbd=0
!
!End initialization indexes
!

      iii=0
      do ibui=1,nbui_max
      do iz_u=1,nz_um
      do iw=1,nwr_u
      do id=1,ndm
       iii=iii+1
       ind_zwd(ibui,iz_u,iw,id)=iii
      enddo
      enddo
      enddo
      enddo

      iii=0
      do ig=1,ng_u
      do id=1,ndm
       iii=iii+1
       ind_gd(ig,id)=iii
      enddo
      enddo

      iii=0
      do ibui=1,nbui_max
      do iz_u=1,nz_um
      do id=1,ndm
       iii=iii+1
       ind_zd(ibui,iz_u,id)=iii
      enddo
      enddo
      enddo
  
      iii=0
      do iz_u=1,nz_um
      do iw=1,nwr_u
      do id=1,ndm
       iii=iii+1
       ind_zrd(iz_u,iw,id)=iii
      enddo
      enddo
      enddo

     iii=0
      do iz_u=1,nz_um
      do iw=1,ngr_u
      do id=1,ndm
       iii=iii+1
       ind_grd(iz_u,iw,id)=iii
      enddo
      enddo
      enddo
     
!
!New indexes for BEM
      
      iii=0
      do iz_u=1,nz_um
      do id=1,ndm
         iii=iii+1
         ind_zdf(iz_u,id)=iii
      enddo ! id
      enddo ! iz_u

      iii=0
      do ibui=1,nbui_max  !Type of building
      do iz_u=1,nz_um     !vertical levels
         iii=iii+1
         ind_bd(ibui,iz_u)=iii
      enddo !iz_u
      enddo !ibui



      iii=0
      do ibui=1,nbui_max !type of building
      do iz_u=1,nz_um !vertical levels
      do id=1,ndm !direction
         iii=iii+1
         ind_wd(ibui,iz_u,id)=iii
      enddo !id
      enddo !iz_u
      enddo !ibui

      iii=0
      do ibui=1,nbui_max!type of building
      do iw=1,ngb_u !layers in the wall (ground below a building)
      do id=1,ndm !direction
         iii=iii+1
         ind_gbd(ibui,iw,id)=iii  
      enddo !id
      enddo !iw 
      enddo !ibui    

      iii=0
      do ibui=1,nbui_max !type of building
      do iw=1,nf_u !layers in the wall (floor)
      do iz_u=1,nz_um-1 !vertical levels
      do id=1,ndm  !direction
         iii=iii+1
         ind_fbd(ibui,iw,iz_u,id)=iii
      enddo !id
      enddo !iz_u
      enddo !iw
      enddo !ibui


      !End of new indexes
   
      if (num_urban_hi.ge.nz_um)then
          write(*,*)'nz_um too small, please increase to at least ', num_urban_hi+1
          stop         
      endif
   
      do ix=its,ite
      do iy=jts,jte
      do iz_u=1,nz_um
          hi_urb(ix,iz_u,iy)=0.
      enddo
      enddo
      enddo

      do ix=its,ite
      do iy=jts,jte
       z(ix,kts,iy)=0.
       do iz=kts+1,kte+1
        z(ix,iz,iy)=z(ix,iz-1,iy)+dz8w(ix,iz-1,iy)
       enddo
       iii=0
       do iz_u=1,num_urban_hi
          hi_urb(ix,iz_u,iy)= hi_urb2d(ix,iz_u,iy)
          if (hi_urb(ix,iz_u,iy)/=0.) then
             iii=iii+1
          endif
       enddo !iz_u
       if (iii.gt.nbui_max) then
          write(*,*) 'nbui_max too small, please increase to at least ',iii
          stop
       endif
      enddo
      enddo


      if (first) then                           ! True only on first call

         call init_para(alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,&
                twini_u,trini_u,tgini_u,albg_u,albw_u,albr_u,albwin_u,emg_u,emw_u,&
                emr_u,emwind_u,z0g_u,z0r_u,nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b,  &
                cop_u,pwin_u,beta_u,sw_cond_u,time_on_u,time_off_u,targtemp_u,    &
                bldac_frc_u,cooled_frc_u,                                         &
                gaptemp_u,targhum_u,gaphum_u,perflo_u,                            &
                gr_frac_roof_u,pv_frac_roof_u,                   & 
                hsesf_u,hsequip,irho,gr_flag_u,gr_type_u)
 
!Initialisation of the urban parameters and calculation of the view factor
        call icBEP(nd_u,h_b,d_b,ss_u,pb_u,nz_u,z_u)
   
      first=.false.

      endif ! first

do ix=its,ite
      do iy=jts,jte
        if (FRC_URB2D(ix,iy).gt.0.) then    ! Calling BEP only for existing urban classes.
	
         iurb=UTYPE_URB2D(ix,iy)

         hi_urb1D=0.
         do iz_u=1,nz_um
            hi_urb1D(iz_u)=hi_urb(ix,iz_u,iy)
           
         enddo

         call icBEPHI_XY(iurb,hb_u,hi_urb1D,ss_urb,pb_urb,    &
                         nz_urb(iurb),z_u)

         call param(iurb,nz_u(iurb),nz_urb(iurb),nzurban(iurb),      &
                    nd_u(iurb),csg_u,csg,alag_u,alag,csr_u,csr,      &
                    alar_u,alar,csw_u,csw,alaw_u,alaw,               &
                    ws_u,ws_urb,ws,bs_u,bs_urb,bs,z0g_u,z0r_u,z0,    &
                    strd_u,strd,drst_u,drst,ss_u,ss_urb,ss,pb_u,     &
                    pb_urb,pb,dzw,dzr,dzf,csf,alaf,dzgb,csgb,alagb,  &
                    lp_urb2d(ix,iy),lb_urb2d(ix,iy),                 &
                    hgt_urb2d(ix,iy),FRC_URB2D(ix,iy))
         
!
!We compute the view factors in the icBEP_XY routine
!  

         call icBEP_XY(iurb,fww_u,fwg_u,fgw_u,fsw_u,fws_u,fsg_u,   &
                         nd_u(iurb),strd,ws,nzurban(iurb),z_u)   

         ibui=0
         nlev=0
         nbui=0
         d_urb=0.
         do iz=1,nz_um		   
         if(ss_urb(iz,iurb).gt.0) then		
           ibui=ibui+1		                
           nlev(ibui)=iz-1
           d_urb(ibui)=ss_urb(iz,iurb)
           nbui=ibui
	 endif	  
         end do  !iz

         if (nbui.gt.nbui_max) then
            write (*,*) 'nbui_max must be increased to',nbui
            stop
         endif



do iz= kts,kte
          ua1D(iz)=u_phy(ix,iz,iy)
          va1D(iz)=v_phy(ix,iz,iy)
	  pt1D(iz)=th_phy(ix,iz,iy)
	  da1D(iz)=rho(ix,iz,iy)
	  pr1D(iz)=p_phy(ix,iz,iy)
	  pt01D(iz)=300.
	  z1D(iz)=z(ix,iz,iy)
          qv1D(iz)=qv_phy(ix,iz,iy)
          a_u1D(iz)=0.
          a_v1D(iz)=0.
          a_t1D(iz)=0.
          a_e1D(iz)=0.
          b_u1D(iz)=0.
          b_v1D(iz)=0.
          b_t1D(iz)=0.
          b_ac1D(iz)=0.
          b_e1D(iz)=0.           
         enddo
	 z1D(kte+1)=z(ix,kte+1,iy)



         do id=1,ndm
         do iz_u=1,nz_um
         do iw=1,nwr_u
         do ibui=1,nbui_max
          tw1D(2*id-1,iz_u,iw,ibui)=tw1_urb4d(ix,ind_zwd(ibui,iz_u,iw,id),iy)
          tw1D(2*id,iz_u,iw,ibui)=tw2_urb4d(ix,ind_zwd(ibui,iz_u,iw,id),iy)
         enddo
         enddo
         enddo
         enddo
	
         do id=1,ndm
           do ig=1,ng_u
            tg1D(id,ig)=tgb_urb4d(ix,ind_gd(ig,id),iy)
           enddo
         
           do iz_u=1,nz_um
             do ir=1,nwr_u
               tr1D(id,iz_u,ir)=trb_urb4d(ix,ind_zrd(iz_u,ir,id),iy)
             enddo
             do ir=1,ngr_u
               if(gr_flag_u.eq.1)then
                 trv1D(id,iz_u,ir)=trv_urb4d(ix,ind_grd(iz_u,ir,id),iy)
                 qr1D(id,iz_u,ir)=qr_urb4d(ix,ind_grd(iz_u,ir,id),iy)
               else
                 trv1D(id,iz_u,ir)=0.
                 qr1D(id,iz_u,ir)=0.
               endif
             enddo
           enddo
        enddo



!Initialize variables for BEM

         tlev1D=0.  !Indoor temperature
         qlev1D=0.  !Indoor humidity

         twlev1D=0. !Window temperature
         tglev1D=0. !Ground temperature
         tflev1D=0. !Floor temperature

         sflev1D=0.    !Sensible heat flux from the a.c.
         lflev1D=0.    !latent heat flux from the a.c.
         consumlev1D=0.!consumption of the a.c.
         eppvlev1D=0.  !electricity production of PV panels
         tpvlev1D=0.
         sfvlev1D=0.   !Sensible heat flux from natural ventilation
         lfvlev1D=0.   !Latent heat flux from natural ventilation
         sfwin1D=0.    !Sensible heat flux from windows
         sfw1D=0.      !Sensible heat flux from walls         

         do iz_u=1,nz_um    !vertical levels
         do ibui=1,nbui_max !Type of building
            tlev1D(iz_u,ibui)= tlev_urb3d(ix,ind_bd(ibui,iz_u),iy)  
            qlev1D(iz_u,ibui)= qlev_urb3d(ix,ind_bd(ibui,iz_u),iy)  
         enddo !ibui
         enddo !iz_u



         do id=1,ndm  !direction
            do iz_u=1,nz_um !vertical levels
               do ibui=1,nbui_max !type of building
                  twlev1D(2*id-1,iz_u,ibui)=tw1lev_urb3d(ix,ind_wd(ibui,iz_u,id),iy)
                  twlev1D(2*id,iz_u,ibui)=tw2lev_urb3d(ix,ind_wd(ibui,iz_u,id),iy)
                  sfwin1D(2*id-1,iz_u,ibui)=sfwin1_urb3d(ix,ind_wd(ibui,iz_u,id),iy)
                  sfwin1D(2*id,iz_u,ibui)=sfwin2_urb3d(ix,ind_wd(ibui,iz_u,id),iy)
               enddo !ibui  
            enddo !iz_u
         enddo !id

         do id=1,ndm !direction
            do iw=1,ngb_u !layer in the wall
               do ibui=1,nbui_max !type of building
                  tglev1D(id,iw,ibui)=tglev_urb3d(ix,ind_gbd(ibui,iw,id),iy)
               enddo !ibui
            enddo !iw
         enddo !id
       
         do id=1,ndm !direction
            do iw=1,nf_u !layer in the walls
               do iz_u=1,nz_um-1 !verticals levels
                  do ibui=1,nbui_max !type of building
                     tflev1D(id,iw,iz_u,ibui)=tflev_urb3d(ix,ind_fbd(ibui,iw,iz_u,id),iy)
                     
                  enddo !ibui
               enddo ! iz_u
             enddo !iw
         enddo !id

!
!End initialization for BEM
!   

        do id=1,ndm
          do iz=1,nz_um
            do ibui=1,nbui_max !type of building
          !!  sfw1D(2*id-1,iz)=sfw1(ix,iy,ind_zd(iz,id))
          !!  sfw1D(2*id,iz)=sfw2(ix,iy,ind_zd(iz,id))
              sfw1D(2*id-1,iz,ibui)=sfw1_urb3d(ix,ind_zd(ibui,iz,id),iy)
              sfw1D(2*id,iz,ibui)=sfw2_urb3d(ix,ind_zd(ibui,iz,id),iy)
            enddo
          enddo
        enddo
 
        do id=1,ndm
          sfg1D(id)=sfg_urb3d(ix,id,iy)
          lfg1D(id)=lfg_urb3d(ix,id,iy)
          dg1D(id)=dg_urb3d(ix,id,iy)

        enddo

	 do id=1,ndm
	 do iz=1,nz_um
	  tpvlev1D(id,iz)=t_pv_urb3d(ix,ind_zdf(iz,id),iy)
	  sfr1D(id,iz)=sfr_urb3d(ix,ind_zdf(iz,id),iy)
          lfr1D(id,iz)=lfr_urb3d(ix,ind_zdf(iz,id),iy)          
          dgr1D(id,iz)=dgr_urb3d(ix,ind_zdf(iz,id),iy)
          if(gr_flag_u.eq.1)then
          sfrv1D(id,iz)=sfrv_urb3d(ix,ind_zdf(iz,id),iy)
          lfrv1D(id,iz)=lfrv_urb3d(ix,ind_zdf(iz,id),iy)
          drain1D(id,iz)=drain_urb4d(ix,ind_zdf(iz,id),iy)
          else
          sfrv1D(id,iz)=0.
          lfrv1D(id,iz)=0.
          drain1D(id,iz)=0.
          endif
	 enddo
	 enddo



         rs1D=swdown(ix,iy)
         rld1D=glw(ix,iy)
         swddir1D=swddir(ix,iy)         !_gl
         swddif1D=swddif(ix,iy)         !_gl
         zr1D=acos(COSZ_URB2D(ix,iy))
         deltar1D=DECLIN_URB
         ah1D=OMG_URB2D(ix,iy)
         

         call BEP1D(itimestep,ix,iy,iurb,kms,kme,kts,kte,z1D,dt,ua1D,va1D,pt1D,da1D,pr1D,pt01D,  &
                   zr1D,deltar1D,ah1D,rs1D,rld1D,alagb,             & 
                   alag,alaw,alar,alaf,csgb,csg,csw,csr,csf,        & 
                   dzr,dzf,dzw,dzgb,xlat(ix,iy),swddir1D,swddif1D, &
                   albg_u(iurb),albw_u(iurb),albr_u(iurb),          &
                   albwin_u(iurb),emg_u(iurb),emw_u(iurb),          &
                   emr_u(iurb),emwind_u(iurb),fww_u,fwg_u,          &
                   fgw_u,fsw_u,fws_u,fsg_u,z0,                      & 
                   nd_u(iurb),strd,drst,ws,bs_urb,bs,ss,pb,         & 
                   nzurban(iurb),z_u,cop_u,pwin_u,beta_u,           & 
                   sw_cond_u,time_on_u,time_off_u,targtemp_u,       &
                   gaptemp_u,targhum_u,gaphum_u,perflo_u,           &
                   gr_frac_roof_u(iurb),pv_frac_roof_u(iurb),  & 
                   hsesf_u,hsequip,irho,gr_flag_u,gr_type_u,        &
                   tw1D,tg1D,tr1D,trv1D,sfw1D,sfg1D,sfr1D,    &
                   sfrv1D,lfrv1D,    &
                   dgr1D,dg1D,lfr1D,lfg1D,                       &
                   drain1D,rainbl(ix,iy),qr1D,                   &
                   a_u1D,a_v1D,a_t1D,a_e1D,                         & 
                   b_u1D,b_v1D,b_t1D,b_ac1D,b_e1D,b_q1D,            & 
                   dlg1D,dl_u1D,sf1D,vl1D,rl_up(ix,iy),             &
                   rs_abs(ix,iy),emiss(ix,iy),grdflx_urb(ix,iy),    &
                   qv1D,tlev1D,qlev1D,sflev1D,lflev1D,consumlev1D,  &
                   eppvlev1D,tpvlev1D,sfvlev1D,lfvlev1D,twlev1D,tglev1D,tflev1D,sfwin1D,tair1D,sfr_indoor1D,sfrpv1D,gfr1D) 
           
          do ibui=1,nbui_max !type of building
	    do iz=1,nz_um   !vertical levels
               do id=1,ndm ! direction
	          sfw1_urb3d(ix,ind_zd(ibui,iz,id),iy)=sfw1D(2*id-1,iz,ibui) 
	          sfw2_urb3d(ix,ind_zd(ibui,iz,id),iy)=sfw1D(2*id,iz,ibui) 
	       enddo
	    enddo
         enddo
 
	 do id=1,ndm
	  sfg_urb3d(ix,id,iy)=sfg1D(id)
          lfg_urb3d(ix,id,iy)=lfg1D(id)
          dg_urb3d(ix,id,iy)=dg1D(id) 
	 enddo
         
	 do id=1,ndm
	 do iz=1,nz_um
          t_pv_urb3d(ix,ind_zdf(iz,id),iy)=tpvlev1D(id,iz) 
	  sfr_urb3d(ix,ind_zdf(iz,id),iy)=sfr1D(id,iz)
          dgr_urb3d(ix,ind_zdf(iz,id),iy)=dgr1D(id,iz)
          lfr_urb3d(ix,ind_zdf(iz,id),iy)=lfr1D(id,iz)
          if(gr_flag_u.eq.1)then 
          sfrv_urb3d(ix,ind_zdf(iz,id),iy)=sfrv1D(id,iz)
          lfrv_urb3d(ix,ind_zdf(iz,id),iy)=lfrv1D(id,iz)
          drain_urb4d(ix,ind_zdf(iz,id),iy)=drain1D(id,iz)
          endif
	 enddo
	 enddo
         
        do ibui=1,nbui_max
         do iz_u=1,nz_um
         do iw=1,nwr_u
         do id=1,ndm
          tw1_urb4d(ix,ind_zwd(ibui,iz_u,iw,id),iy)=tw1D(2*id-1,iz_u,iw,ibui)
          tw2_urb4d(ix,ind_zwd(ibui,iz_u,iw,id),iy)=tw1D(2*id,iz_u,iw,ibui)
         enddo
         enddo
         enddo
         enddo


          do id=1,ndm
            do ig=1,ng_u
              
               tgb_urb4d(ix,ind_gd(ig,id),iy)=tg1D(id,ig)
            enddo
            do iz_u=1,nz_um
               do ir=1,nwr_u
                  trb_urb4d(ix,ind_zrd(iz_u,ir,id),iy)=tr1D(id,iz_u,ir)
               enddo
                if(gr_flag_u.eq.1)then
               do ir=1,ngr_u
                  trv_urb4d(ix,ind_grd(iz_u,ir,id),iy)=trv1D(id,iz_u,ir)
                  qr_urb4d(ix,ind_grd(iz_u,ir,id),iy)=qr1D(id,iz_u,ir)
               enddo
                endif
            enddo
          enddo
!
         
!
!Outputs of BEM
!
        
         do ibui=1,nbui_max !type of building
         do iz_u=1,nz_um !vertical levels
            tlev_urb3d(ix,ind_bd(ibui,iz_u),iy)=tlev1D(iz_u,ibui)  
            qlev_urb3d(ix,ind_bd(ibui,iz_u),iy)=qlev1D(iz_u,ibui)  
         enddo !iz_u
         enddo !ibui
 
         do ibui=1,nbui_max !type of building
         do iz_u=1,nz_um !vertical levels
            do id=1,ndm !direction
               tw1lev_urb3d(ix,ind_wd(ibui,iz_u,id),iy)=twlev1D(2*id-1,iz_u,ibui)
               tw2lev_urb3d(ix,ind_wd(ibui,iz_u,id),iy)=twlev1D(2*id,iz_u,ibui)
               sfwin1_urb3d(ix,ind_wd(ibui,iz_u,id),iy)=sfwin1D(2*id-1,iz_u,ibui)
               sfwin2_urb3d(ix,ind_wd(ibui,iz_u,id),iy)=sfwin1D(2*id,iz_u,ibui)
            enddo !id  
         enddo !iz_u
         enddo !ibui
        
         do ibui=1,nbui_max  !type of building
            do iw=1,ngb_u !layers in the walls
               do id=1,ndm !direction
                  tglev_urb3d(ix,ind_gbd(ibui,iw,id),iy)=tglev1D(id,iw,ibui)
               enddo !id
            enddo !iw
         enddo !ibui

        do ibui=1,nbui_max !type of building 
        do iw=1,nf_u !layer in the walls
               do iz_u=1,nz_um-1 !verticals levels
                 do  id=1,ndm
                    tflev_urb3d(ix,ind_fbd(ibui,iw,iz_u,id),iy)=tflev1D(id,iw,iz_u,ibui)
                  enddo !ibui
               enddo ! iz_u
             enddo !iw
         enddo !id



         sf_ac_urb3d(ix,iy)=0.
         lf_ac_urb3d(ix,iy)=0.
         cm_ac_urb3d(ix,iy)=0.
         ep_pv_urb3d(ix,iy)=0.
         sfvent_urb3d(ix,iy)=0.
         lfvent_urb3d(ix,iy)=0.
         draingr_urb3d(ix,iy)=0.
         qgr_urb3d(ix,iy)=0.
         tgr_urb3d(ix,iy)=0.
         meso_urb=(1./4.)*FRC_URB2D(ix,iy)/((bs_urb(1,iurb)+ws_urb(1,iurb))*bs_urb(2,iurb))+ &
                  (1./4.)*FRC_URB2D(ix,iy)/((bs_urb(2,iurb)+ws_urb(2,iurb))*bs_urb(1,iurb))
          meso_urb_ac=meso_urb*bldac_frc_u(iurb)*cooled_frc_u(iurb)
          roof_frac=FRC_URB2D(ix,iy)*bs_urb(1,iurb)/(bs_urb(1,iurb)+ws_urb(1,iurb))
         ibui=0
         nlev=0
         nbui=0
         d_urb=0.
         do iz=1,nz_um		   
         if(ss_urb(iz,iurb).gt.0) then		
           ibui=ibui+1		                
           nlev(ibui)=iz-1
           d_urb(ibui)=ss_urb(iz,iurb)
           nbui=ibui
	 endif	  
         end do  !iz

       


        do ibui=1,nbui       !type of building
            ep_pv_urb3d(ix,iy)=ep_pv_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*eppvlev1D(ibui)
         do iz_u=1,nlev(ibui) !vertical levels
               sf_ac_urb3d(ix,iy)=sf_ac_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*sflev1D(iz_u,ibui)
               lf_ac_urb3d(ix,iy)=lf_ac_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*lflev1D(iz_u,ibui)
               cm_ac_urb3d(ix,iy)=cm_ac_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*consumlev1D(iz_u,ibui)
        !if(consumlev1D(iz_u,ibui).gt.0.)then
        !print*,'IX',ix,'IY',iy,'IZ_U',iz_u,'IBUI',ibui,'CONSUM',consumlev1D(iz_u,ibui),'D_URB',d_urb(ibui),'MESO_URB',meso_urb_ac

        !endif
               sfvent_urb3d(ix,iy)=sfvent_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*sfvlev1D(iz_u,ibui)
               lfvent_urb3d(ix,iy)=lfvent_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*lfvlev1D(iz_u,ibui)
         enddo !iz_u
         enddo !ibui
       


       if(gr_flag_u.eq.1)then 
       do id=1,ndm
       do iz=2,nz_um-1
        draingr_urb3d(ix,iy)=draingr_urb3d(ix,iy)+d_urb(iz-1)*roof_frac*drain1D(id,iz)*1000
       do ig=1,ngr_u
        qgr_urb3d(ix,iy)=qgr_urb3d(ix,iy)+qr1D(id,iz,ig)/ndm/(nz_um-2)/ngr_u
        tgr_urb3d(ix,iy)=tgr_urb3d(ix,iy)+trv1D(id,iz,ig)/ndm/(nz_um-2)/ngr_u
        
       enddo
       enddo
       enddo
       endif
!End outputs of bem
!
         
        sf_ac=0.
        sf(ix,kts:kte,iy)=0.
        vl(ix,kts:kte,iy)=0.
        a_u(ix,kts:kte,iy)=0.
        a_v(ix,kts:kte,iy)=0.
        a_t(ix,kts:kte,iy)=0.
        a_e(ix,kts:kte,iy)=0.
        b_u(ix,kts:kte,iy)=0.
        b_v(ix,kts:kte,iy)=0.
        b_t(ix,kts:kte,iy)=0.
        b_e(ix,kts:kte,iy)=0.
        b_q(ix,kts:kte,iy)=0.
        dlg(ix,kts:kte,iy)=0.
        dl_u(ix,kts:kte,iy)=0.

        do iz= kts,kte
          sf(ix,iz,iy)=sf1D(iz)
          vl(ix,iz,iy)=vl1D(iz)
          a_u(ix,iz,iy)=a_u1D(iz)
          a_v(ix,iz,iy)=a_v1D(iz)
          a_t(ix,iz,iy)=a_t1D(iz)
          a_e(ix,iz,iy)=a_e1D(iz)
          b_u(ix,iz,iy)=b_u1D(iz)
          b_v(ix,iz,iy)=b_v1D(iz)
          b_t(ix,iz,iy)=b_t1D(iz)
          sf_ac=sf_ac+b_ac1D(iz)*da1D(iz)*cp_u*dz8w(ix,iz,iy)*vl1D(iz)*FRC_URB2D(ix,iy)
          b_e(ix,iz,iy)=b_e1D(iz)
          b_q(ix,iz,iy)=b_q1D(iz)
          dlg(ix,iz,iy)=dlg1D(iz)
          dl_u(ix,iz,iy)=dl_u1D(iz)
        enddo
        sf(ix,kte+1,iy)=sf1D(kte+1)

         endif ! FRC_URB2D


      enddo  ! iy
      enddo  ! ix


        time_bep=time_bep+dt

!      print*, 'ss_urb', ss_urb
!      print*, 'pb_urb', pb_urb
!      print*, 'nz_urb', nz_urb
!      print*, 'd_urb',  d_urb
         
  
      return
      end subroutine BEP_BEM


! ===6=8===============================================================72

      subroutine BEP1D(itimestep,ix,iy,iurb,kms,kme,kts,kte,z,dt,ua,va,pt,da,pr,pt0,   &  
                      zr,deltar,ah,rs,rld,alagb,                       & 
                      alag,alaw,alar,alaf,csgb,csg,csw,csr,csf,        & 
                      dzr,dzf,dzw,dzgb,xlat,swddir,swddif,             &
                      albg,albw,albr,albwin,emg,emw,emr,               & 
                      emwind,fww,fwg,fgw,fsw,fws,fsg,z0,               & 
                      ndu,strd,drst,ws,bs_u,bs,ss,pb,                  & 
                      nzu,z_u,cop_u,pwin_u,beta_u,sw_cond_u,           & 
                      time_on_u,time_off_u,targtemp_u,                 &
                      gaptemp_u,targhum_u,gaphum_u,perflo_u,           &
                      gr_frac_roof,pv_frac_roof,        & 
                      hsesf_u,hsequip,irho,gr_flag,gr_type,                    &
                      tw,tg,tr,trv,sfw,sfg,sfr,            &
                      sfrv,lfrv,dgr,dg,lfr,lfg,drain,rainbl,qr,      & 
                      a_u,a_v,a_t,a_e,                                 &
                      b_u,b_v,b_t,b_ac,b_e,b_q,                        & 
                      dlg,dl_u,sf,vl,rl_up,rs_abs,emiss,grdflx_urb,    &
                      qv,tlev,qlev,sflev,lflev,consumlev,              &
                      eppvlev,tpvlev,sfvlev,lfvlev,twlev,tglev,tflev,sfwin,tmp_u,sfr_indoor,sfrpv,gfr)    
        ! print*,'SFR_AFT',sfr(id,iz)



      implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------

! Data relative to the "mesoscale grid"

!!    integer nz                 ! Number of vertical levels
      integer kms,kme,kts,kte,ix,iy,itimestep
      real(kind=kind_noahmp) z(kms:kme)               ! Altitude above the ground of the cell interfaces.
      real(kind=kind_noahmp) ua(kms:kme)                ! Wind speed in the x direction
      real(kind=kind_noahmp) va(kms:kme)                ! Wind speed in the y direction
      real(kind=kind_noahmp) pt(kms:kme)                ! Potential temperature
      real(kind=kind_noahmp) da(kms:kme)                ! Air density
      real(kind=kind_noahmp) pr(kms:kme)                ! Air pressure
      real(kind=kind_noahmp) pt0(kms:kme)               ! Reference potential temperature (could be equal to "pt")
      real(kind=kind_noahmp) qv(kms:kme)              ! Specific humidity
      real(kind=kind_noahmp) dt                    ! Time step
      real(kind=kind_noahmp) zr                    ! Zenith angle
      real(kind=kind_noahmp) deltar                ! Declination of the sun
      real(kind=kind_noahmp) ah                    ! Hour angle
      real(kind=kind_noahmp) rs                    ! Solar radiation
      real(kind=kind_noahmp) rld                   ! Downward flux of the longwave radiation
      real(kind=kind_noahmp) xlat                  ! Latitude
      real(kind=kind_noahmp) swddir                ! short wave direct solar radiation   !_gl
      real(kind=kind_noahmp) swddif                ! short wave diffuse solar radiation  !_gl

! Data relative to the "urban grid"

      integer iurb               ! Current urban class

!    Radiation parameters
      real(kind=kind_noahmp) albg                  ! Albedo of the ground
      real(kind=kind_noahmp) albw                  ! Albedo of the wall
      real(kind=kind_noahmp) albr                  ! Albedo of the roof
      real(kind=kind_noahmp) albwin                ! Albedo of the windows
      real(kind=kind_noahmp) emwind                ! Emissivity of windows
      real(kind=kind_noahmp) emg                   ! Emissivity of ground
      real(kind=kind_noahmp) emw                   ! Emissivity of wall
      real(kind=kind_noahmp) emr                   ! Emissivity of roof


!    fww,fwg,fgw,fsw,fsg are the view factors used to compute the long and 
!    short wave radation. 
!    The calculation of these factor is explained in the Appendix A of the BLM paper
      real(kind=kind_noahmp) fww(nz_um,nz_um,ndm,nurbm)  !  from wall to wall
      real(kind=kind_noahmp) fwg(nz_um,ndm,nurbm)        !  from wall to ground
      real(kind=kind_noahmp) fgw(nz_um,ndm,nurbm)        !  from ground to wall
      real(kind=kind_noahmp) fsw(nz_um,ndm,nurbm)        !  from sky to wall
      real(kind=kind_noahmp) fws(nz_um,ndm,nurbm)        !  from wall to sky
      real(kind=kind_noahmp) fsg(ndm,nurbm)              !  from sky to ground
      
!    Street parameters
      integer ndu                  ! Number of street direction for each urban class 
      real(kind=kind_noahmp) bs_u(ndm,nurbm)         ! Building width
        
!    Grid parameters
      integer nzu           ! Number of layer in the urban grid
      real(kind=kind_noahmp) z_u(nz_um)       ! Height of the urban grid levels
!FS
      real(kind=kind_noahmp) cop_u(nurbm)
      real(kind=kind_noahmp) pwin_u(nurbm)
      real(kind=kind_noahmp) beta_u(nurbm)
      integer sw_cond_u(nurbm)
      real(kind=kind_noahmp) time_on_u(nurbm)
      real(kind=kind_noahmp) time_off_u(nurbm)
      real(kind=kind_noahmp) targtemp_u(nurbm)
      real(kind=kind_noahmp) gaptemp_u(nurbm)
      real(kind=kind_noahmp) targhum_u(nurbm)
      real(kind=kind_noahmp) gaphum_u(nurbm)
      real(kind=kind_noahmp) perflo_u(nurbm)
      real(kind=kind_noahmp) hsesf_u(nurbm)
      real(kind=kind_noahmp) hsequip(24)
      real(kind=kind_noahmp) irho(24)
      real(kind=kind_noahmp) gr_frac_roof
      real(kind=kind_noahmp) pv_frac_roof
      integer gr_flag
      integer gr_type
      real(kind=kind_noahmp) tpv(nbui_max)
     real(kind=kind_noahmp)  sfpv(nbui_max)
     real(kind=kind_noahmp) sfr_indoor(nbui_max)
! ----------------------------------------------------------------------
! INPUT-OUTPUT
! ----------------------------------------------------------------------

! Data relative to the "urban grid" which should be stored from the current time step to the next one

      real(kind=kind_noahmp) tw(2*ndm,nz_um,nwr_u,nbui_max)  ! Temperature in each layer of the wall [K]
      real(kind=kind_noahmp) tr(ndm,nz_um,nwr_u)  ! Temperature in each layer of the roof [K]
      real(kind=kind_noahmp) tg(ndm,ng_u)          ! Temperature in each layer of the ground [K]
      real(kind=kind_noahmp) trv(ndm,nz_um,ngr_u)  ! Temperature in each layer of the green roof [K]
      real(kind=kind_noahmp) sfw(2*ndm,nz_um,nbui_max)      ! Sensible heat flux from walls
      real(kind=kind_noahmp) sfg(ndm)              ! Sensible heat flux from ground (road)
      real(kind=kind_noahmp) sfr(ndm,nz_um)      ! Sensible heat flux from roofs
      real(kind=kind_noahmp) sfrv(ndm,nz_um)      ! Sensible heat flux from green roofs
      real(kind=kind_noahmp) lfrv(ndm,nz_um)      ! Latent heat flux from green roofs
      real(kind=kind_noahmp) dg(ndm)              ! water depth ground (road)
      real(kind=kind_noahmp) dgr(ndm,nz_um)      ! water depth roofs
      real(kind=kind_noahmp) lfr(ndm,nz_um)      ! Latent heat flux from roofs
      real(kind=kind_noahmp) lfg(ndm)              ! Latent heat flux from ground (road)
      real(kind=kind_noahmp) drain(ndm,nz_um)        ! Green roof drainage
      real(kind=kind_noahmp) rainbl              ! Rainfall 
      real(kind=kind_noahmp) gfg(ndm)             ! Heat flux transferred from the surface of the ground (road) towards the interior
      real(kind=kind_noahmp) gfr(ndm,nz_um)     ! Heat flux transferred from the surface of the roof towards the interior
      real(kind=kind_noahmp) gfw(2*ndm,nz_um,nbui_max)     ! Heat flux transfered from the surface of the walls towards the interior
      real(kind=kind_noahmp) qr(ndm,nz_um,ngr_u)  ! Green Roof soil moisture

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
                         

! Data relative to the "mesoscale grid"

      real(kind=kind_noahmp) sf(kms:kme)             ! Surface of the "mesoscale grid" cells taking into account the buildings
      real(kind=kind_noahmp) vl(kms:kme)               ! Volume of the "mesoscale grid" cells taking into account the buildings
     
!    Implicit and explicit components of the source and sink terms at each levels,
!     the fluxes can be computed as follow: FX = A*X + B   example: Heat fluxes = a_t * pt + b_t
      real(kind=kind_noahmp) a_u(kms:kme)              ! Implicit component of the momentum sources or sinks in the X-direction
      real(kind=kind_noahmp) a_v(kms:kme)              ! Implicit component of the momentum sources or sinks in the Y-direction
      real(kind=kind_noahmp) a_t(kms:kme)              ! Implicit component of the heat sources or sinks
      real(kind=kind_noahmp) a_e(kms:kme)              ! Implicit component of the TKE sources or sinks
      real(kind=kind_noahmp) b_u(kms:kme)              ! Explicit component of the momentum sources or sinks in the X-direction
      real(kind=kind_noahmp) b_v(kms:kme)              ! Explicit component of the momentum sources or sinks in the Y-direction
      real(kind=kind_noahmp) b_t(kms:kme)              ! Explicit component of the heat sources or sinks
      real(kind=kind_noahmp) b_ac(kms:kme)
      real(kind=kind_noahmp) b_e(kms:kme)              ! Explicit component of the TKE sources or sinks
      real(kind=kind_noahmp) b_q(kms:kme)              ! Explicit component of the humidity sources or sinks
      real(kind=kind_noahmp) dlg(kms:kme)              ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real(kind=kind_noahmp) dl_u(kms:kme)             ! Length scale (lb in formula (22) ofthe BLM paper).
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------

      real(kind=kind_noahmp) dz(kms:kme)               ! vertical space steps of the "mesoscale grid"
! Data interpolated from the "mesoscale grid" to the "urban grid"

      real(kind=kind_noahmp) ua_u(nz_um)          ! Wind speed in the x direction
      real(kind=kind_noahmp) va_u(nz_um)          ! Wind speed in the y direction
      real(kind=kind_noahmp) pt_u(nz_um)          ! Potential temperature
      real(kind=kind_noahmp) da_u(nz_um)          ! Air density
      real(kind=kind_noahmp) pt0_u(nz_um)         ! Reference potential temperature
      real(kind=kind_noahmp) pr_u(nz_um)          ! Air pressure
      real(kind=kind_noahmp) qv_u(nz_um)          !Specific humidity

! Data defining the building and street charateristics

      real(kind=kind_noahmp) alag(ng_u)           ! Ground thermal diffusivity for the current urban class [m^2 s^-1] 
      
      real(kind=kind_noahmp) csg(ng_u)            ! Specific heat of the ground material of the current urban class [J m^3 K^-1]
      real(kind=kind_noahmp) csr(nwr_u)            ! Specific heat of the roof material for the current urban class [J m^3 K^-1]
      real(kind=kind_noahmp) csw(nwr_u)            ! Specific heat of the wall material for the current urban class [J m^3 K^-1]

      real(kind=kind_noahmp) z0(ndm,nz_um)      ! Roughness lengths "profiles"
      real(kind=kind_noahmp) ws(ndm)              ! Street widths of the current urban class
      real(kind=kind_noahmp) bs(ndm)              ! Building widths of the current urban class
      real(kind=kind_noahmp) strd(ndm)            ! Street lengths for the current urban class
      real(kind=kind_noahmp) drst(ndm)            ! Street directions for the current urban class
      real(kind=kind_noahmp) ss(nz_um)          ! Probability to have a building with height h
      real(kind=kind_noahmp) pb(nz_um)          ! Probability to have a building with an height equal
      real(kind=kind_noahmp) cdrag(nz_um)
      real(kind=kind_noahmp) alp

! Solar radiation at each level of the "urban grid"

     real(kind=kind_noahmp) rsg(ndm)             ! Short wave radiation from the ground
      real(kind=kind_noahmp) rsw(2*ndm,nz_um)     ! Short wave radiation from the walls
      real(kind=kind_noahmp) rsd(2*ndm,nz_um)     ! Direct Short wave radiation received by the walls
      real(kind=kind_noahmp) rlg(ndm)             ! Long wave radiation from the ground
      real(kind=kind_noahmp) rlw(2*ndm,nz_um)     ! Long wave radiation from the walls

! Potential temperature of the surfaces at each level of the "urban grid"

      real(kind=kind_noahmp) ptg(ndm)             ! Ground potential temperatures 
      real(kind=kind_noahmp) ptr(ndm,nz_um)     ! Roof potential temperatures 
      real(kind=kind_noahmp) ptrv(ndm,nz_um)     ! Roof potential temperatures 
      real(kind=kind_noahmp) ptw(2*ndm,nz_um,nbui_max)     ! Walls potential temperatures 

      real(kind=kind_noahmp) tg_av(ndm) 
! Explicit and implicit component of the momentum, temperature and TKE sources or sinks on
! vertical surfaces (walls) ans horizontal surfaces (roofs and street)
! The fluxes can be computed as follow: Fluxes of X = A*X + B
! Example: Momentum fluxes on vertical surfaces = uva_u * ua_u + uvb_u

      real(kind=kind_noahmp) uhb_u(ndm,nz_um)   ! U (wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) uva_u(2*ndm,nz_um)   ! U (wind component)   Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) uvb_u(2*ndm,nz_um)   ! U (wind component)   Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) vhb_u(ndm,nz_um)   ! V (wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) vva_u(2*ndm,nz_um)   ! V (wind component)   Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) vvb_u(2*ndm,nz_um)   ! V (wind component)   Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) thb_u(ndm,nz_um)   ! Temperature        Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) tva_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) tvb_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, B (explicit) term


 real(kind=kind_noahmp) tvb_ac(2*ndm,nz_um)
      real(kind=kind_noahmp) ehb_u(ndm,nz_um)   ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) evb_u(2*ndm,nz_um)   ! Energy (TKE)         Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) qhb_u(ndm,nz_um)     ! Humidity      Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) qvb_u(2*ndm,nz_um)   ! Humidity      Vertical surfaces, B (explicit) term      
!
      real(kind=kind_noahmp) rs_abs ! solar radiation absorbed by urban surfaces 
      real(kind=kind_noahmp) rl_up ! longwave radiation emitted by urban surface to the atmosphere 
      real(kind=kind_noahmp) emiss ! mean emissivity of the urban surface
      real(kind=kind_noahmp) grdflx_urb ! ground heat flux
      real(kind=kind_noahmp) dt_int ! internal time step
      integer nt_int ! number of internal time step
      integer iz,id, it_int,it
      integer iw

!---------------------------------------
!New variables uses in BEM
!----------------------------------------
   
      real(kind=kind_noahmp) tmp_u(nz_um)     !Air Temperature [K]

      real(kind=kind_noahmp) dzw(nwr_u)       !Layer sizes in the walls
      real(kind=kind_noahmp) dzr(nwr_u)       !Layer sizes in the roofs
      real(kind=kind_noahmp) dzf(nf_u)        !Layer sizes in the floors
      real(kind=kind_noahmp) dzgb(ngb_u)      !Layer sizes in the ground below the buildings

      real(kind=kind_noahmp) csgb(ngb_u)      !Specific heat of the ground material below the buildings 

      real(kind=kind_noahmp) csf(nf_u)        !Specific heat of the floors materials in the buildings 
                            !of the current urban class at each levels[J m^3 K^-1]
      real(kind=kind_noahmp) alar(nwr_u+1)    ! Roof thermal diffusivity for the current urban class [W/m K]
      real(kind=kind_noahmp) alaw(nwr_u+1)    ! Walls thermal diffusivity for the current urban class [W/m K] 
      real(kind=kind_noahmp) alaf(nf_u+1)     ! Floor thermal diffusivity at each wall layers [W/m K]     
      real(kind=kind_noahmp) alagb(ngb_u+1)   ! Ground thermal diffusivity below the building at each wall layer [W/m K] 

      real(kind=kind_noahmp) sfrb(ndm,nbui_max)        ! Sensible heat flux from roofs [W/m2]
      real(kind=kind_noahmp) sfrbpv(ndm,nbui_max)      ! Sensible heat flux from PV panels [W/m2]
      real(kind=kind_noahmp) sfrpv(ndm,nz_um)          ! Sensible heat flux from PV panels [W/m2]
      real(kind=kind_noahmp) sfrvb(ndm,nbui_max)        ! Sensible heat flux from roofs [W/m2]
      real(kind=kind_noahmp) lfrvb(ndm,nbui_max)        ! Sensible heat flux from roofs [W/m2]
      real(kind=kind_noahmp) lfrb(ndm,nbui_max)        ! Sensible heat flux from roofs [W/m2]
  
      real(kind=kind_noahmp) gfrb(ndm,nbui_max)        ! Heat flux flowing inside the roofs [W/m2]
      real(kind=kind_noahmp) sfwb1D(2*ndm,nz_um)    !Sensible heat flux from the walls [W/m2] 
      real(kind=kind_noahmp) sfwin(2*ndm,nz_um,nbui_max)!Sensible heat flux from windows [W/m2]
      real(kind=kind_noahmp) sfwinb1D(2*ndm,nz_um)  !Sensible heat flux from windows [W/m2]
      real(kind=kind_noahmp) gfwb1D(2*ndm,nz_um)    !Heat flux flowing inside the walls [W/m2]

      real(kind=kind_noahmp) qlev(nz_um,nbui_max)      !specific humidity [kg/kg]
      real(kind=kind_noahmp) qlevb1D(nz_um)         !specific humidity [kg/kg] 
      real(kind=kind_noahmp) tlev(nz_um,nbui_max)      !Indoor temperature [K]
      real(kind=kind_noahmp) tlevb1D(nz_um)         !Indoor temperature [K]
      real(kind=kind_noahmp) twb1D(2*ndm,nwr_u,nz_um)     !Wall temperature in BEM [K]      
      real(kind=kind_noahmp) twlev(2*ndm,nz_um,nbui_max)     !Window temperature in BEM [K]
      real(kind=kind_noahmp) twlevb1D(2*ndm,nz_um)        !Window temperature in BEM [K]
      real(kind=kind_noahmp) tglev(ndm,ngb_u,nbui_max)        !Ground temperature below a building in BEM [K]
      real(kind=kind_noahmp) tglevb1D(ngb_u)               !Ground temperature below a building in BEM [K]
      real(kind=kind_noahmp) tflev(ndm,nf_u,nz_um-1,nbui_max)!Floor temperature in BEM[K]
      real(kind=kind_noahmp) tflevb1D(nf_u,nz_um-1)       !Floor temperature in BEM[K]
      real(kind=kind_noahmp) trb(ndm,nwr_u,nbui_max)         !Roof temperature in BEM [K]
      real(kind=kind_noahmp) trvb(ndm,ngr_u,nbui_max)         !Roof temperature in BEM [K]
      real(kind=kind_noahmp) trb1D(nwr_u) 

      real(kind=kind_noahmp) sflev(nz_um,nz_um)     ! sensible heat flux due to the air conditioning systems [W]
      real(kind=kind_noahmp) lflev(nz_um,nz_um)     ! latent heat flux due to the air conditioning systems  [W]
      real(kind=kind_noahmp) consumlev(nz_um,nz_um) ! consumption due to the air conditioning systems [W]
      real(kind=kind_noahmp) sflev1D(nz_um)         ! sensible heat flux due to the air conditioning systems [W]
      real(kind=kind_noahmp) lflev1D(nz_um)         ! latent heat flux due to the air conditioning systems  [W]
      real(kind=kind_noahmp) consumlev1D(nz_um)     ! consumption due to the air conditioning systems [W]
      real(kind=kind_noahmp) eppvlev(nz_um)         ! Electricity production of PV panels [W]
	   real(kind=kind_noahmp) tpvlev(ndm,nz_um)
      real(kind=kind_noahmp) tpvlevb(ndm,nbui_max)        ! Sensible heat flux from roofs [W/m2]
      real(kind=kind_noahmp) sfvlev(nz_um,nz_um)    ! sensible heat flux due to ventilation [W]
      real(kind=kind_noahmp) lfvlev(nz_um,nz_um)    ! latent heat flux due to ventilation [W]
      real(kind=kind_noahmp) sfvlev1D(nz_um)        ! sensible heat flux due to ventilation [W]
      real(kind=kind_noahmp) lfvlev1D(nz_um)        ! Latent heat flux due to ventilation [W]

      real(kind=kind_noahmp) ptwin(2*ndm,nz_um,nbui_max)  ! window potential temperature
      real(kind=kind_noahmp) tw_av(2*ndm,nz_um)        ! Averaged temperature of the wall surfaces
      real(kind=kind_noahmp) twlev_av(2*ndm,nz_um)     ! Averaged temperature of the windows
      real(kind=kind_noahmp) sfw_av(2*ndm,nz_um)       ! Averaged sensible heat from walls
      real(kind=kind_noahmp) sfwind_av(2*ndm,nz_um)    ! Averaged sensible heat from windows
      integer flag_pvp
      integer nbui                !Total number of different type of buildings in an urban class
      integer nlev(nz_um)         !Number of levels in each different type of buildings in an urban class
      integer ibui,ily  
      real(kind=kind_noahmp) :: nhourday   ! Number of hours from midnight, local time
      real(kind=kind_noahmp) :: st4,gamma,fp,lmr,smr,prova
      real(kind=kind_noahmp) hfgr(ndm,nz_um)!heat flux green roof
      real(kind=kind_noahmp) hfgrb(ndm,nbui_max)
      real(kind=kind_noahmp) irri_per_ts
      real(kind=kind_noahmp) irri_now 
      real(kind=kind_noahmp) tr_av(ndm,nz_um)
      real(kind=kind_noahmp) tr_avb(ndm,nbui_max)
      real(kind=kind_noahmp) sfr_avb(ndm,nbui_max)
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
    
! Fix some usefull parameters for the computation of the sources or sinks
!
!initialize the variables inside the param routine

        nhourday=ah/PI*180./15.+12.
        if (nhourday >= 24) nhourday = nhourday - 24
        if (nhourday < 0)  nhourday = nhourday + 24


      if(sum(irho).gt.0)then
        irri_per_ts=h_water/sum(irho)
       else
        irri_per_ts=0.
       endif
       
     if(irho(int(nhourday)+1).ne.0)then
       irri_now=irri_per_ts
     else
       irri_now=0.
     endif
      
      do iz=kts,kte
         dz(iz)=z(iz+1)-z(iz)
      end do
! Interpolation on the "urban grid"
      call interpol(kms,kme,kts,kte,nzu,z,z_u,ua,ua_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,va,va_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pt,pt_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pt0,pt0_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pr,pr_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,da,da_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,qv,qv_u)
! Compute the modification of the radiation due to the buildings
      

      call averaging_temp(tw,twlev,ss,pb,tw_av,twlev_av, &
                           sfw_av,sfwind_av,sfw,sfwin)
                           
     do id=1,ndu
       tg_av(id)=tg(id,ng_u)
     do iz=1,nz_um

       tr_av(id,iz)=((1-gr_frac_roof)*tr(id,iz,nwr_u)**4.+   &
       gr_frac_roof*trv(id,iz,ngr_u)**4.)**(1./4.)

     enddo
     enddo
     
     


   
     call modif_rad(iurb,ndu,nzu,z_u,ws,           &
                    drst,strd,ss,pb,                &
                    tw_av,tg_av,twlev_av,albg,albw,    &
                    emw,emg,pwin_u(iurb),albwin,    &
                    emwind,fww,fwg,fgw,fsw,fsg,     &
                    zr,deltar,ah,xlat,swddir,swddif,      &  !_gl  
                    rs,rld,rsw,rsd,rsg,rlw,rlg)  


 

! calculation of the urban albedo and the upward long wave radiation


       call upward_rad(ndu,nzu,ws,bs,sigma,pb,ss,                 &
                       tg_av,emg,albg,rlg,rsg,sfg,lfg,                   & 
                       tw_av,emw,albw,rlw,rsw,sfw_av,             & 
                       tr_av,emr,albr,emwind,                        &
                       albwin,twlev_av,pwin_u(iurb),sfwind_av,rld,rs,sfr,sfrv,lfr,lfrv, & 
                       rs_abs,rl_up,emiss,grdflx_urb,gr_frac_roof,tpvlev,pv_frac_roof)          
   
    do id=1,ndu
    if(dg(id).le.dgmax) then
      dg(id)=dg(id)+(rainbl+(lfg(id)*dt)/latent)
     endif
    if (dg(id).lt.0) then
      dg(id)=0
    endif
    if (dg(id).gt.dgmax) then
      dg(id)=dgmax
    endif
   do iz=2,nz_um
    if(dgr(id,iz).le.drmax) then
     dgr(id,iz)=dgr(id,iz)+(rainbl+(lfr(id,iz)*dt)/latent)
    endif 
    if (dgr(id,iz).lt.0) then
     dgr(id,iz)=0
    endif
    if (dgr(id,iz).gt.drmax) then
     dgr(id,iz)=drmax
    endif
   enddo
  enddo !id 
 
  


     call surf_temp(ndu,pr_u,dt,                   & 
                    rld,rsg,rlg,                    &
                    tg,alag,csg,emg,albg,ptg,sfg,lfg,gfg)
     if(gr_flag.eq.1)then
     if(gr_frac_roof.gt.0.)then
     hfgr=0.
     call roof_temp_veg(ndu,pr_u,dt,                   &
                    rld,rs,                    &
                    trv,ptrv,sfrv,lfrv,gfr,qr,rainbl,drain,hfgr,tr,alar(5),dzr(5),csr(5),nzu,irri_now,gr_type,pv_frac_roof,tpvlev)
    
     endif
     endif


       
       do iz=1,nzu !Compute the outdoor temperature 
	 tmp_u(iz)=pt_u(iz)*(pr_u(iz)/p0)**(rcp_u) 
       end do

       ibui=0
       nlev=0
       nbui=0
       hfgrb=0. 
       sfrb=0.     !Sensible heat flux from roof
       sfrbpv=0.   !Sensible heat flux from PV panels
       sfrpv=0.    !Sensible heat flux from PV panels
       lfrvb=0.
       lfrb=0.
       sfrvb=0.
       gfrb=0.     !Heat flux flowing inside the roof
       sfwb1D=0.   !Sensible heat flux from walls
       sfwinb1D=0. !Sensible heat flux from windows
       gfwb1D=0.   !Heat flux flowing inside the walls[W/m2]


       twb1D=0.    !Wall temperature
       twlevb1D=0. !Window temperature
       tglevb1D=0. !Ground temperature below a building
       tflevb1D=0. !Floor temperature
       trvb=0.     
       trb=0.      !Roof temperature
       trb1D=0.    !Roof temperature
       tr_avb=0.
       qlevb1D=0. !Indoor humidity
       tlevb1D=0. !indoor temperature

       sflev1D=0.    !Sensible heat flux from the a.c.
       lflev1D=0.    !Latent heat flux from the a.c.
       consumlev1D=0.!Consumption from the a.c.
       tpvlevb=0.
       eppvlev=0.
       sfvlev1D=0.   !Sensible heat flux from the natural ventilation
       lfvlev1D=0.   !Latent heat flux from natural ventilation
       ptw=0.        !Wall potential temperature
       ptwin=0.      !Window potential temperature
       ptr=0.        !Roof potential temperature

       do iz=1,nz_um		   
         if(ss(iz).gt.0) then		
           ibui=ibui+1		                
           nlev(ibui)=iz-1
           nbui=ibui
           do id=1,ndm
              tr_avb(id,ibui)=tr_av(id,iz)
	      tpvlevb(id,ibui)=tpvlev(id,iz)
              hfgrb(id,ibui)=hfgr(id,iz)
              sfrb(id,ibui)=sfr(id,iz)
               sfrvb(id,ibui)=sfrv(id,iz)
              lfrvb(id,ibui)=lfrv(id,iz)
              lfrb(id,ibui)=lfr(id,iz)
              sfr_avb(id,ibui)=(1-gr_frac_roof)*sfr(id,iz)+gr_frac_roof*(sfrv(id,iz))
              do ily=1,nwr_u
                 trb(id,ily,ibui)=tr(id,iz,ily)
              enddo
              do ily=1,ngr_u
                 trvb(id,ily,ibui)=trv(id,iz,ily)
              enddo

           enddo
	 endif	  
       end do  !iz
     
!--------------------------------------------------------------------------------
!Loop over BEM  -----------------------------------------------------------------
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

       do ibui=1,nbui
          do iz=1,nz_um
             qlevb1D(iz)=qlev(iz,ibui)
             tlevb1D(iz)=tlev(iz,ibui) 
          enddo
          
          do id=1,ndm

             do ily=1,nwr_u
                trb1D(ily)=trb(id,ily,ibui)
             enddo
             do ily=1,ngb_u
                tglevb1D(ily)=tglev(id,ily,ibui) 
             enddo

             do ily=1,nf_u
                do iz=1,nz_um-1
                  tflevb1D(ily,iz)=tflev(id,ily,iz,ibui)
                enddo
             enddo

             do iz=1,nz_um
                sfwinb1D(2*id-1,iz)=sfwin(2*id-1,iz,ibui)
                sfwinb1D(2*id,iz)=sfwin(2*id,iz,ibui)
             enddo

             do iz=1,nz_um
                do ily=1,nwr_u
                   twb1D(2*id-1,ily,iz)=tw(2*id-1,iz,ily,ibui)
                   twb1D(2*id,ily,iz)=tw(2*id,iz,ily,ibui)
                enddo
                sfwb1D(2*id-1,iz)=sfw(2*id-1,iz,ibui)
                sfwb1D(2*id,iz)=sfw(2*id,iz,ibui)
                twlevb1D(2*id-1,iz)=twlev(2*id-1,iz,ibui)
                twlevb1D(2*id,iz)=twlev(2*id,iz,ibui)
             enddo
          enddo

    !print*,'HFGR_BEFORE_CALLING_BEM',hfgr(nlev(ibui))

          call BEM(nz_um,nlev(ibui),nhourday,dt,bs_u(1,iurb),                &
                   bs_u(2,iurb),dz_u,nwr_u,nf_u,nwr_u,ngb_u,sfwb1D,gfwb1D,   &
                   sfwinb1D,sfr_avb(1,ibui),lfrb(1,ibui),gfrb(1,ibui),       &
                   sfrbpv(1,ibui),                                           &
                   latent,sigma,albw,albwin,albr,                            &
                   emr,emw,emwind,rsw,rlw,r,cp_u,                            &
                   da_u,tmp_u,qv_u,pr_u,rs,swddif,rld,dzw,csw,alaw,pwin_u(iurb),    &
                   cop_u(iurb),beta_u(iurb),sw_cond_u(iurb),time_on_u(iurb), &
                   time_off_u(iurb),targtemp_u(iurb),gaptemp_u(iurb),        &
                   targhum_u(iurb),gaphum_u(iurb),perflo_u(iurb),            &
                   gr_frac_roof,pv_frac_roof,gr_flag,                        & 
                   ua_u,va_u,                                                &
                   hsesf_u(iurb),hsequip,                                    &
                   dzf,csf,alaf,dzgb,csgb,alagb,dzr,csr,                     &
                   alar,tlevb1D,qlevb1D,twb1D,twlevb1D,tflevb1D,tglevb1D,    &
                   trb1D,sflev1D,lflev1D,consumlev1D,eppvlev(ibui),          &
                   tpvlevb(1,ibui),                                          &
                   sfvlev1D,lfvlev1D,hfgrb(1,ibui),tr_avb(1,ibui),           &
                   tpv(ibui),sfpv(ibui),sfr_indoor(ibui))
          

!
!Temporal modifications
!        
         tpvlevb(2,ibui)=tpvlevb(1,ibui)
         sfrb(2,ibui)=sfrb(1,ibui)
         sfrvb(2,ibui)=sfrvb(1,ibui)
         lfrvb(2,ibui)=lfrvb(1,ibui)
         lfrb(2,ibui)=lfrb(1,ibui)
         sfrbpv(2,ibui)=sfrbpv(1,ibui)
         gfrb(2,ibui)=gfrb(1,ibui)
         hfgrb(2,ibui)=hfgrb(1,ibui)
!End temporal modifications  
!        
           do iz=1,nz_um
             qlev(iz,ibui)=qlevb1D(iz)
             tlev(iz,ibui)=tlevb1D(iz)
             sflev(iz,ibui)=sflev1D(iz)
             lflev(iz,ibui)=lflev1D(iz)
             consumlev(iz,ibui)=consumlev1D(iz)
             sfvlev(iz,ibui)=sfvlev1D(iz)
             lfvlev(iz,ibui)=lfvlev1D(iz)
           enddo
 
           do id=1,ndm
              do ily=1,nwr_u
                 trb(id,ily,ibui)=trb1D(ily)
              enddo   
              do ily=1,ngb_u
                 tglev(id,ily,ibui)=tglevb1D(ily) 
              enddo

              do ily=1,nf_u
              do iz=1,nz_um-1
                 tflev(id,ily,iz,ibui)=tflevb1D(ily,iz)
              enddo
              enddo
           

             do iz=1,nz_um
                do ily=1,nwr_u
                   tw(2*id-1,iz,ily,ibui)=twb1D(2*id-1,ily,iz)
                   tw(2*id,iz,ily,ibui)=twb1D(2*id,ily,iz)
                enddo
                gfw(2*id-1,iz,ibui)=gfwb1D(2*id-1,iz)
                gfw(2*id,iz,ibui)=gfwb1D(2*id,iz)
                twlev(2*id-1,iz,ibui)=twlevb1D(2*id-1,iz)
                twlev(2*id,iz,ibui)=twlevb1D(2*id,iz)
             enddo
           enddo       

        enddo !ibui   
   
!-----------------------------------------------------------------------------
!End loop over BEM -----------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

       ibui=0

        do iz=1,nzu!nz_um	
	   
         if(ss(iz).gt.0) then		
           ibui=ibui+1	
           do id=1,ndm	
              gfr(id,iz)=gfrb(id,ibui)
	      tpvlev(id,iz)=tpvlevb(id,ibui)
              sfr(id,iz)=sfrb(id,ibui)
              hfgr(id,iz)=hfgrb(id,ibui)
              sfrpv(id,iz)=-sfrbpv(id,ibui)
              lfr(id,iz)=lfrb(id,ibui)
              do ily=1,nwr_u
                 tr(id,iz,ily)=trb(id,ily,ibui)
              enddo
              ptr(id,iz)=tr(id,iz,nwr_u)*(pr_u(iz)/p0)**(-rcp_u)
           enddo
         endif
        enddo !iz

!Compute the potential temperature for the vertical surfaces of the buildings

       do id=1,ndm
          do iz=1,nzu!nz_um
             do ibui=1,nbui
                ptw(2*id-1,iz,ibui)=tw(2*id-1,iz,nwr_u,ibui)*(pr_u(iz)/p0)**(-rcp_u) 
                ptw(2*id,iz,ibui)=tw(2*id,iz,nwr_u,ibui)*(pr_u(iz)/p0)**(-rcp_u) 
                ptwin(2*id-1,iz,ibui)=twlev(2*id-1,iz,ibui)*(pr_u(iz)/p0)**(-rcp_u) 
                ptwin(2*id,iz,ibui)=twlev(2*id,iz,ibui)*(pr_u(iz)/p0)**(-rcp_u) 
              
             enddo
          enddo
       enddo
!NEW CDRAG!
     do iz=1,nz_um
       alp=0.
       do id=1,ndu
        alp=alp+bs(id)/(ws(id)+bs(id))*pb(iz)
       enddo
       alp=alp/ndu
       if(alp.lt.0.29)then
        cdrag(iz)=3.32*alp**0.47
       else
        cdrag(iz)=1.85
       endif
     enddo

             
        
! Compute the implicit and explicit components of the sources or sinks on the "urban grid"

      call buildings(iurb,ndu,nzu,z0,cdrag,ua_u,va_u,                               & 
                     pt_u,pt0_u,ptg,ptr,ptrv,da_u,qv_u,pr_u,tmp_u,ptw,ptwin,pwin_u(iurb),drst,     &                      
                     uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u,qvb_u,qhb_u,   & 
                     uhb_u,vhb_u,thb_u,ehb_u,ss,dt,sfw,sfg,sfr,sfrpv,sfrv,lfrv,   &
                     dgr,dg,lfr,lfg,                                                                    &
                     sfwin,pb,bs_u,dz_u,sflev,lflev,sfvlev,lfvlev,tvb_ac,ix,iy,rsg,rs,qr,gr_frac_roof,  &
                     pv_frac_roof,gr_flag,gr_type)  
      



! Calculation of the sensible heat fluxes for the ground, the wall and roof
! Sensible Heat Flux = density * Cp_U * ( A* potential temperature + B )
! where A and B are the implicit and explicit components of the heat sources or sinks.
      
! Interpolation on the "mesoscale grid"

      call urban_meso(ndu,kms,kme,kts,kte,nzu,z,dz,z_u,pb,ss,bs,ws,sf, & 
                     vl,uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u,     &
                     uhb_u,vhb_u,thb_u,ehb_u,qhb_u,qvb_u,              &
                     a_u,a_v,a_t,a_e,b_u,b_v,b_t,b_e,b_q,tvb_ac,b_ac)                    
       

! Calculation of the length scale taking into account the buildings effects

      call interp_length(ndu,kms,kme,kts,kte,nzu,z_u,z,ss,ws,bs,dlg,dl_u)
      
      return
      end subroutine BEP1D

! ===6=8===============================================================72
! ===6=8===============================================================72

       subroutine param(iurb,nzu,nzurb,nzurban,ndu,                   &
                       csg_u,csg,alag_u,alag,csr_u,csr,               &
                       alar_u,alar,csw_u,csw,alaw_u,alaw,             &
                       ws_u,ws_urb,ws,bs_u,bs_urb,bs,z0g_u,z0r_u,z0,  &  
                       strd_u,strd,drst_u,drst,ss_u,ss_urb,ss,pb_u,   &
                       pb_urb,pb,dzw,dzr,dzf,csf,alaf,dzgb,csgb,alagb,&
                       lp_urb,lb_urb,hgt_urb,frc_urb)        

! ----------------------------------------------------------------------
!    This routine prepare some usefull parameters       
! ----------------------------------------------------------------------

      implicit none

  
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer iurb                 ! Current urban class
      integer nzu                  ! Number of vertical urban levels in the current class
      integer ndu                  ! Number of street direction for the current urban class
      integer nzurb                ! Number of vertical urban levels in the current class
      real(kind=kind_noahmp) alag_u(nurbm)           ! Ground thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) alar_u(nurbm)           ! Roof thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) alaw_u(nurbm)           ! Wall thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) bs_u(ndm,nurbm)         ! Building width
      real(kind=kind_noahmp) csg_u(nurbm)            ! Specific heat of the ground material [J m^3 K^-1]
      real(kind=kind_noahmp) csr_u(nurbm)            ! Specific heat of the roof material [J m^3 K^-1]
      real(kind=kind_noahmp) csw_u(nurbm)            ! Specific heat of the wall material [J m^3 K^-1]
      real(kind=kind_noahmp) drst_u(ndm,nurbm)       ! Street direction
      real(kind=kind_noahmp) strd_u(ndm,nurbm)       ! Street length 
      real(kind=kind_noahmp) ws_u(ndm,nurbm)         ! Street width
      real(kind=kind_noahmp) z0g_u(nurbm)            ! The ground's roughness length
      real(kind=kind_noahmp) z0r_u(nurbm)            ! The roof's roughness length
      real(kind=kind_noahmp) ss_u(nz_um,nurbm)       ! The probability that a building has an height equal to "z"
      real(kind=kind_noahmp) pb_u(nz_um,nurbm)       ! The probability that a building has an height greater or equal to "z"
      real(kind=kind_noahmp) lp_urb                ! Building plan area density
      real(kind=kind_noahmp) lb_urb                ! Building surface area to plan area ratio
      real(kind=kind_noahmp) hgt_urb               ! Average building height weighted by building plan area [m]
      real(kind=kind_noahmp) frc_urb               ! Urban fraction

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) alag(ng_u)           ! Ground thermal diffusivity at each ground levels
      real(kind=kind_noahmp) csg(ng_u)            ! Specific heat of the ground material at each ground levels
      real(kind=kind_noahmp) bs(ndm)              ! Building width for the current urban class
      real(kind=kind_noahmp) drst(ndm)            ! street directions for the current urban class
      real(kind=kind_noahmp) strd(ndm)            ! Street lengths for the current urban class
      real(kind=kind_noahmp) ws(ndm)              ! Street widths of the current urban class
      real(kind=kind_noahmp) z0(ndm,nz_um)      ! Roughness lengths "profiles"
      real(kind=kind_noahmp) ss(nz_um)          ! Probability to have a building with height h
      real(kind=kind_noahmp) pb(nz_um)          ! Probability to have a building with an height greater or equal to "z"
      integer nzurban

!-----------------------------------------------------------------------------
!INPUT/OUTPUT
!-----------------------------------------------------------------------------

      real(kind=kind_noahmp) dzw(nwr_u)       !Layer sizes in the walls [m]
      real(kind=kind_noahmp) dzr(nwr_u)       !Layer sizes in the roofs [m]
      real(kind=kind_noahmp) dzf(nf_u)        !Layer sizes in the floors [m]
      real(kind=kind_noahmp) dzgb(ngb_u)      !layer sizes in the ground below the buildings [m]

      real(kind=kind_noahmp) csr(nwr_u)       ! Specific heat of the roof material at each roof levels
      real(kind=kind_noahmp) csw(nwr_u)       ! Specific heat of the wall material at each wall levels

      real(kind=kind_noahmp) csf(nf_u)        !Specific heat of the floors materials in the buildings 
                            !of the current urban class [J m^3 K^-1]
      real(kind=kind_noahmp) csgb(ngb_u)      !Specific heat of the ground material below the buildings 
                            !of the current urban class [J m^3 K^-1]
      real(kind=kind_noahmp) alar(nwr_u+1)    ! Roof thermal diffusivity at each roof levels [W/ m K]
      real(kind=kind_noahmp) alaw(nwr_u+1)    ! Wall thermal diffusivity at each wall levels [W/ m K]
      real(kind=kind_noahmp) alaf(nf_u+1)     ! Floor thermal diffusivity at each wall levels [W/m K]
      real(kind=kind_noahmp) alagb(ngb_u+1)   ! Ground thermal diffusivity below the building at each wall levels [W/m K]
      real(kind=kind_noahmp) bs_urb(ndm,nurbm)         ! Building width
      real(kind=kind_noahmp) ws_urb(ndm,nurbm)         ! Street width
      real(kind=kind_noahmp) ss_urb(nz_um,nurbm)       ! The probability that a building has an height equal to "z"
      real(kind=kind_noahmp) pb_urb(nz_um)             ! Probability that a building has an height greater or equal to z
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer id,ig,ir,iw,iz,iflo,ihu
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------  
!
!Initialize variables
!
      ss=0.
      pb=0.
      csg=0.
      alag=0.
      csgb=0.
      alagb=0.
      csf=0.
      alaf=0.
      csr=0.
      alar=0.
      csw=0.
      alaw=0.
      z0=0.
      ws=0.
      bs=0.
      bs_urb=0.
      ws_urb=0.
      strd=0.
      drst=0.
      nzurban=0

!Define the layer sizes in the walls

      dzgb=(/0.2,0.12,0.08,0.05,0.03,0.02,0.02,0.01,0.005,0.0025/)
      dzr=(/0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.0025/)   
      dzw=(/0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.0025/)
      dzf=(/0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02/) 
  
       ihu=0

       do iz=1,nz_um
          if (ss_urb(iz,iurb)/=0.) then
             ihu=1
             exit
          else
             continue
          endif
       enddo

       if (ihu==1) then
          do iz=1,nzurb+1
             ss(iz)=ss_urb(iz,iurb)
             pb(iz)=pb_urb(iz)
          enddo
          nzurban=nzurb
       else
          do iz=1,nzu+1
             ss(iz)=ss_u(iz,iurb)
             pb(iz)=pb_u(iz,iurb)
             ss_urb(iz,iurb)=ss_u(iz,iurb)
             pb_urb(iz)=pb_u(iz,iurb)
          end do 
          nzurban=nzu
       endif
      
      do ig=1,ngb_u
        csgb(ig) = csg_u(iurb)
        alagb(ig)= csg_u(iurb)*alag_u(iurb)
      enddo
      alagb(ngb_u+1)= csg_u(iurb)*alag_u(iurb)

      do iflo=1,nf_u
        csf(iflo) = csw_u(iurb)
        alaf(iflo)= csw_u(iurb)*alaw_u(iurb) 
      enddo
      alaf(nf_u+1)= csw_u(iurb)*alaw_u(iurb) 
     
      do ir=1,nwr_u
        csr(ir) = csr_u(iurb)
        alar(ir)= csr_u(iurb)*alar_u(iurb)
      enddo
      alar(nwr_u+1)= csr_u(iurb)*alar_u(iurb)

      do iw=1,nwr_u
        csw(iw) = csw_u(iurb)
        alaw(iw)= csw_u(iurb)*alaw_u(iurb)
      enddo
      alaw(nwr_u+1)=csw_u(iurb)*alaw_u(iurb) 

!------------------------------------------------------------------------  
                 
       do ig=1,ng_u
        csg(ig)=csg_u(iurb)
        alag(ig)=alag_u(iurb)
       enddo
       
       do id=1,ndu
          z0(id,1)=z0g_u(iurb)
        do iz=2,nzurban+1
           z0(id,iz)=z0r_u(iurb)
        enddo
       enddo
      
       do id=1,ndu
          strd(id)=strd_u(id,iurb)
          drst(id)=drst_u(id,iurb)     
       enddo

       do id=1,ndu
          if ((hgt_urb<=0.).OR.(lp_urb<=0.).OR.(lb_urb<=0.)) then
              ws(id)=ws_u(id,iurb)
              bs(id)=bs_u(id,iurb)
              bs_urb(id,iurb)=bs_u(id,iurb)
              ws_urb(id,iurb)=ws_u(id,iurb)
          else if ((lp_urb/frc_urb<1.).and.(lp_urb<lb_urb)) then
                  bs(id)=2.*hgt_urb*lp_urb/(lb_urb-lp_urb)
                  ws(id)=2.*hgt_urb*lp_urb*((frc_urb/lp_urb)-1.)/(lb_urb-lp_urb)
                  bs_urb(id,iurb)=bs(id)
                  ws_urb(id,iurb)=ws(id)
               else
                  ws(id)=ws_u(id,iurb)
                  bs(id)=bs_u(id,iurb)
                  bs_urb(id,iurb)=bs_u(id,iurb)
                  ws_urb(id,iurb)=ws_u(id,iurb)
          endif
       enddo
       do id=1,ndu
          if ((bs(id)<=1.).OR.(bs(id)>=150.)) then
             bs(id)=bs_u(id,iurb)
             ws(id)=ws_u(id,iurb)
             bs_urb(id,iurb)=bs_u(id,iurb)
             ws_urb(id,iurb)=ws_u(id,iurb)
          endif
          if ((ws(id)<=1.).OR.(ws(id)>=150.)) then
             ws(id)=ws_u(id,iurb)
             bs(id)=bs_u(id,iurb)
             bs_urb(id,iurb)=bs_u(id,iurb)
             ws_urb(id,iurb)=ws_u(id,iurb)
          endif
       enddo
       return
       end subroutine param
       
! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine interpol(kms,kme,kts,kte,nz_u,z,z_u,c,c_u)

! ----------------------------------------------------------------------
!  This routine interpolate para
!  meters from the "mesoscale grid" to
!  the "urban grid".
!  See p300 Appendix B.1 of the BLM paper.
! ----------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
! Data relative to the "mesoscale grid"
      integer kts,kte,kms,kme            
      real(kind=kind_noahmp) z(kms:kme)          ! Altitude of the cell interface
      real(kind=kind_noahmp) c(kms:kme)            ! Parameter which has to be interpolated
! Data relative to the "urban grid"
      integer nz_u          ! Number of levels
!!    real z_u(nz_u+1)      ! Altitude of the cell interface
      real(kind=kind_noahmp) z_u(nz_um)      ! Altitude of the cell interface

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
!!    real c_u(nz_u)        ! Interpolated paramters in the "urban grid"
      real(kind=kind_noahmp) c_u(nz_um)        ! Interpolated paramters in the "urban grid"      
 
! LOCAL:
! ----------------------------------------------------------------------
      integer iz_u,iz
      real(kind=kind_noahmp) ctot,dz

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

       do iz_u=1,nz_u
        ctot=0.
        do iz=kts,kte
         dz=max(min(z(iz+1),z_u(iz_u+1))-max(z(iz),z_u(iz_u)),0.)
         ctot=ctot+c(iz)*dz
        enddo
        c_u(iz_u)=ctot/(z_u(iz_u+1)-z_u(iz_u))
       enddo
       
       return
       end subroutine interpol
         
! ===6=8===============================================================72       
! ===6=8===============================================================72    

      subroutine  averaging_temp(tw,twlev,ss,pb,tw_av,twlev_av,       &
                                 sfw_av,sfwind_av,sfw,sfwin) 

      implicit none
!
!INPUT VARIABLES
!
      real(kind=kind_noahmp) tw(2*ndm,nz_um,nwr_u,nbui_max)        ! Temperature in each layer of the wall [K]
      real(kind=kind_noahmp) twlev(2*ndm,nz_um,nbui_max)     ! Window temperature in BEM [K]
      real(kind=kind_noahmp) pb(nz_um)                    ! Probability to have a building with an height equal or greater h
      real(kind=kind_noahmp) ss(nz_um)                    ! Probability to have a building with height h
      real(kind=kind_noahmp) sfw(2*ndm,nz_um,nbui_max)             ! Surface fluxes from the walls
      real(kind=kind_noahmp) sfwin(2*ndm,nz_um,nbui_max)     ! Surface fluxes from the windows
!
!OUTPUT VARIABLES
!
      real(kind=kind_noahmp) tw_av(2*ndm,nz_um)           ! Averaged temperature of the wall surfaces
      real(kind=kind_noahmp) twlev_av(2*ndm,nz_um)        ! Averaged temperature of the windows
      real(kind=kind_noahmp) sfw_av(2*ndm,nz_um)          ! Averaged sensible heat from walls
      real(kind=kind_noahmp) sfwind_av(2*ndm,nz_um)       ! Averaged sensible heat from windows
!
!LOCAL VARIABLES
!
      real(kind=kind_noahmp) d_urb(nz_um)    
      integer nlev(nz_um)            
      integer id,iz
      integer nbui,ibui
!
!Initialize Variables
!
      tw_av=0.
      twlev_av=0.
      sfw_av=0.
      sfwind_av=0.
      ibui=0
      nbui=0
      nlev=0
      d_urb=0.

      do iz=1,nz_um		   
         if(ss(iz).gt.0) then		
           ibui=ibui+1
           d_urb(ibui)=ss(iz)
           nlev(ibui)=iz-1
           nbui=ibui		               
         endif
      enddo
      
      do id=1,ndm
         do iz=1,nz_um-1
            if (pb(iz+1).gt.0) then
                do ibui=1,nbui
                   if (iz.le.nlev(ibui)) then
                      tw_av(2*id-1,iz)=tw_av(2*id-1,iz)+(d_urb(ibui)/pb(iz+1))*&
                                       tw(2*id-1,iz,nwr_u,ibui)**4
                      tw_av(2*id,iz)=tw_av(2*id,iz)+(d_urb(ibui)/pb(iz+1))*&
                                     tw(2*id,iz,nwr_u,ibui)**4
                      twlev_av(2*id-1,iz)=twlev_av(2*id-1,iz)+(d_urb(ibui)/pb(iz+1))*&
                                          twlev(2*id-1,iz,ibui)**4
                      twlev_av(2*id,iz)=twlev_av(2*id,iz)+(d_urb(ibui)/pb(iz+1))*&
                                        twlev(2*id,iz,ibui)**4
                      sfw_av(2*id-1,iz)=sfw_av(2*id-1,iz)+(d_urb(ibui)/pb(iz+1))*sfw(2*id-1,iz,ibui)
                      sfw_av(2*id,iz)=sfw_av(2*id,iz)+(d_urb(ibui)/pb(iz+1))*sfw(2*id,iz,ibui)
                      sfwind_av(2*id-1,iz)=sfwind_av(2*id-1,iz)+(d_urb(ibui)/pb(iz+1))*sfwin(2*id-1,iz,ibui)
                      sfwind_av(2*id,iz)=sfwind_av(2*id,iz)+(d_urb(ibui)/pb(iz+1))*sfwin(2*id,iz,ibui)
                   endif
                enddo
                tw_av(2*id-1,iz)=tw_av(2*id-1,iz)**(1./4.)
                tw_av(2*id,iz)=tw_av(2*id,iz)**(1./4.)
                twlev_av(2*id-1,iz)=twlev_av(2*id-1,iz)**(1./4.)
                twlev_av(2*id,iz)=twlev_av(2*id,iz)**(1./4.)
            endif
         enddo !iz         
      enddo !id
      return
      end subroutine averaging_temp
! ===6=8===============================================================72       
! ===6=8===============================================================72    

      subroutine modif_rad(iurb,nd,nz_u,z,ws,drst,strd,ss,pb,    &
                          tw,tg_av,twlev,albg,albw,emw,emg,pwin,albwin,   &
                          emwin,fww,fwg,fgw,fsw,fsg,             &
                          zr,deltar,ah,xlat,swddir,swddif,           &    
                          rs,rl,rsw,rsd,rsg,rlw,rlg)                       
 
! ----------------------------------------------------------------------
! This routine computes the modification of the short wave and 
!  long wave radiation due to the buildings.
! ----------------------------------------------------------------------

      implicit none
 
 
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer iurb              ! current urban class
      integer nd                ! Number of street direction for the current urban class
      integer nz_u              ! Number of layer in the urban grid
      real(kind=kind_noahmp) z(nz_um)           ! Height of the urban grid levels
      real(kind=kind_noahmp) ws(ndm)              ! Street widths of the current urban class
      real(kind=kind_noahmp) drst(ndm)            ! street directions for the current urban class
      real(kind=kind_noahmp) strd(ndm)            ! Street lengths for the current urban class
      real(kind=kind_noahmp) ss(nz_um)          ! probability to have a building with height h
      real(kind=kind_noahmp) pb(nz_um)          ! probability to have a building with an height equal
      real(kind=kind_noahmp) tw(2*ndm,nz_um)    ! Temperature in each layer of the wall [K]
      real(kind=kind_noahmp) tg_av(ndm)         ! Temperature in each layer of the ground [K]
      real(kind=kind_noahmp) albg                 ! Albedo of the ground for the current urban class
      real(kind=kind_noahmp) albw                 ! Albedo of the wall for the current urban class
      real(kind=kind_noahmp) emg                  ! Emissivity of ground for the current urban class
      real(kind=kind_noahmp) emw                  ! Emissivity of wall for the current urban class
      real(kind=kind_noahmp) fgw(nz_um,ndm,nurbm)       ! View factors from ground to wall
      real(kind=kind_noahmp) fsg(ndm,nurbm)             ! View factors from sky to ground
      real(kind=kind_noahmp) fsw(nz_um,ndm,nurbm)       ! View factors from sky to wall
      real(kind=kind_noahmp) fws(nz_um,ndm,nurbm)       ! View factors from wall to sky
      real(kind=kind_noahmp) fwg(nz_um,ndm,nurbm)       ! View factors from wall to ground
      real(kind=kind_noahmp) fww(nz_um,nz_um,ndm,nurbm) ! View factors from wall to wall
      real(kind=kind_noahmp) ah                   ! Hour angle (it should come from the radiation routine)
      real(kind=kind_noahmp) zr                   ! zenith angle
      real(kind=kind_noahmp) deltar               ! Declination of the sun
      real(kind=kind_noahmp) rs                   ! solar radiation
      real(kind=kind_noahmp) rl                   ! downward flux of the longwave radiation
      real(kind=kind_noahmp) xlat                 ! latitudine
      real(kind=kind_noahmp) swddir               ! short wave direct solar radiation  _gl
      real(kind=kind_noahmp) swddif               ! short wave diffuse solar radiation _gl

!
!New variables BEM
!
      real(kind=kind_noahmp) twlev(2*ndm,nz_um)         ! Window temperature in BEM [K]
      real(kind=kind_noahmp) pwin                       ! Coverage area fraction of windows in the walls of the buildings 
      real(kind=kind_noahmp) albwin                     ! Albedo of the windows for the current urban class
      real(kind=kind_noahmp) emwin                      ! Emissivity of the windows for the current urban class
      real(kind=kind_noahmp) alb_av                     ! Averaged albedo (window and wall)
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) rlg(ndm)             ! Long wave radiation at the ground
      real(kind=kind_noahmp) rlw(2*ndm,nz_um)     ! Long wave radiation at the walls
      real(kind=kind_noahmp) rsg(ndm)             ! Short wave radiation at the ground
      real(kind=kind_noahmp) rsw(2*ndm,nz_um)     ! Short wave radiation at the walls
      real(kind=kind_noahmp) rsd(2*ndm,nz_um)     ! Direct Short wave radiation at the walls

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------

      integer id,iz

!  Calculation of the shadow effects

      call shadow_mas(nd,nz_u,zr,deltar,ah,drst,ws,ss,pb,z,        &
                     swddir,rsw,rsg,xlat)
      rsd=rsw

! Calculation of the reflection effects          
      do id=1,nd
         call long_rad(iurb,nz_u,id,emw,emg,emwin,pwin,twlev,      &
                      fwg,fww,fgw,fsw,fsg,tg_av,tw,rlg,rlw,rl,pb)

         alb_av=pwin*albwin+(1.-pwin)*albw
         
        call short_rad_dd(iurb,nz_u,id,alb_av,                        &
                           albg,swddif,fwg,fww,fgw,fsw,fsg,rsg,rsw,pb)
 
 
      enddo
      return
      end subroutine modif_rad



! ===6=8===============================================================72  
! ===6=8===============================================================72     

      subroutine surf_temp(nd,pr,dt,rl,rsg,rlg,              &
                           tg,alag,csg,emg,albg,ptg,sfg,lfg,gfg) 

! ----------------------------------------------------------------------
! Computation of the surface temperatures for walls, ground and roofs 
! ----------------------------------------------------------------------

      implicit none
                  
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------

      integer nd                ! Number of street direction for the current urban class
      real(kind=kind_noahmp) alag(ng_u)           ! Ground thermal diffusivity for the current urban class [m^2 s^-1] 

      real(kind=kind_noahmp) albg                 ! Albedo of the ground for the current urban class

      real(kind=kind_noahmp) csg(ng_u)            ! Specific heat of the ground material of the current urban class [J m^3 K^-1]

      real(kind=kind_noahmp) dt                   ! Time step
      real(kind=kind_noahmp) emg                  ! Emissivity of ground for the current urban class

      real(kind=kind_noahmp) pr(nz_um)            ! Air pressure
      
      real(kind=kind_noahmp) rl                   ! Downward flux of the longwave radiation
      real(kind=kind_noahmp) rlg(ndm)             ! Long wave radiation at the ground
     
      real(kind=kind_noahmp) rsg(ndm)             ! Short wave radiation at the ground
      
      real(kind=kind_noahmp) sfg(ndm)             ! Sensible heat flux from ground (road)

      real(kind=kind_noahmp) lfg(ndm)             ! Latent heat flux from ground (road)

      real(kind=kind_noahmp) gfg(ndm)             ! Heat flux transferred from the surface of the ground (road) toward the interior

      real(kind=kind_noahmp) tg(ndm,ng_u)         ! Temperature in each layer of the ground [K]

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) ptg(ndm)             ! Ground potential temperatures 

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer id,ig,ir,iw,iz

      real(kind=kind_noahmp) rtg(ndm)             ! Total radiation at ground(road) surface (solar+incoming long+outgoing long)

      real(kind=kind_noahmp) tg_tmp(ng_u)

      real(kind=kind_noahmp) dzg_u(ng_u)          ! Layer sizes in the ground
      
      data dzg_u /0.2,0.12,0.08,0.05,0.03,0.02,0.02,0.01,0.005,0.0025/

     
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

        
   
      do id=1,nd

!      Calculation for the ground surfaces
       do ig=1,ng_u
        tg_tmp(ig)=tg(id,ig)
       end do
!	        
!       print*,'alag','cs',alag(1),csg(1)

       call soil_temp(ng_u,dzg_u,tg_tmp,ptg(id),alag,csg,      &
                     rsg(id),rlg(id),pr(1),                    &
                     dt,emg,albg,                              &
                     rtg(id),sfg(id),lfg(id),gfg(id))    

       do ig=1,ng_u
        tg(id,ig)=tg_tmp(ig)
       end do
	
      end do !id
      
      return
      end subroutine surf_temp


! ===6=8===============================================================72  
! ===6=8===============================================================72     


      subroutine roof_temp_veg(nd,pr,dt,rl,rsr,              &
                           trv,ptrv,sfrv,lfrv,gfr,qr,rainbl,drain,hfgroof,tr,alar,dzr,csr,nzu,irri_now,gr_type,pv_frac_roof,tpvlev)

! ----------------------------------------------------------------------
! Computation of the surface temperatures for walls, ground and roofs 
! ----------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) rainbl
      integer nd                ! Number of street direction for the current urban class

      integer nzu                ! Number of urban layers
      real(kind=kind_noahmp) irho(24)                ! Which hour of irrigation\


      real(kind=kind_noahmp) alar           ! Roof thermal diffusivity for the current urban class [m^2 s^-1] 
      real(kind=kind_noahmp) pv_frac_roof
      real(kind=kind_noahmp) csr

      real(kind=kind_noahmp) dzr          ! Layer sizes in the roofs [m]

      real(kind=kind_noahmp) dt                   ! Time step

      real(kind=kind_noahmp) pr(nz_um)            ! Air pressure

      real(kind=kind_noahmp) rl                   ! Downward flux of the longwave radiation

      real(kind=kind_noahmp) rsr                 ! Short wave radiation at the ground

     real(kind=kind_noahmp) tpvlev(ndm,nz_um)      

      real(kind=kind_noahmp) sfrv(ndm,nz_um)             ! Sensible heat flux from ground (road)

      real(kind=kind_noahmp) lfrv(ndm,nz_um)             ! Latent heat flux from ground (road)

      real(kind=kind_noahmp) gfr(ndm,nz_um)             ! Heat flux transferred from the surface of the ground (road) toward the interior

      real(kind=kind_noahmp) trv(ndm,nz_um,ngr_u)         ! Temperature in each layer of the green roof [K]

      real(kind=kind_noahmp) qr(ndm,nz_um,ngr_u)         ! Humidity in each layer of the green roof

      real(kind=kind_noahmp) tr(ndm,nz_um,nwr_u)         !Roof temperature in BEM [K]
 
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) ptrv(ndm,nz_um)             ! Ground potential temperatures 
      
      real(kind=kind_noahmp) hfgroof(ndm,nz_um)
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer id,ig,ir,iw,iz

      real(kind=kind_noahmp) alagr(ngr_u)           ! Green Roof thermal diffusivity for the current urban class [m^2 s^-1] 

      real(kind=kind_noahmp) rtr(ndm,nz_um)             ! Total radiation at ground(road) surface (solar+incoming long+outgoing long)

      real(kind=kind_noahmp) tr_tmp(ngr_u)

      real(kind=kind_noahmp) qr_tmp(ngr_u)
      real(kind=kind_noahmp) qr_tmp_old(ngr_u)
      real(kind=kind_noahmp) dzgr_u(ngr_u)          ! Layer sizes in the green roof
!MODIFICA
      data dzgr_u /0.1,0.003,0.06,0.003,0.05,0.04,0.02,0.0125,0.005,0.0025/
      real(kind=kind_noahmp) cs(ngr_u)  ! Specific heat of the ground material
      real(kind=kind_noahmp) cw
      parameter(cw=4.295e6)
      real(kind=kind_noahmp) s(ngr_u)
      real(kind=kind_noahmp) d(ngr_u)
      real(kind=kind_noahmp) k(ngr_u)
      real(kind=kind_noahmp) qr_m     ! mean soil moisture between layers
      real(kind=kind_noahmp) qrmax(ngr_u)
      real(kind=kind_noahmp) smax(ngr_u)
      real(kind=kind_noahmp) kmax(ngr_u)
      real(kind=kind_noahmp) b(ngr_u)
      real(kind=kind_noahmp) cd(ngr_u)
      real(kind=kind_noahmp) csa(4)
      real(kind=kind_noahmp) ka(4)
      real(kind=kind_noahmp) qref
      parameter(qref=0.37)
      data qrmax /0.0,0.0,0.0,0.0,0.439,0.37,0.37,0.37,0.37,0.37/
      data smax /0,0,0,0,-0.01,-0.1,-0.1,-0.1,-0.1,-0.1/
      data kmax /0,0,0,0,3.32e-3,2.162e-3,2.162e-3,2.162e-3,2.162e-3,2.162e-3/
      data b /0,0,0,0,2.7,3.9,3.9,3.9,3.9,3.9/
      data cd /0,0,0,0,331500,1.342e6,1.342e6,1.342e6,1.342e6,1.342e6/
      data csa /7.5e4,2.1e6,4.48e4,2.1e6/
      data ka /0.035,0.7,0.024,0.7/
      real(kind=kind_noahmp) em_gr(1)
      real(kind=kind_noahmp) alb_gr(1)
      real(kind=kind_noahmp) irri_now
      integer gr_type
      real(kind=kind_noahmp) drain(ndm,nz_um)
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS

      if(gr_type.eq.1)then
      em_gr=0.95
      alb_gr=0.3
      elseif(gr_type.eq.2)then
       em_gr=0.83
       alb_gr=0.154
      endif


      do iz=2,nzu

      do id=1,nd




!      Calculation for the ground surfaces

       do ig=1,ngr_u
        tr_tmp(ig)=trv(id,iz,ig)
        qr(id,iz,ig) = max(qr(id,iz,ig),1e-6) !cenlin, 11/4/2020
        qr_tmp(ig)=qr(id,iz,ig)
        qr_tmp_old(ig)=qr(id,iz,ig) 

      if(ig.le.4) then
 
       cs(ig)=csa(ig)
       alagr(ig)=ka(ig)/csa(ig)

      else
 
 
        if (ig.gt.5) then
        qr_m=(qr(id,iz,ig)*dzgr_u(ig-1)+qr(id,iz,ig-1)*dzgr_u(ig))/(dzgr_u(ig)+dzgr_u(ig-1))
        else
        qr_m=qr(id,iz,ig)
        endif
        cs(ig)=(1-qr_m)*cd(ig)+qr_m*cw
        s(ig)=smax(ig)*(qrmax(ig)/qr_m)**b(ig)
        k(ig)=kmax(ig)*(qr_m/qrmax(ig))**(2*b(ig)+3)
        d(ig)=-b(ig)*kmax(ig)*smax(ig)*((qr_m/qrmax(ig))**(b(ig)+3))/qr_m
        if (log10(abs(s(ig))).le.5.1) then
          alagr(ig)=exp(-(log10(abs(s(ig)))+2.7))*4.186e2/cs(ig)
        endif
        if (log10(abs(s(ig))).gt.5.1) then
          alagr(ig)=0.00041*4.186e2/cs(ig)
        endif

       endif

        end do
        hfgroof(id,iz)=(alar/csr+alagr(1))*(tr_tmp(1)-tr(id,iz,5))/(dzr+dzgr_u(1))
 
       call soil_temp_veg(hfgroof(id,iz),ngr_u,dzgr_u,tr_tmp,ptrv(id,iz),alagr,cs,      &
                     rsr,rl,pr(iz),                    &
                     dt,em_gr(1),alb_gr(1),                              &
                     rtr(id,iz),sfrv(id,iz),lfrv(id,iz),gfr(id,iz),pv_frac_roof,tpvlev(id,iz))
       do ig=1,ngr_u
        trv(id,iz,ig)=tr_tmp(ig)
       end do
        drain(id,iz)=kmax(5)*(qr(id,iz,5)/qrmax(5))**(2*b(5)+3)
        call soil_moist(ngr_u,dzgr_u,qr_tmp,dt,lfrv(id,iz),d,k,rainbl,drain(id,iz),irri_now)
 
     
        do ig=1,ngr_u
          ! qr(id,iz,ig)=min(qr_tmp(ig),qrmax(ig))
           qr(id,iz,ig)=max(min(qr_tmp(ig),qrmax(ig)),1e-6) !cenlin,11/4/2020
         end do
   
      end do !id
      end do !iz

      return
      end subroutine roof_temp_veg

! ===6=8===============================================================72     
! ===6=8===============================================================72  

      subroutine buildings(iurb,nd,nz,z0,cdrag,ua_u,va_u,pt_u,pt0_u,       &
                        ptg,ptr,ptrv,da_u,qv_u,pr_u,tmp_u,ptw,ptwin,pwin,                 &
                        drst,uva_u,vva_u,uvb_u,vvb_u,                &
                        tva_u,tvb_u,evb_u,qvb_u,qhb_u,               &
                        uhb_u,vhb_u,thb_u,ehb_u,ss,dt,sfw,sfg,sfr,sfrpv,sfrv,lfrv,   &
                        dgr,dg,lfr,lfg,                                               &
                        sfwin,pb,bs_u,dz_u,sflev,lflev,sfvlev,lfvlev,tvb_ac,ix,iy,rsg,rs,qr,gr_frac_roof,  &
                        pv_frac_roof,gr_flag,gr_type)                  

! ----------------------------------------------------------------------
! This routine computes the sources or sinks of the different quantities 
! on the urban grid. The actual calculation is done in the subroutines 
! called flux_wall, and flux_flat.
! ----------------------------------------------------------------------

      implicit none

        
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nd                ! Number of street direction for the current urban class
      integer ix,iy
      integer nz                ! number of vertical space steps
      real(kind=kind_noahmp) ua_u(nz_um)          ! Wind speed in the x direction on the urban grid
      real(kind=kind_noahmp) va_u(nz_um)          ! Wind speed in the y direction on the urban grid
      real(kind=kind_noahmp) da_u(nz_um)          ! air density on the urban grid
      real(kind=kind_noahmp) qv_u(nz_um)          ! specific humidity on the urban grid
      real(kind=kind_noahmp) pr_u(nz_um)          ! pressure on the urban grid
      real(kind=kind_noahmp) tmp_u(nz_um)         ! temperaure on the urban grid
      real(kind=kind_noahmp) drst(ndm)            ! Street directions for the current urban class
      real(kind=kind_noahmp) dz
      real(kind=kind_noahmp) pt_u(nz_um)          ! Potential temperature on the urban grid
      real(kind=kind_noahmp) pt0_u(nz_um)         ! reference potential temperature on the urban grid
      real(kind=kind_noahmp) ptg(ndm)             ! Ground potential temperatures 
      real(kind=kind_noahmp) ptr(ndm,nz_um)       ! Roof potential temperatures 
      real(kind=kind_noahmp) ptrv(ndm,nz_um)      ! Green Roof potential temperatures 
      real(kind=kind_noahmp) ptw(2*ndm,nz_um,nbui_max)     ! Walls potential temperatures 
      real(kind=kind_noahmp) ss(nz_um)            ! probability to have a building with height h
      real(kind=kind_noahmp) pb(nz_um)
      real(kind=kind_noahmp) cdrag(nz_um)
      real(kind=kind_noahmp) z0(ndm,nz_um)        ! Roughness lengths "profiles"
      real(kind=kind_noahmp) dt ! time step
      integer iurb              !Urban class
      real(kind=kind_noahmp) rsg(ndm)             ! Solar Radiation
      real(kind=kind_noahmp) rs                  ! Solar Radiation
      real(kind=kind_noahmp) qr(ndm,nz_um,ngr_u)         ! Ground Soil Moisture
      real(kind=kind_noahmp) trv(ndm,nz_um,ngr_u)         ! Ground Soil Moisture
      real(kind=kind_noahmp) roof_frac
      real(kind=kind_noahmp) road_frac
!
!New variables (BEM)
!
      real(kind=kind_noahmp) bs_u(ndm,nurbm)    ! Building width [m]
      real(kind=kind_noahmp) dz_u               ! Urban grid resolution
      real(kind=kind_noahmp) sflev(nz_um,nz_um)     ! sensible heat flux due to the air conditioning systems  [W]
      real(kind=kind_noahmp) lflev(nz_um,nz_um)     ! latent heat flux due to the air conditioning systems  [W]
      real(kind=kind_noahmp) sfvlev(nz_um,nz_um)    ! sensible heat flux due to ventilation [W]
      real(kind=kind_noahmp) lfvlev(nz_um,nz_um)    ! latent heat flux due to ventilation [W]
      real(kind=kind_noahmp) qvb_u(2*ndm,nz_um)
      real(kind=kind_noahmp) qhb_u(ndm,nz_um)
      real(kind=kind_noahmp) ptwin(2*ndm,nz_um,nbui_max)  ! window potential temperature
      real(kind=kind_noahmp) pwin
      real(kind=kind_noahmp) tvb_ac(2*ndm,nz_um)
      real(kind=kind_noahmp) gr_frac_roof
      real(kind=kind_noahmp) pv_frac_roof
      integer gr_flag,gr_type

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
! Explicit and implicit component of the momentum, temperature and TKE sources or sinks on
! vertical surfaces (walls) and horizontal surfaces (roofs and street)
! The fluxes can be computed as follow: Fluxes of X = A*X + B
!  Example: Momentum fluxes on vertical surfaces = uva_u * ua_u + uvb_u

      real(kind=kind_noahmp) uhb_u(ndm,nz_um)   ! U (wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) uva_u(2*ndm,nz_um)   ! U (wind component)   Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) uvb_u(2*ndm,nz_um)   ! U (wind component)   Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) vhb_u(ndm,nz_um)   ! V (wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) vva_u(2*ndm,nz_um)   ! V (wind component)   Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) vvb_u(2*ndm,nz_um)   ! V (wind component)   Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) thb_u(ndm,nz_um)   ! Temperature        Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) tva_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) tvb_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) ehb_u(ndm,nz_um)   ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) evb_u(2*ndm,nz_um)   ! Energy (TKE)         Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) uhb(2*ndm,nz_um)
      real(kind=kind_noahmp) vhb(2*ndm,nz_um)
      real(kind=kind_noahmp) ehb(2*ndm,nz_um)
      real(kind=kind_noahmp) sfw(2*ndm,nz_um,nbui_max)   ! sensible heat flux from walls
      real(kind=kind_noahmp) sfwin(2*ndm,nz_um,nbui_max) ! sensible heat flux form windows
      real(kind=kind_noahmp) sfr(ndm,nz_um)           ! sensible heat flux from roof
      real(kind=kind_noahmp) sfrv(ndm,nz_um)           ! sensible heat flux from roof
      real(kind=kind_noahmp) lfrv(ndm,nz_um)           ! Latent heat flux from roof
      real(kind=kind_noahmp) dgr(ndm,nz_um)           ! sensible heat flux from roof
      real(kind=kind_noahmp) dg(ndm)
      real(kind=kind_noahmp) lfr(ndm,nz_um)           ! Latent heat flux from roof
      real(kind=kind_noahmp) lfg(ndm)                 ! Latent heat flux from street
      real(kind=kind_noahmp) sfrpv(ndm,nz_um)         ! sensible heat flux from PV panels
      real(kind=kind_noahmp) sfg(ndm)                 ! sensible heat flux from street


! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) d_urb(nz_um)
      real(kind=kind_noahmp) uva_tmp
      real(kind=kind_noahmp) vva_tmp
      real(kind=kind_noahmp) uvb_tmp
      real(kind=kind_noahmp) vvb_tmp 
      real(kind=kind_noahmp) evb_tmp     
      integer nlev(nz_um)
      integer id,iz,ibui,nbui,il
      real(kind=kind_noahmp) wfg     !Ground water pool fraction
      real(kind=kind_noahmp) wfr     !Roof water pool fraction 
      real(kind=kind_noahmp) uhbv(2*ndm,nz_um)
      real(kind=kind_noahmp) vhbv(2*ndm,nz_um)
      real(kind=kind_noahmp) ehbv(2*ndm,nz_um)
      real(kind=kind_noahmp) z0v     !Vegetation roughness
      parameter(z0v=0.01)
      real(kind=kind_noahmp) resg
      real(kind=kind_noahmp) rsveg
      real(kind=kind_noahmp) f1,f2,f3,f4
      integer rsv(2)
      real(kind=kind_noahmp) qr_tmp(ngr_u)
      data rsv /0,1/
      real(kind=kind_noahmp) fh,ric,utot
!------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
      dz=dz_u
      ibui=0
      nbui=0
      nlev=0
      d_urb=0.
      
      uva_u=0.
      uvb_u=0.
      vhb_u=0.
      vva_u=0.
      vvb_u=0.
      thb_u=0.
      tva_u=0.
      tvb_u=0.
      tvb_ac=0.
      ehb_u=0.
      evb_u=0.
      qvb_u=0.
      qhb_u=0.
      
      uhb=0.
      vhb=0.
      ehb=0.
      uhbv=0.
      vhbv=0.
      ehbv=0.


      do iz=1,nz_um		   
         if(ss(iz).gt.0)then		
           ibui=ibui+1
           d_urb(ibui)=ss(iz)
           nlev(ibui)=iz-1
           nbui=ibui		               
         endif
      enddo

!        Calculation at the ground surfaces
      do id=1,nd
      
          call flux_flat(dz,z0(id,1),ua_u(1),va_u(1),pt_u(1),pt0_u(1),  &
                       ptg(id),qv_u(1),uhb(id,1),                            & 
                       vhb(id,1),sfg(id),lfg(id),ehb(id,1),da_u(1),pr_u(1))        
          if(dg(id).gt.0)then
           wfg=dg(id)/dgmax
           lfg(id)=-da_u(1)*latent*(-(wfg*lfg(id))/(da_u(1)*latent))
          else
           qhb_u(id,1)=0.
           lfg(id)=0.
          endif   
         thb_u(id,1)=-(sfg(id))/(da_u(1)*cp_u)
         vhb_u(id,1)=vhb(id,1)
         uhb_u(id,1)=uhb(id,1)
         ehb_u(id,1)=ehb(id,1)
         qhb_u(id,1)=-lfg(id)/(da_u(1)*latent)
         do iz=2,nz
            if(ss(iz).gt.0)then
            
               call flux_flat(dz,z0(id,iz),ua_u(iz),&
                       va_u(iz),pt_u(iz),pt0_u(iz), &
                       ptr(id,iz),qv_u(iz),uhb(id,iz),       &
                       vhb(id,iz),sfr(id,iz),lfr(id,iz),ehb(id,iz),da_u(iz),pr_u(iz))
         if(dgr(id,iz).gt.0)then
          wfr=dgr(id,iz)/drmax
          lfr(id,iz)=-da_u(iz)*latent*(-(wfr*lfr(id,iz))/(da_u(iz)*latent))
         else
          lfr(id,iz)=0.
         endif
         if(gr_flag.eq.1.and.gr_frac_roof.gt.0.)then  
         do il=1,ngr_u
           qr_tmp(il)=qr(id,iz,il)
         enddo
               call flux_flat_roof(dz,z0v,ua_u(iz),va_u(iz),pt_u(iz),pt0_u(iz),  &
                       ptrv(id,iz),uhbv(id,iz),                            &
                       vhbv(id,iz),sfrv(id,iz),lfrv(id,iz),ehbv(id,iz),da_u(iz),qv_u(iz),pr_u(iz),rs,qr_tmp,resg,rsveg,f1,f2,f3,f4,gr_type,pv_frac_roof)
         sfr(id,iz)=sfr(id,iz)+pv_frac_roof*sfrpv(id,iz) 
         thb_u(id,iz)=-((1.-gr_frac_roof)*sfr(id,iz)+gr_frac_roof*sfrv(id,iz))/(da_u(iz)*cp_u)
         vhb_u(id,iz)=(1.-gr_frac_roof)*vhb(id,iz)+gr_frac_roof*vhbv(id,iz)
         uhb_u(id,iz)=(1.-gr_frac_roof)*uhb(id,iz)+gr_frac_roof*uhbv(id,iz)
         ehb_u(id,iz)=(1.-gr_frac_roof)*ehb(id,iz)+gr_frac_roof*ehbv(id,iz)
         qhb_u(id,iz)=-(gr_frac_roof*lfrv(id,iz)+(1.-gr_frac_roof)*lfr(id,iz))/(da_u(iz)*latent)
         sfr(id,iz)=sfr(id,iz)-pv_frac_roof*sfrpv(id,iz)
         else
         sfr(id,iz)=sfr(id,iz)+pv_frac_roof*sfrpv(id,iz)
         thb_u(id,iz)=-sfr(id,iz)/(da_u(iz)*cp_u)
         vhb_u(id,iz)=vhb(id,iz)
         uhb_u(id,iz)=uhb(id,iz)
         ehb_u(id,iz)=ehb(id,iz)
         qhb_u(id,iz)=-lfr(id,iz)/(da_u(iz)*latent)
         sfr(id,iz)=sfr(id,iz)-pv_frac_roof*sfrpv(id,iz)
         endif
            else
               uhb_u(id,iz) = 0.0
               vhb_u(id,iz) = 0.0
               thb_u(id,iz) = 0.0
               ehb_u(id,iz) = 0.0
               qhb_u(id,iz) = 0.0
            endif
         enddo
         
         

!        Calculation at the wall surfaces        
 
         do ibui=1,nbui
         do iz=1,nlev(ibui)  
                   
            call flux_wall(ua_u(iz),va_u(iz),pt_u(iz),da_u(iz),             &  
                        ptw(2*id-1,iz,ibui),ptwin(2*id-1,iz,ibui),          &   
                        uva_tmp,vva_tmp,                                    &   
                        uvb_tmp,vvb_tmp,                                    &   
                        sfw(2*id-1,iz,ibui),sfwin(2*id-1,iz,ibui),          &   
                        evb_tmp,drst(id),dt,cdrag(iz))      
   
            if (pb(iz+1).gt.0.) then

                    uva_u(2*id-1,iz)=uva_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*uva_tmp
                    vva_u(2*id-1,iz)=vva_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*vva_tmp
                    uvb_u(2*id-1,iz)=uvb_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*uvb_tmp
                    vvb_u(2*id-1,iz)=vvb_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*vvb_tmp
                    evb_u(2*id-1,iz)=evb_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*evb_tmp
                    tvb_u(2*id-1,iz)=tvb_u(2*id-1,iz)-(d_urb(ibui)/pb(iz+1))*                       &
                                    (sfw(2*id-1,iz,ibui)*(1.-pwin)+sfwin(2*id-1,iz,ibui)*pwin)/     &
                                    da_u(iz)/cp_u-(1./4.)*(d_urb(ibui)/pb(iz+1))*(sfvlev(iz,ibui)-sflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/cp_u
                    tvb_ac(2*id-1,iz)=tvb_ac(2*id-1,iz)-(1./4.)*(d_urb(ibui)/pb(iz+1))*(-sflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/cp_u
                    qvb_u(2*id-1,iz)=qvb_u(2*id-1,iz)-(1./4.)*(d_urb(ibui)/pb(iz+1))*(lfvlev(iz,ibui)-lflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/latent
                                     
            endif

            call flux_wall(ua_u(iz),va_u(iz),pt_u(iz),da_u(iz),    &   
                        ptw(2*id,iz,ibui),ptwin(2*id,iz,ibui),     &    
                        uva_tmp,vva_tmp,                           &    
                        uvb_tmp,vvb_tmp,                           &    
                        sfw(2*id,iz,ibui),sfwin(2*id,iz,ibui),     &   
                        evb_tmp,drst(id),dt,cdrag(iz)) 

            if (pb(iz+1).gt.0.) then

                    uva_u(2*id,iz)=uva_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*uva_tmp
                    vva_u(2*id,iz)=vva_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*vva_tmp
                    uvb_u(2*id,iz)=uvb_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*uvb_tmp
                    vvb_u(2*id,iz)=vvb_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*vvb_tmp
                    evb_u(2*id,iz)=evb_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*evb_tmp
                    tvb_u(2*id,iz)=tvb_u(2*id,iz)-(d_urb(ibui)/pb(iz+1))*                    &
                                    (sfw(2*id,iz,ibui)*(1.-pwin)+sfwin(2*id,iz,ibui)*pwin)/  &
                                     da_u(iz)/cp_u-(1./4.)*(d_urb(ibui)/pb(iz+1))*(sfvlev(iz,ibui)-sflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/cp_u
                    tvb_ac(2*id,iz)=tvb_ac(2*id,iz)-(1./4.)*(d_urb(ibui)/pb(iz+1))*(-sflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/cp_u
                    qvb_u(2*id,iz)=qvb_u(2*id,iz)-(1./4.)*(d_urb(ibui)/pb(iz+1))*(lfvlev(iz,ibui)-lflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/latent

            endif
!
          enddo !iz
         enddo !ibui
         
      end do !id
                
      return
      end subroutine buildings
      

! ===6=8===============================================================72
! ===6=8===============================================================72

        subroutine urban_meso(nd,kms,kme,kts,kte,nz_u,z,dz,z_u,pb,ss,bs,ws,sf,vl,    &
                             uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u, &       
                             uhb_u,vhb_u,thb_u,ehb_u,qhb_u,qvb_u,       &      
                             a_u,a_v,a_t,a_e,b_u,b_v,b_t,b_e,b_q,tvb_ac,b_ac)           

! ----------------------------------------------------------------------
!  This routine interpolates the parameters from the "urban grid" to the
!  "mesoscale grid".
!  See p300-301 Appendix B.2 of the BLM paper.  
! ----------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
! Data relative to the "mesoscale grid"
      integer kms,kme,kts,kte               
      real(kind=kind_noahmp) z(kms:kme)              ! Altitude above the ground of the cell interface
      real(kind=kind_noahmp) dz(kms:kme)               ! Vertical space steps

! Data relative to the "uban grid"
      integer nz_u              ! Number of layer in the urban grid
      integer nd                ! Number of street direction for the current urban class
      real(kind=kind_noahmp) bs(ndm)              ! Building widths of the current urban class
      real(kind=kind_noahmp) ws(ndm)              ! Street widths of the current urban class
      real(kind=kind_noahmp) z_u(nz_um)         ! Height of the urban grid levels
      real(kind=kind_noahmp) pb(nz_um)          ! Probability to have a building with an height equal
      real(kind=kind_noahmp) ss(nz_um)          ! Probability to have a building with height h
      real(kind=kind_noahmp) uhb_u(ndm,nz_um)   ! U (x-wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) uva_u(2*ndm,nz_um)   ! U (x-wind component) Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) uvb_u(2*ndm,nz_um)   ! U (x-wind component) Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) vhb_u(ndm,nz_um)   ! V (y-wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) vva_u(2*ndm,nz_um)   ! V (y-wind component) Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) vvb_u(2*ndm,nz_um)   ! V (y-wind component) Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) thb_u(ndm,nz_um)   ! Temperature        Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) tva_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) tvb_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) tvb_ac(2*ndm,nz_um)
      real(kind=kind_noahmp) ehb_u(ndm,nz_um)   ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) evb_u(2*ndm,nz_um)   ! Energy (TKE)         Vertical surfaces, B (explicit) term
!
!New variables for BEM
!
      real(kind=kind_noahmp) qhb_u(ndm,nz_um)
      real(kind=kind_noahmp) qvb_u(2*ndm,nz_um)
     

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
! Data relative to the "mesoscale grid"
      real(kind=kind_noahmp) sf(kms:kme)             ! Surface of the "mesoscale grid" cells taking into account the buildings
      real(kind=kind_noahmp) vl(kms:kme)               ! Volume of the "mesoscale grid" cells taking into account the buildings
      real(kind=kind_noahmp) a_u(kms:kme)              ! Implicit component of the momentum sources or sinks in the X-direction
      real(kind=kind_noahmp) a_v(kms:kme)              ! Implicit component of the momentum sources or sinks in the Y-direction
      real(kind=kind_noahmp) a_t(kms:kme)              ! Implicit component of the heat sources or sinks
      real(kind=kind_noahmp) a_e(kms:kme)              ! Implicit component of the TKE sources or sinks
      real(kind=kind_noahmp) b_u(kms:kme)              ! Explicit component of the momentum sources or sinks in the X-direction
      real(kind=kind_noahmp) b_v(kms:kme)              ! Explicit component of the momentum sources or sinks in the Y-direction
      real(kind=kind_noahmp) b_t(kms:kme)              ! Explicit component of the heat sources or sinks
      real(kind=kind_noahmp) b_ac(kms:kme)
      real(kind=kind_noahmp) b_e(kms:kme)              ! Explicit component of the TKE sources or sinks
      real(kind=kind_noahmp) b_q(kms:kme)               ! Explicit component of the humidity sources or sinks
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) dzz
      real(kind=kind_noahmp) fact
      integer id,iz,iz_u
      real(kind=kind_noahmp) se,sr,st,su,sv,sq
      real(kind=kind_noahmp )uet(kms:kme)                ! Contribution to TKE due to walls
      real(kind=kind_noahmp) veb,vta,vtb,vte,vtot,vua,vub,vva,vvb,vqb,vtb_ac


! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ---------------------------------------------------------------------- 

! initialisation

      do iz=kts,kte
         a_u(iz)=0.
         a_v(iz)=0.
         a_t(iz)=0.
         a_e(iz)=0.
         b_u(iz)=0.
         b_v(iz)=0.
         b_e(iz)=0.
         b_t(iz)=0.
         b_ac(iz)=0.
         uet(iz)=0.
         b_q(iz)=0.
      end do
            
! horizontal surfaces
      do iz=kts,kte
         sf(iz)=0.
         vl(iz)=0.
      enddo
      sf(kte+1)=0. 
      
      do id=1,nd      
         do iz=kts+1,kte+1
            sr=0.
            do iz_u=2,nz_u
               if(z(iz).lt.z_u(iz_u).and.z(iz).ge.z_u(iz_u-1))then
                  sr=pb(iz_u)
               endif
            enddo
            sf(iz)=sf(iz)+((ws(id)+(1.-sr)*bs(id))/(ws(id)+bs(id)))/nd
         enddo
      enddo

! volume      
      do iz=kts,kte
         do id=1,nd
            vtot=0.
            do iz_u=1,nz_u
               dzz=max(min(z_u(iz_u+1),z(iz+1))-max(z_u(iz_u),z(iz)),0.)
               vtot=vtot+pb(iz_u+1)*dzz
            enddo
            vtot=vtot/(z(iz+1)-z(iz))
            vl(iz)=vl(iz)+(1.-vtot*bs(id)/(ws(id)+bs(id)))/nd
         enddo
      enddo
      
! horizontal surface impact  

      do id=1,nd
      
         fact=1./vl(kts)/dz(kts)*ws(id)/(ws(id)+bs(id))/nd
         b_t(kts)=b_t(kts)+thb_u(id,1)*fact
         b_u(kts)=b_u(kts)+uhb_u(id,1)*fact
         b_v(kts)=b_v(kts)+vhb_u(id,1)*fact 
         b_e(kts)=b_e(kts)+ehb_u(id,1)*fact*(z_u(2)-z_u(1))
         b_q(kts)=b_q(kts)+qhb_u(id,1)*fact         

         do iz=kts,kte
            st=0.
            su=0.
            sv=0.
            se=0.
            sq=0.
            do iz_u=2,nz_u
               if(z(iz).le.z_u(iz_u).and.z(iz+1).gt.z_u(iz_u))then
                  st=st+ss(iz_u)*thb_u(id,iz_u)
                  su=su+ss(iz_u)*uhb_u(id,iz_u)
                  sv=sv+ss(iz_u)*vhb_u(id,iz_u)          
                  se=se+ss(iz_u)*ehb_u(id,iz_u)*(z_u(iz_u+1)-z_u(iz_u))
                  sq=sq+ss(iz_u)*qhb_u(id,iz_u)
               endif
            enddo
      
            fact=bs(id)/(ws(id)+bs(id))/vl(iz)/dz(iz)/nd
            b_t(iz)=b_t(iz)+st*fact
            b_u(iz)=b_u(iz)+su*fact
            b_v(iz)=b_v(iz)+sv*fact
            b_e(iz)=b_e(iz)+se*fact
            b_q(iz)=b_q(iz)+sq*fact
         enddo
      enddo              

! vertical surface impact
           
      do iz=kts,kte 
         uet(iz)=0.
         do id=1,nd              
            vtb=0.
            vtb_ac=0.
            vta=0.
            vua=0.
            vub=0.
            vva=0.
            vvb=0.
            veb=0.
	    vte=0.
            vqb=0.
            do iz_u=1,nz_u
               dzz=max(min(z_u(iz_u+1),z(iz+1))-max(z_u(iz_u),z(iz)),0.)
               fact=dzz/(ws(id)+bs(id))
               vtb=vtb+pb(iz_u+1)*                                  &        
                    (tvb_u(2*id-1,iz_u)+tvb_u(2*id,iz_u))*fact 
               vtb_ac=vtb_ac+pb(iz_u+1)*                            &        
                    (tvb_ac(2*id-1,iz_u)+tvb_ac(2*id,iz_u))*fact     
               vta=vta+pb(iz_u+1)*                                  &        
                   (tva_u(2*id-1,iz_u)+tva_u(2*id,iz_u))*fact
               vua=vua+pb(iz_u+1)*                                  &        
                    (uva_u(2*id-1,iz_u)+uva_u(2*id,iz_u))*fact
               vva=vva+pb(iz_u+1)*                                  &        
                    (vva_u(2*id-1,iz_u)+vva_u(2*id,iz_u))*fact
               vub=vub+pb(iz_u+1)*                                  &        
                    (uvb_u(2*id-1,iz_u)+uvb_u(2*id,iz_u))*fact
               vvb=vvb+pb(iz_u+1)*                                  &        
                    (vvb_u(2*id-1,iz_u)+vvb_u(2*id,iz_u))*fact
               veb=veb+pb(iz_u+1)*                                  &        
                    (evb_u(2*id-1,iz_u)+evb_u(2*id,iz_u))*fact
               vqb=vqb+pb(iz_u+1)*                                  &        
                    (qvb_u(2*id-1,iz_u)+qvb_u(2*id,iz_u))*fact   
            enddo
           
            fact=1./vl(iz)/dz(iz)/nd
            b_t(iz)=b_t(iz)+vtb*fact
            b_ac(iz)=b_ac(iz)+vtb_ac*fact
            a_t(iz)=a_t(iz)+vta*fact
            a_u(iz)=a_u(iz)+vua*fact
            a_v(iz)=a_v(iz)+vva*fact
            b_u(iz)=b_u(iz)+vub*fact
            b_v(iz)=b_v(iz)+vvb*fact
            b_e(iz)=b_e(iz)+veb*fact
            uet(iz)=uet(iz)+vte*fact
            b_q(iz)=b_q(iz)+vqb*fact
         enddo              
      enddo
      

      
      return
      end subroutine urban_meso

! ===6=8===============================================================72 
! ===6=8===============================================================72 

      subroutine interp_length(nd,kms,kme,kts,kte,nz_u,z_u,z,ss,ws,bs,              &
                             dlg,dl_u)

! ----------------------------------------------------------------------     
!    Calculation of the length scales
!    See p272-274 formula (22) and (24) of the BLM paper    
! ----------------------------------------------------------------------     
     
      implicit none


! ----------------------------------------------------------------------     
! INPUT:
! ----------------------------------------------------------------------     
      integer kms,kme,kts,kte                
      real(kind=kind_noahmp) z(kms:kme)              ! Altitude above the ground of the cell interface
      integer nd                ! Number of street direction for the current urban class
      integer nz_u              ! Number of levels in the "urban grid"
      real(kind=kind_noahmp) z_u(nz_um)         ! Height of the urban grid levels
      real(kind=kind_noahmp) bs(ndm)              ! Building widths of the current urban class
      real(kind=kind_noahmp) ss(nz_um)          ! Probability to have a building with height h
      real(kind=kind_noahmp) ws(ndm)              ! Street widths of the current urban class


! ----------------------------------------------------------------------     
! OUTPUT:
! ----------------------------------------------------------------------     
      real(kind=kind_noahmp) dlg(kms:kme)              ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real(kind=kind_noahmp) dl_u(kms:kme)             ! Length scale (lb in formula (22) ofthe BLM paper).

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) dlgtmp
      integer id,iz,iz_u
      real(kind=kind_noahmp) sftot
      real(kind=kind_noahmp) ulu,ssl

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
   
        do iz=kts,kte
         ulu=0.
         ssl=0.
         do id=1,nd        
          do iz_u=2,nz_u
           if(z_u(iz_u).gt.z(iz))then
            ulu=ulu+ss(iz_u)/z_u(iz_u)/nd
            ssl=ssl+ss(iz_u)/nd
           endif
          enddo
         enddo

        if(ulu.ne.0)then
          dl_u(iz)=ssl/ulu
         else
          dl_u(iz)=0.
         endif
        enddo
       

        do iz=kts,kte
         dlg(iz)=0.
          do id=1,nd
           sftot=ws(id)  
           dlgtmp=ws(id)/((z(iz)+z(iz+1))/2.)
           do iz_u=1,nz_u
            if((z(iz)+z(iz+1))/2..gt.z_u(iz_u))then
             dlgtmp=dlgtmp+ss(iz_u)*bs(id)/                           &                
                    ((z(iz)+z(iz+1))/2.-z_u(iz_u))
             sftot=sftot+ss(iz_u)*bs(id)
            endif
           enddo
           dlg(iz)=dlg(iz)+dlgtmp/sftot/nd
         enddo
         dlg(iz)=1./dlg(iz)
        enddo
        
       return
       end subroutine interp_length

! ===6=8===============================================================72
! ===6=8===============================================================72   

      subroutine shadow_mas(nd,nz_u,zr,deltar,ah,drst,ws,ss,pb,z,    &
                           swddir,rsw,rsg,xlat)
        
! ----------------------------------------------------------------------
!         Modification of short wave radiation to take into account
!         the shadow produced by the buildings
! ----------------------------------------------------------------------

      implicit none
     
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nd                ! Number of street direction for the current urban class
      integer nz_u              ! number of vertical layers defined in the urban grid
      real(kind=kind_noahmp) ah                   ! Hour angle (it should come from the radiation routine)
      real(kind=kind_noahmp) deltar               ! Declination of the sun
      real(kind=kind_noahmp) drst(ndm)            ! street directions for the current urban class
      real(kind=kind_noahmp) swddir                   ! solar radiation
      real(kind=kind_noahmp) ss(nz_um)          ! probability to have a building with height h
      real(kind=kind_noahmp) pb(nz_um)          ! Probability that a building has an height greater or equal to h
      real(kind=kind_noahmp) ws(ndm)              ! Street width of the current urban class
      real(kind=kind_noahmp) z(nz_um)           ! Height of the urban grid levels
      real(kind=kind_noahmp) zr                   ! zenith angle
      real(kind=kind_noahmp) xlat
      real(kind=kind_noahmp) xlat_r
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) rsg(ndm)             ! Short wave radiation at the ground
      real(kind=kind_noahmp) rsw(2*ndm,nz_um)     ! Short wave radiation at the walls

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer id,iz,jz
      real(kind=kind_noahmp) aae,aaw,bbb,phix,rd,rtot,wsd
      
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

         xlat_r=xlat*pi/180

        if(swddir.eq.0.or.sin(zr).eq.1)then
           do id=1,nd
             rsg(id)=0.
             do iz=1,nz_u
               rsw(2*id-1,iz)=0.
               rsw(2*id,iz)=0.
            enddo
         enddo
        else
!test              

        if(abs(sin(zr)).gt.1.e-10)then
          if(cos(deltar)*sin(ah)/sin(zr).ge.1)then
           bbb=pi/2.
          elseif(cos(deltar)*sin(ah)/sin(zr).le.-1)then
           bbb=-pi/2.
          else
           bbb=asin(cos(deltar)*sin(ah)/sin(zr))                !
           if(sin(deltar).lt.(cos(zr)*sin(xlat_r)))then         !
           bbb=pi-bbb                                           !
          endif
          endif
         else
          if(cos(deltar)*sin(ah).ge.0)then
           bbb=pi/2.
          elseif(cos(deltar)*sin(ah).lt.0)then
           bbb=-pi/2.
          endif
         endif
        phix=zr
           
         do id=1,nd
         
            rsg(id)=0.

            aae=bbb-drst(id)
            aaw=bbb-drst(id)+pi

            do iz=1,nz_u
               rsw(2*id-1,iz)=0.
               rsw(2*id,iz)=0.
            if(pb(iz+1).gt.0.)then
               do jz=1,nz_u
                if(abs(sin(aae)).gt.1.e-10)then
                  call shade_wall(z(iz),z(iz+1),z(jz+1),phix,aae,   &
                      ws(id),rd)
                  rsw(2*id-1,iz)=rsw(2*id-1,iz)+swddir*rd*ss(jz+1)/pb(iz+1)
                endif

                if(abs(sin(aaw)).gt.1.e-10)then
                  call shade_wall(z(iz),z(iz+1),z(jz+1),phix,aaw,   &
                      ws(id),rd)
                  rsw(2*id,iz)=rsw(2*id,iz)+swddir*rd*ss(jz+1)/pb(iz+1)
                endif
               enddo
            endif
            enddo
        if(abs(sin(aae)).gt.1.e-10)then
            wsd=abs(ws(id)/sin(aae))

            do jz=1,nz_u
               rd=max(0.,wsd-z(jz+1)*tan(phix))
               rsg(id)=rsg(id)+swddir*rd*ss(jz+1)/wsd
            enddo
            rtot=0.

            do iz=1,nz_u
               rtot=rtot+(rsw(2*id,iz)+rsw(2*id-1,iz))*            &
                         (z(iz+1)-z(iz))
            enddo
            rtot=rtot+rsg(id)*ws(id)
        else
            rsg(id)=swddir
        endif


            
         enddo
      endif
         
      return
      end subroutine shadow_mas




         
! ===6=8===============================================================72     
! ===6=8===============================================================72     

      subroutine shade_wall(z1,z2,hu,phix,aa,ws,rd)

! ----------------------------------------------------------------------
! This routine computes the effects of a shadow induced by a building of 
! height hu, on a portion of wall between z1 and z2. See equation A10, 
! and correction described below formula A11, and figure A1. Basically rd
! is the ratio between the horizontal surface illuminated and the portion
! of wall. Referring to figure A1, multiplying radiation flux density on 
! a horizontal surface (rs) by x1-x2 we have the radiation energy per 
! unit time. Dividing this by z2-z1, we obtain the radiation flux 
! density reaching the portion of the wall between z2 and z1 
! (everything is assumed in 2D)
! ----------------------------------------------------------------------

      implicit none
      
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) aa                   ! Angle between the sun direction and the face of the wall (A12)
      real(kind=kind_noahmp) hu                   ! Height of the building that generates the shadow
      real(kind=kind_noahmp) phix                 ! Solar zenith angle
      real(kind=kind_noahmp) ws                   ! Width of the street
      real(kind=kind_noahmp) z1                   ! Height of the level z(iz)
      real(kind=kind_noahmp) z2                   ! Height of the level z(iz+1)

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) rd                   ! Ratio between (x1-x2)/(z2-z1), see Fig. 1A. 
                                ! Multiplying rd by rs (radiation flux 
                                ! density on a horizontal surface) gives 
                                ! the radiation flux density on the 
                                ! portion of wall between z1 and z2. 
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) x1,x2                ! x1,x2 see Fig. A1.

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

      x1=min((hu-z1)*tan(phix),max(0.,ws/sin(aa)))
      
      x2=max((hu-z2)*tan(phix),0.)

      rd=max(0.,sin(aa)*(max(0.,x1-x2))/(z2-z1))
      
      return
      end subroutine shade_wall

! ===6=8===============================================================72     
! ===6=8===============================================================72     

      subroutine long_rad(iurb,nz_u,id,emw,emg,emwin,pwin,twlev,&
                         fwg,fww,fgw,fsw,fsg,tg_av,tw,rlg,rlw,rl,pb)

! ----------------------------------------------------------------------
! This routine computes the effects of the reflections of long-wave 
! radiation in the street canyon by solving the system 
! of 2*nz_u+1 eqn. in 2*nz_u+1
! unkonwn defined in A4, A5 and A6 of the paper (pages 295 and 296).
! The system is solved by solving A X= B,
! with A matrix B vector, and X solution. 
! ----------------------------------------------------------------------

      implicit none

  
      
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) emg                        ! Emissivity of ground for the current urban class
      real(kind=kind_noahmp) emw                        ! Emissivity of wall for the current urban class
      real(kind=kind_noahmp) fgw(nz_um,ndm,nurbm)       ! View factors from ground to wall
      real(kind=kind_noahmp) fsg(ndm,nurbm)             ! View factors from sky to ground
      real(kind=kind_noahmp) fsw(nz_um,ndm,nurbm)       ! View factors from sky to wall
      real(kind=kind_noahmp) fwg(nz_um,ndm,nurbm)       ! View factors from wall to ground
      real(kind=kind_noahmp) fww(nz_um,nz_um,ndm,nurbm) ! View factors from wall to wall
      integer id                      ! Current street direction
      integer iurb                    ! Current urban class
      integer nz_u                    ! Number of layer in the urban grid
      real(kind=kind_noahmp) pb(nz_um)                  ! Probability to have a building with an height equal
      real(kind=kind_noahmp) rl                         ! Downward flux of the longwave radiation
      real(kind=kind_noahmp) tg_av(ndm)               ! Temperature in each layer of the ground [K]
      real(kind=kind_noahmp) tw(2*ndm,nz_um)            ! Temperature in each layer of the wall [K]
!
!New Variables for BEM
!
      real(kind=kind_noahmp) twlev(2*ndm,nz_um)         ! Window temperature in BEM [K]
      real(kind=kind_noahmp) emwin                      ! Emissivity of windows
      real(kind=kind_noahmp) pwin                       ! Coverage area fraction of windows in the walls of the buildings (BEM)
      

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) rlg(ndm)                   ! Long wave radiation at the ground
      real(kind=kind_noahmp) rlw(2*ndm,nz_um)           ! Long wave radiation at the walls

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer i,j
      real(kind=kind_noahmp) aaa(2*nz_um+1,2*nz_um+1)   ! terms of the matrix
      real(kind=kind_noahmp) bbb(2*nz_um+1)             ! terms of the vector

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------


! west wall
       
      do i=1,nz_u
        
        do j=1,nz_u
         aaa(i,j)=0.
        enddo
        
        aaa(i,i)=1.        
       
        do j=nz_u+1,2*nz_u
         aaa(i,j)=-(1.-emw*(1.-pwin)-emwin*pwin)* &
                  fww(j-nz_u,i,id,iurb)*pb(j-nz_u+1)
        enddo
        
!!      aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i,id,iurb)*pb(i+1)
        aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i,id,iurb)
        
        bbb(i)=fsw(i,id,iurb)*rl+emg*fgw(i,id,iurb)*sigma*tg_av(id)**4
        do j=1,nz_u
           bbb(i)=bbb(i)+pb(j+1)*sigma*fww(j,i,id,iurb)* &
                 (emw*(1.-pwin)*tw(2*id,j)**4+emwin*pwin*twlev(2*id,j)**4)+ &
                 fww(j,i,id,iurb)*rl*(1.-pb(j+1))
        enddo
        
       enddo
       
! east wall

       do i=1+nz_u,2*nz_u
        
        do j=1,nz_u
         aaa(i,j)=-(1.-emw*(1.-pwin)-emwin*pwin)*fww(j,i-nz_u,id,iurb)*pb(j+1)
        enddo
        
        do j=1+nz_u,2*nz_u
         aaa(i,j)=0.
        enddo
        
        aaa(i,i)=1.
        
!!      aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i-nz_u,id,iurb)*pb(i-nz_u+1)
        aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i-nz_u,id,iurb)
        
        bbb(i)=fsw(i-nz_u,id,iurb)*rl+  &     
               emg*fgw(i-nz_u,id,iurb)*sigma*tg_av(id)**4

        do j=1,nz_u
         bbb(i)=bbb(i)+pb(j+1)*sigma*fww(j,i-nz_u,id,iurb)*  &   
                (emw*(1.-pwin)*tw(2*id-1,j)**4+emwin*pwin*twlev(2*id-1,j)**4)+&   
                fww(j,i-nz_u,id,iurb)*rl*(1.-pb(j+1))
        enddo
       
       enddo

! ground
       do j=1,nz_u
        aaa(2*nz_u+1,j)=-(1.-emw*(1.-pwin)-emwin*pwin)* &
                         fwg(j,id,iurb)*pb(j+1)
       enddo
       
       do j=nz_u+1,2*nz_u
        aaa(2*nz_u+1,j)=-(1.-emw*(1.-pwin)-emwin*pwin)* &
                         fwg(j-nz_u,id,iurb)*pb(j-nz_u+1)
       enddo
       
       aaa(2*nz_u+1,2*nz_u+1)=1.
       
       bbb(2*nz_u+1)=fsg(id,iurb)*rl
       
       do i=1,nz_u
        bbb(2*nz_u+1)=bbb(2*nz_u+1)+sigma*fwg(i,id,iurb)*pb(i+1)*         &
                      (emw*(1.-pwin)*(tw(2*id-1,i)**4+tw(2*id,i)**4)+     &
                      emwin*pwin*(twlev(2*id-1,i)**4+twlev(2*id,i)**4))+  &
                      2.*fwg(i,id,iurb)*(1.-pb(i+1))*rl                  
       enddo
   

     
       call gaussj(aaa,2*nz_u+1,bbb,2*nz_um+1)

       do i=1,nz_u
        rlw(2*id-1,i)=bbb(i)
       enddo
       
       do i=nz_u+1,2*nz_u
        rlw(2*id,i-nz_u)=bbb(i)
       enddo
       
       rlg(id)=bbb(2*nz_u+1)
  
       return
       end subroutine long_rad
             
! ===6=8===============================================================72
! ===6=8===============================================================72

 
      subroutine short_rad_dd(iurb,nz_u,id,albw,                        & 
                           albg,rsdif,fwg,fww,fgw,fsw,fsg,rsg,rsw,pb)

! ----------------------------------------------------------------------
! This routine computes the effects of the reflections of short-wave 
! (solar) radiation in the street canyon by solving the system 
! of 2*nz_u+1 eqn. in 2*nz_u+1
! unkonwn defined in A4, A5 and A6 of the paper (pages 295 and 296).
! The system is solved by solving A X= B,
! with A matrix B vector, and X solution. 
! ----------------------------------------------------------------------

      implicit none

  
      
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) albg                 ! Albedo of the ground for the current urban class
      real(kind=kind_noahmp) albw                 ! Albedo of the wall for the current urban class
      real(kind=kind_noahmp) rsdif                ! diffused short wave radiation
      real(kind=kind_noahmp) fgw(nz_um,ndm,nurbm)       ! View factors from ground to wall
      real(kind=kind_noahmp) fwg(nz_um,ndm,nurbm)       ! View factors from wall to ground
      real(kind=kind_noahmp) fww(nz_um,nz_um,ndm,nurbm) ! View factors from wall to wall
      real(kind=kind_noahmp) fsg(ndm,nurbm)             ! View factors from sky to ground
      real(kind=kind_noahmp) fsw(nz_um,ndm,nurbm)       ! View factors from sky to wall
      integer id                ! current street direction 
      integer iurb              ! current urban class
      integer nz_u              ! Number of layer in the urban grid
      real(kind=kind_noahmp) pb(nz_um)          ! probability to have a building with an height equal

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) rsg(ndm)             ! Short wave radiation at the ground
      real(kind=kind_noahmp) rsw(2*ndm,nz_um)     ! Short wave radiation at the walls

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer i,j
      real(kind=kind_noahmp) aaa(2*nz_um+1,2*nz_um+1)  ! terms of the matrix
      real(kind=kind_noahmp) bbb(2*nz_um+1)            ! terms of the vector

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

      
! west wall
       
         
        do i=1,nz_u
         do j=1,nz_u
            aaa(i,j)=0.
         enddo

         aaa(i,i)=1.

         do j=nz_u+1,2*nz_u
            aaa(i,j)=-albw*fww(j-nz_u,i,id,iurb)*pb(j-nz_u+1)
         enddo

         aaa(i,2*nz_u+1)=-albg*fgw(i,id,iurb)
         bbb(i)=rsw(2*id-1,i)+fsw(i,id,iurb)*rsdif

      enddo
 
! east wall
       do i=1+nz_u,2*nz_u
         do j=1,nz_u
            aaa(i,j)=-albw*fww(j,i-nz_u,id,iurb)*pb(j+1)
         enddo

         do j=1+nz_u,2*nz_u
            aaa(i,j)=0.
         enddo

        aaa(i,i)=1.
        aaa(i,2*nz_u+1)=-albg*fgw(i-nz_u,id,iurb)
        bbb(i)=rsw(2*id,i-nz_u)+fsw(i-nz_u,id,iurb)*rsdif

      enddo


! ground
      do j=1,nz_u
         aaa(2*nz_u+1,j)=-albw*fwg(j,id,iurb)*pb(j+1)
      enddo

      do j=nz_u+1,2*nz_u
         aaa(2*nz_u+1,j)=-albw*fwg(j-nz_u,id,iurb)*pb(j-nz_u+1)
      enddo

      aaa(2*nz_u+1,2*nz_u+1)=1.
      bbb(2*nz_u+1)=rsg(id)+fsg(id,iurb)*rsdif

      call gaussj(aaa,2*nz_u+1,bbb,2*nz_um+1)

      do i=1,nz_u
         rsw(2*id-1,i)=bbb(i)
      enddo

      do i=nz_u+1,2*nz_u
         rsw(2*id,i-nz_u)=bbb(i)
      enddo

      rsg(id)=bbb(2*nz_u+1)

       
      return
      end subroutine short_rad_dd



! ===6=8===============================================================72     
! ===6=8===============================================================72     
      
      subroutine gaussj(a,n,b,np)

! ----------------------------------------------------------------------
! This routine solve a linear system of n equations of the form
!              A X = B
!  where  A is a matrix a(i,j)
!         B a vector and X the solution
! In output b is replaced by the solution     
! ----------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer np
      real(kind=kind_noahmp) a(np,np)

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) b(np)

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer nmax
      parameter (nmax=150)

      real(kind=kind_noahmp) big,dum
      integer i,icol,irow
      integer j,k,l,ll,n
      integer ipiv(nmax)
      real(kind=kind_noahmp) pivinv

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
       
      do j=1,n
         ipiv(j)=0.
      enddo
       
      do i=1,n
         big=0.
         do j=1,n
            if(ipiv(j).ne.1)then
               do k=1,n
                  if(ipiv(k).eq.0)then
                     if(abs(a(j,k)).ge.big)then
                        big=abs(a(j,k))
                        irow=j
                        icol=k
                     endif
                  elseif(ipiv(k).gt.1)then
                     CALL wrf_error_fatal('singular matrix in gaussj')
                  endif
               enddo
            endif
         enddo
         
         ipiv(icol)=ipiv(icol)+1
         
         if(irow.ne.icol)then
            do l=1,n
               dum=a(irow,l)
               a(irow,l)=a(icol,l)
               a(icol,l)=dum
            enddo
            
            dum=b(irow)
            b(irow)=b(icol)
            b(icol)=dum
          
         endif
         
         if(a(icol,icol).eq.0) CALL wrf_error_fatal('singular matrix in gaussj')
         
         pivinv=1./a(icol,icol)
         a(icol,icol)=1
         
         do l=1,n
            a(icol,l)=a(icol,l)*pivinv
         enddo
         
         b(icol)=b(icol)*pivinv
         
         do ll=1,n
            if(ll.ne.icol)then
               dum=a(ll,icol)
               a(ll,icol)=0.
               do l=1,n
                  a(ll,l)=a(ll,l)-a(icol,l)*dum
               enddo
               
               b(ll)=b(ll)-b(icol)*dum
               
            endif
         enddo
      enddo
      
      return
      end subroutine gaussj



! ===6=8===============================================================72     
! ===6=8===============================================================72     

      subroutine soil_moist(nz,dz,qv,dt,lf,d,k,rainbl,drain,irri_now)

! ----------------------------------------------------------------------
! This routine solves the Fourier diffusion equation for heat in 
! the material (wall, roof, or ground). Resolution is done implicitely.
! Boundary conditions are: 
! - fixed temperature at the interior
! - energy budget at the surface
! ----------------------------------------------------------------------

      implicit none



! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nz                ! Number of layers
      real(kind=kind_noahmp) dt                   ! Time step
      real(kind=kind_noahmp) lf                   ! Latent heat flux at the surface
      real(kind=kind_noahmp) qv(nz)               ! Moisture in each layer [K]
      real(kind=kind_noahmp) dz(nz)               ! Layer sizes [m]
      real(kind=kind_noahmp) rainbl               ! Rainfall [mm]
      real(kind=kind_noahmp) d(nz)                ! Soil water diffusivity
      real(kind=kind_noahmp) k(nz)                ! Hydraulic conductivity
      real(kind=kind_noahmp) gr                   ! Dummy variable 
      real(kind=kind_noahmp) drain
      real(kind=kind_noahmp) irri_now
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
  

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer iz
      real(kind=kind_noahmp) a(nz,3)
      real(kind=kind_noahmp) alpha
      real(kind=kind_noahmp) c(nz)
      real(kind=kind_noahmp) cddz(nz+2)
      real(kind=kind_noahmp) dw     !water density Kg/m3
      parameter(dw=1000.)
!----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

      alpha=rainbl/(dw*dt)+lf/latent/dw+irri_now/dw
      cddz(1)=0.
      do iz=2,nz
         cddz(iz)=2.*d(iz)/(dz(iz)+dz(iz-1))
      enddo
      do iz=1,4
         a(iz,1)=0.
         a(iz,2)=1.
         a(iz,3)=0.
         c(iz)=qv(iz)
      enddo
      do iz=6,nz-1
         a(iz,1)=-cddz(iz)*dt/dz(iz)
         a(iz,2)=1.+dt*(cddz(iz)+cddz(iz+1))/dz(iz)
         a(iz,3)=-cddz(iz+1)*dt/dz(iz)
         c(iz)=qv(iz)+dt*(k(iz+1)-k(iz))/dz(iz)
      enddo
         a(5,1)=0.
         a(5,2)=1.+dt*(cddz(5+1))/dz(5)
         a(5,3)=-cddz(5+1)*dt/dz(5)
         c(5)=qv(5)+dt*(k(5+1)-drain)/dz(5)
      

      a(nz,1)=-dt*cddz(nz)/dz(nz)
      a(nz,2)=1.+dt*cddz(nz)/dz(nz)
      a(nz,3)=0.
      c(nz)=qv(nz)+dt*alpha/dz(nz)-dt*k(nz-1)/dz(nz)

      call invert(nz,a,c,qv)

      return
      end subroutine soil_moist
         
! ===6=8===============================================================72     
! ===6=8===============================================================72     
  

! ===6=8===============================================================72     
! ===6=8===============================================================72     
       
      subroutine soil_temp_veg(heflro,nz,dz,temp,pt,ala,cs,                       &
                          rs,rl,press,dt,em,alb,rt,sf,lf,gf,pv_frac_roof,tpv)

! ----------------------------------------------------------------------
! This routine solves the Fourier diffusion equation for heat in 
! the material (wall, roof, or ground). Resolution is done implicitely.
! Boundary conditions are: 
! - fixed temperature at the interior
! - energy budget at the surface
! ----------------------------------------------------------------------

      implicit none

     
                
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nz                ! Number of layers
      real(kind=kind_noahmp) ala(nz)              ! Thermal diffusivity in each layers [m^2 s^-1] 
      real(kind=kind_noahmp) alb                  ! Albedo of the surface
      real(kind=kind_noahmp) cs(nz)               ! Specific heat of the material [J m^3 K^-1]
      real(kind=kind_noahmp) dt                   ! Time step
      real(kind=kind_noahmp) em                   ! Emissivity of the surface
      real(kind=kind_noahmp) press                ! Pressure at ground level
      real(kind=kind_noahmp) rl                   ! Downward flux of the longwave radiation
      real(kind=kind_noahmp) rs                   ! Solar radiation
      real(kind=kind_noahmp) sf                   ! Sensible heat flux at the surface
      real(kind=kind_noahmp) lf                   ! Latent heat flux at the surface
      real(kind=kind_noahmp) temp(nz)             ! Temperature in each layer [K]
      real(kind=kind_noahmp) dz(nz)               ! Layer sizes [m]
      real(kind=kind_noahmp) heflro                ! Heat flux between roof and green roof 
      real(kind=kind_noahmp) rs_eff
      real(kind=kind_noahmp) rl_eff
      real(kind=kind_noahmp) tpv
      real(kind=kind_noahmp) pv_frac_roof
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) gf                   ! Heat flux transferred from the surface toward the interior
      real(kind=kind_noahmp) pt                   ! Potential temperature at the surface
      real(kind=kind_noahmp) rt                   ! Total radiation at the surface (solar+incoming long+outgoing long)

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer iz
      real(kind=kind_noahmp) a(nz,3)
      real(kind=kind_noahmp) alpha
      real(kind=kind_noahmp) c(nz)
      real(kind=kind_noahmp) cddz(nz+2)
      real(kind=kind_noahmp) tsig

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
     if(pv_frac_roof.gt.0)then 
     rl_eff=(1-pv_frac_roof)*em*rl+em*sigma*tpv**4*pv_frac_roof
      rs_eff=(1.-pv_frac_roof)*rs
     else
      rl_eff=em*rl
      rs_eff=rs
     endif
      tsig=temp(nz)
      alpha=(1.-alb)*rs_eff+rl_eff-em*sigma*(tsig**4)+sf+lf
      cddz(1)=ala(1)/dz(1)
      do iz=2,nz
         cddz(iz)=2.*ala(iz)/(dz(iz)+dz(iz-1))
      enddo
      
      a(1,1)=0.
      a(1,2)=1.
      a(1,3)=0.
      c(1)=temp(1)-heflro*dt/dz(1)
      do iz=2,nz-1
         a(iz,1)=-cddz(iz)*dt/dz(iz)
         a(iz,2)=1.+dt*(cddz(iz)+cddz(iz+1))/dz(iz)          
         a(iz,3)=-cddz(iz+1)*dt/dz(iz)
         c(iz)=temp(iz)
      enddo          
      a(nz,1)=-dt*cddz(nz)/dz(nz)
      a(nz,2)=1.+dt*cddz(nz)/dz(nz)
      a(nz,3)=0.
      c(nz)=temp(nz)+dt*alpha/cs(nz)/dz(nz) 
      
      call invert(nz,a,c,temp)

      pt=temp(nz)*(press/1.e+5)**(-rcp_u)

      rt=(1.-alb)*rs_eff+rl_eff-em*sigma*(tsig**4.)
                        
      gf=(1.-alb)*rs_eff+rl_eff-em*sigma*(tsig**4.)+sf                                   
      return
      end subroutine soil_temp_veg
      
! ===6=8===============================================================72     
! ===6=8===============================================================72     
       
      subroutine soil_temp(nz,dz,temp,pt,ala,cs,                       &
                          rs,rl,press,dt,em,alb,rt,sf,lf,gf)

! ----------------------------------------------------------------------
! This routine solves the Fourier diffusion equation for heat in 
! the material (wall, roof, or ground). Resolution is done implicitely.
! Boundary conditions are: 
! - fixed temperature at the interior
! - energy budget at the surface
! ----------------------------------------------------------------------

      implicit none

     
                
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nz                ! Number of layers
      real(kind=kind_noahmp) ala(nz)              ! Thermal diffusivity in each layers [m^2 s^-1] 
      real(kind=kind_noahmp) alb                  ! Albedo of the surface
      real(kind=kind_noahmp) cs(nz)               ! Specific heat of the material [J m^3 K^-1]
      real(kind=kind_noahmp) dt                   ! Time step
      real(kind=kind_noahmp) em                   ! Emissivity of the surface
      real(kind=kind_noahmp) press                ! Pressure at ground level
      real(kind=kind_noahmp) rl                   ! Downward flux of the longwave radiation
      real(kind=kind_noahmp) rs                   ! Solar radiation
      real(kind=kind_noahmp) sf                   ! Sensible heat flux at the surface
      real(kind=kind_noahmp) lf                   ! Latent heat flux at the surface
      real(kind=kind_noahmp) temp(nz)             ! Temperature in each layer [K]
      real(kind=kind_noahmp) dz(nz)               ! Layer sizes [m]

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) gf                   ! Heat flux transferred from the surface toward the interior
      real(kind=kind_noahmp) pt                   ! Potential temperature at the surface
      real(kind=kind_noahmp) rt                   ! Total radiation at the surface (solar+incoming long+outgoing long)

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer iz
      real(kind=kind_noahmp) a(nz,3)
      real(kind=kind_noahmp) alpha
      real(kind=kind_noahmp) c(nz)
      real(kind=kind_noahmp) cddz(nz+2)
      real(kind=kind_noahmp) tsig

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
       
      tsig=temp(nz)
      alpha=(1.-alb)*rs+em*rl-em*sigma*(tsig**4)+sf+lf
! Compute cddz=2*cd/dz  
      cddz(1)=ala(1)/dz(1)
      do iz=2,nz
         cddz(iz)=2.*ala(iz)/(dz(iz)+dz(iz-1))
      enddo
      
      a(1,1)=0.
      a(1,2)=1.
      a(1,3)=0.
      c(1)=temp(1)
      do iz=2,nz-1
         a(iz,1)=-cddz(iz)*dt/dz(iz)
         a(iz,2)=1.+dt*(cddz(iz)+cddz(iz+1))/dz(iz)          
         a(iz,3)=-cddz(iz+1)*dt/dz(iz)
         c(iz)=temp(iz)
      enddo          
      a(nz,1)=-dt*cddz(nz)/dz(nz)
      a(nz,2)=1.+dt*cddz(nz)/dz(nz)
      a(nz,3)=0.
      c(nz)=temp(nz)+dt*alpha/cs(nz)/dz(nz) 

      
      call invert(nz,a,c,temp)

      pt=temp(nz)*(press/1.e+5)**(-rcp_u)

      rt=(1.-alb)*rs+em*rl-em*sigma*(tsig**4.)
                        
       gf=(1.-alb)*rs+em*rl-em*sigma*(tsig**4.)+sf                                   
      return
      end subroutine soil_temp
      

! ===6=8===============================================================72 
! ===6=8===============================================================72 


      subroutine invert(n,a,c,x)

! ----------------------------------------------------------------------
!        Inversion and resolution of a tridiagonal matrix
!                   A X = C
! ----------------------------------------------------------------------

      implicit none
                
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
       integer n
       real(kind=kind_noahmp) a(n,3)              !  a(*,1) lower diagonal (Ai,i-1)
                                !  a(*,2) principal diagonal (Ai,i)
                                !  a(*,3) upper diagonal (Ai,i+1)
       real(kind=kind_noahmp) c(n)

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
       real(kind=kind_noahmp) x(n)    

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
       integer i

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
                     
       do i=n-1,1,-1                 
          c(i)=c(i)-a(i,3)*c(i+1)/a(i+1,2)
          a(i,2)=a(i,2)-a(i,3)*a(i+1,1)/a(i+1,2)
       enddo
       
       do i=2,n        
          c(i)=c(i)-a(i,1)*c(i-1)/a(i-1,2)
       enddo
        
       do i=1,n
          x(i)=c(i)/a(i,2)
       enddo

       return
       end subroutine invert
  

! ===6=8===============================================================72  
! ===6=8===============================================================72
  
      subroutine flux_wall(ua,va,pt,da,ptw,ptwin,uva,vva,uvb,vvb,  &
                           sfw,sfwin,evb,drst,dt,cdrag)         
       
! ----------------------------------------------------------------------
! This routine computes the surface sources or sinks of momentum, tke,
! and heat from vertical surfaces (walls).   
! ----------------------------------------------------------------------
      implicit none   
         
! INPUT:
! -----
      real(kind=kind_noahmp) drst                 ! street directions for the current urban class
      real(kind=kind_noahmp) da                   ! air density
      real(kind=kind_noahmp) pt                   ! potential temperature
      real(kind=kind_noahmp) ptw                  ! Walls potential temperatures 
      real(kind=kind_noahmp) ptwin                ! Windows potential temperatures
      real(kind=kind_noahmp) ua                   ! wind speed
      real(kind=kind_noahmp) va                   ! wind speed
      real(kind=kind_noahmp) dt                   !time step
      real(kind=kind_noahmp) cdrag
! OUTPUT:
! ------
! Explicit and implicit component of the momentum, temperature and TKE sources or sinks on
! vertical surfaces (walls).
! The fluxes can be computed as follow: Fluxes of X = A*X + B
! Example: Momentum fluxes on vertical surfaces = uva_u * ua_u + uvb_u

      real(kind=kind_noahmp) uva                  ! U (wind component)   Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) uvb                  ! U (wind component)   Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) vva                  ! V (wind component)   Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) vvb                  ! V (wind component)   Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) tva                  ! Temperature          Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) tvb                  ! Temperature          Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) evb                  ! Energy (TKE)         Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) sfw                  ! Surfaces fluxes from the walls
      real(kind=kind_noahmp) sfwin                ! Surfaces fluxes from the windows

! LOCAL:
! -----
      real(kind=kind_noahmp) hc
      real(kind=kind_noahmp) hcwin
      real(kind=kind_noahmp) u_ort
      real(kind=kind_noahmp) vett


! -------------------------
! END VARIABLES DEFINITIONS
! -------------------------

      vett=(ua**2+va**2)**.5         
         
      u_ort=abs((cos(drst)*ua-sin(drst)*va))
       
      uva=-cdrag*u_ort/2.*cos(drst)*cos(drst)
      vva=-cdrag*u_ort/2.*sin(drst)*sin(drst)
         
      uvb=cdrag*u_ort/2.*sin(drst)*cos(drst)*va
      vvb=cdrag*u_ort/2.*sin(drst)*cos(drst)*ua         

      if (vett.lt.4.88) then   
         hc=5.678*(1.09+0.23*(vett/0.3048))  
      else
         hc=5.678*0.53*((vett/0.3048)**0.78)
      endif 

      if (hc.gt.da*cp_u/dt)then
         hc=da*cp_u/dt
      endif

       if (vett.lt.4.88) then
          hcwin=5.678*(0.99+0.21*(vett/0.3048))
       else
          hcwin=5.678*0.50*((vett/0.3048)**0.78)
       endif

       if (hcwin.gt.da*cp_u/dt) then
           hcwin=da*cp_u/dt
       endif
         
!         tvb=hc*ptw/da/cp_u
!         tva=-hc/da/cp_u
!!!!!!!!!!!!!!!!!!!!
! explicit 

      sfw=hc*(pt-ptw)
      sfwin=hcwin*(pt-ptwin)  
       
         
      evb=cdrag*(abs(u_ort)**3.)/2.
              
      return
      end subroutine flux_wall
         
! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine flux_flat_ground(dz,z0,ua,va,pt,pt0,ptg,                     &
                          uhb,vhb,sf,ehb,da,qv,pr,rsg,qg,resg,rsveg,f1,f2,f3,f4,fh,ric,utot,gr_type)
                                
! ----------------------------------------------------------------------
!           Calculation of the flux at the ground 
!           Formulation of Louis (Louis, 1979)       
! ----------------------------------------------------------------------

      implicit none

      real(kind=kind_noahmp) dz                   ! first vertical level
      real(kind=kind_noahmp) pt                   ! potential temperature
      real(kind=kind_noahmp) pt0                  ! reference potential temperature
      real(kind=kind_noahmp) ptg                  ! ground potential temperature
      real(kind=kind_noahmp) ua                   ! wind speed
      real(kind=kind_noahmp) va                   ! wind speed
      real(kind=kind_noahmp) z0                   ! Roughness length
      real(kind=kind_noahmp) da                   ! air density
      real(kind=kind_noahmp) qv                   ! specific humidity
      real(kind=kind_noahmp) pr                   ! pressure
      real(kind=kind_noahmp) rsg                  ! solar radiation
      real(kind=kind_noahmp) qg(ng_u)         ! Ground Soil Moisture

     

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
! Explicit component of the momentum, temperature and TKE sources or sinks on horizontal 
!  surfaces (roofs and street)
! The fluxes can be computed as follow: Fluxes of X = B
!  Example: Momentum fluxes on horizontal surfaces =  uhb_u
      real(kind=kind_noahmp) uhb                  ! U (wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) vhb                  ! V (wind component) Horizontal surfaces, B (explicit) term
!     real thb                  ! Temperature        Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) tva                  ! Temperature          Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) tvb                  ! Temperature          Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) ehb                  ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) sf
      real(kind=kind_noahmp) lf

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) aa,ah
      real(kind=kind_noahmp) z0t
      real(kind=kind_noahmp) al
      real(kind=kind_noahmp) buu
      real(kind=kind_noahmp) c
      real(kind=kind_noahmp) fbuw
      real(kind=kind_noahmp) fbpt
      real(kind=kind_noahmp) fh
      real(kind=kind_noahmp) fm
      real(kind=kind_noahmp) ric
      real(kind=kind_noahmp) tstar
      real(kind=kind_noahmp) qstar
      real(kind=kind_noahmp) ustar
      real(kind=kind_noahmp) utot
      real(kind=kind_noahmp) wstar
      real(kind=kind_noahmp) zz
      real(kind=kind_noahmp) qvsg,qvs,es,esa,fbqq
      real(kind=kind_noahmp) b,cm,ch,rr,tol
      parameter(b=9.4,cm=7.4,ch=5.3,rr=0.74,tol=.001)

      real(kind=kind_noahmp) f
      real(kind=kind_noahmp) f1
      real(kind=kind_noahmp) f2
      real(kind=kind_noahmp) f3
      real(kind=kind_noahmp) f4
      real(kind=kind_noahmp) ta          ! surface air temperature
      real(kind=kind_noahmp) tmp                  ! ground temperature
      real(kind=kind_noahmp) rsveg       ! Stomatal resistance 
      real(kind=kind_noahmp) resg
      real(kind=kind_noahmp) lai         ! leaf area index
      real(kind=kind_noahmp) sdlim       ! radiation limit at which photosyntesis start W/m2
      parameter(sdlim=100.)
      real(kind=kind_noahmp) rsmin ! Minimum stomatal resistance 
      real(kind=kind_noahmp) rsmax ! Maximun stomatal resistance 
      real(kind=kind_noahmp) qw
      parameter(qw=0.06)
      real(kind=kind_noahmp) qref
      parameter(qref=0.37)  
      real(kind=kind_noahmp) hs
      parameter(hs=36.35)
 
      real(kind=kind_noahmp) dzg_u(ng_u)          ! Layer sizes in the ground

      data dzg_u /0.2,0.12,0.08,0.05,0.03,0.02,0.02,0.01,0.005,0.0025/
      
      real(kind=kind_noahmp) gx,dzg_tot
      integer gr_type,iz
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
      z0t=z0/10.
      if(gr_type.eq.1)then
      rsmin=40.
      rsmax=5000.
      lai=2.
      elseif(gr_type.eq.2)then
      rsmin=150.
      rsmax=5000.
      lai=3.
      endif
! computation of the ground temperature
         
      utot=(ua**2.+va**2.)**.5
        
      
!!!! Louis formulation
!
! compute the bulk Richardson Number

      zz=dz/2.
   
!        if(tstar.lt.0.)then
!         wstar=(-ustar*tstar*g*hii/pt)**(1./3.)
!        else
!         wstar=0.
!        endif
!        
!      if (utot.le.0.7*wstar) utot=max(0.7*wstar,0.00001)

      utot=max(utot,0.01)
          
      ric=2.*g_u*zz*(pt-ptg)/((pt+ptg)*(utot**2))
              
      aa=vk/log(zz/z0)
      ah=vk/log(zz/z0t)

! determine the parameters fm and fh for stable, neutral and unstable conditions

      if(ric.gt.0)then
         fm=1/(1+0.5*b*ric)**2.
         fh=fm
      else
         c=b*cm*aa*aa*(zz/z0)**.5
         fm=1-b*ric/(1+c*(-ric)**.5)
         c=b*cm*aa*ah*(zz/z0t)**.5
         c=c*ch/cm
         fh=1-b*ric/(1+c*(-ric)**.5)
      endif
      
      fbuw=-aa*aa*utot*utot*fm
      fbpt=-aa*ah*utot*(pt-ptg)*fh/rr
      tmp=ptg*(pr/p0)**(rcp_u)-273.15 
      es=6.11*(10.**(tmp*7.5/(237.7+tmp)))
      qvsg=0.62197*es/(0.01*pr-0.378*es)
      

      f=0.55*rsg/sdlim*2./lai
      
      f1=(f+rsmin/rsmax)/(1.+f)

      ta=pt*(pr/p0)**(rcp_u)-273.15
      esa=6.11*(10**(ta*7.5/(237.7+ta)))
      qvs=0.62197*esa/(0.01*pr-0.378*esa)

      f2= 1./(1.+hs*(qvs-qv))
      f3=1.-0.0016*(25.-ta)**2.
      f4=0.
      dzg_tot=0.
      do iz=1,ng_u
       gx=(qg(iz)-qw)/(qref-qw)
       if (gx.gt.1)gx=1.
       if (gx.lt.0)gx=0.
       f4=f4+gx*dzg_u(iz)
       dzg_tot=dzg_tot+dzg_u(iz)
      enddo
      f4=f4/dzg_tot

      rsveg=min(rsmin/max(lai*f1*f2*f3*f4,1e-9),rsmax)
      resg= rr/(aa*aa*utot*fh)


      fbqq=-(qv-qvsg)/(resg+rsveg)
      
               
      ustar=(-fbuw)**.5
      tstar=-fbpt/ustar
      qstar=-fbqq/ustar

      al=(vk*g_u*tstar)/(pt*ustar*ustar)                      
      
      buu=-g_u/pt0*ustar*tstar
       
      uhb=-ustar*ustar*ua/utot
      vhb=-ustar*ustar*va/utot 
      sf= ustar*tstar*da*cp_u   
      lf= ustar*qstar*da*latent
       
!     thb= 0.      
      ehb=buu
!!!!!!!!!!!!!!!
         
      return
      end subroutine flux_flat_ground

! ===6=8===============================================================72
! ===6=8===============================================================72
      subroutine flux_flat_roof(dz,z0,ua,va,pt,pt0,ptg,                     &
                          uhb,vhb,sf,lf,ehb,da,qv,pr,rsg,qr,resg,rsveg,f1,f2,f3,f4,gr_type,pv_frac_roof)

! ----------------------------------------------------------------------
!           Calculation of the flux at the ground 
!           Formulation of Louis (Louis, 1979)       
! ----------------------------------------------------------------------

      implicit none

      real(kind=kind_noahmp) dz                   ! first vertical level
      real(kind=kind_noahmp) pt                   ! potential temperature
      real(kind=kind_noahmp) pt0                  ! reference potential temperature
      real(kind=kind_noahmp) ptg                  ! ground potential temperature
      real(kind=kind_noahmp) ua                   ! wind speed
      real(kind=kind_noahmp) va                   ! wind speed
      real(kind=kind_noahmp) z0                   ! Roughness length
      real(kind=kind_noahmp) da                   ! air density
      real(kind=kind_noahmp) qv                   ! specific humidity
      real(kind=kind_noahmp) pr                   ! pressure
      real(kind=kind_noahmp) rsg                  ! solar radiation
      real(kind=kind_noahmp) qr(ngr_u)         ! Ground Soil Moisture
      real(kind=kind_noahmp) pv_frac_roof
      real(kind=kind_noahmp) rs_eff

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
! Explicit component of the momentum, temperature and TKE sources or sinks on horizontal 
!  surfaces (roofs and street)
! The fluxes can be computed as follow: Fluxes of X = B
!  Example: Momentum fluxes on horizontal surfaces =  uhb_u
      real(kind=kind_noahmp) uhb                  ! U (wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) vhb                  ! V (wind component) Horizontal surfaces, B (explicit) term
!     real thb                  ! Temperature        Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) tva                  ! Temperature          Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) tvb                  ! Temperature          Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) ehb                  ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) sf
      real(kind=kind_noahmp) lf

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) aa,ah
      real(kind=kind_noahmp) al
      real(kind=kind_noahmp) buu
      real(kind=kind_noahmp) c
      real(kind=kind_noahmp) fbuw
      real(kind=kind_noahmp) fbpt
      real(kind=kind_noahmp) fh
      real(kind=kind_noahmp) fm
      real(kind=kind_noahmp) ric
      real(kind=kind_noahmp) tstar
      real(kind=kind_noahmp) qstar
      real(kind=kind_noahmp) ustar
      real(kind=kind_noahmp) utot
      real(kind=kind_noahmp) wstar
      real(kind=kind_noahmp) zz
      real(kind=kind_noahmp) z0t
      real(kind=kind_noahmp) qvsg,qvs,es,esa,fbqq
      real(kind=kind_noahmp) b,cm,ch,rr,tol
      parameter(b=9.4,cm=7.4,ch=5.3,rr=0.74,tol=.001)

      real(kind=kind_noahmp) f
      real(kind=kind_noahmp) f1
      real(kind=kind_noahmp) f2
      real(kind=kind_noahmp) f3
      real(kind=kind_noahmp) f4
      real(kind=kind_noahmp) ta          ! surface air temperature
      real(kind=kind_noahmp) tmp                  ! ground temperature
      real(kind=kind_noahmp) rsveg       ! Stomatal resistance 
      real(kind=kind_noahmp) resg
      real(kind=kind_noahmp) lai         ! leaft area index
      real(kind=kind_noahmp) sdlim       ! radiation limit at which photosyntesis start W/m2
      parameter(sdlim=100.)
      real(kind=kind_noahmp) rsmin
      real(kind=kind_noahmp) rsmax ! Maximun stomatal resistance 
      real(kind=kind_noahmp) qw    ! Wilting point
      parameter(qw=0.06) 
      real(kind=kind_noahmp) qref  ! Field capacity
      parameter(qref=0.37)
      real(kind=kind_noahmp) hs
      parameter(hs=36.35)

      real(kind=kind_noahmp) dzgr_u(ngr_u)          ! Layer sizes in the ground

      data dzgr_u /0.1,0.003,0.06,0.003,0.05,0.04,0.02,0.0125,0.005,0.0025/

      real(kind=kind_noahmp) gx,dzgr_tot
      integer gr_type,iz
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS

! ----------------------------------------------------------------------
      z0t=z0/10.
      if(gr_type.eq.1)then
      rsmin=40.
      rsmax=5000.
      lai=2.
      elseif(gr_type.eq.2)then
      rsmin=150.
      rsmax=5000.
      lai=3.
      endif
     rs_eff=(1-pv_frac_roof)*rsg
! computation of the ground temperature

      utot=(ua**2.+va**2.)**.5

!!!! Louis formulation
!
! compute the bulk Richardson Number

      zz=dz/2.


      utot=max(utot,0.01)

      ric=2.*g_u*zz*(pt-ptg)/((pt+ptg)*(utot**2))

      aa=vk/log(zz/z0)
      ah=vk/log(zz/z0t)

      if(ric.gt.0.)then
         fm=1./(1.+0.5*b*ric)**2.
         fh=fm
      else
         c=b*cm*aa*aa*(zz/z0)**.5
         fm=1.-b*ric/(1.+c*(-ric)**.5)
         c=b*cm*aa*ah*(zz/z0t)**.5
         c=c*ch/cm
         fh=1.-b*ric/(1+c*(-ric)**.5)
      endif

      fbuw=-aa*aa*utot*utot*fm
      fbpt=-aa*ah*utot*(pt-ptg)*fh/rr
      tmp=ptg*(pr/p0)**(rcp_u)-273.15
      es=6.11*(10.**(tmp*7.5/(237.7+tmp)))
      qvsg=0.62197*es/(0.01*pr-0.378*es)


      f=0.55*rs_eff/sdlim*2./lai

      f1=(f+rsmin/rsmax)/(1.+f)

      ta=pt*(pr/p0)**(rcp_u)-273.15
      esa=6.11*(10**(ta*7.5/(237.7+ta)))
      qvs=0.62197*esa/(0.01*pr-0.378*esa)

      f2= 1./(1.+hs*(qvs-qv))
      f3=1.-0.0016*(25.-ta)**2.
      f4=0.
      dzgr_tot=0.
      do iz=5,ngr_u
       gx=(qr(iz)-qw)/(qref-qw)
       if (gx.gt.1)gx=1.
       if (gx.lt.0)gx=0.
       f4=f4+gx*dzgr_u(iz)
       dzgr_tot=dzgr_tot+dzgr_u(iz)
      enddo
      f4=f4/dzgr_tot

      rsveg=min(rsmin/max(lai*f1*f2*f3*f4,1e-9),rsmax)


      resg= rr/(aa*aa*utot*fh)


      fbqq=-(qv-qvsg)/(resg+rsveg)

      ustar=(-fbuw)**.5
      tstar=-fbpt/ustar
      qstar=-fbqq/ustar

      al=(vk*g_u*tstar)/(pt*ustar*ustar)

      buu=-g_u/pt0*ustar*tstar

      uhb=-ustar*ustar*ua/utot
      vhb=-ustar*ustar*va/utot
      sf= ustar*tstar*da*cp_u
      lf= ustar*qstar*da*latent

      ehb=buu
      end subroutine flux_flat_roof
      
!!!!!!!===============================

! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine flux_flat(dz,z0,ua,va,pt,pt0,ptg,qv,                   &
                          uhb,vhb,sf,lf,ehb,da,pr)
                                
! ----------------------------------------------------------------------
!           Calculation of the flux at the ground 
!           Formulation of Louis (Louis, 1979)       
! ----------------------------------------------------------------------

      implicit none
      real(kind=kind_noahmp) pr
      real(kind=kind_noahmp) dz                   ! first vertical level
      real(kind=kind_noahmp) pt                   ! potential temperature
      real(kind=kind_noahmp) pt0                  ! reference potential temperature
      real(kind=kind_noahmp) ptg                  ! ground potential temperature
      real(kind=kind_noahmp) ua                   ! wind speed
      real(kind=kind_noahmp) va                   ! wind speed
      real(kind=kind_noahmp) z0                   ! Roughness length
      real(kind=kind_noahmp) da                   ! air density
      real(kind=kind_noahmp) qv
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
! Explicit component of the momentum, temperature and TKE sources or sinks on horizontal 
!  surfaces (roofs and street)
! The fluxes can be computed as follow: Fluxes of X = B
!  Example: Momentum fluxes on horizontal surfaces =  uhb_u
      real(kind=kind_noahmp) uhb                  ! U (wind component) Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) vhb                  ! V (wind component) Horizontal surfaces, B (explicit) term
!     real thb                  ! Temperature        Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) tva                  ! Temperature          Vertical surfaces, A (implicit) term
      real(kind=kind_noahmp) tvb                  ! Temperature          Vertical surfaces, B (explicit) term
      real(kind=kind_noahmp) ehb                  ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real(kind=kind_noahmp) sf
      real(kind=kind_noahmp) lf
       
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real(kind=kind_noahmp) aa
      real(kind=kind_noahmp) al
      real(kind=kind_noahmp) buu
      real(kind=kind_noahmp) c
      real(kind=kind_noahmp) fbuw
      real(kind=kind_noahmp) fbpt
      real(kind=kind_noahmp) fh
      real(kind=kind_noahmp) fm
      real(kind=kind_noahmp) ric
      real(kind=kind_noahmp) tstar
      real(kind=kind_noahmp) ustar
      real(kind=kind_noahmp) qstar
      real(kind=kind_noahmp) utot
      real(kind=kind_noahmp) wstar
      real(kind=kind_noahmp) zz
      real(kind=kind_noahmp) qvsg,qvs,es,esa,fbqq,tmp,resg
      real(kind=kind_noahmp) b,cm,ch,rr,tol
      parameter(b=9.4,cm=7.4,ch=5.3,rr=0.74,tol=.001)

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------


! computation of the ground temperature
         
      utot=(ua**2+va**2)**.5
        
      
!!!! Louis formulation
!
! compute the bulk Richardson Number

      zz=dz/2.
   

      utot=max(utot,0.01)
          
      ric=2.*g_u*zz*(pt-ptg)/((pt+ptg)*(utot**2))
              
      aa=vk/log(zz/z0)


     
      tmp=ptg*(pr/(1.e+5))**(rcp_u)-273.15 
      es=6.11*(10**(tmp*7.5/(237.7+tmp)))
      qvsg=0.62197*es/(0.01*pr-0.378*es)



! determine the parameters fm and fh for stable, neutral and unstable conditions

      if(ric.gt.0.)then
         fm=1./(1.+0.5*b*ric)**2
         fh=fm
      else
         c=b*cm*aa*aa*(zz/z0)**.5
         fm=1.-b*ric/(1.+c*(-ric)**.5)
         c=c*ch/cm
         fh=1.-b*ric/(1.+c*(-ric)**.5)
      endif

      resg= rr/(aa*aa*utot*fh)
      fbuw=-aa*aa*utot*utot*fm
      fbpt=-aa*aa*utot*(pt-ptg)*fh/rr
      fbqq=-(qv-qvsg)/(resg)
               
      ustar=(-fbuw)**.5
      tstar=-fbpt/ustar
      qstar=-fbqq/ustar
      al=(vk*g_u*tstar)/(pt*ustar*ustar)                      
      
      buu=-g_u/pt0*ustar*tstar
       
      uhb=-ustar*ustar*ua/utot
      vhb=-ustar*ustar*va/utot 
      sf= ustar*tstar*da*cp_u  
      lf= ustar*qstar*da*latent 
      ehb=buu
!!!!!!!!!!!!!!!
         
      return
      end subroutine flux_flat
!!!!!!!!!!!!!================!!!!!!!!!!!!!!!!!!!      
! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine icBEP (nd_u,h_b,d_b,ss_u,pb_u,nz_u,z_u)

      implicit none

!    Street parameters
      integer nd_u(nurbm)     ! Number of street direction for each urban class
      real(kind=kind_noahmp) h_b(nz_um,nurbm)   ! Bulding's heights [m]
      real(kind=kind_noahmp) d_b(nz_um,nurbm)   ! The probability that a building has an height h_b
! -----------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------

      real(kind=kind_noahmp) ss_u(nz_um,nurbm)     ! The probability that a building has an height equal to z
      real(kind=kind_noahmp) pb_u(nz_um,nurbm)     ! The probability that a building has an height greater or equal to z

!    Grid parameters
      integer nz_u(nurbm)     ! Number of layer in the urban grid
      real(kind=kind_noahmp) z_u(nz_um)       ! Height of the urban grid levels


! -----------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------

      integer iz_u,id,ilu,iurb

      real(kind=kind_noahmp) dtot
      real(kind=kind_noahmp) hbmax

! -----------------------------------------------------------------------
!     This routine initialise the urban paramters for the BEP module
!------------------------------------------------------------------------
!
!Initialize variables
!
 !
      nz_u=0
      z_u=0.
      ss_u=0.
      pb_u=0.

! Computation of the urban levels height

      z_u(1)=0.

      do iz_u=1,nz_um-1
         z_u(iz_u+1)=z_u(iz_u)+dz_u
      enddo

! Normalisation of the building density

      do iurb=1,nurbm
         dtot=0.
         do ilu=1,nz_um
            dtot=dtot+d_b(ilu,iurb)
         enddo
         do ilu=1,nz_um
            d_b(ilu,iurb)=d_b(ilu,iurb)/dtot
         enddo
      enddo

! Compute the view factors, pb and ss

      do iurb=1,nurbm
         hbmax=0.
         nz_u(iurb)=0
         do ilu=1,nz_um
            if(h_b(ilu,iurb).gt.hbmax)hbmax=h_b(ilu,iurb)
         enddo

         do iz_u=1,nz_um-1
            if(z_u(iz_u+1).gt.hbmax)go to 10
         enddo

 10      continue
          nz_u(iurb)=iz_u+1

         do id=1,nd_u(iurb)

            do iz_u=1,nz_u(iurb)
               ss_u(iz_u,iurb)=0.
               do ilu=1,nz_um
                  if(z_u(iz_u).le.h_b(ilu,iurb)                      &
                    .and.z_u(iz_u+1).gt.h_b(ilu,iurb))then
                        ss_u(iz_u,iurb)=ss_u(iz_u,iurb)+d_b(ilu,iurb)
                  endif
               enddo
            enddo

            pb_u(1,iurb)=1.
            do iz_u=1,nz_u(iurb)
               pb_u(iz_u+1,iurb)=max(0.,pb_u(iz_u,iurb)-ss_u(iz_u,iurb))
            enddo

         enddo
      end do


      return
      end subroutine icBEP

! ===6=8===============================================================72
! ===6=8===============================================================72
    

      subroutine view_factors(iurb,nz_u,id,dxy,z,ws,fww,fwg,fgw,fsg,fsw,fws) 
     
      implicit none

 

! -----------------------------------------------------------------------
!     Input
!------------------------------------------------------------------------

      integer iurb            ! Number of the urban class
      integer nz_u            ! Number of levels in the urban grid
      integer id              ! Street direction number
      real(kind=kind_noahmp) ws                 ! Street width
      real(kind=kind_noahmp) z(nz_um)         ! Height of the urban grid levels
      real(kind=kind_noahmp) dxy                ! Street lenght


! -----------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------

!   fww,fwg,fgw,fsw,fsg are the view factors used to compute the long wave
!   and the short wave radation. They are the part of radiation from a surface
!   or from the sky to another surface.

      real(kind=kind_noahmp) fww(nz_um,nz_um,ndm,nurbm)            !  from wall to wall
      real(kind=kind_noahmp) fwg(nz_um,ndm,nurbm)                  !  from wall to ground
      real(kind=kind_noahmp) fgw(nz_um,ndm,nurbm)                  !  from ground to wall
      real(kind=kind_noahmp) fsw(nz_um,ndm,nurbm)                  !  from sky to wall
      real(kind=kind_noahmp) fws(nz_um,ndm,nurbm)                  !  from wall to sky
      real(kind=kind_noahmp) fsg(ndm,nurbm)                        !  from sky to ground


! -----------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------

      integer jz,iz

      real(kind=kind_noahmp) hut
      real(kind=kind_noahmp) f1,f2,f12,f23,f123,ftot
      real(kind=kind_noahmp) fprl,fnrm
      real(kind=kind_noahmp) a1,a2,a3,a4,a12,a23,a123

! -----------------------------------------------------------------------
!     This routine calculates the view factors
!------------------------------------------------------------------------
        
      hut=z(nz_u+1)
        
      do jz=1,nz_u      
      
! radiation from wall to wall
       
         do iz=1,nz_u
     
            call fprls (fprl,dxy,abs(z(jz+1)-z(iz  )),ws)
            f123=fprl
            call fprls (fprl,dxy,abs(z(jz+1)-z(iz+1)),ws)
            f23=fprl
            call fprls (fprl,dxy,abs(z(jz  )-z(iz  )),ws)
            f12=fprl
            call fprls (fprl,dxy,abs(z(jz  )-z(iz+1)),ws)
            f2 = fprl
       
            a123=dxy*(abs(z(jz+1)-z(iz  )))
            a12 =dxy*(abs(z(jz  )-z(iz  )))
            a23 =dxy*(abs(z(jz+1)-z(iz+1)))
            a1  =dxy*(abs(z(iz+1)-z(iz  )))
            a2  =dxy*(abs(z(jz  )-z(iz+1)))
            a3  =dxy*(abs(z(jz+1)-z(jz  )))
       
            ftot=0.5*(a123*f123-a23*f23-a12*f12+a2*f2)/a1
       
            fww(iz,jz,id,iurb)=ftot*a1/a3

         enddo 

! radiation from ground to wall
       
         call fnrms (fnrm,z(jz+1),dxy,ws)
         f12=fnrm
         call fnrms (fnrm,z(jz)  ,dxy,ws)
         f1=fnrm
       
         a1 = ws*dxy
         
         a12= ws*dxy
       
         a4=(z(jz+1)-z(jz))*dxy
       
         ftot=(a12*f12-a12*f1)/a1
                    
         fgw(jz,id,iurb)=ftot*a1/a4
     
!  radiation from sky to wall
     
         call fnrms(fnrm,hut-z(jz)  ,dxy,ws)
         f12 = fnrm
         call fnrms (fnrm,hut-z(jz+1),dxy,ws)
         f1 =fnrm
       
         a1 = ws*dxy
       
         a12= ws*dxy
              
         a4 = (z(jz+1)-z(jz))*dxy
       
         ftot=(a12*f12-a12*f1)/a1
        
         fsw(jz,id,iurb)=ftot*a1/a4       
      
      enddo

! radiation from wall to sky      
       do iz=1,nz_u
       call fnrms(fnrm,ws,dxy,hut-z(iz))
       f12=fnrm
       call fnrms(fnrm,ws,dxy,hut-z(iz+1))
       f1=fnrm
       a1 = (z(iz+1)-z(iz))*dxy
       a2 = (hut-z(iz+1))*dxy
       a12= (hut-z(iz))*dxy
       a4 = ws*dxy
       ftot=(a12*f12-a2*f1)/a1
       fws(iz,id,iurb)=ftot*a1/a4 
 
      enddo
!!!!!!!!!!!!!


       do iz=1,nz_u

! radiation from wall to ground
      
         call fnrms (fnrm,ws,dxy,z(iz+1))
         f12=fnrm
         call fnrms (fnrm,ws,dxy,z(iz  ))
         f1 =fnrm
         
         a1= (z(iz+1)-z(iz) )*dxy
       
         a2 = z(iz)*dxy
         a12= z(iz+1)*dxy
         a4 = ws*dxy

         ftot=(a12*f12-a2*f1)/a1        
                    
         fwg(iz,id,iurb)=ftot*a1/a4
        
      enddo

! radiation from sky to ground
      
      call fprls (fprl,dxy,ws,hut)
      fsg(id,iurb)=fprl

      return
      end subroutine view_factors

! ===6=8===============================================================72
! ===6=8===============================================================72


      SUBROUTINE fprls (fprl,a,b,c)

      implicit none

     
            
      real(kind=kind_noahmp) a,b,c
      real(kind=kind_noahmp) x,y
      real(kind=kind_noahmp) fprl


      x=a/c
      y=b/c
      
      if(a.eq.0.or.b.eq.0.)then
       fprl=0.
      else
       fprl=log( ( (1.+x**2)*(1.+y**2)/(1.+x**2+y**2) )**.5)+  &
           y*((1.+x**2)**.5)*atan(y/((1.+x**2)**.5))+          &  
           x*((1.+y**2)**.5)*atan(x/((1.+y**2)**.5))-          &   
           y*atan(y)-x*atan(x)
       fprl=fprl*2./(pi*x*y)
      endif
      
      return
      end subroutine fprls

! ===6=8===============================================================72     
! ===6=8===============================================================72

      SUBROUTINE fnrms (fnrm,a,b,c)

      implicit none



      real(kind=kind_noahmp) a,b,c
      real(kind=kind_noahmp) x,y,z,a1,a2,a3,a4,a5,a6
      real(kind=kind_noahmp) fnrm
      
      x=a/b
      y=c/b
      z=x**2+y**2
      
      if(y.eq.0.or.x.eq.0)then
       fnrm=0.
      else
       a1=log( (1.+x*x)*(1.+y*y)/(1.+z) )
       a2=y*y*log(y*y*(1.+z)/z/(1.+y*y) )
       a3=x*x*log(x*x*(1.+z)/z/(1.+x*x) )
       a4=y*atan(1./y)
       a5=x*atan(1./x)
       a6=sqrt(z)*atan(1./sqrt(z))
       fnrm=0.25*(a1+a2+a3)+a4+a5-a6
       fnrm=fnrm/(pi*y)
      endif
      
      return
      end subroutine fnrms
  ! ===6=8===============================================================72  
     
        SUBROUTINE init_para(alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,&
        twini_u,trini_u,tgini_u,albg_u,albw_u,albr_u,albwin_u,emg_u,emw_u,&
        emr_u,emwind_u,z0g_u,z0r_u,nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b,  &
        cop_u,pwin_u,beta_u,sw_cond_u,time_on_u,time_off_u,targtemp_u,    &
        bldac_frc_u,cooled_frc_u,                                         &
        gaptemp_u, targhum_u,gaphum_u,perflo_u,                           &
        gr_frac_roof_u,pv_frac_roof_u,                   &
        hsesf_u,hsequip,irho,gr_flag_u,gr_type_u)

 
! initialization routine, where the variables from the table are read

      implicit none
      integer iurb            ! urban class number
!    Building parameters      
      real(kind=kind_noahmp) alag_u(nurbm)      ! Ground thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) alaw_u(nurbm)      ! Wall thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) alar_u(nurbm)      ! Roof thermal diffusivity [m^2 s^-1]
      real(kind=kind_noahmp) csg_u(nurbm)       ! Specific heat of the ground material [J m^3 K^-1]
      real(kind=kind_noahmp) csw_u(nurbm)       ! Specific heat of the wall material [J m^3 K^-1]
      real(kind=kind_noahmp) csr_u(nurbm)       ! Specific heat of the roof material [J m^3 K^-1]
      real(kind=kind_noahmp) twini_u(nurbm)     ! Temperature inside the buildings behind the wall [K]
      real(kind=kind_noahmp) trini_u(nurbm)     ! Temperature inside the buildings behind the roof [K]
      real(kind=kind_noahmp) tgini_u(nurbm)     ! Initial road temperature

!    Radiation parameters
      real(kind=kind_noahmp) albg_u(nurbm)      ! Albedo of the ground
      real(kind=kind_noahmp) albw_u(nurbm)      ! Albedo of the wall
      real(kind=kind_noahmp) albr_u(nurbm)      ! Albedo of the roof
      real(kind=kind_noahmp) albwin_u(nurbm)    ! Albedo of the window
      real(kind=kind_noahmp) emg_u(nurbm)       ! Emissivity of ground
      real(kind=kind_noahmp) emw_u(nurbm)       ! Emissivity of wall
      real(kind=kind_noahmp) emr_u(nurbm)       ! Emissivity of roof
      real(kind=kind_noahmp) emwind_u(nurbm)    ! Emissivity of windows

!    Roughness parameters
      real(kind=kind_noahmp) z0g_u(nurbm)       ! The ground's roughness length      
      real(kind=kind_noahmp) z0r_u(nurbm)       ! The roof's roughness length

!    Street parameters
      integer nd_u(nurbm)     ! Number of street direction for each urban class

      real(kind=kind_noahmp) strd_u(ndm,nurbm)  ! Street length (fix to greater value to the horizontal length of the cells)
      real(kind=kind_noahmp) drst_u(ndm,nurbm)  ! Street direction [degree]
      real(kind=kind_noahmp) ws_u(ndm,nurbm)    ! Street width [m]
      real(kind=kind_noahmp) bs_u(ndm,nurbm)    ! Building width [m]
      real(kind=kind_noahmp) h_b(nz_um,nurbm)   ! Bulding's heights [m]
      real(kind=kind_noahmp) d_b(nz_um,nurbm)   ! The probability that a building has an height h_b

      integer i,iu
      integer nurb ! number of urban classes used
        real(kind=kind_noahmp), intent(out) :: bldac_frc_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: cooled_frc_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: cop_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: pwin_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: beta_u(nurbm)
      integer, intent(out) :: sw_cond_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: time_on_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: time_off_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: targtemp_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: gaptemp_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: targhum_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: gaphum_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: perflo_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: gr_frac_roof_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: pv_frac_roof_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: hsesf_u(nurbm)
      real(kind=kind_noahmp), intent(out) :: hsequip(24)
      real(kind=kind_noahmp), intent(out) :: irho(24)
      integer, intent(out) :: gr_flag_u,gr_type_u
!
!Initialize some variables
!  
     
       h_b=0.
       d_b=0.

       nurb=ICATE
       do iu=1,nurb                         
          nd_u(iu)=0
       enddo

       csw_u=CAPB_TBL / (( 1.0 / 4.1868 ) * 1.E-6)
       csr_u=CAPR_TBL / (( 1.0 / 4.1868 ) * 1.E-6)
       csg_u=CAPG_TBL / (( 1.0 / 4.1868 ) * 1.E-6)
       do i=1,icate
         alaw_u(i)=AKSB_TBL(i) / csw_u(i) / (( 1.0 / 4.1868 ) * 1.E-2)
         alar_u(i)=AKSR_TBL(i) / csr_u(i) / (( 1.0 / 4.1868 ) * 1.E-2)
         alag_u(i)=AKSG_TBL(i) / csg_u(i) / (( 1.0 / 4.1868 ) * 1.E-2)
       enddo
       twini_u=TBLEND_TBL
       trini_u=TRLEND_TBL
       tgini_u=TGLEND_TBL
       albw_u=ALBB_TBL
       albr_u=ALBR_TBL
       albg_u=ALBG_TBL
       emw_u=EPSB_TBL
       emr_u=EPSR_TBL
       emg_u=EPSG_TBL
       z0r_u=Z0R_TBL
       z0g_u=Z0G_TBL
       nd_u=NUMDIR_TBL
!FS
     !  print*, 'g alla call', gr_frac_roof_u(iurb)
       bldac_frc_u = bldac_frc_tbl
       cooled_frc_u = cooled_frc_tbl
       cop_u = cop_tbl
       pwin_u = pwin_tbl
       beta_u = beta_tbl
       sw_cond_u = sw_cond_tbl
       time_on_u = time_on_tbl
       time_off_u = time_off_tbl
       targtemp_u = targtemp_tbl
       gaptemp_u = gaptemp_tbl
       targhum_u = targhum_tbl
       gaphum_u = gaphum_tbl
       perflo_u = perflo_tbl
       gr_frac_roof_u =gr_frac_roof_tbl
       gr_flag_u=gr_flag_tbl
       pv_frac_roof_u = pv_frac_roof_tbl
       hsesf_u = hsesf_tbl
       hsequip = hsequip_tbl
       irho=irho_tbl
       gr_type_u=gr_type_tbl
       do iu=1,icate
              if(ndm.lt.nd_u(iu))then
                write(*,*)'ndm too small in module_sf_bep_bem, please increase to at least ', nd_u(iu)
                write(*,*)'remember also that num_urban_layers should be equal or greater than nz_um*ndm*nwr-u!'
                stop
              endif
         do i=1,nd_u(iu)
           drst_u(i,iu)=STREET_DIRECTION_TBL(i,iu) * pi/180.
           ws_u(i,iu)=STREET_WIDTH_TBL(i,iu)
           bs_u(i,iu)=BUILDING_WIDTH_TBL(i,iu)
         enddo
       enddo
       do iu=1,ICATE
          if(nz_um.lt.numhgt_tbl(iu)+3)then
              write(*,*)'nz_um too small in module_sf_bep, please increase to at least ',numhgt_tbl(iu)+3
              write(*,*)'remember also that num_urban_layers should be equal or greater than nz_um*ndm*nwr-u!'
              stop
          endif
         do i=1,NUMHGT_TBL(iu)
           h_b(i,iu)=HEIGHT_BIN_TBL(i,iu)
           d_b(i,iu)=HPERCENT_BIN_TBL(i,iu)
         enddo
       enddo

       do i=1,ndm
        do iu=1,nurbm
         strd_u(i,iu)=100000.
        enddo
       enddo

       do iu=1,nurb  
          emwind_u(iu)=0.9                       
          call albwindow(albwin_u(iu))  
       enddo
       
       return
       end subroutine init_para
!==============================================================
!==============================================================
!====6=8===============================================================72         
!====6=8===============================================================72 

       subroutine upward_rad(ndu,nzu,ws,bs,sigma,pb,ss,                &
                       tg_av,emg_u,albg_u,rlg,rsg,sfg,lfg,                   & 
                       tw,emw_u,albw_u,rlw,rsw,sfw,                   & 
                       tr_av,emr_u,albr_u,emwind,albwind,twlev,pwin,     &
                       sfwind,rld,rs, sfr,sfrv,lfr,lfrv,                            & 
                       rs_abs,rl_up,emiss,grdflx_urb,gr_frac_roof,tpvlev,pv_frac_roof)
!
! IN this surboutine we compute the upward longwave flux, and the albedo
! needed for the radiation scheme
!
      implicit none

!
!INPUT VARIABLES
!
      real(kind=kind_noahmp) rsw(2*ndm,nz_um)        ! Short wave radiation at the wall for a given canyon direction [W/m2]
      real(kind=kind_noahmp) rlw(2*ndm,nz_um)         ! Long wave radiation at the walls for a given canyon direction [W/m2]
      real(kind=kind_noahmp) rsg(ndm)                   ! Short wave radiation at the canyon for a given canyon direction [W/m2]
      real(kind=kind_noahmp) rlg(ndm)                   ! Long wave radiation at the ground for a given canyon direction [W/m2]
      real(kind=kind_noahmp) rs                        ! Short wave radiation at the horizontal surface from the sun [W/m2]  
      real(kind=kind_noahmp) sfw(2*ndm,nz_um)      ! Sensible heat flux from walls [W/m2]
      real(kind=kind_noahmp) sfg(ndm)              ! Sensible heat flux from ground (road) [W/m2]
      real(kind=kind_noahmp) lfg(ndm)
      real(kind=kind_noahmp) sfr(ndm,nz_um)      ! Sensible heat flux from roofs [W/m2]   
      real(kind=kind_noahmp) lfr(ndm,nz_um)
      real(kind=kind_noahmp) lfrv(ndm,nz_um)
      real(kind=kind_noahmp) sfrv(ndm,nz_um)
      real(kind=kind_noahmp) gr_frac_roof
      real(kind=kind_noahmp) rld                        ! Long wave radiation from the sky [W/m2]
      real(kind=kind_noahmp) albg_u                    ! albedo of the ground/street
      real(kind=kind_noahmp) albw_u                    ! albedo of the walls
      real(kind=kind_noahmp) albr_u                    ! albedo of the roof 
      real(kind=kind_noahmp) ws(ndm)                        ! width of the street
      real(kind=kind_noahmp) bs(ndm)
                        ! building size
      real(kind=kind_noahmp) pb(nz_um)                ! Probability to have a building with an height equal or higher   
      integer nzu
      real(kind=kind_noahmp) ss(nz_um)                ! Probability to have a building of a given height
      real(kind=kind_noahmp) sigma                       
      real(kind=kind_noahmp) emg_u                       ! emissivity of the street
      real(kind=kind_noahmp) emw_u                       ! emissivity of the wall
      real(kind=kind_noahmp) emr_u                       ! emissivity of the roof
      real(kind=kind_noahmp) tw(2*ndm,nz_um)  ! Temperature in each layer of the wall [K]
      real(kind=kind_noahmp) tr_av(ndm,nz_um)  ! Temperature in each layer of the roof [K]
      real(kind=kind_noahmp) tpvlev(ndm,nz_um)
      real(kind=kind_noahmp) pv_frac_roof
      real(kind=kind_noahmp) tg_av(ndm)          ! Temperature in each layer of the ground [K]
      integer id ! street direction
      integer ndu ! number of street directions
!
!New variables BEM
!
      real(kind=kind_noahmp) emwind               !Emissivity of the windows
      real(kind=kind_noahmp) albwind              !Albedo of the windows
      real(kind=kind_noahmp) twlev(2*ndm,nz_um)   !Averaged Temperature of the windows 
      real(kind=kind_noahmp) pwin                 !Coverage area fraction of the windows
      real(kind=kind_noahmp) gflwin               !Heat stored for the windows
      real(kind=kind_noahmp) sfwind(2*ndm,nz_um)  !Sensible heat flux from windows [W/m2]

!OUTPUT/INPUT
      real(kind=kind_noahmp) rs_abs  ! absrobed solar radiationfor this street direction
      real(kind=kind_noahmp) rl_up   ! upward longwave radiation for this street direction
      real(kind=kind_noahmp) emiss ! mean emissivity
      real(kind=kind_noahmp) grdflx_urb ! ground heat flux 
!LOCAL
      integer iz,iw
      real(kind=kind_noahmp) rl_inc,rl_emit
      real(kind=kind_noahmp) gfl
      integer ix,iy,iwrong

         iwrong=1
      do iz=1,nzu+1
      do id=1,ndu
      if(tr_av(id,iz).lt.100.)then
              write(203,*) tr_av(id,iz)
              write(*,*)'in upward_rad ',iz,id,iw,tr_av(id,iz)
              iwrong=0
      endif     
      enddo
      enddo
           if(iwrong.eq.0)stop

      rl_up=0.
 
      rs_abs=0.
      rl_inc=0.
      emiss=0.
      rl_emit=0.
      grdflx_urb=0.
      do id=1,ndu          
       rl_emit=rl_emit-( emg_u*sigma*(tg_av(id)**4.)+(1-emg_u)*rlg(id))*ws(id)/(ws(id)+bs(id))/ndu
       rl_inc=rl_inc+rlg(id)*ws(id)/(ws(id)+bs(id))/ndu       
       rs_abs=rs_abs+(1.-albg_u)*rsg(id)*ws(id)/(ws(id)+bs(id))/ndu
         gfl=(1.-albg_u)*rsg(id)+emg_u*rlg(id)-emg_u*sigma*(tg_av(id)**4.)+sfg(id)+lfg(id)
         grdflx_urb=grdflx_urb-gfl*ws(id)/(ws(id)+bs(id))/ndu  
 
         do iz=2,nzu
             rl_emit=rl_emit-(emr_u*sigma*(1.-pv_frac_roof)*tr_av(id,iz)**4.+0.79*sigma*pv_frac_roof*tpvlev(id,iz)**4+ &
                     (1-emr_u)*rld*(1.-pv_frac_roof)+(1-0.79)*pv_frac_roof*rld)*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
             rl_inc=rl_inc+rld*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
             rs_abs=rs_abs+((1.-albr_u)*rs*(1.-pv_frac_roof)+(1.-0.11)*rs*pv_frac_roof)*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
             gfl=(1.-albr_u)*rs*(1-pv_frac_roof)+emr_u*rld*(1-pv_frac_roof)+pv_frac_roof*emr_u*sigma*tpvlev(id,iz)**4 &
                -emr_u*sigma*(tr_av(id,iz)**4.)+(1-gr_frac_roof)*sfr(id,iz)+(sfrv(id,iz)+lfrv(id,iz))*gr_frac_roof+(1.-gr_frac_roof)*lfr(id,iz)
             grdflx_urb=grdflx_urb-gfl*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu  
         enddo
           
         do iz=1,nzu 
           
            rl_emit=rl_emit-(emw_u*(1.-pwin)*sigma*(tw(2*id-1,iz)**4.+tw(2*id,iz)**4.)+ &
                            (emwind*pwin*sigma*(twlev(2*id-1,iz)**4.+twlev(2*id,iz)**4.))+ &
                ((1.-emw_u)*(1.-pwin)+pwin*(1.-emwind))*(rlw(2*id-1,iz)+rlw(2*id,iz)))* &
                            dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu

            rl_inc=rl_inc+((rlw(2*id-1,iz)+rlw(2*id,iz)))*dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu

            rs_abs=rs_abs+(((1.-albw_u)*(1.-pwin)+(1.-albwind)*pwin)*(rsw(2*id-1,iz)+rsw(2*id,iz)))*&
                          dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu 

            gfl=(1.-albw_u)*(rsw(2*id-1,iz)+rsw(2*id,iz)) +emw_u*( rlw(2*id-1,iz)+rlw(2*id,iz) )   &
             -emw_u*sigma*( tw(2*id-1,iz)**4.+tw(2*id,iz)**4. )+(sfw(2*id-1,iz)+sfw(2*id,iz))   

            gflwin=(1.-albwind)*(rsw(2*id-1,iz)+rsw(2*id,iz)) +emwind*(rlw(2*id-1,iz)+rlw(2*id,iz))   &
             -emwind*sigma*( twlev(2*id-1,iz)**4.+twlev(2*id,iz)**4.)+(sfwind(2*id-1,iz)+sfwind(2*id,iz)) 
               
           
            grdflx_urb=grdflx_urb-(gfl*(1.-pwin)+pwin*gflwin)*dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu

         enddo
          
      enddo
        emiss=(emg_u+emw_u+emr_u)/3.
        rl_up=(rl_inc+rl_emit)-rld
       
         
      return

      END SUBROUTINE upward_rad

!====6=8===============================================================72         
!====6=8===============================================================72 
! ===6================================================================72
! ===6================================================================72

         subroutine albwindow(albwin)
		
!-------------------------------------------------------------------
	 implicit none


! -------------------------------------------------------------------
!Based on the 
!paper of J.Karlsson and A.Roos(2000):"modelling the angular behaviour
!of the total solar energy transmittance of windows"
!Solar Energy Vol.69,No.4,pp. 321-329.      
! -------------------------------------------------------------------
!Input
!-----	
        
!Output
!------
         real(kind=kind_noahmp) albwin	        ! albedo of the window  
!Local
!-----
	 real(kind=kind_noahmp) a,b,c		!Polynomial coefficients
	 real(kind=kind_noahmp) alfa,delta,gama	!Polynomial powers
	 real(kind=kind_noahmp) g0	        !transmittance when the angle 
                                !of incidence is normal to the surface.
         real(kind=kind_noahmp) asup,ainf
	 real(kind=kind_noahmp) fonc

!Constants
!--------------------
         
         real(kind=kind_noahmp) epsilon              !accuracy of the integration
         parameter (epsilon=1.e-07) 
         real(kind=kind_noahmp) n1,n2                !Index of refraction for glasses and air
         parameter(n1=1.,n2=1.5)
         integer intg,k
!--------------------------------------------------------------------		
         if (q_num.eq.0) then
           write(*,*) 'Category parameter of the windows no valid'
           stop
         endif

         g0=4.*n1*n2/((n1+n2)*(n1+n2))
	 a=8.
	 b=0.25/q_num
         c=1.-a-b	
	 alfa =5.2 + (0.7*q_num)
	 delta =2.
	 gama =(5.26+0.06*p_num)+(0.73+0.04*p_num)*q_num

         intg=1
!----------------------------------------------------------------------


100      asup=0.
         ainf=0.

         do k=1,intg
          call foncs(fonc,(pi*k/intg),a,b,c,alfa,delta,gama)
          asup=asup+(pi/intg)*fonc
         enddo

         intg=intg+1

         do k=1,intg
          call foncs(fonc,(pi*k/intg),a,b,c,alfa,delta,gama)
          ainf=ainf+(pi/intg)*fonc
         enddo
	 
         if(abs(asup-ainf).lt.epsilon) then
           albwin=1-g0+(g0/2.)*asup
         else
           goto 100
         endif
        
!---------------------------------------------------------------------- 	
	return
	end subroutine albwindow
!====================================================================72
!====================================================================72

        subroutine foncs(fonc,x,aa,bb,cc,alf,delt,gam)

        implicit none
!
        real(kind=kind_noahmp) x,aa,bb,cc
        real(kind=kind_noahmp) alf,delt,gam
        real(kind=kind_noahmp) fonc
  
        fonc=(((aa*(x**alf))/(pi**alf))+   &
             ((bb*(x**delt))/(pi**delt))+  &
             ((cc*(x**gam))/(pi**gam)))*sin(x)
        
        return
	end subroutine foncs
!====================================================================72
!====================================================================72  

      subroutine icBEP_XY(iurb,fww_u,fwg_u,fgw_u,fsw_u,             &
                          fws_u,fsg_u,ndu,strd,ws,nzu,z_u)                               

      implicit none       
        
!    Street parameters
      integer ndu     ! Number of street direction for each urban class
      integer iurb

      real(kind=kind_noahmp) strd(ndm)        ! Street length (fix to greater value to the horizontal length of the cells)
      real(kind=kind_noahmp) ws(ndm)          ! Street width [m]

!    Grid parameters
      integer nzu          ! Number of layer in the urban grid
      real(kind=kind_noahmp) z_u(nz_um)       ! Height of the urban grid levels
! -----------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------

!   fww_u,fwg_u,fgw_u,fsw_u,fsg_u are the view factors used to compute the long wave
!   and the short wave radation. They are the part of radiation from a surface
!   or from the sky to another surface.

      real(kind=kind_noahmp) fww_u(nz_um,nz_um,ndm,nurbm)         !  from wall to wall
      real(kind=kind_noahmp) fwg_u(nz_um,ndm,nurbm)               !  from wall to ground
      real(kind=kind_noahmp) fgw_u(nz_um,ndm,nurbm)               !  from ground to wall
      real(kind=kind_noahmp) fsw_u(nz_um,ndm,nurbm)               !  from sky to wall
      real(kind=kind_noahmp) fws_u(nz_um,ndm,nurbm)               !  from sky to wall
      real(kind=kind_noahmp) fsg_u(ndm,nurbm)                     !  from sky to ground

! -----------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------

      integer id

! -----------------------------------------------------------------------
!     This routine compute the view factors
!------------------------------------------------------------------------
!
!Initialize
!
      fww_u=0.
      fwg_u=0.
      fgw_u=0.
      fsw_u=0.
      fws_u=0.
      fsg_u=0.
      
      do id=1,ndu

            call view_factors(iurb,nzu,id,strd(id),z_u,ws(id),  &    
                              fww_u,fwg_u,fgw_u,fsg_u,fsw_u,fws_u) 
      
      enddo               
      return       
      end subroutine icBEP_XY
!====================================================================72
!====================================================================72  
      subroutine icBEPHI_XY(iurb,hb_u,hi_urb1D,ss_u,pb_u,nzu,z_u)

      implicit none   
!-----------------------------------------------------------------------
!    Inputs
!-----------------------------------------------------------------------
!    Street parameters
!
      real(kind=kind_noahmp) hi_urb1D(nz_um)    ! The probability that a building has an height h_b
      integer iurb            ! Number of the urban class
!
!     Grid parameters
!
      real(kind=kind_noahmp) z_u(nz_um)         ! Height of the urban grid levels
! -----------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------

      real(kind=kind_noahmp) ss_u(nz_um,nurbm)  ! The probability that a building has an height equal to z
      real(kind=kind_noahmp) pb_u(nz_um)        ! The probability that a building has an height greater or equal to z
!        
!    Grid parameters
!
      integer nzu                ! Number of layer in the urban grid

! -----------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------
      real(kind=kind_noahmp) hb_u(nz_um)        ! Bulding's heights [m]
      integer iz_u,id,ilu

      real(kind=kind_noahmp) dtot
      real(kind=kind_noahmp) hbmax

!------------------------------------------------------------------------

!Initialize variables
!
      
      nzu=0
      ss_u=0.
      pb_u=0.
      
! Normalisation of the building density

         dtot=0.
         hb_u=0.

         do ilu=1,nz_um
            dtot=dtot+hi_urb1D(ilu)
         enddo

         do ilu=1,nz_um
            if (hi_urb1D(ilu)<0.) then
!              write(*,*) 'WARNING, HI_URB1D(ilu) < 0 IN BEP_BEM'
               go to 20
            endif
         enddo

         if (dtot.gt.0.) then
            continue
         else
!           write(*,*) 'WARNING, HI_URB1D <= 0 IN BEP_BEM'
            go to 20
         endif

         do ilu=1,nz_um
            hi_urb1D(ilu)=hi_urb1D(ilu)/dtot
         enddo
         
         hb_u(1)=dz_u   
         do ilu=2,nz_um
            hb_u(ilu)=dz_u+hb_u(ilu-1)
         enddo
           

! Compute pb and ss 
      
            
         hbmax=0.
       
         do ilu=1,nz_um
            if (hi_urb1D(ilu)>0.and.hi_urb1D(ilu)<=1.) then
                hbmax=hb_u(ilu)
            endif
         enddo
         
         do iz_u=1,nz_um-1
            if(z_u(iz_u+1).gt.hbmax)go to 10
         enddo

10       continue 
        
         nzu=iz_u+1
      
         if ((nzu+1).gt.nz_um) then 
             write(*,*) 'error, nz_um has to be increased to at least',nzu+1
             stop
         endif

            do iz_u=1,nzu
               ss_u(iz_u,iurb)=0.
               do ilu=1,nz_um
                  if(z_u(iz_u).le.hb_u(ilu)                      &    
                    .and.z_u(iz_u+1).gt.hb_u(ilu))then            
                        ss_u(iz_u,iurb)=ss_u(iz_u,iurb)+hi_urb1D(ilu)
                  endif 
               enddo
            enddo

            pb_u(1)=1.
            do iz_u=1,nzu
               pb_u(iz_u+1)=max(0.,pb_u(iz_u)-ss_u(iz_u,iurb))
            enddo

20     continue    
       return
       end subroutine icBEPHI_XY
!====================================================================72
!====================================================================72
END MODULE module_sf_bep_bem

! ===6=8===============================================================72
! ===6=8===============================================================72

      FUNCTION bep_bem_nurbm () RESULT (bep_bem_val_nurbm)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nurbm
         bep_bem_val_nurbm = nurbm
      END FUNCTION bep_bem_nurbm

      FUNCTION bep_bem_ndm () RESULT (bep_bem_val_ndm)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_ndm
         bep_bem_val_ndm = ndm
      END FUNCTION bep_bem_ndm

      FUNCTION bep_bem_nz_um () RESULT (bep_bem_val_nz_um)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nz_um
         bep_bem_val_nz_um = nz_um
      END FUNCTION bep_bem_nz_um

      FUNCTION bep_bem_ng_u () RESULT (bep_bem_val_ng_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_ng_u
         bep_bem_val_ng_u = ng_u
      END FUNCTION bep_bem_ng_u

      FUNCTION bep_bem_nwr_u () RESULT (bep_bem_val_nwr_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nwr_u
         bep_bem_val_nwr_u = nwr_u
      END FUNCTION bep_bem_nwr_u

      FUNCTION bep_bem_nf_u () RESULT (bep_bem_val_nf_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nf_u
         bep_bem_val_nf_u = nf_u
      END FUNCTION bep_bem_nf_u


      FUNCTION bep_bem_ngb_u () RESULT (bep_bem_val_ngb_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_ngb_u
         bep_bem_val_ngb_u = ngb_u
      END FUNCTION bep_bem_ngb_u

      FUNCTION bep_bem_nbui_max () RESULT (bep_bem_val_nbui_max)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nbui_max
         bep_bem_val_nbui_max = nbui_max
      END FUNCTION bep_bem_nbui_max

 
   FUNCTION bep_bem_ngr_u () RESULT (bep_bem_val_ngr_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_ngr_u
         bep_bem_val_ngr_u = ngr_u
      END FUNCTION bep_bem_ngr_u

