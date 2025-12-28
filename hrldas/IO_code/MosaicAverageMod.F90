module MosaicAverageMod

  !!! This module is part of NoahMP Mosaic/Subgrid Tiling Scheme
  !!! Purpose: To calculate grid average value from all subgrid fractions

  ! ------------------------ Code history -----------------------------------
  ! Original code : Prasanth Valayamkunnath (IISER Thiruvananthapuram)
  ! Date          : September 12, 2025
  ! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType

  implicit none

  ! Interface block for MosaicAverage
  interface MosaicAverage
    module procedure MosaicAverage_2d
    module procedure MosaicAverage_3d
    module procedure MosaicAverage_2d_int
  end interface MosaicAverage

  contains

  !=== Calculate averages along subgrid fractions ===!

  function MosaicAverage_2d(InVariable,NMPIO) result(OutAverage)
    real(kind=kind_noahmp), dimension(:,:,:), intent(in) :: InVariable
    real(kind=kind_noahmp), allocatable, dimension(:,:)  :: OutAverage
    type(NoahmpIO_type), intent(in)                      :: NMPIO
    integer :: ixpar, jxpar, nxpar

    ! Get dimensions
    ixpar = size(InVariable, 1)
    jxpar = size(InVariable, 2)
    nxpar = size(InVariable, 3)

    ! Check dimensions of SubGrdFracMosaic
    if (size(NMPIO%SubGrdFracMosaic, 1) /= ixpar .or. &
        size(NMPIO%SubGrdFracMosaic, 2) /= jxpar .or. &
        size(NMPIO%SubGrdFracMosaic, 3) /= nxpar) then
      stop "Error: Dimension mismatch in NoahmpIO%SubGrdFracMosaic for MosaicAverage_2d"
    end if

    ! Allocate output array
    allocate(OutAverage(ixpar, jxpar))

    ! Calculate weighted average
    OutAverage = sum(InVariable * NMPIO%SubGrdFracMosaic, dim=3)

  end function MosaicAverage_2d

  function MosaicAverage_3d(InVariable,NMPIO) result(OutAverage)
    real(kind=kind_noahmp), dimension(:,:,:,:), intent(in)  :: InVariable
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)   :: OutAverage
    real(kind=kind_noahmp), allocatable, dimension(:,:,:,:) :: SubGrdFrac
    type(NoahmpIO_type), intent(in)                         :: NMPIO
    integer :: ixpar, jxpar, kxpar, nxpar, k

    ! Get dimensions
    ixpar = size(InVariable, 1)
    kxpar = size(InVariable, 2)
    jxpar = size(InVariable, 3)
    nxpar = size(InVariable, 4)

    ! Check dimensions of SubGrdFracMosaic
    if (size(NMPIO%SubGrdFracMosaic, 1) /= ixpar .or. &
        size(NMPIO%SubGrdFracMosaic, 2) /= jxpar .or. &
        size(NMPIO%SubGrdFracMosaic, 3) /= nxpar) then
      stop "Error: Dimension mismatch in NoahmpIO%SubGrdFracMosaic for MosaicAverage_3d"
    end if

    ! Allocate arrays
    allocate(OutAverage(ixpar, kxpar, jxpar))
    allocate(SubGrdFrac(ixpar, kxpar, jxpar, nxpar))

    ! Populate SubGrdFrac
    do k = 1, kxpar
      SubGrdFrac(:, k, :, :) = NMPIO%SubGrdFracMosaic
    end do

    ! Calculate weighted average
    OutAverage = sum(InVariable * SubGrdFrac, dim=4)

    ! Deallocate temporary array
    deallocate(SubGrdFrac)

  end function MosaicAverage_3d

  function MosaicAverage_2d_int (InVariable, NMPIO) result(OutAverage)
    integer, dimension(:,:,:), intent(in) :: InVariable
    integer, allocatable, dimension(:,:)  :: OutAverage
    type(NoahmpIO_type), intent(in)       :: NMPIO
    integer :: ixpar, jxpar, nxpar

    ! Get dimensions
    ixpar = size(InVariable, 1)
    jxpar = size(InVariable, 2)
    nxpar = size(InVariable, 3)

    ! Check dimensions of SubGrdFracMosaic
    if (size(NMPIO%SubGrdFracMosaic, 1) /= ixpar .or. &
        size(NMPIO%SubGrdFracMosaic, 2) /= jxpar .or. &
        size(NMPIO%SubGrdFracMosaic, 3) /= nxpar) then
      stop "Error: Dimension mismatch in NoahmpIO%SubGrdFracMosaic for MosaicAverage_2d"
    end if

    ! Allocate output array
    allocate(OutAverage(ixpar, jxpar))

    ! Calculate weighted average
    OutAverage = nint(sum(InVariable * NMPIO%SubGrdFracMosaic, dim=3))

  end function MosaicAverage_2d_int

end module MosaicAverageMod