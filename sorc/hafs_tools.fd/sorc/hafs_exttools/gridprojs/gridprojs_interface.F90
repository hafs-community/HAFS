module gridprojs_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! gridprojs :: gridprojs_interface
  ! Copyright (C) 2018 Henry R. Winterbottom

  ! Email: henry.winterbottom@noaa.gov

  ! This program is free software: you can redistribute it and/or
  ! modify it under the terms of the GNU General Public License as
  ! published by the Free Software Foundation, either version 3 of the
  ! License, or (at your option) any later version.

  ! This program is distributed in the hope that it will be useful,
  ! but WITHOUT ANY WARRANTY; without even the implied warranty of
  ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  ! General Public License for more details.

  ! You should have received a copy of the GNU General Public License
  ! along with this program.  If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: gridprojs_intkind
  public :: gridprojs_realkind
  public :: gridprojs_spval
  public :: specgrids_compute
  public :: specgrids_cleanup
  public :: specgrids_grid

  ! Define local variables

  integer,                                                    parameter :: gridprojs_intkind  = 4
  integer,                                                    parameter :: gridprojs_realkind = 4  
  type specgrids_grid
     real(gridprojs_realkind),  dimension(:),               allocatable :: lats
     real(gridprojs_realkind),  dimension(:),               allocatable :: lons
     real(gridprojs_realkind)                                           :: dx
     real(gridprojs_realkind)                                           :: dy
     real(gridprojs_realkind)                                           :: rlat_min
     real(gridprojs_realkind)                                           :: rlat_max
     real(gridprojs_realkind)                                           :: rlon_min
     real(gridprojs_realkind)                                           :: rlon_max
     integer(gridprojs_intkind)                                         :: ncoords
     integer(gridprojs_intkind)                                         :: nlats
     integer(gridprojs_intkind)                                         :: nlons
     integer(gridprojs_intkind)                                         :: ntrunc
  end type specgrids_grid
  real(gridprojs_realkind),                                   parameter :: pi                 = acos(-1.0)
  real(gridprojs_realkind),                                   parameter :: deg2rad            = pi/180.0
  real(gridprojs_realkind),                                   parameter :: rad2deg            = 1.0/deg2rad
  real(gridprojs_realkind),                                   parameter :: gridprojs_spval    = huge(1.0)

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! init_specgrids.f90

  ! DESCRIPTION:

  ! This subroutine allocates and initializes all arrays (when
  ! necessary) within the user specified FORTRAN specgrids_grid
  ! variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN specgrids_grid variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN specgrids_grid variable containing allocated and
  !   initialized variables.
  
  !-----------------------------------------------------------------------
  
  subroutine init_specgrids(grid)

    ! Define variables passed to routine

    type(specgrids_grid)                                                :: grid

    !=====================================================================

    ! Define local variables
    
    call get_gridinfo(grid)

    ! Allocate memory for local variables
    
    if(.not. allocated(grid%lats)) allocate(grid%lats(grid%ncoords))
    if(.not. allocated(grid%lons)) allocate(grid%lons(grid%ncoords))

    !=====================================================================
    
  end subroutine init_specgrids

  !=======================================================================

  ! SUBROUTINE:

  ! specgrids_compute.f90

  ! DESCRIPTION:

  ! This subroutine defines the Gaussian-latitude grid geographical
  ! coordinate values in accordance with the spectral truncation
  ! specified by the user.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN specgrids_grid variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN specgrids_grid variable containing the
  !   Gaussian-latitude grid geographical coordinate values in
  !   accordance with the spectral truncation specified by the user

  !-----------------------------------------------------------------------
  
  subroutine specgrids_compute(grid)

    ! Define variables passed to routine

    type(specgrids_grid)                                                :: grid
    
    ! Define variables computed within routine
    
    real(kind=4),               dimension(:),               allocatable :: slat
    real(kind=4),               dimension(:),               allocatable :: wlat
    real(kind=4),               dimension(:),               allocatable :: work

    ! Define counting variables
    
    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables
    
    call init_specgrids(grid)

    ! Allocate memory for local variables
    
    if(.not. allocated(slat)) allocate(slat(grid%nlats))
    if(.not. allocated(wlat)) allocate(wlat(grid%nlats))
    if(.not. allocated(work)) allocate(work(grid%nlats))

    ! Define local variables

    call gausslat(grid%nlats,slat,wlat)
    work         = acos(slat) - pi/2.0
    grid%ncoords = 0

    ! Loop through local variable
    
    do j = 1, grid%nlats

       ! Loop through local variable
       
       do i = 1, grid%nlons
          
          ! Define local variables
          
          grid%ncoords            = grid%ncoords + 1
          grid%lats(grid%ncoords) = work(grid%nlats - j + 1)
          grid%lons(grid%ncoords) = (grid%rlon_min +                       &
               & (i-1)*grid%dx)*deg2rad
          
       end do ! do i = 1, grid%nlons
       
    end do ! do j = 1, grid%nlats

    ! Define local variables

    grid%lats = grid%lats*rad2deg
    grid%lons = grid%lons*rad2deg

    ! Deallocate memory for local variables
    
    if(allocated(slat)) deallocate(slat)
    if(allocated(wlat)) deallocate(wlat)
    if(allocated(work)) deallocate(work)

    !=====================================================================

  end subroutine specgrids_compute

  !=======================================================================

  ! SUBROUTINE:

  ! specgrids_cleanup.f90

  ! DESCRIPTION:

  ! This subroutine deallocates all arrays within the user specified
  ! FORTRAN specgrids_grid variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN specgrids_grid variable.

  !-----------------------------------------------------------------------

  subroutine specgrids_cleanup(grid)

    ! Define variables passed to routine

    type(specgrids_grid)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lats)) deallocate(grid%lats)
    if(allocated(grid%lons)) deallocate(grid%lons)

    !=====================================================================

  end subroutine specgrids_cleanup

  !=======================================================================

  ! SUBROUTINE:

  ! get_gridinfo.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN specgrids_grid variable
  ! attributes in accordance with the user specified spectral
  ! truncation (ntrunc) value; if the spectral truncation does not
  ! match a corresponding spectral truncation within this routine, the
  ! respective grid attribute values will be set to the value of the
  ! global variable gridprojs_spval.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN specgrids_grid variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN specgrids_grid variable containing the
  !   Gaussian-latitude grid attributes in accordance with the user
  !   specified spectral truncation (ntrunc).

  !-----------------------------------------------------------------------
  
  subroutine get_gridinfo(grid)

    ! Define variables passed to routine

    type(specgrids_grid)                                                :: grid

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(grid%ntrunc .eq. 62) then

       ! Define local variables

       grid%nlons    = 192
       grid%nlats    = 94
       grid%dx       = 1.875000
       grid%dy       = 1.875000
       grid%rlon_min = 0.00
       grid%rlon_max = 358.125
       grid%rlat_min = -88.542
       grid%rlat_max = 88.542

    elseif(grid%ntrunc .eq. 126) then

       ! Define local variables
       
       grid%nlons    = 384
       grid%nlats    = 190
       grid%dx       = 0.937500
       grid%dy       = 0.937500
       grid%rlon_min = 0.00
       grid%rlon_max = 359.0625
       grid%rlat_min = -89.277
       grid%rlat_max = 89.277
       
    elseif(grid%ntrunc .eq. 254) then
       
       ! Define local variables

       grid%nlons    = 768
       grid%nlats    = 384
       grid%dx       = 0.468750
       grid%dy       = 0.468750
       grid%rlon_min = 0.00
       grid%rlon_max = 359.531
       grid%rlat_min = -89.642
       grid%rlat_max = 89.642
       
    elseif(grid%ntrunc .eq. 382) then
       
       ! Define local variables

       grid%nlons    = 1152
       grid%nlats    = 576
       grid%dx       = 0.312499
       grid%dy       = 0.312499
       grid%rlon_min = 0.00
       grid%rlon_max = 359.687
       grid%rlat_min = -89.761
       grid%rlat_max = 89.761
       
    elseif(grid%ntrunc .eq. 574) then
       
       ! Define local variables

       grid%nlons    = 1760
       grid%nlats    = 880
       grid%dx       = 0.20454545
       grid%dy       = 0.20454545
       grid%rlon_min = 0.00
       grid%rlon_max = 359.79545455
       grid%rlat_min = -89.844
       grid%rlat_max = 89.844
       
    elseif(grid%ntrunc .eq. 1534) then
       
       ! Define local variables

       grid%nlons    = 3072
       grid%nlats    = 1536
       grid%dx       = 0.11718750
       grid%dy       = 0.11718750
       grid%rlon_min = 0.00
       grid%rlon_max = 359.883000
       grid%rlat_min = -89.910
       grid%rlat_max = 89.910

    else  ! if(grid%ntrunc .eq. 62)

       ! Define local variables

       grid%nlons    = 0
       grid%nlats    = 0
       grid%dx       = gridprojs_spval
       grid%dy       = gridprojs_spval
       grid%rlon_min = gridprojs_spval
       grid%rlon_max = gridprojs_spval
       grid%rlat_min = gridprojs_spval
       grid%rlat_max = gridprojs_spval

    end if ! if(grid%ntrunc .eq. 62)

    ! Define local variables

    grid%ncoords = (grid%nlons*grid%nlats)
       
    !=====================================================================

  end subroutine get_gridinfo

  !=======================================================================

end module gridprojs_interface
