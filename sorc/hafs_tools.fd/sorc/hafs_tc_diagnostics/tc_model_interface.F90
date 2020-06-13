module tc_model_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: tc_model_interface
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

  ! Define associate modules and subroutines

  use constants_interface
  use grid_methods_interface
  use interpolation_interface
  use kinds_interface
  use math_methods_interface
  use mpi_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: tc_model

  ! Define local variables

  type(tcmdiag_struct)                                                  :: tcmdiag

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! interpolate_subregion.f90

  ! DESCRIPTION:

  ! This subroutine interpolates a sub-region (dst_sub_grid) of the
  ! destination geographical projection (dst_grid).

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing geographical
  !   locations and values to remapped to the sub-region grid
  !   projection.

  ! * dst_sub_grid; a FORTRAN grid_struct variable containing the
  !   geographical locations within the destination grid projection
  !   for remapping of the source geographical projection (src_grid)
  !   values.

  ! * dst_grid; a FORTRAN grid_struct variable to contain the source
  !   grid values remapped within the destination grid sub-region.

  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the source
  !   grid values remapped within the destination grid sub-region.

  !-----------------------------------------------------------------------

  subroutine interpolate_subregion(src_grid,dst_sub_grid,dst_grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: dst_sub_grid
    type(grid_struct)                                                   :: src_grid

    ! Define variables computed within routine

    integer                                                             :: dst_idx

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Compute local variables

    call intrp_slint(src_grid,dst_sub_grid)

    ! Define local variables

    dst_grid%var = 0.0

    ! Loop through local variable
    
    do i = 1, dst_sub_grid%ncoords

       ! Define local variables

       dst_idx               = dst_sub_grid%idx(i)
       dst_grid%var(dst_idx) = dst_sub_grid%var(i)

    end do ! do i = 1, dst_sub_grid%ncoords

    !=====================================================================

  end subroutine interpolate_subregion

  !=======================================================================

  ! SUBROUTINE:

  ! intrp_slint.f90

  ! DESCRIPTION:

  ! This subroutine performs horizontal interpolation using the SLINT
  ! methodology.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid attributes, including the variable to be interpolated.

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   destination grid attributes for the geographical locations
  !   (e.g., latitude and longitude).

  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   interpolated variable (within the var attribute).

  !-----------------------------------------------------------------------

  subroutine intrp_slint(src_grid,dst_grid)

    ! Define variables passed to routine
    
    type(grid_struct)                                                   :: src_grid
    type(grid_struct)                                                   :: dst_grid

    ! Define variables computed within routine

    type(slint_struct)                                                  :: slint

    !=====================================================================

    ! Define local variables
    
    slint%dst_ncoords = dst_grid%ncoords
    slint%src_ncoords = src_grid%ncoords
    call variable_interface_setup_struct(slint)
    slint%dst_lat     = dst_grid%lat
    slint%dst_lon     = dst_grid%lon
    slint%src_lat     = src_grid%lat
    slint%src_lon     = src_grid%lon
    
    ! Compute local variables

    call interpolation_interface_init(slint)
    call interpolation_interface_bilinear(slint,src_grid%var)

    ! Define local variables

    dst_grid%var = slint%var

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(slint)
 
    !=====================================================================

  end subroutine intrp_slint

  !=======================================================================

  ! SUBROUTINE:

  ! recenter_variable.f90

  ! DESCRIPTION:

  ! This subroutine recenters a user-specified variable relative to
  ! the user-specified reference geographical location; a new
  ! grid-projection, relative to the user-specified reference
  ! geographical location, is returned within the FORTRAN remap_struct
  ! variable.

  ! INPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable containing (at minimum)
  !   the geographical locations corresponding to the variable to be
  !   recentered.

  ! * var; a FORTRAN 4-byte real-value array variable of dimension
  !   filter%ncoords (see the FORTRAN filter_struct variable).

  ! * recenter; a FORTRAN recenter_struct variable.

  ! OUTPUT VARIABLES:
  
  ! * recenter; a FORTRAN recenter_struct variable containing the
  !   recentered variable.

  !-----------------------------------------------------------------------

  subroutine recenter_variable(filter,var,recenter)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(recenter_struct)                                               :: recenter
    real(r_kind)                                                        :: var(filter%ncoords)

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid

    !=====================================================================

    ! Define local variables

    recenter%area  = tcmdiag%area
    recenter%clat  = tcmdiag%xlat
    recenter%clon  = tcmdiag%xlon
    recenter%darea = tcmdiag%darea
    call interpolation_interface_init(recenter)
    dst_grid%nx    = recenter%nx
    dst_grid%ny    = recenter%ny
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat   = recenter%lat
    dst_grid%lon   = recenter%lon
    src_grid%nx    = filter%nx
    src_grid%ny    = filter%ny
    call variable_interface_setup_struct(src_grid)
    src_grid%lat   = filter%lat
    src_grid%lon   = filter%lon
    src_grid%var   = var

    ! Compute local variables

    call intrp_slint(src_grid,dst_grid)

    ! Define local variables

    recenter%var = dst_grid%var

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(src_grid)

    !=====================================================================

  end subroutine recenter_variable

  !=======================================================================

  ! SUBROUTINE:

  ! remap_grid.f90

  ! DESCRIPTION:

  ! This subroutine remaps a variable to the original map projection
  ! specified by the user.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the grid
  !   projection and the variable to be remapped.

  ! * dst_grid; a FORTRAN grid_struct variable containing the grid
  !   projection to which the user-specified variable is to be
  !   remapped.

  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the remapped
  !   user-specified variable.

  !-----------------------------------------------------------------------

  subroutine remap_grid(src_grid,dst_grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_sub_grid 

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    dst_sub_grid%ncoords = 0

    ! Loop through local variable

    do i = 1, dst_grid%ncoords

       ! Check local variable and proceed accordingly

       if((dst_grid%lat(i) .ge. minval(src_grid%lat)) .and.                &
            & (dst_grid%lat(i) .le. maxval(src_grid%lat)) .and.            &
            & (dst_grid%lon(i) .ge. minval(src_grid%lon)) .and.            &
            & (dst_grid%lon(i) .le. maxval(src_grid%lon))) then

          ! Define local variables

          dst_sub_grid%ncoords = dst_sub_grid%ncoords + 1

       end if ! if((dst_grid%lat(i) .ge. minval(src_grid%lat))
              ! .and. (dst_grid%lat(i) .le. maxval(src_grid%lat))
              ! .and. (dst_grid%lon(i) .ge. minval(src_grid%lon))
              ! .and. (dst_grid%lon(i) .le. maxval(src_grid%lon)))

    end do ! do i = 1, dst_grid%ncoords

    ! Define local variables

    call variable_interface_setup_struct(dst_sub_grid)
    dst_sub_grid%ncoords = 0
    
    ! Loop through local variable

    do i = 1, dst_grid%ncoords

       ! Check local variable and proceed accordingly

       if((dst_grid%lat(i) .ge. minval(src_grid%lat)) .and.                &
            & (dst_grid%lat(i) .le. maxval(src_grid%lat)) .and.            &
            & (dst_grid%lon(i) .ge. minval(src_grid%lon)) .and.            &
            & (dst_grid%lon(i) .le. maxval(src_grid%lon))) then
    
          ! Define local variables

          dst_sub_grid%ncoords                   = dst_sub_grid%ncoords +  &
               & 1
          dst_sub_grid%lat(dst_sub_grid%ncoords) = dst_grid%lat(i)
          dst_sub_grid%lon(dst_sub_grid%ncoords) = dst_grid%lon(i)
          dst_sub_grid%idx(dst_sub_grid%ncoords) = i

       end if ! if((dst_grid%lat(i) .ge. minval(src_grid%lat))
              ! .and. (dst_grid%lat(i) .le. maxval(src_grid%lat))
              ! .and. (dst_grid%lon(i) .ge. minval(src_grid%lon))
              ! .and. (dst_grid%lon(i) .le. maxval(src_grid%lon)))

    end do ! do i = 1, dst_grid%ncoords

    ! Compute local variables

    call interpolate_subregion(src_grid,dst_sub_grid,dst_grid)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_sub_grid)

    !=====================================================================

  end subroutine remap_grid

  !=======================================================================

  ! SUBROUTINE:

  ! tc_model.f90

  ! DESCRIPTION:

  ! This subroutine is the interface-layer routine to estimate the
  ! tropical cyclone (TC) tangential wind profile using
  ! Fourier-transform methods.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! * filter; a FORTRAN filter_struct variable.

  ! OUTPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable containing the
  !   tangential wind (see the tngwnd attribute).

  !-----------------------------------------------------------------------

  subroutine tc_model(meteo,vrtxinfo,filter)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    !=====================================================================

    ! Compute local variables

    call wnd2d_tcm(meteo,filter,vrtxinfo)

    !=====================================================================

  end subroutine tc_model

  !=======================================================================

  ! SUBROUTINE:

  ! tcmdiag_init.f90

  ! DESCRIPTION:

  ! This subroutine initializes the values for the FORTRAN
  ! tcmdiag_struct variable; the user specified namelist variables and
  ! FORTRAN vrtxinfo_struct variable attribute values are used to
  ! assign the tcmdiag_struct variable attribute values while the TCM
  ! grid attributes are computed using the user specified namelist
  ! variables and the FORTRAN vrtxinfo_struct variable attribute
  ! values.

  ! INPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! * reflat; a FORTRAN 4-byte real-values variable containing the
  !   reference latitude coordinate value; units are degrees.

  ! * reflon; a FORTRAN 4-byte real-values variable containing the
  !   reference longitude coordinate value; units are degrees.

  !-----------------------------------------------------------------------

  subroutine tcmdiag_init(vrtxinfo,reflat,reflon)

    ! Define variables passed to routine

    type(vrtxinfo_struct)                                               :: vrtxinfo
    real(r_kind)                                                        :: reflat
    real(r_kind)                                                        :: reflon

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    real(r_kind)                                                        :: gchead(4)
    real(r_kind)                                                        :: area

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    gchead(1)   = 0.0
    grid%gclat  = reflat
    grid%gclon  = reflon
    grid%gcdist = vrtxinfo%vrtxsz
    grid%gchead = gchead(1)

    ! Compute local variables

    call grid_methods_gcgeo(grid)
    
    ! Check local variable and proceed accordingly

    if(grid%gclon .gt. 180.0) then

       ! Define local variables

       grid%grlon = grid%grlon + 360.0

    end if ! if(grid%gclon .gt. 180.0)

    ! Define local variables
    
    area = max(abs(grid%gclat - grid%grlat),abs(grid%gclon - grid%grlon))

    ! Loop through local variable

    do i = 2, 4

       ! Define local variables

       gchead(i)   = gchead(i-1) + 90.0
       grid%gchead = gchead(i)

       ! Compute local variables

       call grid_methods_gcgeo(grid)

       ! Check local variable and proceed accordingly

       if(grid%gclon .gt. 180.0) then
          
          ! Define local variables
          
          grid%grlon = grid%grlon + 360.0
          
       end if ! if(grid%gclon .gt. 180.0)

       ! Define local variables

       !grid%grlon = grid%grlon + 360.0
       area = max(max(area,abs(grid%gclat - grid%grlat)),abs(grid%gclon -  &
            & grid%grlon))

    end do ! do i = 2, 4

    ! Define local variables

    tcmdiag%area    = area
    tcmdiag%dangle  = tcm_dangle
    tcmdiag%darea   = tcm_darea
    tcmdiag%dradius = tcm_dradius
    tcmdiag%mxwvn   = tcm_mxwvn
    tcmdiag%roci    = vrtxinfo%roci
    tcmdiag%xlat    = reflat
    tcmdiag%xlon    = reflon

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    !=====================================================================

  end subroutine tcmdiag_init

  !=======================================================================

  ! SUBROUTINE:

  ! wnd2d_tcm.f90

  ! DESCRIPTION:

  ! This subroutine computes the wave-number spectra for the
  ! respective variable using the Fast-Fourier Transform (FFT); the
  ! variable is first recentered relative to the geographical location
  ! of the TC; the 2-dimensional FFT is computed along each azimuth
  ! for all radii within the user specified radius of influence; the
  ! spectral components to be retained by the user are than projected
  ! back to the geographical map projection within the FORTRAN
  ! filter_struct variable.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable.
  
  ! * filter; a FORTRAN filter_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable containing the
  !   tangential wind (see the tngwnd attribute).

  !-----------------------------------------------------------------------

  subroutine wnd2d_tcm(meteo,filter,vrtxinfo)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    type(grid_struct)                                                   :: dst_grid
    type(recenter_struct)                                               :: recenter
    type(wnd2d_struct)                                                  :: wnd2d
    real(r_kind),               dimension(:,:),             allocatable :: mpi_tngwnd
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_tngwnd))                                        &
         & allocate(mpi_tngwnd(filter%ncoords,filter%nz))

    ! Define local variables

    dst_grid%nx  = filter%nx
    dst_grid%ny  = filter%ny
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat = filter%lat
    dst_grid%lon = filter%lon
    mpi_tngwnd   = 0.0

    ! Loop through local variable

   do j = 1, filter%nz

      ! Define local variables
       
      call mpi_interface_gettasklev(mpi_procid,j,mpi_lev)

      ! Check local variable and proceed accordingly

      if(mpi_lev .ne. mpi_noproc_assign) then

         ! Define local variables

         call tcmdiag_init(vrtxinfo,filter%clat(mpi_lev),                  &
              & filter%clon(mpi_lev))

         ! Compute local variables

         call recenter_variable(filter,meteo%tngwnd(:,mpi_lev),recenter)
         
         ! Define local variables
      
         wnd2d%nx        = recenter%nx
         wnd2d%ny        = recenter%ny
         wnd2d%maxradius = maxval(recenter%radius)
         wnd2d%dradius   = tcmdiag%dradius
         wnd2d%dangle    = tcmdiag%dangle
         call variable_interface_setup_struct(wnd2d)
         wnd2d%var       = recenter%var

         ! Compute local variables

         call math_methods_wnd(wnd2d,recenter,grid)
         
         ! Define local variables

         grid%lon = grid%lon + 360.0
         grid%var = 0.0

         ! Loop through local variable

         do i = 1, min((tcmdiag%mxwvn + 1),wnd2d%nh)

            ! Compute local variables

            grid%var = grid%var + reshape(wnd2d%wnvar(i,:,:),              &
                 & shape(grid%var))

         end do ! do i = 1, min((tcmdiag%mxwvn + 1),wnd2d%nh)
         
         ! Compute local variables

         call remap_grid(grid,dst_grid)

         ! Define local variables

         mpi_tngwnd(:,mpi_lev) = dst_grid%var

         ! Deallocate memory for local variables

         call variable_interface_cleanup_struct(grid)
         call variable_interface_cleanup_struct(recenter)
         call variable_interface_cleanup_struct(wnd2d)

      end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do j = 1, filter%nz

    ! Define local variables

    call mpi_interface_waitall()
    call mpi_reduce(mpi_tngwnd,filter%tngwnd,(filter%ncoords*filter%nz),   &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_tngwnd)) deallocate(mpi_tngwnd)
    call variable_interface_cleanup_struct(dst_grid)    

    ! Define local variables

    call mpi_bcast(filter%tngwnd,(filter%ncoords*filter%nz),mpi_real,      &
         & mpi_masternode,mpi_comm_world,mpi_ierror)

    !=====================================================================

  end subroutine wnd2d_tcm

  !=======================================================================

end module tc_model_interface
