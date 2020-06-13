module filter_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: filter_interface
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

  ! Define associated modules and subroutines

  use constants_interface
  use fileio_interface
  use grid_methods_interface
  use kinds_interface
  use math_methods_interface
  use mpi_interface
  use namelist_interface
  use tc_model_interface
  use tropcyc_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: filter_interface_define

  ! Define local variables

  type(filterdiag_struct)                                               :: filterdiag

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:
  
  ! centroid.f90

  ! DESCRIPTION:

  ! This subroutine estimates the centroid (e.g., center of mass)
  ! position as a function of vertical level; this method follows from
  ! that of Kurihara et al., [1995] but has been modified to use the
  ! Rossby-Ertel potential vorticity method discussed in Davis et al.,
  ! [2008].

  ! REFERENCES:

  ! Davis, C. A., S. C. Jones, and M. Riemer, 2008: Hurricane vortex
  ! dynamics during Atlantic extratropical transition. J. Atmos. Sci.,
  ! 65, 714–736.

  ! Kurihara, Y., M. A. Bender, R. E. Tuleya, and R. J. Ross, 1995:
  ! Improvements in the GFDL Hurricane Prediction
  ! System. Mon. Wea. Rev., 123, 2791–2801.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable; it is assumed that polar
  !   coordinates (e.g., radial distance and azimuth) relatice to the
  !   reference location (i.e., the reflat and reflon attributes)
  !   within the vrtxinfo_struct variable have been computed a'priori.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable containing (at
  !   minimum) the reference geographical location (i.e., the reflat
  !   and reflon attributes) for the respective event of interest.

  ! * filter; a FORTRAN filter_struct variable.

  ! OUTPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable containing the centroid
  !   geographical location (i.e., clat and clon attributes) and the
  !   grid coordinate (the cidx attribute) as a function of the total
  !   number of meteo_struct variable levels (i.e., the nz attribute).

  !-----------------------------------------------------------------------

  subroutine centroid(grid,meteo,vrtxinfo,filter)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(kdtree_struct)                                                 :: kdtree
    real(r_kind),               dimension(:),               allocatable :: mpi_clat
    real(r_kind),               dimension(:),               allocatable :: mpi_clon
    real(r_kind)                                                        :: clat
    real(r_kind)                                                        :: clon
    real(r_kind)                                                        :: denom
    integer,                    dimension(:),               allocatable :: mpi_cidx
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================
    
    ! Allocate memory for local variables

    if(.not. allocated(mpi_clat)) allocate(mpi_clat(filter%nz))
    if(.not. allocated(mpi_clon)) allocate(mpi_clon(filter%nz))
    if(.not. allocated(mpi_cidx)) allocate(mpi_cidx(filter%nz))

    ! Define local variables

    dst_grid%nx    = 1
    dst_grid%ny    = 1
    call variable_interface_setup_struct(dst_grid)
    src_grid%nx    = meteo%nx
    src_grid%ny    = meteo%ny
    call variable_interface_setup_struct(src_grid)
    src_grid%lat   = grid%lat
    src_grid%lon   = grid%lon
    kdtree%ncoords = 1
    kdtree%nn      = 1
    call variable_interface_setup_struct(kdtree)
    mpi_clat       = 0.0
    mpi_clon       = 0.0
    mpi_cidx       = 0

    ! Loop through local variable

    do j = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,j,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Define local variables
          
          clat  = 0.0
          clon  = 0.0
          denom = 0.0

          ! Loop through local variable

          do i = 1, (meteo%nx*meteo%ny)

             ! Check local variable and proceed accordingly

             if(grid%radius(i) .le. filterdiag%maxradius) then

             ! Compute local variables

                clat  = clat + meteo%pv(i,mpi_lev)*vrtxinfo%reflat
                clon  = clon + meteo%pv(i,mpi_lev)*vrtxinfo%reflon
                denom = denom + meteo%pv(i,mpi_lev)

             end if ! if(grid%radius(i) .le. filterdiag%maxradius)

          end do ! do i = 1, (meteo%nx*meteo%ny)

          ! Define local variables

          dst_grid%lat(1) = clat/denom
          dst_grid%lon(1) = clon/denom

          ! Compute local variables

          call math_methods_kdtree(src_grid,dst_grid,kdtree)

          ! Define local variables

          mpi_clat(mpi_lev) = dst_grid%lat(1)
          mpi_clon(mpi_lev) = dst_grid%lon(1)
          mpi_cidx(mpi_lev) = kdtree%idx(1,1)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do j = 1, meteo%nz

    ! Define local variables

    call mpi_interface_waitall()
    call mpi_reduce(mpi_clat,filter%clat,filter%nz,mpi_real,mpi_sum,       &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_clon,filter%clon,filter%nz,mpi_real,mpi_sum,       &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_cidx,filter%cidx,filter%nz,mpi_integer,mpi_sum,    &
         & mpi_masternode,mpi_comm_world,mpi_ierror)    

    ! Deallocate memory for local variables

    if(allocated(mpi_clat)) deallocate(mpi_clat)
    if(allocated(mpi_clon)) deallocate(mpi_clon)
    if(allocated(mpi_cidx)) deallocate(mpi_cidx)
    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(src_grid)
    call variable_interface_cleanup_struct(kdtree)

    ! Define local variables

    call mpi_bcast(filter%clat,filter%nz,mpi_real,mpi_masternode,          &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(filter%clon,filter%nz,mpi_real,mpi_masternode,          &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(filter%cidx,filter%nz,mpi_integer,mpi_masternode,       &
         & mpi_comm_world,mpi_ierror)

    !=====================================================================

  end subroutine centroid

  !=======================================================================

  ! SUBROUTINE:

  ! define_filter.f90

  ! DESCRIPTION:

  ! This subroutine defines the mass and momentum variable filtering
  ! regions using both the user specified filter attributes (via the
  ! namelist) and the respective TC vortex attributes; the filtering
  ! regions are computed relative to the centroid position at each
  ! vertical level; all filtering regions above the depth of the TC
  ! vortex (as defined by the FORTRAN vrtxinfo_struct variable
  ! 'vrtxdpth' attribute) are reset to zero.

  ! REFERENCES:

  ! Kurihara, Y., M. A. Bender, and R. J. Ross, 1993: An
  ! initialization scheme of hurricane models by vortex
  ! specification. Mon. Wea. Rev., 121, 2030–2045.

  ! Kurihara, Y., M. A. Bender, R. E. Tuleya, and R. J. Ross, 1995:
  ! Improvements in the GFDL Hurricane Prediction
  ! System. Mon. Wea. Rev., 123, 2791–2801.

  ! Winterbottom, H. R., and E. P. Chassignet, 2011: A vortex
  ! isolation and removal algorithm for numerical weather prediction
  ! model tropical cyclone applications, J. Adv. Model. Earth Syst.,
  ! 3, M11003.

  ! INPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable containing the
  !   computed size attributes for the respective TC vortex.

  ! * filter; a FORTRAN filter_struct variable containing (at minimum)
  !   the radial distance and azimuths relative the position of the
  !   respective TC vortex.

  ! OUTPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable containing the mass and
  !   momentum filter regions via the 'tfltr' and 'kfltr' attributes,
  !   respectively.

  !-----------------------------------------------------------------------

  subroutine define_filter(vrtxinfo,filter)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(subregion_struct)                                              :: subregion
    real(r_kind),               dimension(:,:),             allocatable :: mpi_kfltr
    real(r_kind),               dimension(:,:),             allocatable :: mpi_tfltr
    real(r_kind)                                                        :: angle
    real(r_kind)                                                        :: fltrval
    real(r_kind)                                                        :: mx_radius
    real(r_kind)                                                        :: radius
    integer                                                             :: idx
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_kfltr))                                          &
         & allocate(mpi_kfltr(filter%ncoords,filter%nz))
    if(.not. allocated(mpi_tfltr))                                          &
         & allocate(mpi_tfltr(filter%ncoords,filter%nz))

    ! Define local variables

    filter%kfltr = 0.0
    filter%tfltr = 0.0
    mpi_kfltr    = 0.0
    mpi_tfltr    = 0.0

    ! Loop through local variable

    do j = 1, filter%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,j,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Define local variables

          filterdiag%maxradius = vrtxinfo%vrtxsz
          where(filter%radius(:,mpi_lev) .le. filterdiag%mn_dist)           &
               & mpi_kfltr(:,mpi_lev) = 1.0
          mpi_tfltr(:,mpi_lev) = mpi_kfltr(:,mpi_lev)

          ! Loop through local variable
       
          do i = 1, filter%ncoords
             
             ! Define local variables
       
             if(filter%tngwnd(i,mpi_lev) .ge. filterdiag%twndthrsh) then
             
                ! Define local variables

                mpi_kfltr(i,mpi_lev) = 1.0
                filterdiag%maxradius = max(filterdiag%maxradius,            &
                     & filter%radius(i,mpi_lev))

             end if ! if(filter%tngwnd(idx,mpi_lev)
                    ! .ge. filterdiag%twndthrsh)

          end do ! do i = 1, subregion%ncoords

          ! Define local variables

          call filter_subregion(filter%radius(:,mpi_lev),subregion)
          angle = 0.0

          ! Loop through local variable

          do while(angle .lt. ((2.0*pi*rad2deg) + filterdiag%dangle))

             ! Define local variables

             radius = filterdiag%mn_dist

             ! Loop through local variable

             do while(radius .le. filterdiag%maxradius)
             
                ! Define local variables

                fltrval   = 0.0
                mx_radius = 0.0

                ! Loop through local variable

                do i = 1, subregion%ncoords

                   ! Define local variables

                   idx = subregion%idx(i)
                
                   ! Check local variable and proceed accordingly

                   if((filter%angle(idx,mpi_lev) .ge. angle) .and.          &
                        & (filter%angle(idx,mpi_lev) .lt. (angle +          &
                        & filterdiag%dangle)) .and.                         &
                        & (filter%radius(idx,mpi_lev) .ge. radius) .and.    &
                        & (filter%radius(idx,mpi_lev) .lt. (radius +        &
                        & filterdiag%dradius))) then

                      ! Define local variables
                   
                      fltrval   = max(fltrval,mpi_kfltr(idx,mpi_lev))
                      mx_radius = max(mx_radius,radius)
                   
                   end if ! if((filter%angle(idx,mpi_lev) .ge. angle)
                          ! .and. (filter%angle(idx,mpi_lev)
                          ! .lt. (angle + filterdiag%dangle))
                          ! .and. (filter%radius(idx,mpi_lev)
                          ! .le. radius)
                          ! .and. (filter%radius(idx,mpi_lev)
                          ! .gt. (radius + filterdiag%dradius)))

                end do ! do i = 1, subregion%ncoords

                ! Check local variable and proceed accordingly

                if(fltrval .gt. 0.0) then

                   ! Loop through local variable

                   do i = 1, subregion%ncoords

                      ! Define local variables

                      idx = subregion%idx(i)

                      ! Check local variable and proceed accordingly

                      if((filter%angle(idx,mpi_lev) .ge. angle) .and.       &
                           & (filter%angle(idx,mpi_lev) .lt. (angle +       &
                           & filterdiag%dangle)) .and.                      &
                           & (filter%radius(idx,mpi_lev) .le. mx_radius))   &
                           & then

                         ! Define local variables

                         mpi_kfltr(idx,mpi_lev) = 1.0

                      end if ! if((filter%angle(idx,mpi_lev)
                             ! .ge. angle)
                             ! .and. (filter%angle(idx,mpi_lev)
                             ! .lt. (angle + filterdiag%dangle))
                             ! .and. (filter%radius(idx,mpi_lev)
                             ! .le. mx_radius)

                   end do ! do i = 1, subregion%ncoords

                end if ! if(fltrval .gt. 0.0)

                ! Define local variables
             
                radius = radius + filterdiag%dradius

             end do ! do while(radius .le. filterdiag%maxradius)

             ! Define local variables

             angle = angle + filterdiag%dangle

          end do ! do while(angle .lt. ((2.0*pi*rad2deg) +
                 ! filterdiag%dangle))

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(subregion)

          ! Loop through local variable

          do i = 1, filter%ncoords

             ! Check local variable and proceed accordingly
        
             if(filter%radius(i,mpi_lev) .le. (vrtxinfo%roci*               &
                  & filterdiag%tinfltfac)) then
             
                ! Define local variables
             
                mpi_tfltr(i,mpi_lev) = 1.0
             
             end if ! if(filter%radius(idx,mpi_lev)
                    ! .le. (vrtxinfo%roci*filterdiag%tinfltfac))

          end do ! do i = 1, filter%ncoords

          ! Define local variables

          if(filter%plev(mpi_lev) .lt. filterdiag%ptop) then

             ! Define local variables

             mpi_kfltr(:,mpi_lev) = 0.0
             mpi_tfltr(:,mpi_lev) = 0.0

          end if ! if(filter%plev(mpi) .lt. filterdiag%ptop)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do j = 1, filter%nz

    ! Define local variables

    call mpi_interface_waitall()
    call mpi_reduce(mpi_kfltr,filter%kfltr,(filter%ncoords*filter%nz),     &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_tfltr,filter%tfltr,(filter%ncoords*filter%nz),     &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_kfltr)) deallocate(mpi_kfltr)
    if(allocated(mpi_tfltr)) deallocate(mpi_tfltr)

    ! Define local variables

    call mpi_bcast(filter%kfltr,(filter%ncoords*filter%nz),mpi_real,       &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(filter%tfltr,(filter%ncoords*filter%nz),mpi_real,       &
         & mpi_masternode,mpi_comm_world,mpi_ierror)

    !=====================================================================

  end subroutine define_filter

  !=======================================================================

  ! SUBROUTINE:

  ! filter_interface_define.f90

  ! DESCRIPTION:

  ! This subroutine is the interface level to define the filtering
  ! regions for the mass and momentum variables relative to the TC
  ! vortex attributes.

  ! INPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable containing the
  !   respective TC vortex attributes; these attributes define the
  !   values used to specify the respective TC filter.

  ! OUTPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable containing the mass and
  !   momentum variable filter attributes relative to the specified TC
  !   vortex attributes.

  !-----------------------------------------------------------------------

  subroutine filter_interface_define(filter,meteo,vrtxinfo)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Check local variable and proceed accordingly

       if(mpi_procid .eq. mpi_masternode) then

          ! Define local variables

          write(6,500) vrtxinfo%id

       end if ! if(mpi_procid .eq. mpi_masternode)

       ! Define local variables

       call mpi_interface_waitall()

    end if ! if(debug)    

    ! Define local variables

    call filterdiag_init(vrtxinfo)
    grid%nx  = meteo%nx
    grid%ny  = meteo%ny
    call variable_interface_setup_struct(grid)
    grid%lat = meteo%xlat
    grid%lon = meteo%xlon

    ! Compute local variables

    call grid_methods_polarcoords(grid,vrtxinfo%reflon,vrtxinfo%reflat)    
    call centroid(grid,meteo,vrtxinfo,filter)    
    call radial_wind(meteo,vrtxinfo,filter)
    call define_filter(vrtxinfo,filter)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    ! Define local variables

500 format('FILTER_INTERFACE_DEFINE: Defining filter for TC ', a4, '.')

    !=====================================================================

  end subroutine filter_interface_define

  !=======================================================================

  ! SUBROUTINE:

  ! filterdiag_init.f90

  ! DESCRIPTION:

  ! This subroutine initializes the values for the FORTRAN
  ! filterdiag_struct variable; the user specified namelist variables
  ! are used to assign the filterdiag_struct variable attribute
  ! values.

  ! INPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable containing the
  !   respective TC vortex attributes; these attributes define the
  !   values used to specify the respective TC filter.

  !-----------------------------------------------------------------------

  subroutine filterdiag_init(vrtxinfo)

    ! Define variables passed to routine

    type(vrtxinfo_struct)                                               :: vrtxinfo

    !=====================================================================

    ! Define local variables

    filterdiag%dangle    = filter_dangle
    filterdiag%dradius   = filter_dradius
    filterdiag%dwndthrsh = filter_dwndthrsh
    filterdiag%kinfltfac = filter_kinfltfac
    filterdiag%maxradius = vrtxinfo%vrtxsz
    filterdiag%mn_dist   = (filter_kinfltfac*vrtxinfo%rmw)
    filterdiag%ptop      = vrtxinfo%vrtxdpth
    filterdiag%tinfltfac = filter_tinfltfac
    filterdiag%twndthrsh = filter_twndthrsh
    filterdiag%wndthrsh  = filter_wndthrsh

    !=====================================================================

  end subroutine filterdiag_init

  !=======================================================================

  ! SUBROUTINE:

  ! filter_subregion.f90

  ! DESCRIPTION:

  ! This subroutine defines the indices of the computational grid
  ! which are within the maximum radius of the filter region in
  ! accordance with the user specifications; the intention of this
  ! subroutine is to improve the overall efficience of the filter
  ! determination (e.g., speed up performance).

  ! INPUT VARIABLES:

  ! * radius; a FORTRAN 4-byte real-valued array containing the radial
  !   distances, relative to a geographical location.

  ! * subregion; a FORTRAN subregion_struct variable.

  ! OUTPUT VARIABLES:

  ! * subregion; a FORTRAN subregion_struct variable containing the
  !   grid indices (within the idx attribute) for all computational
  !   grid locations within the user specified filter region; the
  !   total number of values within the aforementioned filter region
  !   is defined by the 'ncoords' attribute.

  !-----------------------------------------------------------------------

  subroutine filter_subregion(radius,subregion)

    ! Define variables passed to routine

    type(subregion_struct)                                              :: subregion
    real(r_kind)                                                        :: radius(:)

    ! Define variables computed within routine

    integer                                                             :: count

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    subregion%ncoords = 0

    ! Loop though local variable

    do i = 1, size(radius)

       ! Check local variable and proceed accordingly

       if(radius(i) .le. filterdiag%maxradius) then

          ! Define local variables

          subregion%ncoords = subregion%ncoords + 1
          
       end if ! if(radius(i) .le. filterdiag%maxradius)

    end do ! do i = 1, size(radius)

    ! Define local variables

    call variable_interface_setup_struct(subregion)
    count = 0

    ! Loop though local variable

    do i = 1, size(radius)

       ! Check local variable and proceed accordingly

       if(radius(i) .le. filterdiag%maxradius) then

          ! Define local variables

          count                = count + 1
          subregion%idx(count) = i

       end if ! if(radius(i) .le. filterdiag%maxradius)

    end do ! do i = 1, size(radius)

    !=====================================================================

  end subroutine filter_subregion

  !=======================================================================

  ! SUBROUTINE:

  ! radial_wind.f90

  ! DESCRIPTION:
  
  ! This subroutine estimates the tangential wind using Fast-Fourier
  ! transform methods.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! * filter; a FORTRAN filter_struct variable; it is assumed that all
  !   array and attributes have been initialized prior to calling this
  !   subroutine.

  ! OUTPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable containing the
  !   tangential wind (tngwnd attribute) and the polar-coordinate
  !   projection (the angle and radius attributes) variables.

  !-----------------------------------------------------------------------

  subroutine radial_wind(meteo,vrtxinfo,filter)

    ! Define variables passed to subroutine

    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    real(r_kind),               dimension(:,:),             allocatable :: mpi_angle
    real(r_kind),               dimension(:,:),             allocatable :: mpi_radius
    real(r_kind),               dimension(:,:),             allocatable :: mpi_tngwnd  
    real(r_kind),               dimension(:),               allocatable :: uwnd
    real(r_kind),               dimension(:),               allocatable :: vwnd
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_angle))                                         &
         & allocate(mpi_angle(meteo%ncoords,meteo%nz))
    if(.not. allocated(mpi_radius))                                        &
         & allocate(mpi_radius(meteo%ncoords,meteo%nz))
    if(.not. allocated(mpi_tngwnd))                                        &
         & allocate(mpi_tngwnd(meteo%ncoords,meteo%nz))
    if(.not. allocated(uwnd))                                              &
         & allocate(uwnd(meteo%ncoords))
    if(.not. allocated(vwnd))                                              &
         & allocate(vwnd(meteo%ncoords))

    ! Define local variables

    grid%nx      = meteo%nx
    grid%ny      = meteo%ny
    call variable_interface_setup_struct(grid)
    grid%lat     = meteo%xlat
    grid%lon     = meteo%xlon
    meteo%tngwnd = 0.0
    mpi_angle    = 0.0
    mpi_radius   = 0.0
    mpi_tngwnd   = 0.0

    ! Loop through local variable

    do j = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,j,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Compute local variables

          call grid_methods_polarcoords(grid,filter%clon(mpi_lev),         &
               & filter%clat(mpi_lev))

          ! Define local variables
          
          mpi_angle(:,mpi_lev)  = grid%angle
          mpi_radius(:,mpi_lev) = grid%radius

          ! Loop through local variable

          do i = 1, meteo%ncoords

             ! Check local variable and proceed accordingly

             if((meteo%u(i,mpi_lev) .ne. spval) .and. (meteo%v(i,mpi_lev)  &
                  & .ne. spval)) then

                ! Compute local variables

                uwnd(i)               = -1.0*(meteo%u(i,mpi_lev)*          &
                     & cos(grid%angle(i)*deg2rad))
                vwnd(i)               = meteo%v(i,mpi_lev)*                &
                     & sin(grid%angle(i)*deg2rad)
                mpi_tngwnd(i,mpi_lev) = sqrt(uwnd(i)*uwnd(i) +             &
                     & vwnd(i)*vwnd(i))

             else   ! if((meteo%u(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%v(i,mpi_lev) .ne. spval))

                ! Define local variables

                mpi_tngwnd(i,mpi_lev) = spval

             end if ! if((meteo%u(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%v(i,mpi_lev) .ne. spval))

          end do ! do i = 1, meteo%ncoords
          
       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do j = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_angle,filter%angle,(filter%ncoords*filter%nz),     &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_radius,filter%radius,(filter%ncoords*filter%nz),   &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_tngwnd,meteo%tngwnd,(meteo%ncoords*meteo%nz),      &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_angle))  deallocate(mpi_angle)
    if(allocated(mpi_radius)) deallocate(mpi_radius)
    if(allocated(mpi_tngwnd)) deallocate(mpi_tngwnd)
    if(allocated(uwnd))       deallocate(uwnd)
    if(allocated(vwnd))       deallocate(vwnd)
    call variable_interface_cleanup_struct(grid)

    ! Define local variables

    call mpi_bcast(filter%angle,(filter%ncoords*filter%nz),mpi_real,       &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(filter%radius,(filter%ncoords*filter%nz),mpi_real,      &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%tngwnd,(meteo%ncoords*meteo%nz),mpi_real,         &
         & mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Compute local variables

    call tc_model(meteo,vrtxinfo,filter)

    !=====================================================================

  end subroutine radial_wind

  !=======================================================================

end module filter_interface
