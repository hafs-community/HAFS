module tcdiags_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: tcdiags_methods_interface
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

  use analysis_interface
  use fileio_interface
  use filter_interface
  use grid_methods_interface
  use interpolation_interface
  use kinds_interface
  use math_methods_interface
  use meteo_methods_interface
  use mpi_interface
  use namelist_interface
  use tropcyc_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: tcdiags_methods_tccps
  public :: tcdiags_methods_tcenv
  public :: tcdiags_methods_tcmpi
  public :: tcdiags_methods_tcmsi

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! check_tcv.f90

  ! DESCRIPTION:

  ! This subroutine ingests a FORTRAN tcv_struct variable and computes
  ! the radial distance relative to the reference geographical
  ! location; the mean sea-level pressure (MSLP) values from the
  ! ingested FORTRAN meteo_struct variable are then evaluated within
  ! the user specified threshold radius; if any value is equal to the
  ! spval (missing data) attribute, the FORTRAN logical variable
  ! tcv_pass is set to .false.; this check is performed only for
  ! regional model applications.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable containing all tropical
  !   cyclone attributes (obtained from the NCEP tracker forecast
  !   files) for the respective TCV event.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * radius_threshold; a FORTRAN 4-byte valued variable containing
  !   the threshold radial distance within which to search for missing
  !   data values.

  ! * tcv_pass; a FORTRAN logical variable.
  
  ! OUTPUT VARIABLES:

  ! * tcv_pass; a FORTRAN logical variable; if .true., this implies
  !   that either a global model application is being executed (e.g.,
  !   is_glbmdl = .true.) or there are no missing data values (e.g.,
  !   spval attribute values) within the threshold radial distance
  !   specified by the user; if .false., this implies that missing
  !   data values where located within the threshold radial distance
  !   specified by the user (for regional model, is_rgnlmdl = .true.,
  !   applications only).

  !-----------------------------------------------------------------------

  subroutine check_tcv(tcv,meteo,radius_threshold,tcv_pass)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    type(tcv_struct)                                                    :: tcv
    logical                                                             :: tcv_pass
    real(r_kind)                                                        :: radius_threshold
    
    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    integer                                                             :: nspval

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Define local variables

    tcv_pass = .false.
    
    ! Check local variable and proceed accordingly

    if(is_glblmdl) tcv_pass = .true.

    ! Check local variable and proceed accordingly

    if(is_rgnlmdl) then

       ! Define local variables

       grid%nx  = meteo%nx
       grid%ny  = meteo%ny
       call variable_interface_setup_struct(grid)
       grid%lat = meteo%xlat
       grid%lon = meteo%xlon
       
       ! Compute local variables

       call grid_methods_polarcoords(grid,tcv%reflon,tcv%reflat)

       ! Define local variables

       nspval = 0

       ! Loop through local variable

       do i = 1, grid%ncoords

          ! Check local variable and proceed accordingly

          if(grid%radius(i) .le. radius_threshold) then

             ! Check local variable and proceed accordingly

             if(meteo%pmsl(i) .eq. spval) then

                ! Define local variables

                nspval = nspval + 1

             end if ! if(meteo%pmsl(i) .eq. spval)

          end if ! if(grid%radius(i) .le. radius_threshold)

       end do ! do i = 1, grid%ncoords
       
       ! Check local variable and proceed accordingly

       if(nspval .eq. 0) tcv_pass = .true.
       
       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(grid)
       
    end if ! if(is_rgnlmdl)

    !=====================================================================
    
  end subroutine check_tcv
  
  !=======================================================================

  ! SUBROUTINE:

  ! tcdiags_methods_tccps.f90

  ! DESCRIPTION:

  ! This subroutine computes the tropical cyclone (TC) cyclone phase
  ! space (CPS) lower- and upper-troposphere thermal structure
  ! parameters described by Hart [2003].

  ! REFERENCES:

  ! Hart, R. E., 2003: A cyclone phase space derived from thermal wind
  ! and thermal asymmetry. Mon. Wea. Rev., 131, 585-616.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable array containing all viable
  !   tropical cyclone attributes (obtained from the NCEP tracker
  !   forecast files).

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * tccps; a FORTRAN tccps_struct variable.

  ! OUTPUT VARIABLES:

  ! * tccps; a FORTRAN tccps_struct variable containing the input
  !   variables to the cyclone phase algorithm as well as the output
  !   variables.

  !-----------------------------------------------------------------------

  subroutine tcdiags_methods_tccps(tcv,meteo,tccps)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    type(tccps_struct)                                                  :: tccps
    type(tcv_struct)                                                    :: tcv

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Check local variable and proceed accordingly

       if(mpi_procid .eq. mpi_masternode) then

          ! Define local variables

          write(6,500) tcv%tcid

       end if ! if(mpi_procid .eq. mpi_masternode)

       ! Define local variables

       call mpi_interface_waitall()

    end if ! if(debug)

    ! Define local variables

    tccps%nx             = meteo%nx
    tccps%ny             = meteo%ny
    tccps%nz             = meteo%nz
    tccps%clat           = tcv%reflat
    tccps%clon           = tcv%reflon
    call variable_interface_setup_struct(tccps)
    tccps%lats           = meteo%xlat
    tccps%lons           = meteo%xlon
    tccps%p              = meteo%p
    tccps%z              = meteo%z
    mpi_taskgrid%ncoords = (meteo%nx*meteo%ny)
    mpi_taskgrid%nlevs   = meteo%nz
    call variable_interface_setup_struct(mpi_taskgrid)
    call mpi_interface_partition(debug)

    ! Compute local variables

    call meteo_methods_cps_thermal(meteo,tccps)

    ! Define local variables

    call mpi_interface_waitall()

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(mpi_taskgrid)

    ! Define local variables

500 format('TCDIAGS_METHODS_TCCPS: Computing the thermal structures for '  &
         & 'TC ', a, '.')

    !=====================================================================

  end subroutine tcdiags_methods_tccps

  !=======================================================================

  ! SUBROUTINE:

  ! tcdiags_methods_tcenv.f90

  ! DESCRIPTION:

  ! This subroutine estimates the environment within which TC events
  ! are embedded as described (loosely) in Winterbottom and Chassignet
  ! [2011]; two netcdf files are written by this routine:

  ! tcenv.<analdate>.analysis.nc: This file contains the state and
  ! diagnostic variables prior to the application of the TC filtering
  ! algorithm.

  ! tcenv.<analdate>.filtered.nc: This file contains the state and
  ! diagnostic variables after the application of the TC filtering
  ! algorithm.
  
  ! REFERENCES:

  ! Winterbottom, H. R., and E. P. Chassignet, 2011: A vortex
  ! isolation and removal algorithm for numerical weather prediction
  ! model tropical cyclone applications, J. Adv. Model. Earth Syst.,
  ! 3, M11003.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable array containing all viable
  !   tropical cyclone attributes (obtained from the NCEP tracker
  !   forecast files).

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * tcenv; a FORTRAN tcevn_struct variable.

  ! OUTPUT VARIABLES:

  ! * tcenv; a FORTRAN tcenv_struct variable containing the diagnostic
  !   variables computed for the respective TC environment(s).

  !-----------------------------------------------------------------------

  subroutine tcdiags_methods_tcenv(tcv,meteo,tcenv)

    ! Define variables passed to routine

    type(tcv_struct)                                                    :: tcv(:)
    type(meteo_struct)                                                  :: meteo
    type(tcenv_struct)                                                  :: tcenv
    
    ! Define variables computed within routine

    type(vrtxinfo_struct),      dimension(:),               allocatable :: vrtxinfo
    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo_env
    character(len=500)                                                  :: filename
    logical                                                             :: tcv_pass
    real(r_kind)                                                        :: radius_threshold

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Check local variable and proceed accordingly

       if(mpi_procid .eq. mpi_masternode) then

          ! Define local variables

          write(6,500)

       end if ! if(mpi_procid .eq. mpi_masternode)

       ! Define local variables

       call mpi_interface_waitall()

    end if ! if(debug)
    
    ! Define local variables

    mpi_taskgrid%ncoords = (meteo%nx*meteo%ny)
    mpi_taskgrid%nlevs   = meteo%nz
    call variable_interface_setup_struct(mpi_taskgrid)
    call mpi_interface_partition(debug)
    tcenv%valid_time     = analdate
    tcenv%nx             = meteo%nx
    tcenv%ny             = meteo%ny
    call variable_interface_setup_struct(tcenv)
    tcenv%lats           = meteo%xlat
    tcenv%lons           = meteo%xlon
    filename             = trim(adjustl(datapath))//'tcenv.'//analdate//   &
         & '.analysis.nc'
    call fileio_interface_write(filename,meteo)

    ! Check local variable and proceed accordingly

    if(is_filter_tcs) then
    
       ! Allocate memory for local variables

       if(.not. allocated(vrtxinfo)) allocate(vrtxinfo(size(tcv)))

       ! Compute local variables

       call tropcyc_methods_attrs(meteo,vrtxinfo,tcv)
       call meteo_methods_lyrmnwnds(meteo,tcenv)

       ! Define local variables

       tcenv%u850_200 = tcenv%u850_200_shear
       tcenv%v850_200 = tcenv%v850_200_shear
       filter%nx      = meteo%nx
       filter%ny      = meteo%ny
       filter%nz      = meteo%nz
       call variable_interface_setup_struct(filter)
       filter%lat     = meteo%xlat
       filter%lon     = meteo%xlon
       filter%plev    = meteo%p(1,:)
       meteo_env      = meteo
    
       ! Loop through local variable

       do i = 1, size(tcv)

          ! Define local variables

          radius_threshold = filter_maxradius*(max(filter_kinfltfac,       &
               & filter_tinfltfac))
          call check_tcv(tcv(i),meteo,radius_threshold,tcv_pass)
          
          ! Check local variable and proceed accordingly

          if(vrtxinfo(i)%vmax .lt. tc_filter_vmin_mps) tcv_pass = .false.

          ! Check local variable and proceed accordingly

          if(tcv_pass) then

             ! Define local variables
       
             tcenv%tcid(i)  = tcv(i)%tcid
             tcenv%tclat(i) = tcv(i)%reflat
             tcenv%tclon(i) = tcv(i)%reflon
       
             ! Compute local variables
          
             call filter_interface_define(filter,meteo_env,vrtxinfo(i))

             ! Define local variables

             call fileio_interface_write(tcv(i),filter)

             ! Compute local variables

             call analysis(meteo_env,filter)

          end if ! if(tcv_pass)

       end do ! do i = 1, size(tcv)

       ! Define local variables

       meteo_env%u = spval
       meteo_env%v = spval
    
       ! Compute local variables

       call meteo_methods_winds(meteo_env)
       call meteo_methods_diagnostics(meteo_env)
       call meteo_methods_lyrmnwnds(meteo_env,tcenv)
    
       ! Define local variables
    
       filename = trim(adjustl(datapath))//'tcenv.'//analdate//            &
            & '.filtered.nc'
       call fileio_interface_write(filename,meteo_env)

       ! Deallocate memory for local variables

       if(allocated(vrtxinfo)) deallocate(vrtxinfo)
       call variable_interface_cleanup_struct(filter)
       call variable_interface_cleanup_struct(meteo_env)
       
    end if ! if(is_filter_tcs)
       
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(mpi_taskgrid)

    ! Define local variables
    
500 format('TCDIAGS_METHODS_TCENV: Estimating the TC environment.')
   
    !=====================================================================

  end subroutine tcdiags_methods_tcenv
  
  !=======================================================================

  ! SUBROUTINE:

  ! tcdiags_methods_tcmpi.f90

  ! DESCRIPTION:

  ! This subroutine computes the tropical cyclone (TC) maximum
  ! potential intensity (MPI) as described by Emanuel [1988] and
  ! writes an external netcdf file (tcmpi.nc) containing the
  ! respective TC MPI metrics.

  ! REFERENCES:

  ! Emanuel, K. A., 1988: The Maximum Intensity of
  ! Hurricanes. J. Atmos. Sci., 45, 1143–1155.

  ! NOTE:

  ! * All input variables to the PCMIN subroutine are rescaled to
  !   their original MKS units prior to exiting this subroutine.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable array containing all viable
  !   tropical cyclone attributes (obtained from the NCEP tracker
  !   forecast files).

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * tcmpi; a FORTRAN tcmpi_struct variable.

  ! OUTPUT VARIABLES:

  ! * tcmpi; a FORTRAN tcmpi_struct variable containing the input
  !   variables to the maximum potential intensity algorithm as well
  !   as the output variables.

  !-----------------------------------------------------------------------

  subroutine tcdiags_methods_tcmpi(tcv,meteo,tcmpi)

    ! Define variables passed to routine

    type(tcv_struct)                                                    :: tcv(:)
    type(meteo_struct)                                                  :: meteo
    type(tcmpi_struct)                                                  :: tcmpi

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    logical                                                             :: tcv_pass
    real(r_kind),               dimension(:),               allocatable :: mask
    real(r_kind),               dimension(:),               allocatable :: mpi_vmax
    real(r_kind),               dimension(:),               allocatable :: mpi_pmin
    real(r_kind)                                                        :: radius_threshold
    integer,                    dimension(:),               allocatable :: mpi_ifl
    integer                                                             :: mpi_id

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Check local variable and proceed accordingly

       if(mpi_procid .eq. mpi_masternode) then

          ! Define local variables

          write(6,500)

       end if ! if(mpi_procid .eq. mpi_masternode)

       ! Define local variables

       call mpi_interface_waitall()

    end if ! if(debug)

    ! Define local variables

    tcmpi%valid_time     = analdate
    tcmpi%nx             = meteo%nx
    tcmpi%ny             = meteo%ny
    tcmpi%nz             = meteo%nz
    call variable_interface_setup_struct(tcmpi)
    tcmpi%sst            = spval
    tcmpi%mxrt           = spval
    tcmpi%p              = spval
    tcmpi%t              = spval
    tcmpi%lats           = meteo%xlat
    tcmpi%lons           = meteo%xlon
    where(meteo%tsfc .ne. spval)   tcmpi%sst  = meteo%tsfc - 273.16
    tcmpi%pmsl           = meteo%pmsl/100.0
    where(meteo%wvmxrt .ne. spval) tcmpi%mxrt = meteo%wvmxrt*1000.0
    where(meteo%p .ne. spval)      tcmpi%p    = meteo%p/100.0
    where(meteo%t .ne. spval)      tcmpi%t    = meteo%t - 273.16
    grid%nx              = meteo%nx
    grid%ny              = meteo%ny
    call variable_interface_setup_struct(grid)
    grid%lat             = meteo%xlat
    grid%lon             = meteo%xlon
    mpi_taskgrid%ncoords = (meteo%nx*meteo%ny)
    mpi_taskgrid%nlevs   = meteo%nz
    mpi_id               = mpi_procid + 1
    call variable_interface_setup_struct(mpi_taskgrid)
    call mpi_interface_partition(debug)

    ! Allocate memory for local variable

    if(.not. allocated(mask)) allocate(mask(meteo%ncoords))

    ! Define local variables

    mask = meteo%land
    where(meteo%pmsl .eq. spval) mask = 1.0

    ! Loop through local variable

    do i = 1, size(tcv)
       
       ! Define local variables

       radius_threshold = tcmpi_maskout_radius
       call check_tcv(tcv(i),meteo,radius_threshold,tcv_pass)

       ! Check local variable and proceed accordingly

       if(tcv_pass) then

          ! Define local variables
          
          tcmpi%tcid(i)  = tcv(i)%tcid
          tcmpi%tclat(i) = tcv(i)%reflat
          tcmpi%tclon(i) = tcv(i)%reflon

          ! Compute local variables

          call grid_methods_radialgrid(grid,tcv(i)%reflon,tcv(i)%reflat)

          ! Define local variables

          where(grid%radius .le. tcmpi_maskout_radius) mask = 1.0

       end if ! if(tcv_pass)
          
    end do ! do i = 1, size(tcv)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    ! Allocate memory for local variables

    if(.not. allocated(mpi_pmin)) allocate(mpi_pmin(mpi_taskgrid%ncoords))
    if(.not. allocated(mpi_vmax)) allocate(mpi_vmax(mpi_taskgrid%ncoords))
    if(.not. allocated(mpi_ifl))  allocate(mpi_ifl(mpi_taskgrid%ncoords))

    ! Define local variables

    mpi_pmin = 0.0
    mpi_vmax = 0.0
    mpi_ifl  = 0

    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

       ! Check local variable and proceed accordingly

       if(i .ne. mpi_noproc_assign) then

          ! Check local variable and proceed accordingly

          if(mask(i) .ne. 1.0) then

             ! Compute local variables

             call pcmin(tcmpi%sst(i),tcmpi%pmsl(i),tcmpi%p(i,:),           &
                  & tcmpi%t(i,:),tcmpi%mxrt(i,:),tcmpi%nz,tcmpi%nz,        &
                  & mpi_pmin(i),mpi_vmax(i),mpi_ifl(i))

          else   ! if(mask(i) .ne. 1.0)

             ! Define local variables

             mpi_pmin(i) = spval
             mpi_vmax(i) = spval
             mpi_ifl(i)  = -99

          end if ! if(mask(i) .ne. 1.0)

       end if ! if(i .ne. mpi_noproc_assign)

    end do ! do i = mpi_taskgrid%begin(mpi_id),
           ! mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_pmin,tcmpi%pmin,mpi_taskgrid%ncoords,mpi_real,     &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_vmax,tcmpi%vmax,mpi_taskgrid%ncoords,mpi_real,     &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_ifl,tcmpi%ifl,mpi_taskgrid%ncoords,mpi_integer,    &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mask))     deallocate(mask)
    if(allocated(mpi_vmax)) deallocate(mpi_vmax)
    if(allocated(mpi_pmin)) deallocate(mpi_pmin)
    if(allocated(mpi_ifl))  deallocate(mpi_ifl)

    ! Define local variables

    call mpi_bcast(tcmpi%pmin,mpi_taskgrid%ncoords,mpi_real,               &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(tcmpi%vmax,mpi_taskgrid%ncoords,mpi_real,               &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(tcmpi%ifl,mpi_taskgrid%ncoords,mpi_integer,             &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    tcmpi%mxrt = meteo%wvmxrt
    tcmpi%pmsl = meteo%pmsl
    tcmpi%p    = meteo%p
    tcmpi%sst  = meteo%tsfc
    tcmpi%t    = meteo%t

    ! Check local variable and proceed accordingly

    if(debug .and. (mpi_procid .eq. mpi_masternode)) then

       ! Define local variables

       write(6,501) minval(tcmpi%pmin,tcmpi%pmin .ne. spval),              &
            & maxval(tcmpi%pmin,tcmpi%pmin .ne. spval)
       write(6,502) minval(tcmpi%vmax,tcmpi%vmax .ne. spval),              &
            & maxval(tcmpi%vmax,tcmpi%vmax .ne. spval)

    end if ! if(debug .and. (mpi_procid .eq. mpi_masternode))

    ! Define local variables

    call mpi_interface_waitall()
    where(tcmpi%vmax .eq. spval) tcmpi%sst  = spval
    where(tcmpi%vmax .eq. spval) tcmpi%pmsl = spval

    ! Loop through local variable

    do i = 1, tcmpi%nz

       ! Define local variables

       where(tcmpi%vmax .eq. spval) tcmpi%mxrt(:,i) = spval
       where(tcmpi%vmax .eq. spval) tcmpi%p(:,i)    = spval
       where(tcmpi%vmax .eq. spval) tcmpi%t(:,i)    = spval
       
    end do ! do i = 1, tcmpi%nz
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(mpi_taskgrid)

    ! Define local variables

500 format('TCDIAGS_METHODS_TCMPI: Computing the TC maximum potential ',   &
         & 'intensity.')
501 format('TCDIAGS_METHODS_TCMPI minimum pressure (min/max; hPa): ',      &
         & 2(f13.5))
502 format('TCDIAGS_METHODS_TCMPI maximum wind speed (min/max; m/s): ',    &
         & 2(f13.5))

    !=====================================================================

  end subroutine tcdiags_methods_tcmpi

  !=======================================================================

  ! SUBROUTINE:

  ! tcdiags_methods_tcmsi.f90

  ! DESCRIPTION:

  ! This subroutine computes the tropical cyclone (TC) multi-scale
  ! intensity (MSI) index values.

  ! REFERENCES:

  ! Vukicevic, T., E. Uhlhorn, P. Reasor, and B. Klotz, 2014: A Novel
  ! Multiscale Intensity Metric for Evaluation of Tropical Cyclone
  ! Intensity Forecasts. J. Atmos. Sci., 71, 1292–1304.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable containing, at minimum, the
  !   reference geographical location for the respective TC events.

  ! * meteo; a FORTRAN meteo_struct variable containing, at minimum,
  !   the 10-meter wind vector components.

  ! * tcmsi; a FORTRAN tcmsi_struct variable.

  ! * tcv_pass; a FORTRAN logical variable.

  ! OUTPUT VARIABLES:

  ! * tcmsi; a FORTRAN tcmsi_struct variable containing the tropical
  !   cyclone (TC) multi-scale intensity (MSI) index values and
  !   related diagnostics values.

  ! * tcv_pass; a FORTRAN logical variable; if .true., this implies
  !   that either a global model application is being executed (e.g.,
  !   is_glbmdl = .true.) or there are no missing data values (e.g.,
  !   spval attribute values) within the threshold radial distance
  !   specified by the user; if .false., this implies that missing
  !   data values where located within the threshold radial distance
  !   specified by the user (for regional model, is_rgnlmdl = .true.,
  !   applications only).
  
  !-----------------------------------------------------------------------

  subroutine tcdiags_methods_tcmsi(tcv,meteo,tcmsi,tcv_pass)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    type(tcmsi_struct)                                                  :: tcmsi
    type(tcv_struct)                                                    :: tcv
    logical                                                             :: tcv_pass
    
    ! Define variables computed within routine

    type(vrtxinfo_struct),      dimension(:),               allocatable :: vrtxinfo
    type(tcv_struct)                                                    :: tcv_local(1)
    type(grid_struct)                                                   :: grid
    type(grid_struct)                                                   :: wndgrid
    type(recenter_struct)                                               :: recenter
    type(slint_struct)                                                  :: remap
    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    type(wnd2d_struct)                                                  :: wnd2d
    real(r_kind),               dimension(:),               allocatable :: wndvar
    real(r_kind)                                                        :: radius_threshold
    real(r_kind)                                                        :: dist
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables
    
    radius_threshold = 0.0
    
    ! Check local variable and proceed accordingly

    if(is_rgnlmdl) then

       ! Compute local variables

       call grid_methods_radialdist(tcv%reflon,tcv%reflat,                 &
            & (tcv%reflon+tcmsi_area),tcv%reflat,dist)

       ! Define local variables

       radius_threshold = max(radius_threshold,dist)

       ! Compute local variables

       call grid_methods_radialdist(tcv%reflon,tcv%reflat,                 &
            & (tcv%reflon-tcmsi_area),tcv%reflat,dist)

       ! Define local variables

       radius_threshold = max(radius_threshold,dist)

       ! Compute local variables

       call grid_methods_radialdist(tcv%reflon,tcv%reflat,tcv%reflon,      &
            & (tcv%reflat+tcmsi_area),dist)

       ! Define local variables

       radius_threshold = max(radius_threshold,dist)

       ! Compute local variables

       call grid_methods_radialdist(tcv%reflon,tcv%reflat,tcv%reflon,      &
            & (tcv%reflat-tcmsi_area),dist)

       ! Define local variables

       radius_threshold = max(radius_threshold,dist)
       
    end if ! if(is_rgnlmdl)

    ! Define local variables
    
    call check_tcv(tcv,meteo,radius_threshold,tcv_pass)

    ! Check local variable and proceed accordingly

    if(.not. tcv_pass) goto 1000
    
    ! Define local variables

    mpi_taskgrid%ncoords = (meteo%nx*meteo%ny)
    mpi_taskgrid%nlevs   = meteo%nz
    call variable_interface_setup_struct(mpi_taskgrid)
    call mpi_interface_partition(debug)
    tcv_local(1)         = tcv
    
    ! Allocate memory for local variables

    if(.not. allocated(vrtxinfo)) allocate(vrtxinfo(1))

    ! Compute local variables

    call tropcyc_methods_attrs(meteo,vrtxinfo(1),tcv_local)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(mpi_taskgrid)
    
    ! Define local variables

    recenter%area  = tcmsi_area
    recenter%clat  = tcv%reflat
    recenter%clon  = tcv%reflon
    recenter%darea = tcmsi_darea
    grid%nx        = meteo%nx
    grid%ny        = meteo%ny
    call variable_interface_setup_struct(grid)
    grid%lat       = meteo%xlat
    grid%lon       = meteo%xlon
    grid%var       = spval
    where((meteo%u10m .ne. spval) .and. (meteo%v10m .ne. spval))           &
         & grid%var = sqrt(meteo%u10m*meteo%u10m + meteo%v10m*meteo%v10m)

    ! Compute local variables

    call interpolation_interface_recenter(grid,recenter)

    ! Define local variables

    wnd2d%nx        = recenter%nx
    wnd2d%ny        = recenter%ny
    wnd2d%maxradius = maxval(recenter%radius)
    wnd2d%dangle    = tcmsi_dangle
    wnd2d%dradius   = tcmsi_dradius
    call variable_interface_setup_struct(wnd2d)
    wnd2d%var       = recenter%var

    ! Compute local variables

    call math_methods_wnd(wnd2d,recenter,wndgrid)

    ! Allocate memory for local variables
    
    if(.not. allocated(wndvar)) allocate(wndvar(wnd2d%nr*wnd2d%na))

    ! Define local variables

    tcmsi%nx          = recenter%nx
    tcmsi%ny          = recenter%ny
    tcmsi%nh          = wnd2d%nh
    call variable_interface_setup_struct(tcmsi)
    tcmsi%lats        = reshape(recenter%lat,shape(tcmsi%lats))
    tcmsi%lons        = reshape(recenter%lon,shape(tcmsi%lons))
    tcmsi%radius      = reshape(recenter%radius,shape(tcmsi%radius))
    tcmsi%w10m        = reshape(recenter%var,shape(tcmsi%w10m))
    tcmsi%ike_hur     = vrtxinfo(1)%ike_hur
    tcmsi%ike_ts      = vrtxinfo(1)%ike_ts
    tcmsi%r34kt       = vrtxinfo(1)%r34kt
    tcmsi%r50kt       = vrtxinfo(1)%r50kt
    tcmsi%r64kt       = vrtxinfo(1)%r64kt
    tcmsi%rmw         = vrtxinfo(1)%rmw
    tcmsi%roci        = vrtxinfo(1)%roci
    remap%dst_ncoords = (recenter%nx*recenter%ny)
    remap%src_ncoords = (wndgrid%nx*wndgrid%ny)
    call variable_interface_setup_struct(remap)
    remap%dst_lat     = recenter%lat
    remap%dst_lon     = recenter%lon
    remap%src_lat     = wndgrid%lat
    remap%src_lon     = wndgrid%lon
    
    ! Compute local variables

    call interpolation_interface_init(remap)

    ! Loop through local variable

    do i = 1, wnd2d%nh
    
       ! Define local variables

       wndvar = reshape(wnd2d%wnvar(i,:,:),shape(wndvar))

       ! Compute local variables

       call interpolation_interface_bilinear(remap,wndvar)

       ! Define local variables

       tcmsi%wndvar(i,:,:) = reshape(remap%var,shape(tcmsi%wndvar(i,:,:)))

    end do ! do i = 1, wnd2d%nh

    ! Deallocate memory for local variables

    if(allocated(wndvar)) deallocate(wndvar)

    ! Define local variables
    
    vargrid%nvals = (recenter%nx*recenter%ny)
    call variable_interface_setup_struct(vargrid)
    vargrid%var   = reshape(tcmsi%w10m,shape(vargrid%var))
    
    ! Compute local variables
    
    call math_methods_stats(vargrid,statgrid)
    
    ! Define local variables
    
    tcmsi%vmax  = statgrid%varmax
    vargrid%var = reshape(tcmsi%wndvar(1,:,:),shape(vargrid%var))

    ! Compute local variables
    
    call math_methods_stats(vargrid,statgrid)
    
    ! Define local variables
    
    tcmsi%vwn0  = statgrid%varmax
    vargrid%var = reshape(tcmsi%wndvar(2,:,:),shape(vargrid%var))
    
    ! Compute local variables
    
    call math_methods_stats(vargrid,statgrid)
    
    ! Define local variables
    
    tcmsi%vwn1  = statgrid%varmax
    vargrid%var = reshape((tcmsi%wndvar(1,:,:) +                           &
         & tcmsi%wndvar(2,:,:)),shape(vargrid%var))

    ! Compute local variables
    
    call math_methods_stats(vargrid,statgrid)
    
    ! Define local variables
    
    tcmsi%vwn0pwn1 = statgrid%varmax
    vargrid%var    = 0.0

    ! Loop through local variable

    do i = 3, wnd2d%nh

       ! Define local variables

       vargrid%var = vargrid%var + reshape(tcmsi%wndvar(i,:,:),            &
            & shape(vargrid%var))

    end do ! do i = 3, wnd2d%nh

    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables

    tcmsi%veps = statgrid%varmax
1000 continue

    ! Deallocate memory for local variables

    if(allocated(vrtxinfo)) deallocate(vrtxinfo)
    call variable_interface_cleanup_struct(grid)
    call variable_interface_cleanup_struct(recenter)
    call variable_interface_cleanup_struct(remap)
    call variable_interface_cleanup_struct(vargrid)
    call variable_interface_cleanup_struct(wnd2d)
    call variable_interface_cleanup_struct(wndgrid)

    !=====================================================================

  end subroutine tcdiags_methods_tcmsi

  !=======================================================================

end module tcdiags_methods_interface
