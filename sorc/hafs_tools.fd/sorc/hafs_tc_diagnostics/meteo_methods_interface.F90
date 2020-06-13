module meteo_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: meteo_methods_interface
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
  use grid_methods_interface
  use kinds_interface
  use math_methods_interface
  use mpi_interface
  use namelist_interface
  use sphere
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: meteo_methods_coriolis
  public :: meteo_methods_cps_thermal
  public :: meteo_methods_diagnostics
  public :: meteo_methods_divergence
  public :: meteo_methods_divgwnd
  public :: meteo_methods_hrmnwnd
  public :: meteo_methods_lyrmnwnds
  public :: meteo_methods_moisture
  public :: meteo_methods_pott
  public :: meteo_methods_pv
  public :: meteo_methods_rotnwnd
  public :: meteo_methods_strmfunc
  public :: meteo_methods_velopot
  public :: meteo_methods_vorticity
  public :: meteo_methods_winds
  public :: meteo_methods_wndspd

  ! Define local variables

  logical                                                               :: compute_sphereharms = .false.
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! ccesvp.f90

  ! DESCRIPTION:

  ! This subroutine computes the saturation vapor pressure and
  ! saturation water vapor mixing ratio in accordance with the
  ! Clausius-Clapeyron equation.

  ! INPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing (at
  !   minimum) the temperature (t; Kelvin) profile.

  ! OUTPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing the
  !   saturation vapor pressure (svp; Pascals) and the saturation
  !   water vapor mixing ratio (swvmxrt; kilograms per kilogram)
  !   computed from the Clausius-Clapeyron equation.

  !-----------------------------------------------------------------------

  subroutine ccesvp(moisture)

    ! Define variables passed to routine

    type(moisture_struct)                                               :: moisture

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_svp
    real(r_kind),               dimension(:,:),             allocatable :: mpi_swvmxrt
    real(r_kind)                                                        :: e0
    real(r_kind)                                                        :: t0
    integer                                                             :: mpi_id

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_svp))                                           &
         & allocate(mpi_svp(moisture%ncoords,moisture%nz))
    if(.not. allocated(mpi_swvmxrt))                                       &
         & allocate(mpi_swvmxrt(moisture%ncoords,moisture%nz))

    ! Define local variables

    call mpi_interface_gettaskid(mpi_procid,mpi_id)
    mpi_svp     = 0.0
    mpi_swvmxrt = 0.0
    e0          = 611.0
    t0          = 273.15

    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

       ! Loop through local variable

       do j = 1, moisture%nz

          ! Check local variable and proceed accordingly

          if(moisture%t(i,j) .ne. spval) then

             ! Compute local variables

             mpi_svp(i,j)     = e0*exp((ltnthtvpr/rv)*((1.0/t0) -          &
                  & (1.0/moisture%t(i,j))))
             mpi_swvmxrt(i,j) = (mpi_svp(i,j)*rd)/(rv*(moisture%p(i,j) -   &
                  & mpi_svp(i,j)))

          else   ! if(moisture%t(i,j) .ne. spval)

             ! Define local variables

             mpi_svp(i,j)     = spval
             mpi_swvmxrt(i,j) = spval

          end if ! if(moisture%t(i,j) .ne. spval)

       end do ! do j = 1, moisture%nz

    end do ! i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_svp,moisture%svp,(moisture%ncoords*moisture%nz),   &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_swvmxrt,moisture%swvmxrt,(moisture%ncoords*        &
         & moisture%nz),mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,    &
         & mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_svp))     deallocate(mpi_svp)
    if(allocated(mpi_swvmxrt)) deallocate(mpi_swvmxrt)

    ! Define local variables

    call mpi_bcast(moisture%svp,(moisture%ncoords*moisture%nz),mpi_real,   &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(moisture%swvmxrt,(moisture%ncoords*moisture%nz),        &
         & mpi_real,mpi_masternode,mpi_comm_world,mpi_ierror)

    !=====================================================================

  end subroutine ccesvp

  !=======================================================================

  ! SUBROUTINE:

  ! glbl_sphereharms.f90

  ! DESCRIPTION:

  ! This subroutine computes the relative vorticity and divergence
  ! using spherical harmonic spectral transforms from gridded-space
  ! (using zonal- and meridional-winds) to spectral space (returning
  ! the spectral coefficients for divergence and vorticity); the
  ! spectral coefficients are then transformed to gridded-space to
  ! return the gridded vorticity and divergence.

  ! REFERENCES:

  ! Adams, J., and P. Swarztrauber, 1997. SPHEREPACK 2.0: A model
  ! development facility. NCAR Technical Note-TN-436+STR. The National
  ! Center for Atmospheric Research, Boulder, CO.

  ! https://www2.cisl.ucar.edu/resources/legacy/spherepack/documentation

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the gridded zonal- and meridional-wind variables.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the vortcity
  !   and divergence, derived from the zonal- and meridional-wind
  !   components.

  !-----------------------------------------------------------------------

  subroutine glbl_sphereharms(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    complex(r_kind),            dimension(:),               allocatable :: dspec 
    complex(r_kind),            dimension(:),               allocatable :: zspec 
    complex(r_kind),            dimension(:),               allocatable :: zero 
    real(r_kind),               dimension(:,:),             allocatable :: dgrd
    real(r_kind),               dimension(:,:),             allocatable :: mpi_divg
    real(r_kind),               dimension(:,:),             allocatable :: mpi_vort
    real(r_kind),               dimension(:,:),             allocatable :: mpi_wdivu
    real(r_kind),               dimension(:,:),             allocatable :: mpi_wdivv
    real(r_kind),               dimension(:,:),             allocatable :: mpi_wrotu
    real(r_kind),               dimension(:,:),             allocatable :: mpi_wrotv
    real(r_kind),               dimension(:,:),             allocatable :: ugrd
    real(r_kind),               dimension(:,:),             allocatable :: vgrd
    real(r_kind),               dimension(:,:),             allocatable :: zgrd
    integer                                                             :: mpi_lev
    integer                                                             :: ndimspec

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

    ! Check local variable and proceed accordingly

    if(ntrunc .eq. -999) then

       ! Define local variables

       ndimspec = (meteo%ny - 1)

    else   ! if(ntrunc .eq. -999)
    
       ! Define local variables

       ndimspec = (ntrunc + 1)*(ntrunc + 2)/2

    end if ! if(ntrunc .eq. -999)

    ! Check local variable and proceed accordingly

    if((meteo%u(1,1) .ne. spval) .and. (meteo%v(1,1) .ne. spval)) then

       ! Define local variables

       compute_sphereharms = .true.
    
       ! Allocate memory for local variables

       if(.not. allocated(mpi_divg))                                       &
            & allocate(mpi_divg(meteo%ncoords,meteo%nz))
       if(.not. allocated(mpi_vort))                                       &
            & allocate(mpi_vort(meteo%ncoords,meteo%nz))
       if(.not. allocated(mpi_wdivu))                                      &
            & allocate(mpi_wdivu(meteo%ncoords,meteo%nz))
       if(.not. allocated(mpi_wdivv))                                      &
            & allocate(mpi_wdivv(meteo%ncoords,meteo%nz))
       if(.not. allocated(mpi_wrotu))                                      &
            & allocate(mpi_wrotu(meteo%ncoords,meteo%nz))
       if(.not. allocated(mpi_wrotv))                                      &
            & allocate(mpi_wrotv(meteo%ncoords,meteo%nz))
       if(.not. allocated(dgrd))                                           &
            & allocate(dgrd(meteo%nx,meteo%ny))
       if(.not. allocated(ugrd))                                           &
         & allocate(ugrd(meteo%nx,meteo%ny))
       if(.not. allocated(vgrd))                                           &
         & allocate(vgrd(meteo%nx,meteo%ny))
       if(.not. allocated(zgrd))                                           &
            & allocate(zgrd(meteo%nx,meteo%ny))
       if(.not. allocated(dspec))                                          &
            & allocate(dspec(ndimspec))
       if(.not. allocated(zspec))                                          &
            & allocate(zspec(ndimspec))
       if(.not. allocated(zero))                                           &
            & allocate(zero(ndimspec))

       ! Define local variables

       mpi_divg  = 0.0
       mpi_vort  = 0.0
       mpi_wdivu = 0.0
       mpi_wdivv = 0.0
       mpi_wrotu = 0.0
       mpi_wrotv = 0.0
       dspec     = cmplx(0.0)
       zero      = cmplx(0.0)
       zspec     = cmplx(0.0)

       ! Loop through local variable

       do i = 1, meteo%nz

          ! Define local variables
       
          call mpi_interface_gettasklev(mpi_procid,i,mpi_lev)

          ! Check local variable and proceed accordingly
          
          if(mpi_lev .ne. mpi_noproc_assign) then

             ! Define local variables
             
             ugrd = reshape(meteo%u(:,mpi_lev),shape(ugrd))
             vgrd = reshape(meteo%v(:,mpi_lev),shape(vgrd))

             ! Compute local variables
          
             call getvrtdivspec(ugrd,vgrd,zspec,dspec,rearth_equator)
             call spectogrd(zspec,zgrd)

             ! Define local variables

             mpi_vort(:,mpi_lev) = reshape(zgrd,                           &
                  & shape(mpi_vort(:,mpi_lev)))

             ! Compute local variables
             
             call spectogrd(dspec,dgrd)
             
             ! Define local variables
             
             mpi_divg(:,mpi_lev) = reshape(dgrd,                           &
                  & shape(mpi_divg(:,mpi_lev)))
          
             ! Compute local variables

             call getuv(zspec,zero,ugrd,vgrd,rearth_equator)

             ! Define local variables

             mpi_wrotu(:,mpi_lev) = reshape(ugrd,                          &
                  & shape(mpi_wrotu(:,mpi_lev)))
             mpi_wrotv(:,mpi_lev) = reshape(vgrd,                          &
                  & shape(mpi_wrotv(:,mpi_lev)))

             ! Compute local variables

             call getuv(zero,dspec,ugrd,vgrd,rearth_equator)

             ! Define local variables

             mpi_wdivu(:,mpi_lev) = reshape(ugrd,                          &
                  & shape(mpi_wdivu(:,mpi_lev)))
             mpi_wdivv(:,mpi_lev) = reshape(vgrd,                          &
                  & shape(mpi_wdivv(:,mpi_lev)))

          end if ! if(mpi_lev .ne. mpi_noproc_assign)

       end do ! do i = 1, meteo%nz

       ! Define local variables

       call mpi_reduce(mpi_divg,meteo%divg,(meteo%ncoords*meteo%nz),       &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
       call mpi_reduce(mpi_vort,meteo%vort,(meteo%ncoords*meteo%nz),       &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
       call mpi_reduce(mpi_wdivu,meteo%wdivu,(meteo%ncoords*meteo%nz),     &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
       call mpi_reduce(mpi_wdivv,meteo%wdivv,(meteo%ncoords*meteo%nz),     &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
       call mpi_reduce(mpi_wrotu,meteo%wrotu,(meteo%ncoords*meteo%nz),     &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
       call mpi_reduce(mpi_wrotv,meteo%wrotv,(meteo%ncoords*meteo%nz),     &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

       ! Deallocate memory for local variables

       call cleanup()
       if(allocated(mpi_divg))  deallocate(mpi_divg)
       if(allocated(mpi_vort))  deallocate(mpi_vort)
       if(allocated(mpi_wdivu)) deallocate(mpi_wdivu)
       if(allocated(mpi_wdivv)) deallocate(mpi_wdivv)
       if(allocated(mpi_wrotu)) deallocate(mpi_wrotu)
       if(allocated(mpi_wrotv)) deallocate(mpi_wrotv)
       if(allocated(dgrd))      deallocate(dgrd)
       if(allocated(ugrd))      deallocate(ugrd)
       if(allocated(vgrd))      deallocate(vgrd)
       if(allocated(zgrd))      deallocate(zgrd)
       if(allocated(dspec))     deallocate(dspec)
       if(allocated(zspec))     deallocate(zspec)
       if(allocated(zero))      deallocate(zero)

    else   ! if((meteo%u(1,1) .ne. spval) .and. (meteo%v(1,1)
           ! .ne. spval))

       ! Define local variables

       compute_sphereharms = .true.
       
       ! Allocate memory for local variables

       if(.not. allocated(mpi_wdivu))                                      &
            & allocate(mpi_wdivu(meteo%ncoords,meteo%nz))
       if(.not. allocated(mpi_wdivv))                                      &
            & allocate(mpi_wdivv(meteo%ncoords,meteo%nz))
       if(.not. allocated(mpi_wrotu))                                      &
            & allocate(mpi_wrotu(meteo%ncoords,meteo%nz))
       if(.not. allocated(mpi_wrotv))                                      &
            & allocate(mpi_wrotv(meteo%ncoords,meteo%nz))
       if(.not. allocated(dgrd))                                           &
            & allocate(dgrd(meteo%nx,meteo%ny))
       if(.not. allocated(ugrd))                                           &
            & allocate(ugrd(meteo%nx,meteo%ny))
       if(.not. allocated(vgrd))                                           &
            & allocate(vgrd(meteo%nx,meteo%ny))
       if(.not. allocated(zgrd))                                           &
            & allocate(zgrd(meteo%nx,meteo%ny))
       if(.not. allocated(dspec))                                          &
            & allocate(dspec(ndimspec))
       if(.not. allocated(zspec))                                          &
            & allocate(zspec(ndimspec))
       if(.not. allocated(zero))                                           &
            & allocate(zero(ndimspec))

       ! Define local variables

       mpi_wdivu = 0.0
       mpi_wdivv = 0.0
       mpi_wrotu = 0.0
       mpi_wrotv = 0.0
       zero      = 0.0
       
       ! Loop through local variable
       
       do i = 1, meteo%nz
          
          ! Define local variables
          
          call mpi_interface_gettasklev(mpi_procid,i,mpi_lev)
          
          ! Check local variable and proceed accordingly
          
          if(mpi_lev .ne. mpi_noproc_assign) then
             
             ! Define local variables
             
             zgrd = reshape(meteo%vort(:,mpi_lev),shape(zgrd))
             dgrd = reshape(meteo%divg(:,mpi_lev),shape(dgrd))
             
             ! Compute local variables
             
             call grdtospec(zgrd,zspec)
             call grdtospec(dgrd,dspec)
             call getuv(zspec,zero,ugrd,vgrd,rearth_equator)
             
             ! Define local variables
          
             mpi_wrotu(:,mpi_lev) = reshape(ugrd,                          &
                  & shape(mpi_wrotu(:,mpi_lev)))
             mpi_wrotv(:,mpi_lev) = reshape(vgrd,                          &
                  & shape(mpi_wrotv(:,mpi_lev)))

             ! Compute local variables

             call getuv(zero,dspec,ugrd,vgrd,rearth_equator)

             ! Define local variables

             mpi_wdivu(:,mpi_lev) = reshape(ugrd,                          &
                  & shape(mpi_wdivu(:,mpi_lev)))
             mpi_wdivv(:,mpi_lev) = reshape(vgrd,                          &
                  & shape(mpi_wdivv(:,mpi_lev)))

          end if ! if(mpi_lev .ne. mpi_noproc_assign)
          
       end do ! do i = 1, meteo%nz

       ! Define local variables

       call mpi_reduce(mpi_wdivu,meteo%wdivu,(meteo%ncoords*meteo%nz),     &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
       call mpi_reduce(mpi_wdivv,meteo%wdivv,(meteo%ncoords*meteo%nz),     &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
       call mpi_reduce(mpi_wrotu,meteo%wrotu,(meteo%ncoords*meteo%nz),     &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
       call mpi_reduce(mpi_wrotv,meteo%wrotv,(meteo%ncoords*meteo%nz),     &
            & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

       ! Deallocate memory for local variables
       
       call cleanup()
       if(allocated(mpi_wdivu)) deallocate(mpi_wdivu)
       if(allocated(mpi_wdivv)) deallocate(mpi_wdivv)
       if(allocated(mpi_wrotu)) deallocate(mpi_wrotu)
       if(allocated(mpi_wrotv)) deallocate(mpi_wrotv)
       if(allocated(dgrd))      deallocate(dgrd)
       if(allocated(ugrd))      deallocate(ugrd)
       if(allocated(vgrd))      deallocate(vgrd)
       if(allocated(zgrd))      deallocate(zgrd)
       if(allocated(dspec))     deallocate(dspec)
       if(allocated(zspec))     deallocate(zspec)
       if(allocated(zero))      deallocate(zero)
       
    end if ! if((meteo%u(1,1) .ne. spval) .and. (meteo%v(1,1)
           ! .ne. spval))
       
    ! Define local variables
    
    call mpi_bcast(meteo%u,(meteo%ncoords*meteo%nz),mpi_real,              &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%v,(meteo%ncoords*meteo%nz),mpi_real,              &
         & mpi_masternode,mpi_comm_world,mpi_ierror)    
    call mpi_bcast(meteo%divg,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%vort,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)   
    call mpi_bcast(meteo%divg,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%vort,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%wdivu,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%wdivv,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%wrotu,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%wrotv,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('GLBL_SPHEREHARMS: Computing global vorticity, divergence, '    &
         & 'and total wind components.')

    !=====================================================================

  end subroutine glbl_sphereharms

  !=======================================================================

  ! SUBROUTINE:

  ! layer_mean.f90

  ! DESCRIPTION:

  ! This subroutine computes the layer mean for a user specified
  ! variable.

  ! INPUT VARIABLES:

  ! * lyrmn; a FORTRAN lyrmn_struct variable containing the pressure
  !   profile values (p) as well as the variable profile (var) for
  !   which to compute the layer mean.

  ! OUTPUT VARIABLES:

  ! * lyrmn; a FORTRAN lyrmn_struct variable containing the user
  !   specified layer-mean variable value (varmean).

  !-----------------------------------------------------------------------

  subroutine layer_mean(lyrmn)

    ! Define variables passed to routine

    type(lyrmn_struct)                                                  :: lyrmn

    ! Define variables computed within routine

    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    real(r_kind),               dimension(:),               allocatable :: mpi_var
    integer                                                             :: mpi_id

    ! Define counting variable

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_var)) allocate(mpi_var(lyrmn%ncoords))
    
    ! Define local variables

    call mpi_interface_gettaskid(mpi_procid,mpi_id)
    vargrid%nx = lyrmn%nz
    vargrid%ny = 1
    call variable_interface_setup_struct(vargrid)
    mpi_var    = 0.0
    
    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)
    
       ! Define local variables

       vargrid%var = spval

       ! Loop through local variable
       
       do j = 1, lyrmn%nz

          ! Check local variable and proceed accordingly

          if((lyrmn%p(i,j) .le. lyrmn%pmax) .and. (lyrmn%p(i,j) .ge.       &
               & lyrmn%pmin) .and. (lyrmn%var(i,j) .ne. spval)) then

             ! Define local variables

             vargrid%var(j) = lyrmn%var(i,j)

          end if ! if((lyrmn%p(i,j) .le. lyrmn%pmax)
                 ! .and. (lyrmn%p(i,j) .ge. lyrmn%pmin)
                 ! .and. (lyrmn%var(i,j) .ne. spval))
          
       end do ! do j = 1, lyrmn%nz

       ! Compute local variables

       call math_methods_stats(vargrid,statgrid)

       ! Define local variables

       mpi_var(i) = statgrid%mean
       
    end do ! do i = mpi_taskgrid%begin(mpi_id),
           ! mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_var,lyrmn%varmean,lyrmn%ncoords,mpi_real,mpi_sum,  &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    
    ! Deallocate memory for local variables

    if(allocated(mpi_var)) deallocate(mpi_var)
    call variable_interface_cleanup_struct(vargrid)

    ! Define local variables

    call mpi_bcast(lyrmn%varmean,lyrmn%ncoords,mpi_real,mpi_masternode,    &
         & mpi_comm_world,mpi_ierror)
    
    !=====================================================================

  end subroutine layer_mean
    
  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_coriolis.f90

  ! DESCRIPTION:

  ! This subroutine computes the Coriolis parameter using the latitude
  ! geographical coordinate values; units are revolutions per second
  ! (1/s).

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the latitude values for the simulation grid.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the Coriolis
  !   parameter derived from the latitude coordinate values.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_coriolis(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: mpi_cori
    integer                                                             :: mpi_id

    ! Define counting variable

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_cori)) allocate(mpi_cori(meteo%ncoords))

    ! Define local variables

    call mpi_interface_gettaskid(mpi_procid,mpi_id)
    mpi_cori = 0.0

    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)
       
       ! Compute local variables
       
       mpi_cori(i) = 2.0*earth_omega*(meteo%xlat(i)*deg2rad)

    end do ! do i = mpi_taskgrid%begin(mpi_id),
           ! mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_cori,meteo%cori,meteo%ncoords,mpi_real,mpi_sum,    &
         & mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_cori)) deallocate(mpi_cori)

    ! Define local variables

    call mpi_bcast(meteo%cori,meteo%ncoords,mpi_real,mpi_masternode,       &
         & mpi_comm_world,mpi_ierror)

    !=====================================================================

  end subroutine meteo_methods_coriolis

  !=======================================================================

  ! SUBROUTINE: 

  ! meteo_methods_cps_thermal.f90

  ! DESCRIPTION:

  ! This subroutine computes the thermal attributes (e.g., lower-
  ! versus upper-troposphere) using the cyclone phase space (CPS)
  ! diagnostics of Hart [2003].

  ! REFERENCES:

  ! Hart, R. E., 2003: A cyclone phase space derived from thermal wind
  ! and thermal asymmetry. Mon. Wea. Rev., 131, 585-616.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable.
  
  ! * tccps; a FORTRAN tccps_struct variable.

  ! OUTPUT VARIABLES:

  ! * tccps; a FORTRAN tccps_struct containing FORTRAN 4-byte real
  !   value specifying the lower-troposphere CPS thermal wind value
  !   (vlt; unitless) and the upper-troposphere CPS thermal wind value
  !   (vut; unitless).

  !-----------------------------------------------------------------------

  subroutine meteo_methods_cps_thermal(meteo,tccps)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    type(tccps_struct)                                                  :: tccps

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    real(r_kind),               dimension(:),               allocatable :: dstz
    real(r_kind),               dimension(:),               allocatable :: mask
    real(r_kind),               dimension(:),               allocatable :: mpi_work
    real(r_kind)                                                        :: dstp
    real(r_kind)                                                        :: dz
    real(r_kind)                                                        :: logp
    real(r_kind)                                                        :: sumx
    real(r_kind)                                                        :: sumy
    real(r_kind)                                                        :: sumx2
    real(r_kind)                                                        :: sumxy
    integer                                                             :: mpi_id
    integer                                                             :: nisolev

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    mpi_id      = mpi_procid + 1
    grid%nx     = meteo%nx
    grid%ny     = meteo%ny
    call variable_interface_setup_struct(grid)
    grid%lat    = meteo%xlat
    grid%lon    = meteo%xlon

    ! Compute local variables

    call grid_methods_radialgrid(grid,tccps%clon,tccps%clat)

    ! Define local variables

    tccps%radius = grid%radius

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    ! Allocate memory for local variables

    if(.not. allocated(dstz))     allocate(dstz(tccps%ncoords))
    if(.not. allocated(mask))     allocate(mask(tccps%ncoords))
    if(.not. allocated(mpi_work)) allocate(mpi_work(tccps%ncoords))

    ! Define local variables

    dstz     = 0.0
    mask     = 0.0
    where(tccps%radius .le. tccps_maxradius) mask = 1.0
    mpi_work = 0.0
    sumx     = 0.0
    sumy     = 0.0
    sumx2    = 0.0
    sumxy    = 0.0
    nisolev  = 0

    ! Loop through local variable

    do j = 2, tccps%nz

       ! Check local variable and proceed accordingly

       if((tccps%p(1,j) .le. 90000.0) .and. (tccps%p(1,j) .ge.             & 
            & 60000.0)) then
          
          ! Define local variables

          nisolev = nisolev + 1

          ! Loop through local variable

          do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)
             
             ! Check local variable and proceed accordingly

             if(mask(i) .eq. 1.0) then

                ! Define local variables

                mpi_work(i) = tccps%z(i,j)
            
             else   ! if(mask(i) .ne. 0.0)

                ! Define local variables

                mpi_work(i) = spval
   
             end if ! if(mask(i) .ne. 0.0)

          end do ! do i = mpi_taskgrid%begin(mpi_id),
                 ! mpi_taskgrid%end(mpi_id)

       end if ! if((tccps%p(1,j) .le. 90000.0) .and. (tccps%p(1,j)
              ! .ge. 60000.0))

       ! Define local variables

       call mpi_reduce(mpi_work,dstz,tccps%ncoords,mpi_real,mpi_sum,      &
            & mpi_masternode,mpi_comm_world,mpi_ierror)

       ! Compute local variables

       dz    = maxval(dstz,dstz .ne. spval) - minval(dstz,dstz .ne.       &
            & spval)
       logp  = log(tccps%p(1,j)/100.0)
       sumx  = sumx  + logp
       sumy  = sumy  + dz
       sumx2 = sumx2 + logp*logp
       sumxy = sumxy + logp*dz

    end do ! do j = 2, tccps%nz

    ! Compute local variables

    tccps%vlt = (sumx*sumy - nisolev*sumxy)/(sumx*sumx - nisolev*sumx2)

    ! Check local variable and proceed accordingly

    if(abs(tccps%vlt) .gt. 2000.0) tccps%vlt = spval
    
    ! Define local variables

    call mpi_interface_waitall()
    call mpi_bcast(tccps%vlt,1,mpi_real,mpi_masternode,mpi_comm_world,    &
         & mpi_ierror)
    dstz     = 0.0
    mask     = 0.0
    where(tccps%radius .le. tccps_maxradius) mask = 1.0
    mpi_work = 0.0
    sumx     = 0.0
    sumy     = 0.0
    sumx2    = 0.0
    sumxy    = 0.0
    nisolev  = 0

    ! Loop through local variable

    do j = 2, tccps%nz

       ! Check local variable and proceed accordingly

       if((tccps%p(1,j) .le. 60000.0) .and. (tccps%p(1,j) .ge.             & 
            & 30000.0)) then
          
          ! Define local variables

          nisolev = nisolev + 1

          ! Loop through local variable

          do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)
             
             ! Check local variable and proceed accordingly

             if(mask(i) .eq. 1.0) then

                ! Define local variables

                mpi_work(i) = tccps%z(i,j)
            
             else   ! if(mask(i) .ne. 0.0)

                ! Define local variables

                mpi_work(i) = spval
   
             end if ! if(mask(i) .ne. 0.0)

          end do ! do i = mpi_taskgrid%begin(mpi_id),
                 ! mpi_taskgrid%end(mpi_id)

       end if ! if((tccps%p(1,j) .le. 60000.0) .and. (tccps%p(1,j)
              ! .ge. 30000.0))

       ! Define local variables

       call mpi_reduce(mpi_work,dstz,tccps%ncoords,mpi_real,mpi_sum,      &
            & mpi_masternode,mpi_comm_world,mpi_ierror)

       ! Compute local variables

       dz    = maxval(dstz,dstz .ne. spval) - minval(dstz,dstz .ne.       &
            & spval)
       logp  = log(tccps%p(1,j)/100.0)
       sumx  = sumx  + logp
       sumy  = sumy  + dz
       sumx2 = sumx2 + logp*logp
       sumxy = sumxy + logp*dz

    end do ! do j = 2, tccps%nz

    ! Compute local variables

    tccps%vut = (sumx*sumy - nisolev*sumxy)/(sumx*sumx - nisolev*sumx2)

    ! Check local variable and proceed accordingly

    if(abs(tccps%vut) .gt. 2000.0) tccps%vut = spval
    
    ! Define local variables

    call mpi_interface_waitall()
    call mpi_bcast(tccps%vut,1,mpi_real,mpi_masternode,mpi_comm_world,    &
         & mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(dstz))     deallocate(dstz)
    if(allocated(mask))     deallocate(mask)
    if(allocated(mpi_work)) deallocate(mpi_work)
    
    !=====================================================================

  end subroutine meteo_methods_cps_thermal

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_diagnostics.f90

  ! DESCRIPTION:

  ! This subroutine computes all diagnostics variables required by the
  ! routines throughout the high-level methodology.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing, at minimum,
  !   the following variables:

  !   (1) Sea-level pressure (pmsl; Pascals).

  !   (2) Orography heights (orog; meters).

  !   (3) Common latitude grid (xlat; degrees).

  !   (4) Common longitude grid (xlon; degrees).

  !   (5) Specific humidity profile (q; kilograms per kilogram).

  !   (6) Temperature profile (t; Kelvin).

  !   (7) Geopotential height profile (z; meters)

  !   (8) Zonal wind velocity (u; meters per second).

  !   (9) Meridional wind velocity (v; meters per second).

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the following
  !   variables:

  !   (1)  Wind speed profile (wndspd; meters per second).

  !   (2)  Divergence profile (divg; rotations per second).

  !   (3)  Vorticity profile (vort; rotations per second).

  !   (4)  Streamfunction profile (psi; meters^2 per second).

  !   (5)  Velocity potential profile (chi; meters^2 per second).

  !   (6)  Divergent wind (both u- and v-component) profiles (wdivu and
  !        wdivv, respectively; meters per second).

  !   (7)  Rotational wind (both u- and v-component) profiles (wrotu
  !        and wrotv, respectively; meters per second).

  !   (8)  Harmonic wind (both u- and v-component) profiles (whrmu and
  !        whrmv, respectively; meters per second).

  !   (9)  Potential temperature profile (pott; Kelvin).

  !   (10) Potential vorticity profile (pv; meters^2 per second,
  !        Kelvin per kilogram).

  !-----------------------------------------------------------------------

  subroutine meteo_methods_diagnostics(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(moisture_struct)                                               :: moisture

    !=====================================================================

    ! Compute local variables

    call meteo_methods_vorticity(meteo)
    call meteo_methods_divergence(meteo)
    call meteo_methods_strmfunc(meteo)
    call meteo_methods_velopot(meteo)
    call meteo_methods_divgwnd(meteo)
    call meteo_methods_rotnwnd(meteo)
    call meteo_methods_hrmnwnd(meteo)
    call meteo_methods_wndspd(meteo)
    call meteo_methods_pott(meteo)
    call meteo_methods_coriolis(meteo)
    call meteo_methods_pv(meteo)

    ! Define local variables

    moisture%nx = meteo%nx
    moisture%ny = meteo%ny
    moisture%nz = meteo%nz
    call variable_interface_setup_struct(moisture)
    moisture%p  = meteo%p
    moisture%q  = meteo%q
    moisture%t  = meteo%t

    ! Compute local variables

    call meteo_methods_moisture(moisture)

    ! Define local variables

    meteo%rh     = moisture%rh
    meteo%wvmxrt = moisture%wvmxrt

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(moisture)
    
    !=====================================================================

  end subroutine meteo_methods_diagnostics

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_divergence.f90

  ! DESCRIPTION:

  ! This subroutine will compute the dot product of the wind field
  ! components (u and v) in order to estimate the relative divergence
  ! field; units are 1./seconds.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively).

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the
  !   divergence, derived from the zonal- and meridional-wind
  !   components and the grid projection attributes.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_divergence(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    !=====================================================================

    ! Compute local variables

    if(is_glblmdl .and. (.not. compute_sphereharms))                       &
         & call glbl_sphereharms(meteo)
    if(is_rgnlmdl) call rgnl_divergence(meteo)

    !=====================================================================

  end subroutine meteo_methods_divergence

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_divgwnd.f90

  ! DESCRIPTION:

  ! This subroutine computes the divergent wind field components;
  ! units are meters per second.
 
  ! REFERENCES:

  ! Lynch, P., 1989: Partitioning the wind in a limited
  ! domain. Mon. Wea. Rev., 117, 1492–1500.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively); if the velocity potential array (e.g., chi) is
  !   not composed of missing data values (i.e., spval), the velocity
  !   potential is not recomputed and the velocity potential passed
  !   within the meteo_struct variable is used.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the divergent
  !   wind components function derived from the zonal- and
  !   meridional-wind components and the grid projection attributes;
  !   if the velocity component array (e.g., chi) was composed
  !   entirely of missing data values (i.e., spval) on input, the
  !   meteo_struct variable also contains the computed velocity
  !   potential values.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_divgwnd(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    !=====================================================================

    ! Compute local variables

    if(is_glblmdl .and. (.not. compute_sphereharms))                       &
         & call glbl_sphereharms(meteo)
    if(is_rgnlmdl) call rgnl_divgwnd(meteo)

    !=====================================================================

  end subroutine meteo_methods_divgwnd

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_hrmnwnd.f90

  ! DESCRIPTION:

  ! This subroutine computes the harmonic wind field components; units
  ! are meters per second.
 
  ! REFERENCES:

  ! Lynch, P., 1989: Partitioning the wind in a limited
  ! domain. Mon. Wea. Rev., 117, 1492–1500.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the zonal- and
  !   meridional-wind components (u and v, respectively), the
  !   rotational wind component arrays (e.g., wrotu and wrotv), and
  !   the divergent wind component arrays (e.g., wdivu and wdivv).

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the harmonic
  !   wind components derived from the zonal- and meridional-wind
  !   components; if either the rotational or divergent wind
  !   components was composed entirely of missing data values (i.e.,
  !   spval) on input, the meteo_struct variable also contains the
  !   respective computed values.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_hrmnwnd(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_whrmu
    real(r_kind),               dimension(:,:),             allocatable :: mpi_whrmv
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j

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

    ! Allocate memory for local variables

    if(.not. allocated(mpi_whrmu))                                          &
         & allocate(mpi_whrmu(meteo%ncoords,meteo%nz))
    if(.not. allocated(mpi_whrmv))                                          &
         & allocate(mpi_whrmv(meteo%ncoords,meteo%nz))

    ! Define local variables

    mpi_whrmu = 0.0
    mpi_whrmv = 0.0

    ! Loop through local variable

    do j = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,j,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Loop through local variable

          do i = 1, meteo%ncoords

             ! Check local variable and proceed accordingly

             if((meteo%u(i,mpi_lev) .ne. spval) .and.                       &
                  & (meteo%wrotu(i,mpi_lev) .ne. spval) .and.               &
                  & (meteo%wdivu(i,mpi_lev) .ne. spval) .and.               &
                  & (meteo%v(i,mpi_lev) .ne. spval) .and.                   &
                  & (meteo%wrotv(i,mpi_lev) .ne. spval) .and.               &
                  & (meteo%wdivv(i,mpi_lev) .ne. spval)) then

                ! Define local variables

                mpi_whrmu(i,mpi_lev) = meteo%u(i,mpi_lev) -                 &
                     & (meteo%wrotu(i,mpi_lev) + meteo%wdivu(i,mpi_lev))
                mpi_whrmv(i,mpi_lev) = meteo%v(i,mpi_lev) -                 &
                     & (meteo%wrotv(i,mpi_lev) + meteo%wdivv(i,mpi_lev))

             else   ! if((meteo%u(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%wrotu(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%wdivu(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%v(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%wrotv(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%wdivv(i,mpi_lev) .ne. spval))

                ! Define local variables

                mpi_whrmu(i,mpi_lev) = spval
                mpi_whrmv(i,mpi_lev) = spval

             end if ! if((meteo%u(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%wrotu(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%wdivu(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%v(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%wrotv(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%wdivv(i,mpi_lev) .ne. spval))

          end do ! do i = 1, meteo%ncoords

          ! Define local variables

          if(debug) write(6,501) mpi_lev, minval(mpi_whrmu(:,mpi_lev),     &
               & mpi_whrmu(:,mpi_lev) .ne. spval),                         &
               & maxval(mpi_whrmu(:,mpi_lev),mpi_whrmu(:,mpi_lev) .ne.     &
               & spval), minval(mpi_whrmv(:,mpi_lev),                      &
               & mpi_whrmv(:,mpi_lev) .ne. spval),                         &
               & maxval(mpi_whrmv(:,mpi_lev),mpi_whrmv(:,mpi_lev) .ne.     &
               & spval)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do j = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_whrmu,meteo%whrmu,(meteo%ncoords*meteo%nz),         &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_whrmv,meteo%whrmv,(meteo%ncoords*meteo%nz),         &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_whrmu)) deallocate(mpi_whrmu)
    if(allocated(mpi_whrmv)) deallocate(mpi_whrmv)

    ! Define local variables

    call mpi_bcast(meteo%whrmu,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%whrmv,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('METEO_METHODS_HRMNWND: Computing the harmonic wind.')
501 format('METEO_METHODS_HRMNWND (level/u-min/u-max/v-min/v-max): ',i,     &
         & 4(f13.5))

    !=====================================================================

  end subroutine meteo_methods_hrmnwnd

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_lyrmnwnds.f90

  ! DESCRIPTION:

  ! This subroutine computes the respective layer-mean values for the
  ! vector winds as described in Velden and Leslie [1991] and DeMaria
  ! et al., [2005] suitable for TC prediction.

  ! REFERENCES:

  ! DeMaria, M., M. Mainelli, L. K. Shay, J. A. Knaff, and J. Kaplan,
  ! 2005: Further improvements to the Statistical Hurricane Intensity
  ! Prediction Scheme (SHIPS). Wea. Forecasting, 20, 531–543.
  
  ! Velden, C. S. and L. M. Leslie, 1991: The basic relationship
  ! between tropical cyclone intensity and the depth of the
  ! environmental steering layer in the Australian
  ! Region. Wea. Forecasting, 6, 244–253.
  
  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the zonal- and
  !   meridional-wind components (u and v, respectively).

  ! * tcenv; a FORTRAN tcenv_struct variable.

  ! OUTPUT VARIABLES:

  ! * tcenv; a FORTRAN tcenv_struct variable containing the layer mean
  !   wind diagnostics described in the aforementioned references.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_lyrmnwnds(meteo,tcenv)
  
    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    type(tcenv_struct)                                                  :: tcenv

    ! Define variables computed within routine

    type(lyrmn_struct)                                                  :: lyrmn

    !=====================================================================

    ! Define local variables

    lyrmn%nx   = meteo%nx
    lyrmn%ny   = meteo%ny
    lyrmn%nz   = meteo%nz
    call variable_interface_setup_struct(lyrmn)
    lyrmn%p    = meteo%p
    lyrmn%pmin = 20000.0
    lyrmn%pmax = 85000.0    
    lyrmn%var  = meteo%u

    ! Compute local variables

    call layer_mean(lyrmn)

    ! Define local variables

    tcenv%u850_200_shear = lyrmn%varmean
    lyrmn%var            = meteo%v

    ! Compute local variables

    call layer_mean(lyrmn)

    ! Define local variables

    tcenv%v850_200_shear = lyrmn%varmean    
    lyrmn%pmin = 30000.0
    lyrmn%pmax = 85000.0    
    lyrmn%var  = meteo%u
    
    ! Compute local variables

    call layer_mean(lyrmn)

    ! Define local variables

    tcenv%u925 = lyrmn%varmean
    tcenv%u935 = lyrmn%varmean
    tcenv%u945 = lyrmn%varmean
    lyrmn%var  = meteo%v

    ! Compute local variables

    call layer_mean(lyrmn)

    ! Define local variables
    
    tcenv%v925 = lyrmn%varmean
    tcenv%v935 = lyrmn%varmean
    tcenv%v945 = lyrmn%varmean
    lyrmn%pmin = 40000.0
    lyrmn%pmax = 85000.0    
    lyrmn%var  = meteo%u
    
    ! Compute local variables

    call layer_mean(lyrmn)

    ! Define local variables

    tcenv%u955 = lyrmn%varmean
    tcenv%u965 = lyrmn%varmean 
    lyrmn%var  = meteo%v

    ! Compute local variables

    call layer_mean(lyrmn)
    
    ! Define local variables

    tcenv%v955 = lyrmn%varmean
    tcenv%v965 = lyrmn%varmean     
    lyrmn%pmin = 50000.0
    lyrmn%pmax = 85000.0    
    lyrmn%var  = meteo%u

    ! Compute local variables

    call layer_mean(lyrmn)

    ! Define local variables

    tcenv%u975  = lyrmn%varmean
    tcenv%u985  = lyrmn%varmean
    tcenv%u995  = lyrmn%varmean
    tcenv%u1005 = lyrmn%varmean 
    lyrmn%var   = meteo%v    

    ! Compute local variables

    call layer_mean(lyrmn)    
    
    ! Define local variables

    tcenv%v975  = lyrmn%varmean
    tcenv%v985  = lyrmn%varmean
    tcenv%v995  = lyrmn%varmean
    tcenv%v1005 = lyrmn%varmean    
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(lyrmn)
    
    !=====================================================================

  end subroutine meteo_methods_lyrmnwnds
    
  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_moisture.f90 

  ! DESCRIPTION:

  ! This subroutine computes all moisture quantities in accordance
  ! with the information provided by the user.

  ! INPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing (at
  !   minimum) the total pressure (p; Pascals) and temperature (t;
  !   Kelvin) profiles; the moisture quantities are calculated based
  !   on the moisture variable(s) provided by the user for either
  !   specific humidity (q; kilograms per kilogram), relative humidity
  !   (rh; ratio), and/or water vapor mixing ratio (wvmxrt; kilograms
  !   per kilogram).

  ! OUTPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing all
  !   derived moisture quantities:

  !   + relative humidity (rh; ratio)
  !   + saturation vapor pressure (svp; Pascals)
  !   + specific humidity (q; kilograms per kilogram)
  !   + vapor pressure (vp; Pascals)
  !   + water vapor mixing ratio (wvmxrt; kilograms per kilogram)

  !-----------------------------------------------------------------------

  subroutine meteo_methods_moisture(moisture)

    ! Define variables passed to routine

    type(moisture_struct)                                               :: moisture

    !=====================================================================

    ! Compute local variables

    call ccesvp(moisture)
    call q2wvmxrt(moisture)
    call wvmxrt2rh(moisture)

    !=====================================================================

  end subroutine meteo_methods_moisture

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_pott.f90 

  ! DESCRIPTION:

  ! This subroutine computes the potential temperature; units are
  ! Kelvin.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the pressure (p) and temperature (t).

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the potential
  !   temperature

  !-----------------------------------------------------------------------

  subroutine meteo_methods_pott(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_pott
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j

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

    ! Allocate memory for local variables

    if(.not. allocated(mpi_pott))                                          &
         & allocate(mpi_pott(meteo%ncoords,meteo%nz))

    ! Define local variables

    mpi_pott = 0.0

    ! Loop through local variable

    do j = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,j,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Loop through local variable

          do i = 1, meteo%ncoords

             ! Check local variable and proceed accordingly

             if(meteo%t(i,mpi_lev) .ne. spval) then
          
                ! Compute local variables

                mpi_pott(i,mpi_lev) = meteo%t(i,mpi_lev)*(100000.0/        &
                     & meteo%p(i,mpi_lev))**rd_over_cp_mass

             else   ! if(meteo%t(i,mpi_lev) .ne. spval)

                ! Define local variables

                mpi_pott(i,mpi_lev) = spval

             end if ! if(meteo%t(i,mpi_lev) .ne. spval)

          end do ! do i = 1, meteo%ncoords
          
          ! Define local variables

          if(debug) write(6,501) mpi_lev, minval(mpi_pott(:,mpi_lev),      &
               & mpi_pott(:,mpi_lev) .ne. spval),                          &
               & maxval(mpi_pott(:,mpi_lev),mpi_pott(:,mpi_lev) .ne.       &
               & spval)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do j = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_pott,meteo%pott,(meteo%ncoords*meteo%nz),          &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_pott)) deallocate(mpi_pott)

    ! Define local variables

    call mpi_bcast(meteo%pott,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('METEO_METHODS_POTT: Computing potential temperature.')
501 format('METEO_METHODS_POTT (level/min/max): ',i,2(f13.5))

    !=====================================================================

  end subroutine meteo_methods_pott

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_pv.f90

  ! DESCRIPTION:

  ! This subroutine computes the Ertel potential vorticity as
  ! formulated by Rossby; units are (meters^2 x Kelvin)/(seconds *
  ! kilograms).

  ! REFERENCES:

  ! Rossby, C.G., 1936. Dynamics of steady ocean currents in the light
  ! of experimental fluid mechanics. Massachusetts Institute of
  ! Technology and Woods Hole Oceanographic Institution.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the latitude grid coordinate value, and the 3-dimensional
  !   temperature (t) and pressure (p); if the Coriolis parameter
  !   array (cori), the potential temperature (pott), and/or the
  !   vorticity (vort) arrays are not composed of missing data values,
  !   the passed variable arrays are used.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the Ertel
  !   potential vorticity (pv).

  !-----------------------------------------------------------------------

  subroutine meteo_methods_pv(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_pv
    real(r_kind)                                                        :: dthetadp
    integer                                                             :: jm1
    integer                                                             :: jp1
    integer                                                             :: mpi_id
    
    ! Define counting variables

    integer                                                             :: i, j

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

    ! Allocate memory for local variables

    if(.not. allocated(mpi_pv)) allocate(mpi_pv(meteo%ncoords,meteo%nz))

    ! Define local variables

    call mpi_interface_gettaskid(mpi_procid,mpi_id)
    mpi_pv = 0.0

    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

       ! Loop through local variable

       do j = 1, meteo%nz

          ! Define local variables

          jp1 = min(j+1,meteo%nz)
          jm1 = max(j-1,1)

          ! Check local variable and proceed accordingly

          if((meteo%pott(i,jm1) .ne. spval) .and. (meteo%pott(i,jp1) .ne.  &
               & spval) .and. (meteo%p(i,jm1) .ne. spval) .and.            &
               & (meteo%p(i,jp1) .ne. spval) .and. (meteo%vort(i,j) .ne.   &
               & spval) .and. (meteo%cori(i) .ne. spval)) then

             ! Compute local variables

             dthetadp = (meteo%pott(i,jp1) - meteo%pott(i,jm1))/           &
                  & (meteo%p(i,jp1) - meteo%p(i,jm1))

             ! Check local variable and proceed accordingly

             if(variable_interface_check_nan(dthetadp)) then

                ! Define local variables

                mpi_pv(i,j) = spval

             else   ! if(variable_interface_check_nan(dthetadp))

                ! Compute local variables

                mpi_pv(i,j) = -1.0*grav*(meteo%vort(i,j) + meteo%cori(i))* &
                     & dthetadp

             end if ! if(variable_interface_check_nan(dthetadp))

          else   ! if(meteo%pott(i,j) .ne. spval)

             ! Define local variables

             mpi_pv(i,j) = spval

          end if ! if((meteo%pott(i,jm1) .ne. spval)
                 ! .and. (meteo%pott(i,jp1) .ne. spval)
                 ! .and. (meteo%p(i,jm1) .ne. spval)
                 ! .and. (meteo%p(i,jp1) .ne. spval)
                 ! .and. (meteo%vort(i,j) .ne. spval)
                 ! .and. (meteo%cori(i) .ne. spval))

       end do ! do j = 1, meteo%nz

    end do ! do i = mpi_taskgrid%begin(mpi_id),
           ! mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_pv,meteo%pv,(meteo%ncoords*meteo%nz),mpi_real,     &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_pv)) deallocate(mpi_pv)

    ! Define local variables

    call mpi_bcast(meteo%pv,(meteo%ncoords*meteo%nz),mpi_real,             &
         & mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Check local variable and proceed accordingly

    if(debug .and. (mpi_procid .eq. mpi_masternode)) then

       ! Loop through local variable

       do i = 1, meteo%nz

          ! Define local variables

          write(6,501) i, minval(meteo%pv(:,i),meteo%pv(:,i) .ne. spval),  &
               & maxval(meteo%pv(:,i),meteo%pv(:,i) .ne. spval)

       end do ! do i = 1, meteo%nz

    end if ! if(debug .and. (mpi_procid .eq. mpi_masternode))

    ! Define local variables

    call mpi_interface_waitall()
500 format('METEO_METHODS_PV: Computing potential vorticity.')
501 format('METEO_METHODS_PV (level/min/max): ',i,2(e13.5))

    !=====================================================================

  end subroutine meteo_methods_pv

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_rotnwnd.f90

  ! DESCRIPTION:

  ! This subroutine computes the rotational wind field components;
  ! units are meters per second.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively); if the stream-function array (e.g., psi) is not
  !   composed of missing data values (i.e., spval), the
  !   stream-function is not recomputed and the stream-function passed
  !   within the meteo_struct variable is used.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the rotational
  !   wind components function derived from the zonal- and
  !   meridional-wind components and the grid projection attributes;
  !   if the stream-function array (e.g., psi) was composed entirely
  !   of missing data values (i.e., spval) on input, the meteo_struct
  !   variable also contains the computed stream-function values.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_rotnwnd(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    !=====================================================================

    ! Compute local variables

    if(is_glblmdl .and. (.not. compute_sphereharms))                       &
         & call glbl_sphereharms(meteo)
    if(is_rgnlmdl) call rgnl_rotnwnd(meteo)

    !=====================================================================

  end subroutine meteo_methods_rotnwnd

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_strmfunc.f90

  ! DESCRIPTION:

  ! This subroutine computes the horizontal streamfunction from the
  ! horizontal relative vorticity at each grid level, using the MKL
  ! Intel fast Poisson solver routines; units are
  ! (meters*meters)/second.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively); if the vorticity array (e.g., vort) is not
  !   composed of missing data values (i.e., spval), the vorticity is
  !   not recomputed and the vorticity passed within the meteo_struct
  !   variable is used.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the stream
  !   function derived from the zonal- and meridional-wind components
  !   and the grid projection attributes; if the vorticity array
  !   (e.g., vort) was composed entirely of missing data values (i.e.,
  !   spval) on input, the meteo_struct variable also contains the
  !   computed vorticity values.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_strmfunc(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(poisson_struct)                                                :: poisson
    real(r_kind),               dimension(:,:),             allocatable :: mpi_psi
    integer                                                             :: mpi_lev

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

    poisson%nx    = meteo%nx
    poisson%ny    = meteo%ny
    call variable_interface_setup_struct(poisson)
    poisson%xmin  = dble(1.0)
    poisson%xmax  = dble(maxval(meteo%dx)*(poisson%nx + 1))
    poisson%ymin  = dble(1.0)
    poisson%ymax  = dble(maxval(meteo%dy)*(poisson%ny + 1))
    poisson%q     = dble(0.0)
    poisson%bd_ax = dble(0.0)
    poisson%bd_bx = dble(0.0)
    poisson%bd_ay = dble(0.0)
    poisson%bd_by = dble(0.0)

    ! Check local variable and proceed accordingly

    if(is_glblmdl) poisson%bctype = 'PPDD'
    if(is_rgnlmdl) poisson%bctype = 'DDNN'

    ! Allocate memory for local variables

    if(.not. allocated(mpi_psi))                                           &
         & allocate(mpi_psi(meteo%ncoords,meteo%nz))

    ! Define local variables

    mpi_psi = 0.0

    ! Loop through local variable

    do i = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,i,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Define local variables

          poisson%rhs                            = dble(0.0)
          poisson%rhs(1:poisson%nx,1:poisson%ny) =                         &
               & dble(reshape(meteo%vort(:,mpi_lev),                       &
               & shape(poisson%rhs(1:poisson%nx,1:poisson%ny))))
          where(poisson%rhs .eq. spval) poisson%rhs = 0.0
          poisson%rhs                            = dble(-1.0)*poisson%rhs

          ! Compute local variables

          call math_methods_poisson(poisson)

          ! Define local variables

          mpi_psi(:,mpi_lev) = reshape(real(poisson%sol(1:poisson%nx,      &
               & 1:poisson%ny)),shape(mpi_psi(:,mpi_lev)))
          where(meteo%vort(:,mpi_lev) .eq. spval) mpi_psi(:,mpi_lev) =     &
               & spval
          if(debug) write(6,501) mpi_lev, minval(mpi_psi(:,mpi_lev),       &
               & mpi_psi(:,mpi_lev) .ne. spval),                           &
               & maxval(mpi_psi(:,mpi_lev),mpi_psi(:,mpi_lev) .ne. spval)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do i = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_psi,meteo%psi,(meteo%ncoords*meteo%nz),mpi_real,   &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_psi)) deallocate(mpi_psi)
    call variable_interface_cleanup_struct(poisson)

    ! Define local variables

    call mpi_bcast(meteo%psi,(meteo%ncoords*meteo%nz),mpi_real,            &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('METEO_METHODS_STRMFUNC: Computing stream-function.')
501 format('METEO_METHODS_STRMFUNC (level/min/max): ',i,2(d13.5))

    !=====================================================================

  end subroutine meteo_methods_strmfunc

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_velopot.f90

  ! DESCRIPTION:

  ! This subroutine computes the horizontal velocity potential from
  ! the horizontal divergence at each grid level, using the MKL Intel
  ! fast Poisson solver routines; units are (meters*meters)/second.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively); if the divergence array (e.g., divg) is not
  !   composed of missing data values (i.e., spval), the divergence is
  !   not recomputed and the divergence passed within the meteo_struct
  !   variable is used.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the velocity
  !   potential derived from the zonal- and meridional-wind components
  !   and the grid projection attributes; if the divergence array
  !   (e.g., divg) was composed entirely of missing data values (i.e.,
  !   spval) on input, the meteo_struct variable also contains the
  !   computed divergence values.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_velopot(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(poisson_struct)                                                :: poisson
    real(r_kind),               dimension(:,:),             allocatable :: mpi_chi
    integer                                                             :: mpi_lev

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

    poisson%nx    = meteo%nx
    poisson%ny    = meteo%ny
    call variable_interface_setup_struct(poisson)
    poisson%xmin  = dble(1.0)
    poisson%xmax  = dble(maxval(meteo%dx)*(poisson%nx + 1))
    poisson%ymin  = dble(1.0)
    poisson%ymax  = dble(maxval(meteo%dy)*(poisson%ny + 1))
    poisson%q     = dble(0.0)
    poisson%bd_ax = dble(0.0)
    poisson%bd_bx = dble(0.0)
    poisson%bd_ay = dble(0.0)
    poisson%bd_by = dble(0.0)

    ! Check local variable and proceed accordingly

    if(is_glblmdl) poisson%bctype = 'PPDD'
    if(is_rgnlmdl) poisson%bctype = 'DDNN'

    ! Allocate memory for local variables

    if(.not. allocated(mpi_chi))                                           &
         & allocate(mpi_chi(meteo%ncoords,meteo%nz))

    ! Define local variables

    mpi_chi = 0.0

    ! Loop through local variable

    do i = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,i,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Define local variables

          poisson%rhs                            = dble(0.0)
          poisson%rhs(1:poisson%nx,1:poisson%ny) =                         &
               & dble(reshape(meteo%divg(:,mpi_lev),                       &
               & shape(poisson%rhs(1:poisson%nx,1:poisson%ny))))
          where(poisson%rhs .eq. spval) poisson%rhs = 0.0
          poisson%rhs                            = dble(-1.0)*poisson%rhs

          ! Compute local variables

          call math_methods_poisson(poisson)

          ! Define local variables

          mpi_chi(:,mpi_lev) = real(reshape(poisson%sol(1:poisson%nx,      &
               & 1:poisson%ny),shape(mpi_chi(:,mpi_lev))))
          where(meteo%divg(:,mpi_lev) .eq. spval) mpi_chi(:,mpi_lev) =     &
               & spval
          if(debug) write(6,501) mpi_lev, minval(mpi_chi(:,mpi_lev),       &
               & mpi_chi(:,mpi_lev) .ne. spval),                           &
               & maxval(mpi_chi(:,mpi_lev),mpi_chi(:,mpi_lev) .ne. spval)
          
       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do i = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_chi,meteo%chi,(meteo%ncoords*meteo%nz),mpi_real,   &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_chi)) deallocate(mpi_chi)
    call variable_interface_cleanup_struct(poisson)

    ! Define local variables

    call mpi_bcast(meteo%chi,(meteo%ncoords*meteo%nz),mpi_real,            &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('METEO_METHODS_VELOPOT: Computing velocity potential.')
501 format('METEO_METHODS_VELOPOT (level/min/max): ',i,2(d13.5))

    !=====================================================================

  end subroutine meteo_methods_velopot

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_vorticity.f90

  ! DESCRIPTION:

  ! This subroutine will compute the curl of the wind field components
  ! (u and v) in order to estimate the relative vorticity field; units
  ! are 1./seconds.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively).

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the vortcity,
  !   derived from the zonal- and meridional-wind components and the
  !   grid projection attributes.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_vorticity(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    !=====================================================================

    ! Compute local variables

    if(is_glblmdl .and. (.not. compute_sphereharms))                       &
         & call glbl_sphereharms(meteo)
    if(is_rgnlmdl) call rgnl_vorticity(meteo)

    !=====================================================================

  end subroutine meteo_methods_vorticity

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_winds.f90

  ! This subroutine computes the vector wind components (e.g., u and
  ! v) using the vorticity and divergence to compute the
  ! streamfunction and velocity potential to subsequently derive the
  ! divergent and rotational wind components; it is assumed that the
  ! harmonic components of the wind field are already known prior to
  ! calling this routine.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the divergence (divg), vorticity (vort), and harmonic wind speed
  !   (whrmu and whrmv, respectively) profiles.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the vector
  !   wind components (u and v) and the divergent (wdivu and wdivv)
  !   and the rotational (wrotu and wrotv) wind components.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_winds(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_u
    real(r_kind),               dimension(:,:),             allocatable :: mpi_v
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j

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

    ! Allocate memory for local variables

    if(.not. allocated(mpi_u)) allocate(mpi_u(meteo%ncoords,meteo%nz))
    if(.not. allocated(mpi_v)) allocate(mpi_v(meteo%ncoords,meteo%nz))    

    ! Define local variables

    meteo%u             = spval
    meteo%v             = spval
    compute_sphereharms = .false.
    
    ! Compute local variables

    call meteo_methods_strmfunc(meteo)
    call meteo_methods_velopot(meteo)
    call meteo_methods_divgwnd(meteo)
    call meteo_methods_rotnwnd(meteo)

    ! Define local variables

    mpi_u = 0.0
    mpi_v = 0.0

    ! Loop through local variable

    do j = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,j,mpi_lev)
       
       ! Check local variable and proceed accordingly
       
       if(mpi_lev .ne. mpi_noproc_assign) then
       
          ! Loop through local variable

          do i = 1, meteo%ncoords

             ! Check local variable and proceed accordingly

             if((meteo%wdivu(i,mpi_lev) .ne. spval) .and.                   &
                  & (meteo%wdivv(i,mpi_lev) .ne. spval) .and.               &
                  & (meteo%wrotu(i,mpi_lev) .ne. spval) .and.               &
                  & (meteo%wrotv(i,mpi_lev) .ne. spval) .and.               &
                  & (meteo%whrmu(i,mpi_lev) .ne. spval) .and.               &
                  & (meteo%whrmv(i,mpi_lev) .ne. spval)) then

                ! Define local variables
    
                mpi_u(i,mpi_lev) = meteo%wdivu(i,mpi_lev) +                 &
                     & meteo%wrotu(i,mpi_lev) + meteo%whrmu(i,mpi_lev)
                mpi_v(i,mpi_lev) = meteo%wdivv(i,mpi_lev) +                 &
                     & meteo%wrotv(i,mpi_lev) + meteo%whrmv(i,mpi_lev)

             else   ! if((meteo%u(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%v(i,mpi_lev) .ne. spval))

                ! Define local variables

                mpi_u(i,mpi_lev) = spval
                mpi_v(i,mpi_lev) = spval

             end if ! if((meteo%u(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%v(i,mpi_lev) .ne. spval))

          end do ! do i = 1, meteo%ncoords

          ! Define local variables
          
          if(debug) write(6,501) mpi_lev, minval(mpi_u(:,mpi_lev),         &
               & mpi_u(:,mpi_lev) .ne. spval), maxval(mpi_u(:,mpi_lev),    &
               & mpi_u(:,mpi_lev) .ne. spval), minval(mpi_v(:,mpi_lev),    &
               & mpi_v(:,mpi_lev) .ne. spval), maxval(mpi_v(:,mpi_lev),    &
               & mpi_v(:,mpi_lev) .ne. spval)
             
       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do j = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_u,meteo%u,(meteo%ncoords*meteo%nz),mpi_real,       &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_v,meteo%v,(meteo%ncoords*meteo%nz),mpi_real,       &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_u)) deallocate(mpi_u)
    if(allocated(mpi_v)) deallocate(mpi_v)
       
    ! Define local variables

    call mpi_bcast(meteo%u,(meteo%ncoords*meteo%nz),mpi_real,              &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%v,(meteo%ncoords*meteo%nz),mpi_real,              &
         & mpi_masternode,mpi_comm_world,mpi_ierror) 
500 format('METEO_METHODS_WINDS: Deriving the wind components.')
501 format('METEO_METHODS_WINDS (level/u-min/u-max/v-min/v-max): ',i,      &
         & 4(f13.5))
    
    !=====================================================================

  end subroutine meteo_methods_winds
  
  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_wndspd.f90

  ! This subroutine computes the wind speed magnitude using the zonal-
  ! and meridional-wind components (u and v, respectively); units are
  ! meters per second.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- (u-) and meridional- (v-) wind speed profiles.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the wind-speed
  !   magnitude profile (wndspd).

  !-----------------------------------------------------------------------

  subroutine meteo_methods_wndspd(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_wndspd
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j

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

    ! Allocate memory for local variables

    if(.not. allocated(mpi_wndspd))                                        &
         & allocate(mpi_wndspd(meteo%ncoords,meteo%nz))

    ! Define local variables

    mpi_wndspd = 0.0

    ! Loop through local variable

    do j = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,j,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Loop through local variable

          do i = 1, meteo%ncoords

             ! Check local variable and proceed accordingly

             if((meteo%u(i,mpi_lev) .ne. spval) .and. (meteo%v(i,mpi_lev)  &
                  & .ne. spval)) then

                ! Compute local variables

                mpi_wndspd(i,mpi_lev) = sqrt(meteo%u(i,mpi_lev)*           &
                     & meteo%u(i,mpi_lev) + meteo%v(i,mpi_lev)*            &
                     & meteo%v(i,mpi_lev))

             else   ! if((meteo%u(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%v(i,mpi_lev) .ne. spval))

                ! Define local variables

                mpi_wndspd(i,mpi_lev) = spval

             end if ! if((meteo%u(i,mpi_lev) .ne. spval)
                    ! .and. (meteo%v(i,mpi_lev) .ne. spval))

          end do ! do i = 1, meteo%ncoords

          ! Define local variables

          if(debug) write(6,501) mpi_lev, minval(mpi_wndspd(:,mpi_lev),    &
               & mpi_wndspd(:,mpi_lev) .ne. spval),                        &
               & maxval(mpi_wndspd(:,mpi_lev),mpi_wndspd(:,mpi_lev)        &
               & .ne. spval)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do i = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_wndspd,meteo%wndspd,(meteo%ncoords*meteo%nz),      &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_wndspd)) deallocate(mpi_wndspd)

    ! Define local variables

    call mpi_bcast(meteo%wndspd,(meteo%ncoords*meteo%nz),mpi_real,         &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('METEO_METHODS_WNDSPD: Computing wind speed.')
501 format('METEO_METHODS_WNDSPD (level/min/max): ',i,2(f13.5))

    !=====================================================================

  end subroutine meteo_methods_wndspd

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_wvmxrt.f90

  ! DESCRIPTION:

  ! This subroutine computes the water-vapor mixing ratio values
  ! corresponding to either the user provided specific (kilogram per
  ! kilogram) or relative humidity (ratio) values; units are kilograms
  ! per kilogram.

  ! NOTE: Currently only the specific humidity conversion is
  !       supported.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   either the specific humidity (q) or relative humidity (ratio)
  !   values.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the
  !   water-vapor mixing ration values computed from the user
  !   specified moisture information.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_wvmxrt(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, meteo%nz

       ! Check local variable and proceed accordingly

       if(meteo%q(1,i) .ne. spval) then
          
          ! Compute local variables
          
!          call q2wvmxrt(meteo%q(:,i),meteo%wvmxrt(:,i))

       end if ! if(meteo%q(1,j) .ne. spval)

    end do ! do i = 1, meteo%nz

    !=====================================================================

  end subroutine meteo_methods_wvmxrt

  !=======================================================================

  ! SUBROUTINE:

  ! q2wvmxrt.f90

  ! DESCRIPTION:

  ! This subroutine computes the specific humidity profile from the
  ! water-vapor mixing ratio values profile.

  ! INPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing (at
  !   minimum) the specific humidity (q; kilograms per kilogram)
  !   profile.

  ! OUTPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing the
  !   water-vapor mixing ratio (wvmxrt; kilograms per kilogram)
  !   profile.

  !-----------------------------------------------------------------------

  subroutine q2wvmxrt(moisture)

    ! Define variables passed to routine

    type(moisture_struct)                                               :: moisture

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_wvmxrt
    integer                                                             :: mpi_id

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_wvmxrt))                                        &
         & allocate(mpi_wvmxrt(moisture%ncoords,moisture%nz))

    ! Define local variables

    call mpi_interface_gettaskid(mpi_procid,mpi_id)
    mpi_wvmxrt = 0.0

    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

       ! Loop through local variable

       do j = 1, moisture%nz

          ! Check local variable and proceed accordingly

          if(moisture%q(i,j) .ne. spval) then

             ! Compute local variables

             mpi_wvmxrt(i,j) = moisture%q(i,j)/(1.0 - moisture%q(i,j))

             ! Check local variable and proceed accordingly

             if(mpi_wvmxrt(i,j) .lt. 0.0) mpi_wvmxrt(i,j) = clpval

          else   ! if(moisture%q(i,j) .ne. spval) 

             ! Define local variables

             mpi_wvmxrt(i,j) = spval

          end if ! if(moisture%q(i,j) .ne. spval) 

       end do ! do j = 1, moisture%nz

    end do ! do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_wvmxrt,moisture%wvmxrt,(moisture%ncoords*          &
         & moisture%nz),mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,    &
         & mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_wvmxrt)) deallocate(mpi_wvmxrt)

    ! Define local variables

    call mpi_bcast(moisture%wvmxrt,(moisture%ncoords*moisture%nz),         &
         & mpi_real,mpi_masternode,mpi_comm_world,mpi_ierror)

    !=====================================================================

  end subroutine q2wvmxrt

  !=======================================================================

  ! SUBROUTINE:

  ! rgnl_divergence.f90

  ! DESCRIPTION:

  ! This subroutine computes the relative divergence for a regional
  ! forecast model grid using finite-difference approximations.
  
  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively).

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the
  !   divergence, derived from the zonal- and meridional-wind
  !   components and the grid projection attributes.

  !-----------------------------------------------------------------------

  subroutine rgnl_divergence(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: divg
    real(r_kind),               dimension(:,:),             allocatable :: dx
    real(r_kind),               dimension(:,:),             allocatable :: dy
    real(r_kind),               dimension(:,:),             allocatable :: mapfac
    real(r_kind),               dimension(:,:),             allocatable :: mpi_divg
    real(r_kind),               dimension(:,:),             allocatable :: u
    real(r_kind),               dimension(:,:),             allocatable :: v
    real(r_kind)                                                        :: dudx
    real(r_kind)                                                        :: dvdy
    real(r_kind)                                                        :: dsx
    real(r_kind)                                                        :: dsy
    integer                                                             :: im1
    integer                                                             :: ip1
    integer                                                             :: jm1
    integer                                                             :: jp1
    integer                                                             :: mpi_lev

    ! Define counting variable

    integer                                                             :: i, j, k

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

    ! Allocate memory for local variables

    if(.not. allocated(mpi_divg))                                          &
         & allocate(mpi_divg(meteo%ncoords,meteo%nz))
    if(.not. allocated(divg))                                              &
         & allocate(divg(meteo%nx,meteo%ny))
    if(.not. allocated(dx))                                                &
         & allocate(dx(meteo%nx,meteo%ny))
    if(.not. allocated(dy))                                                &
         & allocate(dy(meteo%nx,meteo%ny))
    if(.not. allocated(mapfac))                                            &
         & allocate(mapfac(meteo%nx,meteo%ny))
    if(.not. allocated(u))                                                 &
         & allocate(u(meteo%nx,meteo%ny))
    if(.not. allocated(v))                                                 &
         & allocate(v(meteo%nx,meteo%ny))

    ! Define local variables

    dx       = reshape(meteo%dx,shape(dx))
    dy       = reshape(meteo%dy,shape(dy))
    mapfac   = reshape(meteo%mapfac,shape(mapfac))
    mpi_divg = 0.0

    ! Loop through local variable

    do k = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,k,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Define local variables

          u = reshape(meteo%u(:,mpi_lev),shape(u))
          v = reshape(meteo%v(:,mpi_lev),shape(v))

          ! Loop through local variable
          
          do j = 1, meteo%ny

             ! Define local variables
             
             jp1 = min(j+1,meteo%ny)
             jm1 = max(j-1,1)

             ! Loop through local variable

             do i = 1, meteo%nx

                ! Define local variables

                ip1 = min(i+1,meteo%nx)
                im1 = max(i-1,1)

                ! Check local variable and proceed accordingly

                if((u(ip1,j) .ne. spval) .and. (u(im1,j) .ne. spval)       &
                     & .and. (v(i,jp1) .ne. spval) .and. (v(i,jm1) .ne.    &
                     & spval)) then

                   ! Compute local variables

                   dsx       = (ip1 - im1)*dx(i,j)
                   dsy       = (jp1 - jm1)*dy(i,j)
                   dudx      = (u(ip1,j)/mapfac(ip1,j) - u(im1,j)/         &
                        & mapfac(im1,j))/dsx*(mapfac(i,j)*mapfac(i,j))
                   dvdy      = (v(i,jp1)/mapfac(i,jp1) - v(i,jm1)/         &
                        & mapfac(i,jm1))/dsy*(mapfac(i,j)*mapfac(i,j))
                   divg(i,j) = dudx + dvdy

                else   ! if((u(ip1,j) .ne. spval) .and. (u(im1,j)
                       ! .ne. spval) .and. (v(i,jp1) .ne. spval)
                       ! .and. (v(i,jm1) .ne. spval)) 

                   ! Define local variables

                   divg(i,j) = spval

                end if ! if((u(ip1,j) .ne. spval) .and. (u(im1,j)
                       ! .ne. spval) .and. (v(i,jp1) .ne. spval)
                       ! .and. (v(i,jm1) .ne. spval))

             end do ! do i = 1, meteo%nx

          end do ! do j = 1, meteo%ny

          ! Define local variables

          mpi_divg(:,mpi_lev) = reshape(divg,shape(mpi_divg(:,mpi_lev)))
          if(debug) write(6,501) mpi_lev, minval(mpi_divg(:,mpi_lev),      &
               & mpi_divg(:,mpi_lev) .ne. spval),                          &
               & maxval(mpi_divg(:,mpi_lev),mpi_divg(:,mpi_lev) .ne.       &
               & spval)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do k = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_divg,meteo%divg,(meteo%ncoords*meteo%nz),          &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_divg)) deallocate(mpi_divg)
    if(allocated(mapfac))   deallocate(mapfac)
    if(allocated(divg))     deallocate(divg)
    if(allocated(dx))       deallocate(dx)
    if(allocated(dy))       deallocate(dy)
    if(allocated(v))        deallocate(v)
    if(allocated(u))        deallocate(u)

    ! Define local variables

    call mpi_bcast(meteo%divg,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('RGNL_DIVERGENCE: Computing regional divergence.')
501 format('RGNL_DIVERGENCE (level/min/max): ',i,2(f13.5))

    !=====================================================================

  end subroutine rgnl_divergence

  !=======================================================================

  ! SUBROUTINE:

  ! rgnl_divgwnd.f90

  ! DESCRIPTION:

  ! This subroutine computes the divergent wind field components for a
  ! regional forecast model grid; units are meters per second.
 
  ! REFERENCES:

  ! Lynch, P., 1989: Partitioning the wind in a limited
  ! domain. Mon. Wea. Rev., 117, 1492–1500.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively); if the velocity potential array (e.g., chi) is
  !   not composed of missing data values (i.e., spval), the velocity
  !   potential is not recomputed and the velocity potential passed
  !   within the meteo_struct variable is used.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the divergent
  !   wind components function derived from the zonal- and
  !   meridional-wind components and the grid projection attributes;
  !   if the velocity component array (e.g., chi) was composed
  !   entirely of missing data values (i.e., spval) on input, the
  !   meteo_struct variable also contains the computed velocity
  !   potential values.

  !-----------------------------------------------------------------------

  subroutine rgnl_divgwnd(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: chi
    real(r_kind),               dimension(:,:),             allocatable :: dx
    real(r_kind),               dimension(:,:),             allocatable :: dy
    real(r_kind),               dimension(:,:),             allocatable :: mapfac
    real(r_kind),               dimension(:,:),             allocatable :: mpi_wdivu
    real(r_kind),               dimension(:,:),             allocatable :: mpi_wdivv
    real(r_kind),               dimension(:,:),             allocatable :: u
    real(r_kind),               dimension(:,:),             allocatable :: v
    real(r_kind)                                                        :: dchidx
    real(r_kind)                                                        :: dchidy
    real(r_kind)                                                        :: dsx
    real(r_kind)                                                        :: dsy
    integer                                                             :: im1
    integer                                                             :: ip1
    integer                                                             :: jm1
    integer                                                             :: jp1 
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j, k

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

    ! Allocate memory for local variables

    if(.not. allocated(chi))                                               &
         & allocate(chi(meteo%nx,meteo%ny))
    if(.not. allocated(dx))                                                &
         & allocate(dx(meteo%nx,meteo%ny))
    if(.not. allocated(dy))                                                &
         & allocate(dy(meteo%nx,meteo%ny))
    if(.not. allocated(mapfac))                                            &
         & allocate(mapfac(meteo%nx,meteo%ny))
    if(.not. allocated(mpi_wdivu))                                         &
         & allocate(mpi_wdivu(meteo%ncoords,meteo%nz))
    if(.not. allocated(mpi_wdivv))                                         &
         & allocate(mpi_wdivv(meteo%ncoords,meteo%nz))
    if(.not. allocated(u))                                                 &
         & allocate(u(meteo%nx,meteo%ny))
    if(.not. allocated(v))                                                 &
         & allocate(v(meteo%nx,meteo%ny))

    ! Define local variables

    dx        = reshape(meteo%dx,shape(dx))
    dy        = reshape(meteo%dy,shape(dy))
    mapfac    = reshape(meteo%mapfac,shape(mapfac))
    mpi_wdivu = 0.0
    mpi_wdivv = 0.0

    ! Check local variable and proceed accordingly

    if(meteo%chi(1,1) .eq. spval) then

       ! Compute local variables

       call meteo_methods_velopot(meteo)

    end if ! if(meteo%chi(1,1) .eq. spval)

    ! Loop through local variable

    do k = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,k,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Define local variables

          chi = reshape(meteo%chi(:,mpi_lev),shape(chi))

          ! Loop through local variable
          
          do j = 1, meteo%ny
             
             ! Define local variables
             
             jp1 = min(j+1,meteo%ny)
             jm1 = max(j-1,1)

             ! Loop through local variable
             
             do i = 1, meteo%nx
                
                ! Define local variables
                
                ip1 = min(i+1,meteo%nx)
                im1 = max(i-1,1)

                ! Check local variable and proceed accordingly

                if((chi(ip1,j) .ne. spval) .and. (chi(im1,j) .ne. spval)   &
                     & .and. (chi(i,jp1) .ne. spval) .and. (chi(i,jm1)     &
                     & .ne. spval)) then
                
                   ! Compute local variables
                   
                   dsx    = (ip1 - im1)*dx(i,j)
                   dsy    = (jp1 - jm1)*dy(i,j)
                   dchidx = (chi(ip1,j)/mapfac(ip1,j) - chi(im1,j)/        &
                        & mapfac(im1,j))/dsx*(mapfac(i,j)*mapfac(i,j))
                   dchidy = (chi(i,jp1)/mapfac(i,jp1) - chi(i,jm1)/        &
                        & mapfac(i,jm1))/dsy*(mapfac(i,j)*mapfac(i,j))

                   ! Check local variable and proceed accordingly

                   if(variable_interface_check_nan(dchidx) .or.            &
                        & variable_interface_check_nan(dchidy)) then

                      ! Define local variables

                      u(i,j) = spval
                      v(i,j) = spval

                   else   ! if(variable_interface_check_nan(dchidx)
                          ! .or. variable_interface_check_nan(dchidy))

                      ! Define local variables
             
                      u(i,j) = dchidx
                      v(i,j) = dchidy

                   end if ! if(variable_interface_check_nan(dchidx)
                          ! .or. variable_interface_check_nan(dchidy))

                else   ! if((chi(ip1,j) .ne. spval) .and. (chi(im1,j)
                       ! .ne. spval) .and. (chi(i,jp1) .ne. spval)
                       ! .and. (chi(i,jm1) .ne. spval)

                   ! Define local variables

                   u(i,j) = spval
                   v(i,j) = spval

                end if ! if((chi(ip1,j) .ne. spval) .and. (chi(im1,j)
                       ! .ne. spval) .and. (chi(i,jp1) .ne. spval)
                       ! .and. (chi(i,jm1) .ne. spval)

             end do ! do i = 1, meteo%nx

          end do ! do j = 1, meteo%ny

          ! Define local variables

          mpi_wdivu(:,mpi_lev) = reshape(u,shape(mpi_wdivu(:,mpi_lev)))
          mpi_wdivv(:,mpi_lev) = reshape(v,shape(mpi_wdivv(:,mpi_lev)))
          if(debug) write(6,501) mpi_lev, minval(mpi_wdivu(:,mpi_lev),     &
               & mpi_wdivu(:,mpi_lev) .ne. spval),                         &
               & maxval(mpi_wdivu(:,mpi_lev),mpi_wdivu(:,mpi_lev) .ne.     &
               & spval), minval(mpi_wdivv(:,mpi_lev),                      &
               & mpi_wdivv(:,mpi_lev) .ne. spval),                         &
               & maxval(mpi_wdivv(:,mpi_lev),mpi_wdivv(:,mpi_lev) .ne.     &
               & spval)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do k = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_wdivu,meteo%wdivu,(meteo%ncoords*meteo%nz),        &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_wdivv,meteo%wdivv,(meteo%ncoords*meteo%nz),        &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(chi))       deallocate(chi)
    if(allocated(dx))        deallocate(dx)
    if(allocated(dy))        deallocate(dy)
    if(allocated(mapfac))    deallocate(mapfac)
    if(allocated(mpi_wdivu)) deallocate(mpi_wdivu)
    if(allocated(mpi_wdivv)) deallocate(mpi_wdivv)
    if(allocated(u))         deallocate(u)
    if(allocated(v))         deallocate(v)

    ! Define local variables

    call mpi_bcast(meteo%wdivu,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%wdivv,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('RGNL_DIVGWND: Computing the regional divergent wind.')
501 format('RGNL_DIVGWND (level/u-min/u-max/v-min/v-max): ',i,4(f13.5))

    !=====================================================================

  end subroutine rgnl_divgwnd

  !=======================================================================

  ! SUBROUTINE:

  ! rgnl_rotnwnd.f90

  ! DESCRIPTION:
  
  ! This subroutine computes the rotational wind field components for
  ! a regional forecast model grid; units are meters per second.

  ! REFERENCES:

  ! Lynch, P., 1989: Partitioning the wind in a limited
  ! domain. Mon. Wea. Rev., 117, 1492–1500.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively); if the stream-function array (e.g., psi) is not
  !   composed of missing data values (i.e., spval), the
  !   stream-function is not recomputed and the stream-function passed
  !   within the meteo_struct variable is used.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the rotational
  !   wind components function derived from the zonal- and
  !   meridional-wind components and the grid projection attributes;
  !   if the stream-function array (e.g., psi) was composed entirely
  !   of missing data values (i.e., spval) on input, the meteo_struct
  !   variable also contains the computed stream-function values.

  !-----------------------------------------------------------------------

  subroutine rgnl_rotnwnd(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: dx
    real(r_kind),               dimension(:,:),             allocatable :: dy
    real(r_kind),               dimension(:,:),             allocatable :: mapfac
    real(r_kind),               dimension(:,:),             allocatable :: mpi_wrotu
    real(r_kind),               dimension(:,:),             allocatable :: mpi_wrotv
    real(r_kind),               dimension(:,:),             allocatable :: psi
    real(r_kind),               dimension(:,:),             allocatable :: u
    real(r_kind),               dimension(:,:),             allocatable :: v
    real(r_kind)                                                        :: dpsidx
    real(r_kind)                                                        :: dpsidy
    real(r_kind)                                                        :: dsx
    real(r_kind)                                                        :: dsy
    integer                                                             :: im1
    integer                                                             :: ip1
    integer                                                             :: jm1
    integer                                                             :: jp1
    integer                                                             :: mpi_lev    

    ! Define counting variables

    integer                                                             :: i, j, k

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(dx))                                                &
         & allocate(dx(meteo%nx,meteo%ny))
    if(.not. allocated(dy))                                                &
         & allocate(dy(meteo%nx,meteo%ny))
    if(.not. allocated(mapfac))                                            &
         & allocate(mapfac(meteo%nx,meteo%ny))
    if(.not. allocated(mpi_wrotu))                                         &
         & allocate(mpi_wrotu(meteo%ncoords,meteo%nz))
    if(.not. allocated(mpi_wrotv))                                         &
         & allocate(mpi_wrotv(meteo%ncoords,meteo%nz))
    if(.not. allocated(psi))                                               &
         & allocate(psi(meteo%nx,meteo%ny))
    if(.not. allocated(u))                                                 &
         & allocate(u(meteo%nx,meteo%ny))
    if(.not. allocated(v))                                                 &
         & allocate(v(meteo%nx,meteo%ny))

    ! Define local variables

    dx        = reshape(meteo%dx,shape(dx))
    dy        = reshape(meteo%dy,shape(dy))
    mapfac    = reshape(meteo%mapfac,shape(mapfac))
    mpi_wrotu = 0.0
    mpi_wrotv = 0.0

    ! Check local variable and proceed accordingly

    if(meteo%psi(1,1) .eq. spval) then

       ! Compute local variables

       call meteo_methods_strmfunc(meteo)

    end if ! if(meteo%psi(1,1) .eq. spval)

    ! Loop through local variable

    do k = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,k,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Define local variables

          psi = reshape(meteo%psi(:,mpi_lev),shape(psi))

          ! Loop through local variable
          
          do j = 1, meteo%ny
             
             ! Define local variables

             jp1 = min(j+1,meteo%ny)
             jm1 = max(j-1,1)

             ! Loop through local variable

             do i = 1, meteo%nx

                ! Define local variables

                ip1 = min(i+1,meteo%nx)
                im1 = max(i-1,1)

                ! Check local variable and proceed accordingly

                if((psi(ip1,j) .ne. spval) .and. (psi(im1,j) .ne. spval)   &
                     & .and. (psi(i,jp1) .ne. spval) .and. (psi(i,jm1)     &
                     & .ne. spval)) then

                   ! Compute local variables

                   dsx    = (ip1 - im1)*dx(i,j)
                   dsy    = (jp1 - jm1)*dy(i,j)
                   dpsidy = (psi(i,jp1)/mapfac(i,jp1) - psi(i,jm1)/        &
                        & mapfac(i,jm1))/dsy*(mapfac(i,j)*mapfac(i,j))
                   dpsidx = (psi(ip1,j)/mapfac(ip1,j) - psi(im1,j)/        &
                        & mapfac(im1,j))/dsx*(mapfac(i,j)*mapfac(i,j))    

                   ! Check local variable and proceed accordingly

                   if(variable_interface_check_nan(dpsidx) .or.            &
                        & variable_interface_check_nan(dpsidy)) then

                      ! Define local variables

                      u(i,j) = spval
                      v(i,j) = spval

                   else   ! if(variable_interface_check_nan(dpsidx)
                          ! .or. variable_interface_check_nan(dpsidy)

                      ! Define local variables

                      u(i,j) = -1.0*dpsidy
                      v(i,j) = dpsidx

                   end if ! if(variable_interface_check_nan(dpsidx)
                          ! .or. variable_interface_check_nan(dpsidy)

                else   ! if((psi(ip1,j) .ne. spval) .and. (psi(im1,j)
                       ! .ne. spval) .and. (psi(i,jp1) .ne. spval)
                       ! .and. (psi(i,jm1) .ne. spval))

                   ! Define local variables

                   u(i,j) = spval
                   v(i,j) = spval

                end if ! if((psi(ip1,j) .ne. spval) .and. (psi(im1,j)
                       ! .ne. spval) .and. (psi(i,jp1) .ne. spval)
                       ! .and. (psi(i,jm1) .ne. spval))

             end do ! do i = 1, meteo%nx

          end do ! do j = 1, meteo%ny

          ! Define local variables

          mpi_wrotu(:,mpi_lev) = reshape(u,shape(mpi_wrotu(:,mpi_lev)))
          mpi_wrotv(:,mpi_lev) = reshape(v,shape(mpi_wrotv(:,mpi_lev)))
          if(debug) write(6,501) mpi_lev, minval(mpi_wrotu(:,mpi_lev),     &
               & mpi_wrotu(:,mpi_lev) .ne. spval),                         &
               & maxval(mpi_wrotu(:,mpi_lev),mpi_wrotu(:,mpi_lev) .ne.     &
               & spval), minval(mpi_wrotv(:,mpi_lev),                      &
               & mpi_wrotv(:,mpi_lev) .ne. spval),                         &
               & maxval(mpi_wrotv(:,mpi_lev),mpi_wrotv(:,mpi_lev) .ne.     &
               & spval)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do k = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_wrotu,meteo%wrotu,(meteo%ncoords*meteo%nz),        &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_wrotv,meteo%wrotv,(meteo%ncoords*meteo%nz),        &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(dx))        deallocate(dx)
    if(allocated(dy))        deallocate(dy)
    if(allocated(mapfac))    deallocate(mapfac)
    if(allocated(mpi_wrotu)) deallocate(mpi_wrotu)
    if(allocated(mpi_wrotv)) deallocate(mpi_wrotv)
    if(allocated(psi))       deallocate(psi)
    if(allocated(u))         deallocate(u)
    if(allocated(v))         deallocate(v)

    ! Define local variables

    call mpi_bcast(meteo%wrotu,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%wrotv,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('RGNL_ROTNWND: Computing the regional rotational wind.')
501 format('RGNL_ROTNWND (level/u-min/u-max/v-min/v-max): ',i,4(f13.5))

    !=====================================================================

  end subroutine rgnl_rotnwnd

  !=======================================================================

  ! SUBROUTINE:

  ! rgnl_vorticity.f90

  ! DESCRIPTION:

  ! This subroutine computes the relative vorticity for a regional
  ! forecast model grid using finite-difference approximations.
  
  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the zonal- and meridional-wind components (u and v,
  !   respectively), and the grid projection attributes for grid
  !   distance and map-distortion factor (dx, dy, and mapfac,
  !   respectively).

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the vortcity,
  !   derived from the zonal- and meridional-wind components and the
  !   grid projection attributes.

  !-----------------------------------------------------------------------

  subroutine rgnl_vorticity(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: dx
    real(r_kind),               dimension(:,:),             allocatable :: dy
    real(r_kind),               dimension(:,:),             allocatable :: mapfac
    real(r_kind),               dimension(:,:),             allocatable :: mpi_vort
    real(r_kind),               dimension(:,:),             allocatable :: u
    real(r_kind),               dimension(:,:),             allocatable :: v
    real(r_kind),               dimension(:,:),             allocatable :: vort
    real(r_kind)                                                        :: dvdx
    real(r_kind)                                                        :: dudy
    real(r_kind)                                                        :: dsx
    real(r_kind)                                                        :: dsy
    integer                                                             :: im1
    integer                                                             :: ip1
    integer                                                             :: jm1
    integer                                                             :: jp1
    integer                                                             :: mpi_lev

    ! Define counting variable

    integer                                                             :: i, j, k

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

    ! Allocate memory for local variables

    if(.not. allocated(mpi_vort))                                          &
         & allocate(mpi_vort(meteo%ncoords,meteo%nz))
    if(.not. allocated(dx))                                                &
         & allocate(dx(meteo%nx,meteo%ny))
    if(.not. allocated(dy))                                                &
         & allocate(dy(meteo%nx,meteo%ny))
    if(.not. allocated(mapfac))                                            &
         & allocate(mapfac(meteo%nx,meteo%ny))
    if(.not. allocated(u))                                                 &
         & allocate(u(meteo%nx,meteo%ny))
    if(.not. allocated(v))                                                 &
         & allocate(v(meteo%nx,meteo%ny))
    if(.not. allocated(vort))                                              &
         & allocate(vort(meteo%nx,meteo%ny))

    ! Define local variables

    dx       = reshape(meteo%dx,shape(dx))
    dy       = reshape(meteo%dy,shape(dy))
    mapfac   = reshape(meteo%mapfac,shape(mapfac))
    mpi_vort = 0.0

    ! Loop through local variable

    do k = 1, meteo%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,k,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Define local variables

          u = reshape(meteo%u(:,mpi_lev),shape(u))
          v = reshape(meteo%v(:,mpi_lev),shape(v))

          ! Loop through local variable

          do j = 1, meteo%ny

             ! Define local variables
             
             jp1 = min(j+1,meteo%ny)
             jm1 = max(j-1,1)

             ! Loop through local variable

             do i = 1, meteo%nx
                
                ! Define local variables
                
                ip1 = min(i+1,meteo%nx)
                im1 = max(i-1,1)

                ! Check local variable and proceed accordingly

                if((v(ip1,j) .ne. spval) .and. (v(im1,j) .ne. spval)       &
                     & .and. (u(i,jp1) .ne. spval) .and. (u(i,jm1) .ne.    &
                     & spval)) then

                   ! Compute local variables

                   dsx       = (ip1 - im1)*dx(i,j)
                   dsy       = (jp1 - jm1)*dy(i,j)
                   dvdx      = (v(ip1,j)/mapfac(ip1,j) - v(im1,j)/         &
                        & mapfac(im1,j))/dsx*(mapfac(i,j)*mapfac(i,j))
                   dudy      = (u(i,jp1)/mapfac(i,jp1) - u(i,jm1)/         &
                        & mapfac(i,jm1))/dsy*(mapfac(i,j)*mapfac(i,j))
                   vort(i,j) = dvdx - dudy

                else   ! if((v(ip1,j) .ne. spval) .and. (v(im1,j)
                       ! .ne. spval) .and. (u(i,jp1) .ne. spval)
                       ! .and. (u(i,jm1) .ne. spval))

                   ! Define local variables

                   vort(i,j) = spval

                end if ! if((v(ip1,j) .ne. spval) .and. (v(im1,j)
                       ! .ne. spval) .and. (u(i,jp1) .ne. spval)
                       ! .and. (u(i,jm1) .ne. spval))
             
             end do ! do i = 1, meteo%nx

          end do ! do j = 1, meteo%ny

          ! Define local variables

          mpi_vort(:,mpi_lev) = reshape(vort,shape(mpi_vort(:,mpi_lev)))
          if(debug) write(6,501) mpi_lev, minval(mpi_vort(:,mpi_lev),      &
               & mpi_vort(:,mpi_lev) .ne. spval),                          &
               & maxval(mpi_vort(:,mpi_lev),mpi_vort(:,mpi_lev) .ne.       &
               & spval)

       end if ! if(mpi_lev .ne. mpi_noproc_assign)

    end do ! do k = 1, meteo%nz

    ! Define local variables

    call mpi_reduce(mpi_vort,meteo%vort,(meteo%ncoords*meteo%nz),          &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_vort)) deallocate(mpi_vort)
    if(allocated(mapfac))   deallocate(mapfac)
    if(allocated(dx))       deallocate(dx)
    if(allocated(dy))       deallocate(dy)
    if(allocated(v))        deallocate(v)
    if(allocated(u))        deallocate(u)
    if(allocated(vort))     deallocate(vort)

    ! Define local variables

    call mpi_bcast(meteo%vort,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('RGNL_VORTICITY: Computing regional relative vorticity.')
501 format('RGNL_VORTICITY (level/min/max): ',i,2(f13.5))

    !=====================================================================

  end subroutine rgnl_vorticity

  !=======================================================================

  ! SUBROUTINE:

  ! rh2wvmxrt.f90

  ! DESCRIPTION:

  ! This subroutine will compute the water-vapor mixing ratio profile
  ! from the relative humidity and saturation water-vapor mixing
  ! ratio profiles.

  ! INPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing (at
  !   minimum) the relative humidity (rh; ratio) and saturation
  !   water-vapor mixing ratio (swvmxrt; kilograms per kilogram)
  !   profiles.

  ! OUTPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing the
  !   water-vapor mixing ratio (wvmxrt; kilograms per kilogram)
  !   profile.

  !-----------------------------------------------------------------------

  subroutine rh2wvmxrt(moisture)

    ! Define variables passed to routine

    type(moisture_struct)                                               :: moisture

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_wvmxrt
    integer                                                             :: mpi_id

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_wvmxrt))                                        &
         & allocate(mpi_wvmxrt(moisture%ncoords,moisture%nz))

    ! Define local variables

    call mpi_interface_gettaskid(mpi_procid,mpi_id)
    mpi_wvmxrt = 0.0

    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

       ! Loop through local variable

       do j = 1, moisture%nz

          ! Check local variable and proceed accordingly

          if((moisture%swvmxrt(i,j) .ne. spval) .and. (moisture%rh(i,j)    &
               & .ne. spval)) then
       
             ! Compute local variables

             mpi_wvmxrt(i,j) = moisture%swvmxrt(i,j)*moisture%rh(i,j)

             ! Check local variable and proceed accordingly

             if(mpi_wvmxrt(i,j) .lt. 0.0) mpi_wvmxrt(i,j) = clpval

          else   ! if((moisture%swvmxrt(i,j) .ne. spval)
                 ! .and. (moisture%rh(i,j) .ne. spval))

             ! Define local variables

             mpi_wvmxrt(i,j) = spval

          end if ! if((moisture%swvmxrt(i,j) .ne. spval)
                 ! .and. (moisture%rh(i,j) .ne. spval))

       end do ! do j = 1, moisture%nz

    end do ! do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_wvmxrt,moisture%wvmxrt,(moisture%ncoords*          &
         & moisture%nz),mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,    &
         & mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_wvmxrt)) deallocate(mpi_wvmxrt)

    ! Define local variables

    call mpi_bcast(moisture%wvmxrt,(moisture%ncoords*moisture%nz),         &
         & mpi_real,mpi_masternode,mpi_comm_world,mpi_ierror)

    !=====================================================================

  end subroutine rh2wvmxrt

  !=======================================================================

  ! SUBROUTINE:

  ! wvmxrt2q.f90

  ! DESCRIPTION:

  ! This subroutine will compute the specific humidity from the
  ! water-vapor mixing ratio.

  ! INPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing (at
  !   minimum) the water-vapor mixing ratio (wvmxrt; kilograms per
  !   kilogram) profile.

  ! OUTPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing the
  !   specfic humidity (q; kilograms per kilogram) profile.

  !-----------------------------------------------------------------------

  subroutine wvmxrt2q(moisture)

    ! Define variables passed to routine

    type(moisture_struct)                                               :: moisture

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_q
    integer                                                             :: mpi_id

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_q))                                             &
         & allocate(mpi_q(moisture%ncoords,moisture%nz))

    ! Define local variables

    call mpi_interface_gettaskid(mpi_procid,mpi_id)
    mpi_q = 0.0

    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

       ! Loop through local variable

       do j = 1, moisture%nz

          ! Check local variable and proceed accordingly

          if(moisture%wvmxrt(i,j) .ne. spval) then

             ! Compute local variables

             mpi_q(i,j) = moisture%wvmxrt(i,j)/(1.0 +                      &
                  & moisture%wvmxrt(i,j))

             ! Check local variable and proceed accordingly

             if(mpi_q(i,j) .lt. 0.0) mpi_q(i,j) = clpval

          else   ! if(moisture%wvmxrt(i,j) .ne. spval)

             ! Define local variables

             mpi_q(i,j) = spval

          end if ! if(moisture%wvmxrt(i,j) .ne. spval)

       end do ! do j = 1, moisture%nz

    end do ! do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_q,moisture%q,(moisture%ncoords*moisture%nz),       &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_q)) deallocate(mpi_q)

    ! Define local variables

    call mpi_bcast(moisture%q,(moisture%ncoords*moisture%nz),mpi_real,     &
         & mpi_masternode,mpi_comm_world,mpi_ierror)

    !=====================================================================
    
  end subroutine wvmxrt2q

  !=======================================================================

  ! SUBROUTINE:

  ! wvmxrt2rh.f90

  ! DESCRIPTION:

  ! This subroutine will compute the relative humidity profile from
  ! the water-vapor mixing ratio profile and the vapor pressure
  ! profile from the relative humidity and saturation vapor pressure
  ! profiles.

  ! INPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing (at
  !   minimum) the water-vapor mixing ratio (wvmxrt; kilograms per
  !   kilogram) profile.

  ! OUTPUT VARIABLES:

  ! * moisture; a FORTRAN moisture_struct variable containing the
  !   relative humidity (rh; ratio) and vapor pressure (vp; Pascals)
  !   profiles.

  !-----------------------------------------------------------------------

  subroutine wvmxrt2rh(moisture)

    ! Define variables passed to routine

    type(moisture_struct)                                               :: moisture

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: mpi_rh
    real(r_kind),               dimension(:,:),             allocatable :: mpi_vp
    integer                                                             :: mpi_id

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mpi_rh))                                            &
         & allocate(mpi_rh(moisture%ncoords,moisture%nz))
    if(.not. allocated(mpi_vp))                                            &
         & allocate(mpi_vp(moisture%ncoords,moisture%nz))

    ! Define local variables

    call mpi_interface_gettaskid(mpi_procid,mpi_id)
    mpi_rh = 0.0
    mpi_vp = 0.0

    ! Loop through local variable

    do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

       ! Loop through local variable

       do j = 1, moisture%nz

          ! Check local variable and proceed accordingly

          if((moisture%wvmxrt(i,j) .ne. spval) .and.                       &
               & (moisture%swvmxrt(i,j) .ne. spval) .and.                  &
               & (moisture%svp(i,j) .ne. spval)) then

             ! Compute local variables

             mpi_rh(i,j) = moisture%wvmxrt(i,j)/moisture%swvmxrt(i,j)

             ! Check local variable and proceed accordingly

             if(mpi_rh(i,j) .lt. 0.0) mpi_rh(i,j) = clpval

             ! Compute local variables

             mpi_vp(i,j) = mpi_rh(i,j)*moisture%svp(i,j)

             ! Check local variable and proceed accordingly

             if(mpi_vp(i,j) .lt. 0.0) mpi_vp(i,j) = clpval          

          else

             ! Define local variables

             mpi_rh(i,j) = spval
             mpi_vp(i,j) = spval

          end if ! if((moisture%wvmxrt(i,j) .ne. spval)
                 ! .and. (moisture%swvmxrt(i,j) .ne. spval)
                 ! .and. (moisture%svp(i,j) .ne. spval))
 
       end do ! do j = 1, moisture%nz

    end do ! do i = mpi_taskgrid%begin(mpi_id), mpi_taskgrid%end(mpi_id)

    ! Define local variables

    call mpi_reduce(mpi_rh,moisture%rh,(moisture%ncoords*moisture%nz),     &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_vp,moisture%vp,(moisture%ncoords*moisture%nz),     &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_rh)) deallocate(mpi_rh)
    if(allocated(mpi_vp)) deallocate(mpi_vp)

    ! Define local variables

    call mpi_bcast(moisture%rh,(moisture%ncoords*moisture%nz),mpi_real,    &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(moisture%vp,(moisture%ncoords*moisture%nz),mpi_real,    &
         & mpi_masternode,mpi_comm_world,mpi_ierror)

    !=====================================================================
    
  end subroutine wvmxrt2rh

  !=======================================================================

end module meteo_methods_interface

