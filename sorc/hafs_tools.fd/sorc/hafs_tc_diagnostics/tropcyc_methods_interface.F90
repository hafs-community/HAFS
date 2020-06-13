module tropcyc_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: tropcyc_methods_interface
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
  use interpolation_interface
  use kinds_interface
  use math_methods_interface
  use mpi_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: tropcyc_methods_attrs

  ! Define local variables

  type(tcdiag_struct)                                                   :: tcdiag

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! ike.f90

  ! DESCRIPTION:

  ! This subroutine estimates the integrated kinetic energy (IKE)
  ! values for tropical storm force winds (e.g., > 18 m/s) and
  ! hurricane force winds (e.g., > 33 m/s) as described in Powell and
  ! Reinhold [2007].

  ! REFERENCES:

  ! Powell, M. D. and T. A. Reinhold, 2007: Tropical Cyclone
  ! Destructive Potential by Integrated Kinetic
  ! Energy. Bull. Amer. Meteor. Soc., 88, 513–526.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing (at minimum) the
  !   latitude and longitude coordinates (lat and lon attributes,
  !   respectively) for the computational grid.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the
  !   respective IKE attributes have been defined.

  !-----------------------------------------------------------------------

  subroutine ike(grid,meteo,vrtxinfo)
  
    ! Define variables passed to routine
    
    type(grid_struct)                                                   :: grid
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: volume
    real(r_kind)                                                        :: wnd10m

    ! Define counting variables

    integer                                                             :: i
  
    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(volume)) allocate(volume(meteo%nx*meteo%ny))

    ! Compute local variables

    call grid_methods_polarcoords(grid,vrtxinfo%reflon,vrtxinfo%reflat)
    volume = meteo%dx*meteo%dy

    ! Define local variables

    vrtxinfo%ike_hur = 0.0
    vrtxinfo%ike_ts  = 0.0
    
    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)

       ! Check local variable and proceed accordingly

       if(grid%radius(i) .le. vrtxinfo%vrtxsz) then

          ! Define local variables

          wnd10m = sqrt(meteo%u10m(i)*meteo%u10m(i) + meteo%v10m(i)*       &
               & meteo%v10m(i))

          ! Check local variable and proceed accordingly

          if(wnd10m .gt. 33.0) then

             ! Compute local variables

             vrtxinfo%ike_hur = vrtxinfo%ike_hur + (0.5*wnd10m*wnd10m)*    &
                  & volume(i)

          end if ! if(wnd10m .gt. 33.0)

          ! Check local variable and proceed accordingly

          if(wnd10m .gt. 18.0) then

             ! Compute local variables

             vrtxinfo%ike_ts = vrtxinfo%ike_hur + (0.5*wnd10m*wnd10m)*     &
                  & volume(i)

          end if ! if(wnd10m .gt. 18.0)

       end if ! if(grid%radius(i) .le. vrtxinfo%vrtxsz)

    end do ! do i = 1, (meteo%nx*meteo%ny)
    
    ! Define local variables

    vrtxinfo%ike_hur = vrtxinfo%ike_hur/(10.0**12)
    vrtxinfo%ike_ts  = vrtxinfo%ike_ts/(10.0**12)    

    ! Deallocate memory for local variables

    if(allocated(volume)) deallocate(volume)
    
    !=====================================================================

  end subroutine ike
  
  !=======================================================================

  ! SUBROUTINE:

  ! mslp.f90

  ! DESCRIPTION:

  ! This subroutine refines the reference geographical location (e.g.,
  ! longitude and latitude) relative to the location of the minium
  ! sea-level pressure (MSLP) value.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing (at minimum) the
  !   latitude and longitude coordinates (lat and lon attributes,
  !   respectively) for the computational grid.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the reference
  !   geographical location (e.g., reflon and reflat) have been
  !   updated to the position of the minimum sea-level pressure (mslp)
  !   relative to the reference location.

  !-----------------------------------------------------------------------

  subroutine mslp(grid,meteo,vrtxinfo)
  
    ! Define variables passed to routine
    
    type(grid_struct)                                                   :: grid
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid
    real(r_kind),               dimension(:),               allocatable :: mask
    integer                                                             :: minidx
    
    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mask)) allocate(mask((meteo%nx*meteo%ny)))
    
    ! Define local variables

    mask       = 0.0
    where(grid%radius .le. tcdiag%srchradius) mask = 1.0
    vargrid%nx = count(mask .eq. 1.0)
    vargrid%ny = 1
    call variable_interface_setup_struct(vargrid)
    j          = 0
    
    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)

       ! Check local variable and proceed accordingly

       if(mask(i) .eq. 1.0) then

          ! Define local variables
       
          j              = j + 1
          vargrid%var(j) = meteo%pmsl(i) 

       end if ! if(mask(i) .eq. 1.0)
          
    end do ! do i = 1, (grid%nx*grid%ny)

    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables

    vrtxinfo%mslp = statgrid%varmin
    minidx        = -9999
    
    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)
       
       ! Check local variable and proceed accordingly

       if(mask(i) .eq. 1.0 .and. meteo%pmsl(i) .eq. vrtxinfo%mslp) then

          ! Define local variable

          minidx = i

       end if ! if(mask(i) .eq. 1.0 .and. meteo%pmsl(i)
              ! .eq. vrtxinfo%mslp)

    end do ! do i = 1, (grid%nx*grid%ny)

    ! Check local variable and proceed accordingly

    if(minidx .ne. -9999) then

       ! Define local variables

       vrtxinfo%reflat = meteo%xlat(minidx)
       vrtxinfo%reflon = meteo%xlon(minidx)
       
    end if ! if(minidx .ne. -9999)
    
    ! Deallocate memory for local variables

    if(allocated(mask)) deallocate(mask)
 
    !=====================================================================
    
  end subroutine mslp

  !=======================================================================

  ! SUBROUTINE:

  ! oci.f90

  ! DESCRIPTION:

  ! This subroutine computes the attributes of the outer-most closed
  ! isobar (OCI) of the respective tropical cyclone (TC), namely the
  ! pressure and radius of the OCI. This is accomplished as follows:
  !
  ! 1. A mask is defined, relative to the TC, where the sea-level
  !    pressure is less than or equal to a threshold pressure
  !    specified by the user.
  !
  ! 2. The pressure of the outer-most closed isobar (POCI) is computed
  !    by finding the minimum isobar to occur within radial distance
  !    bins as defined by a user specified radial distance interval.
  !
  ! 3. The mask is then refined by setting all locations where the
  !    sea-level pressure is greater than POCI and subsequently
  !    searching for discontinuties.
  !
  ! 4. The radius of the outer-most closed isobar (ROCI) is determined
  !    by locating the maximum radial distance where the POCI occurs.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the pressure
  !   of the outer-most closed isobar (poci) and radius of outer-most
  !   closed isobar (roci) have been defined.

  !-----------------------------------------------------------------------

  subroutine oci(grid,meteo,vrtxinfo)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(spline_struct)                                                 :: spline
    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    real(r_kind),               dimension(:),               allocatable :: mask
    real(r_kind),               dimension(:),               allocatable :: radius_arr
    real(r_kind)                                                        :: angle
    real(r_kind)                                                        :: maxradius
    real(r_kind)                                                        :: radius   
    real(r_kind)                                                        :: roci
    integer                                                             :: nangle
    
    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    vrtxinfo%poci = vrtxinfo%mslp
    maxradius     = tcdiag%vrtxsz
    nangle        = int(360.0/tcdiag%dangle) + 1

    ! Allocate memory for local variables

    if(.not. allocated(mask))       allocate(mask((meteo%nx*meteo%ny)))
    if(.not. allocated(radius_arr)) allocate(radius_arr(nangle))

    ! Compute local variables

    call grid_methods_polarcoords(grid,vrtxinfo%reflon,vrtxinfo%reflat)
    
    ! Define local variables

    mask       = 0.0
    vargrid%nx = (meteo%nx*meteo%ny)
    vargrid%ny = 1
    call variable_interface_setup_struct(vargrid)
    
    ! Loop through local variable
    
    do i = 1, (meteo%nx*meteo%ny)
       
       ! Check local variable and proceed accordingly
     
       if((grid%radius(i) .le. maxradius) .and. (meteo%pmsl(i) .le.       &
            & 102000.0)) then
             
          ! Define local variables
                
          mask(i) = 1.0

       end if ! if((grid%radius(i) .le. maxradius)
              ! .and. (meteo%pmsl(i) .le. 102000.0))

    end do ! do i = 1, (meteo%nx*meteo%ny)

    ! Define local variables

    radius = 0.0

    ! Loop through local variables

    do while(radius .le. maxradius)

       ! Define local variables

       vargrid%var = spval

       ! Loop through local variable
       
       do i = 1, (meteo%nx*meteo%ny)
       
          ! Check local variable and proceed accordingly
          
          if((mask(i) .eq. 1.0) .and. (grid%radius(i) .ge. radius) .and.  &
               & (grid%radius(i) .lt. (radius + tcdiag%dradius))) then

             ! Define local variables
             
             vargrid%var(i) = meteo%pmsl(i)
             
          end if ! if((mask(i) .eq. 1.0) .and. (grid%radius(i)
                 ! .ge. radius) .and. (grid%radius(i) .lt. (radius +
                 ! tcdiag%dradius)))

       end do ! do i = 1, (meteo%nx*meteo%ny)

       ! Compute local variables

       call math_methods_stats(vargrid,statgrid)

       ! Check local variable and proceed accordingly

       if(statgrid%varmin .ne. spval) then

          ! Define local variables

          vrtxinfo%poci = max(vrtxinfo%poci,statgrid%varmin)

       end if ! if(statgrid%varmin .ne. spval)

       ! Define local variables
       
       radius = radius + tcdiag%dradius

    end do ! do while(radius .le. maxradius)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vargrid)
    
    ! Define local variables

    where(meteo%pmsl .ge. vrtxinfo%poci) mask = 0.0
    angle = 0.0
    
    ! Loop through local variable

    do while(angle .le. 360.0)

       ! Define local variables

       j = 0
       
       ! Loop through local variable
       
       do i = 1, (meteo%nx*meteo%ny)
          
          ! Check local variable and proceed accordingly
        
          if(grid%angle(i) .ge. angle .and. grid%angle(i) .lt. (angle     &
               & + tcdiag%dangle)) then

             ! Define local variables

             j = j + 1
             
          end if ! if(grid%angle(i) .ge. angle .and. grid%angle(i)
                 ! .lt. (angle + tcdiag%dangle))
             
       end do ! do i = 1, (meteo%nx*meteo%ny)

       ! Define local variables

       spline%n = j
       call variable_interface_setup_struct(spline)
       j        = 0
       
       ! Loop through local variable
       
       do i = 1, (meteo%nx*meteo%ny)
          
          ! Check local variable and proceed accordingly
             
          if(grid%angle(i) .ge. angle .and. grid%angle(i) .lt. (angle     &
               & + tcdiag%dangle)) then

             ! Define local variables

             j            = j + 1
             spline%xa(j) = grid%radius(i)
             spline%ya(j) = mask(i)
             
          end if ! if(grid%angle(i) .ge. angle .and. grid%angle(i)
                 ! .lt. (angle + tcdiag%dangle))
             
       end do ! do i = 1, (meteo%nx*meteo%ny)

       ! Loop through local variable

       do i = 1, spline%n

          ! Define local variables

          maxradius = spline%xa(i)
          
          ! Check local variable and proceed accordingly

          if(spline%ya(i) .eq. 0.0) then

             ! Define local variables

             goto 1000

          end if ! if(spline%ya(i) .eq. 0.0)

       end do ! do i = 1, spline%n

       ! Define local variables

1000   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(spline)
       
       ! Loop through local variable
       
       do i = 1, (meteo%nx*meteo%ny)
          
          ! Check local variable and proceed accordingly
         
          if(grid%angle(i) .ge. angle .and. grid%angle(i) .lt. (angle     &
               & + tcdiag%dangle)) then

             ! Check local variable and proceed accordingly

             if(grid%radius(i) .ge. maxradius) then

                ! Define local variables

                mask(i) = 0.0

             end if ! if(grid%radius(i) .ge. maxradius)
             
          end if ! if(grid%angle(i) .ge. angle .and. grid%angle(i)
                 ! .lt. (angle + tcdiag%dangle))
             
       end do ! do i = 1, (meteo%nx*meteo%ny)

       ! Define local variables

       angle = angle + tcdiag%dangle
       
    end do ! do while(angle .le. 360.0)

    ! Define local variables
    
    angle      = 0.0
    nangle     = 0
    radius_arr = spval
    
    ! Loop through local variable

    do while(angle .le. 360.0)

       ! Define local variables

       j      = 0
       nangle = nangle + 1
       roci   = -spval
       
       ! Loop through local variable

       do i = 1, (meteo%nx*meteo%ny)

          ! Check local variable and proceed accordingly

          if(mask(i) .eq. 1.0 .and. grid%angle(i) .ge. angle .and.        &
               & grid%angle(i) .lt. (angle + tcdiag%dangle)) then
          
             ! Define local variables

             j = j + 1
             
          end if ! if(mask(i) .eq. 1.0 .and. grid%angle(i) .ge. angle
                 ! .and. grid%angle(i) .lt. (angle + tcdiag%dangle))
             
       end do ! do i = 1, (meteo%nx*meteo%ny)

       ! Define local variables

       spline%n = j
       call variable_interface_setup_struct(spline)
       j        = 0
       
       ! Loop through local variable

       do i = 1, (meteo%nx*meteo%ny)

          ! Check local variable and proceed accordingly

          if(mask(i) .eq. 1.0 .and. grid%angle(i) .ge. angle .and.        &
               & grid%angle(i) .lt. (angle + tcdiag%dangle)) then

             ! Define local variables

             j            = j + 1
             spline%xa(j) = grid%radius(i)
             spline%ya(j) = meteo%pmsl(i)
             
          end if ! if(mask(i) .eq. 1.0 .and. grid%angle(i) .ge. angle
                 ! .and. grid%angle(i) .lt. (angle + tcdiag%dangle))
             
       end do ! do i = 1, (meteo%nx*meteo%ny)
       
       ! Compute local variables

       call math_methods_sort_array(spline,.false.,.true.)
       
       ! Loop through local variable

       do i = 1, spline%n

          ! Check local variable and proceed accordingly

          if(spline%ya(i) .le. vrtxinfo%poci) then

             ! Define local variables

             roci = max(roci,spline%xa(i))
             
          end if ! if(spline%ya(i) .ge. vrtxinfo%poci)
          
       end do ! do i = 1, spline%n

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(spline)

       ! Define local variables

       radius_arr(nangle) = roci
       angle              = angle + tcdiag%dangle
       
    end do ! do while(angle .le. 360.0)

    ! Define local variables

    vargrid%nx    = nangle
    vargrid%ny    = 1
    vargrid%nvals = (vargrid%nx*vargrid%ny)
    call variable_interface_setup_struct(vargrid)    
    where(abs(radius_arr) .eq. spval) radius_arr = spval
    vargrid%var = radius_arr
    
    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables
 
    vrtxinfo%roci = statgrid%varmax
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vargrid)
    if(allocated(mask))       deallocate(mask)
    if(allocated(radius_arr)) deallocate(radius_arr)

    !=====================================================================

  end subroutine oci

  !=======================================================================

  ! SUBROUTINE:

  ! steerdepth.f90

  ! DESCRIPTION:

  ! This subroutine computes the climatological steering depth for the
  ! respective TC event as a function of the minimum sea-level
  ! pressure.

  ! REFERENCES:

  ! Velden, C. S. and L. M. Leslie, 1991: The basic relationship
  ! between tropical cyclone intensity and the depth of the
  ! environmental steering layer in the Australian
  ! Region. Wea. Forecasting, 6, 244–253.

  ! INPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the
  !   outflow-layer attribute value, oflev, is defined.

  !-----------------------------------------------------------------------

  subroutine steerdepth(vrtxinfo)

    ! Define variables passed to routine

    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(spline_struct)                                                 :: spline

    !=====================================================================

    ! Define local variables

    spline%n  = 9
    call variable_interface_setup_struct(spline)
    spline%xa = (/100500.0,99500.0,98500.0,97500.0,96500.0,95500.0,        &
         & 94500.0,93500.0,93400.0/)
    spline%ya = (/50000.0,50000.0,50000.0,50000.0,40000.0,40000.0,         &
         & 30000.0,30000.0,30000.0/)
    spline%x  = vrtxinfo%mslp

    ! Check local variable and proceed accordingly

    if(spline%x .gt. maxval(spline%xa)) then

       ! Define local variables

       vrtxinfo%strdpth = maxval(spline%ya)

    else if(spline%x .lt. minval(spline%xa)) then

       ! Define local variables

       vrtxinfo%strdpth = minval(spline%ya)

    else   ! if(spline%x .gt. maxval(spline%xa))

       ! Compute local variables
    
       call interpolation_interface_spline(spline)

       ! Define local variables

       vrtxinfo%strdpth = spline%y

    end if ! if(spline%x .gt. maxval(spline%xa))

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(spline)

    !=====================================================================

  end subroutine steerdepth

  !=======================================================================

  ! SUBROUTINE:

  ! tcdiag_init.f90

  ! DESCRIPTION:

  ! This subroutine initializes the values for the FORTRAN
  ! tcdiag_struct variable; the user specified namelist variables are
  ! used to assign the tcdiag_struct variable attribute values.

  !-----------------------------------------------------------------------

  subroutine tcdiag_init()

    !=====================================================================

    ! Define local variables

    tcdiag%dangle      = tcd_dangle
    tcdiag%dpmsl       = tcd_dpmsl
    tcdiag%dradius     = tcd_dradius
    tcdiag%maxradius   = tcd_maxradius
    tcdiag%pvu_tcdpth  = tcd_pvu_tcdpth
    tcdiag%srcharea    = tcd_srcharea
    tcdiag%srchradius  = tcd_srchradius
    tcdiag%tcdpth_pbot = tcd_tcdpth_pbot
    tcdiag%tcdpth_ptop = tcd_tcdpth_ptop
    tcdiag%thetasfc    = tcd_thetasfc
    tcdiag%vrtxsz      = tcd_vrtxsz

    !=====================================================================

  end subroutine tcdiag_init

  !=======================================================================

  ! SUBROUTINE:

  ! tropcyc_methods_attrs.f90

  ! DESCRIPTION:

  ! This subroutine computes the tropical cyclone (TC) attributes as a
  ! function of the TC-vitals records.

  ! NOTE: Do not modify the order of TC diagnostic value calculations
  ! since the subroutines have inter-dependencies; if you modify the
  ! order of the subroutine calls for the respective TC diagnostic
  ! value calculations, you are on your own.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct containing the relevant TC-vitals
  !   records.

  ! * meteo; a FORTRAN meteo_struct containing (at minumum) the
  !   geographical coordinates (xlat and xlon) attribute variables,
  !   the sea-level pressure (pmsl), and the vertical profiles for
  !   vorticity and divergence.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable of dimension
  !   tcv%ntcs.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable of dimension
  !   tcv%ntcs containing all TC attribute values.

  !-----------------------------------------------------------------------

  subroutine tropcyc_methods_attrs(meteo,vrtxinfo,tcv)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    type(tcv_struct)                                                    :: tcv(:)
    type(vrtxinfo_struct)                                               :: vrtxinfo(size(tcv))

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call tcdiag_init()
    grid%nx  = meteo%nx
    grid%ny  = meteo%ny
    call variable_interface_setup_struct(grid)
    grid%lat = meteo%xlat
    grid%lon = meteo%xlon
    if(debug .and. (mpi_procid .eq. mpi_masternode)) write(6,500)

    ! Loop through local variable

    do i = 1, size(tcv)

       ! Define local variables

       vrtxinfo(i)%id     = tcv(i)%tcid
       vrtxinfo(i)%obslat = tcv(i)%reflat
       vrtxinfo(i)%obslon = tcv(i)%reflon

       ! Check local variable and proceed accordingly

       if(is_glblmdl .and. (vrtxinfo(i)%obslon .lt. 0))                    &
            & vrtxinfo(i)%obslon = vrtxinfo(i)%obslon + 360.0

       ! Compute local variables

       call grid_methods_polarcoords(grid,tcv(i)%reflon,tcv(i)%reflat)
       call mslp(grid,meteo,vrtxinfo(i))
       call steerdepth(vrtxinfo(i))
       call oci(grid,meteo,vrtxinfo(i))
       call vrtxsz(grid,meteo,vrtxinfo(i))
       call vrtxtlt(grid,meteo,vrtxinfo(i))
       call vrtxdpth(grid,meteo,vrtxinfo(i))
       call vmax(grid,meteo,vrtxinfo(i))
       call wradii(grid,meteo,vrtxinfo(i))
       call ike(grid,meteo,vrtxinfo(i))
       
       ! Define local variables

       if(debug .and. (mpi_procid .eq. mpi_masternode))                    &
            & call write_vrtxinfo(vrtxinfo(i))

    end do ! do i = 1, size(tcv)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    ! Define local variables

    call mpi_interface_waitall()
500 format(//,'::: FORECAST MODEL TROPICAL CYCLONE DIAGNOSTICS :::')

    !=====================================================================

  end subroutine tropcyc_methods_attrs

  !=======================================================================

  ! SUBROUTINE:

  ! vmax.f90

  ! DESCRIPTION:

  ! This subroutine computes the maximum wind attributes, including
  ! the maximum wind speed (vmax) and the radius at which the maximum
  ! wind speed occurs (i.e., the radius of maximum winds, rmw) using
  ! the wind-speed interpolated to a height of 10-meters.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing (at minimum) the
  !   latitude and longitude coordinates (lat and lon attributes,
  !   respectively) for the computational grid.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the maximum
  !   wind speed (vmax) and the radius of maximum wind (rmw)
  !   attributes have been defined.

  !-----------------------------------------------------------------------

  subroutine vmax(grid,meteo,vrtxinfo)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    real(r_kind),               dimension(:),               allocatable :: wnd10m

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(wnd10m)) allocate(wnd10m(meteo%nx*meteo%ny))

    ! Compute local variables

    call grid_methods_polarcoords(grid,vrtxinfo%reflon,vrtxinfo%reflat)

    ! Define local variables

    wnd10m = spval

    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)

       ! Check local variable and proceed accordingly

       if(grid%radius(i) .le. vrtxinfo%vrtxsz) then

          ! Define local variables

          wnd10m(i) = sqrt(meteo%u10m(i)*meteo%u10m(i) + meteo%v10m(i)*    &
               & meteo%v10m(i))

       end if ! if(grid%radius(i) .le. vrtxinfo%vrtxsz)

    end do ! do i = 1, (meteo%nx*meteo%ny)

    ! Define local variables

    vargrid%nx  = (meteo%nx*meteo%ny)
    vargrid%ny  = 1
    call variable_interface_setup_struct(vargrid)
    vargrid%var = wnd10m

    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables

    vrtxinfo%vmax = statgrid%varmax

    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)

       ! Check local variable and proceed accordingly

       if(grid%radius(i) .le. vrtxinfo%vrtxsz) then

          ! Check local variable and proceed accordingly

          if(wnd10m(i) .eq. vrtxinfo%vmax) then

             ! Define local variables

             vrtxinfo%rmw = grid%radius(i)

          end if ! if(wnd10m(i) .eq. vrtxinfo%vmax)

       end if ! if(grid%radius(i) .le. vrtxinfo%vrtxsz)

    end do ! do i = 1, (meteo%nx*meteo%ny)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vargrid)
    if(allocated(wnd10m)) deallocate(wnd10m)

    !=====================================================================

  end subroutine vmax

 !=======================================================================

  ! SUBROUTINE:

  ! vrtxdpth.f90

  ! DESCRIPTION:

  ! This subroutine estimates the pressure-level depth of the tropical
  ! cyclone (TC) vortex using the user specified PVU iso-surface
  ! within the isobaric elevations specified by the user; the search
  ! radius used to identify the model forecast/analysis minimum
  ! sea-level pressure is ued to refine the region of interest
  ! relative to the respective TC position and PVU mean value within
  ! the region is computed at each level; the depth of the TC vortex
  ! (vrtxdpth) value is determined by linearly interpolating to the
  ! specified PVU elevation in order to retrieve an estimate of the
  ! isobaric elevation for the PVU value.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the size of
  !   the TC vortex (i.e., vrtxsz; units are meters) is defined.

  !-----------------------------------------------------------------------

  subroutine vrtxdpth(grid,meteo,vrtxinfo)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(vargrid_struct)                                                :: vargrid
    type(vertgrid_struct)                                               :: vertgrid
    type(spline_struct)                                                 :: spline
    type(statgrid_struct)                                               :: statgrid
    real(r_kind),               dimension(:,:),             allocatable :: pvu
    real(r_kind),               dimension(:),               allocatable :: pvu_mean
    real(r_kind)                                                        :: latscl
    integer                                                             :: count

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(pvu))                                               &
         & allocate(pvu((meteo%nx*meteo%ny),meteo%nz))
    if(.not. allocated(pvu_mean))                                          &
         & allocate(pvu_mean(meteo%nz))

    ! Compute local variables

    call grid_methods_polarcoords(grid,vrtxinfo%reflon,vrtxinfo%reflat)
    latscl = vrtxinfo%reflat/abs(vrtxinfo%reflat)

    ! Define local variables

    pvu        = spval
    where(meteo%pv .ne. spval) pvu = (meteo%pv*1.e6)*latscl
    pvu_mean   = 0.0
    vargrid%nx = (meteo%nx*meteo%ny)
    vargrid%ny = 1
    spline%n   = 0
    call variable_interface_setup_struct(vargrid)

    ! Loop through local variable

    do j = 1, meteo%nz

       ! Define local variables

       vargrid%var = spval

       ! Loop through local variable

       do i = 1, (meteo%nx*meteo%ny)

          ! Check local variable and proceed accordingly

          if(grid%radius(i) .le. tcdiag%srchradius) then

             ! Define local variables

             vargrid%var(i) = pvu(i,j)

          end if ! if((grid%radius(i) .le. vrtxinfo%tltdist)
                 ! .and. (meteo%p(i,j) .le. tcdiag%tcdpth_pbot)
                 ! .and. (meteo%p(i,j) .ge. tcdiag%tcdpth_ptop))

       end do ! do i = 1, (meteo%nx*meteo%ny)

       ! Compute local variables

       call math_methods_stats(vargrid,statgrid)

       ! Define local variables

       pvu_mean(j) = statgrid%mean

       ! Check local variable and proceed accordingly

       if((meteo%p(1,j) .le. tcdiag%tcdpth_pbot) .and. (meteo%p(1,j) .ge.  &
            & tcdiag%tcdpth_ptop)) then

          ! Define local variables

          spline%n = spline%n + 1

       end if ! if((meteo%p(1,j) .le. tcdiag%tcdpth_pbot)
              ! .and. (meteo%p(1,j) .ge. tcdiag%tcdpth_ptop))

    end do ! do j = 1, meteo%nz

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vargrid)
    if(allocated(pvu)) deallocate(pvu)

    ! Define local variables

    call variable_interface_setup_struct(spline)
    count = 0

    ! Loop through local variable

    do j = 1, meteo%nz
    
       ! Check local variable and proceed accordingly

       if((meteo%p(1,j) .le. tcdiag%tcdpth_pbot) .and. (meteo%p(1,j) .ge.  &
            & tcdiag%tcdpth_ptop)) then
          
          ! Define local variables

          count            = count + 1
          spline%xa(count) = pvu_mean(j)
          spline%ya(count) = meteo%p(1,j)

       end if ! if((meteo%p(1,j) .le. tcdiag%tcdpth_pbot)
              ! .and. (meteo%p(1,j) .ge. tcdiag%tcdpth_ptop))

    end do ! do j = 1, meteo%nz

    ! Define local variables

    call math_methods_sort_array(spline,.true.,.false.)
    vertgrid%src_nz    = spline%n
    vertgrid%dst_nz    = 1
    call variable_interface_setup_struct(vertgrid)
    vertgrid%src_coord = spline%xa
    vertgrid%dst_coord = tcdiag%pvu_tcdpth
    vertgrid%src_var   = spline%ya

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(spline)

    ! Compute local variables

    call interpolation_interface_vertical(vertgrid,linear=.true.)

    ! Define local variables

    vrtxinfo%vrtxdpth = vertgrid%dst_var(1)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vertgrid)
    if(allocated(pvu_mean)) deallocate(pvu_mean)

    !=====================================================================

  end subroutine vrtxdpth

  !=======================================================================

  ! SUBROUTINE:

  ! vrtxsz.f90

  ! DESCRIPTION:

  ! This subroutine computes the size of the tropical cyclone (TC)
  ! vortex using the user specified (i.e., 310K) isentropic level and
  ! the user specified (i.e., 2.0) potential voriticy unit (PVU)
  ! contour.

  ! REFERENCES:

  ! Davis, C. A., S. C. Jones, and M. Riemer, 2008: Hurricane vortex
  ! dynamics during Atlantic extratropical transition. J. Atmos. Sci.,
  ! 65, 714–736.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the size of
  !   the TC vortex (i.e., vrtxsz; units are meters) is defined.

  !-----------------------------------------------------------------------

  subroutine vrtxsz(grid,meteo,vrtxinfo)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(spline_struct)                                                 :: spline
    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    type(vertgrid_struct)                                               :: vertgrid
    real(r_kind),               dimension(:),               allocatable :: isotropvu
    real(r_kind),               dimension(:),               allocatable :: work_pvu
    real(r_kind),               dimension(:),               allocatable :: work_radius
    real(r_kind),               dimension(:),               allocatable :: radius_arr
    real(r_kind)                                                        :: angle
    real(r_kind)                                                        :: radius
    integer                                                             :: work_nvals
    integer                                                             :: arr_cnt

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(isotropvu))                                         &
         & allocate(isotropvu(meteo%nx*meteo%ny))
    if(.not. allocated(work_pvu))                                          &
         & allocate(work_pvu(meteo%nx*meteo%ny))
    if(.not. allocated(work_radius))                                       &
         & allocate(work_radius(meteo%nx*meteo%ny))

    ! Define local variables

    vertgrid%src_nz = meteo%nz
    vertgrid%dst_nz = 1
    call variable_interface_setup_struct(vertgrid)

    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)

       ! Define local variables

       vertgrid%src_coord = meteo%pott(i,:)
       vertgrid%src_var   = meteo%pv(i,:)
       vertgrid%dst_coord = tcdiag%thetasfc

       ! Compute local variables

       call interpolation_interface_vertical(vertgrid,linear=.true.)

       ! Check local variable and proceed accordingly

       if(vertgrid%dst_var(1) .ne. spval) then

          ! Define local variables

          isotropvu(i) = vertgrid%dst_var(1)*1.e6

       else   ! if(vertgrid%dst_var(1) .ne. spval)

          ! Define local variables

          isotropvu(i) = spval

       end if ! if(vertgrid%dst_var(1) .ne. spval)

    end do ! do i = 1, (meteo%nx*meteo%ny)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vertgrid)

    ! Allocate memory for local variables

    if(.not. allocated(radius_arr))                                       &
         & allocate(radius_arr(meteo%nx*meteo%ny))

    ! Define local variables

    angle      = 0.0
    arr_cnt    = 0
    radius_arr = spval

    ! Loop through local variable

    do while(angle .le. 360.0)

       ! Define local variables

       work_pvu    = spval
       work_radius = spval
       work_nvals  = 0

       ! Loop through local variable
       
       do i = 1, (meteo%nx*meteo%ny)
          
          ! Check local variable and proceed accordingly

          if(grid%angle(i) .ge. angle .and. grid%angle(i) .lt.            &
               & (angle + tcdiag%dangle) .and. grid%radius(i) .le.        &
               & tcdiag%maxradius) then

             ! Define local variables

             work_nvals              = work_nvals + 1
             work_pvu(work_nvals)    = isotropvu(i)
             work_radius(work_nvals) = grid%radius(i)

          end if ! if(grid%angle(i) .ge. angle .and. grid%angle(i)
                 ! .lt. (angle + tcdiag%dangle) .and. grid%radius(i)
                 ! .le. tcdiag%maxradius)

       end do ! do i = 1, (grid%nx*grid%ny)

       ! Check local variable and proceed accordingly

       if(work_nvals .gt. 1) then

          ! Define local variables

          radius    = 0.0
          spline%n  = work_nvals
          call variable_interface_setup_struct(spline)
          spline%xa = work_radius(1:spline%n)
          spline%ya = work_pvu(1:spline%n)

          ! Loop through local variable

          do while(radius .le. tcdiag%maxradius)

             ! Define local variables

             spline%x = radius

             ! Compute local variables

             call interpolation_interface_spline(spline)

             ! Check local variable and proceed accordingly

             if(spline%y .le. 2.0) then

                ! Define local variables

                arr_cnt             = arr_cnt + 1
                radius_arr(arr_cnt) = radius

             end if ! if(spline%y .le. 2.0)

             ! Define local variables

             radius = radius + tcdiag%dradius

          end do ! do while(radius .le. tcdiag%maxradius)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(spline)

       end if ! if(work_nvals .gt. 1)

       ! Define local variables

       angle = angle + tcdiag%dangle

    end do ! do while(angle .le. 360.0)

    ! Deallocate memory for local variables

    if(allocated(isotropvu))   deallocate(isotropvu)
    if(allocated(work_pvu))    deallocate(work_pvu)
    if(allocated(work_radius)) deallocate(work_radius)

    ! Define local variables

    vargrid%nx  = size(radius_arr)
    vargrid%ny  = 1
    call variable_interface_setup_struct(vargrid)
    vargrid%var = radius_arr
    
    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Check local variable and proceed accordingly

    if(statgrid%mean .eq. spval) then

       ! Define local variables

       vrtxinfo%vrtxsz = tcdiag%maxradius

    else   ! if(statgrid%mean .eq. spval)
    
       ! Define local variables

       vrtxinfo%vrtxsz = statgrid%mean

    end if ! if(statgrid%mean .eq. spval)

    ! Deallocate local variables

    call variable_interface_cleanup_struct(vargrid)
    if(allocated(radius_arr)) deallocate(radius_arr)

    !=====================================================================

  end subroutine vrtxsz

  !=======================================================================

  ! SUBROUTINE:

  ! vrtxtlt.f90

  ! DESCRIPTION:

  ! This subroutine computes the attributes of the tropical cyclone
  ! (TC) vortex tilt -- namely, the distance from the 1-km to 12-km
  ! geometrical height potential vorticity positions (see reference
  ! below) and the angle between the aforementioned geometrical
  ! heights; with respect to the reference (below), a height of 12-km
  ! is used as opposed to 7-km in order to enable evaluation of the
  ! metrics within for the entire spectrum of plausible TC depths.

  ! REFERENCES:

  ! Davis, C. A., S. C. Jones, and M. Riemer, 2008: Hurricane vortex
  ! dynamics during Atlantic extratropical transition. J. Atmos. Sci.,
  ! 65, 714–736.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the size of
  !   the TC vortex (i.e., vrtxsz; units are meters) is defined.

  !-----------------------------------------------------------------------

  subroutine vrtxtlt(grid,meteo,vrtxinfo)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(pyththrm_struct)                                               :: pyththrm
    type(vertgrid_struct)                                               :: vertgrid
    real(r_kind),               dimension(:),               allocatable :: pvu_12km
    real(r_kind),               dimension(:),               allocatable :: pvu_1km
    real(r_kind)                                                        :: lon_12km
    real(r_kind)                                                        :: lat_12km
    real(r_kind)                                                        :: lon_1km
    real(r_kind)                                                        :: lat_1km
    real(r_kind)                                                        :: denom_12km
    real(r_kind)                                                        :: denom_1km
    real(r_kind)                                                        :: rtpt_a
    real(r_kind)                                                        :: rtpt_b
    real(r_kind)                                                        :: rtpt_c

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(pvu_1km))  allocate(pvu_1km(meteo%nx*meteo%ny))
    if(.not. allocated(pvu_12km)) allocate(pvu_12km(meteo%nx*meteo%ny))

    ! Define local variables

    vertgrid%src_nz = meteo%nz
    vertgrid%dst_nz = 1
    call variable_interface_setup_struct(vertgrid)

    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)

       ! Define local variables

       vertgrid%src_coord = meteo%z(i,:)
       vertgrid%src_var   = meteo%pv(i,:)
       vertgrid%dst_coord = 1000.0

       ! Compute local variables

       call interpolation_interface_vertical(vertgrid,linear=.true.)

       ! Define local variables

       pvu_1km(i)         = vertgrid%dst_var(1)
       vertgrid%dst_coord = 12000.0

       ! Compute local variables

       call interpolation_interface_vertical(vertgrid,linear=.true.)

       ! Define local variables

       pvu_12km(i) = vertgrid%dst_var(1)

    end do ! do i = 1, (meteo%nx*meteo%ny)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vertgrid)

    ! Define local variables

    lon_1km    = 0.0
    lat_1km    = 0.0
    denom_1km  = 0.0
    lon_12km   = 0.0
    lat_12km   = 0.0
    denom_12km = 0.0

    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)

       ! Check local variable and proceed accordingly

       if(grid%radius(i) .le. tcdiag%srcharea) then

          ! Compute local variables

          lat_1km    = lat_1km + pvu_1km(i)*vrtxinfo%reflat
          lon_1km    = lon_1km + pvu_1km(i)*vrtxinfo%reflon
          denom_1km  = denom_1km + pvu_1km(i)
          lat_12km   = lat_12km + pvu_12km(i)*vrtxinfo%reflat
          lon_12km   = lon_12km + pvu_12km(i)*vrtxinfo%reflon
          denom_12km = denom_12km + pvu_12km(i)
          
       end if ! if(grid%radius(i) .le. tcdiag%srcharea)

    end do ! do i = 1, (meteo%nx*meteo%ny)

    ! Compute local variables

    lon_1km  = lon_1km/denom_1km
    lat_1km  = lat_1km/denom_1km
    lon_12km = lon_12km/denom_12km
    lat_12km = lat_12km/denom_12km

    ! Deallocate memory for local variables

    if(allocated(pvu_12km)) deallocate(pvu_12km)
    if(allocated(pvu_1km))  deallocate(pvu_1km)

    ! Compute local variables

    call grid_methods_radialdist(lon_1km,lat_1km,lon_12km,lat_12km,        &
         & vrtxinfo%tltdist)

    ! Define local variables

    pyththrm = pyththrm_struct(a=vrtxinfo%tltdist,b=11000.0)

    ! Compute local variables

    call math_methods_pyththrm(pyththrm)
    vrtxinfo%tltangle = ((pi/2.0) - pyththrm%anga*deg2rad)*rad2deg

    !=====================================================================

  end subroutine vrtxtlt

  !=======================================================================

  ! SUBROUTINE:

  ! wradii.f90

  ! DESCRIPTION:

  ! This subroutine computes the wind radii attributes for the 34-,
  ! 50-, and 64-knot winds using the wind-speed interpolated to a
  ! height of 10-meters.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing (at minimum) the
  !   latitude and longitude coordinates (lat and lon attributes,
  !   respectively) for the computational grid.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable where the wind
  !   radii attributes have been defined.

  !-----------------------------------------------------------------------

  subroutine wradii(grid,meteo,vrtxinfo)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    type(meteo_struct)                                                  :: meteo
    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define variables computed within routine

    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    real(r_kind),               dimension(:),               allocatable :: wnd10m

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(wnd10m)) allocate(wnd10m(meteo%nx*meteo%ny))

    ! Compute local variables

    call grid_methods_polarcoords(grid,vrtxinfo%reflon,vrtxinfo%reflat)

    ! Define local variables

    wnd10m = spval

    ! Loop through local variable

    do i = 1, (meteo%nx*meteo%ny)

       ! Check local variable and proceed accordingly

       if(grid%radius(i) .le. vrtxinfo%vrtxsz) then

          ! Define local variables

          wnd10m(i) = sqrt(meteo%u10m(i)*meteo%u10m(i) + meteo%v10m(i)*    &
               & meteo%v10m(i))*1.94384

       end if ! if(grid%radius(i) .le. vrtxinfo%vrtxsz)

    end do ! do i = 1, (meteo%nx*meteo%ny)

    ! Define local variables

    vargrid%nx  = (meteo%nx*meteo%ny)
    vargrid%ny  = 1
    call variable_interface_setup_struct(vargrid)
    vargrid%var = grid%radius
    where((wnd10m .lt. 34.0) .or. (wnd10m .eq. spval)) vargrid%var = spval

    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables

    vrtxinfo%r34kt = statgrid%varmax
    if(vrtxinfo%r34kt .eq. spval) vrtxinfo%r34kt = -9.9e-6
    vargrid%var    = grid%radius
    where((wnd10m .lt. 50.0) .or. (wnd10m .eq. spval)) vargrid%var = spval

    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables
    
    vrtxinfo%r50kt = statgrid%varmax
    if(vrtxinfo%r50kt .eq. spval) vrtxinfo%r50kt = -9.9e-6
    vargrid%var    = grid%radius
    where((wnd10m .lt. 64.0) .or. (wnd10m .eq. spval)) vargrid%var = spval

    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables
    
    vrtxinfo%r64kt = statgrid%varmax
    if(vrtxinfo%r64kt .eq. spval) vrtxinfo%r64kt = -9.9e-6

    ! Deallocate memory for local variables

    if(allocated(wnd10m)) deallocate(wnd10m)
    call variable_interface_cleanup_struct(vargrid)

    !=====================================================================

  end subroutine wradii

  !=======================================================================

  ! SUBROUTINE:

  ! write_vrtxinfo.f90

  ! DESCRIPTION:

  ! This subroutine will write the computed TC vortex diagnostic
  ! values to the user's terminal.

  ! INPUT VARIABLES:

  ! * vrtxinfo; a FORTRAN vrtxinfo_struct variable containing all TC
  !   attribute values.

  !-----------------------------------------------------------------------

  subroutine write_vrtxinfo(vrtxinfo)

    ! Define variables passed to routine

    type(vrtxinfo_struct)                                               :: vrtxinfo

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    write(6,*) ''
    write(6,500) vrtxinfo%id
    write(6,502) ('-',i=1,50)
    write(6,501) 'OBSERVATION LON (degrees)          ', vrtxinfo%obslon
    write(6,501) 'OBSERVATION LAT (degrees)          ', vrtxinfo%obslat
    write(6,501) 'REFERENCE LON   (degrees)          ', vrtxinfo%reflon
    write(6,501) 'REFERENCE LAT   (degrees)          ', vrtxinfo%reflat
    write(6,501) 'VMAX            (meters per second)', vrtxinfo%vmax
    write(6,501) 'RMW             (meters)           ', vrtxinfo%rmw
    write(6,501) 'MSLP            (Pascals)          ', vrtxinfo%mslp
    write(6,501) 'POCI            (Pascals)          ', vrtxinfo%poci
    write(6,501) 'ROCI            (meters)           ', vrtxinfo%roci
    write(6,501) 'VORTEX SIZE     (meters)           ', vrtxinfo%vrtxsz
    write(6,501) 'VORTEX TILT     (degrees)          ', vrtxinfo%tltangle
    write(6,501) 'VORTEX DEPTH    (Pascals)          ', vrtxinfo%vrtxdpth
    write(6,501) 'STEERING DEPTH  (Pascals)          ', vrtxinfo%strdpth
    write(6,501) 'RADIUS 34KTS    (meters)           ', vrtxinfo%r34kt
    write(6,501) 'RADIUS 50KTS    (meters)           ', vrtxinfo%r50kt
    write(6,501) 'RADIUS 64KTS    (meters)           ', vrtxinfo%r64kt
    write(6,501) 'HURRICANE IKE   (Terra-Joules)     ', vrtxinfo%ike_hur
    write(6,501) 'STORM IKE       (Terra-Joules)     ', vrtxinfo%ike_ts
    write(6,502) ('-',i=1,50)
    write(6,*) ''
500 format('EVENT: ', a)
501 format(a35,':',1x,f13.2)
502 format(50(a1))

    !=====================================================================

  end subroutine write_vrtxinfo

  !=======================================================================

end module tropcyc_methods_interface
