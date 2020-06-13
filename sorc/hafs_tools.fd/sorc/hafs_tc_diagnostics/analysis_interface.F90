module analysis_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: analysis_interface
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

  use fileio_interface
  use filter_interface
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
  public :: analysis

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! analysis.f90

  ! DESCRIPTION:

  ! This subroutine estimates the background-state (e.g., the
  ! meteorological state in the absence of the tropical cyclone) using
  ! filtering algorithms for the mass and momentum variables.

  ! REFERENCES:

  ! Winterbottom, H. R., and E. P. Chassignet, 2011: A vortex
  ! isolation and removal algorithm for numerical weather prediction
  ! model tropical cyclone applications, J. Adv. Model. Earth Syst.,
  ! 3, M11003.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * filter; a FORTRAN filter_struct variable containing the mass and
  !   momentum filter attributes.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the filtered
  !   (e.g., respective-TC free) meteorological variable fields.

  !-----------------------------------------------------------------------

  subroutine analysis(meteo,filter)
  
    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo

    !=====================================================================
    
    ! Compute local variables

    call mass_filter(meteo,filter)
    call momentum_filter(meteo,filter)

    !=====================================================================

  end subroutine analysis

  !=======================================================================

  ! SUBROUTINE:

  ! filter_mass.f90

  ! DESCRIPTION:

  ! This subroutine filters the thermodynamic variables using a
  ! recursive interpolation algorithm; beginning from the largest
  ! radii of the filtering region, the region is successively refined
  ! in accordance with the user namelist specifications until all
  ! radii have been interpolated to.

  ! REFERENCES:

  ! Winterbottom, H. R., and E. P. Chassignet, 2011: A vortex
  ! isolation and removal algorithm for numerical weather prediction
  ! model tropical cyclone applications, J. Adv. Model. Earth Syst.,
  ! 3, M11003.  

  ! INPUT VARIABLES:

  ! * bkgrdfltr; a FORTRAN bkgrdfltr_struct variable containing the
  !   filtering region (filter), the geographical projection (lat and
  !   lon), the radial distance relative to the reference location
  !   (radius), and the thermodynamic variables to be filtered (e.g.,
  !   pmsl, q, t, and z).

  ! OUTPUT VARIABLES:

  ! * bkgrdfltr; a FORTRAN bkgrdfltr_struct variable containing the
  !   filtered thermodynamic variables (pmsl, q, t, and z).

  !-----------------------------------------------------------------------

  subroutine filter_mass(bkgrdfltr)

    ! Define variables passed to routine

    type(bkgrdfltr_struct)                                              :: bkgrdfltr

    ! Define variables computed within subroutine

    type(smthr9pt_struct)                                               :: smthr9pt
    real(r_kind),               dimension(:,:),             allocatable :: mpi_q
    real(r_kind),               dimension(:,:),             allocatable :: mpi_t
    real(r_kind),               dimension(:,:),             allocatable :: mpi_z
    real(r_kind),               dimension(:),               allocatable :: mpi_pmsl
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables
    
    smthr9pt%nx         = bkgrdfltr%nx
    smthr9pt%ny         = bkgrdfltr%ny
    smthr9pt%thrshratio = filter_ratiomvar
    call variable_interface_setup_struct(smthr9pt)
    
    ! Allocate memory for local variables

    if(.not. allocated(mpi_q))                                             &
         & allocate(mpi_q(bkgrdfltr%ncoords,bkgrdfltr%nz))
    if(.not. allocated(mpi_t))                                             &
         & allocate(mpi_t(bkgrdfltr%ncoords,bkgrdfltr%nz))
    if(.not. allocated(mpi_z))                                             &
         & allocate(mpi_z(bkgrdfltr%ncoords,bkgrdfltr%nz))
    if(.not. allocated(mpi_pmsl))                                          &
         & allocate(mpi_pmsl(bkgrdfltr%ncoords))

    ! Define local variables

    mpi_q    = 0.0
    mpi_t    = 0.0
    mpi_z    = 0.0
    mpi_pmsl = 0.0

    ! Loop through local variable

    do i = 1, bkgrdfltr%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,i,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Check local variable and proceed accordingly

          if(debug) write(6,500) mpi_lev

          ! Define local variables

          if(mpi_lev .eq. 1) mpi_pmsl = bkgrdfltr%pmsl
          mpi_q(:,mpi_lev) = bkgrdfltr%q(:,mpi_lev)
          mpi_t(:,mpi_lev) = bkgrdfltr%t(:,mpi_lev)
          mpi_z(:,mpi_lev) = bkgrdfltr%z(:,mpi_lev)          

          ! Check local variable and proceed accordingly

          if(maxval(bkgrdfltr%filter(:,mpi_lev)) .le. 0.0) goto 1000

          ! Check local variable and proceed accordingly

          if(mpi_lev .eq. 1) then
          
             ! Define local variables

             smthr9pt%var    = mpi_pmsl
             smthr9pt%cidx   = bkgrdfltr%cidx(mpi_lev)
             smthr9pt%filter = bkgrdfltr%filter(:,mpi_lev)

             ! Compute local variables

             call smoother(smthr9pt)

             ! Define local variables

             mpi_pmsl = smthr9pt%var

          end if ! if(mpi_lev .eq. 1)

          ! Define local variables

          smthr9pt%cidx   = bkgrdfltr%cidx(mpi_lev)
          smthr9pt%filter = bkgrdfltr%filter(:,mpi_lev)
          smthr9pt%var    = mpi_q(:,mpi_lev)

          ! Compute local variables

          call smoother(smthr9pt)

          ! Define local variables

          mpi_q(:,mpi_lev) = smthr9pt%var
          smthr9pt%var     = mpi_t(:,mpi_lev)

          ! Compute local variables

          call smoother(smthr9pt)

          ! Define local variables

          mpi_t(:,mpi_lev) = smthr9pt%var
          smthr9pt%var     = mpi_z(:,mpi_lev)

          ! Compute local variables

          call smoother(smthr9pt)

          ! Define local variables

          mpi_z(:,mpi_lev) = smthr9pt%var
          
       end if ! if(mpi_lev .ne. mpi_noproc_assign)

       ! Define local variables

1000   continue

    end do ! do i = 1, bkgrdfltr%nz

    ! Define local variables

    call mpi_interface_waitall()
    call mpi_reduce(mpi_q,bkgrdfltr%q,(bkgrdfltr%ncoords*bkgrdfltr%nz),    &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_t,bkgrdfltr%t,(bkgrdfltr%ncoords*bkgrdfltr%nz),    &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_z,bkgrdfltr%z,(bkgrdfltr%ncoords*bkgrdfltr%nz),    &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_pmsl,bkgrdfltr%pmsl,bkgrdfltr%ncoords,mpi_real,    &
         & mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(mpi_q))    deallocate(mpi_q)
    if(allocated(mpi_t))    deallocate(mpi_t)
    if(allocated(mpi_q))    deallocate(mpi_z)
    if(allocated(mpi_pmsl)) deallocate(mpi_pmsl)
    call variable_interface_cleanup_struct(smthr9pt)

    ! Define local variables

    call mpi_bcast(bkgrdfltr%q,(bkgrdfltr%ncoords*bkgrdfltr%nz),mpi_real,  &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(bkgrdfltr%t,(bkgrdfltr%ncoords*bkgrdfltr%nz),mpi_real,  &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(bkgrdfltr%z,(bkgrdfltr%ncoords*bkgrdfltr%nz),mpi_real,  &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(bkgrdfltr%pmsl,bkgrdfltr%ncoords,mpi_real,              &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('FILTER_MASS: Filtering mass variables for level ', i3.3, '.')    

    !=====================================================================

  end subroutine filter_mass

  !=======================================================================

  ! SUBROUTINE:

  ! filter_momentum.f90

  ! DESCRIPTION:

  ! This subroutine filters the momentum variables (e.g., the
  ! vorticity and divergence) using a 9-point nearest-neighbor
  ! smoothing function; only values within the respective level filter
  ! region are updated by the methods within this routine.

  ! REFERENCES:

  ! Winterbottom, H. R., and E. P. Chassignet, 2011: A vortex
  ! isolation and removal algorithm for numerical weather prediction
  ! model tropical cyclone applications, J. Adv. Model. Earth Syst.,
  ! 3, M11003.  

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the momentum variables for the vorticity and divergence (vort
  !   and divg, respectively).

  ! * bkgrdfltr; a FORTRAN bkgrdfltr_struct variable containing the
  !   filtering region (filter), the geographical projection (lat and
  !   lon), the radial distance relative to the reference location
  !   (radius), and the momentum variables to be filtered (e.g., vort
  !   and divg).

  !-----------------------------------------------------------------------

  subroutine filter_momentum(meteo,bkgrdfltr)

    ! Define variables passed to routine

    type(bkgrdfltr_struct)                                              :: bkgrdfltr
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(smthr9pt_struct)                                               :: smthr9pt  
    real(r_kind),               dimension(:,:),             allocatable :: mpi_divg
    real(r_kind),               dimension(:,:),             allocatable :: mpi_vort
    real(r_kind),               dimension(:,:),             allocatable :: mpi_whrmu
    real(r_kind),               dimension(:,:),             allocatable :: mpi_whrmv
    integer                                                             :: mpi_lev

    ! Define counting variables

    integer                                                             :: i, j, k

    !=====================================================================

    ! Define local variables
    
    smthr9pt%nx         = bkgrdfltr%nx
    smthr9pt%ny         = bkgrdfltr%ny
    smthr9pt%thrshratio = filter_ratiokvar
    call variable_interface_setup_struct(smthr9pt)
    
    ! Allocate memory for local variables
   
    if(.not. allocated(mpi_divg))                                          &
         & allocate(mpi_divg(bkgrdfltr%ncoords,bkgrdfltr%nz))
    if(.not. allocated(mpi_vort))                                          &
         & allocate(mpi_vort(bkgrdfltr%ncoords,bkgrdfltr%nz))
    if(.not. allocated(mpi_whrmu))                                         &
         & allocate(mpi_whrmu(bkgrdfltr%ncoords,bkgrdfltr%nz))
    if(.not. allocated(mpi_whrmv))                                         &
         & allocate(mpi_whrmv(bkgrdfltr%ncoords,bkgrdfltr%nz))

    ! Define local variables

    mpi_divg  = 0.0
    mpi_vort  = 0.0
    mpi_whrmu = 0.0
    mpi_whrmv = 0.0
    
    ! Loop through local variable

    do k = 1, bkgrdfltr%nz

       ! Define local variables
       
       call mpi_interface_gettasklev(mpi_procid,k,mpi_lev)

       ! Check local variable and proceed accordingly

       if(mpi_lev .ne. mpi_noproc_assign) then

          ! Check local variable and proceed accordingly

          if(debug) write(6,500) mpi_lev

          ! Define local variables

          mpi_divg(:,mpi_lev)  = bkgrdfltr%divg(:,mpi_lev)
          mpi_vort(:,mpi_lev)  = bkgrdfltr%vort(:,mpi_lev)
          mpi_whrmu(:,mpi_lev) = meteo%whrmu(:,mpi_lev)
          mpi_whrmv(:,mpi_lev) = meteo%whrmv(:,mpi_lev)
          
          ! Check local variable and proceed accordingly

          if(maxval(bkgrdfltr%filter(:,mpi_lev)) .le. 0.0) goto 1000

          ! Define local variables

          smthr9pt%cidx   = bkgrdfltr%cidx(mpi_lev)
          smthr9pt%filter = bkgrdfltr%filter(:,mpi_lev)
          smthr9pt%var    = mpi_divg(:,mpi_lev)
          
          ! Compute local variables

          call smoother(smthr9pt)

          ! Define local variables

          mpi_divg(:,mpi_lev) = smthr9pt%var
          smthr9pt%var        = mpi_vort(:,mpi_lev)

          ! Compute local variables

          call smoother(smthr9pt)

          ! Define local variables

          mpi_vort(:,mpi_lev) = smthr9pt%var
          smthr9pt%var        = mpi_whrmu(:,mpi_lev)

          ! Compute local variables

          call smoother(smthr9pt)

          ! Define local variables

          mpi_whrmu(:,mpi_lev) = smthr9pt%var
          smthr9pt%var         = mpi_whrmv(:,mpi_lev)

          ! Compute local variables

          call smoother(smthr9pt)

          ! Define local variables

          mpi_whrmv(:,mpi_lev) = smthr9pt%var
          
       end if ! if(mpi_lev .ne. mpi_noproc_assign)

       ! Define local variables

1000   continue

    end do ! do k = 1, bkgrdfltr%nz

    ! Define local variables

    call mpi_interface_waitall()
    call mpi_reduce(mpi_divg,meteo%divg,(meteo%ncoords*meteo%nz),          &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_vort,meteo%vort,(meteo%ncoords*meteo%nz),          &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_whrmu,meteo%whrmu,(meteo%ncoords*meteo%nz),        &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_reduce(mpi_whrmv,meteo%whrmv,(meteo%ncoords*meteo%nz),        &
         & mpi_real,mpi_sum,mpi_masternode,mpi_comm_world,mpi_ierror)
    
    ! Deallocate memory for local variables

    if(allocated(mpi_divg))  deallocate(mpi_divg)
    if(allocated(mpi_vort))  deallocate(mpi_vort)
    if(allocated(mpi_whrmu)) deallocate(mpi_whrmu)
    if(allocated(mpi_whrmv)) deallocate(mpi_whrmv)      
    call variable_interface_cleanup_struct(smthr9pt)
        
    ! Define local variables

    call mpi_bcast(meteo%divg,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%vort,(meteo%ncoords*meteo%nz),mpi_real,           &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%whrmu,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%whrmv,(meteo%ncoords*meteo%nz),mpi_real,          &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
500 format('FILTER_MOMENTUM: Filtering momentum variables for level ',     &
         & i3.3, '.')

    !=====================================================================

  end subroutine filter_momentum
  
  !=======================================================================

  ! SUBROUTINE:

  ! mass_filter.f90

  ! DESCRIPTION:

  ! This subroutine filters the thermodynamic (e.g., mass) variables
  ! using recursive smoothing filter application.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   mass variables for temperature (t), geopotential height (z),
  !   moisture (q), and sea-level pressure (pmsl).

  ! * filter; a FORTRAN filter_struct variable.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the filtered
  !   mass variables for temperature (t), geopotential height (z),
  !   moisture (q), and sea-level pressure (pmsl).

  !-----------------------------------------------------------------------

  subroutine mass_filter(meteo,filter)
  
    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(bkgrdfltr_struct)                                              :: bkgrdfltr

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    bkgrdfltr%ncoords = meteo%ncoords
    bkgrdfltr%nx      = meteo%nx
    bkgrdfltr%ny      = meteo%ny
    bkgrdfltr%nz      = meteo%nz
    call variable_interface_setup_struct(bkgrdfltr)
    bkgrdfltr%lat     = meteo%xlat
    bkgrdfltr%lon     = meteo%xlon
    bkgrdfltr%filter  = filter%tfltr
    bkgrdfltr%radius  = filter%radius
    bkgrdfltr%pmsl    = meteo%pmsl
    bkgrdfltr%q       = meteo%q
    bkgrdfltr%t       = meteo%t
    bkgrdfltr%z       = meteo%z
    bkgrdfltr%cidx    = filter%cidx
    
    ! Compute local variables

    call filter_mass(bkgrdfltr)

    ! Define local variables

    meteo%pmsl = bkgrdfltr%pmsl
    meteo%q    = bkgrdfltr%q
    meteo%t    = bkgrdfltr%t
    meteo%z    = bkgrdfltr%z

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(bkgrdfltr)

    !=====================================================================

  end subroutine mass_filter

  !=======================================================================

  ! SUBROUTINE:

  ! momentum_filter.f90

  ! DESCRIPTION:

  ! This subroutine filters the momentum variables using a recursive
  ! smoothing filter application.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing (at minimum)
  !   the momentum variables for the harmonic zonal- and
  !   meridional-winds (whrmu and whrmv, respectively), the divergent
  !   zonal- and meridional-winds (wdivu and wdivv, respectively), and
  !   the rotational zonal- and meridional-winds (wrotu and wrotv,
  !   respectively); the total zonal- and meridional-wind fields (u
  !   and v, respectively) are computed from the sum of the respective
  !   filtered components.

  ! * filter; a FORTRAN filter_struct variable.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the filtered
  !   momentum variables for the divergent, harmonic, and rotational
  !   wind components. 

  !-----------------------------------------------------------------------

  subroutine momentum_filter(meteo,filter)
  
    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(bkgrdfltr_struct)                                              :: bkgrdfltr

    !=====================================================================

    ! Define local variables

    bkgrdfltr%ncoords = meteo%ncoords
    bkgrdfltr%nx      = meteo%nx
    bkgrdfltr%ny      = meteo%ny
    bkgrdfltr%nz      = meteo%nz
    call variable_interface_setup_struct(bkgrdfltr)
    bkgrdfltr%lat     = meteo%xlat
    bkgrdfltr%lon     = meteo%xlon
    bkgrdfltr%filter  = filter%kfltr
    bkgrdfltr%radius  = filter%radius
    bkgrdfltr%divg    = meteo%divg
    bkgrdfltr%vort    = meteo%vort
    bkgrdfltr%whrmu   = meteo%whrmu
    bkgrdfltr%whrmv   = meteo%whrmv
    bkgrdfltr%cidx    = filter%cidx

    ! Compute local variables

    call filter_momentum(meteo,bkgrdfltr)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(bkgrdfltr)

    !=====================================================================

  end subroutine momentum_filter

  !=======================================================================

  ! SUBROUTINE:

  ! smoother.f90

  ! DESCRIPTION:

  ! This subroutine applies an interative 9-point smoothing algorithm
  ! to a user specified variable field until the threshold criteria is
  ! met.

  ! INPUT VARIABLES:

  ! * smthr9pt; a FORTRAN smthr9pt_struct variable containing (at
  !   minimum) the variable to be filtered (var), the filtering region
  !   (filter), and the threshold variance change (thrshratio) for the
  !   interative/recursive 9-point smoothing algorithm.

  ! OUTPUT VARIABLES:

  ! * smthr9pt; a FORTRAN smthr9pt_struct variable containing the
  !   filtered variable (var).

  !-----------------------------------------------------------------------

  subroutine smoother(smthr9pt)

    ! Define variables passed to routine

    type(smthr9pt_struct)                                               :: smthr9pt

    ! Define variables computed within routine

    type(recenter_struct)                                               :: recenter
    type(statgrid_struct)                                               :: statgrid
    type(statgrid_struct)                                               :: statgrid_fltr
    type(vargrid_struct)                                                :: vargrid
    real(r_kind),               dimension(:,:),             allocatable :: fltr2d
    real(r_kind),               dimension(:,:),             allocatable :: mask
    real(r_kind),               dimension(:,:),             allocatable :: work    
    real(r_kind),               dimension(:),               allocatable :: varfixed
    real(r_kind)                                                        :: vari_prev
    real(r_kind)                                                        :: vari_ratio
    real(r_kind)                                                        :: vari_diff
    integer                                                             :: xmin
    integer                                                             :: xmax
    integer                                                             :: ymin
    integer                                                             :: ymax

    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(fltr2d))                                            &
         & allocate(fltr2d(smthr9pt%nx,smthr9pt%ny))
    if(.not. allocated(mask))                                              &
         & allocate(mask(smthr9pt%nx,smthr9pt%ny))
    if(.not. allocated(work))                                              &
         & allocate(work(smthr9pt%nx,smthr9pt%ny))
    if(.not. allocated(varfixed))                                          &
         & allocate(varfixed(smthr9pt%ncoords))
    
    ! Define local variables

    recenter%ncoords = smthr9pt%ncoords
    recenter%nx      = smthr9pt%nx
    recenter%ny      = smthr9pt%ny
    call variable_interface_setup_struct(recenter)
    smthr9pt%minx    = (smthr9pt%nx - 1)
    smthr9pt%maxx    = 2
    smthr9pt%miny    = (smthr9pt%ny - 1)
    smthr9pt%maxy    = 2
    vargrid%nx       = smthr9pt%ncoords
    vargrid%ny       = 1
    call variable_interface_setup_struct(vargrid)    
    recenter%ridx    = smthr9pt%cidx
    recenter%var     = smthr9pt%filter
    varfixed         = smthr9pt%var

    ! Check local variable and proceed accordingly

    if(is_glblmdl) call math_methods_recentergrid(recenter,frwd=.true.)

    ! Define local variables

    smthr9pt%filter = recenter%var
    fltr2d          = reshape(smthr9pt%filter,shape(fltr2d))
    recenter%var    = smthr9pt%var

    ! Check local variable and proceed accordingly

    if(is_glblmdl) call math_methods_recentergrid(recenter,frwd=.true.)

    ! Define local variables

    smthr9pt%var = recenter%var

    ! Loop through local variable

    do j = 1, smthr9pt%ny  
       
       ! Loop through local variable
       
       do i = 1, smthr9pt%nx    
          
          ! Check local variable and proceed accordingly

          if(fltr2d(i,j) .eq. 1.0) then

             ! Define local variables

             smthr9pt%minx = max(min(smthr9pt%minx,i),2)
             smthr9pt%maxx = min(max(smthr9pt%maxx,i),(smthr9pt%nx-1))
             smthr9pt%miny = max(min(smthr9pt%miny,j),2)
             smthr9pt%maxy = min(max(smthr9pt%maxy,j),(smthr9pt%ny-1))

          end if ! if(fltr2d(i,j) .eq. 1.0)
                
       end do ! do i = 1, smthr9pt%nx
             
    end do ! do j = 1, smthr9pt%ny

    ! Define local variables
    
    xmin = smthr9pt%minx
    xmax = smthr9pt%maxx
    ymin = smthr9pt%miny
    ymax = smthr9pt%maxy
    mask = 0.0

    ! Loop through local variable
    
    do j = ymin, ymax
       
       ! Loop through local variable
       
       do i = xmin, xmax
          
          ! Check local variable and proceed accordingly
          
          if(fltr2d(i,j) .le. 0.0) then
             
             ! Define local variables
             
             mask(i,j) = 1.0
             
          end if ! if(fltr2d(i,j) .le. 0.0)
          
       end do ! do i = xmin, xmax
       
    end do ! do j = ymin, ymax    

    ! Define local variables

    fltr2d                    = reshape(smthr9pt%var,                &
         & shape(fltr2d))
    work                      = spval
    work(xmin:xmax,ymin:ymax) = fltr2d(xmin:xmax,ymin:ymax)
    vargrid%var               = reshape(work,shape(vargrid%var))

    ! Compute local variables
    
    call math_methods_stats(vargrid,statgrid)
    call math_methods_stats(vargrid,statgrid_fltr) 
    
    ! Define local variables
    
    vari_ratio = (statgrid_fltr%vari/statgrid%vari)
    vari_prev  = statgrid_fltr%vari
    vari_diff  = 1.0

    ! Loop through local variable
    
    do while(vari_diff .ge. smthr9pt%thrshratio)
       
       ! Compute local variables
       
       call math_methods_9ptsmthr(smthr9pt)
       
       ! Define local variables
       
       fltr2d                    = reshape(smthr9pt%var,shape(fltr2d))
       work                      = spval
       work(xmin:xmax,ymin:ymax) = fltr2d(xmin:xmax,ymin:ymax)
       vargrid%var               = reshape(work,shape(vargrid%var))
       
       ! Compute local variables
       
       call math_methods_stats(vargrid,statgrid_fltr)
       vari_ratio = (statgrid_fltr%vari/statgrid%vari) 
       vari_diff  = abs(vari_ratio - vari_prev/statgrid%vari)
       vari_prev  = statgrid_fltr%vari
       
    end do ! do while(vari_diff .ge. smthr9pt%thrshratio)

    ! Define local variables

    recenter%var = smthr9pt%var

    ! Check local variable and proceed accordingly

    if(is_glblmdl) call math_methods_recentergrid(recenter,bkwd=.true.)

    ! Define local variables

    smthr9pt%var = recenter%var
    recenter%var = smthr9pt%filter

    ! Check local variable and proceed accordingly

    if(is_glblmdl) call math_methods_recentergrid(recenter,bkwd=.true.)

    ! Define local variables

    smthr9pt%filter = recenter%var

    ! Deallocate memory for local variables

    if(allocated(fltr2d))   deallocate(fltr2d)
    if(allocated(mask))     deallocate(mask)
    if(allocated(work))     deallocate(work)
    if(allocated(varfixed)) deallocate(varfixed)
    call variable_interface_cleanup_struct(recenter)
    call variable_interface_cleanup_struct(vargrid)
    
    !=====================================================================
    
  end subroutine smoother
  
  !=======================================================================

end module analysis_interface
