module ensmemgen_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: ensmemgen_interface
  ! Copyright (C) 2020 Henry R. Winterbottom

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
  use kinds_interface
  use math_methods_interface
  use meteo_methods_interface
  use namelist_interface
  use nemsio_interface
  use pattern_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: ensmem_analysis

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! compute_pattern.f90

  ! DESCRIPTION:

  ! This subroutine computes a random pattern from a Gaussian 'white'
  ! noise spectrum advanced via an auto-regressive process; an
  ! external netcdf file 'pattern.nc' is written to the namelist
  ! 'datapath' attribute which contains the geographical (e.g.,
  ! latitude and longitude) grid and the computed random number
  ! pattern.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the
  !   geographical (e.g., latitude and longitude) grid and the
  !   computed random number pattern.

  !-----------------------------------------------------------------------  

  subroutine compute_pattern(pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern

    ! Define variables computed within routine

    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: pattern_filename

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_read_pattern) then

       ! Define local variables

       call fileio_interface_read(ensmem_pattern_filename,pattern)

    end if ! if(is_read_pattern)
       
    ! Check local variable and proceed accordingly

    if(.not. is_read_pattern) then
    
       ! Check local variable and proceed accordingly
       
       if(is_nemsio) then 
          
          ! Define local variables
          
          call nemsio_interface_init(ensmem_filename,nemsio)
          pattern%nx     = nemsio%nx
          pattern%ny     = nemsio%ny
          pattern%ntrunc = nemsio%jcap
    
       end if ! if(is_nemsio)

       ! Define local variables

       call pattern_interface_compute(pattern)

       ! Check local variable and proceed accordingly

       if(is_nemsio) then     

          ! Define local variables

          pattern%lat = reshape(nemsio%lat,shape(pattern%lat))
          pattern%lon = reshape(nemsio%lon,shape(pattern%lon))

       end if ! if(is_nemsio)

       ! Define local variables

       pattern_filename = trim(adjustl(datapath))//'pattern.nc'
       call fileio_interface_write(pattern_filename,pattern)
    
       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(nemsio)
       
    end if ! if(.not. is_read_pattern)

    ! Define local variables
    
    if(debug) write(6,500) minval(pattern%rnp), maxval(pattern%rnp)
500 format('COMPUTE_PATTERN: Random number pattern values min/max = ',     &
         & f13.5,1x,f13.5)
       
    !=====================================================================

  end subroutine compute_pattern

  !=======================================================================

  ! SUBROUTINE:

  ! compute_perturbations_global.f90

  ! DESCRIPTION:

  ! This subroutine applies random perturbations to the principle
  ! components at (or above) the variance explained threshold
  ! specified by the user and updates the corresponding file for the
  ! respective ensemble member to be generated.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the
  !   computed random number pattern.

  !-----------------------------------------------------------------------

  subroutine compute_perturbations_global(pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern

    ! Define variables computed within routine

    type(json_pattern_struct),  dimension(:),               allocatable :: json

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(json)

    ! Loop through local variable

    do i = 1, size(json)

       ! Check local variable and proceed accordingly

       if(trim(adjustl(json(i)%variable_name)) .ne. 'uv') then

          ! Check local variable and proceed accordingly

          if(is_nemsio) then

             ! Compute local variables

             call nemsio_perturbations_scalar(pattern,json(i))
             
          end if ! if(is_nemsio)

       else   ! if(trim(adjustl(json(i)%variable_name)) .ne. 'uv')

          ! Check local variable and proceed accordingly

          if(is_nemsio) then
             
             ! Compute local variables
             
             call nemsio_perturbations_winds(pattern,json(i))

          end if ! if(is_nemsio)

       end if ! if(trim(adjustl(json(i)%variable_name)) .ne. 'uv')

    end do ! do i = 1, size(json)

    ! Deallocate memory for local variables

    if(allocated(json)) deallocate(json)

    !=====================================================================

  end subroutine compute_perturbations_global

  !=======================================================================

  ! SUBROUTINE:

  ! compute_perturbations_regional.f90

  ! DESCRIPTION:

  ! This subroutine applies random perturbations to the principle
  ! components at (or above) the variance explained threshold
  ! specified by the user and updates the corresponding file for the
  ! respective ensemble member to be generated.

  ! INPUT VARIABLES:
  
  ! * pattern; a FORTRAN pattern_struct variable containing the
  !   computed random number pattern.

  ! * tcv; a FORTRAN tcv_struct variable containing the TC-vitals
  !   attributes.
  
  !-----------------------------------------------------------------------

  subroutine compute_perturbations_regional(pattern,tcv)

    ! Define variables passed to routine

    type(tcv_struct)                                                    :: tcv(:)
    type(pattern_struct)                                                :: pattern

    ! Define variables computed within routine

    type(json_pattern_struct),  dimension(:),               allocatable :: json

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(json)

    ! Loop through local variable

    do i = 1, size(json)

       ! Check local variable and proceed accordingly

       if(trim(adjustl(json(i)%variable_name)) .ne. 'uv') then

          ! Check local variable and proceed accordingly

          if(is_nemsio) then

             ! Compute local variables

             call nemsio_perturbations_scalar(pattern,json(i),tcv)
             
          end if ! if(is_nemsio)

       else   ! if(trim(adjustl(json(i)%variable_name)) .ne. 'uv')

          ! Check local variable and proceed accordingly

          if(is_nemsio) then
             
             ! Compute local variables
             
             call nemsio_perturbations_winds(pattern,json(i),tcv)

          end if ! if(is_nemsio)

       end if ! if(trim(adjustl(json(i)%variable_name)) .ne. 'uv')

    end do ! do i = 1, size(json)

    ! Deallocate memory for local variables

    if(allocated(json)) deallocate(json)

    !=====================================================================

  end subroutine compute_perturbations_regional
  
  !=======================================================================

  ! SUBROUTINE:

  ! ensmem_analysis.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to apply a 'white' noise
  ! generated random pattern of perturbations to user specified
  ! analysis variables as a means to generate error growth as a means
  ! to describe/generate ensemble members.

  !-----------------------------------------------------------------------

  subroutine ensmem_analysis()

    ! Define variables computed within routine

    type(tcv_struct),           dimension(:),               allocatable :: tcv
    type(pattern_struct)                                                :: pattern

    !=====================================================================

    ! Compute local variables
       
    call compute_pattern(pattern)

    ! Check local variable and proceed accordingly

    if(is_ensgen_regional) then    

       ! Define local variables
       
       call fileio_interface_read(tcv_filename,tcv)
       
       ! Compute local variables
       
       call compute_perturbations_regional(pattern,tcv)

    end if ! if(is_ensgen_regional)
       
    ! Check local variable and proceed accordingly

    if(.not. is_ensgen_regional) then
       
       ! Compute local variables

       call compute_perturbations_global(pattern)

    end if ! if(.not. is_ensgen_regional)
       
    ! Deallocate memory for local variables

    if(allocated(tcv)) deallocate(tcv)
    call variable_interface_cleanup_struct(pattern)

    !=====================================================================

  end subroutine ensmem_analysis

  !=======================================================================

  ! SUBROUTINE:

  ! nemsio_perturbations_scalar.f90

  ! DESCRIPTION:

  ! This subroutine applies perturbations to scalar variable fields
  ! using PCA and random spatially- and temporally correlated (e.g.,
  ! Gaussian 'white' noise) patterns.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the
  !   computed random number pattern.

  ! * json; a FORTRAN json_pattern_struct variable.

  ! OPTIONAL INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable containing the TC-vitals
  !   attributes.  

  !-----------------------------------------------------------------------

  subroutine nemsio_perturbations_scalar(pattern,json,tcv)

    ! Define variables passed to routine

    type(tcv_struct), optional                                          :: tcv(:)
    type(json_pattern_struct)                                           :: json
    type(pattern_struct)                                                :: pattern

    ! Define variables computed within routine

    type(nems_struct)                                                   :: nemsio
    type(pattern_struct)                                                :: pattern_local
    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid
    type(svd_struct)                                                    :: svd
    real(r_double),             dimension(:,:),             allocatable :: svd_a
    real(r_double),             dimension(:),               allocatable :: svd_s
    integer                                                             :: idx
    integer                                                             :: idxp1

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call variable_interface_vtable_lookup(json,nemsio)
    call nemsio_interface_init(ensmem_filename,nemsio)
    pattern_local%nx  = pattern%nx
    pattern_local%ny  = pattern%ny
    call variable_interface_setup_struct(pattern_local)
    pattern_local%lat = pattern%lat
    pattern_local%lon = pattern%lon
    pattern_local%rnp = pattern%rnp

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(nemsio)

    ! Allocate memory for local variables

    if(.not. allocated(svd_a)) allocate(svd_a(nemsio%nx,nemsio%ny))
    if(.not. allocated(svd_s)) allocate(svd_s(nemsio%ny))

    ! Check local variable and proceed accordingly

    if(json%levtype .eq. 3) then

       ! Loop through local variable
       
       do i = max(json%min_lev,1), min(json%max_lev,nemsio%nz)
          
          ! Define local variables

          nemsio%lev   = i
          call fileio_interface_read(ensmem_filename,nemsio)
          if(debug) write(6,500) trim(adjustl(nemsio%varname)),           &
               & nemsio%lev, minval(nemsio%var), maxval(nemsio%var)
          svd%nx       = nemsio%nx
          svd%ny       = nemsio%ny
          call variable_interface_setup_struct(svd)
          svd%a        = reshape(dble(nemsio%var),shape(svd%a))
          svd_a        = svd%a
          svd%dcnstrct = .true.
          svd%rcnstrct = .false.
          
          ! Compute local variables

          call math_methods_pca(svd)
          call math_methods_pca_vari_cutoff(svd,json%vari_thresh,idx,     &
               & idxp1)

          ! Define local variables

          svd_s                    = svd%s
          svd%s                    = dble(0.0)
          svd%s(idxp1:size(svd%s)) = svd_s(idxp1:size(svd%s))
          svd%dcnstrct             = .false.
          svd%rcnstrct             = .true.

          ! Compute local variables

          call math_methods_pca(svd)

          ! Define local variables

          vargrid%nx  = svd%nx
          vargrid%ny  = svd%ny
          call variable_interface_setup_struct(vargrid)
          vargrid%var = reshape(real(svd%a),shape(vargrid%var))

          ! Compute local variables

          call math_methods_stats(vargrid,statgrid)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(vargrid)

          ! Define local variables

          pattern_local%rnp_max_scale = statgrid%vari
          pattern_local%rnp_min_scale = -1.0*statgrid%vari

          ! Check local variable and proceed accordingly

          if(is_ensgen_regional) then
          
             ! Compute local variables

             call rescale_rnp(pattern_local,tcv)

          else   ! if(is_ensgen_regional)

             ! Compute local variables

             call rescale_rnp(pattern_local)

          end if ! if(is_ensgen_regional)
             
          ! Define local variables

          svd_a      = svd_a + svd%a*dble(pattern_local%rnp)
          nemsio%var = reshape(real(svd_a),shape(nemsio%var))

          ! Check local variable and proceed accordingly

          if(json%clip) then 

             ! Define local variables

             where(nemsio%var .lt. 0.0) nemsio%var = clpval

          end if ! if(json%clip)

          ! Define local variables

          if(debug) write(6,501) trim(adjustl(nemsio%varname)),            &
               & nemsio%lev, minval(nemsio%var), maxval(nemsio%var)
          call fileio_interface_write(ensmem_filename,nemsio)

          ! Deallocate memory for local variables
          
          call variable_interface_cleanup_struct(nemsio)
          call variable_interface_cleanup_struct(svd)

       end do ! do i = max(json%min_lev,1),
              ! min(json%max_lev,nemsio%nz)

    end if ! if(json%levtype .eq. 3)

    ! Deallocate memory for local variables

    if(allocated(svd_a)) deallocate(svd_a)
    if(allocated(svd_s)) deallocate(svd_s)
    call variable_interface_cleanup_struct(pattern_local)

    ! Define local variables

500 format('NEMSIO_PERTURBATIONS_SCALER: Input variable ',a,1x,            &
         & 'level = ',i3.3,1x,'min/max = ',2f13.5)
501 format('NEMSIO_PERTURBATIONS_SCALER: Output variable ',a,1x,           &
         & 'level = ',i3.3,1x,'min/max = ',2f13.5)

    !=====================================================================

  end subroutine nemsio_perturbations_scalar

  !=======================================================================

  ! SUBROUTINE:

  ! nemsio_perturbations_vector.f90

  ! DESCRIPTION:

  ! This subroutine applies perturbations to vector (derived) variable
  ! fields using PCA and random spatially- and temporally correlated
  ! (e.g., Gaussian 'white' noise) patterns.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the
  !   computed random number pattern.

  ! * json; a FORTRAN json_pattern_struct variable.

  ! OPTIONAL INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable containing the TC-vitals
  !   attributes.

  !-----------------------------------------------------------------------

  subroutine nemsio_perturbations_winds(pattern,json,tcv)

    ! Define variables passed to routine

    type(tcv_struct), optional                                          :: tcv(:)
    type(json_pattern_struct)                                           :: json
    type(pattern_struct)                                                :: pattern

    ! Define variables computed within routine

    type(meteo_struct)                                                  :: meteo
    type(nems_struct)                                                   :: nemsio
    type(pattern_struct)                                                :: pattern_local
    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid
    type(svd_struct)                                                    :: svd
    real(r_double),             dimension(:,:),             allocatable :: svd_a
    real(r_double),             dimension(:),               allocatable :: svd_s
    real(r_kind),               dimension(:,:),             allocatable :: wspd
    integer                                                             :: idx
    integer                                                             :: idxp1
    
    ! Define counting variables

    integer                                                             :: i    

    !=====================================================================

    ! Define local variables

    call variable_interface_vtable_lookup(json,nemsio)
    call nemsio_interface_init(ensmem_filename,nemsio)
    pattern_local%nx  = pattern%nx
    pattern_local%ny  = pattern%ny
    call variable_interface_setup_struct(pattern_local)
    pattern_local%lat = pattern%lat
    pattern_local%lon = pattern%lon
    pattern_local%rnp = pattern%rnp
    meteo%nx          = nemsio%nx
    meteo%ny          = nemsio%ny
    meteo%nz          = 1
    call variable_interface_setup_struct(meteo)
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(nemsio)

    ! Allocate memory for local variables

    if(.not. allocated(svd_a)) allocate(svd_a(nemsio%nx,nemsio%ny))
    if(.not. allocated(svd_s)) allocate(svd_s(nemsio%ny))
    if(.not. allocated(wspd))  allocate(wspd(nemsio%nx,nemsio%ny))

    ! Check local variable and proceed accordingly

    if(json%levtype .eq. 3) then

       ! Loop through local variable
       
       do i = max(json%min_lev,1), min(json%max_lev,nemsio%nz)

          ! Define local variables
          
          nemsio%lev      = i
          nemsio%varname  = 'ugrd'
          call fileio_interface_read(ensmem_filename,nemsio)
          if(debug) write(6,500) trim(adjustl(nemsio%varname)),           &
               & nemsio%lev, minval(nemsio%var), maxval(nemsio%var)
          meteo%u(:,1)    = nemsio%var
          nemsio%varname  = 'vgrd'
          call fileio_interface_read(ensmem_filename,nemsio)
          if(debug) write(6,500) trim(adjustl(nemsio%varname)),           &
               & nemsio%lev, minval(nemsio%var), maxval(nemsio%var)
          meteo%v(:,1)    = nemsio%var
          meteo%wdir(:,1) = spval
          meteo%wspd(:,1) = spval        
          
          ! Compute local variables

          call meteo_methods_winds(meteo)

          ! Define local variables

          wspd         = reshape(meteo%wspd(:,1),shape(wspd))
          svd%nx       = nemsio%nx
          svd%ny       = nemsio%ny
          call variable_interface_setup_struct(svd)
          svd%a        = reshape(dble(wspd),shape(svd%a))
          svd_a        = svd%a
          svd%dcnstrct = .true.
          svd%rcnstrct = .false.
          
          ! Compute local variables

          call math_methods_pca(svd)
          call math_methods_pca_vari_cutoff(svd,json%vari_thresh,idx,     &
               & idxp1)

          ! Define local variables

          svd_s                    = svd%s
          svd%s                    = dble(0.0)
          svd%s(idxp1:size(svd%s)) = svd_s(idxp1:size(svd%s))
          svd%dcnstrct             = .false.
          svd%rcnstrct             = .true.

          ! Compute local variables

          call math_methods_pca(svd)

          ! Define local variables

          vargrid%nx  = svd%nx
          vargrid%ny  = svd%ny
          call variable_interface_setup_struct(vargrid)
          vargrid%var = reshape(real(svd%a),shape(vargrid%var))

          ! Compute local variables

          call math_methods_stats(vargrid,statgrid)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(vargrid)

          ! Define local variables

          pattern_local%rnp_max_scale = statgrid%vari
          pattern_local%rnp_min_scale = -1.0*statgrid%vari
          pattern_local%rnp           = pattern%rnp

          ! Check local variable and proceed accordingly

          if(is_ensgen_regional) then
          
             ! Compute local variables

             call rescale_rnp(pattern_local,tcv)

          else   ! if(is_ensgen_regional)

             ! Compute local variables

             call rescale_rnp(pattern_local)

          end if ! if(is_ensgen_regional)

          ! Define local variables

          svd_a           = svd_a + svd%a*dble(pattern_local%rnp)
          meteo%wspd(:,1) = reshape(svd_a,shape(meteo%wspd(:,1)))
          meteo%u(:,1)    = spval
          meteo%v(:,1)    = spval
          
          ! Compute local variables

          call meteo_methods_winds(meteo)

          ! Define local variables

          nemsio%var     = meteo%u(:,1)
          nemsio%varname = 'ugrd'
          call fileio_interface_write(ensmem_filename,nemsio)
          if(debug) write(6,501) trim(adjustl(nemsio%varname)),           &
               & nemsio%lev, minval(nemsio%var), maxval(nemsio%var)
          nemsio%var     = meteo%v(:,1)
          nemsio%varname = 'vgrd'
          call fileio_interface_write(ensmem_filename,nemsio)
          if(debug) write(6,501) trim(adjustl(nemsio%varname)),           &
               & nemsio%lev, minval(nemsio%var), maxval(nemsio%var)

          ! Deallocate memory for local variables
          
          call variable_interface_cleanup_struct(nemsio)
          call variable_interface_cleanup_struct(svd)          
          
       end do ! do i = max(json%min_lev,1),
              ! min(json%max_lev,nemsio%nz)

    end if ! if(json%levtype .eq. 3)

    ! Deallocate memory for local variables

    if(allocated(svd_a)) deallocate(svd_a)
    if(allocated(svd_s)) deallocate(svd_s)
    if(allocated(wspd))  deallocate(wspd)
    call variable_interface_cleanup_struct(meteo)
    call variable_interface_cleanup_struct(pattern_local)

    ! Define local variables

500 format('NEMSIO_PERTURBATIONS_WINDS: Input variable ',a,1x,             &
         & 'level = ',i3.3,1x,'min/max = ',2f13.5)
501 format('NEMSIO_PERTURBATIONS_WINDS: Output variable ',a,1x,            &
         & 'level = ',i3.3,1x,'min/max = ',2f13.5)
    
    !=====================================================================

  end subroutine nemsio_perturbations_winds

  !=======================================================================

  ! SUBROUTINE:

  ! rescale_rnp.f90

  ! DESCRIPTION:

  ! This subroutine rescales the values defining the random number
  ! pattern between the threshold values specified by the user.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the random
  !   number, spatially and temporally correlated, pattern within the
  !   'rnp' attribute.

  ! OPTIONAL INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable array containing the ????

  ! OUTPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the random
  !   number, spatially and temporally correlated, pattern re-scale
  !   within the interval specified by the user in the 'rnp'
  !   attribute.

  !-----------------------------------------------------------------------

  subroutine rescale_rnp(pattern,tcv)

    ! Define variables passed to routine

    type(tcv_struct), optional                                          :: tcv(:)
    type(pattern_struct)                                                :: pattern

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(kdtree_struct)                                                 :: kdtree
    real(r_kind),               dimension(:),               allocatable :: rnp
    real(r_kind),               dimension(:),               allocatable :: rnp_tcv
    real(r_kind)                                                        :: radius(4)
    real(r_kind)                                                        :: rnp_maxval
    real(r_kind)                                                        :: rnp_max
    real(r_kind)                                                        :: rnp_minval
    real(r_kind)                                                        :: rnp_min
    real(r_kind)                                                        :: tcv_lat
    real(r_kind)                                                        :: tcv_lon
    integer                                                             :: ncoords

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    rnp_maxval = maxval(pattern%rnp)
    rnp_max    = pattern%rnp_max_scale
    rnp_minval = minval(pattern%rnp)
    rnp_min    = pattern%rnp_min_scale

    ! Loop through local variable

    do j = 1, pattern%ny

       ! Loop through local variable

       do i = 1, pattern%nx

          ! Compute local variables

          pattern%rnp(i,j) = rnp_min + ((pattern%rnp(i,j) - rnp_minval)*  &
               & (rnp_max - rnp_min))/(rnp_maxval - rnp_minval)

       end do ! do i = 1, pattern%nx

    end do ! do j = 1, pattern%ny

    ! Check local variable and proceed accordingly

    if(present(tcv)) then

       ! Define local variables

       kdtree%src_ncoords = 1
       kdtree%dst_ncoords = (pattern%nx*pattern%ny)
       kdtree%nn          = (pattern%nx*pattern%ny)
       call variable_interface_setup_struct(kdtree)
       kdtree%nalloc      = (pattern%nx*pattern%ny)
       dst_grid%ncoords   = (pattern%nx*pattern%ny)
       call variable_interface_setup_struct(dst_grid)
       dst_grid%lat       = reshape(pattern%lat,shape(dst_grid%lat))
       dst_grid%lon       = reshape(pattern%lon,shape(dst_grid%lon))
       
       ! Allocate memory for local variables

       if(.not. allocated(rnp))                                           &
            & allocate(rnp(pattern%nx*pattern%ny))
       if(.not. allocated(rnp_tcv))                                       &
            & allocate(rnp_tcv(pattern%nx*pattern%ny))

       ! Define local variables

       rnp     = reshape(pattern%rnp,shape(rnp))
       rnp_tcv = 0.0
       
       ! Loop through local variable

       do i = 1, size(tcv)
          
          ! Compute local variables

          call math_methods_radialdist(tcv(i)%lon,tcv(i)%lat,             &
               & tcv(i)%lon+tc_region_area,tcv(i)%lat+tc_region_area,     &
               & radius(1))
          call math_methods_radialdist(tcv(i)%lon,tcv(i)%lat,             &
               & tcv(i)%lon-tc_region_area,tcv(i)%lat+tc_region_area,     &
               & radius(2))
          call math_methods_radialdist(tcv(i)%lon,tcv(i)%lat,             &
               & tcv(i)%lon+tc_region_area,tcv(i)%lat-tc_region_area,     &
               & radius(3))
          call math_methods_radialdist(tcv(i)%lon,tcv(i)%lat,             &
               & tcv(i)%lon-tc_region_area,tcv(i)%lat-tc_region_area,     &
               & radius(4))

          ! Define local variables

          src_grid%clat = tcv(i)%lat
          src_grid%clon = tcv(i)%lon          
          kdtree%r2     = maxval(radius)*maxval(radius)
          
          ! Compute local variables

          call math_methods_kdtree_r2(src_grid,dst_grid,kdtree)
          
          ! Loop through local variable

          do j = 1, kdtree%nfound

             ! Define local variables

             rnp_tcv(kdtree%idx(1,j)) = rnp(kdtree%idx(1,j))

          end do ! do j = 1, kdtree%nfound
          
       end do ! do i = 1, size(tcv)

       ! Define local variables

       pattern%rnp = reshape(rnp_tcv,shape(pattern%rnp))

       ! Deallocate memory for local variables

       if(allocated(rnp))     deallocate(rnp)
       if(allocated(rnp_tcv)) deallocate(rnp_tcv)
       call variable_interface_cleanup_struct(dst_grid)
       call variable_interface_cleanup_struct(kdtree)

    end if ! if(present(tcv))

    !=====================================================================

  end subroutine rescale_rnp

  !=======================================================================

end module ensmemgen_interface
