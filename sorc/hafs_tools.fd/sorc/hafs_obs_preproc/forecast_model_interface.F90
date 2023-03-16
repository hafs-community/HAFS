module forecast_model_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: forecast_model_interface
  ! Copyright (C) 2019 Henry R. Winterbottom

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

  ! Review the README, within the top-level directory, which provides
  ! relevant instructions and (any) references cited by algorithms
  ! within this software suite.

  !=======================================================================
  
  ! Define associated modules and subroutines

  use bufrio_interface
  use constants_interface
  use fileio_interface
  use grid_methods_interface
  use gridprojs_interface
  use gridtrans_interface
  use kinds_interface
  use math_methods_interface
  use namelist_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: forecast_model_fv3
  interface observation_assignments
     module procedure fv3_observations
  end interface observation_assignments
  interface rotate_winds
     module procedure fv3_rotate_winds
  end interface rotate_winds
  interface write_fcstmdl_bufr
     module procedure fv3_bufr
  end interface write_fcstmdl_bufr
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! forecast_model_fv3.f90

  ! DESCRIPTION:

  ! This subroutine is the driver-level subroutine to ingest the
  ! necessary variable fields from the FV3 model forecast files and
  ! project the observations into a reference frame relative to the
  ! available geographical locations specified by the user; the BUFR
  ! observations are derived and written to an external user specified
  ! filepath.

  !-----------------------------------------------------------------------

  subroutine forecast_model_fv3()

    ! Define variables computed within routine

    type(fcstmdl_struct),       dimension(:),               allocatable :: fcstmdl
    type(tcinfo_struct),        dimension(:),               allocatable :: tcinfo
    type(fv3_struct)                                                    :: fv3
    type(grid_struct)                                                   :: grid
    type(windrotate_struct)                                             :: windrotate
    character(len=500)                                                  :: filename
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(tcinfo_filename,tcinfo)
    call fileio_interface_read(fv3,windrotate)
    
    ! Check local variable and proceed accordingly

    if(is_global) grid%ncoords   = (size(fv3_orog_filename)*fv3%nx*fv3%ny)
    if(is_regional) grid%ncoords = (fv3%nx*fv3%ny)

    ! Define local variables

    call variable_interface_setup_struct(grid)
   
    ! Check local variable and proceed accordingly
    
    if(is_rotate_winds .and. is_regional) call rotate_winds(fv3,windrotate)

    ! Deallocate memory for local variables

    call windrotate_cleanup(windrotate)

    ! Allocate memory for local variables

    if(.not. allocated(fcstmdl)) allocate(fcstmdl(size(tcinfo)))

    ! Define local variables

    fcstmdl(:)%nz = fv3%nz
    grid%lat      = fv3%lat
    grid%lon      = fv3%lon
    
    ! Loop through local variables

    do i = 1, size(tcinfo)

       ! Define local variables

       grid%gclat = tcinfo(i)%mdl_clat
       grid%gclon = tcinfo(i)%mdl_clon

       ! Compute local variables

       call grid_methods_polarcoords(grid)
       call observation_locations(grid,fcstmdl(i))
       call observation_assignments(fcstmdl(i),tcinfo(i),fv3)
       
    end do ! do i = 1, size(tcinfo)

    ! Deallocate memory for local variables

    if(allocated(tcinfo)) deallocate(tcinfo)
    call variable_interface_cleanup_struct(grid)

    ! Define local variables

    call write_fcstmdl_bufr(fcstmdl,fv3)
    
    ! Deallocate memory for local variables

    if(allocated(fcstmdl)) deallocate(fcstmdl)
    call variable_interface_cleanup_struct(fv3)
    
    !=====================================================================

  end subroutine forecast_model_fv3

  !=======================================================================

  ! SUBROUTINE:

  ! bufr_record.f90

  ! DESCRIPTION:

  ! This subroutine constructs a BUFR record and writes the respective
  ! record to the external BUFR file.

  ! INPUT VARIABLES:

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable containing the
  !   observation values; it is assumed that the wind variables have
  !   already been rotated from model space to observation space.

  ! * idx; a FORTRAN integer value specifying the horizontal
  !   coordinate value within the respective FORTRAN fcstmdl_struct
  !   variable arrays.

  ! * kdx; a FORTRAN integer value specifying the vertical coordinate
  !   value within the respective FORTRAN fcstmdl_struct variable
  !   arrays.

  ! * bufr_info; a FORTRAN bufr_info_struct variable containing the
  !   BUFR variable attributes.

  ! * bufr; a FORTRAN bufr_struct variable containing the BUFR record
  !   and filename attributes.

  ! * obid; a FORTRAN integer value used to create a unique variable
  !   identifier for the respective BUFR record.
  
  !-----------------------------------------------------------------------

  subroutine bufr_record(fcstmdl,idx,kdx,bufr_info,bufr,obid)

    ! Define variables passed to routine

    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info    
    type(fcstmdl_struct)                                                :: fcstmdl
    integer                                                             :: idx
    integer                                                             :: kdx
    integer                                                             :: obid

    ! Define variables computed within routine

    character(len=8)                                                    :: cacobid
    real(r_double)                                                      :: racobid
    
    !=====================================================================

    ! Define local variables

    equivalence(racobid,cacobid)
    write(cacobid,500) obid
    call variable_interface_setup_struct(bufr)
    bufr%hdr(1)   = dble(racobid)
    bufr%hdr(2)   = dble(fcstmdl%lon(idx))
    bufr%hdr(3)   = dble(fcstmdl%lat(idx))
    bufr%hdr(4)   = 0.0
    bufr%hdr(5)   = bufr_info%obs_type_mass
    bufr%obs(1,1) = fcstmdl%p(idx,kdx)/100.0
    bufr%obs(2,1) = fcstmdl%q(idx,kdx)*1000.0*1000.0
    bufr%obs(3,1) = (fcstmdl%t(idx,kdx) - 273.15)
    bufr%qcf(1,1) = 2.0
    bufr%qcf(2,1) = 2.0
    bufr%qcf(3,1) = 2.0    
    call bufrio_interface_write(bufr)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(bufr)

    ! Define local variables

    call variable_interface_setup_struct(bufr)
    bufr%hdr(1)   = dble(racobid)
    bufr%hdr(2)   = dble(fcstmdl%lon(idx))
    bufr%hdr(3)   = dble(fcstmdl%lat(idx))
    bufr%hdr(4)   = 0.0
    bufr%hdr(5)   = bufr_info%obs_type_wind
    bufr%obs(1,1) = fcstmdl%p(idx,kdx)/100.0
    bufr%obs(4,1) = fcstmdl%u(idx,kdx)
    bufr%obs(5,1) = fcstmdl%v(idx,kdx)
    bufr%qcf(1,1) = 2.0
    bufr%qcf(4,1) = 2.0  
    call bufrio_interface_write(bufr)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(bufr)    
    
    ! Define local variables

500 format(i8.8)

    !=====================================================================
    
  end subroutine bufr_record
    
  !=======================================================================

  ! SUBROUTINE:

  ! fv3_bufr.f90

  ! DESCRIPTION:

  ! This subroutine writes the FV3 forecast model derived observations
  ! to an external user specified BUFR file.

  ! INPUT VARIABLES:

  ! * fcstmdl; an array of FORTRAN fcstmdl_struct variables which has
  !   been populated with the contents from the FORTRAN fv3_struct
  !   variable relative to the respective geographical locations.

  ! * fv3; a FORTRAN fv3_struct variable containing with variable
  !   arrays which have been populated from the contents of the
  !   external FV3 netcdf files.

  !-----------------------------------------------------------------------

  subroutine fv3_bufr(fcstmdl,fv3)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: fcstmdl(:)
    type(fv3_struct)                                                    :: fv3

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info
    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid    
    type(kdtree_struct)                                                 :: kdtree
    character(len=500)                                                  :: lbufr_filepath
    character(len=500)                                                  :: filename
    integer                                                             :: nobs

    ! Define counting variables

    integer                                                             :: i, j, k

    !=====================================================================

    ! Define local variables

    bufr_info%filename = bufr_info_filepath
    call fileio_interface_read(bufr_info)
    bufr%hdstr         = 'SID XOB YOB DHR TYP'
    bufr%obstr         = 'POB QOB TOB UOB VOB'
    bufr%qcstr         = 'PQM QQM TQM WQM'
    bufr%oestr         = 'POE QOE TOE WOE'
    bufr%subset        = trim(adjustl(bufr_info%subset))
    bufr%mxmn          = 5
    bufr%mxlv          = 1
    lbufr_filepath     = trim(adjustl(bufr_filepath))
    call bufrio_interface_idate(analdate,bufr)
    call bufrio_interface_open(lbufr_filepath,bufr_tblpath,bufr,.false.,   &
         & .false.,.true.)    
    nobs               = 0
    
    ! Loop through local variable

    do i = 1, size(fcstmdl)
       
       ! Check local variable and proceed accordingly

       if(.not. mask_land) then

          ! Loop through local variable

          do j = 1, fcstmdl(i)%nobs
             
             ! Loop through local variable
             
             do k = 1, fcstmdl(i)%nz
                
                ! Define local variables

                nobs = nobs + 1
                call bufr_record(fcstmdl(i),j,k,bufr_info,bufr,nobs)
                fcstmdl(i)%usage(j,k) = .true.

             end do ! do k = 1, fcstmdl(i)%nz

          end do ! do j = 1, fcstmdl(i)%nobs

       end if ! if(.not. mask_land)

       ! Check local variable and proceed accordingly
       
       if(mask_land) then

          ! Define local variables

          src_grid%ncoords = fv3%ncoords
          call variable_interface_setup_struct(src_grid)
          src_grid%lat     = fv3%lat 
          src_grid%lon     = fv3%lon          
          dst_grid%ncoords = fcstmdl(i)%nobs
          call variable_interface_setup_struct(dst_grid)
          dst_grid%lat     = fcstmdl(i)%lat 
          dst_grid%lon     = fcstmdl(i)%lon
          kdtree%ncoords   = src_grid%ncoords
          kdtree%nn        = 1
          call variable_interface_setup_struct(kdtree)

          ! Check local variable and proceed accordingly

          if(max_orog_hght .eq. spval) max_orog_hght = 0.0

          ! Compute local variables

          call math_methods_kdtree_nn(src_grid,dst_grid,kdtree)

          ! Loop through local variable

          do j = 1, fcstmdl(i)%nobs

             ! Loop through local variable

             do k = 1, fcstmdl(i)%nz
                
                ! Check local variable and proceed accordingly

                if(mask_land) then                

                   ! Check local variable and proceed accordingly

                   if((fv3%orog(kdtree%idx(j,1)) .le. max_orog_hght) .and. &
                        & (fcstmdl(i)%orog(j) .le. max_orog_hght)) then

                      ! Define local variables

                      nobs                  = nobs + 1
                      call bufr_record(fcstmdl(i),j,k,bufr_info,bufr,nobs)
                      fcstmdl(i)%usage(j,k) = .true.
                      
                   end if ! if((fv3%orog(kdtree%idx(j,1))
                          ! .le. max_orog_hght)
                          ! .and. (fcstmdl(i)%orog(j)
                          ! .le. max_orog_hght))

                end if ! if(mask_land)

             end do ! do k = 1, fcstmdl(i)%nz
                
          end do ! do j = 1, fcstmdl(i)%nobs
                       
       end if ! if(mask_land)

       ! Define local variables

       write(filename,500) trim(adjustl(datapath)), i
       call fileio_interface_write(fcstmdl(i),filename)
          
       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(fcstmdl(i))
       call variable_interface_cleanup_struct(src_grid)
       call variable_interface_cleanup_struct(dst_grid)
       call variable_interface_cleanup_struct(kdtree)
       
    end do ! do i = 1, size(fcstmdl)

    ! Define local variables

    call bufrio_interface_close(.false.,.true.)

    ! Define local variables

500 format(a,'bufr_obs_locations','_',i2.2,'.nc')
 
    !=====================================================================
    
  end subroutine fv3_bufr
    
  !=======================================================================

  ! SUBROUTINE:

  ! fv3_observations.f90

  ! DESCRIPTION:

  ! This subroutine computes and populates the arrays within the
  ! FORTRAN fcstmdl_struct variable with values from the FORTRAN
  ! fv3_struct variable; the geographical coordinate values are
  ! updated relative to the difference between the observed and the
  ! forecast model geographical positions.

  ! INPUT VARIABLES:

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable which has been
  !   initialized and observation locations assigned.

  ! * tcinfo; a FORTRAN tcinfo_struct variable now containing the
  !   tropical cyclone attributes retrieved from the user specified
  !   file.
  
  ! * fv3; a FORTRAN fv3_struct variable containing with variable
  !   arrays which have been populated from the contents of the
  !   external FV3 netcdf files.
  
  ! OUTPUT VARIABLES:

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable which has been
  !   populated with the contents from the FORTRAN fv3_struct
  !   variable.

  !-----------------------------------------------------------------------

  subroutine fv3_observations(fcstmdl,tcinfo,fv3)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: fcstmdl
    type(fv3_struct)                                                    :: fv3
    type(tcinfo_struct)                                                 :: tcinfo

    ! Define variables computed within routine

    type(fv3_struct)                                                    :: fv3_local
    real(r_kind)                                                        :: dlat
    real(r_kind)                                                        :: dlon
    
    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================  

    ! Check local variable and proceed accordingly

    if(is_relocate) then
    
       ! Compute local variables

       dlat = tcinfo%obs_clat - tcinfo%mdl_clat
       dlon = tcinfo%obs_clon - tcinfo%mdl_clon

    else  ! if(is_relocate)

       ! Define local variables

       dlat = 0.0
       dlon = 0.0

    end if ! if(is_relocate)

    ! Define local variables

    fv3_local = fv3

    ! Loop through local variable

    do i = 1, fcstmdl%nobs
 
       ! Define local variables

       fcstmdl%p(i,:)   = fv3_local%p(fcstmdl%idx(i),:)
       fcstmdl%q(i,:)   = fv3_local%q(fcstmdl%idx(i),:)
       fcstmdl%t(i,:)   = fv3_local%t(fcstmdl%idx(i),:)
       fcstmdl%u(i,:)   = fv3_local%ua(fcstmdl%idx(i),:)
       fcstmdl%v(i,:)   = fv3_local%va(fcstmdl%idx(i),:)
       fcstmdl%lat(i)   = fv3_local%lat(fcstmdl%idx(i)) + dlat
       fcstmdl%lon(i)   = fv3_local%lon(fcstmdl%idx(i)) + dlon
       fcstmdl%slmsk(i) = fv3_local%slmsk(fcstmdl%idx(i))
       
    end do ! do i = 1, fcstmdl%nobs

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(fv3_local)
           
    !=====================================================================  
    
  end subroutine fv3_observations

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_rotate_winds.f90

  ! DESCRIPTION:

  ! This subroutine rotates the FV3 forecast model vector winds to an
  ! Earth-relative rotation and interpolates the vector the FV3 grid
  ! cell centers.

  ! INPUT VARIABLES:

  ! * fv3; a FORTRAN fv3_struct variable containing (at minimum) the
  !   model grid projection information.

  ! * windrotate; a FORTRAN windrotate_struct variable containing the
  !   rotation coefficients for the grid projection.

  ! OUTPUT VARIABLES:

  ! * fv3; a FORTRAN fv3_struct variable containing the respective
  !   forecast model winds rotated to an Earth-relative coordinate
  !   system.

  !-----------------------------------------------------------------------

  subroutine fv3_rotate_winds(fv3,windrotate)

    ! Define variables passed to routine

    type(fv3_struct)                                                    :: fv3
    type(windrotate_struct)                                             :: windrotate

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: uwnd
    real(r_kind),               dimension(:,:),             allocatable :: vwnd

    ! Define counting variables

    integer                                                             :: i, j, k
    
    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(uwnd)) allocate(uwnd(fv3%nx,fv3%ny))
    if(.not. allocated(vwnd)) allocate(vwnd(fv3%nx,fv3%ny))
    
    ! Loop through local variable

    do k = 1, fv3%nz

       ! Loop through local variable

       do j = 1, fv3%ny

          ! Loop through local variable

          do i = 1, fv3%nx

             ! Compute local variables

             uwnd(i,j) = 0.5*((fv3%u(i,j,k)*windrotate%sangv(i,j) -        &
                  & fv3%v(i,j,k)*windrotate%sangu(i,j))/                   &
                  & (windrotate%cangu(i,j)*windrotate%sangv(i,j) -         &
                  & windrotate%sangu(i,j)*windrotate%cangv(i,j)) +         &
                  & (fv3%u(i,j+1,k)*windrotate%sangv(i+1,j) -              &
                  & fv3%v(i+1,j,k)*windrotate%sangu(i,j+1))/               &
                  & (windrotate%cangu(i,j+1)*windrotate%sangv(i+1,j) -     &
                  & windrotate%sangu(i,j+1)*windrotate%cangv(i+1,j)))
             vwnd(i,j) = 0.5*((fv3%u(i,j,k)*windrotate%cangv(i,j) -        &
                  & fv3%v(i,j,k)*windrotate%cangu(i,j))/                   &
                  & (windrotate%sangu(i,j)*windrotate%cangv(i,j) -         &
                  & windrotate%cangu(i,j)*windrotate%sangv(i,j)) +         &
                  & (fv3%u(i,j+1,k)*windrotate%cangv(i+1,j) -              &
                  & fv3%v(i+1,j,k)*windrotate%cangu(i,j+1))/               &
                  & (windrotate%sangu(i,j+1)*windrotate%cangv(i+1,j) -     &
                  & windrotate%cangu(i,j+1)*windrotate%sangv(i+1,j)))
             
          end do ! do i = 1, fv3%nx

       end do ! do j = 1, fv3%ny

       ! Define local variables

       fv3%ua(:,k) = reshape(uwnd,shape(fv3%ua(:,k)))
       fv3%va(:,k) = reshape(vwnd,shape(fv3%va(:,k)))
       
    end do ! do k = 1, fv3%nz
       
    ! Deallocate memory for local variables

    if(allocated(uwnd)) deallocate(uwnd)
    if(allocated(vwnd)) deallocate(vwnd)
    
    !=====================================================================

  end subroutine fv3_rotate_winds
  
  !=======================================================================

  ! SUBROUTINE:

  ! observation_locations.f90

  ! DESCRIPTION:

  ! This subroutine determines all grid locations representing
  ! observations to be processed; the algorithm below utilizes KD-tree
  ! searching algorithm to identify forecast model grid cell locations
  ! that fall within a user specified radius; further, thinning is
  ! performed in accordance with the user specified namelist variable
  ! 'sample_radius'.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the geographical
  !   locations of the forecast model grid as well as the reference
  !   location about which to determine observation locations.

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable.

  ! OUTPUT VARIABLES:

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable which has been
  !   initialized and contains the grid cell locations (idx) for
  !   observations to be processed.

  !-----------------------------------------------------------------------

  subroutine observation_locations(grid,fcstmdl)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: fcstmdl
    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    type(kdtree_struct)                                                 :: kdtree
    real(r_kind)                                                        :: radius_max
    real(r_kind)                                                        :: radius_min

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================    

    ! Define local variables
    
    kdtree%ncoords = 1
    kdtree%nn      = grid%ncoords
    call variable_interface_setup_struct(kdtree)
    kdtree%nalloc  = grid%ncoords
    kdtree%r2      = (tc_radius*tc_radius)

    ! Compute local variables

    call math_methods_kdtree_r2(grid,grid,kdtree)
    
    ! Check local variable and proceed accordingly

    if(sample_radius .eq. spval) then

       ! Define local variables

       fcstmdl%nobs = kdtree%nfound
       call variable_interface_setup_struct(fcstmdl)
       fcstmdl%idx  = kdtree%idx(1,1:kdtree%nfound)

    else   ! if(sample_radius .eq. spval)

       ! Define local variables

       fcstmdl%nobs = 0
       radius_min   = 0.0
       radius_max   = radius_min + sample_radius

       ! Loop through local variable

       do while(radius_max .le. tc_radius)

          ! Loop through local variable
       
          do i = 1, kdtree%nfound

             ! Check local variable

             if((sqrt(kdtree%r2dist(1,i)) .ge. radius_min) .and.           &
                  & (sqrt(kdtree%r2dist(1,i)) .lt. radius_max)) then

                ! Define local variables

                fcstmdl%nobs = fcstmdl%nobs + 1

             end if ! if((sqrt(kdtree%r2dist(1,i)) .ge. radius_min)
                    ! .and. (sqrt(kdtree%r2dist(1,i))
                    ! .lt. radius_max))

          end do ! do i = 1, kdtree%nfound

          ! Define local variables

          radius_min = radius_max + sample_radius
          radius_max = radius_min + sample_radius
          
       end do ! do while(radius .le. tc_radius)

       ! Define local variables

       call variable_interface_setup_struct(fcstmdl)
       fcstmdl%nobs = 0
       radius_min   = 0.0
       radius_max   = radius_min + sample_radius

       ! Loop through local variable

       do while(radius_max .le. tc_radius)

          ! Loop through local variable
       
          do i = 1, kdtree%nfound

             ! Check local variable

             if((sqrt(kdtree%r2dist(1,i)) .ge. radius_min) .and.           &
                  & (sqrt(kdtree%r2dist(1,i)) .lt. radius_max)) then

                ! Define local variables

                fcstmdl%nobs              = fcstmdl%nobs + 1
                fcstmdl%idx(fcstmdl%nobs) = kdtree%idx(1,i)

             end if ! if((sqrt(kdtree%r2dist(1,i)) .ge. radius_min)
                    ! .and. (sqrt(kdtree%r2dist(1,i))
                    ! .lt. radius_max))

          end do ! do i = 1, kdtree%nfound

          ! Define local variables

          radius_min = radius_max + sample_radius
          radius_max = radius_min + sample_radius
          
       end do ! do while(radius .le. tc_radius)       
       
    end if ! if(sample_radius .eq. spval)
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(kdtree)    

    !=====================================================================
    
  end subroutine observation_locations

  !=======================================================================
  
end module forecast_model_interface
