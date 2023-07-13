module recon_vdm_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: recon_vdm_interface
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
  use kinds_interface
  use math_methods_interface
  use meteo_methods_interface
  use namelist_interface
  use time_methods_interface
  use variable_interface
  use wmm_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: recon_vdm
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================
  
  ! SUBROUTINE:

  ! recon_vdm.f90

  ! DESCRIPTION:

  ! This is the driver routine for preparing National Oceanic and
  ! Atmospheric Administration (NOAA) National Hurricane Center (NHC)
  ! aircraft reconnissance vortex data message (VDM) files within a
  ! Binary Universal Formatted (BUFR) file.

  !-----------------------------------------------------------------------

  subroutine recon_vdm()

    ! Define variables computed within routine

    type(vdm_struct)                                                    :: vdm
    
    !=====================================================================

    ! Define local variables

    call fileio_interface_read(recon_filelist,vdm)
    
    ! Compute local variables

    call obs_locations(vdm)
    call obs_winds(vdm)

    ! Define local variables

    call vdm_bufr(vdm)
       
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vdm)
    
    !=====================================================================
    
  end subroutine recon_vdm
  
  !=======================================================================

  ! SUBROUTINE:

  ! obs_locations.f90

  ! DESCRIPTION:

  ! This subroutine estimates the flight-level observation locations
  ! using the fix location and the bearing (heading) and distance
  ! relative to the respective fix location; the flight-level bearing
  ! (heading) values are correct for the magnetic declination
  ! (variation), resulting from the reconnissance mission compass
  ! derived bearing, using the World Magnetic Model (WMM).

  ! REFERENCES:

  ! https://www.ngdc.noaa.gov/geomag/WMM/

  ! INPUT VARIABLES:

  ! * vdm; a FORTRAN vdm_struct variable.

  ! OUTPUT VARIABLES:

  ! * vdm; a FORTRAN vdm_struct variable with latitude and longitude
  !   estimates computed from the flight-level observation attributes.

  !-----------------------------------------------------------------------

  subroutine obs_locations(vdm)

    ! Define variables passed to routine

    type(vdm_struct)                                                    :: vdm

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    type(wmm_struct)                                                    :: wmm
    
    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Loop through local variable

    do i = 1, vdm%nvdm

       ! Loop through local variable
       
       do j = 1, vdm%nobs
       
          ! Define local variables

          grid%gclat         = vdm%fix_lat(i)
          grid%gclon         = vdm%fix_lon(i)
          wmm%coeff_filepath = wmm_coeff_filepath
          wmm%glat           = vdm%fix_lat(i)
          wmm%glon           = vdm%fix_lon(i)
          read(vdm%fix_time(i)(1:4),'(f4.0)') wmm%year
          
          ! Check local variable and proceed accordingly

          if((vdm%obs_head(i,j) .ne. spval) .and. (vdm%obs_dist(i,j) .ne.  &
               & spval)) then

             ! Define local variables

             wmm%alt = vdm%obs_alt(i,j)/1000.0
             
             ! Compute local variables

             call wmm_compute(wmm)

             ! Define local variables

             grid%gchead = (vdm%obs_head(i,j) + wmm%dec) + 270.0
             grid%gcdist = vdm%obs_dist(i,j)

             ! Compute local variables

             call grid_methods_gcgeo(grid)

             ! Define local variables

             vdm%obs_lat(i,j) = grid%gclat
             vdm%obs_lon(i,j) = grid%gclon

          end if ! if((vdm%obs_head(i,j) .ne. spval)
                 ! .and. (vdm%obs_dist(i,j) .ne. spval))

       end do ! do j = 1, vdm%nobs

    end do ! do i = 1, vdm%nvdm

    !=====================================================================

  end subroutine obs_locations

  !=======================================================================

  ! SUBROUTINE:

  ! obs_winds.f90

  ! DESCRIPTION:

  ! This subroutine computes the vector wind components (e.g., u- and
  ! v-) using the wind speed and direction.

  ! REFERENCES:

  ! http://jrscience.wcp.muohio.edu/downloads/VortexDataMessage.pdf

  ! INPUT VARIABLES:

  ! * vdm; a FORTRAN vdm_struct variable.

  ! OUTPUT VARIABLES:

  ! * vdm; a FORTRAN vdm_struct variable with zonal (u-) and
  !   meridional (v-) wind estimates computed from the flight-level
  !   observation attributes.

  !-----------------------------------------------------------------------

  subroutine obs_winds(vdm)

    ! Define variables passed to routine

    type(vdm_struct)                                                    :: vdm

    ! Define variables computed within routine

    real(r_kind)                                                        :: dir
    real(r_kind)                                                        :: spd
    
    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Loop through local variable

    do i = 1, vdm%nvdm

       ! Loop through local variable
       
       do j = 1, vdm%nobs

          ! Check local variable and proceed accordingly

          if((vdm%obs_wdir(i,j) .ne. spval) .and. (vdm%obs_wspd(i,j) .ne.  &
               & spval)) then

             ! Define local variables
             
             dir = (vdm%obs_wdir(i,j)*deg2rad)
             spd = vdm%obs_wspd(i,j)
          
             ! Compute local variables

             vdm%obs_u(i,j) = abs(spd)*sin(dir)
             vdm%obs_v(i,j) = abs(spd)*cos(dir)

          end if ! if((vdm%obs_wdir(i,j) .ne. spval)
                 ! .and. (vdm%obs_wspd(i,j) .ne. spval))
             
       end do ! do j = 1, vdm%nobs

    end do ! do i = 1, vdm%nvdm

    !=====================================================================
    
  end subroutine obs_winds

  !=======================================================================

  ! SUBROUTINE:

  ! vdm_bufr.f90

  ! DESCRIPTION:
  
  ! This subroutine constucts an external Binary Universal Formatted
  ! (BUFR) file containing each available observation within the
  ! FORTRAN vdm_struct variable.

  ! INPUT VARIABLES:

  ! * vdm; a FORTRAN vdm_struct variable.

  !-----------------------------------------------------------------------

  subroutine vdm_bufr(vdm)

    ! Define variables passed to routine

    type(vdm_struct)                                                    :: vdm

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info
    character(len=500)                                                  :: lbufr_filepath
    character(len=8)                                                    :: cacobid
    real(r_double)                                                      :: anljday
    real(r_double)                                                      :: obsjday
    real(r_double)                                                      :: racobid
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: mm
    integer                                                             :: nn
    integer                                                             :: ss
    integer                                                             :: yyyy

    ! Define counting variables

    integer                                                             :: i, j 

    !=====================================================================

    ! Define local variables

    equivalence(racobid,cacobid)
    call time_methods_date_attributes(analdate,yyyy,mm,dd,hh,nn,ss)
    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,anljday)
    bufr_info%filename = bufr_info_filepath
    call fileio_interface_read(bufr_info)
    bufr%hdstr         = 'SID XOB YOB DHR TYP'
    bufr%obstr         = 'POB UOB VOB'
    bufr%qcstr         = 'PQM WQM'
    bufr%oestr         = 'POE WOE'
    bufr%subset        = trim(adjustl(bufr_info%subset))
    bufr%mxmn          = 5
    bufr%mxlv          = 1
    lbufr_filepath     = trim(adjustl(bufr_filepath))
    call bufrio_interface_idate(analdate,bufr)
    call bufrio_interface_open(lbufr_filepath,bufr_tblpath,bufr,.false.,   &
         & .false.,.true.)

    ! Loop through local variable

    do i = 1, vdm%nvdm

       ! Loop through local variable
       
       do j = 1, vdm%nobs

          ! Check local variable and proceed accordingly

          if((vdm%obs_u(i,j) .ne. spval) .and. (vdm%obs_v(i,j) .ne.        &
               & spval) .and. (vdm%obs_lon(i,j) .ne. spval) .and.          &
               & (vdm%obs_lat(i,j) .ne. spval)) then
          
             ! Define local variables

             call variable_interface_setup_struct(bufr)
             call time_methods_date_attributes(vdm%obs_time(i,j),yyyy,mm,  &
                  & dd,hh,nn,ss)

             ! Compute local variables

             call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,obsjday)

             ! Define local variables

             write(cacobid,500) hh, nn, ss
             bufr%hdr(1)   = dble(racobid)
             bufr%hdr(2)   = dble(vdm%obs_lon(i,j) + 360.0)
             bufr%hdr(3)   = dble(vdm%obs_lat(i,j))
             bufr%hdr(4)   = (obsjday - anljday)*dble(24.0)
             bufr%hdr(5)   = bufr_info%obs_type_wind
             bufr%obs(1,1) = vdm%obs_plev(i,j)/100.0
             bufr%obs(2,1) = vdm%obs_u(i,j)
             bufr%obs(3,1) = vdm%obs_v(i,j)
             bufr%qcf(1,1) = 2.0
             bufr%qcf(2,1) = 2.0
             call bufrio_interface_write(bufr)

             ! Deallocate memory for local variables

             call variable_interface_cleanup_struct(bufr)

          end if ! if((vdm%obs_u(i,j) .ne. spval)
                 ! .and. (vdm%obs_v(i,j) .ne. spval)
                 ! .and. (vdm%obs_lon(i,j) .ne. spval)
                 ! .and. (vdm%obs_lat(i,j) .ne. spval))
             
       end do ! do j = 1, vdm%nobs

    end do ! do i = 1, vdm%nvdm

    ! Define local variables

    call bufrio_interface_close(.false.,.true.)
500 format('VD',a2,a2,a2)
    
    !=====================================================================

  end subroutine vdm_bufr
    
  !=======================================================================
  
end module recon_vdm_interface
