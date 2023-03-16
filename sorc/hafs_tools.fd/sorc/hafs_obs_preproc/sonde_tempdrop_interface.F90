module sonde_tempdrop_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: sonde_tempdrop_interface
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
  use netcdf_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: sonde_tempdrop
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! sonde_tempdrop.f90

  ! DESCRIPTION:

  ! This is the driver routine for the decoding and formatting of the
  ! National Oceanic and Atmospheric Administration (NOAA) Atlantic
  ! Oceanographic and Meteorological Laboratory (AOML) Hurricane
  ! Research Division (HRD) TEMP-DROP message sondes and subqeuently
  ! preparing a Binary Universal Formatted (BUFR) file.

  !-----------------------------------------------------------------------

  subroutine sonde_tempdrop()

    ! Define variables computed within routine

    type(hsa_struct),           dimension(:),               allocatable :: hsa
    type(meteo_struct)                                                  :: meteo
    type(sonde_struct)                                                  :: sonde

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(sonde)

    ! Allocate memory for local variables

    if(.not. allocated(hsa)) allocate(hsa(sonde%nsondes))

    ! Loop through local variable

    do i = 1, sonde%nsondes

       ! Compute local variables

       call tempdrop_to_hsa(sonde%filename(i),hsa(i),meteo)

       ! Define local variables

       call fileio_interface_write(tempdrop_hsa_table_file,                &
            & sonde%filename(i),hsa(i))

       ! Check local variable and proceed accordingly

       if(tempdrop_compute_drift) then

          ! Define local variables

          call check_tempdrop_msg(sonde%filename(i),hsa(i))

          ! Check local variable and proceed accordingly

          if(hsa(i)%process) then
          
             ! Compute local variables
             
             call sonde_drift(sonde,hsa(i),meteo)
             
          end if ! if(hsa(i)%process)

       end if ! if(tempdrop_compute_drift)
          
       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(meteo)
          
    end do ! do i = 1, sonde%nsondes

    ! Define local variables

    call sonde_bufr(hsa)

    ! Loop through local variable

    do i = 1, sonde%nsondes

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(hsa(i))
       
    end do ! do i = 1, sonde%nsondes

    ! Deallocate memory for local variables

    if(allocated(hsa)) deallocate(hsa)
    call variable_interface_cleanup_struct(sonde)
    
    !=====================================================================

  end subroutine sonde_tempdrop

  !=======================================================================

  ! SUBROUTINE:

  ! check_tempdrop_msg.f90

  ! DESCRIPTION:

  ! This subroutine checks that the parameters required to compute the
  ! drift correction are available within the respective TEMP-DROP
  ! formatted observation/message file; the attribute 'process' within
  ! the FORTRAN hsa_struct variable is updated accordingly.

  ! INPUT VARIABLES:

  ! * infile; a FORTRAN character string specifying the path to the
  !   external file containing the TEMPDROP messages.

  ! * hsa; a FORTRAN hsa_struct variable.

  ! OUTPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable where the attribute value
  !   'process' as been defined accordingly.

  !-----------------------------------------------------------------------

  subroutine check_tempdrop_msg(infile,hsa)

    ! Define variables passed to routine
    
    type(hsa_struct)                                                    :: hsa
    character(len=500)                                                  :: infile

    ! Define variables passed to routine

    character(len=70)                                                   :: line
    logical                                                             :: find_rel
    logical                                                             :: find_spg
    logical                                                             :: find_spl
    
    !=====================================================================

    ! Define local variables

    hsa%process = .false.
    find_rel    = .false.
    find_spg    = .false.
    find_spl    = .false.
    open(99,file=trim(adjustl(infile)),status='old')
100 read(99,'(a)',end=1000) line
    if(index(line,'REL') .ne. 0) find_rel = .true.
    goto 100
1000 continue
    close(99)
    open(99,file=trim(adjustl(infile)),status='old')
101 read(99,'(a)',end=1001) line
    if(index(line,'SPG') .ne. 0) find_spg = .true.
    goto 101
1001 continue
    close(99)
    open(99,file=trim(adjustl(infile)),status='old')
102 read(99,'(a)',end=1002) line
    if(index(line,'SPL') .ne. 0) find_spl = .true.
    goto 102
1002 continue
    close(99)

    ! Check local variable and proceed accordingly

    if(find_rel .and. (find_spg .or. find_spl)) then

       ! Define local variables

       hsa%process = .true.

    end if ! if(find_rel .and. (find_spg .or. find_spl))
    
    !=====================================================================

  end subroutine check_tempdrop_msg
  
  !=======================================================================

  ! SUBROUTINE:

  ! compute_drift.f90

  ! DESCRIPTION:

  ! This subroutine computes the drift-correction estimate for the
  ! sonde trajectory; the updated geographical locations and time
  ! information are written to the respective FORTRAN hsa_struct
  ! variables.

  ! INPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing the decoded
  !   TEMP-DROP attributes for the release and splash times, the
  !   timestamp attributes, and the filename to which the decoded
  !   TEMP-DROP message is written.

  ! * hsa_interp; a FORTRAN hsa_struct variable containing updated
  !   observed variable profiles obtain via interpolation.

  ! * meteo; a FORTRAN meteo_struct variable containing both observed
  !   and diagnostic variables computed from the decoded TEMP-DROP
  !   message.

  ! OUTPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing the
  !   drift-corrected HSA decoded TEMP-DROP messages.

  ! * meteo; a FORTRAN meteo_struct variable containing
  !   drift-corrected observed and diagnostic variables computed from
  !   the decoded TEMP-DROP message.

  !-----------------------------------------------------------------------

  subroutine compute_drift(hsa,hsa_interp,meteo)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa
    type(hsa_struct)                                                    :: hsa_interp
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    type(statgrid_struct)                                               :: statgrid
    character(len=8)                                                    :: yymmdd
    character(len=4)                                                    :: hhnn
    real(r_double)                                                      :: julian_time
    real(r_double)                                                      :: spg_julian
    real(r_kind)                                                        :: avgp
    real(r_kind)                                                        :: avgt
    real(r_kind)                                                        :: dtime
    real(r_kind)                                                        :: fallrate
    real(r_kind)                                                        :: gsndfall2
    real(r_kind)                                                        :: norma
    real(r_kind)                                                        :: normb
    real(r_kind)                                                        :: plat
    real(r_kind)                                                        :: plon
    real(r_kind)                                                        :: ppres
    real(r_kind)                                                        :: ptemp
    real(r_kind)                                                        :: ptime
    real(r_kind)                                                        :: spgtime
    real(r_kind)                                                        :: timediff
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: mm
    integer                                                             :: nn
    integer                                                             :: rel_hmsts
    integer                                                             :: spg_hmsts
    integer                                                             :: ss
    integer                                                             :: time_hmsts
    integer                                                             :: time_sthms
    integer                                                             :: yyyy

    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Define local variables

    call time_methods_hmsts(hsa_interp%logtime,time_hmsts)
    hsa_interp%fallrate = spval

    ! Check local variable and proceed accordingly

    if(hsa_interp%spgtime .eq. -999) then

       ! Define local variables

       statgrid%n = hsa_interp%nz
       call variable_interface_setup_struct(statgrid)
       statgrid%var = hsa_interp%t
       where(statgrid%var .le. -90.0) statgrid%var = spval     

       ! Compute local variables

       avgp     = 0.5*(hsa_interp%pmax + hsa_interp%pmin)
       call math_methods_stats(statgrid)
       avgt     = statgrid%mean
       fallrate = gsndfall2(avgp,avgt,spval,hsa_interp%psfc,.true.)/100.0       
       call time_methods_hmsts(hsa_interp%logtime,time_hmsts)
       spgtime  = float(time_hmsts) + (hsa_interp%pmax -                   &
            & hsa_interp%pmin)/fallrate
       call time_methods_sthms(int(spgtime),time_sthms)

       ! Define local variables

       hsa_interp%spgtime = time_sthms

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(statgrid)

    end if ! if(hsa_interp%spgtime .eq. -999)

    ! Compute local variables

    call time_methods_hmsts(hsa_interp%reltime,time_hmsts)

    ! Define local variables

    hsa_interp%time     = time_hmsts
    hsa_interp%fallrate = 0.0

    ! Loop through local variable

    do i = 1, hsa_interp%nz

       ! Check local variable and proceed accordingly

       if((hsa_interp%p(i) .ge. hsa_interp%pmin) .and. (hsa_interp%t(i)    &
            & .ge. -90.0)) then

	    ! Define local variables

	    ppres = hsa_interp%p(i)
	    ptemp = hsa_interp%t(i)
	    goto 1000

       end if ! if((hsa_interp%p(i) .ge. hsa_interp%pmin) .and.
              ! (hsa_interp%t(i) .ge. -90.0))

    end do ! do i = 1, hsa_interp%nz

    ! Define local variables

1000 continue
    call time_methods_hmsts(hsa_interp%reltime,time_hmsts)
    ptime                          = float(time_hmsts)
    hsa_interp%time(hsa_interp%nz) = ptime

    ! Loop through local variable

    do i = (hsa_interp%nz - 1), 1, -1

       ! Check local variable and proceed accordingly

       if(hsa_interp%t(i) .ge. -90.0) then

       	    ! Define local variables
       
	    avgp = 0.5*(ppres + hsa_interp%p(i))
            avgt = 0.5*(ptemp + hsa_interp%t(i))

            ! Compute local variables
            
            hsa_interp%fallrate(i) = gsndfall2(avgp,avgt,spval,            &
                 & hsa_interp%psfc,.true.)/100.0
            timediff               = (hsa_interp%p(i) - ppres)/            &
                 & hsa_interp%fallrate(i)
            timediff               = max(timediff,0.0)
            hsa_interp%time(i)     = ptime - timediff

            ! Define local variables

	    ppres = hsa_interp%p(i)
	    ptemp = hsa_interp%t(i)
            ptime = hsa_interp%time(i)

       end if ! if(hsa_interp%t(i) .ge. -90.0)

    end do ! do i = (hsa_interp%nz - 1), 1, -1

    ! Define local variables

    plat           = hsa%rellat
    plon           = hsa%rellon
    ptime          = hsa_interp%time(hsa_interp%nz)
    hsa_interp%lon = plon
    hsa_interp%lat = plat

    ! Loop through local variable

    do i = hsa_interp%nz, 2, -1

       ! Check local variable and proceed accordingly

       if((hsa_interp%p(i) .le. hsa_interp%pmax) .and. (hsa_interp%p(i)    &
            & .ge. hsa_interp%pmin) .and. ((hsa_interp%tail(i) .eq.        &
            & 'MANL') .or. (hsa_interp%tail(i) .eq. 'SIGL'))) then

          ! Define local variables

          grid%gclat = plat
          grid%gclon = -1.0*plon

          ! Compute local variables

          dtime     = ptime - hsa_interp%time(i)
          grid%gchead = 270.0 + atan2(hsa_interp%v(i),hsa_interp%u(i))*    &
               & rad2deg
          grid%gcdist = sqrt(hsa_interp%u(i)**2.0 + hsa_interp%v(i)**2.0)* &
               & dtime

          ! Compute local variables

          call grid_methods_gcgeo(grid)

          ! Define local variables

          hsa_interp%lon(i) = -1.0*grid%gclon
          hsa_interp%lat(i) = grid%gclat
          plat              = hsa_interp%lat(i)
          plon              = hsa_interp%lon(i)
          ptime             = hsa_interp%time(i)

       else if(hsa_interp%p(i) .eq. 1070.0) then 

          ! Define local variables

          hsa_interp%lon(i) = hsa_interp%spglon
          hsa_interp%lat(i) = hsa_interp%spglat

       else   ! if((hsa_interp%p(i) .le. hsa_interp%pmax)
              ! .and. (hsa_interp%p(i) .ge. hsa_interp%pmin)
              ! .and. ((hsa_interp%tail(i) .eq. 'MANL')
              ! .or. (hsa_interp%tail(i) .eq. 'SIGL')))

          ! Define local variables

          hsa_interp%lon(i) = hsa%lon(i)
          hsa_interp%lat(i) = hsa%lat(i)

       end if ! if((hsa_interp%p(i) .le. hsa_interp%pmax)
              ! .and. (hsa_interp%p(i) .ge. hsa_interp%pmin)
              ! .and. ((hsa_interp%tail(i) .eq. 'MANL')
              ! .or. (hsa_interp%tail(i) .eq. 'SIGL')))

    end do ! do i = hsa_interp%nz, 2, -1

    ! Define local variables

    statgrid%n   = hsa_interp%nz
    call variable_interface_setup_struct(statgrid)
    norma        = min(hsa%rellon,hsa%spglon)
    normb        = max(hsa%rellon,hsa%spglon)
    statgrid%var = hsa_interp%lon

    ! Compute local variables

    call math_methods_normalize_values(statgrid,norma,normb)

    ! Define local variables

    if(tempdrop_normalize) hsa_interp%lon = statgrid%var

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(statgrid)

    ! Define local variables

    statgrid%n   = hsa_interp%nz
    call variable_interface_setup_struct(statgrid)
    norma        = min(hsa%rellat,hsa%spglat)
    normb        = max(hsa%rellat,hsa%spglat)
    statgrid%var = hsa_interp%lat

    ! Compute local variables

    call math_methods_normalize_values(statgrid,norma,normb)

    ! Define local variables

    if(tempdrop_normalize) hsa_interp%lat = statgrid%var

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(statgrid)

    ! Define local variables

    j = 0
    
    ! Loop through local variable

    do i = 1, hsa_interp%nz

       ! Check local variable and proceed accordingly

       if((hsa_interp%p(i) .le. hsa_interp%pmax) .and. (hsa_interp%p(i)    &
            & .ge. hsa_interp%pmin) .and. ((hsa_interp%tail(i) .eq.        &
            & 'MANL') .or. (hsa_interp%tail(i) .eq. 'SIGL'))) then

          ! Compute local variables

          timediff    = (hsa_interp%time(hsa_interp%nz) -                  &
               & hsa_interp%time(i))/86400.0
          julian_time = hsa_interp%rel_julian + dble(timediff)
          
          ! Define local variables

          j              = j + 1
          meteo%lon(j)   = -1.0*dble(hsa_interp%lon(i))
          meteo%lat(j)   = dble(hsa_interp%lat(i))
          meteo%jdate(j) = julian_time

       end if ! if((hsa_interp%p(i) .le. hsa_interp%pmax)
              ! .and. (hsa_interp%p(i) .ge. hsa_interp%pmin)
              ! .and. ((hsa_interp%tail(i) .eq. 'MANL')
              ! .or. (hsa_interp%tail(i) .eq. 'SIGL')))

    end do ! do i = 1, hsa_interp%nz

    ! Loop through local variable

    do i = hsa_interp%nz, 1, -1
 
       ! Compute local variables

       timediff    = (hsa_interp%time(hsa_interp%nz) -                     &
            & hsa_interp%time(i))/86400.0
       julian_time = hsa_interp%rel_julian + dble(timediff)
          
       ! Define local variables
     
       call time_methods_gregorian_date(dble(julian_time),yyyy,mm,dd,hh,   &
            & nn,ss)       
       timediff        = real(julian_time - dble(int(julian_time)))
       write(yymmdd,'(i4,2(i2.2))') yyyy, mm, dd
       write(hhnn,'(2(i2.2))') hh, nn
       read(yymmdd(3:8),'(f7.0)') hsa%yymmdd(i)
       read(hhnn,'(i4)') hsa%gmt(i)
       hsa%lon(i) = hsa_interp%lon(i)
       hsa%lat(i) = hsa_interp%lat(i)

    end do ! do i = hsa_interp%nz, 1, -1

    ! Define local variables

    call time_methods_hmsts(hsa%spgtime,spg_hmsts)
    
    ! Check local variable and proceed accordingly

    if(spg_hmsts .ge. hsa_interp%rel_hmsts) then

       ! Define local variables
 
       hsa%spg_julian = hsa%rel_julian + dble((dble(spg_hmsts -            &
            & hsa_interp%rel_hmsts)/86400.0))

    end if ! if(spg_hmsts .ge. hsa_interp%rel_hmsts)

    ! Check local variable and proceed accordingly

    if(spg_hmsts .lt. hsa_interp%rel_hmsts) then

       ! Define local variables

       hsa%spg_julian = hsa_interp%rel_julian +                            &
            & dble(dble(spg_hmsts)/86400.0)

    end if ! if(spg_hmsts .lt. hsa_interp%rel_hmsts)

    !=====================================================================

  end subroutine compute_drift

  !=======================================================================

  ! SUBROUTINE:

  ! compute_meteo.f90

  ! DESCRIPTION:

  ! This subroutine computes the diagnostic meteorological variables
  ! along the sonde trajectory from the collected observed values;
  ! this is used in order to compute other (useful) diagnostics (e.g.,
  ! Skew-T/Log-P).

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing populated
  !   arrays for observed variables collected along the sonde
  !   trajectory.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing populated
  !   diagnostic variable arrays computed from the observed variables
  !   collected along the sonde trajectory.

  !-----------------------------------------------------------------------

  subroutine compute_meteo(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define counting variables

    integer                                                             :: i    

    !=====================================================================
    
    ! Loop through local variable

    do i = 1, meteo%nz

       ! Check local variable and proceed accordingly

       if(meteo%p(i) .gt. meteo%psfc) then

          ! Define local variables

          meteo%p(i)  = spval
          meteo%t(i)  = spval
          meteo%z(i)  = spval
          meteo%u(i)  = spval
          meteo%v(i)  = spval
          meteo%rh(i) = spval

       end if ! if(meteo%p(i) .gt. meteo%psfc)

    end do ! do i = 1, meteo%nz

    ! Compute local variables

    call meteo_methods_wvmxrt(meteo)
    call meteo_methods_dwpttemp(meteo)
    call meteo_methods_wspdwdir(meteo)

    !=====================================================================

  end subroutine compute_meteo
  
  !=======================================================================

  ! SUBROUTINE:

  ! interp_sonde.f90

  ! DESCRIPTION:

  ! This subroutine attempts to fill missing observation values along
  ! a sonde trajectory using the pressure level of the observation and
  ! other available values along the profile; this subroutine also
  ! populates the observation variables within the FORTRAN
  ! meteo_struct variable.

  ! NOTE:

  ! Only observations collected along the sonde trajectory that occur
  ! at the designated mandatory (MANL) and significant (SIGL) levels
  ! are used for interpolation.

  ! INPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! OUTPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing updated observed
  !   variable profiles obtain via interpolation.

  ! * meteo; a FORTRAN meteo_struct variable containing populated
  !   observed variable arrays.
  
  !-----------------------------------------------------------------------

  subroutine interp_sonde(hsa,meteo)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(interp_p_struct)                                               :: interp_p
    real(r_kind)                                                        :: dstp
    real(r_kind)                                                        :: dstvar
    integer                                                             :: nz

    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Define local variables
    
    where(hsa%t .le. -90.0) hsa%t = spval
    nz          = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge. hsa%pmin) &
         & .and. ((hsa%tail .eq. 'MANL') .or. (hsa%tail .eq. 'SIGL')))
    interp_p%nz = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge. hsa%pmin) &
         & .and. (hsa%t .ne. spval) .and. ((hsa%tail .eq. 'MANL') .or.    &
         & (hsa%tail .eq. 'SIGL')))
    meteo%nz    = nz
    call variable_interface_setup_struct(meteo)
    j           = 0

    ! Loop through local variables

    do i = 1, hsa%nz

       ! Check local variable and proceed accordingly

       if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin) .and.   &
            & ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) &
            & then

          ! Define local variables

          j          = j + 1
          meteo%p(j) = dble(hsa%p(i)*100.0)
          
       end if ! if(hsa%p(i) .le. hsa%pmax .and. hsa%p(i) .ge. hsa%pmin
              ! .and. ((hsa%tail(i) .eq. 'MANL' .or. hsa%tail(i)
              ! .eq. 'SIGL')))

    end do ! do i = 1, hsa%nz

    ! Define local variables

    interp_p%psfc = hsa%psfc
    meteo%psfc    = dble(hsa%psfc*100.0)

    ! Loop through local variable

    do while((interp_p%nz .lt. nz) .and. (interp_p%nz .gt. 1))

       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa%nz

          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)      &
               & .and. (hsa%t(i) .ne. spval) .and. ((hsa%tail(i) .eq.     &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa%p(i)
             interp_p%var(j) = hsa%t(i)

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%t(i) .ne. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz
       
       ! Loop through local variables

       do i = 1, hsa%nz
 
          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)      &
               & .and. (hsa%t(i) .eq. spval) .and. ((hsa%tail(i) .eq.     &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then         

             ! Define local variables

             dstp = hsa%p(i)
           
             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa%t(i) = dstvar
             goto 1000

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%t(i) .eq. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Define local variables

1000   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge.         &
            & hsa%pmin) .and. (hsa%t .ne. spval) .and. ((hsa%tail .eq.     &
            & 'MANL') .or. (hsa%tail .eq. 'SIGL')))

    end do ! do while((interp_p%nz .lt. nz) .and. (interp_p%nz
           ! .gt. 1))

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa%nz

       ! Check local variable and proceed accordingly

       if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin) .and.    &
            & ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i) .eq. 'SIGL')))  &
            & then

          ! Define local variables

          j          = j + 1
          meteo%t(j) = dble(hsa%t(i) + 273.15)
          
       end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
              ! .ge. hsa%pmin) .and. ((hsa%tail(i) .eq. 'MANL')
              ! .or. (hsa%tail(i) .eq. 'SIGL')))

    end do ! do i = 1, hsa%nz

    ! Define local variables
    
    where(hsa%t .eq. spval) hsa%t = -99.0
    where(hsa%u .le. -90.0) hsa%u = spval
    interp_p%nz  = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge.           &
         & hsa%pmin) .and. (hsa%u .ne. spval) .and. ((hsa%tail .eq.        &
         & 'MANL' .or. hsa%tail .eq. 'SIGL')))
    
    ! Loop through local variable

    do while((interp_p%nz .lt. nz) .and. (interp_p%nz .gt. 1))
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa%nz

          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)       &
               & .and. (hsa%u(i) .ne. spval) .and. ((hsa%tail(i) .eq.      &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa%p(i)
             interp_p%var(j) = hsa%u(i)

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%u(i) .ne. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Loop through local variables

       do i = 1, hsa%nz
 
          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)       &
               & .and. (hsa%u(i) .eq. spval) .and. ((hsa%tail(i) .eq.      &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then         

             ! Define local variables

             dstp = hsa%p(i)

             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa%u(i) = dstvar
             goto 1001

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%u(i) .eq. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Define local variables

1001   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge.         &
            & hsa%pmin) .and. (hsa%u .ne. spval) .and. ((hsa%tail .eq.     &
            & 'MANL') .or. (hsa%tail .eq. 'SIGL')))

    end do ! do while((interp_p%nz .lt. nz) .and. (interp_p%nz
           ! .gt. 1))

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa%nz

       ! Check local variable and proceed accordingly

       if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin) .and.    &
            & ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i) .eq. 'SIGL')))  &
            & then

          ! Define local variables

          j          = j + 1
          meteo%u(j) = dble(hsa%u(i))
          
       end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
              ! .ge. hsa%pmin) .and. ((hsa%tail(i) .eq. 'MANL')
              ! .or. (hsa%tail(i) .eq. 'SIGL')))

    end do ! do i = 1, hsa%nz

    ! Define local variables

    where(hsa%u .eq. spval) hsa%u = -99.0
    where(hsa%v .le. -90.0) hsa%v = spval
    interp_p%nz  = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge.           &
         & hsa%pmin) .and. (hsa%v .ne. spval) .and. ((hsa%tail .eq.        &
         & 'MANL') .or. (hsa%tail .eq. 'SIGL')))
    
    ! Loop through local variable

    do while((interp_p%nz .lt. nz) .and. (interp_p%nz .gt. 1))
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa%nz

          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)       &
               & .and. (hsa%v(i) .ne. spval) .and. ((hsa%tail(i) .eq.      &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa%p(i)
             interp_p%var(j) = hsa%v(i)

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%v(i) .ne. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Loop through local variables

       do i = 1, hsa%nz
 
          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)       &
               & .and. (hsa%v(i) .eq. spval) .and. ((hsa%tail(i) .eq.      &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then         

             ! Define local variables

             dstp = hsa%p(i)

             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa%v(i) = dstvar
             goto 1002

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%v(i) .eq. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Define local variables

1002   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge.         &
            & hsa%pmin) .and. (hsa%v .ne. spval) .and. ((hsa%tail .eq.     &
            & 'MANL' .or. hsa%tail .eq. 'SIGL')))

    end do ! do while((interp_p%nz .lt. nz) .and. (interp_p%nz
           ! .gt. 1))

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa%nz

       ! Check local variable and proceed accordingly

       if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin) .and.    &
            & ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i) .eq. 'SIGL')))  &
            & then

          ! Define local variables

          j          = j + 1
          meteo%v(j) = dble(hsa%v(i))
          
       end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
              ! .ge. hsa%pmin) .and. ((hsa%tail(i) .eq. 'MANL')
              ! .or. (hsa%tail(i) .eq. 'SIGL')))

    end do ! do i = 1, hsa%nz

    ! Define local variables

    where(hsa%v .eq. spval) hsa%v = -99.0
    where(hsa%z .le. -90.0) hsa%z = spval
    interp_p%nz  = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge. hsa%pmin) &
         & .and. (hsa%z .ne. spval) .and. ((hsa%tail .eq. 'MANL') .or.     &
         & (hsa%tail .eq. 'SIGL')))
    
    ! Loop through local variable

    do while((interp_p%nz .lt. nz) .and. (interp_p%nz .gt. 1))
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa%nz

          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)       &
               & .and. (hsa%z(i) .ne. spval) .and. ((hsa%tail(i) .eq.      &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa%p(i)
             interp_p%var(j) = hsa%z(i)

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%z(i) .ne. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Define local variables

       j = 0

       ! Loop through local variables

       do i = 1, hsa%nz
 
          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)      &
               & .and. (hsa%z(i) .eq. spval) .and. ((hsa%tail(i) .eq.     &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then         

             ! Define local variables

             dstp = hsa%p(i)

             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa%z(i) = dstvar
             goto 1003

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%z(i) .eq. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Define local variables

1003   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge.        &
            & hsa%pmin) .and. (hsa%z .ne. spval) .and. ((hsa%tail .eq.    &
            & 'MANL') .or. (hsa%tail .eq. 'SIGL')))

    end do ! do while((interp_p%nz .lt. nz) .and. (interp_p%nz
           ! .gt. 1))

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa%nz

       ! Check local variable and proceed accordingly

       if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin) .and.   &
            & ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) &
            & then

          ! Define local variables

          j          = j + 1
          meteo%z(j) = dble(hsa%z(i))
          
       end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
              ! .ge. hsa%pmin) .and. ((hsa%tail(i) .eq. 'MANL')
              ! .or. (hsa%tail(i) .eq. 'SIGL')))

    end do ! do i = 1, hsa%nz

    ! Define local variables

    where(hsa%z .eq. spval) hsa%z = -99.0
    where(hsa%rh .le. -90.0) hsa%rh = spval

    ! Define local variables

    interp_p%nz  = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge.          &
         & hsa%pmin) .and. (hsa%rh .ne. spval) .and. ((hsa%tail .eq.      &
         & 'MANL') .or. (hsa%tail .eq. 'SIGL')))
    
    ! Loop through local variable

    do while((interp_p%nz .lt. nz) .and. (interp_p%nz .gt. 1))
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa%nz

          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)      &
               & .and. (hsa%rh(i) .ne. spval) .and. ((hsa%tail(i) .eq.    &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa%p(i)
             interp_p%var(j) = hsa%rh(i)

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%rh(i) .ne. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Define local variables

       j = 0

       ! Loop through local variables

       do i = 1, hsa%nz
 
          ! Check local variable and proceed accordingly

          if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin)      &
               & .and. (hsa%rh(i) .eq. spval) .and. ((hsa%tail(i) .eq.    &
               & 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) then         

             ! Define local variables

             dstp = hsa%p(i)

             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa%rh(i) = dstvar
             goto 1004

          end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
                 ! .ge. hsa%pmin) .and. (hsa%rh(i) .eq. spval)
                 ! .and. ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i)
                 ! .eq. 'SIGL')))

       end do ! do i = 1, hsa%nz

       ! Define local variables

1004   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count((hsa%p .le. hsa%pmax) .and. (hsa%p .ge.        &
            & hsa%pmin) .and. (hsa%rh .ne. spval) .and. ((hsa%tail .eq.   &
            & 'MANL') .or. (hsa%tail .eq. 'SIGL')))

    end do ! do while((interp_p%nz .lt. nz) .and. (interp_p%nz
           ! .gt. 1))

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa%nz

       ! Check local variable and proceed accordingly

       if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i) .ge. hsa%pmin) .and.   &
            & ((hsa%tail(i) .eq. 'MANL') .or. (hsa%tail(i) .eq. 'SIGL'))) &
            & then

          ! Define local variables

          j           = j + 1
          meteo%rh(j) = dble(hsa%rh(i))
          
       end if ! if((hsa%p(i) .le. hsa%pmax) .and. (hsa%p(i)
              ! .ge. hsa%pmin) .and. ((hsa%tail(i) .eq. 'MANL')
              ! .or. (hsa%tail(i) .eq. 'SIGL')))

    end do ! do i = 1, hsa%nz

    ! Define local variables

    where(hsa%rh .eq. spval) hsa%rh = -99.0
    
    !=====================================================================
    
  end subroutine interp_sonde

  !=======================================================================

  ! SUBROUTINE:

  ! plevs_sonde.f90

  ! DESCRIPTION:

  ! This subroutine defines the pressure levels bounded by the sonde
  ! observations.

  ! INPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing (at minimum) the
  !   level attribute ('tail') and the pressure levels ('p'
  !   attribute).

  ! OUTPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing the lowest
  !   atmospheric pressure level for the sonde observation (pmax), the
  !   highest atmospheric pressure level for the sonde observation
  !   (pmin), and the number of valid levels (nmnlevs).
  
  !-----------------------------------------------------------------------

  subroutine plevs_sonde(hsa)
  
    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================
    
    ! Define local variables

    hsa%nmnlevs = 0
    hsa%pmax    = -spval
    hsa%pmin    = spval

    ! Loop through local variable

    do i = 1, hsa%nz

       ! Check local variable and proceed accordingly

       if((hsa%tail(i) .eq. 'MANL' .or. hsa%tail(i) .eq. 'SIGL') .and.     &
            & hsa%p(i) .lt. 1070.0) then

          ! Define local variables
         
          hsa%nmnlevs = hsa%nmnlevs + 1 
          hsa%pmax    = max(hsa%p(i),hsa%pmax)
          hsa%pmin    = min(hsa%p(i),hsa%pmin)

       end if ! if(hsa%tail(i) .eq. 'MANL' .or. hsa%tail(i)
              ! .eq. 'SIGL')

    end do ! do i = 1, hsa%nz
       
    !=====================================================================
    
  end subroutine plevs_sonde

  !=======================================================================

  ! SUBROUTINE:

  ! sonde_bufr.f90

  ! DESCRIPTION:
  
  ! This subroutine constucts an external Binary Universal Formatted
  ! (BUFR) file containing each available observations within the
  ! FORTRAN hsa_struct variable array.

  ! INPUT VARIABLES:

  ! * hsa; an array of FORTRAN hsa_struct variables.

  !-----------------------------------------------------------------------

  subroutine sonde_bufr(hsa)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa(:)

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info
    type(meteo_struct)                                                  :: meteo
    character(len=500)                                                  :: filename
    character(len=500)                                                  :: lbufr_filepath
    character(len=8)                                                    :: cacobid
    character(len=6)                                                    :: acid
    character(len=8)                                                    :: cdate
    character(len=6)                                                    :: ctime
    character(len=6)                                                    :: obsn
    character(len=2)                                                    :: msnid
    real(r_double)                                                      :: anljday
    real(r_double)                                                      :: obsjday
    real(r_double)                                                      :: racobid
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: mm
    integer                                                             :: nn
    integer                                                             :: ss
    integer                                                             :: strstrt
    integer                                                             :: strstop
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
    bufr%obstr         = 'POB QOB TOB UOB VOB ZOB'
    bufr%qcstr         = 'PQM QQM TQM WQM ZQM'
    bufr%oestr         = 'POE QOE TOE WOE ZOE'
    bufr%subset        = trim(adjustl(bufr_info%subset))
    bufr%mxmn          = 6
    bufr%mxlv          = 1
    lbufr_filepath     = trim(adjustl(bufr_filepath))
    call bufrio_interface_idate(analdate,bufr)
    call bufrio_interface_open(lbufr_filepath,bufr_tblpath,bufr,.false.,   &
         & .false.,.true.)

    ! Loop through local variable

    do i = 1, size(hsa)

       ! Define local variables
       
       strstrt  = index(trim(adjustl(hsa(i)%filename)),'/',back=.true.)    &
            & + 1
       strstop  = len(trim(adjustl(hsa(i)%filename)))
       filename = trim(adjustl(hsa(i)%filename(strstrt:strstop)))
       msnid    = filename(1:2)
       acid     = filename(1:6)
       obsn     = filename(7:8)

       ! Check local variable and proceed accordingly

       if(trim(adjustl(msnid)) .eq. 'AF') then

          ! Define local variables

          write(cacobid,500) trim(adjustl(acid(3:5))), trim(adjustl(obsn))

       end if ! if(trim(adjustl(msnid)) .eq. 'AF')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(msnid)) .eq. 'NA') then

          ! Define local variables

          write(cacobid,501) trim(adjustl(obsn))

       end if ! if(trim(adjustl(msnid)) .eq. 'NA')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(msnid)) .eq. 'NO') then

          ! Define local variables

          write(cacobid,502) trim(adjustl(acid(5:5))), trim(adjustl(obsn))

       end if ! if(trim(adjustl(msnid)) .eq. 'NO')

       ! Loop through local variable

       do j = 1, hsa(i)%nz

          ! Check local variable and proceed accordingly

          if((hsa(i)%yymmdd(j) .ne. hsa_spval) .and. ((hsa(i)%tail(j) .eq. &
               & 'SIGL') .or. (hsa(i)%tail(j) .eq. 'MANL'))) then

             ! Define local variables

             call variable_interface_setup_struct(bufr)
             write(cdate,'(i8)')        (20000000 +                        &
                  & (int(hsa(i)%yymmdd(j))))
             write(ctime,'(i4.4,i2.2)') hsa(i)%gmt(j), 0
             write(bufr%cdate,503) cdate(1:4), cdate(5:6), cdate(7:8),     &
                  & ctime(1:2), ctime(3:4), ctime(5:6)
             call time_methods_date_attributes(bufr%cdate,yyyy,mm,dd,hh,   &
                  & nn,ss)

             ! Compute local variables

             call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,obsjday)

             ! Define local variables

             bufr%hdr(1) = dble(racobid)
             bufr%hdr(2) = dble(-1.0*hsa(i)%lon(j) + 360.0)
             bufr%hdr(3) = dble(hsa(i)%lat(j))
             bufr%hdr(4) = (obsjday - anljday)*dble(24.0)
             bufr%hdr(5) = bufr_info%obs_type_mass
             meteo%nz    = 1
             call variable_interface_setup_struct(meteo)

             ! Check local variable and proceed accordingly

             if(hsa(i)%t(j) .ne. hsa_spval) then

                ! Define local variables

                bufr%obs(1,1) = hsa(i)%p(j)
                bufr%obs(3,1) = hsa(i)%t(j)
                bufr%obs(6,1) = hsa(i)%z(j)
                bufr%qcf(1,1) = 2.0
                bufr%qcf(3,1) = 2.0
                bufr%qcf(6,1) = 2.0
                
                ! Check local variable and proceed accordingly

                if(hsa(i)%rh(j) .ne. hsa_spval) then

                   ! Define local variables

                   meteo%p(1)  = dble(hsa(i)%p(j)*100.0)
                   meteo%rh(1) = dble(hsa(i)%rh(j))
                   meteo%t(1)  = dble(hsa(i)%t(j) + 273.15)

                   ! Compute local variables

                   call meteo_methods_spechumd(meteo)

                   ! Define local variables
                   
                   bufr%obs(2,1) = real(meteo%q(1))*1000.0*1000.0
                   bufr%qcf(2,1) = 2.0 

                end if ! if(hsa(i)%rh(j) .ne. hsa_spval)

                ! Define local variables

                call bufrio_interface_write(bufr)

             end if ! if(hsa(i)%t(j) .ne. hsa_spval)

             ! Deallocate memory for local variables

             call variable_interface_cleanup_struct(bufr)
             call variable_interface_cleanup_struct(meteo)

             ! Define local variables

	     call variable_interface_setup_struct(bufr)
             bufr%hdr(1) = dble(racobid)
             bufr%hdr(2) = dble(-1.0*hsa(i)%lon(j) + 360.0)
             bufr%hdr(3) = dble(hsa(i)%lat(j))
             bufr%hdr(4) = (obsjday - anljday)*dble(24.0)
             bufr%hdr(5) = bufr_info%obs_type_wind

	     ! Check local variable and proceed accordingly

             if(hsa(i)%u(j) .ne. hsa_spval .and. hsa(i)%v(j) .ne.          &
                  & hsa_spval) then

                ! Define local variables

                bufr%obs(1,1) = hsa(i)%p(j)
                bufr%obs(4,1) = hsa(i)%u(j)
                bufr%obs(5,1) = hsa(i)%v(j)
                bufr%obs(6,1) = hsa(i)%z(j)
                bufr%qcf(1,1) = 2.0
                bufr%qcf(4,1) = 2.0
                bufr%qcf(6,1) = 2.0
                call bufrio_interface_write(bufr)

             end if ! if(hsa(i)%u(j) .ne. hsa_spval .and. hsa(i)%v(j)
                    ! .ne.  hsa_spval)

             ! Deallocate memory for local variables
          
	     call variable_interface_cleanup_struct(bufr)           
             
          end if ! if((hsa(i)%yymmdd(j) .ne. hsa_spval)
                 ! .and. ((hsa(i)%tail(j) .eq. 'SIGL')
                 ! .or. (hsa(i)%tail(j) .eq. 'MANL')))
             
       end do ! do j = 1, hsa(i)%nz

    end do ! do i = 1, size(hsa)

    ! Define local variables

    call bufrio_interface_close(.false.,.true.)
500 format(a3,a2,'A')
501 format('872',a2,'A')
502 format('AA',a1,a2,'A')
503 format(a4,'-',a2,'-',a2,'_',a2,':',a2,':',a2)

    !=====================================================================

  end subroutine sonde_bufr
  
  !=======================================================================

  ! SUBROUTINE:

  ! sonde_drift.f90

  ! DESCRIPTION:

  ! This subroutine estimates the horizontal displacement of the sonde
  ! as a function of the theoretical fall rate and the wind profile;
  ! vertical interpolation along a linear-log pressure coordinate is
  ! performed to estimate the theoretical fall rate and the zonal- and
  ! meridional-displacement via the zonal- and meridional-wind vector
  ! components, respectively; an external file containing the HSA
  ! formatted data record with the new timestamps and geographical
  ! coordinates is written to:

  ! <datapath>/<aircraft ID>_<observation number>_<timestamp>.hsa.drft

  ! REFERENCES:

  ! Hock, T. F., and J. L. Franklin, 1999: The NCAR GPS
  ! Dropwindsonde. Bull. Amer. Meteor. Soc., 80, 407–420.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   external file containing the HSA formatted decoded TEMPDROP
  !   message; the output file generated by this routine appends the
  !   string '.drft' to this filename.

  ! * meteo; a FORTRAN meteo_struct variable containing the aircraft
  !   identifier (acid), the observation number (obnum), and the
  !   full-path to the TEMPDROP file (tempdrop_name); the arrays
  !   within this struct are populated within this routine.

  !-----------------------------------------------------------------------

  subroutine sonde_drift(sonde,hsa,meteo)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa
    type(meteo_struct)                                                  :: meteo
    type(sonde_struct)                                                  :: sonde

    ! Define variables computed within routine

    type(hsa_struct)                                                    :: hsa_interp
    character(len=500)                                                  :: error_filename
    character(len=500)                                                  :: skewt_filename
    
    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================
    
    ! Check local variable and proceed accordingly

    if(hsa%nz .le. 0) goto 1000

    ! Define local variables

    call plevs_sonde(hsa)

    ! Check local variable and proceed accordingly

    if(hsa%nmnlevs .le. 1) goto 1000

    ! Compute local variables

    call timeinfo_sonde(hsa)

    ! Define local variables

    hsa_interp%nz = hsa%nz
    call variable_interface_setup_struct(hsa_interp)
    hsa_interp    = hsa

    ! Compute local variables

    call interp_sonde(hsa_interp,meteo)
    call compute_meteo(meteo)
    call compute_drift(hsa,hsa_interp,meteo)

    ! Define local variables

    error_filename = trim(adjustl(hsa%filename))//'.error'
    skewt_filename = trim(adjustl(hsa%filename))//'.nc'
    call fileio_interface_write(error_filename,hsa,meteo)
    hsa%filename   = trim(adjustl(hsa%filename))//'.drft'
    call fileio_interface_write(hsa)

    ! Check local variable and proceed accordingly

    if(tempdrop_write_nc_skewt) then

       ! Define local variables

       call fileio_interface_write(sonde,meteo,skewt_filename)

    end if ! if(tempdrop_write_nc_skewt)

    ! Define local variables
    
1000 continue

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(hsa_interp)
    
    !=====================================================================

  end subroutine sonde_drift
  
  !=======================================================================

  ! SUBROUTINE:

  ! tempdrop_to_hsa.f90

  ! DESCRIPTION:

  ! This subroutine decodes the TEMP-DROP formatted message to the
  ! AOML HRD HSA format and writes the results to an external
  ! formatted file.

  ! INPUT VARIABLES:

  ! * infile; a FORTRAN character string specifying the path to the
  !   external file containing the TEMPDROP messages.

  ! * hsa; a FORTRAN hsa_struct variable.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! OUTPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing the decoded
  !   TEMP-DROP attributes for the release and splash times, the
  !   timestamp attributes, and the filename to which the decoded
  !   TEMP-DROP message is written; the output filename is as follows:

  ! <datapath>/<aircraft ID>_<observation number>_<timestamp>.hsa

  ! * meteo; a FORTRAN meteo_struct variable containing the aircraft
  !   identifier (acid), the observation number (obnum), and the
  !   full-path to the TEMPDROP file (tempdrop_name).

  !-----------------------------------------------------------------------

  subroutine tempdrop_to_hsa(infile,hsa,meteo)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa
    type(meteo_struct)                                                  :: meteo
    character(len=500)                                                  :: infile

    ! Define variables computed within routine

    character(len=3),           dimension(12)                           :: month_data
    character(len=70)                                                   :: line
    character(len=5)                                                    :: acid
    character(len=3)                                                    :: month
    character(len=2)                                                    :: obnum
    logical                                                             :: exist
    logical                                                             :: exist_chk
    integer                                                             :: lupa
    integer                                                             :: iac
    integer                                                             :: iwx
    integer                                                             :: iflag
    integer                                                             :: iyrs
    integer                                                             :: imns
    integer                                                             :: idys
    integer                                                             :: ihrs
    integer                                                             :: inns
    integer                                                             :: strstrt
    integer                                                             :: strstop

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================
    
    ! Define local variables

    hsa%spgtime = -999
    data month_data/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',       &
         & 'Sep','Oct','Nov','Dec'/
    iyrs        = 0
    imns        = 0
    idys        = 0
    iwx         = 1
    iflag       = 1
    lupa        = 150
    open(12,file=trim(adjustl(infile)),status='old')
20  read(12,'(a)',end=1000) line

    ! Check local variable and proceed accordingly

    if(line(1:5) .eq. 'Sonde') then

       ! Define local variables

       read(line(30:31),'(i2)') idys
       read(line(33:35),'(a3)') month
       read(line(37:38),'(i2)') iyrs
       hsa%yyyy = 1900 + iyrs

       ! Loop through local variable

       do i = 1, size(month_data)

          ! Check local variable and proceed accordingly

          if(month_data(i) .eq. month) then

             ! Define local variables

             imns = i

          end if ! if(month_data(i) .eq. month)

       end do ! do i = 1, size(month_data)

    end if ! if(line(1:5) .eq. 'Sonde')

    ! Check local variable and proceed accordingly

    if(line(1:5) .eq. '61616') then

       ! Define local variables

       read(line(7:11),'(a)') acid
       strstrt = index(line,'OB')
       strstop = strstrt + 5
       read(line(strstop-2:strstop),'(a2)') obnum

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obnum)) .eq. '') obnum = '00'
       
       ! Check local variable and proceed accordingly

       if(acid(1:1) .eq. 'A') then

          ! Define local variables

          iac = 30

       else if(acid(5:5) .eq. 'F') then

          ! Define local variables

          iac = 8

       else   ! if(acid(1:1) .eq. 'A')

          ! Define local variables

          read(line(11:11),'(i1)') iac
          iac = iac + 40

       end if ! if(acid(1:1) .eq. 'A')

    end if ! if(line(1:5) .eq. '61616')

    ! Check local variable and proceed accordingly

    if(idys .eq. 0 .or. imns .eq. 0 .or. iyrs .eq. 0) then
       
       ! Define local variables
       
       strstrt = index(infile,'/',back=.true.) + 1
       strstop = strstrt + 3
       read(infile(strstrt:strstop),'(i4)') hsa%yyyy
       strstrt = index(infile,'/',back=.true.) + 3
       strstop = strstrt + 1
       read(infile(strstrt:strstop),'(i2)') iyrs
       strstrt = strstop + 1
       strstop = strstrt + 1
       read(infile(strstrt:strstop),'(i2)') imns
       strstrt = strstop + 1
       strstop = strstrt + 1
       read(infile(strstrt:strstop),'(i2)') idys
       strstrt = strstop + 1
       strstop = strstrt + 1
       read(infile(strstrt:strstop),'(i2)') ihrs
       strstrt = strstop + 1
       strstop = strstrt + 1
       read(infile(strstrt:strstop),'(i2)') inns

    end if ! if(idys .eq. 0 .or. imns .eq. 0 .or. iyrs .eq. 0)

    ! Define local variables

    hsa%mm              = imns
    hsa%dd              = idys
    hsa%hh              = ihrs
    hsa%nn              = inns
    hsa%ss              = 0
    goto 20
1000 continue
    close(12)
    meteo%acid          = acid
    meteo%obnum         = obnum
    meteo%tempdrop_name = infile
    write(hsa%filename,500) trim(adjustl(datapath)), acid, obnum, iyrs,    &
         & imns,idys
    inquire(file=trim(adjustl(hsa%filename)),exist=exist)
    
    ! Check local variable and proceed accordingly
    
    if(exist) then

       ! Loop through local variable

       do i = 1, 1000

       	  ! Define local variables
    
	  write(hsa%filename,501) trim(adjustl(datapath)), acid, obnum,    &
             & iyrs, imns, idys, i
	  inquire(file=trim(adjustl(hsa%filename)),exist=exist_chk)
          if(.not. exist_chk) goto 1001

       end do ! do i = 1, 1000

    end	if ! if(exist)

    ! Define local variables
    
1001 continue
    open(99,file=trim(adjustl(hsa%filename)),form='formatted')
    open(12,file=trim(adjustl(infile)),status='old')
21  read(12,'(a)',end=1002) line
       
    ! Check local variable and proceed accordingly

    if(index(line,'REL') .ne. 0) then

       ! Check local variable and proceed accordingly

       if((len(trim(adjustl(line))) - index(line,'REL')) .lt. 21)         &
            & return

       ! Define local variables

       call spginfo(line,hsa%reltime,hsa%rellat,hsa%rellon,'REL')

    endif ! if(index(line,'REL') .ne. 0)

    ! Check local variable and proceed accordingly

    if(index(line,'SPG') .ne. 0) then

       ! Check local variable and proceed accordingly

       if((len(trim(adjustl(line))) - index(line,'SPG')) .lt. 21)         &
            & return

       ! Define local variables

       call spginfo(line,hsa%spgtime,hsa%spglat,hsa%spglon,'SPG')

    endif ! if(index(line,'SPG') .ne. 0)

    ! Check local variable and proceed accordingly

    if(index(line,'SPL') .ne. 0) then

       ! Check local variable and proceed accordingly

       if((len(trim(adjustl(line))) - index(line,'SPL')) .lt. 21)         &
            & return

       ! Define local variables

       call spginfo(line,hsa%spgtime,hsa%spglat,hsa%spglon,'SPL')

    end if ! if(index(line,'SPL') .ne. 0)

    ! Check local variable and proceed accordingly

    if((index(line,'XX') .ne. 0)) then

       ! Compute local variables

       call drop(99,iwx,iflag,iyrs,imns,idys,line,hsa%psfc,meteo%acid,    &
            & meteo%obnum)

    end if ! if(index(line,'XX') .ne. 0)

    ! Define local variables
    
    goto 21
1002 continue
    close(12)
    close(99)
    call fileio_interface_read(hsa)
500 format(a,a5,'_'a2,'_',3(i2.2),'.hsa')
501 format(a,a5,'_'a2,'_',3(i2.2),'_'(i2.2),'.hsa')

    !=====================================================================

  end subroutine tempdrop_to_hsa

  !=======================================================================

  ! SUBROUTINE:

  ! timeinfo_sonde.f90

  ! DESCRIPTION:

  ! This subroutine computes the Julian date for the sonde release and
  ! returns the FORTRAN hsa_struct variable time attributes.

  ! INPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing (at minimum) the
  !   sonde release time (specified in the 'reltime' attribute) and
  !   the year, month, and day of month timestamp (specified in the
  !   'yymmdd' attribute).

  ! OUTPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable where the timestamp
  !   attributes for year, month, and day of month ('yyyy', 'mm', and
  !   'dd' attributes, respectively) have been defined and the Julian
  !   date timestamp for the sonde release (rel_julian) has been
  !   computed.

  !-----------------------------------------------------------------------

  subroutine timeinfo_sonde(hsa)  

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa

    ! Define variables computed within routine

    character(len=8)                                                    :: yymmdd
    real(r_double)                                                      :: hsa_julian
    real(r_double)                                                      :: julian_day
    integer                                                             :: dd
    integer                                                             :: dd_hsa
    integer                                                             :: dd_rel
    integer                                                             :: hh_hsa
    integer                                                             :: hh_rel
    integer                                                             :: mm
    integer                                                             :: mm_hsa
    integer                                                             :: mm_rel
    integer                                                             :: nn_hsa
    integer                                                             :: nn_rel
    integer                                                             :: seconds
    integer                                                             :: ss_hsa
    integer                                                             :: ss_rel
    integer                                                             :: yyyy
    integer                                                             :: yyyy_hsa
    integer                                                             :: yyyy_rel

    !=====================================================================    

    ! Define local variables

    seconds = int((hsa%hh*3600.0) + (hsa%nn*60.0) + hsa%ss)
    
    ! Compute local variables

    call time_methods_julian_day(hsa%yyyy,hsa%mm,hsa%dd,0,0,0,hsa_julian)
    
    ! Define local variables

    hsa_julian  = hsa_julian + real(seconds)/86400.0
    write(yymmdd,'(i6)') int(hsa%yymmdd(hsa%nz))
    read(yymmdd(1:2),'(i2)') yyyy
    read(yymmdd(3:4),'(i2)') mm
    read(yymmdd(5:6),'(i2)') dd
    hsa%yyyy    = 2000 + yyyy
    hsa%mm      = mm
    hsa%dd      = dd
    hsa%logtime = hsa%gmt(1)*100

    ! Compute local variables
    
    call time_methods_julian_day(hsa%yyyy,hsa%mm,hsa%dd,0,0,0,julian_day)
    call time_methods_hmsts(hsa%reltime,hsa%rel_hmsts)

    ! Define local variables
    
    hsa%rel_julian = julian_day + real(hsa%rel_hmsts)/86400.0

    ! Compute local variables

    call time_methods_gregorian_date(hsa_julian,yyyy_hsa,mm_hsa,dd_hsa,   &
         & hh_hsa,nn_hsa,ss_hsa)
    call time_methods_gregorian_date(hsa%rel_julian,yyyy_rel,mm_rel,      &
         & dd_rel,hh_rel,nn_rel,ss_rel)

    ! Check local variable and proceed accordingly

    if((dd_rel .eq. dd_hsa) .and. (hh_rel .gt. hh_hsa)) then

       ! Define local variables

       hsa%rel_julian = hsa%rel_julian - 1.0

    end if ! if((dd_rel .eq. dd_hsa) .and. (hh_rel .gt. hh_hsa))

    !=====================================================================

  end subroutine timeinfo_sonde
  
  !=======================================================================

end module sonde_tempdrop_interface
