module tempdrop_sonde_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! tempdrop-sonde :: tempdrop_sonde_interface
  ! Copyright (C) 2017 Henry R. Winterbottom

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

  use constants_interface
  use diagnostics_interface
  use fileio_interface
  use grid_methods_interface
  use kinds_interface
  use math_methods_interface
  use meteo_methods_interface
  use namelist_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: tempdrop_sonde

  ! Define local variables

  type(hsa_struct)                                                      :: hsa_grid

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! tempdrop_sonde.f90

  ! DESCRIPTION:

  ! This is the driver routine for the decoding and formatting of the
  ! National Oceanic and Atmospheric Administration (NOAA) Atlantic
  ! Oceanographic and Meteorological Laboratory (AOML) Hurricane
  ! Research Division (HRD) TEMPDROP sondes.

  !-----------------------------------------------------------------------

  subroutine tempdrop_sonde()

    ! Define variables computed within routine

    type(meteo_struct)                                                  :: meteo_grid
    character(len=500)                                                  :: out_file_hsa
    real(r_kind)                                                        :: time_start

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call diagnostics_time_start(time_start)
    call namelist()
    call fileio_interface_read(sonde_filelist)
    open(999,file=trim(adjustl(datapath))//'hsa-map.table',form=           &
         & 'formatted')

    ! Loop through local variable

    do i = 1, nfiles

       ! Check local variable and proceed accordingly
    
       if(write_hsa) then
       
          ! Compute local variables
      
          call decode_tempdrop_hsa(sonde_filename(i),out_file_hsa,         &
               & meteo_grid)

          ! Define local variables

          write(999,500) trim(adjustl(sonde_filename(i))),                 &
               & trim(adjustl(out_file_hsa)) 

          ! Check local variable and proceed accordingly
       
          if(write_hsa_drift) then 
          
             ! Compute local variables
          
             call tempdrop_drift_hsa(out_file_hsa,meteo_grid) 

          end if ! if(write_hsa_drift)
          
          ! Deallocate memory for local variables
       
          call variable_interface_cleanup_struct(hsa_grid)

       end if ! if(write_hsa)

    end do ! do i = 1, nfiles

    ! Define local variables

    close(999)

    ! Deallocate memory for local variables

    if(allocated(sonde_filename)) deallocate(sonde_filename)

    ! Define local variables

    call diagnostics_time_stop(time_start)
500 format(a,1x,a)

    !=====================================================================

  end subroutine tempdrop_sonde

  !=======================================================================

  ! SUBROUTINE:

  ! decode_tempdrop_hsa.f90

  ! DESCRIPTION:

  ! This subroutine decodes the TEMPDROP message to the HRD HSA format
  ! and writes the results to an external formatted file and returns
  ! the filename; the output filename is as follows:

  ! <datapath>/<aircraft ID>>_<observation number>_<timestamp>.hsa

  ! INPUT VARIABLES:

  ! * infile; a FORTRAN character string specifying the path to the
  !   external file containing the TEMPDROP messages.

  ! * outfile; a FORTRAN character string specifying the path to the
  !   external file to contain the HSA formatted decoded TEMPDROP
  !   message.

  ! * meteo_grid; a FORTRAN meteo_struct variable.

  ! OUTPUT VARIABLES:

  ! * outfile; a FORTRAN character string specifying the path to the
  !   external file to contain the HSA formatted decoded TEMPDROP
  !   message.

  ! * meteo_grid; a FORTRAN meteo_struct variable containing the
  !   aircraft identifier (acid), the observation number (obnum), and
  !   the full-path to the TEMPDROP file (tempdrop_name).

  !-----------------------------------------------------------------------

  subroutine decode_tempdrop_hsa(infile,outfile,meteo_grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo_grid
    character(len=500)                                                  :: infile
    character(len=500)                                                  :: outfile

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
    integer                                                             :: strstrt
    integer                                                             :: strstop

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================
    
    ! Define local variables

    hsa_grid%spgtime = -999
    data month_data/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',       &
         & 'Sep','Oct','Nov','Dec'/
    iyrs             = 0
    imns             = 0
    idys             = 0
    iwx              = 1
    iflag            = 1
    lupa             = 150
    open(12,file=trim(adjustl(infile)),status='old')
20  read(12,'(a)',end=1000) line

    ! Check local variable and proceed accordingly

    if(line(1:5) .eq. 'Sonde') then

       ! Define local variables

       read(line(30:31),'(i2)') idys
       read(line(33:35),'(a3)') month
       read(line(37:38),'(i2)') iyrs
       hsa_grid%yyyy = 1900 + iyrs

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

       ! Check local variable and proceed accordingly

       if(line(34:34) .eq. ' ') then

          ! Define local variables

          read(line(35:36),'(a)') obnum

       else   ! if(line(34:34) .eq. ' ')

          ! Define local variables

          read(line(36:37),'(a)') obnum

       end if ! if(line(34:34) .eq. ' ')

       ! Check local variable and proceed accordingly

       if(obnum(1:1) .eq. ' ') then

          ! Define local variables

          obnum = '00'

       else if(obnum(1:2) .eq. '  ') then

          ! Define local variables

          obnum = '00'

       else if(obnum(2:2) .eq. ' ') then

          ! Define local variables

          obnum = '00'

       end if ! if(obnum(1:2) .eq. '  ')
       
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
       read(infile(strstrt:strstop),'(i4)') hsa_grid%yyyy
       strstrt = index(infile,'/',back=.true.) + 3
       strstop = strstrt + 1
       read(infile(strstrt:strstop),'(i2)') iyrs
       strstrt = strstop + 1
       strstop = strstrt + 1
       read(infile(strstrt:strstop),'(i2)') imns
       strstrt = strstop + 1
       strstop = strstrt + 1
       read(infile(strstrt:strstop),'(i2)') idys

    end if ! if(idys .eq. 0 .or. imns .eq. 0 .or. iyrs .eq. 0)

    ! Define local variables

    hsa_grid%mm              = imns
    hsa_grid%dd              = idys
    goto 20
1000 continue
    close(12)
    meteo_grid%acid          = acid
    meteo_grid%obnum         = obnum
    meteo_grid%tempdrop_name = infile
    write(outfile,500) trim(adjustl(datapath)), acid, obnum, iyrs, imns,   &
         & idys
    inquire(file=trim(adjustl(outfile)),exist=exist)
    
    ! Check local variable and proceed accordingly
    
    if(exist) then

       ! Loop through local variable

       do i = 1, 1000

       	  ! Define local variables
    
	  write(outfile,501) trim(adjustl(datapath)), acid, obnum, iyrs,   &
	     imns, idys, i
	  inquire(file=trim(adjustl(outfile)),exist=exist_chk)
          if(.not. exist_chk) goto 1001

       end do ! do i = 1, 1000

    end	if ! if(exist)

1001 continue
    open(99,file=trim(adjustl(outfile)),form='formatted')
    open(12,file=trim(adjustl(infile)),status='old')
21  read(12,'(a)',end=1002) line

    ! Check local variable and proceed accordingly

    if(index(line,'REL') .ne. 0) then

       ! Check local variable and proceed accordingly

       if((len(trim(adjustl(line))) - index(line,'REL')) .lt. 21)         &
            & return

       ! Define local variables

       call spginfo(line,hsa_grid%reltime,hsa_grid%rellat,                &
            & hsa_grid%rellon,'REL')

    endif ! if(index(line,'REL') .ne. 0)

    ! Check local variable and proceed accordingly

    if(index(line,'SPG') .ne. 0) then

       ! Check local variable and proceed accordingly

       if((len(trim(adjustl(line))) - index(line,'SPG')) .lt. 21)         &
            & return

       ! Define local variables

       call spginfo(line,hsa_grid%spgtime,hsa_grid%spglat,                &
            & hsa_grid%spglon,'SPG')

    endif ! if(index(line,'SPG') .ne. 0)

    ! Check local variable and proceed accordingly

    if(index(line,'SPL') .ne. 0) then

       ! Check local variable and proceed accordingly

       if((len(trim(adjustl(line))) - index(line,'SPL')) .lt. 21)         &
            & return

       ! Define local variables

       call spginfo(line,hsa_grid%spgtime,hsa_grid%spglat,                &
            & hsa_grid%spglon,'SPL')

    end if ! if(index(line,'SPL') .ne. 0)

    ! Check local variable and proceed accordingly

    if((index(line,'XX') .ne. 0)) then 

       ! Compute local variables

       call drop(99,iwx,iflag,iyrs,imns,idys,line,hsa_grid%psfc,          &
            & meteo_grid%acid,meteo_grid%obnum)

    end if ! if(index(line,'XX') .ne. 0)

    ! Define local variables
    
    goto 21
1002 continue
    close(12)
    close(99)
500 format(a,a5,'_'a2,'_',3(i2.2),'.hsa')
501 format(a,a5,'_'a2,'_',3(i2.2),'_'(i2.2),'.hsa')

    !=====================================================================

  end subroutine decode_tempdrop_hsa

  !=======================================================================

  ! SUBROUTINE:

  ! tempdrop_drift_hsa.f90

  ! DESCRIPTION:

  ! This subroutine estimates the horizontal displacement of the sonde
  ! as a function of the theoretical fall rate and the wind profile;
  ! vertical interpolation along a linear-log pressure coordinate is
  ! performed to estimate the theoretical fall rate and the zonal- and
  ! meridional-displacement via the zonal- and meridional-wind vector
  ! components, respectively; an external file containing the HSA
  ! formatted data record with the new timestamps and geographical
  ! coordinates is written to:

  ! <datapath>/<aircraft ID>>_<observation number>_<timestamp>.hsa.drft

  ! REFERENCES:

  ! Hock, T. F., and J. L. Franklin, 1999: The NCAR GPS
  ! Dropwindsonde. Bull. Amer. Meteor. Soc., 80, 407–420.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   external file containing the HSA formatted decoded TEMPDROP
  !   message; the output file generated by this routine appends the
  !   string '.drft' to this filename.

  ! * meteo_grid; a FORTRAN meteo_struct variable containing the
  !   aircraft identifier (acid), the observation number (obnum), and
  !   the full-path to the TEMPDROP file (tempdrop_name); the arrays
  !   within this struct are populated within this routine.

  !-----------------------------------------------------------------------

  subroutine tempdrop_drift_hsa(filename,meteo_grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo_grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    type(hsa_struct)                                                    :: hsa_interp_grid
    type(interp_p_struct)                                               :: interp_p
    type(statgrid_struct)                                               :: statgrid
    character(len=8)                                                    :: yymmdd
    character(len=4)                                                    :: hhnn
    real(r_double)                                                      :: julian_time
    real(r_double)                                                      :: julian_day
    real(r_double)                                                      :: rel_julian
    real(r_double)                                                      :: spg_julian
    real(r_kind)                                                        :: dstp
    real(r_kind)                                                        :: dstvar
    real(r_kind)                                                        :: dummy
    real(r_kind)                                                        :: fallrate
    real(r_kind)                                                        :: gsndfall2
    real(r_kind)                                                        :: avgp
    real(r_kind)                                                        :: avgt
    real(r_kind)                                                        :: pmax
    real(r_kind)                                                        :: pmin
    real(r_kind)                                                        :: timediff
    real(r_kind)                                                        :: spgtime
    real(r_kind)                                                        :: dx
    real(r_kind)                                                        :: dy
    real(r_kind)                                                        :: dtime
    real(r_kind)                                                        :: dlon
    real(r_kind)                                                        :: dlat
    real(r_kind)                                                        :: plon
    real(r_kind)                                                        :: plat
    real(r_kind)                                                        :: ptime
    real(r_kind)                                                        :: ptemp
    real(r_kind)                                                        :: ppres
    real(r_kind)                                                        :: norma
    real(r_kind)                                                        :: normb
    integer                                                             :: nmnlevs
    integer                                                             :: seconds
    integer                                                             :: nz
    integer                                                             :: time_hmsts
    integer                                                             :: time_sthms
    integer                                                             :: rel_hmsts
    integer                                                             :: spg_hmsts
    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: nn
    integer                                                             :: ss
    integer                                                             :: gmt

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================
    
    ! Define local variables

    hsa_grid%nz = 0
    open(99,file=trim(adjustl(filename)),form='formatted')
1000 read(99,*,end=1001,err=1007) dummy
    hsa_grid%nz = hsa_grid%nz + 1
    goto 1000
1001 continue
    close(99)

    ! Check local variable and proceed accordingly

    if(hsa_grid%nz .le. 0) goto 1007

    ! Define local variables

    hsa_interp_grid%nz = hsa_grid%nz
    call variable_interface_setup_struct(hsa_grid)
    call variable_interface_setup_struct(hsa_interp_grid)
    pmax               = -spval
    pmin               = spval
    open(99,file=trim(adjustl(filename)),form='formatted')
    nmnlevs            = 0

    ! Loop through local variable

    do i = 1, hsa_grid%nz

       ! Define local variables

       read(99,501) hsa_grid%wx(i), hsa_grid%yymmdd(i), hsa_grid%gmt(i),  &
            & hsa_grid%lat(i), hsa_grid%lon(i), hsa_grid%p(i),            &
            & hsa_grid%t(i), hsa_grid%rh(i), hsa_grid%z(i),               &
            & hsa_grid%u(i), hsa_grid%v(i), hsa_grid%tail(i)
        
       ! Check local variable and proceed accordingly

       if((hsa_grid%tail(i) .eq. 'MANL' .or. hsa_grid%tail(i) .eq.        &
            & 'SIGL') .and. hsa_grid%p(i) .lt. 1070.0) then

          ! Define local variables
         
          nmnlevs = nmnlevs + 1 
          pmax    = max(hsa_grid%p(i),pmax)
          pmin    = min(hsa_grid%p(i),pmin)

       end if ! if(hsa_grid%tail(i) .eq. 'MANL' .or. hsa_grid%tail(i)
              ! .eq. 'SIGL')

    end do ! do i = 1, hsa_grid%nz

    ! Define local variables
    
    close(99)

    ! Check local variable and proceed accordingly

    if(nmnlevs .le. 1) goto 1007

    ! Define local variables

    hsa_grid%logtime = hsa_grid%gmt(1)*100
    call time_methods_julian_day(hsa_grid%yyyy,hsa_grid%mm,hsa_grid%dd,   &
         & julian_day)
    call time_methods_hmsts(hsa_grid%reltime,time_hmsts)
    rel_hmsts        = time_hmsts
    julian_day       = julian_day + real(time_hmsts)/86400.0
    rel_julian       = julian_day
    hsa_interp_grid  = hsa_grid
    where(hsa_interp_grid%t .le. -90.0) hsa_interp_grid%t = spval
    nz               = count(hsa_interp_grid%p .le. pmax .and.            &
         & hsa_interp_grid%p .ge. pmin .and. (hsa_interp_grid%tail .eq.   &
         & 'MANL' .or. hsa_interp_grid%tail .eq. 'SIGL'))
    interp_p%nz      = count(hsa_interp_grid%p .le. pmax .and.            &
         & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%t .ne.       &
         & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.             &
         & hsa_interp_grid%tail .eq. 'SIGL'))
    meteo_grid%nz    = nz
    call variable_interface_setup_struct(meteo_grid)
    j                = 0

    ! Loop through local variables

    do i = 1, hsa_interp_grid%nz

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i) .ge.  &
            & pmin .and. (hsa_interp_grid%tail(i) .eq. 'MANL' .or.        &
            & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

          ! Define local variables

          j               = j + 1
          meteo_grid%p(j) = dble(hsa_interp_grid%p(i)*100.0)
          
       end if ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

    end do ! do i = 1, hsa_interp_grid%nz

    ! Define local variables

    interp_p%psfc   = hsa_grid%psfc
    meteo_grid%psfc = dble(hsa_grid%psfc*100.0)

    ! Loop through local variable

    do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz

          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%t(i) .ne. spval .and.    &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa_interp_grid%p(i)
             interp_p%var(j) = hsa_interp_grid%t(i)

          end if ! if(hsa_interp_grid%p(i) .le. 1070.0
                 ! .and. hsa_interp_grid%t(i) .gt. -90.0
                 ! .and. (hsa_interp_grid%tail .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz
 
          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%t(i) .eq. spval .and.    &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then         

             ! Define local variables

             dstp = hsa_interp_grid%p(i)
           
             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa_interp_grid%t(i) = dstvar
             goto 1002

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%t(i) .lt. -90.0
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Define local variables

1002   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count(hsa_interp_grid%p .le. pmax .and.              &
            & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%t .ne.    &
            & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.          &
            & hsa_interp_grid%tail .eq. 'SIGL'))

    end do ! do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa_interp_grid%nz

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i) .ge.  &
            & pmin .and. (hsa_interp_grid%tail(i) .eq. 'MANL' .or.        &
            & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

          ! Define local variables

          j               = j + 1
          meteo_grid%t(j) = dble(hsa_interp_grid%t(i) + 273.15)
          
       end if ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

    end do ! do i = 1, hsa_interp_grid%nz

    ! Check local variable and proceed accordingly

    where(hsa_interp_grid%t .eq. spval) hsa_interp_grid%t = -99.0

    ! Define local variables

    where(hsa_interp_grid%u .le. -90.0) hsa_interp_grid%u = spval
    interp_p%nz  = count(hsa_interp_grid%p .le. pmax .and.                &
         & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%u .ne.       &
         & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.             &
         & hsa_interp_grid%tail .eq. 'SIGL'))
    
    ! Loop through local variable

    do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz

          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%u(i) .ne. spval .and.    &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa_interp_grid%p(i)
             interp_p%var(j) = hsa_interp_grid%u(i)

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%u(i) .ne. spval
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz
 
          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%u(i) .eq. spval .and.    &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then         

             ! Define local variables

             dstp = hsa_interp_grid%p(i)

             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa_interp_grid%u(i) = dstvar
             goto 1003

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%u(i) .ne. spval
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Define local variables

1003   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count(hsa_interp_grid%p .le. pmax .and.              &
            & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%u .ne.    &
            & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.          &
            & hsa_interp_grid%tail .eq. 'SIGL'))

    end do ! do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa_interp_grid%nz

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i) .ge.  &
            & pmin .and. (hsa_interp_grid%tail(i) .eq. 'MANL' .or.        &
            & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

          ! Define local variables

          j               = j + 1
          meteo_grid%u(j) = dble(hsa_interp_grid%u(i))
          
       end if ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

    end do ! do i = 1, hsa_interp_grid%nz

    ! Check local variable and proceed accordingly

    where(hsa_interp_grid%u .eq. spval) hsa_interp_grid%u = -99.0
    where(hsa_interp_grid%v .le. -90.0) hsa_interp_grid%v = spval

    ! Define local variables

    interp_p%nz  = count(hsa_interp_grid%p .le. pmax .and.                &
         & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%v .ne.       &
         & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.             &
         & hsa_interp_grid%tail .eq. 'SIGL'))
    
    ! Loop through local variable

    do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz

          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%v(i) .ne. spval .and.    &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa_interp_grid%p(i)
             interp_p%var(j) = hsa_interp_grid%v(i)

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%v(i) .ne. spval
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz
 
          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%v(i) .eq. spval .and.    &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then         

             ! Define local variables

             dstp = hsa_interp_grid%p(i)

             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa_interp_grid%v(i) = dstvar
             goto 1004

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%v(i) .eq. spval
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Define local variables

1004   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count(hsa_interp_grid%p .le. pmax .and.              &
            & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%v .ne.    &
            & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.          &
            & hsa_interp_grid%tail .eq. 'SIGL'))

    end do ! do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa_interp_grid%nz

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i) .ge.  &
            & pmin .and. (hsa_interp_grid%tail(i) .eq. 'MANL' .or.        &
            & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

          ! Define local variables

          j               = j + 1
          meteo_grid%v(j) = dble(hsa_interp_grid%v(i))
          
       end if ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

    end do ! do i = 1, hsa_interp_grid%nz

    ! Check local variable and proceed accordingly

    where(hsa_interp_grid%v .eq. spval) hsa_interp_grid%v = -99.0
    where(hsa_interp_grid%z .le. -90.0) hsa_interp_grid%z = spval

    ! Define local variables

    interp_p%nz  = count(hsa_interp_grid%p .le. pmax .and.                &
         & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%z .ne.       &
         & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.             &
         & hsa_interp_grid%tail .eq. 'SIGL'))
    
    ! Loop through local variable

    do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz

          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%z(i) .ne. spval .and.    &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa_interp_grid%p(i)
             interp_p%var(j) = hsa_interp_grid%z(i)

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%z(i) .ne. spval
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Define local variables

       j = 0

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz
 
          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%z(i) .eq. spval .and.    &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then         

             ! Define local variables

             dstp = hsa_interp_grid%p(i)

             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa_interp_grid%z(i) = dstvar
             goto 1005

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%z(i) .eq. spval
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Define local variables

1005   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count(hsa_interp_grid%p .le. pmax .and.              &
            & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%z .ne.    &
            & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.          &
            & hsa_interp_grid%tail .eq. 'SIGL'))

    end do ! do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa_interp_grid%nz

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i) .ge.  &
            & pmin .and. (hsa_interp_grid%tail(i) .eq. 'MANL' .or.        &
            & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

          ! Define local variables

          j               = j + 1
          meteo_grid%z(j) = dble(hsa_interp_grid%z(i))
          
       end if ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

    end do ! do i = 1, hsa_interp_grid%nz

    ! Check local variable and proceed accordingly

    where(hsa_interp_grid%z .eq. spval) hsa_interp_grid%z = -99.0
    where(hsa_interp_grid%rh .le. -90.0) hsa_interp_grid%rh = spval

    ! Define local variables

    interp_p%nz  = count(hsa_interp_grid%p .le. pmax .and.                &
         & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%rh .ne.      &
         & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.             &
         & hsa_interp_grid%tail .eq. 'SIGL'))
    
    ! Loop through local variable

    do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)
    
       ! Define local variables

       call variable_interface_setup_struct(interp_p)
       j = 0

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz

          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%rh(i) .ne. spval .and.   &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

             ! Define local variables

             j               = j + 1
             interp_p%p(j)   = hsa_interp_grid%p(i)
             interp_p%var(j) = hsa_interp_grid%rh(i)

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%rh(i) .ne. spval
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Define local variables

       j = 0

       ! Loop through local variables

       do i = 1, hsa_interp_grid%nz
 
          ! Check local variable and proceed accordingly

          if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i)    &
               & .ge. pmin .and. hsa_interp_grid%rh(i) .eq. spval .and.   &
               & (hsa_interp_grid%tail(i) .eq. 'MANL' .or.                &
               & hsa_interp_grid%tail(i) .eq. 'SIGL')) then         

             ! Define local variables

             dstp = hsa_interp_grid%p(i)

             ! Compute local variables
      
             call math_methods_llp_interp(interp_p,dstp,dstvar)

             ! Define local variables

             hsa_interp_grid%rh(i) = dstvar
             goto 1006

          end if ! if(hsa_interp_grid%p(i) .le. pmax
                 ! .and. hsa_interp_grid%p(i) .ge. pmin
                 ! .and. hsa_interp_grid%rh(i) .eq. spval
                 ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
                 ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

       end do ! do i = 1, hsa_interp_grid%nz

       ! Define local variables

1006   continue

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(interp_p)

       ! Define local variables

       interp_p%nz = count(hsa_interp_grid%p .le. pmax .and.              &
            & hsa_interp_grid%p .ge. pmin .and. hsa_interp_grid%rh .ne.   &
            & spval .and. (hsa_interp_grid%tail .eq. 'MANL' .or.          &
            & hsa_interp_grid%tail .eq. 'SIGL'))

    end do ! do while(interp_p%nz .lt. nz .and. interp_p%nz .gt. 1)

    ! Define local variables

    j = 0

    ! Loop through local variables

    do i = 1, hsa_interp_grid%nz

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i) .ge.  &
            & pmin .and. (hsa_interp_grid%tail(i) .eq. 'MANL' .or.        &
            & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

          ! Define local variables

          j                = j + 1
          meteo_grid%rh(j) = dble(hsa_interp_grid%rh(i))
          
       end if ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

    end do ! do i = 1, hsa_interp_grid%nz

    ! Check local variable and proceed accordingly

    where(hsa_interp_grid%rh .eq. spval) hsa_interp_grid%rh = -99.0

    ! Loop through local variable

    do i = 1, meteo_grid%nz

       ! Check local variable and proceed accordingly

       if(meteo_grid%p(i) .gt. meteo_grid%psfc) then

          ! Define local variables

          meteo_grid%p(i)  = spval
          meteo_grid%t(i)  = spval
          meteo_grid%z(i)  = spval
          meteo_grid%u(i)  = spval
          meteo_grid%v(i)  = spval
          meteo_grid%rh(i) = spval

       end if ! if(meteo_grid%p(i) .gt. meteo_grid%psfc)

    end do ! do i = 1, meteo_grid%nz

    ! Compute local variables

    call meteo_methods_wvmxrt(meteo_grid)
    call meteo_methods_dwpttemp(meteo_grid)
    call meteo_methods_wspdwdir(meteo_grid)
    call time_methods_hmsts(hsa_interp_grid%logtime,time_hmsts)

    ! Define local variables

    hsa_interp_grid%fallrate = spval

    ! Check local variable and proceed accordingly

    if(hsa_grid%spgtime .eq. -999) then

       ! Define local variables

       statgrid%n = hsa_interp_grid%nz
       call variable_interface_setup_struct(statgrid)
       statgrid%var = hsa_interp_grid%t
       where(statgrid%var .le. -90.0) statgrid%var = spval     

       ! Compute local variables

       avgp     = 0.5*(pmax + pmin)
       call math_methods_stats(statgrid)
       avgt     = statgrid%mean
       fallrate = gsndfall2(avgp,avgt,spval,hsa_interp_grid%psfc,         &
            & .true.)/100.0
       call time_methods_hmsts(hsa_interp_grid%logtime,time_hmsts)
       spgtime  = float(time_hmsts) + (pmax-pmin)/fallrate
       call time_methods_sthms(int(spgtime),time_sthms)

       ! Define local variables

       hsa_interp_grid%spgtime = time_sthms

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(statgrid)

    end if ! if(hsa_interp_grid%spgtime .eq. -999)

    ! Compute local variables

    call time_methods_hmsts(hsa_interp_grid%reltime,time_hmsts)

    ! Define local variables

    hsa_interp_grid%time     = time_hmsts
    hsa_interp_grid%fallrate = 0.0

    ! Loop through local variable

    do i = 1, hsa_interp_grid%nz

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .ge. pmin .and. hsa_interp_grid%t(i)       &
            & .ge. -90.0) then

	    ! Define local variables

	    ppres = hsa_interp_grid%p(i)
	    ptemp = hsa_interp_grid%t(i)
	    goto 600

       end if ! if(hsa_interp_grid%p(i) .ge. pmin .and. 
              ! hsa_interp_grid%t(i) .ge. -90.0)

    end do ! do i = 1, hsa_interp_grid%nz

    ! Define local variables

600 continue
    call time_methods_hmsts(hsa_interp_grid%reltime,time_hmsts)
    ptime                                    = float(time_hmsts)
    hsa_interp_grid%time(hsa_interp_grid%nz) = ptime

    ! Loop through local variable

    do i = (hsa_interp_grid%nz - 1), 1, -1

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%t(i) .ge. -90.0) then

       	    ! Define local variables
       
	    avgp = 0.5*(ppres + hsa_interp_grid%p(i))
            avgt = 0.5*(ptemp + hsa_interp_grid%t(i))

            ! Compute local variables
          
            hsa_interp_grid%fallrate(i) = gsndfall2(avgp,avgt,spval,      &
                 & hsa_interp_grid%psfc,.true.)/100.0
            timediff                    = (hsa_interp_grid%p(i) - ppres)/ &
                 & hsa_interp_grid%fallrate(i)
            timediff                    = max(timediff,0.0)
            hsa_interp_grid%time(i)     = ptime - timediff

            ! Define local variables

	    ppres = hsa_interp_grid%p(i)
	    ptemp = hsa_interp_grid%t(i)
	    ptime = hsa_interp_grid%time(i)

       end if ! if(hsa_interp_grid%t(i) .ge. -90.0)

    end do ! do i = (hsa_interp_grid%nz - 1), 1, -1

    ! Define local variables

    plon                 = hsa_grid%rellon
    plat                 = hsa_grid%rellat
    ptime                = hsa_interp_grid%time(hsa_interp_grid%nz)
    hsa_interp_grid%lon  = plon
    hsa_interp_grid%lat  = plat

    ! Loop through local variable

    do i = hsa_interp_grid%nz, 2, -1

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i) .ge.  &
            & pmin .and. (hsa_interp_grid%tail(i) .eq. 'MANL' .or.        &
            & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

          ! Define local variables

          grid%lon = -1.0*plon
          grid%lat = plat

          ! Compute local variables

          dtime     = ptime - hsa_interp_grid%time(i)
          grid%head = 270.0 + atan2(hsa_interp_grid%v(i),                 &
               & hsa_interp_grid%u(i))*rad2deg
          grid%dist = sqrt(hsa_interp_grid%u(i)**2.0 +                    &
               & hsa_interp_grid%v(i)**2.0)*dtime

          ! Compute local variables

          call grid_methods_gcgeo(grid)

          ! Define local variables

          hsa_interp_grid%lon(i) = -1.0*grid%lon
          hsa_interp_grid%lat(i) = grid%lat
          ptime                  = hsa_interp_grid%time(i)
          plon                   = hsa_interp_grid%lon(i)
          plat                   = hsa_interp_grid%lat(i)

       else if(hsa_interp_grid%p(i) .eq. 1070.0) then 

          ! Define local variables

          hsa_interp_grid%lon(i) = hsa_interp_grid%spglon
          hsa_interp_grid%lat(i) = hsa_interp_grid%spglat

       else   ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

          ! Define local variables

          hsa_interp_grid%lon(i) = hsa_grid%lon(i)
          hsa_interp_grid%lat(i) = hsa_grid%lat(i)

       end if ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

    end do ! do i = hsa_interp_grid%nz, 2, -1

    ! Define local variables

    statgrid%n   = hsa_interp_grid%nz
    call variable_interface_setup_struct(statgrid)
    norma        = min(hsa_grid%rellon,hsa_grid%spglon)
    normb        = max(hsa_grid%rellon,hsa_grid%spglon)
    statgrid%var = hsa_interp_grid%lon

    ! Compute local variables

    call math_methods_normalize_values(statgrid,norma,normb)

    ! Define local variables

    if(if_normalize) hsa_interp_grid%lon = statgrid%var

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(statgrid)

    ! Define local variables

    statgrid%n   = hsa_interp_grid%nz
    call variable_interface_setup_struct(statgrid)
    norma        = min(hsa_grid%rellat,hsa_grid%spglat)
    normb        = max(hsa_grid%rellat,hsa_grid%spglat)
    statgrid%var = hsa_interp_grid%lat

    ! Compute local variables

    call math_methods_normalize_values(statgrid,norma,normb)

    ! Define local variables

    if(if_normalize) hsa_interp_grid%lat = statgrid%var

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(statgrid)

    ! Define local variables

    j = 0

    ! Loop through local variable

    do i = 1, hsa_interp_grid%nz

       ! Check local variable and proceed accordingly

       if(hsa_interp_grid%p(i) .le. pmax .and. hsa_interp_grid%p(i) .ge.  &
            & pmin .and. (hsa_interp_grid%tail(i) .eq. 'MANL' .or.        &
            & hsa_interp_grid%tail(i) .eq. 'SIGL')) then

          ! Compute local variables

          timediff    = (hsa_interp_grid%time(hsa_interp_grid%nz) -       &
               & hsa_interp_grid%time(i))/86400.0
          julian_time = julian_day + dble(timediff)

          ! Define local variables

          j                   = j + 1
          meteo_grid%lon(j)   = -1.0*dble(hsa_interp_grid%lon(i))
          meteo_grid%lat(j)   = dble(hsa_interp_grid%lat(i))
          meteo_grid%jdate(j) = julian_time

       end if ! if(hsa_interp_grid%p(i) .le. pmax
              ! .and. hsa_interp_grid%p(i) .ge. pmin
              ! .and. (hsa_interp_grid%tail(i) .eq. 'MANL'
              ! .or. hsa_interp_grid%tail(i) .eq. 'SIGL'))

    end do ! do i = 1, hsa_interp_grid%nz

    ! Loop through local variable

    do i = hsa_interp_grid%nz, 1, -1
 
       ! Compute local variables

       timediff    = (hsa_interp_grid%time(hsa_interp_grid%nz) -          &
            & hsa_interp_grid%time(i))/86400.0
       julian_time = julian_day + dble(timediff)

       ! Define local variables

       call time_methods_gregorian_date(dble(julian_time),yyyy,mm,dd,hh,  &
            & nn,ss)
       timediff        = real(julian_time - dble(int(julian_time)))
       write(yymmdd,'(i4,2(i2.2))') yyyy, mm, dd
       write(hhnn,'(2(i2.2))') hh, nn
       read(yymmdd(3:8),'(f7.0)') hsa_grid%yymmdd(i)
       read(hhnn,'(i4)') hsa_grid%gmt(i)
       hsa_grid%lon(i) = hsa_interp_grid%lon(i)
       hsa_grid%lat(i) = hsa_interp_grid%lat(i)

    end do ! do i = hsa_interp_grid%nz, 1, -1

    ! Define local variables

    open(99,file=trim(adjustl(filename))//'.base',form='formatted')
    call time_methods_hmsts(hsa_grid%spgtime,spg_hmsts)
    
    ! Check local variable and proceed accordingly

    if(spg_hmsts .ge. rel_hmsts) then

       ! Define local variables
 
       spg_julian = rel_julian + dble((dble(spg_hmsts -                   &
            & rel_hmsts)/86400.0))

    end if ! if(spg_hmsts .ge. rel_hmsts)

    ! Check local variable and proceed accordingly

    if(spg_hmsts .lt. rel_hmsts) then

       ! Define local variables

       spg_julian = rel_julian + dble(dble(spg_hmsts)/86400.0)

    end if ! if(spg_hmsts .lt. rel_hmsts)

    ! Define local variables

    write(99,500) -1.0*hsa_grid%spglon, hsa_grid%spglat,                  &
         & (spg_julian - rel_julian)*86400.0, meteo_grid%lon(1),          &
         & meteo_grid%lat(1), (meteo_grid%jdate(1) -                      &
         & meteo_grid%jdate(meteo_grid%nz))*86400.0
    close(99)

    ! Define local variables

    open(99,file=trim(adjustl(filename))//'.drft',form='formatted')

    ! Loop through local variable

    do i = 1, hsa_interp_grid%nz

       ! Define local variables

       write(99,501) hsa_grid%wx(i), hsa_grid%yymmdd(i), hsa_grid%gmt(i), &
            & hsa_grid%lat(i), hsa_grid%lon(i), hsa_grid%p(i),            &
            & hsa_grid%t(i), hsa_grid%rh(i), hsa_grid%z(i),               &
            & hsa_grid%u(i), hsa_grid%v(i), hsa_grid%tail(i)       

    end do ! do i = 1, hsa_interp_grid%nz

    ! Define local variables

    close(99)

    ! Check local variable and proceed accordingly

    if(write_nc_skewt) then

       ! Define local variables

       filename = trim(adjustl(filename))//'.nc'
       call fileio_interface_write(meteo_grid,filename)

    end if !if(write_nc_skewt)

    ! Define local variables

1007 continue

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(meteo_grid)
    call variable_interface_cleanup_struct(hsa_interp_grid)

    ! Define local variables
500 format(6(f13.5,1x))
501 format(i2,1x,f7.0,1x,i4,1x,2(f7.3,1x),3(f6.1,1x),f7.1,2(f6.1,1x),a4)

    !=====================================================================

  end subroutine tempdrop_drift_hsa

  !=======================================================================

end module tempdrop_sonde_interface
