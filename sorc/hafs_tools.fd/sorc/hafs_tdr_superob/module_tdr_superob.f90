MODULE TDR_SUPEROB

USE TDR_COMMON

IMPLICIT NONE

integer, parameter, private :: max_allowed_obs = 100000

CONTAINS

SUBROUTINE split_bufr_sweeps(bfr_ascii_filename, superob_filename, debug_level)
!
! Description:
!   Splits decoded bufr output into individual sweeps for further processing
!
! Input:
!   bfr_ascii_filename - File containing decoded BUFR data to be superobbed
!   debug_level        - 0=quiet, 1=ASCII files for each sweep, 2=sweep ASCII + extra debug files in calc_superobs
!
! Output Files:
!   swp_filename       - (if debug_level .ge. 1) File containing radials for current sweep
!
implicit none

!------ Variable declarations ------
! File names/handles
character(21),intent(in)                       :: bfr_ascii_filename
character(120),intent(in)                      :: superob_filename
character(120)                                 :: swp_filename
character(120)                                 :: writeout_fmt
integer,intent(in)                             :: debug_level
integer                                        :: bfrascii_unit, swp_unit, iost
logical                                        :: fileexists

character(2)                                   :: tailno
character(2), dimension(max_allowed_obs)       :: swp_tailno
character(15)                                  :: datestring
double precision                               :: radial_date
double precision                               :: radial_stalat, radial_stalon, radial_stahgt, radial_azm, radial_tilt
double precision, dimension(:), allocatable    :: radial_obs
double precision, dimension(max_allowed_obs)   :: swp_date, swp_stalat, swp_stalon, swp_stahgt, swp_azm, swp_tilt
double precision, dimension(:,:), allocatable  :: swp_obs
double precision                               :: azmdiff
integer                                        :: levs, k, i
integer                                        :: firstradial, dir_flag
integer                                        :: n_radials_swp
real, allocatable, dimension(:)                :: radius

!------ End variable declarations ------

! Assign file units and writeout format for sweep file
bfrascii_unit = 13
swp_unit = 14
writeout_fmt = '(A2,1X,F15.0,1X,2(F6.2,1X),F7.1,1X,2(F6.2,1X),100(F7.2,1X))'

write(6,*) ' '
write(6,*) 'Processing decoded BUFR file ',trim(bfr_ascii_filename),' by sweep'
write(6,*) ' '

if (debug_level .ge. 1) write(6,*) '-- Opening decoded BUFR ASCII file'
open(bfrascii_unit,file=trim(bfr_ascii_filename),status='old',action='read',iostat=iost)
call check_iostat_open('split_bufr_sweeps',bfr_ascii_filename,iost)

! Get number of gates and radius info from decoded BUFR file
if (debug_level .ge. 1) write(6,*) '-- Reading in number of gates'
read(bfrascii_unit,fmt='(I3)',iostat=iost) levs
call check_iostat_read('split_bufr_sweeps',bfr_ascii_filename,iost,.true.)
allocate(radius(levs))
if (debug_level .ge. 1) write(6,*) '-- Reading in radius info'
read(bfrascii_unit,fmt='(8(F10.2,1X))') (radius(k),k=1,levs)

! Allocate ob arrays
allocate(radial_obs(levs))
allocate(swp_obs(max_allowed_obs,levs))

! Initialize TDR scan variables
!
! Note:
! - Cannot determine whether TDR scan direction is fore or aft from a single BUFR record because the aircraft
!   heading is not provided in the file. Different scan directions are simply denoted as "_0" and "_1".
! - Not all files marked "_0" or "_1" are necessarily in the same direction as each other, but all radials within
!   an individual file will point in the same direction.
!
dir_flag = 0           ! 0 or 1
firstradial = 0        ! first sweep indicator
n_radials_swp = 0      ! number of radials in current sweep

! Read decoded BUFR file
if (debug_level .ge. 1) write(6,*) '-- Reading in radial data'
do
  read(bfrascii_unit,fmt=writeout_fmt,iostat=iost) &
    tailno,radial_date,radial_stalat,radial_stalon,radial_stahgt,radial_azm,radial_tilt,(radial_obs(k),k=1,levs)

  if (iost .lt. 0) then
    call check_iostat_read('split_bufr_sweeps',bfr_ascii_filename,iost,.false.)

    ! Check whether there is a sweep in progress. If so, superob it and dump to file (if debug)
    if (n_radials_swp .gt. 0) then

      if (debug_level .ge. 1) write(6,*) '-- Processing final sweep...'

      write(datestring, '(F15.0)') swp_date(1)
      write(swp_filename, '(A2,A,A14,A,I1,A4)') swp_tailno(1),'_',datestring,'_',dir_flag,'.swp'

      if (debug_level .ge. 1) then

        write(6,*) ' '
        write(6,*) '-- Writing final sweep file ',trim(swp_filename)

        inquire(file=swp_filename,exist=fileexists)
        if(fileexists) then
          write(6,*) '-- Found existing file ',trim(swp_filename),'. Appending _2 to filename and proceeding.'
          swp_filename = swp_filename(1:len_trim(swp_filename)-4)//'_2'//'.swp'
        endif

        open(swp_unit, file=swp_filename, status='new', action='write', iostat=iost)
        call check_iostat_open('split_bufr_sweeps',swp_filename,iost)

        write(swp_unit,fmt='(I4)',iostat=iost) levs
        call check_iostat_write('split_bufr_sweeps',swp_filename,iost)

        write(swp_unit,fmt='(8(F10.2,1X))') (radius(k),k=1,levs)
        call check_iostat_write('split_bufr_sweeps',swp_filename,iost)

        do i=1,n_radials_swp
          write(swp_unit,fmt=writeout_fmt,iostat=iost) swp_tailno(i), swp_date(i), swp_stalat(i), &
            swp_stalon(i), swp_stahgt(i), swp_azm(i), swp_tilt(i), (swp_obs(i,k),k=1,levs)
          call check_iostat_write('split_bufr_sweeps',swp_filename,iost)
        enddo

        close(swp_unit)

      endif ! if (debug_level .ge. 1)

      call calc_superobs(swp_tailno(1), swp_date(1:n_radials_swp), swp_stalat(1:n_radials_swp), &
        swp_stalon(1:n_radials_swp), swp_stahgt(1:n_radials_swp), swp_azm(1:n_radials_swp), &
        swp_tilt(1:n_radials_swp), swp_obs(1:n_radials_swp,1:levs), radius, swp_filename, &
        superob_filename, debug_level)

    endif ! if (n_radials_swp .gt. 0)

    exit

  elseif (iost .gt. 0) then
    call check_iostat_read('split_bufr_sweeps',bfr_ascii_filename,iost,.false.)

  else
    if (firstradial .eq. 0) then

      n_radials_swp = n_radials_swp + 1

      ! Store obs in sweep arrays
      swp_tailno(n_radials_swp)    = tailno
      swp_date(n_radials_swp)      = radial_date
      swp_stalat(n_radials_swp)    = radial_stalat
      swp_stalon(n_radials_swp)    = radial_stalon
      swp_stahgt(n_radials_swp)    = radial_stahgt
      swp_azm(n_radials_swp)       = radial_azm
      swp_tilt(n_radials_swp)      = radial_tilt
      do k=1,levs
        swp_obs(n_radials_swp,k)   = radial_obs(k)
      enddo

      ! Mark first record as read
      firstradial = 1

    else

      ! Compare azimuth angle from current record to previous one to determine if radar direction has changed
      ! and a new sweep needs to be started. This will be a big jump, >100 deg.
      !azmdiff = abs(abs(swp_azm(n_radials_swp)) - abs(radial_azm))
      azmdiff = min(abs(swp_azm(n_radials_swp) - radial_azm), (360 - abs(swp_azm(n_radials_swp) - radial_azm)))

      if (azmdiff .gt. 100.) then

        write(datestring, '(F15.0)') swp_date(1)
        write(swp_filename, '(A2,A,A14,A,I1,A4)') swp_tailno(1),'_',datestring,'_',dir_flag,'.swp'

        ! (Debug) Dump old sweep to file
        if (debug_level .ge. 1) then

          write(6,*) ' '
          write(6,*) '-- Writing out sweep file ',trim(swp_filename)

          inquire(file=swp_filename,exist=fileexists)
          if(fileexists) then
            write(6,*) '-- Found existing file ',trim(swp_filename),'. Appending _2 to filename and proceeding.'
            swp_filename = swp_filename(1:len_trim(swp_filename)-4)//'_2'//'.swp'
          endif

          open(unit=swp_unit, file=swp_filename, status='new', action='write', iostat=iost)
          call check_iostat_open('split_bufr_sweeps',swp_filename,iost)

          write(unit=swp_unit, fmt='(I4)', iostat=iost) levs
          call check_iostat_write('split_bufr_sweeps',swp_filename,iost)

          write(unit=swp_unit, fmt='(8(F10.2,1X))', iostat=iost) (radius(k),k=1,levs)
          call check_iostat_write('split_bufr_sweeps',swp_filename,iost)

          do i = 1,n_radials_swp
            write(swp_unit, fmt=writeout_fmt, iostat=iost) swp_tailno(i),swp_date(i),swp_stalat(i),swp_stalon(i), &
              swp_stahgt(i),swp_azm(i),swp_tilt(i),(swp_obs(i,k),k=1,levs)
            call check_iostat_write('split_bufr_sweeps',swp_filename,iost)
          enddo

          close(swp_unit)

        endif ! if (debug_level .ge. 1) Dump old sweep to file

        ! Calculate superobs for old sweep
        if (debug_level .ge. 1) write(6,*) '-- Calculating superobs'
        call calc_superobs(swp_tailno(1), swp_date(1:n_radials_swp), swp_stalat(1:n_radials_swp), &
          swp_stalon(1:n_radials_swp), swp_stahgt(1:n_radials_swp), swp_azm(1:n_radials_swp), &
          swp_tilt(1:n_radials_swp), swp_obs(1:n_radials_swp,1:levs), radius, swp_filename, &
          superob_filename, debug_level)

        ! Switch direction for new sweep
        if (dir_flag .eq. 0) then
          dir_flag = 1
        elseif (dir_flag .eq. 1) then
          dir_flag = 0
        else
          write(6,*) '-- Unexpected dir_flag value (dir_flag = ',char(dir_flag),'). Stopping...'
          stop
        endif

        ! Reset sweep arrays and store current record
        swp_date   = 0.0
        swp_stalat = 0.0
        swp_stalon = 0.0
        swp_stahgt = 0.0
        swp_azm    = 0.0
        swp_tilt   = 0.0
        swp_obs    = 0.0

        n_radials_swp = 1

        swp_tailno(n_radials_swp)    = tailno
        swp_date(n_radials_swp)      = radial_date
        swp_stalat(n_radials_swp)    = radial_stalat
        swp_stalon(n_radials_swp)    = radial_stalon
        swp_stahgt(n_radials_swp)    = radial_stahgt
        swp_azm(n_radials_swp)       = radial_azm
        swp_tilt(n_radials_swp)      = radial_tilt
        do k=1,levs
          swp_obs(n_radials_swp,k)   = radial_obs(k)
        enddo

      else

        n_radials_swp = n_radials_swp + 1

        swp_tailno(n_radials_swp)    = tailno
        swp_date(n_radials_swp)      = radial_date
        swp_stalat(n_radials_swp)    = radial_stalat
        swp_stalon(n_radials_swp)    = radial_stalon
        swp_stahgt(n_radials_swp)    = radial_stahgt
        swp_azm(n_radials_swp)       = radial_azm
        swp_tilt(n_radials_swp)      = radial_tilt
        do k=1,levs
          swp_obs(n_radials_swp,k)   = radial_obs(k)
        enddo

      endif ! azimuth angle check

    endif ! firstradial check

  endif ! iostat check

enddo ! big do loop

close(bfrascii_unit)

END SUBROUTINE split_bufr_sweeps

!!!!!!!!!!!!!!!

SUBROUTINE calc_superobs(tailno, timerad, rrlat, rrlon, rra, eazm, eev, input_vr, &
  radius_m, swp_filename, superob_filename, debug_level)
!
! Description:
!   Calculates superobs for one TDR sweep. Appends superobs to main superob file.
!   Based on TDR superob code developed by Fuqing Zhang, Yonghui Weng, and John Gamache.
!
! Input:
!   tailno           - NOAA aircraft tail number (42, 43, or 49)
!   timerad          - datestamp (YYYYMMDDHHMMSS)
!   rrlat            - radar latitude (deg)
!   rrlon            - radar longitude (deg)
!   rra              - radar altitude (m)
!   eazm             - radar azimuth (deg)
!   eev              - radar tilt/elevation angle (deg)
!   input_vr         - observations
!   radius_m         - radius (m) corresponding to each gate in obs array
!   swp_filename     - file containing writeout of sweep records (used for debug)
!   superob_filename - file containing all superobs calculated from input BUFR file
!   debug_level      - 0=quiet, 1=ASCII files for each sweep, 2=sweep ASCII + extra debug files in calc_superobs
!
! Output Files:
!   superob_filename - ASCII file containing superobs for all sweeps, all aircraft
!   swpso_filename   - (debug_level .ge. 1) File containing superobs for current sweep
!   debug_*_filename - (debug_level .eq. 2) Files containing details of superob grid, obs, etc. for current sweep
!
implicit none

!------ Variable declarations ------
! File names/units
character(120),intent(in)       :: swp_filename, superob_filename
character(120)                  :: swp_filename_base
character(120)                  :: swpso_filename
character(120)                  :: debug_binlist_filename, debug_rawobs_filename, debug_qc1obs_filename
character(120)                  :: debug_binrawct_filename, debug_binavg_filename
integer, intent(in)             :: debug_level
integer                         :: swp_unit, swpso_unit, so_unit
integer                         :: debug_binlist_unit, debug_rawobs_unit, debug_qc1obs_unit, debug_binct_unit
integer                         :: debug_binrawct_unit, debug_binavg_unit
logical                         :: fileexists

! Namelist
character(11)                   :: nml_filename
integer                         :: nml_unit, iost
real                            :: elevmax, minraddis, maxraddis, draddis, dazm, stdrv, minazspan, minrv, maxrv
integer                         :: minbinct, nclose
namelist /so_attribute/ elevmax, minraddis, maxraddis, draddis, dazm, stdrv, minazspan, minrv, maxrv, minbinct, nclose

! Sweep parameters
integer                         :: i, j, k, irawradial
integer                         :: nrawradials, maxgates
character(2), intent(in)        :: tailno
real, intent(in)                :: radius_m(:)
real, allocatable, dimension(:) :: radius
double precision, intent(in)    :: timerad(:)
double precision, intent(in)    :: rrlat(:), rrlon(:), rra(:), eazm(:), eev(:), input_vr(:,:)
real, allocatable, dimension(:) :: theta, rtrackmult
character(28)                   :: swptime_str

! Superob bin parameters
integer                         :: iradbin, iazbin
integer                         :: i_bin, n_rad_bins, n_azm_bins, n_all_bins, n_all_bins_check
integer                         :: max_obs_bin
real                            :: r_dis
real                            :: bin_rge_n, bin_azm_n, bin_da_n, bin_az_span, bin_dr_ctr
real, allocatable, dimension(:) :: bin_rge, bin_azm, bin_hgt_offaxis, bin_da_ctr

! Superob parameters
integer                                         :: min_elv_idx, n_raw_obs, n_qc1_obs
integer                                         :: n, isobin
real                                            :: vr_missing
real                                            :: min_elv
real                                            :: max_radius, alpha1, d1, d2, d3, d4
real                                            :: x, y, alpha
real                                            :: dda, ddr, mean
integer, allocatable, dimension(:)              :: raw_num_bin
real, dimension(max_allowed_obs)                :: rtrack, rtrack_qc1, raw_hgt, hgt_qc1
real, allocatable, dimension(:,:)               :: rtrack2
double precision, dimension(max_allowed_obs,11) :: raw_vr, vr_qc1
double precision, allocatable, dimension(:,:,:) :: raw_data_bin
double precision, allocatable, dimension(:,:)   :: bin_stats
double precision, allocatable, dimension(:,:)   :: so_vr
double precision, allocatable, dimension(:,:)   :: close_rv
double precision, allocatable, dimension(:)     :: close_dis

! Other constants
real   :: deg2rad, rad2deg

!------ End variable declarations ------

! Assign file units
nml_unit                = 9
nml_filename            = 'tdr_so.nml'

debug_binlist_unit      = 91
debug_rawobs_unit       = 92
debug_qc1obs_unit       = 93
debug_binct_unit        = 94
debug_binavg_unit       = 95

swp_unit                = 100
swpso_unit              = 101
so_unit                 = 102

swpso_filename          = trim(swp_filename_base)//'.so'
swp_filename_base       = swp_filename(1:len_trim(swp_filename)-4) ! remove .swp
swptime_str             = swp_filename_base(4:17) ! extract timestamp for sweep

debug_binlist_filename  = 'debug_calcso_binlist.'//trim(swp_filename_base)//'.txt'
debug_rawobs_filename   = 'debug_calcso_rawobs.'//trim(swp_filename_base)//'.txt'
debug_qc1obs_filename   = 'debug_calcso_qc1obs.'//trim(swp_filename_base)//'.txt'
debug_binrawct_filename = 'debug_calcso_binlist_rawct.'//trim(swp_filename_base)//'.txt'
debug_binavg_filename   = 'debug_calcso_binavg.'//trim(swp_filename_base)//'.txt'

! Constants
deg2rad                 = 3.141596/180.
rad2deg                 = 180./3.141596
vr_missing              = -888.
max_obs_bin             = 1000

! Convert radius array from m to km
allocate(radius(size(radius_m)))
radius = radius_m/1000.

!!!!!!!!!!!!!!!!!
! Read namelist !
!!!!!!!!!!!!!!!!!

if (debug_level .ge. 1) write(6,*) '(In calc_superobs)'

if (debug_level .ge. 1) write(6,*) '-- Reading in namelist'
call check_file_missing('calc_superobs',nml_filename)
open(unit=nml_unit, file=nml_filename, status='old', access='sequential', iostat=iost)
call check_iostat_open('calc_superobs',nml_filename,iost)
read(unit=nml_unit, nml=so_attribute, iostat=iost)
call check_iostat_read('calc_superobs',nml_filename,iost,.true.)
close(nml_unit)


!!!!!!!!!!!!!!!!!!
! Define SO bins !
!!!!!!!!!!!!!!!!!!

i_bin = 0
n_all_bins = 0
n_azm_bins = 0
n_rad_bins = int((maxraddis-minraddis) / draddis) + 1
bin_dr_ctr = draddis / 2.

! Loop through to get a head count so we can allocate the arrays
prelim_range_loop: do iradbin = 1 , n_rad_bins
  r_dis = minraddis + draddis*(float(iradbin) - 0.5)
  bin_az_span = minazspan - ((minazspan - dazm)/(n_rad_bins - 1))*(iradbin-1)
  bin_az_span = min(bin_az_span,89.)
  n_azm_bins = int(360. / bin_az_span)

  prelim_azimuth_loop : do iazbin = 1,n_azm_bins
    n_all_bins = n_all_bins + 1;
    bin_rge_n = r_dis
    bin_azm_n = 0. + ( bin_az_span * (iazbin - 1) )
    bin_da_n = bin_az_span / 2.

  enddo prelim_azimuth_loop
enddo prelim_range_loop

! Allocate arrays with superob bin count
allocate(bin_rge(n_all_bins))
allocate(bin_azm(n_all_bins))
allocate(bin_da_ctr(n_all_bins))
allocate(bin_hgt_offaxis(n_all_bins))

! Loop again to populate arrays
if (debug_level .eq. 2) then
  open(unit=debug_binlist_unit,file=debug_binlist_filename,status='replace',iostat=iost)
  call check_iostat_open('calc_superobs',debug_binlist_filename,iost)
endif

i_bin = 0
pop_range_loop: do iradbin = 1, n_rad_bins
  r_dis = minraddis + draddis*(float(iradbin) - 0.5)
  bin_az_span = minazspan - ((minazspan - dazm)/(n_rad_bins - 1))*(iradbin-1)
  bin_az_span = min(bin_az_span,89.)
  n_azm_bins = int(360. / bin_az_span)

  pop_azimuth_loop : do iazbin = 1, n_azm_bins
    i_bin = i_bin + 1;

    bin_rge(i_bin) = r_dis
    bin_azm(i_bin) = 0. + ( bin_az_span * (iazbin - 1) )
    bin_da_ctr(i_bin) = bin_az_span / 2.
    bin_hgt_offaxis(i_bin) = cos(bin_azm(i_bin) * deg2rad) * bin_rge(i_bin)

    if (debug_level .eq. 2) then
      write(debug_binlist_unit,'(i5, 1X, 3(f8.3,1X))', iostat=iost) i_bin,bin_azm(i_bin),&
        r_dis,bin_hgt_offaxis(i_bin)
      call check_iostat_write('calc_superobs',debug_binlist_filename,iost)
    endif ! if (debug_level .eq. 2)

  enddo pop_azimuth_loop
enddo pop_range_loop

if (debug_level .eq. 2) close(debug_binlist_unit)

n_all_bins_check = i_bin
if(n_all_bins .ne. n_all_bins_check) then
  write(6,*) '-- ERROR (calc_superobs): Mismatch in number of possible SO bins. Stopping...'
  stop
endif

! Get header data
maxgates = 0
nrawradials = 0
maxgates = size(radius)
nrawradials = size(timerad,1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculate additional angles for obs !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Difference between min azm angles in the sweep and gate azm angle
! accounts for orientation of aircraft.
min_elv = 0.
min_elv_idx = 0
max_radius = radius(maxgates) ! farthest distance from radar

! Search for radial with min. elevation angle wrt nadir ("bottom" of sweep)
do irawradial = 1, nrawradials
  if (eev(irawradial) < min_elv) then
    min_elv = eev(irawradial)
    min_elv_idx = irawradial
  endif
enddo

! Calculate theta (ob azimuth in superob cone local coordinates)
! and rtrackmult (angular distance off track, i.e., axis of superob grid cone)
! alpha1 = half-angle of sweep cone
! d1 = horizontal component of range along nadir
! d2 = radius of cone with half-angle == azm diff and hgt == along-track dist
!      (perpendicular distance off nadir)
! d3 = d2
! d4 = vertical component of range along zenith
! theta = local azimuth angle around cone
! rtrackmult = adjustment to range to align it with cone
allocate(theta(nrawradials))
allocate(rtrackmult(nrawradials))
do j = 1, nrawradials
  alpha1 = eazm(j) - eazm(min_elv_idx)  ! deg
  d1 = max_radius * cos(eev(j)*deg2rad) ! km
  d2 = d1 * sin(alpha1*deg2rad)         ! km
  d3 = d2;                              ! km
  d4 = max_radius * sin(eev(j)*deg2rad) ! km
  theta(j) = atan2(d3,d4)*rad2deg       ! deg
  rtrackmult(j) = sqrt(d3*d3 + d4*d4)/max_radius ! non-dimensional

  if(theta(j) .lt. 0.) theta(j) = theta(j)+360.
  if(theta(j) .gt. 360.) theta(j) = theta(j)-360.
enddo

! --- Make array of all raw obs
!            range for vr, eev, and radius
if (debug_level .eq. 2) then
  open(unit=debug_rawobs_unit,file=trim(debug_rawobs_filename),iostat=iost,status='replace')
  call check_iostat_open('calc_superobs',debug_rawobs_filename,iost)
endif

n_raw_obs = 0
do j = 1, nrawradials
  do i = 1, maxgates
    n_raw_obs = n_raw_obs+1
    raw_vr(n_raw_obs,1) = timerad(j)
    raw_vr(n_raw_obs,2) = rrlat(j)
    raw_vr(n_raw_obs,3) = rrlon(j)
    raw_vr(n_raw_obs,4) = rra(j)
    raw_vr(n_raw_obs,5) = eazm(j)
    raw_vr(n_raw_obs,6) = eev(j)
    raw_vr(n_raw_obs,7) = radius(i) ! km
    raw_vr(n_raw_obs,8) = input_vr(j,i)
    raw_vr(n_raw_obs,9) = theta(j)

    ! Convert azimuth range from [0,360) to (-180,180]
    if (raw_vr(n_raw_obs,5) .gt. 180) raw_vr(n_raw_obs,5) = raw_vr(n_raw_obs,5) - 360.

    rtrack(n_raw_obs)   = radius(i)*rtrackmult(j)
    raw_hgt(n_raw_obs)  = cos(theta(j)*deg2rad)*rtrack(n_raw_obs)

    if (debug_level .eq. 2) then
      write(debug_rawobs_unit,'(f15.0,11(f12.3))',iostat=iost)(raw_vr(n_raw_obs,k),k=1,9),&
        rtrack(n_raw_obs),raw_hgt(n_raw_obs)
      call check_iostat_write('calc_superobs',debug_rawobs_filename,iost)
    endif
  enddo
enddo

if (debug_level .eq. 2) close(debug_rawobs_unit)

! QC Step 1: Skip obs with missing values or obs outside prescribed
!            range for vr, eev, and radius
if (debug_level .eq. 2) then
  open(unit=debug_qc1obs_unit,file=debug_qc1obs_filename,iostat=iost,status='replace')
  call check_iostat_open('calc_superobs',debug_qc1obs_filename,iost)
endif

n_qc1_obs = 0
do i = 1,n_raw_obs
  if (abs(raw_vr(i,8)) >= minrv .and. abs(raw_vr(i,8)) <= maxrv .and. raw_vr(i,7) >= minraddis) then
    n_qc1_obs             = n_qc1_obs + 1
    vr_qc1(n_qc1_obs,1)   = raw_vr(i,1)
    vr_qc1(n_qc1_obs,2)   = raw_vr(i,2)
    vr_qc1(n_qc1_obs,3)   = raw_vr(i,3)
    vr_qc1(n_qc1_obs,4)   = raw_vr(i,4)
    vr_qc1(n_qc1_obs,5)   = raw_vr(i,5)
    vr_qc1(n_qc1_obs,6)   = raw_vr(i,6)
    vr_qc1(n_qc1_obs,7)   = raw_vr(i,7)
    vr_qc1(n_qc1_obs,8)   = raw_vr(i,8)
    vr_qc1(n_qc1_obs,9)   = raw_vr(i,9)
    rtrack_qc1(n_qc1_obs) = rtrack(i)
    hgt_qc1(n_qc1_obs)    = raw_hgt(i)

    if (debug_level .eq. 2) then
      write(debug_qc1obs_unit,'(f15.0,11(f12.3))',iostat=iost)(vr_qc1(n_qc1_obs,j),j=1,9),&
        rtrack_qc1(n_qc1_obs),hgt_qc1(n_qc1_obs)
      call check_iostat_write('calc_superobs',debug_qc1obs_filename,iost)
    endif ! if (debug_level .eq. 2)
  endif ! if (abs(raw_vr(i,8)) >= minrv .and. abs(raw_vr(i,8)) <= maxrv .and. raw_vr(i,7) >= minraddis)
enddo

if (debug_level .ge. 1) write(6,*) '-- Number of obs passed QC1: ',n_qc1_obs

if (debug_level .eq. 2) close(debug_qc1obs_unit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Divide up obs up into superob bins !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
allocate(rtrack2(n_all_bins,max_obs_bin))
allocate(raw_num_bin(n_all_bins))
allocate(raw_data_bin(n_all_bins,max_obs_bin,11))
allocate(so_vr(n_all_bins,11))

if (debug_level .eq. 2) then
  open(unit=debug_binrawct_unit,file=debug_binrawct_filename,status='replace',iostat=iost)
  call check_iostat_open('calc_superobs',debug_binrawct_filename,iost)
endif

do isobin = 1,n_all_bins
  n = 0
  do j = 1,n_qc1_obs
    dda = abs(vr_qc1(j,9)-bin_azm(isobin))
    if (dda .le. bin_da_ctr(isobin)) then
      ddr = abs(rtrack_qc1(j)-bin_rge(isobin))
      if ((ddr .le. bin_dr_ctr) .and. (n .lt. max_obs_bin)) then
        n = n + 1
        raw_data_bin(isobin,n,1:9) = vr_qc1(j,1:9)
        rtrack2(isobin,n) = rtrack_qc1(j)
      endif
    endif
  enddo

  raw_num_bin(isobin) = n

  if (debug_level .eq. 2) then
    write(debug_binrawct_unit,'(I4)',iostat=iost) n
    call check_iostat_write('calc_superobs',debug_binrawct_filename,iost)
  endif

enddo

! ---- Average superobs bins
if (debug_level .eq. 2) then
  open(unit=debug_binavg_unit,file=debug_binavg_filename,iostat=iost,status='replace')
  call check_iostat_open('calc_superobs',debug_binavg_filename,iost)
endif

allocate(bin_stats(n_all_bins,4))
do i = 1,n_all_bins
  if (raw_num_bin(i) >= minbinct) then

    ! Mean
    mean = 0.
    do j = 1,raw_num_bin(i)
      mean = mean + raw_data_bin(i, j, 8)
    enddo
    mean = mean/raw_num_bin(i)

    ! Std
    !stdm = 0.
    !do j = 1, raw_num_bin(i)
    !    dvr = abs(raw_data_bin(i, j, 8) - mean) / stdrv

    bin_stats(i,1) = raw_num_bin(i)
    bin_stats(i,2) = mean
    !bin_stats(i,3) = stdev
    !bin_stats(i,4) =

    if (debug_level .eq. 2) then
      write(debug_binavg_unit,'(f15.0,8(f12.3),i4)',iostat=iost)raw_data_bin(i,1,1:3),bin_hgt_offaxis(i),&
        bin_azm(i),raw_data_bin(i,1,6),bin_rge(i),bin_stats(i,2),&
        rtrack2(i,1),raw_num_bin(i)
      call check_iostat_write('calc_superobs',debug_binavg_filename,iost)
    endif

  else
    bin_stats(i,1) = raw_num_bin(i)
    bin_stats(i,2) = vr_missing
  endif
enddo
if (debug_level .eq. 2) close(debug_binavg_unit)

! Pin down elevation angle of superob bin based on closest ob to bin center
allocate(close_rv(nclose,9))
allocate(close_dis(nclose))
do i=1,n_all_bins
  if(raw_num_bin(i) >= minbinct) then
    n = nclose ! cap number of obs in bin at nclose
    if (raw_num_bin(i) <= nclose) n = raw_num_bin(i)
    close_rv(1:nclose,1:9) = vr_missing
    close_dis(1:nclose) = 999.
    do j=1,n
      close_rv(j,1:9) = raw_data_bin(i, j, 1:9)

      ! Angle between bin center azimuth and raw ob azimuth in radians
      alpha = abs(raw_data_bin(i,j,9) - bin_azm(i))*deg2rad ! radians
      ! Along-track distance (x) and cross-track distance (y) between ob and bin center
      x = rtrack2(i,j)*cos(alpha) - bin_rge(i)
      y = rtrack2(i,j)*sin(alpha)
      close_dis(j) = sqrt( x*x + y*y )
    enddo

    j = minloc(close_dis(1:n),dim=1)  ! j = index of closest distance in close_dis array
    so_vr(i,1:7) = close_rv(j,1:7)
    so_vr(i,8) = bin_stats(i,2) ! mean vr
    so_vr(i,9) = close_rv(j,9)
  else
    so_vr(i,8) = vr_missing
  endif
enddo

! Write out individual superobs, excluding bins with missing values, to file *.so
if (debug_level .ge. 1) then
  open(unit=swpso_unit,file=trim(swp_filename_base)//'.so',iostat=iost)
  call check_iostat_open('calc_superobs',swp_filename_base,iost)

  do i=1,n_all_bins
    if ( so_vr(i,8) .ne. vr_missing ) then
      write(swpso_unit,'(A2,1X,A14,1X,8(f12.3))',iostat=iost) tailno,swptime_str,so_vr(i,2:9)
      call check_iostat_write('calc_superobs',swpso_filename,iost)
    endif
  enddo
  close(swpso_unit)
endif ! if (debug_level .ge. 1)

! Append superobs to master file for all sweeps
inquire(file=superob_filename, exist=fileexists)
if (fileexists) then
  open(unit=so_unit, file=superob_filename, status='old', action='write', position='append', iostat=iost)
  call check_iostat_open('calc_superobs',superob_filename,iost)
  do i=1,n_all_bins
    if ( so_vr(i,8) .ne. vr_missing ) then
      write(so_unit,'(A2,1X,A14,1X,8(f12.3))',iostat=iost) tailno,swptime_str,so_vr(i,2:9)
      call check_iostat_write('calc_superobs',superob_filename,iost)
    endif
  enddo
  close(so_unit)
else
  open(unit=so_unit, file=superob_filename, status='new', action='write', iostat=iost)
  call check_iostat_open('calc_superobs',superob_filename,iost)
  do i=1,n_all_bins
    if ( so_vr(i,8) .ne. vr_missing ) then
      write(unit=so_unit, fmt='(A2,1X,A14,1X,8(f12.3))', iostat=iost) tailno,swptime_str,so_vr(i,2:9)
      call check_iostat_write('calc_superobs',superob_filename,iost)
    endif
  enddo
  close(so_unit)
endif

if (debug_level .eq. 1) write(6,*) '(End calc_superobs)'

END SUBROUTINE calc_superobs


END MODULE TDR_SUPEROB
