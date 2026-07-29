MODULE NEXRAD_BUFR
    USE NEXRAD_COMMON
    USE module_nexrad_io, ONLY: write_superobs_netcdf
    IMPLICIT NONE

    LOGICAL, PUBLIC :: radar_box, radar_sites
    REAL(8), PUBLIC :: radar_rmesh, radar_zmesh, refl_lowbnd_rw
 

CONTAINS

    SUBROUTINE get_bufr_tbl(bfrin_filename, bfrtbl_filename)
        CHARACTER(120), INTENT(IN) :: bfrin_filename
        CHARACTER(len=*), INTENT(IN) :: bfrtbl_filename
        INTEGER :: bfrin_unit, bfrtbl_unit, iost

        bfrin_unit  = 11
        bfrtbl_unit = 13

        OPEN(UNIT=bfrin_unit, FILE=TRIM(bfrin_filename), FORM='unformatted', STATUS='old', IOSTAT=iost)
        CALL check_iostat_open('get_bufr_tbl', bfrin_filename, iost)
        CALL openbf(bfrin_unit, 'IN', bfrin_unit)

        OPEN(UNIT=bfrtbl_unit, FILE=bfrtbl_filename, STATUS='replace', IOSTAT=iost)
        CALL check_iostat_open('get_bufr_tbl', bfrtbl_filename, iost)

        CALL dxdump(bfrin_unit, bfrtbl_unit)

        CALL closbf(bfrin_unit)
        CLOSE(bfrtbl_unit)
        CLOSE(bfrin_unit)
    END SUBROUTINE get_bufr_tbl

    !==================================================================
    ! Scan the input BUFR file ONCE to extract every unique
    ! radar station ID and its lat/lon
    !==================================================================
    SUBROUTINE get_radar_sites_from_bufr(bfrin_filename, max_sites, &
                                          site_ids, site_lat, site_lon, site_hgt, num_sites)
        CHARACTER(120), INTENT(IN) :: bfrin_filename
        INTEGER, INTENT(IN)        :: max_sites
        CHARACTER(len=4), DIMENSION(max_sites), INTENT(OUT) :: site_ids
        REAL(8), DIMENSION(max_sites), INTENT(OUT) :: site_lat, site_lon, site_hgt
        INTEGER, INTENT(OUT) :: num_sites

        INTEGER :: lnbufr, iost, levs, idate, i
        CHARACTER(80) :: hdrstr
        CHARACTER(8)  :: subset, cstaid
        CHARACTER(4)  :: stn_id, last_stn_id
        DOUBLE PRECISION :: hdr(5)
        LOGICAL :: already_found
        INTEGER :: ireadmg, ireadsb

        lnbufr  = 10
        hdrstr  = 'SSTN CLAT CLON HSMSL HSALG'
        num_sites = 0
        last_stn_id = 'NONE'

        OPEN(lnbufr, FILE=TRIM(bfrin_filename), FORM='unformatted', IOSTAT=iost)
        CALL check_iostat_open('get_radar_sites_from_bufr', bfrin_filename, iost)
        CALL openbf(lnbufr, 'IN', lnbufr)
        CALL datelen(10)

        msg_scan: DO WHILE (ireadmg(lnbufr, subset, idate) == 0)
            IF ( subset(1:7) /= 'NC00601' .AND. subset(1:7) /= 'NC00602' .AND. subset(1:7) /= 'NC00603') CYCLE

            sb_scan: DO WHILE (ireadsb(lnbufr) == 0)
                CALL ufbint(lnbufr, hdr, 5, 1, levs, hdrstr)
                cstaid = TRANSFER(hdr(1), cstaid)
                stn_id = cstaid(1:4)
                
                if (stn_id == last_stn_id) CYCLE sb_scan

                already_found = .FALSE.
                DO i = 1, num_sites
                    IF (site_ids(i) == stn_id) THEN
                        already_found = .TRUE.
                        last_stn_id   = stn_id
                        EXIT
                    END IF
                END DO

                IF (.NOT. already_found) THEN
                   IF (num_sites < max_sites) THEN
                       num_sites = num_sites + 1
                       site_ids(num_sites) = stn_id
                       site_lat(num_sites) = hdr(2)   
                       site_lon(num_sites) = hdr(3)   
                       site_hgt(num_sites) = hdr(4)+hdr(5)
                       
                       last_stn_id = stn_id
                    ELSE
                       PRINT *, "WARNING: max_sites limit reached in get_radar_sites_from_bufr!"
                    END IF
                END IF
            END DO sb_scan
        END DO msg_scan
        CALL closbf(lnbufr); CLOSE(lnbufr)
    END SUBROUTINE get_radar_sites_from_bufr

    !==================================================================
    ! GSI-like Superob Generation Subroutine 
    !==================================================================
    SUBROUTINE radar_bufr_read_all(bfrin_filename, bfrso_filename, ncso_filename, active_stations, num_active, &
                                   ref_year, ref_month, ref_day, ref_hour, &
                                   del_azimuth, del_elev, del_range, del_time, &
                                   elev_angle_max, range_max, minnum, &
                                   active_lats, active_lons, active_hgts)
        IMPLICIT NONE
        ! ----- Arguments ---------------------------------------------
        CHARACTER(len=*), INTENT(IN) :: bfrin_filename, bfrso_filename, ncso_filename
        INTEGER, INTENT(IN) :: num_active, minnum
        INTEGER, INTENT(IN) :: ref_year, ref_month, ref_day, ref_hour
        CHARACTER(len=4), DIMENSION(num_active), INTENT(IN) :: active_stations
        REAL(8), DIMENSION(num_active), INTENT(IN) :: active_lats, active_lons, active_hgts
        REAL(8), INTENT(IN) :: del_azimuth, del_elev, del_range, del_time, elev_angle_max, range_max

        ! ---- BUFR handles / loop variables -------------------------
        INTEGER :: inbufr, outbufr, iost, ireadmg, ireadsb
        CHARACTER(8) :: subset
        INTEGER :: idate, levs, n_gates, i, krad, iloc
        CHARACTER(4) :: stn_id, adj_stn_id
        DOUBLE PRECISION :: hdr(10), rwnd(3, 4000)
        DOUBLE PRECISION :: rdisttest(4000) 
       
        !---- Superob bin dimensions -------------------------------- 
        INTEGER :: nazbin, nrbin, nelbin, nthisrad, nthisbins
        INTEGER :: iazbin, irbin, ielbin
        REAL(8) :: rdelaz, rdelr, rdelel
        
        REAL(8), ALLOCATABLE :: bins(:,:,:)
        INTEGER, ALLOCATABLE :: ibins(:,:)
      
        ! ---- Time variables ---------------------------------------- 
        INTEGER :: obs_y, obs_m, obs_d, obs_h, obs_min, obs_sec
        REAL(8) :: diff_hrs
 
        ! ---- Per-observation working variables ---------------------
        REAL(8) :: stn_az, stn_el, range, thisvr
        REAL(8) :: thiscount, thisrange, thisazimuth, thistilt
        REAL(8) :: thisvr_avg, thisvr2, thiserr

        ! ---- 4/3 earth-curvature beam geometry ---------------------
        REAL(8) :: aactual, a43, b, c, ha, epsh, h, thishgt
        REAL(8) :: celev0, selev0, celev, selev
        REAL(8) :: thistiltr, gamma
        REAL(8) :: corrected_tilt
        REAL(8), PARAMETER :: four_thirds = 4.0_8 / 3.0_8
        REAL(8), PARAMETER :: r8          = 8.0_8

        ! ---- radar box variables ------------------------------------
        INTEGER :: mlat, mlonx, nlevz
        INTEGER :: ilev, ilat, ilon
        REAL(8) :: rkm2dg, twopi, dx, dy, dlat_grid, dlon_grid
        REAL(8) :: delat, halfpi
        REAL(8) :: rlat_min, rlon_min

        ! ---- Spherical projection -----------------------------------
        REAL(8) :: thisazimuthr, rlonloc, rlatloc
        REAL(8) :: this_stalatr, rlon0, clat0, slat0
        REAL(8) :: rlonglob, rlatglob, clat1
        REAL(8) :: caz0, saz0, cdlon, sdlon, caz1, saz1
        REAL(8) :: corrected_azimuth
        REAL(8) :: thislat, thislon
        REAL(8) :: stn_hgt

        ! ---- Gate distance scale factor  ---------------
        REAL(8) :: ddiffmin, distfact
        INTEGER :: idups  

        ! ----- Per-radar and global diagnostics ----
        ! These track vr range, error range, azimuth correction, tilt
        ! correction, and distance correction for each radar and globally.
        INTEGER :: nsuper, nsuperall
        REAL(8) :: vrmax, vrmin, errmax, errmin
        REAL(8) :: vrmaxall, vrminall, errmaxall, errminall
        REAL(8) :: delazmmax, delazmmaxall
        REAL(8) :: deltiltmax, deltiltmin, deltiltmaxall, deltiltminall
        REAL(8) :: deldistmax, deldistmin, deldistmaxall, deldistminall

        ! ---- Output buffers ----------------------------------------
        INTEGER :: total_superobs, obs_idx
        CHARACTER(4), ALLOCATABLE :: out_stn(:)
        REAL(4), ALLOCATABLE :: out_stlat(:), out_stlon(:), out_elv(:)
        REAL(4), ALLOCATABLE :: out_lat(:), out_lon(:)
        REAL(4), ALLOCATABLE :: out_range(:), out_az(:), out_tilt(:)
        REAL(4), ALLOCATABLE :: out_hgt(:), out_vr(:), out_err(:)
        REAL(4), ALLOCATABLE :: out_toff(:)
        INTEGER(8), ALLOCATABLE :: out_datetime(:)
        INTEGER(8) :: cycle_epoch

        CHARACTER(len=*), PARAMETER :: tbname = 'SUPEROB_RADAR::'

        IF (num_active <= 0) RETURN

        ! ---- Set up superob bin dimensions -------------------------
        nazbin = NINT(360.0_8 / del_azimuth)
        nrbin  = NINT(range_max / del_range)
        nelbin = NINT(elev_angle_max / del_elev)
        rdelaz = 1.0_8 / (360.0_8 / nazbin)
        rdelr  = 1.0_8 / (range_max / nrbin)
        rdelel = 1.0_8 / (elev_angle_max / nelbin)

        IF (radar_box) THEN
            ! --- radar_box grid geometry, exactly as Program 1 ------
            twopi      = 2.0_8 * PI_BUFR
            ! rkm2dg: degrees per km on the equator
            rkm2dg     = 360.0_8 / (twopi * EARTH_RADIUS_M * 1.0E-3_8)
            dlat_grid  = 5.0_8            ! +/-2.5 deg box in lat
            dlon_grid  = 5.0_8            ! +/-2.5 deg box in lon
            halfpi     = 0.5_8 * PI_BUFR
            dx         = radar_rmesh * rkm2dg    ! horizontal spacing in degrees
            dy         = dx
            mlat       = MAX(2, NINT(dlat_grid / dy))
            mlonx      = MAX(2, NINT(dlon_grid / dx))
            delat      = dlat_grid / REAL(mlat, 8)   ! actual lat cell size (deg)
            nlevz      = NINT(15000.0_8 / radar_zmesh)
            nthisrad   = nlevz * mlat * mlonx
            WRITE(6,'(A,2I5,I6,A,F8.4,A,F8.1)') &
                ' radar_box grid: mlat,mlonx,nlevz=', mlat, mlonx, nlevz, &
                '  delat=', delat, '  radar_zmesh=', radar_zmesh
        ELSE
            nthisrad = nrbin * nazbin * nelbin
        END IF
        
        ALLOCATE(bins(6, nthisrad, num_active))
        ALLOCATE(ibins(nthisrad, num_active))
        bins = 0.0_8; ibins = 0

        ! ============================================================
        ! PASS 1: Detect gate distance scale factor (distfact)
        ! ============================================================
        ddiffmin = HUGE(1.0_8)
        idups    = 0

        inbufr = 10
!        OPEN(inbufr, FILE=TRIM(bfrin_filename), FORM='unformatted', IOSTAT=iost)
!        CALL check_iostat_open('radar_bufr_read_all pass1', bfrin_filename, iost)
!        CALL openbf(inbufr, 'IN', inbufr)
!        CALL datelen(10)

!        DO WHILE (ireadmg(inbufr, subset, idate) == 0)
!            DO WHILE (ireadsb(inbufr) == 0)
!                CALL ufbint(inbufr, rdisttest, 1, 4000, n_gates, 'DIST125M')
!                IF (n_gates > 1) THEN
!                    DO i = 1, n_gates - 1
!                        IF (NINT(ABS(rdisttest(i+1) - rdisttest(i))) == 0) THEN
!                            idups = idups + 1
!                        ELSE
!                            ddiffmin = MIN(ABS(rdisttest(i+1) - rdisttest(i)), ddiffmin)
!                        END IF
!                    END DO
!                END IF
!            END DO
!        END DO
!        CALL closbf(inbufr); CLOSE(inbufr)

        ! Determine distfact from minimum gate spacing (FIX 2)
!        distfact = 0.0_8
!        IF (NINT(ddiffmin) == 1) distfact = 250.0_8
!       IF (NINT(ddiffmin) == 2) distfact = 125.0_8
        distfact = 125.0_8
!        IF (distfact == 0.0_8) THEN
!            WRITE(6,*) 'radar_bufr_read_all: WARNING - gate distance scale factor ', &
!                       'undetermined, defaulting to 125'
!            distfact = 125.0_8
!        END IF
        WRITE(6, '(A,F6.1,A,I10)') ' radar_bufr_read_all: distfact=', distfact, &
                                     '  idups=', idups

        ! Pass2: READ AND BIN DATA
        inbufr = 10
        OPEN(inbufr, FILE=TRIM(bfrin_filename), FORM='unformatted', IOSTAT=iost)
        CALL check_iostat_open('radar_bufr_read_all pass2', bfrin_filename, iost)
        CALL openbf(inbufr, 'IN', inbufr)
        CALL datelen(10)
        
        DO WHILE (ireadmg(inbufr, subset, idate) == 0)
            DO WHILE (ireadsb(inbufr) == 0)
                CALL ufbint(inbufr, hdr, 10, 1, levs, 'SSTN YEAR MNTH DAYS HOUR MINU SECO ANAZ ANEL QCRW')
                ! ---------------------------------------------------------
                ! FILTER 1: Elevation Check (Cheapest scalar comparison)
                ! ---------------------------------------------------------
                stn_el = hdr(9)
                IF (stn_el > elev_angle_max) CYCLE

                ! ---------------------------------------------------------
                ! FILTER 2: Station Check (Avoid string adjustment in loop)
                ! ---------------------------------------------------------
                stn_id = TRANSFER(hdr(1), stn_id)
                adj_stn_id = ADJUSTL(stn_id)  ! Adjust only once per observation
            
                krad = 0
                DO i = 1, num_active
                    ! Assumes active_stations is already left-adjusted!
                    IF (adj_stn_id == active_stations(i)) THEN 
                        krad = i
                        EXIT
                    END IF
                END DO
                IF (krad == 0) CYCLE

                ! ---------------------------------------------------------
                ! FILTER 3: Time Check (Most expensive calculation, do last)
                ! ---------------------------------------------------------            
                obs_y   = INT(hdr(2))
                obs_m   = INT(hdr(3))
                obs_d   = INT(hdr(4))
                obs_h   = INT(hdr(5))
                obs_min = INT(hdr(6))
                obs_sec = INT(hdr(7))
                diff_hrs = time_diff_hours(ref_year, ref_month, ref_day, ref_hour, &
                                           obs_y, obs_m, obs_d, obs_h, obs_min, obs_sec)
                IF (ABS(diff_hrs) > del_time) CYCLE

                stn_az = 90.0_8 - hdr(8)

                ! ############ DEBUG ################
                !write(6,*) 'after QC out time, elevation, and station name, currently process radar ', stn_id
                ! ############end of DEBUG ##########
                ! --- Standard-mode bin indices (used when .NOT. radar_box) ---
                IF (.NOT. radar_box) THEN
                    ! iazbin with upper-bound guard
                    iazbin = MOD(INT(stn_az * rdelaz), nazbin) + 1
                    IF (iazbin <= 0) iazbin = iazbin + nazbin
                    IF (iazbin <= 0 .OR. iazbin > nazbin) THEN
                        WRITE(6,*) 'radar_bufr_read_all: ERROR in iazbin, program stops'
                        STOP 99
                    END IF

                    ielbin = CEILING(stn_el * rdelel)
                    IF (ielbin < 1 .OR. ielbin > nelbin) CYCLE
                END IF

                ! Read gates and accumulate into bins
                CALL ufbint(inbufr, rwnd, 3, 4000, n_gates, 'DIST125M DMVR DVSW')

                DO i = 1, n_gates
                    range = distfact * rwnd(1,i) 
                    thisvr = rwnd(2,i)
                    IF (range > range_max) CYCLE
                    IF (thisvr > 1.0E5_8) CYCLE 
                    IF (rwnd(3,i) > 1.e5) CYCLE 
                   
                    if (.not. radar_box) then
                        ! ---- Standard azimuth/range/elevation binning ----
                        irbin = CEILING(range * rdelr)
                        IF (irbin < 1 .OR. irbin > nrbin) CYCLE

                        iloc = nrbin * (nazbin * (ielbin - 1) + (iazbin - 1)) + irbin
                        bins(1, iloc, krad) = bins(1, iloc, krad) + range
                        bins(2, iloc, krad) = bins(2, iloc, krad) + stn_az
                        bins(3, iloc, krad) = bins(3, iloc, krad) + stn_el
                        bins(4, iloc, krad) = bins(4, iloc, krad) + thisvr
                        bins(5, iloc, krad) = bins(5, iloc, krad) + (thisvr**2)
                        bins(6, iloc, krad) = bins(6, iloc, krad) + (diff_hrs * 3600.0_8)
                        ibins(iloc, krad)   = ibins(iloc, krad) + 1
                    else
                        IF (ABS(active_lats(krad)) > 89.5_8) CYCLE 
                        stn_hgt      = active_hgts(krad)
                        this_stalatr = active_lats(krad) * PI_BUFR / 180.0_8
                        rlon0        = active_lons(krad) * PI_BUFR / 180.0_8
                        clat0        = COS(this_stalatr)
                        slat0        = SIN(this_stalatr)
 
                        ! 4/3 earth-curvature beam height
                        aactual   = EARTH_RADIUS_M + stn_hgt
                        a43       = four_thirds * aactual
                        thistiltr = stn_el * PI_BUFR / 180.0_8
                        selev0    = SIN(thistiltr)
                        celev0    = COS(thistiltr)

                        b    = range * (range + 2.0_8 * aactual * selev0)
                        c    = SQRT(aactual**2 + b)
                        ha   = b / (aactual + c)
                        epsh = (range**2 - ha**2) / (r8 * aactual)
                        h    = ha - epsh
                        thishgt = stn_hgt + h        ! MSL height

                        celev = celev0
                        selev = selev0
                        IF (range >= 1.0_8) THEN
                            celev = a43 * celev0 / (a43 + h)
                            selev = (range**2 + h**2 + 2.0_8 * a43 * h) / &
                                    (2.0_8 * range * (a43 + h))
                        END IF
 
                        corrected_tilt=atan2(selev,celev)*180.0_8/PI_BUFR
                        gamma = 0.5_8 * range * (celev0 + celev)

                        ! Spherical projection -> earth lat/lon
                        thisazimuthr = stn_az * PI_BUFR / 180.0_8
                        rlonloc      = (gamma / EARTH_RADIUS_M) * COS(thisazimuthr)
                        rlatloc      = (gamma / EARTH_RADIUS_M) * SIN(thisazimuthr)

                        CALL invtllv(rlonloc, rlatloc, rlon0, clat0, slat0, &
                                     rlonglob, rlatglob)

                        thislat = rlatglob * 180.0_8 / PI_BUFR
                        thislon = rlonglob * 180.0_8 / PI_BUFR

                        IF (ABS(thislat) > 89.5_8) CYCLE

                        ! Grid cell indices within the 5x5 deg box
                        ! rlat_min / rlon_min are the SW corner of the box
                        rlat_min = active_lats(krad) - 2.5_8
                        rlon_min = active_lons(krad) - 2.5_8

                        ilev = CEILING(thishgt / radar_zmesh)
                        ilat = CEILING((thislat - rlat_min) / delat)
                        ilon = CEILING((thislon - rlon_min) / (dlon_grid / REAL(mlonx, 8)))

                        IF (ilev < 1 .OR. ilat < 1 .OR. ilon < 1) CYCLE
                        IF (ilev > nlevz .OR. ilat > mlat .OR. ilon > mlonx) CYCLE

                        ! Column-major iloc matching Program 1:
                        !   iloc = mlat*(mlonx*(ilev-1) + (ilon-1)) + ilat
                        iloc = mlat * (mlonx * (ilev - 1) + (ilon - 1)) + ilat

                        bins(1, iloc, krad) = bins(1, iloc, krad) + range
                        bins(2, iloc, krad) = bins(2, iloc, krad) + stn_az
                        bins(3, iloc, krad) = bins(3, iloc, krad) + stn_el
                        bins(4, iloc, krad) = bins(4, iloc, krad) + thisvr
                        bins(5, iloc, krad) = bins(5, iloc, krad) + thisvr**2
                        bins(6, iloc, krad) = bins(6, iloc, krad) + (diff_hrs * 3600.0_8)
                        ibins(iloc, krad)   = ibins(iloc, krad) + 1

                    end if ! radar_box end    
                END DO     ! gate loop
            END DO         ! subset loop
        END DO             ! message loop
        CALL closbf(inbufr); CLOSE(inbufr)

        ! 2. ALLOCATE BUFFERS FOR OUTPUT
        total_superobs = 0
        DO krad = 1, num_active
            DO iloc = 1, nthisrad
                IF (ibins(iloc, krad) >= minnum) total_superobs = total_superobs + 1
            END DO
        END DO

        ALLOCATE(out_stn(total_superobs), out_stlat(total_superobs), out_stlon(total_superobs), out_elv(total_superobs))
        ALLOCATE(out_lat(total_superobs), out_lon(total_superobs))
        ALLOCATE(out_range(total_superobs), out_az(total_superobs), out_tilt(total_superobs))
        ALLOCATE(out_hgt(total_superobs), out_vr(total_superobs), out_err(total_superobs))
        ALLOCATE(out_toff(total_superobs), out_datetime(total_superobs))

        ! calculate the cycle time in seconds since 1970-01-01 00:00:00z
        cycle_epoch = INT(time_diff_hours(1970, 1, 1, 0,  ref_year, ref_month, &
                                          ref_day, ref_hour, 0, 0) * 3600.0_8, 8)

        ! ============================================================
        ! Create superobs and write out  
        ! ============================================================

        outbufr = 12
        OPEN(outbufr, FILE=TRIM(bfrso_filename), FORM='unformatted', IOSTAT=iost)
        
        obs_idx = 0
        nsuperall    = 0
        vrmaxall     = -HUGE(1.0_8);  vrminall    =  HUGE(1.0_8)
        errmaxall    = -HUGE(1.0_8);  errminall   =  HUGE(1.0_8)
        delazmmaxall = -HUGE(1.0_8)
        deltiltmaxall= -HUGE(1.0_8);  deltiltminall=  HUGE(1.0_8)
        deldistmaxall= -HUGE(1.0_8);  deldistminall=  HUGE(1.0_8)

        DO krad = 1, num_active
            ! Precompute variables for this specific radar site
            this_stalatr = active_lats(krad) * PI_BUFR / 180.0_8
            rlon0        = active_lons(krad) * PI_BUFR / 180.0_8
            clat0        = COS(this_stalatr)
            slat0        = SIN(this_stalatr)
            stn_hgt      = active_hgts(krad) 

            ! Skip radars too close to poles (|lat| > 89.5)
            IF (ABS(active_lats(krad)) > 89.5_8) CYCLE

            ! Reset per-radar diagnostics 
            nsuper     = 0
            vrmax      = -HUGE(1.0_8);  vrmin     =  HUGE(1.0_8)
            errmax     = -HUGE(1.0_8);  errmin    =  HUGE(1.0_8)
            delazmmax  = -HUGE(1.0_8)
            deltiltmax = -HUGE(1.0_8);  deltiltmin=  HUGE(1.0_8)
            deldistmax = -HUGE(1.0_8);  deldistmin=  HUGE(1.0_8)

            DO iloc = 1, nthisrad
                IF (ibins(iloc, krad) < minnum) CYCLE

                thiscount = 1.0_8 / REAL(ibins(iloc, krad), 8)
                
                thisrange   = bins(1, iloc, krad) * thiscount
                thisazimuth = bins(2, iloc, krad) * thiscount
                thistilt    = bins(3, iloc, krad) * thiscount
                thisvr_avg  = bins(4, iloc, krad) * thiscount
                thisvr2     = bins(5, iloc, krad) * thiscount
                thiserr     = SQRT(ABS(thisvr2 - (thisvr_avg**2)))

                ! Per-radar vr / err diagnostics 
                vrmax  = MAX(vrmax,  thisvr_avg)
                vrmin  = MIN(vrmin,  thisvr_avg)
                errmax = MAX(errmax, thiserr)
                errmin = MIN(errmin, thiserr)

                ! 4/3 Earth rule beam height calculation
                aactual = EARTH_RADIUS_M + stn_hgt
                a43 = four_thirds * aactual
                
                thistiltr = thistilt * PI_BUFR / 180.0_8
                celev0 = COS(thistiltr)
                selev0 = SIN(thistiltr)
                    
                b = thisrange * (thisrange + 2.0_8 * aactual * selev0)
                c = SQRT(aactual**2 + b)
                ha = b / (aactual + c)
                epsh = (thisrange**2 - ha**2) / (8.0_8 * aactual)
                h = ha - epsh

                thishgt = stn_hgt + h
                    
                ! True Geographic Footprint Projection
                celev = celev0
                selev = selev0
                IF (thisrange >= 1.0_8) THEN
                    celev = a43 * celev0 / (a43 + h)
                    selev = (thisrange**2 + h**2 + 2.0_8 * a43 * h) / (2.0_8 * thisrange * (a43 + h))
                END IF
                
                corrected_tilt = ATAN2(selev, celev) * 180.0_8 / PI_BUFR
                ! Per-radar tilt-correction diagnostics 
                deltiltmax = MAX(corrected_tilt - thistilt, deltiltmax)
                deltiltmin = MIN(corrected_tilt - thistilt, deltiltmin)

                ! arc-corrected along-surface range
                gamma = 0.5_8 * thisrange * (celev0 + celev)

                ! Per-radar distance-correction diagnostics 
                deldistmax = MAX(gamma - thisrange, deldistmax)
                deldistmin = MIN(gamma - thisrange, deldistmin)

                ! --------------------------------------------------
                ! Spherical projection -> earth lat/lon
                ! --------------------------------------------------
                thisazimuthr = thisazimuth * PI_BUFR / 180.0_8
                rlonloc      = (gamma / EARTH_RADIUS_M) * COS(thisazimuthr)
                rlatloc      = (gamma / EARTH_RADIUS_M) * SIN(thisazimuthr)
 
                CALL invtllv(rlonloc, rlatloc, rlon0, clat0, slat0, rlonglob, rlatglob)
                    
                thislat = rlatglob * 180.0_8 / PI_BUFR
                thislon = rlonglob * 180.0_8 / PI_BUFR
                
                ! Skip near-polar superobs
                IF (ABS(thislat) > 89.5_8) CYCLE

                ! geodetically corrected azimuth
                clat1 = COS(rlatglob)
                caz0  = COS(thisazimuthr)
                saz0  = SIN(thisazimuthr)
                cdlon = COS(rlonglob - rlon0)
                sdlon = SIN(rlonglob - rlon0)
                caz1  = clat0 * caz0 / clat1
                saz1  = saz0 * cdlon - caz0 * sdlon * slat0
                corrected_azimuth = ATAN2(saz1, caz1) * 180.0_8 / PI_BUFR

                ! Per-radar azimuth-correction diagnostic :
                ! minimum absolute wrap-around difference across +-720 deg
                delazmmax = MAX(                                          &
                    MIN(ABS(corrected_azimuth - thisazimuth - 720.0_8),  &
                        ABS(corrected_azimuth - thisazimuth - 360.0_8),  &
                        ABS(corrected_azimuth - thisazimuth        ),     &
                        ABS(corrected_azimuth - thisazimuth + 360.0_8),  &
                        ABS(corrected_azimuth - thisazimuth + 720.0_8)), &
                    delazmmax)   
                ! --------------------------------------------------
                ! Fill output arrays
                ! --------------------------------------------------
                obs_idx = obs_idx + 1
                out_stn(obs_idx)   = active_stations(krad)
                out_stlat(obs_idx) = active_lats(krad)
                out_stlon(obs_idx) = active_lons(krad)
                out_lat(obs_idx)   = REAL(thislat, 4)
                out_lon(obs_idx)   = REAL(thislon, 4)
                out_elv(obs_idx)   = REAL(stn_hgt, 4)
                out_range(obs_idx) = REAL(gamma, 4)             ! gamma not thisrange
                out_az(obs_idx)    = REAL(corrected_azimuth, 4) ! corrected azimuth
                out_tilt(obs_idx)  = REAL(corrected_tilt, 4)    ! corrected tilt
                out_hgt(obs_idx)   = REAL(thishgt, 4)           ! MSL height
                out_vr(obs_idx)    = REAL(thisvr_avg, 4)
                out_err(obs_idx)   = REAL(thiserr, 4)
                out_toff(obs_idx)  = REAL(bins(6, iloc, krad) * thiscount, 4)
                out_datetime(obs_idx) = cycle_epoch + INT(out_toff(obs_idx), 8)

                ! Write binary superob record:  ------DISABLED because netcdf is written
                !   stn_id, stn_lat, stn_lon, stn_hgt,
                !   thistime, thislat, thislon, thishgt,
                !   thisvr, corrected_azimuth, thiserr, corrected_tilt, gamma
                !WRITE(outbufr) active_stations(krad),             &
                !               active_lats(krad),                 &
                !               active_lons(krad),                 &
                !               stn_hgt,                           &
                !               bins(6, iloc, krad) * thiscount,   &
                !               thislat,                           &
                !               thislon,                           &
                !               thishgt,                           &    
                !               thisvr_avg,                        &
                !               corrected_azimuth,                 &    
                !               thiserr,                           &
                !               corrected_tilt,                    &   
                !               gamma          

                nsuper = nsuper + 1                          
            END DO ! iloc
            IF (nsuper > 0) THEN
                WRITE(6,*) ' for radar ', active_stations(krad), &
                           ' nsuper=', nsuper, ' delazmmax=', delazmmax
                WRITE(6,*) ' vrmin,max=',   vrmin,  vrmax,  &
                           ' errmin,max=',  errmin, errmax
                WRITE(6,*) ' deltiltmin,max=',  deltiltmin,  deltiltmax, &
                           ' deldistmin,max=',  deldistmin,  deldistmax

                ! Accumulate into global diagnostics (Program 1)
                vrminall     = MIN(vrminall,      vrmin)
                vrmaxall     = MAX(vrmaxall,      vrmax)
                errminall    = MIN(errminall,     errmin)
                errmaxall    = MAX(errmaxall,     errmax)
                delazmmaxall = MAX(delazmmaxall,  delazmmax)
                deltiltmaxall= MAX(deltiltmaxall, deltiltmax)
                deldistmaxall= MAX(deldistmaxall, deldistmax)
                deltiltminall= MIN(deltiltminall, deltiltmin)
                deldistminall= MIN(deldistminall, deldistmin)
                nsuperall    = nsuperall + nsuper
            END IF
        END DO     ! krad
        CLOSE(outbufr)
        ! Global summary 
        WRITE(6,*) ' total number of superobs written=', nsuperall
        WRITE(6,*) ' vrmin,maxall=',     vrminall,    vrmaxall
        WRITE(6,*) ' errmin,maxall=',    errminall,   errmaxall
        WRITE(6,*) ' delazmmaxall=',     delazmmaxall
        WRITE(6,*) ' deltiltmin,maxall=',deltiltminall,deltiltmaxall
        WRITE(6,*) ' deldistmin,maxall=',deldistminall,deldistmaxall

        ! 4. WRITE TO NETCDF
        IF (nsuperall > 0) THEN
            CALL write_superobs_netcdf(ncso_filename, nsuperall, out_stn, &
                                       out_stlat, out_stlon, out_elv, &
                                       out_lat, out_lon, &
                                       out_range, out_az, out_tilt, out_hgt, &
                                       out_vr, out_err, &
                                       out_toff, out_datetime)
        END IF
        WRITE(6,*) ' total number of superobs written=', obs_idx

        ! Clean up
        DEALLOCATE(bins, ibins)
        DEALLOCATE(out_stn, out_stlat, out_stlon, out_elv)
        DEALLOCATE(out_lat, out_lon, out_range, out_az, out_tilt, out_hgt, out_vr, out_err)
        DEALLOCATE(out_toff, out_datetime)
    END SUBROUTINE radar_bufr_read_all

    !==================================================================
    ! Spherical Projection Helper for True Geolocation
    !==================================================================
    !==================================================================
    ! Helper: Compute Absolute Difference in Hours using Julian Days
    !==================================================================
END MODULE NEXRAD_BUFR
