MODULE NEXRAD_COMMON
    IMPLICIT NONE

    REAL(8), PARAMETER :: EARTH_RADIUS_KM = 6371.0_8
    REAL(8), PARAMETER :: EARTH_RADIUS_M  = 6371000.0_8
    REAL(8), PARAMETER :: PI_BUFR = 3.14159265358979323846_8

CONTAINS

    SUBROUTINE check_iostat_open(subname, filename, iostat)
        CHARACTER(*), INTENT(IN) :: subname, filename
        INTEGER, INTENT(IN)      :: iostat
        IF (iostat .NE. 0) THEN
            WRITE(6,*) '-- ERROR (',TRIM(subname),'): Could not open file ',TRIM(filename),', iostat=',iostat
            STOP 1
        END IF
    END SUBROUTINE check_iostat_open

    SUBROUTINE check_iostat_read(subname, filename, iostat, eoferror)
        CHARACTER(*), INTENT(IN)  :: subname, filename
        INTEGER, INTENT(IN)       :: iostat
        LOGICAL, INTENT(IN)       :: eoferror
        IF (iostat .NE. 0) THEN
            IF (iostat .LT. 0) THEN
                IF (eoferror) THEN
                    WRITE(6,*) '-- ERROR (',TRIM(subname),'): Unexpected EOF reached reading file ',TRIM(filename)
                    STOP 1
                ELSE
                    WRITE(6,*) '-- Info (',TRIM(subname),'): Reached EOF reading file ',TRIM(filename)
                END IF
            ELSE
                WRITE(6,*) '-- ERROR (',TRIM(subname),'): Could not read file ',TRIM(filename),', iostat=',iostat
                STOP 1
            END IF
        END IF
    END SUBROUTINE check_iostat_read

    SUBROUTINE check_file_missing(subname, filename)
        CHARACTER(*), INTENT(IN) :: subname, filename
        LOGICAL                  :: fileexists
        INQUIRE(FILE=filename, EXIST=fileexists)
        IF (.NOT. fileexists) THEN
            WRITE(6,*) '-- ERROR (',TRIM(subname),'): Could not find ',TRIM(filename),'. Stopping...'
            STOP 1
        END IF
    END SUBROUTINE check_file_missing

    SUBROUTINE check_delete_existing(subname, filename)
        CHARACTER(*), INTENT(IN) :: subname, filename
        INTEGER                  :: lu, iost
        LOGICAL                  :: fileexists
        lu = 99
        INQUIRE(FILE=filename, EXIST=fileexists)
        IF (fileexists) THEN
            OPEN(UNIT=lu, FILE=filename, STATUS='old', IOSTAT=iost)
            IF (iost .EQ. 0) CLOSE(UNIT=lu, STATUS='delete')
        END IF
    END SUBROUTINE check_delete_existing

    !==================================================================
    ! Domain & Range check
    !==================================================================
    SUBROUTINE check_radar_in_range(site_lat, site_lon, geolon, geolat, &
                                     nx, ny, range_km, inside, min_dist)
        IMPLICIT NONE
        REAL(8), INTENT(IN)  :: site_lat, site_lon, range_km
        REAL(4), INTENT(IN)  :: geolon(nx,ny), geolat(nx,ny)
        INTEGER, INTENT(IN)  :: nx, ny
        LOGICAL, INTENT(OUT) :: inside
        REAL(8), INTENT(OUT) :: min_dist

        INTEGER :: ii, jj
        REAL(8) :: glon, glat, d
        INTEGER, PARAMETER :: stride = 4

        min_dist = HUGE(1.0_8)

        DO jj = 1, ny, stride
            DO ii = 1, nx, stride
                glon = REAL(geolon(ii,jj), 8)
                glat = REAL(geolat(ii,jj), 8)
                d = haversine_km(site_lat, site_lon, glat, glon)
                IF (d < min_dist) min_dist = d
                IF (min_dist <= range_km) THEN
                    inside = .TRUE.
                    RETURN
                END IF
            END DO
        END DO
        inside = (min_dist <= range_km)
    END SUBROUTINE check_radar_in_range

    !==================================================================
    ! Haversine great-circle distance
    !==================================================================
    FUNCTION haversine_km(lat1, lon1, lat2, lon2) RESULT(dist)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: lat1, lon1, lat2, lon2
        REAL(8) :: dist, phi1, phi2, dphi, dlambda, a, c, lon1_adj, lon2_adj

        lon1_adj = lon1
        lon2_adj = lon2
        IF (lon1_adj > 180.0_8) lon1_adj = lon1_adj - 360.0_8
        IF (lon2_adj > 180.0_8) lon2_adj = lon2_adj - 360.0_8

        phi1    = lat1 * PI_BUFR / 180.0_8
        phi2    = lat2 * PI_BUFR / 180.0_8
        dphi    = (lat2 - lat1) * PI_BUFR / 180.0_8
        dlambda = (lon2_adj - lon1_adj) * PI_BUFR / 180.0_8

        a = SIN(dphi/2.0_8)**2 + COS(phi1) * COS(phi2) * SIN(dlambda/2.0_8)**2
        c = 2.0_8 * ATAN2(SQRT(a), SQRT(1.0_8 - a))
        dist = EARTH_RADIUS_KM * c
    END FUNCTION haversine_km

    !==================================================================
    ! Spherical Projection Helper for True Geolocation
    !==================================================================
    SUBROUTINE invtllv(alm, aph, tlmo, ctph0, stph0, tlm, tph)
        REAL(8), INTENT(IN)  :: alm, aph, tlmo, ctph0, stph0
        REAL(8), INTENT(OUT) :: tlm, tph
        REAL(8) :: relm, srlm, crlm, sph, cph, cc, anum, denom

        relm = alm
        srlm = SIN(relm)
        crlm = COS(relm)
        sph  = SIN(aph)
        cph  = COS(aph)

        cc    = cph * crlm
        anum  = cph * srlm
        denom = ctph0 * cc - stph0 * sph

        tlm = tlmo + ATAN2(anum, denom)
        tph = ASIN(ctph0 * sph + stph0 * cc)
    END SUBROUTINE invtllv

    !==================================================================
    ! Compute Absolute Difference in Hours using Julian Days
    !==================================================================
    FUNCTION time_diff_hours(y1, m1, d1, h1, y2, m2, d2, h2, mn2, s2) RESULT(diff)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: y1, m1, d1, h1
        INTEGER, INTENT(IN) :: y2, m2, d2, h2, mn2, s2
        REAL(8) :: diff
        INTEGER :: jd1, jd2, a1, y_1, m_1, a2, y_2, m_2

        ! Julian day for ref time
        a1 = (14 - m1) / 12
        y_1 = y1 + 4800 - a1
        m_1 = m1 + 12 * a1 - 3
        jd1 = d1 + (153 * m_1 + 2) / 5 + 365 * y_1 + y_1 / 4 - y_1 / 100 + y_1 / 400 - 32045

        ! Julian day for obs time
        a2 = (14 - m2) / 12
        y_2 = y2 + 4800 - a2
        m_2 = m2 + 12 * a2 - 3
        jd2 = d2 + (153 * m_2 + 2) / 5 + 365 * y_2 + y_2 / 4 - y_2 / 100 + y_2 / 400 - 32045

        ! Diff in hours = (jd2 - jd1)*24 + diff in hours/mins/secs
        diff = REAL(jd2 - jd1, 8) * 24.0_8 + &
               REAL(h2 - h1, 8) + &
               (REAL(mn2, 8) / 60.0_8) + &
               (REAL(s2, 8) / 3600.0_8)
    END FUNCTION time_diff_hours
END MODULE NEXRAD_COMMON
