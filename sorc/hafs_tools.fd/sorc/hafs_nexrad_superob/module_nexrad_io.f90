!==================================================================
! File: module_nexrad_io.f90
!==================================================================
MODULE module_nexrad_io
    USE netcdf          ! Required for nf90_ functions and constants
    USE ISO_C_BINDING   ! Required for C interoperability
    IMPLICIT NONE

CONTAINS

    !==================================================================
    ! Write Superobs to NetCDF-4 Format (Using C-Binding for Strings)
    !==================================================================
    SUBROUTINE write_superobs_netcdf(nc_filename, num_obs, stn_ids, stn_lats, stn_lons, &
                                     stn_elvs, lats, lons, &
                                     ranges, azimuths, tilts, heights, vrs, errs, &
                                     toff, datetime)
        
        CHARACTER(len=*), INTENT(IN) :: nc_filename
        INTEGER, INTENT(IN) :: num_obs
        CHARACTER(4), DIMENSION(num_obs), INTENT(IN) :: stn_ids
        REAL(4), DIMENSION(num_obs), INTENT(IN) :: stn_lats, stn_lons, stn_elvs
        REAL(4), DIMENSION(num_obs), INTENT(IN) :: lats, lons, ranges, azimuths
        REAL(4), DIMENSION(num_obs), INTENT(IN) :: tilts, heights, vrs, errs
        REAL(4), DIMENSION(num_obs), INTENT(IN) :: toff
        INTEGER(8), DIMENSION(num_obs), INTENT(IN) :: datetime

        INTEGER :: ncid, loc_dimid, str_dimid, status
        INTEGER :: grp_meta, grp_val, grp_err, grp_type
        
        INTEGER :: var_loc, v_dt, v_toff, v_lat, v_lon, v_stn, v_stlat, v_stlon, v_stel, v_raz, v_rtilt
        INTEGER :: v_grng, v_cos_cos, v_sin_cos, v_sint, v_hgt
        INTEGER :: v_vr_val, v_vr_err, v_vr_typ

        REAL(4), ALLOCATABLE :: cos_cos_arr(:), sin_cos_arr(:), sin_t_arr(:)
        INTEGER(4), ALLOCATABLE :: loc_idx(:), typ_arr(:)
        
        INTEGER :: i
        REAL(4) :: az_rad, t_rad
        REAL(4), PARAMETER :: D2R = 3.14159265 / 180.0

        ALLOCATE(cos_cos_arr(num_obs), sin_cos_arr(num_obs), sin_t_arr(num_obs))
        ALLOCATE(loc_idx(num_obs), typ_arr(num_obs))
        

        DO i = 1, num_obs
            loc_idx(i) = i
            typ_arr(i) = 999        
            
            az_rad = azimuths(i) * D2R
            t_rad  = tilts(i) * D2R
            cos_cos_arr(i) = COS(t_rad) * COS(az_rad)
            sin_cos_arr(i) = COS(t_rad) * SIN(az_rad)
            sin_t_arr(i)   = SIN(t_rad)

        END DO

        status = nf90_create(TRIM(nc_filename), IOR(NF90_CLOBBER, NF90_NETCDF4), ncid)

        status = nf90_def_dim(ncid, "Location", num_obs, loc_dimid)
        status = nf90_def_dim(ncid, "stationIdlength", 4, str_dimid)
        status = nf90_def_var(ncid, "Location", NF90_INT, loc_dimid, var_loc)
        status = nf90_put_att(ncid, var_loc, "_FillValue", 2147483647)

        status = nf90_def_grp(ncid, "MetaData", grp_meta)
        status = nf90_def_grp(ncid, "ObsValue", grp_val)
        status = nf90_def_grp(ncid, "ObsError", grp_err)
        status = nf90_def_grp(ncid, "ObsType", grp_type)

        status = nf90_def_var(grp_meta, "dateTime", NF90_INT64, loc_dimid, v_dt)
        status = nf90_put_att(grp_meta, v_dt, "_FillValue", 9223372036854775807_8)
        status = nf90_put_att(grp_meta, v_dt, "long_name", "Datetime")
        status = nf90_put_att(grp_meta, v_dt, "units", "seconds since 1970-01-01T00:00:00Z")

        status = nf90_def_var(grp_meta, "timeOffset", NF90_FLOAT, loc_dimid, v_toff)
        status = nf90_put_att(grp_meta, v_toff, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_toff, "long_name", "Observation Time Minus Reference Time")
        status = nf90_put_att(grp_meta, v_toff, "units", "s")

        status = nf90_def_var(grp_meta, "latitude", NF90_FLOAT, loc_dimid, v_lat)
        status = nf90_put_att(grp_meta, v_lat, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_lat, "long_name", "Latitude")
        status = nf90_put_att(grp_meta, v_lat, "units", "degree_north")
        status = nf90_put_att(grp_meta, v_lat, "valid_range", (/ -90.0, 90.0 /))

        status = nf90_def_var(grp_meta, "longitude", NF90_FLOAT, loc_dimid, v_lon)
        status = nf90_put_att(grp_meta, v_lon, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_lon, "long_name", "Longitude")
        status = nf90_put_att(grp_meta, v_lon, "units", "degree_east")

        status = nf90_def_var(grp_meta, "stationIdentification", NF90_CHAR, [str_dimid, loc_dimid], v_stn)
        status = nf90_put_att(grp_meta, v_stn, "long_name", "Station Identification")

        status = nf90_def_var(grp_meta, "stationLatitude", NF90_FLOAT, loc_dimid, v_stlat)
        status = nf90_put_att(grp_meta, v_stlat, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_stlat, "long_name", "Radar site latitude")
        status = nf90_put_att(grp_meta, v_stlat, "units", "m")

        status = nf90_def_var(grp_meta, "stationLongitude", NF90_FLOAT, loc_dimid, v_stlon)
        status = nf90_put_att(grp_meta, v_stlon, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_stlon, "long_name", "Radar site longitude")
        status = nf90_put_att(grp_meta, v_stlon, "units", "m")

        status = nf90_def_var(grp_meta, "stationElevation", NF90_FLOAT, loc_dimid, v_stel)
        status = nf90_put_att(grp_meta, v_stel, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_stel, "long_name", "Height Of Station Ground Above MSL")
        status = nf90_put_att(grp_meta, v_stel, "units", "m")

        status = nf90_def_var(grp_meta, "radar_azimuth", NF90_FLOAT, loc_dimid, v_raz)
        status = nf90_put_att(grp_meta, v_raz, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_raz, "long_name", "Antenna Azimuth Angle")
        status = nf90_put_att(grp_meta, v_raz, "units", "degree")

        status = nf90_def_var(grp_meta, "radar_tilt", NF90_FLOAT, loc_dimid, v_rtilt)
        status = nf90_put_att(grp_meta, v_rtilt, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_rtilt, "long_name", "Antenna Elevation Angle")
        status = nf90_put_att(grp_meta, v_rtilt, "units", "degree")

        status = nf90_def_var(grp_meta, "gateRange", NF90_FLOAT, loc_dimid, v_grng)
        status = nf90_put_att(grp_meta, v_grng, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_grng, "long_name", "Distance From Antenna")
        status = nf90_put_att(grp_meta, v_grng, "units", "m")

        status = nf90_def_var(grp_meta, "cosAzimuthCosTilt", NF90_FLOAT, loc_dimid, v_cos_cos)
        status = nf90_put_att(grp_meta, v_cos_cos, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_cos_cos, "long_name", "cos tilt x cos azimuth")

        status = nf90_def_var(grp_meta, "sinAzimuthCosTilt", NF90_FLOAT, loc_dimid, v_sin_cos)
        status = nf90_put_att(grp_meta, v_sin_cos, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_sin_cos, "long_name", "sin tilt x cos azimuth")

        status = nf90_def_var(grp_meta, "sinTilt", NF90_FLOAT, loc_dimid, v_sint)
        status = nf90_put_att(grp_meta, v_sint, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_sint, "long_name", "sin tilt")

        status = nf90_def_var(grp_meta, "height", NF90_FLOAT, loc_dimid, v_hgt)
        status = nf90_put_att(grp_meta, v_hgt, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_meta, v_hgt, "long_name", "observation height")
        status = nf90_put_att(grp_meta, v_hgt, "units", "m")

        status = nf90_def_var(grp_val, "radialVelocity", NF90_FLOAT, loc_dimid, v_vr_val)
        status = nf90_put_att(grp_val, v_vr_val, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_val, v_vr_val, "long_name", "Doppler Mean Radial Velocity")
        status = nf90_put_att(grp_val, v_vr_val, "units", "m s-1")

        status = nf90_def_var(grp_err, "radialVelocity", NF90_FLOAT, loc_dimid, v_vr_err)
        status = nf90_put_att(grp_err, v_vr_err, "_FillValue", 3.402823e+38)
        status = nf90_put_att(grp_err, v_vr_err, "long_name", "Observation error for radialVelocity")
        status = nf90_put_att(grp_err, v_vr_err, "units", "m s-1")

        status = nf90_def_var(grp_type, "radialVelocity", NF90_INT, loc_dimid, v_vr_typ)
        status = nf90_put_att(grp_type, v_vr_typ, "_FillValue", 2147483647)
        status = nf90_put_att(grp_type, v_vr_typ, "long_name", "Observation Type for radialVelocity")
        status = nf90_put_att(grp_type, v_vr_typ, "units", "1")

        status = nf90_enddef(ncid)

        status = nf90_put_var(ncid, var_loc, loc_idx)
        
        status = nf90_put_var(grp_meta, v_dt, datetime)
        status = nf90_put_var(grp_meta, v_toff, toff)
        status = nf90_put_var(grp_meta, v_lat, lats)
        status = nf90_put_var(grp_meta, v_lon, lons)
        status = nf90_put_var(grp_meta, v_stn, stn_ids)        
        status = nf90_put_var(grp_meta, v_stlat, stn_lats) 
        status = nf90_put_var(grp_meta, v_stlon, stn_lons) 
        status = nf90_put_var(grp_meta, v_stel, stn_elvs)
        status = nf90_put_var(grp_meta, v_raz, azimuths)
        status = nf90_put_var(grp_meta, v_rtilt, tilts)
        status = nf90_put_var(grp_meta, v_grng, ranges)
        status = nf90_put_var(grp_meta, v_cos_cos, cos_cos_arr)
        status = nf90_put_var(grp_meta, v_sin_cos, sin_cos_arr)
        status = nf90_put_var(grp_meta, v_sint, sin_t_arr)
        status = nf90_put_var(grp_meta, v_hgt, heights)

        status = nf90_put_var(grp_val, v_vr_val, vrs)
        status = nf90_put_var(grp_err, v_vr_err, errs)
        status = nf90_put_var(grp_type, v_vr_typ, typ_arr)

        status = nf90_close(ncid)

        DEALLOCATE(cos_cos_arr, sin_cos_arr, sin_t_arr, loc_idx, typ_arr)
    END SUBROUTINE write_superobs_netcdf

END MODULE module_nexrad_io
