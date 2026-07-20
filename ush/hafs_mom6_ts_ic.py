#! /usr/bin/env python3
################################################################################
# Script Name: hafs_mom6_ts_ic.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script makes the neccesary changes to the ts netcdf file from HYCOM
#   so it can be read by MOM6.
# History:
#   10/02/2025: Added the script for MOM6 coupling in HAFS workflow
# Usage:
#    ./hafs_ocean_ts_ic.py tsfile_from_hycom tsfile_for_hafs_mom6
################################################################################

import argparse
import numpy as np
import netCDF4 as nc
import time as Time

if __name__ == "__main__":

    st = Time.time()

    # get command line args
    parser = argparse.ArgumentParser(
        description="Makes the neccesary changes to the ts netcdf file from HYCOM so it can be read by MOM6")
    parser.add_argument('tsfile_from_hycom', type=str, help="Name of the ts file from HYCOM")
    parser.add_argument('tsfile_for_hafs_mom6', type=str, help="Name of the ts file for HAFS-MOM6")

    args = parser.parse_args()

    tsfile_from_hycom = args.tsfile_from_hycom
    tsfile_for_hafs_mom6 = args.tsfile_for_hafs_mom6

    print(args)

    # Read velocity file on ts grid
    tsnc = nc.Dataset(tsfile_from_hycom,'r')
    time = np.asarray(tsnc['MT'])
    latitude = np.asarray(tsnc['Latitude'])
    longitude = np.asarray(tsnc['Longitude'])
    depth = np.asarray(tsnc['Depth'])
    temp = np.asarray(tsnc['pot_temp'])
    salt = np.asarray(tsnc['salinity'])
    fillvalue = tsnc['pot_temp']._FillValue

    # Create netcdf file
    nc_file= nc.Dataset(tsfile_for_hafs_mom6, 'w', format='NETCDF4')

    # Add a global attribute
    #nc_file.description = 'NetCDF file with the interpolated u and v field from RTOFS onto a MOM6 grid on staggered points'

    # Define dimensions
    time_dim = nc_file.createDimension('time', None)  # Unlimited dimension
    depth_dim = nc_file.createDimension('depth',depth.shape[0])
    latitude_dim = nc_file.createDimension('latitude',latitude.shape[0])
    longitude_dim = nc_file.createDimension('longitude',longitude.shape[0] )

    # Create variables
    time_var = nc_file.createVariable('time', 'f8', ('time',))
    depth_var = nc_file.createVariable('depth', 'f8', ('depth',))
    latitude_var = nc_file.createVariable('latitude', 'f8', ('latitude',))
    longitude_var = nc_file.createVariable('longitude', 'f8', ('longitude',))
    temp_var = nc_file.createVariable('temp', 'f8', ('time','depth','latitude','longitude',),fill_value=fillvalue)
    salt_var = nc_file.createVariable('salt', 'f8', ('time','depth','latitude','longitude',),fill_value=fillvalue)

    # Add attributes to variables
    time_var.long_name = 'time'
    time_var.units = tsnc['MT'].units
    time_var.calendar = tsnc['MT'].calendar

    depth_var.long_name = 'depth'
    depth_var.units = tsnc['Depth'].units
    depth_var.positive = 'down'

    latitude_var.long_name = 'latitude'
    latitude_var.units = 'degrees_north'

    longitude_var.long_name = 'longitude'
    longitude_var.units = 'degrees_east'

    temp_var.long_name = tsnc['pot_temp'].long_name
    temp_var.units = tsnc['pot_temp'].units

    salt_var.long_name = tsnc['salinity'].long_name
    salt_var.units = tsnc['salinity'].units

    # Write data to variables and convert from float to double to ensure reproducibility
    time_var[:] = time
    depth_var[:] = depth
    latitude_var[:] = latitude
    longitude_var[:] = longitude
    temp_var[:] = (temp.astype(np.float32)).astype(np.float64)
    salt_var[:] = (salt.astype(np.float32)).astype(np.float64)

    nc_file.close()

    et = Time.time()
    elapse_time = et - st
    print('Elapse time = ',elapse_time,' seconds')
