#! /usr/bin/env python3
################################################################################
# Script Name: hafs_mom6_ssh_ic.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script makes the neccesary changes to the ssh netcdf file from HYCOM 
#   so it can be read by MOM6.
# History:
#   10/02/2025: Added the script for MOM6 coupling in HAFS workflow
# Usage:
#    ./hafs_ocean_ssh_ic.py sshfile_from_hycom sshfile_for_hafs_mom6
################################################################################

import argparse
import numpy as np
import netCDF4 as nc
import time as Time

if __name__ == "__main__":

    st = Time.time()

    # get command line args
    parser = argparse.ArgumentParser(
        description="Makes the neccesary changes to the ssh netcdf file from HYCOM so it can be read by MOM6")
    parser.add_argument('sshfile_from_hycom', type=str, help="Name of the ssh file from HYCOM")
    parser.add_argument('sshfile_for_hafs_mom6', type=str, help="Name of the ssh file for HAFS-MOM6")

    args = parser.parse_args()

    sshfile_from_hycom = args.sshfile_from_hycom
    sshfile_for_hafs_mom6 = args.sshfile_for_hafs_mom6

    print(args)

    # Read velocity file on ts grid
    sshnc = nc.Dataset(sshfile_from_hycom,'r')
    time = np.asarray(sshnc['MT'])
    latitude = np.asarray(sshnc['Latitude'])
    longitude = np.asarray(sshnc['Longitude'])
    ssh = np.asarray(sshnc['ssh'])
    #fillvalue = sshnc['ssh']._FillValue
    fillvalue = 0.0
    
    # Make fill values = 0
    ssh[ssh>10000] = fillvalue

    # Create netcdf file
    nc_file= nc.Dataset(sshfile_for_hafs_mom6, 'w', format='NETCDF4')
    
    # Add a global attribute
    #nc_file.description = 'NetCDF file with the interpolated u and v field from RTOFS onto a MOM6 grid on staggered points'
    
    # Define dimensions
    time_dim = nc_file.createDimension('time', None)  # Unlimited dimension
    latitude_dim = nc_file.createDimension('latitude',latitude.shape[0])
    longitude_dim = nc_file.createDimension('longitude',longitude.shape[0] )
    
    # Create variables
    time_var = nc_file.createVariable('time', 'f8', ('time',))
    latitude_var = nc_file.createVariable('latitude', 'f4', ('latitude',))
    longitude_var = nc_file.createVariable('longitude', 'f4', ('longitude',))
    ssh_var = nc_file.createVariable('ssh', 'f4', ('time','latitude','longitude',),fill_value=fillvalue)
    
    # Add attributes to variables
    time_var.long_name = 'time'
    time_var.units = sshnc['MT'].units
    time_var.calendar = sshnc['MT'].calendar

    latitude_var.long_name = 'latitude'
    latitude_var.units = 'degrees_north'
    
    longitude_var.long_name = 'longitude'
    longitude_var.units = 'degrees_east'
    
    ssh_var.long_name = sshnc['ssh'].long_name
    ssh_var.units = sshnc['ssh'].units

    # Write data to variables
    time_var[:] = time
    latitude_var[:] = latitude
    longitude_var[:] = longitude
    ssh_var[:] = ssh
    
    nc_file.close()
    
    et = Time.time()
    elapse_time = et - st
    print('Elapse time = ',elapse_time,' seconds')
