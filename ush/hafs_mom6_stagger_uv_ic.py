#! /usr/bin/env python3
################################################################################
# Script Name: hafs_mom6_stagger_uv_ic.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script shifts u and v velocities on tracer points to a staggered MOM6 grid.
# History:
#   10/02/2025: Added the script for MOM6 coupling in HAFS workflow
# Usage:
#    ./hafs_mom6_stagger_uv_ic.py uvfile_hycom uvfile_stagger_for_hafs_mom6
################################################################################

import argparse
import numpy as np
import netCDF4 as nc
import time as Time

if __name__ == "__main__":

    st = Time.time()

    # get command line args
    parser = argparse.ArgumentParser(
        description="Shift u and v velocities on tracer points to a staggered MOM6 grid")
    #parser.add_argument('hgridfile_mom6', type=str, help="Name of the hgrid MOM6 file that contains the lonq and latq information ")
    parser.add_argument('uvfile_hycom', type=str, help="Name of the HYCOM file that containts the u and v velocities on the original HYCOM grid")
    parser.add_argument('uvfile_stagger_for_hafs_mom6', type=str, help="Name of the output file that will containe the staggered u and v velocities")

    args = parser.parse_args()
    print(args)

    #hgridfile_mom6 = args.hgridfile_mom6
    uvfile_hycom = args.uvfile_hycom
    print(uvfile_hycom)
    uvfile_stagger_for_hafs_mom6 = args.uvfile_stagger_for_hafs_mom6
    print(uvfile_stagger_for_hafs_mom6)

    print(args)

    # Read velocity file on ts grid
    uvnc = nc.Dataset(uvfile_hycom,'r')
    u = np.asarray(uvnc['u'])
    v = np.asarray(uvnc['v'])
    lath = np.asarray(uvnc['Latitude'])
    lonh = np.asarray(uvnc['Longitude'])
    depth = np.asarray(uvnc['Depth'])
    fillvalue = 0.0

    # Convert u and v to double precission
    u = u.astype(np.float64)
    v = v.astype(np.float64)

    # Make fill values nan
    u[u>1000] = np.nan
    v[v>1000] = np.nan

    # Read latq and lonq
    #hgridnc = nc.Dataset(hgridfile_mom6,'r')
    #latq = np.asarray(hgridnc['nyp'])[1::2]
    #lonq = np.asarray(hgridnc['nxp'])[1::2]

    # Define u_stagger
    u_stagger = np.empty((u.shape[1],u.shape[2],u.shape[3]+1))
    u_stagger[:] = np.nan
    # Avering u on ts points to u points
    u_stagger[:,:,1:-1] = (u[0,:,:,:-1] + u[0,:,:,1:])/2
    # Filling the first cross section of u
    u_stagger[:,:,0] = u_stagger[:,:,1]
    # Filling the last cross section of u
    u_stagger[:,:,-1] = u_stagger[:,:,-2]
    # Fill nans with fillvalue
    u_stagger[np.isnan(u_stagger)] = fillvalue

    # Define v_stagger
    v_stagger = np.empty((v.shape[1],v.shape[2]+1,u.shape[3]))
    v_stagger[:] = np.nan
    # Avering u on ts points to u points
    v_stagger[:,1:-1,:] = (v[0,:,:-1,:] + v[0,:,1:,:])/2
    # Filling the first cross section of u
    v_stagger[:,0,:] = v_stagger[:,1,:]
    # Filling the last cross section of u
    v_stagger[:,-1,:] = v_stagger[:,-2,:]
    # Fill nans with fillvalue
    v_stagger[np.isnan(v_stagger)] = fillvalue

    # Save rotated velocities into netcdf file
    nc_file= nc.Dataset(uvfile_stagger_for_hafs_mom6, 'w', format='NETCDF4')

    # Add a global attribute
    nc_file.description = 'NetCDF file with the interpolated u and v field from RTOFS on staggered points'

    # Define dimensions
    depth_dim = nc_file.createDimension('depth',depth.shape[0])
    lath_dim = nc_file.createDimension('lath',lath.shape[0])
    lonh_dim = nc_file.createDimension('lonh',lonh.shape[0] )
    latq_dim = nc_file.createDimension('latq',lath.shape[0]+1)
    lonq_dim = nc_file.createDimension('lonq',lonh.shape[0]+1)

    # Create variables
    depth_var = nc_file.createVariable('depth', 'f8', ('depth',))
    lath_var = nc_file.createVariable('lath', 'f8', ('lath',))
    lonh_var = nc_file.createVariable('lonh', 'f8', ('lonh',))
    #latq_var = nc_file.createVariable('latq', 'f4', ('latq',))
    #lonq_var = nc_file.createVariable('lonq', 'f4', ('lonq',))
    u_var = nc_file.createVariable('u', 'f8', ('depth','lath','lonq',),fill_value=fillvalue)
    v_var = nc_file.createVariable('v', 'f8', ('depth','latq','lonh',),fill_value=fillvalue)

    # Add attributes to variables
    lath_var.long_name = 'latitude'
    lath_var.units = 'degrees_north'

    lonh_var.long_name = 'longitude'
    lonh_var.units = 'degrees_east'

    u_var.long_name = 'eastward_sea_water_velocity'
    u_var.units = 'm/s'

    v_var.long_name = 'northward_sea_water_velocity'
    v_var.units = 'm/s'

    # Write data to variables and convert from float to double to ensure reproducibility
    depth_var[:] = depth
    lath_var[:] = lath
    lonh_var[:] = lonh
    u_var[:] = (u_stagger.astype(np.float32)).astype(np.float64)
    v_var[:] = (v_stagger.astype(np.float32)).astype(np.float64)

    nc_file.close()

    et = Time.time()
    elapse_time = et - st
    print('Elapse time = ',elapse_time,' seconds')

