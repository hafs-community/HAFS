#! /usr/bin/env python3
################################################################################
# Script Name: hafs_mom6_obc_from_rtofs.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script prepares HAFS MOM6 coupling needed open boundary conditions.
# History:
#
# Usage:
#   ./hafs_mom6_obc_from_rtofs.py inputdir outputdir ssh_file_in ts_file_in \
#     uv_file_in lon_name_in lat_name_in hgrid_out_file \
#     lon_name_hgrid_out lat_name_hgrid_out
################################################################################
import sys
import argparse
import time as Time
import numpy as np
import xarray as xr
import netCDF4 as nc
from scipy import interpolate
try:
    import esmpy as ESMF
except ImportError or ModuleNotFoundError:
    import ESMF as ESMF

from lib_obc_segments import obc_segment
from lib_obc_variable import obc_variable
from lib_obc_vectvariable import obc_vectvariable
from lib_ioncdf import write_obc_file

if __name__ == "__main__":
    # get command line args
    parser = argparse.ArgumentParser(
        description="Generates the open boundary conditions for MOM6 from the RTOFS netcdf files")
    parser.add_argument('inputdir', type=str, help="Location of rtofs output converted to netcdf files")
    parser.add_argument('outputdir', type=str, help="Location where the generated obc files will reside")
    parser.add_argument('ssh_file_in', type=str, help="Name of the file that contains the RTOFS sea surface height")
    parser.add_argument('ts_file_in', type=str, help="Name of the file that contains the RTOFS temperature and salinity field")
    parser.add_argument('uv_file_in', type=str, help="Name of the file that contains the RTOFS u and v velocity fields")
    parser.add_argument('lon_name_in', type=str, help="Name of the longitude variable in the RTOFS netcdf files")
    parser.add_argument('lat_name_in', type=str, help="Name of the latitude variable in the RTOFS netcdf files")
    parser.add_argument('hgrid_out_file', type=str, help="Name of the MOM6 super grid file, e,g. ocean_hgrid.nc")
    parser.add_argument('lon_name_hgrid_out', type=str, help="Name of the longitude variable in the MOM6 super grid file")
    parser.add_argument('lat_name_hgrid_out', type=str, help="Name of the latitude variable in the MOM6 super grid file")

    args = parser.parse_args()

    inputdir = args.inputdir
    outputdir = args.outputdir

    ssh_file_in = args.ssh_file_in
    ts_file_in = args.ts_file_in
    uv_file_in = args.uv_file_in
    lon_name_in = args.lon_name_in
    lat_name_in = args.lat_name_in

    hgrid_out_file = args.hgrid_out_file
    lon_name_hgrid_out = args.lon_name_hgrid_out
    lat_name_hgrid_out = args.lat_name_hgrid_out

    print(args)

    st = Time.time()

    #############################################################
    # Open hgrid file
    hgrid_out = xr.open_dataset(hgrid_out_file,decode_times=False)

    # Read dimensions of super grid (ocean_hgrid.nc)
    Nx = hgrid_out.nxp.values[-1]
    Ny = hgrid_out.nyp.values[-1]

    #############################################################
    # ---------- define segments on MOM grid -----------------------
    north = obc_segment('segment_001',hgrid_out_file,istart=Nx,iend=0,jstart=Ny,jend=Ny)
    south = obc_segment('segment_002',hgrid_out_file,istart=0,iend=Nx,jstart=0,jend=0)
    east  = obc_segment('segment_003',hgrid_out_file,istart=Nx,iend=Nx,jstart=0,jend=Ny)
    west  = obc_segment('segment_004',hgrid_out_file,istart=0,iend=0,jstart=Ny,jend=0)

    # ---------- define variables on each segment ------------------
    temp_south = obc_variable(south,'temp',geometry='surface',obctype='radiation',use_locstream=True)
    temp_north = obc_variable(north,'temp',geometry='surface',obctype='radiation',use_locstream=True)
    temp_east  = obc_variable(east, 'temp',geometry='surface',obctype='radiation',use_locstream=True)
    temp_west  = obc_variable(west, 'temp',geometry='surface',obctype='radiation',use_locstream=True)

    salt_south = obc_variable(south,'salt',geometry='surface',obctype='radiation',use_locstream=True)
    salt_north = obc_variable(north,'salt',geometry='surface',obctype='radiation',use_locstream=True)
    salt_east  = obc_variable(east, 'salt',geometry='surface',obctype='radiation',use_locstream=True)
    salt_west  = obc_variable(west, 'salt',geometry='surface',obctype='radiation',use_locstream=True)

    ssh_south = obc_variable(south,'ssh',geometry='line',obctype='flather',use_locstream=True)
    ssh_north = obc_variable(north,'ssh',geometry='line',obctype='flather',use_locstream=True)
    ssh_east  = obc_variable(east ,'ssh',geometry='line',obctype='flather',use_locstream=True)
    ssh_west  = obc_variable(west ,'ssh',geometry='line',obctype='flather',use_locstream=True)

    vel_south  = obc_vectvariable(south,'u','v',geometry='surface',obctype='radiation',use_locstream=True)
    vel_north  = obc_vectvariable(north,'u','v',geometry='surface',obctype='radiation',use_locstream=True)
    vel_east   = obc_vectvariable(east ,'u','v',geometry='surface',obctype='radiation',use_locstream=True)
    vel_west   = obc_vectvariable(west ,'u','v',geometry='surface',obctype='radiation',use_locstream=True)

    #################################################################
    # Finding regridding weights
    interp_t2s_south_weight = temp_south.interpolate_from(ts_file_in,'pot_temp',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'])

    interp_t2s_north_weight = temp_north.interpolate_from(ts_file_in,'pot_temp',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'])

    interp_t2s_east_weight = temp_east.interpolate_from(ts_file_in,'pot_temp',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'])

    interp_t2s_west_weight = temp_west.interpolate_from(ts_file_in,'pot_temp',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'])

    interp_u2s_south_weight, interp_v2s_south_weight = vel_south.interpolate_from(uv_file_in,'u','v',frame=0,depthname='Depth',timename='MT',coord_names_u=['Longitude','Latitude'],coord_names_v=['Longitude','Latitude'])

    interp_u2s_north_weight, interp_v2s_north_weight = vel_north.interpolate_from(uv_file_in,'u','v',frame=0,depthname='Depth',timename='MT',coord_names_u=['Longitude','Latitude'],coord_names_v=['Longitude','Latitude'])

    interp_u2s_east_weight, interp_v2s_east_weight = vel_east.interpolate_from(uv_file_in,'u','v',frame=0,depthname='Depth',timename='MT',coord_names_u=['Longitude','Latitude'],coord_names_v=['Longitude','Latitude'])

    interp_u2s_west_weight, interp_v2s_west_weight = vel_west.interpolate_from(uv_file_in,'u','v',frame=0,depthname='Depth',timename='MT',coord_names_u=['Longitude','Latitude'],coord_names_v=['Longitude','Latitude'])

    #################################################################
    # Regridding
    temp_south.interpolate_from(ts_file_in,'pot_temp',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_south_weight)

    temp_north.interpolate_from(ts_file_in,'pot_temp',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_north_weight)

    temp_east.interpolate_from(ts_file_in,'pot_temp',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_east_weight)

    temp_west.interpolate_from(ts_file_in,'pot_temp',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_west_weight)

    salt_south.interpolate_from(ts_file_in,'salinity',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_south_weight)

    salt_north.interpolate_from(ts_file_in,'salinity',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_north_weight)

    salt_east.interpolate_from(ts_file_in,'salinity',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_east_weight)

    salt_west.interpolate_from(ts_file_in,'salinity',frame=0,from_global=False,depthname='Depth',timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_west_weight)

    ssh_south.interpolate_from(ssh_file_in,'ssh',frame=0,timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_south_weight)

    ssh_north.interpolate_from(ssh_file_in,'ssh',frame=0,timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_north_weight)

    ssh_east.interpolate_from(ssh_file_in,'ssh',frame=0,timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_east_weight)

    ssh_west.interpolate_from(ssh_file_in,'ssh',frame=0,timename='MT',coord_names=['Longitude','Latitude'],interpolator=interp_t2s_west_weight)

    vel_south.interpolate_from(uv_file_in,'u','v',frame=0,depthname='Depth',timename='MT',coord_names_u=['Longitude','Latitude'],coord_names_v=['Longitude','Latitude'],interpolator_u=interp_u2s_south_weight,interpolator_v=interp_v2s_south_weight)

    vel_north.interpolate_from(uv_file_in,'u','v',frame=0,depthname='Depth',timename='MT',coord_names_u=['Longitude','Latitude'],coord_names_v=['Longitude','Latitude'],interpolator_u=interp_u2s_north_weight,interpolator_v=interp_v2s_north_weight)

    vel_east.interpolate_from(uv_file_in,'u','v',frame=0,depthname='Depth',timename='MT',coord_names_u=['Longitude','Latitude'],coord_names_v=['Longitude','Latitude'],interpolator_u=interp_u2s_east_weight,interpolator_v=interp_v2s_east_weight)

    vel_west.interpolate_from(uv_file_in,'u','v',frame=0,depthname='Depth',timename='MT',coord_names_u=['Longitude','Latitude'],coord_names_v=['Longitude','Latitude'],interpolator_u=interp_u2s_west_weight,interpolator_v=interp_v2s_west_weight)

    ##############################################
    # Writing obc for temp and salinity to netcdf files
    list_segments_south = [south]
    list_segments_north = [north]
    list_segments_east = [east]
    list_segments_west = [west]

    list_variables_south = [temp_south,salt_south]
    list_variables_north = [temp_north,salt_north]
    list_variables_east = [temp_east,salt_east]
    list_variables_west = [temp_west,salt_west]

    list_vectvariables_south = []
    list_vectvariables_north = []
    list_vectvariables_east = []
    list_vectvariables_west = []

    #----------- time --------------------------------------------
    time = temp_south.timesrc
    time.calendar = nc.Dataset(ts_file_in)['MT'].calendar

    # ---------- write to file -----------------------------------
    fileout_south = ts_file_in.split('_')[0]+'_ts_obc_south.nc'
    fileout_north = ts_file_in.split('_')[0]+'_ts_obc_north.nc'
    fileout_east = ts_file_in.split('_')[0]+'_ts_obc_east.nc'
    fileout_west = ts_file_in.split('_')[0]+'_ts_obc_west.nc'

    write_obc_file(list_segments_south,list_variables_south,list_vectvariables_south,time,output=fileout_south)
    write_obc_file(list_segments_north,list_variables_north,list_vectvariables_north,time,output=fileout_north)
    write_obc_file(list_segments_east,list_variables_east,list_vectvariables_east,time,output=fileout_east)
    write_obc_file(list_segments_west,list_variables_west,list_vectvariables_west,time,output=fileout_west)

    ##############################################
    # Writing obc ssh to netcdf files
    list_segments_south = [south]
    list_segments_north = [north]
    list_segments_east = [east]
    list_segments_west = [west]

    list_variables_south = [ssh_south]
    list_variables_north = [ssh_north]
    list_variables_east = [ssh_east]
    list_variables_west = [ssh_west]

    list_vectvariables_south = []
    list_vectvariables_north = []
    list_vectvariables_east = []
    list_vectvariables_west = []

    #----------- time --------------------------------------------
    time = temp_south.timesrc
    time.calendar = nc.Dataset(ssh_file_in)['MT'].calendar

    # ---------- write to file -----------------------------------
    fileout_south = ssh_file_in.split('_')[0]+'_ssh_obc_south.nc'
    fileout_north = ssh_file_in.split('_')[0]+'_ssh_obc_north.nc'
    fileout_east = ssh_file_in.split('_')[0]+'_ssh_obc_east.nc'
    fileout_west = ssh_file_in.split('_')[0]+'_ssh_obc_west.nc'

    write_obc_file(list_segments_south,list_variables_south,list_vectvariables_south,time,output=fileout_south)
    write_obc_file(list_segments_north,list_variables_north,list_vectvariables_north,time,output=fileout_north)
    write_obc_file(list_segments_east,list_variables_east,list_vectvariables_east,time,output=fileout_east)
    write_obc_file(list_segments_west,list_variables_west,list_vectvariables_west,time,output=fileout_west)

    ##############################################
    # Writing obc for u and v velovities to netcdf files
    list_segments_south = [south]
    list_segments_north = [north]
    list_segments_east = [east]
    list_segments_west = [west]

    list_variables_south = []
    list_variables_north = []
    list_variables_east = []
    list_variables_west = []

    list_vectvariables_south = [vel_south]
    list_vectvariables_north = [vel_north]
    list_vectvariables_east = [vel_east]
    list_vectvariables_west = [vel_west]

    #----------- time --------------------------------------------
    time = vel_south.timesrc
    time.calendar = nc.Dataset(uv_file_in)['MT'].calendar

    # ---------- write to file -----------------------------------
    fileout_south = uv_file_in.split('_')[0]+'_uv_obc_south.nc'
    fileout_north = uv_file_in.split('_')[0]+'_uv_obc_north.nc'
    fileout_east = uv_file_in.split('_')[0]+'_uv_obc_east.nc'
    fileout_west = uv_file_in.split('_')[0]+'_uv_obc_west.nc'

    write_obc_file(list_segments_south,list_variables_south,list_vectvariables_south,time,output=fileout_south)
    write_obc_file(list_segments_north,list_variables_north,list_vectvariables_north,time,output=fileout_north)
    write_obc_file(list_segments_east,list_variables_east,list_vectvariables_east,time,output=fileout_east)
    write_obc_file(list_segments_west,list_variables_west,list_vectvariables_west,time,output=fileout_west)

    et = Time.time()
    elapse_time = et - st
    print('Elapse_time = ', elapse_time, ' seconds')
