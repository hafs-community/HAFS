#!/usr/bin/env python3

# Usage:
#   ./hafs_mom6_gfs_forcings.py ${YMDH} -t ${temporal_folder} -l ${Length_hours} -s ${COMINgfs}
# Example:
#   ./hafs_mom6_gfs_forcings.py 2020082512 -t . -l 120 -s /your/hafs-input/COMGFSv16/

import os
import glob
import argparse
from dateutil.parser import parse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta

# Variables averaged over several forecast hours
gfs_vars_average = ['"(USWRF):(surface)"',\
                    '"(DSWRF):(surface)"',\
                    '"(ULWRF):(surface)"',\
                    '"(DLWRF):(surface)"',\
                    '"(UFLX):(surface)"',\
                    '"(VFLX):(surface)"',\
                    '"(SHTFL):(surface)"',\
                    '"(LHTFL):(surface)"']

# Instantaneos variables
gfs_vars_inst = ['"(UGRD):(10 m above ground)"',\
                 '"(VGRD):(10 m above ground)"',\
                 '"(PRES):(surface)"',\
                 '"(PRATE):(surface)"']

gfs_vars = [gfs_vars_average, gfs_vars_inst]

def get_ymdh_fcst_hr_to_read_gfs_file(date_ini,date_iforc,fdate,delta_t):

    if delta_t <= 0:
        date_ii = date_iforc - timedelta(hours=6)
        y = str(date_ii.year)
        m = [str(date_ii.month) if len(str(date_ii.month))>1 else '0'+str(date_ii.month)][0]
        d = [str(date_ii.day) if len(str(date_ii.day))>1 else '0'+str(date_ii.day)][0]
        h = [str(date_ii.hour) if len(str(date_ii.hour))>1 else '0'+str(date_ii.hour)][0]
        fcsthr = fdate.hour-int(h)
        if fcsthr < 0:
            fcsthr = 24 + fcsthr
        if len(str(fcsthr))==1:
            fcst_hr = '00'+str(fcsthr)
        if len(str(fcsthr))==2:
            fcst_hr = '0'+str(fcsthr)
        if len(str(fcsthr))==3:
            fcst_hr = str(fcsthr)
    else:
        y = str(date_ini.year)
        m = [str(date_ini.month) if len(str(date_ini.month))>1 else '0'+str(date_ini.month)][0]
        d = [str(date_ini.day) if len(str(date_ini.day))>1 else '0'+str(date_ini.day)][0]
        h = [str(date_ini.hour) if len(str(date_ini.hour))>1 else '0'+str(date_ini.hour)][0]
        if len(str(delta_t))==1:
            fcst_hr = '00'+str(delta_t)
        if len(str(delta_t))==2:
            fcst_hr = '0'+str(delta_t)
        if len(str(delta_t))==3:
            fcst_hr = str(delta_t)

    return y, m, d, h, fcst_hr

def proc(date_s, length_hours, gfs_folder, tmp):

    print("Preparing forcing file to cover forcast period of")
    print(" Start: ", date_s)
    print(" Length: ", length_hours)
    date_ini = date_s
    date_end = date_ini + timedelta(hours = length_hours)
    for type,vars in enumerate(gfs_vars):
        print(type)
        print(vars)
        for var in vars:
            if type == 0:
                print('Average fields')
                print(var)
                # date_iforc is cycle tile + 3h, then the time will be shifted -3 hours for
                # average fields. Then the initial time will be equal to the cycle time
                date_iforc = date_ini + timedelta(hours = 3)
                nfcst_hr = int((date_end - date_iforc).total_seconds()/(3600*3)) + 3
                files_nc = ''
                for n in np.arange(nfcst_hr):
                    hafs_fcst_hr = str(int(n*3))
                    fdate = date_iforc + n*timedelta(hours = 3)
                    delta_t = int((fdate - date_ini).total_seconds()/3600)
                    # We need to shift - 3 hours the time to account for the fact
                    # that these are average fields
                    shifted_time = fdate - timedelta(hours = 3)
                    ndays = (shifted_time - datetime(1970,1,1)).days
                    nseconds = (shifted_time - datetime(1970,1,1)).seconds
                    shifted_timestamp = ndays*24*3600 + nseconds
                    print(n)
                    print('fdate=',fdate)
                    print('delta_t=',delta_t)
                    print('shifted_time=',shifted_time)
                    y, m, d, h, fcst_hr = get_ymdh_fcst_hr_to_read_gfs_file(date_ini,date_iforc,fdate,delta_t)
                    if n == np.max(np.arange(nfcst_hr)):
                        print('Last forecast hour')
                        fcst_hr = str(int(fcst_hr) - 3)
                    file_gfs = gfs_folder + '/gfs.'+y+m+d+'/'+h+'/atmos/gfs.t'+h+'z.pgrb2.0p25.f'+fcst_hr
                    print(file_gfs)
                    if os.path.exists(file_gfs):
                        file_found = True
                        print('Forecast hour = ',fcst_hr)
                        print("Exists: ",file_gfs)
                    else:
                        raise Exception("file was not found on disk")
                    gfs_var_name = var.split('"')[1].split(':')[0][1:-1]
                    tmp_gfs_nc = 'gfs_global_' +y+m+d+h+ '_f' + hafs_fcst_hr + '_' + gfs_var_name + '.nc'
                    # Change from wgrib2 to netcdf
                    cmd = 'wgrib2 ' + file_gfs + ' -match ' + var + ' -netcdf ' + tmp_gfs_nc
                    os.system(cmd)

                    # Open ncfile
                    ncfile = nc.Dataset(tmp_gfs_nc,'a')
                    # Add calendar type to time variable
                    ncfile['time'][:] = shifted_timestamp
                    ncfile.variables['time'].short_name = "time"
                    ncfile.variables['time'].long_name = "time"
                    #ncfile.variables['time'].calendar = "julian"
                    ncfile.variables['time'].calendar = "gregorian"
                    ncfile.variables['time'].reference_time_description = " "
                    ncfile.variables['time'].units = "seconds since 1970-01-01 00:00:00"
                    ncfile.close()
                    # This is the string that contains all netcdf files that need to be concatenated
                    files_nc = files_nc + ' ' + tmp_gfs_nc

                print('Concatenating netcdf files for different forecast times')
                cmd = 'ncrcat ' + files_nc + ' gfs_global_' +y+m+d+h+ '_' + gfs_var_name + '.nc'
                print(cmd)
                os.system(cmd)
                os.system('rm ' + 'gfs_global_' +y+m+d + '*_f*' + gfs_var_name + '.nc')
  
            if type == 1:
                print('Instantaneous fields')
                print(var)
                # date_iforc is equeal to cycle tile for instantaneos fields
                date_iforc = date_ini - timedelta(hours = 0)
                nfcst_hr = int((date_end - date_iforc).total_seconds()/(3600*3)) + 2
                files_nc = ''
                for n in np.arange(nfcst_hr):
                    hafs_fcst_hr = str(int(n*3))
                    fdate = date_iforc + n*timedelta(hours = 3)
                    delta_t = int((fdate - date_ini).total_seconds()/3600)
                    # No shifting in time is needed because these are
                    # instantaneous fields
                    shifted_time = fdate - timedelta(hours = 0)
                    ndays = (shifted_time - datetime(1970,1,1)).days
                    nseconds = (shifted_time - datetime(1970,1,1)).seconds
                    shifted_timestamp = ndays*24*3600 + nseconds
                    print(n)
                    print('fdate=',fdate)
                    print('delta_t=',delta_t)
                    print('shifted_time=',shifted_time)
                    y, m, d, h, fcst_hr = get_ymdh_fcst_hr_to_read_gfs_file(date_ini,date_iforc,fdate,delta_t)
                    file_gfs = gfs_folder + '/gfs.'+y+m+d+'/'+h+'/atmos/gfs.t'+h+'z.pgrb2.0p25.f'+fcst_hr
                    print(file_gfs)
                    if os.path.exists(file_gfs):
                        file_found = True
                        print('Forecast hour = ',fcst_hr)
                        print("Exists: ",file_gfs)
                    else:
                        raise Exception("file was not found on disk")
                    gfs_var_name = var.split('"')[1].split(':')[0][1:-1]
                    tmp_gfs_nc = 'gfs_global_' +y+m+d+h+ '_f' + hafs_fcst_hr + '_' + gfs_var_name + '.nc'
                    # Change from wgrib2 to netcdf
                    cmd = 'wgrib2 ' + file_gfs + ' -match ' + var + ' -netcdf ' + tmp_gfs_nc
                    os.system(cmd)

                    # Open ncfile
                    ncfile = nc.Dataset(tmp_gfs_nc,'a')
                    # Add calendar type to time variable
                    ncfile['time'][:] = shifted_timestamp
                    ncfile.variables['time'].short_name = "time"
                    ncfile.variables['time'].long_name = "time"
                    #ncfile.variables['time'].calendar = "julian"
                    ncfile.variables['time'].calendar = "gregorian"
                    ncfile.variables['time'].reference_time_description = " "
                    ncfile.variables['time'].units = "seconds since 1970-01-01 00:00:00"
                    ncfile.close()
                    # This is the string that contains all netcdf files that need to be concatenated
                    files_nc = files_nc + ' ' + tmp_gfs_nc

                print('Concatenating netcdf files for different forecast times')
                cmd = 'ncrcat ' + files_nc + ' gfs_global_' +y+m+d+h+ '_' + gfs_var_name + '.nc'
                print(cmd)
                os.system(cmd)
                os.system('rm ' + 'gfs_global_' +y+m+d + '*_f*' + gfs_var_name + '.nc')

    # Obtain net longwave and shortwave radiation file
    cmd = 'ncks -A ' + 'gfs_global_' + y+m+d+h + '_ULWRF.nc' +  ' -o ' + 'gfs_global_' + y+m+d+h + '_LWRF.nc'
    os.system(cmd)

    cmd = 'ncks -A ' + 'gfs_global_' + y+m+d+h + '_DLWRF.nc' +  ' -o ' + 'gfs_global_' + y+m+d+h + '_LWRF.nc'
    os.system(cmd)

    cmd = 'ncap2 -v -O -s NETLW_surface=DLWRF_surface-ULWRF_surface ' + 'gfs_global_' + y+m+d+h + '_LWRF.nc' + ' gfs_global_' + y+m+d+h + '_NETLW.nc'
    os.system(cmd)

    cmd = 'ncatted -O -a long_name,NETLW_surface,o,c,"Net Long-Wave Radiation Flux" gfs_global_' + y+m+d+h + '_NETLW.nc'
    os.system(cmd)
    cmd = 'ncatted -O -a short_name,NETLW_surface,o,c,"NETLW_surface" gfs_global_' + y+m+d+h + '_NETLW.nc'
    os.system(cmd)

    cmd = 'ncks -A ' + 'gfs_global_' + y+m+d+h + '_USWRF.nc' +  ' -o ' + 'gfs_global_' + y+m+d+h + '_SWRF.nc'
    os.system(cmd)

    cmd = 'ncks -A ' + 'gfs_global_' + y+m+d+h + '_DSWRF.nc' +  ' -o ' + 'gfs_global_' + y+m+d+h + '_SWRF.nc'
    os.system(cmd)

    cmd = 'ncap2 -v -O -s NETSW_surface=DSWRF_surface-USWRF_surface ' + 'gfs_global_' + y+m+d+h +'_SWRF.nc' + ' gfs_global_' + y+m+d+h + '_NETSW.nc'
    os.system(cmd)

    cmd = 'ncatted -O -a long_name,NETSW_surface,o,c,"Net Short-Wave Radiation Flux" gfs_global_' + y+m+d+h + '_NETSW.nc'
    os.system(cmd)
    cmd = 'ncatted -O -a short_name,NETSW_surface,o,c,"NETSW_surface" gfs_global_' + y+m+d+h + '_NETSW.nc'
    os.system(cmd)

    # Add four components to the NETSW radiation file
    # SWVDF=Visible Diffuse Downward Solar Flux. SWVDF=0.285*NETSW_surface
    # SWVDR=Visible Beam Downward Solar Flux. SWVDR=0.285*NETSW_surface
    # SWNDF=Near IR Diffuse Downward Solar Flux. SWNDF=0.215*NETSW_surface
    # SWNDR=Near IR Beam Downward Solar Flux. SWNDR=0.215*NETSW_surface

    cmd = 'ncap2 -v -O -s "SWVDF_surface=float(0.285*NETSW_surface)" ' + 'gfs_global_' + y+m+d+h +'_NETSW.nc' + ' gfs_global_' + y+m+d+h + '_SWVDF.nc'
    os.system(cmd)

    cmd = 'ncatted -O -a long_name,SWVDF_surface,o,c,"Visible Diffuse Downward Solar Flux" gfs_global_' + y+m+d+h + '_SWVDF.nc'
    os.system(cmd)
    cmd = 'ncatted -O -a short_name,SWVDF_surface,o,c,"SWVDF_surface" gfs_global_' + y+m+d+h + '_SWVDF.nc'
    os.system(cmd)

    cmd = 'ncap2 -v -O -s "SWVDR_surface=float(0.285*NETSW_surface)" ' + 'gfs_global_' + y+m+d+h +'_NETSW.nc' + ' gfs_global_' + y+m+d+h + '_SWVDR.nc'
    os.system(cmd)

    cmd = 'ncatted -O -a long_name,SWVDR_surface,o,c,"Visible Beam Downward Solar Flux" gfs_global_' + y+m+d+h + '_SWVDR.nc'
    os.system(cmd)
    cmd = 'ncatted -O -a short_name,SWVDR_surface,o,c,"SWVDR_surface" gfs_global_' + y+m+d+h + '_SWVDR.nc'
    os.system(cmd)

    cmd = 'ncap2 -v -O -s "SWNDF_surface=float(0.215*NETSW_surface)" ' + 'gfs_global_' + y+m+d+h +'_NETSW.nc' + ' gfs_global_' + y+m+d+h + '_SWNDF.nc'
    os.system(cmd)

    cmd = 'ncatted -O -a long_name,SWNDF_surface,o,c,"Near IR Diffuse Downward Solar Flux" gfs_global_' + y+m+d+h + '_SWNDF.nc'
    os.system(cmd)
    cmd = 'ncatted -O -a short_name,SWNDF_surface,o,c,"SWNDF_surface" gfs_global_' + y+m+d+h + '_SWNDF.nc'
    os.system(cmd)

    cmd = 'ncap2 -v -O -s "SWNDR_surface=float(0.215*NETSW_surface)" ' + 'gfs_global_' + y+m+d+h +'_NETSW.nc' + ' gfs_global_' + y+m+d+h + '_SWNDR.nc'
    os.system(cmd)

    cmd = 'ncatted -O -a long_name,SWNDR_surface,o,c,"Near IR Beam Downward Solar Flux" gfs_global_' + y+m+d+h + '_SWNDR.nc'
    os.system(cmd)
    cmd = 'ncatted -O -a short_name,SWNDR_surface,o,c,"SWVDR_surface" gfs_global_' + y+m+d+h + '_SWNDR.nc'
    os.system(cmd)

    # Concatenate all files
    cmd = 'cdo merge gfs_global_' + y+m+d+h + '_NETLW.nc gfs_global_' + y+m+d+h + '_NETSW.nc gfs_global_' + y+m+d+h + '_SWVDF.nc gfs_global_' + y+m+d+h + '_SWVDR.nc gfs_global_' + y+m+d+h + '_SWNDF.nc gfs_global_' + y+m+d+h + '_SWNDR.nc gfs_global_' + y+m+d+h + '_LHTFL.nc gfs_global_' + y+m+d+h + '_SHTFL.nc gfs_global_' + y+m+d+h + '_UFLX.nc gfs_global_' + y+m+d+h + '_VFLX.nc gfs_global_' + y+m+d+h + '_UGRD.nc gfs_global_' + y+m+d+h + '_VGRD.nc gfs_global_' + y+m+d+h + '_PRES.nc gfs_global_' + y+m+d+h + '_PRATE.nc ' + 'ocean_forcings.nc'
    os.system(cmd)

if __name__ == "__main__":

    # get command line args
    parser = argparse.ArgumentParser(
        description="Download the atmospheric surface fields needed to force the ocean model forecast. Files are converted from grib2 to netcdf.")
    parser.add_argument('date', help="start date, in any valid date format")
    parser.add_argument('-l','--length_hours', type=int, default=24,
        help="length of required forcing in hours (date + length). Default %(default)s")
    parser.add_argument('-t','--tmp', type=str, default="./",
        help="location to temporarily store the downloaded grib2 files before"+
        " being converted. Default: %(default)s")
    parser.add_argument('-s','--source', type=str,
        default="/work/noaa/hwrf/noscrub/hafs-input/COMGFSv16/",
        help="location of GFS output")

    args = parser.parse_args()
    args.date = datetime.strptime(args.date,'%Y%m%d%H')

    print(args)
    date_s = args.date
    length_hours = args.length_hours
    gfs_folder = args.source
    tmp = args.tmp

    print("Preparing forcing file to cover forcast period of")
    print(" Start: ", date_s)
    print(" Length: ", length_hours)
    print(' ')

    date_ini = date_s
    date_end = date_ini + timedelta(hours = length_hours)

    proc(date_s = args.date, length_hours = args.length_hours,
         gfs_folder = args.source,tmp = args.tmp)


