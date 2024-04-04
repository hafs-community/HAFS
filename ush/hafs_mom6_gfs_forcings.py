#! /usr/bin/env python3
################################################################################
# Script Name: hafs_mom6_gfs_forcings.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script generates atmospheric forcing files needed by MOM6 coupling.
# History:
#
# Usage:
#   ./hafs_mom6_gfs_forcings.py ${YMDH} -l ${Length_hours}
################################################################################
import os
import glob
import argparse
from dateutil.parser import parse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta

# Variables averaged over several forecast hours
gfs_vars_average = ['USWRF_surface',\
                    'DSWRF_surface',\
                    'ULWRF_surface',\
                    'DLWRF_surface',\
                    'UFLX_surface',\
                    'VFLX_surface',\
                    'SHTFL_surface',\
                    'LHTFL_surface']

# Instantaneos variables
gfs_vars_inst = ['UGRD_10maboveground',\
                 'VGRD_10maboveground',\
                 'PRES_surface',\
                 'PRATE_surface',\
                 'TMP_surface']

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

if __name__ == "__main__":
    # get command line args
    parser = argparse.ArgumentParser(
        description="Download the atmospheric surface fields needed to force the ocean model forecast. Files are converted from grib2 to netcdf.")
    parser.add_argument('date', help="start date, in any valid date format")
    parser.add_argument('-l','--length_hours', type=int, default=24,
        help="length of required forcing in hours (date + length). Default %(default)s")

    args = parser.parse_args()
    args.date = datetime.strptime(args.date,'%Y%m%d%H')

    print(args)
    date_s = args.date
    length_hours = args.length_hours
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
                gfs_var_name = var
                file_var_name = var.split('_')[0]
                print('gfs_var_name ', gfs_var_name)
                date_iforc = date_ini + timedelta(hours = 0)
                nfcst_hr = int((date_end - date_iforc).total_seconds()/(3600*3)) + 2
                files_nc = ''
                for n in np.arange(nfcst_hr):
                    hafs_fcst_hr = str(int(n*3))
                    fdate = date_iforc + n*timedelta(hours = 3)
                    delta_t = int((fdate - date_ini).total_seconds()/3600)
                    shifted_time = fdate - timedelta(hours = 0)
                    ndays = (shifted_time - datetime(1970,1,1)).days
                    nseconds = (shifted_time - datetime(1970,1,1)).seconds
                    shifted_timestamp = ndays*24*3600 + nseconds
                    print(n)
                    print('fdate=',fdate)
                    print('delta_t=',delta_t)
                    print('shifted_time=',shifted_time)
                    y, m, d, h, fcst_hr = get_ymdh_fcst_hr_to_read_gfs_file(date_ini,date_iforc,fdate,delta_t)
                    if n < np.max(np.arange(nfcst_hr)):
                        if (int(fcst_hr)+int(h))%2 == 0:
                            y1, m1, d1, h1, fcst_hr1 = get_ymdh_fcst_hr_to_read_gfs_file(date_ini,date_iforc,fdate,delta_t)
                            print(y1,m1,d1,h1,fcst_hr1)
                            y2, m2, d2, h2, fcst_hr2 = get_ymdh_fcst_hr_to_read_gfs_file(date_ini,date_iforc,fdate,delta_t+3)
                            print(y2,m2,d2,h2,fcst_hr2)
                            file_gfs1 = 'gfs_global_' +y1+m1+d1+h1+ '_f' + fcst_hr1 + '.nc'
                            file_gfs2 = 'gfs_global_' +y2+m2+d2+h2+ '_f' + fcst_hr2 + '.nc'
                            tmp_gfs_nc1 = 'gfs_global_' +y1+m1+d1+h1+ '_f' + fcst_hr1 + '_' + hafs_fcst_hr + '_' + file_var_name + '.nc'
                            tmp_gfs_nc2 = 'gfs_global_' +y2+m2+d2+h2+ '_f' + fcst_hr2 + '_' + hafs_fcst_hr + '_' + file_var_name + '.nc'
                            # Extracting gfs_var_name from file_gfs
                            cmd = 'ncks -v ' + gfs_var_name + ' ' + file_gfs1 + ' ' + tmp_gfs_nc1
                            print(cmd)
                            os.system(cmd)
                            cmd = 'ncks -v ' + gfs_var_name + ' ' + file_gfs2 + ' ' + tmp_gfs_nc2
                            print(cmd)
                            os.system(cmd)
                            # Open ncfiles
                            ncfile1 = nc.Dataset(tmp_gfs_nc1,'a')
                            ncfile2 = nc.Dataset(tmp_gfs_nc2,'a')
                            flux1 = ncfile1[gfs_var_name][:]
                            flux2 = ncfile2[gfs_var_name][:]
                            # Weighted average
                            flux = 1/3 * flux1 + 2/3 * flux2
                            # write corrected flux value
                            ncfile1[gfs_var_name][:] = flux
                        else:
                            y1, m1, d1, h1, fcst_hr1 = get_ymdh_fcst_hr_to_read_gfs_file(date_ini,date_iforc,fdate,delta_t+3)
                            print(y1,m1,d1,h1,fcst_hr1)
                            file_gfs1 = 'gfs_global_' +y1+m1+d1+h1+ '_f' + fcst_hr1 + '.nc'
                            tmp_gfs_nc1 = 'gfs_global_' +y1+m1+d1+h1+ '_f' + fcst_hr1 + '_' + hafs_fcst_hr + '_' + file_var_name + '.nc'
                            # Extracting gfs_var_name from file_gfs
                            cmd = 'ncks -v ' + gfs_var_name + ' ' + file_gfs1 + ' ' + tmp_gfs_nc1
                            print(cmd)
                            os.system(cmd)
                            # Open ncfile
                            ncfile1 = nc.Dataset(tmp_gfs_nc1,'a')

                        # Add calendar type to time variable
                        ncfile1['time'][:] = shifted_timestamp
                        ncfile1.variables['time'].short_name = "time"
                        ncfile1.variables['time'].long_name = "time"
                        ncfile1.variables['time'].calendar = "gregorian"
                        ncfile1.variables['time'].reference_time_description = " "
                        ncfile1.variables['time'].units = "seconds since 1970-01-01 00:00:00"
                        ncfile1.close()
                        tmp_gfs_nc_intp = 'gfs_global_f' + hafs_fcst_hr + '_' + file_var_name + '.nc'
                        cmd = 'cp ' + tmp_gfs_nc1 + ' ' + tmp_gfs_nc_intp
                        os.system(cmd)

                    else:
                        print('Last forecast hour')
                        tmp_gfs_nc_intp_prev = 'gfs_global_f' + str(int(hafs_fcst_hr)-3) + '_' + file_var_name + '.nc'
                        tmp_gfs_nc_intp = 'gfs_global_f' + hafs_fcst_hr + '_' + file_var_name + '.nc'
                        cmd = 'cp ' + tmp_gfs_nc_intp_prev + ' ' + tmp_gfs_nc_intp
                        print(cmd)
                        os.system(cmd)
                        # Open ncfile1
                        ncfile1 = nc.Dataset(tmp_gfs_nc_intp,'a')
                        ncfile1['time'][:] = shifted_timestamp
                        ncfile1.close()

                    # This is the string that contains all netcdf files that need to be concatenated
                    files_nc = files_nc + ' ' + tmp_gfs_nc_intp

                print('Concatenating netcdf files for different forecast times')
                cmd = 'ncrcat ' + files_nc + ' gfs_global_' +y+m+d+h+ '_' + file_var_name + '.nc'
                print(cmd)
                os.system(cmd)
                os.system('rm ' + 'gfs_global_*f*' + file_var_name + '.nc')

            if type == 1:
                print('Instantaneous fields')
                gfs_var_name = var
                file_var_name = var.split('_')[0]
                print(gfs_var_name)
                # date_iforc is equeal to cycle time for instantaneos fields
                date_iforc = date_ini - timedelta(hours = 0)
                nfcst_hr = int((date_end - date_iforc).total_seconds()/(3600*3)) + 2
                files_nc = ''
                for n in np.arange(nfcst_hr):
                    hafs_fcst_hr = str(int(n*3))
                    fdate = date_iforc + n*timedelta(hours = 3)
                    # No shifting in time is needed because these are
                    # instantaneous fields
                    delta_t = int((fdate - date_ini).total_seconds()/3600)
                    shifted_time = fdate - timedelta(hours = 0)
                    ndays = (shifted_time - datetime(1970,1,1)).days
                    nseconds = (shifted_time - datetime(1970,1,1)).seconds
                    shifted_timestamp = ndays*24*3600 + nseconds
                    print(n)
                    print('fdate=',fdate)
                    print('delta_t=',delta_t)
                    print('shifted_time=',shifted_time)
                    y, m, d, h, fcst_hr = get_ymdh_fcst_hr_to_read_gfs_file(date_ini,date_iforc,fdate,delta_t)
                    #y = str(date_ini.year)
                    #m = [str(date_ini.month) if len(str(date_ini.month))>1 else '0'+str(date_ini.month)][0]
                    #d = [str(date_ini.day) if len(str(date_ini.day))>1 else '0'+str(date_ini.day)][0]
                    #h = [str(date_ini.hour) if len(str(date_ini.hour))>1 else '0'+str(date_ini.hour)][0]
                    #if len(str(delta_t))==1:
                    #    fcst_hr = '00'+str(delta_t)
                    #if len(str(delta_t))==2:
                    #    fcst_hr = '0'+str(delta_t)
                    #if len(str(delta_t))==3:
                    #    fcst_hr = str(delta_t)
                    print(y,m,d,h,fcst_hr)
                    file_gfs = 'gfs_global_' +y+m+d+h+ '_f' + fcst_hr + '.nc'
                    tmp_gfs_nc = 'gfs_global_' +y+m+d+h+ '_f' + fcst_hr + '_' + hafs_fcst_hr + '_' + file_var_name + '.nc'

                    # Extracting gfs_var_name from file_gfs
                    cmd = 'ncks -v ' + gfs_var_name + ' ' + file_gfs + ' ' + tmp_gfs_nc
                    print(cmd)
                    os.system(cmd)

                    print('gfs_var_name ', gfs_var_name)
                    # Open ncfile
                    ncfile = nc.Dataset(tmp_gfs_nc,'a')
                    # Add calendar type to time variable
                    ncfile['time'][:] = shifted_timestamp
                    ncfile.variables['time'].short_name = "time"
                    ncfile.variables['time'].long_name = "time"
                    ncfile.variables['time'].calendar = "gregorian"
                    ncfile.variables['time'].reference_time_description = " "
                    ncfile.variables['time'].units = "seconds since 1970-01-01 00:00:00"
                    ncfile.close()
                    # This is the string that contains all netcdf files that need to be concatenated
                    files_nc = files_nc + ' ' + tmp_gfs_nc

                print('Concatenating netcdf files for different forecast times')
                cmd = 'ncrcat ' + files_nc + ' gfs_global_' +y+m+d+h+ '_' + file_var_name + '.nc'
                print(cmd)
                os.system(cmd)
                os.system('rm ' + 'gfs_global_*f*' + file_var_name + '.nc')

