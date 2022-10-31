#!/usr/bin/env python3

# (C) Copyright 2019-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# in IPython:
# run gfs_forcing.py 20200701 -o atm_20200701.nc -t ../2020/20200701
#

import argparse
import collections
import datetime
from dateutil.parser import parse
import numpy
import os
import sys
import netCDF4 as nc
import pygrib
import shutil
import numpy as np

# these fields are obtained from 3 hr forecasts
inst_fields=[
    ("SLP", {'name':'Pressure reduced to MSL'}),
    ("U10", {'name':'10 metre U wind component'}),
    ("V10", {'name':'10 metre V wind component'}),
    ("T2",  {'name':'2 metre temperature'}),
    ("Q2",  {'name':'2 metre specific humidity', 'typeOfLevel':'heightAboveGround', 'level':2}),
]

# these fields are obtained from 6 hour forecasts ( averaged from 0-6, so valid at same time
#  as the above instantaneous fields)
flux_fields=[
    ("DSWRF", {'name':'Downward short-wave radiation flux'}),
    ("DLWRF", {'name':'Downward long-wave radiation flux'}),
    ("PRATE", {'name':'Precipitation rate', 'stepType':'avg'}),
]

# These sources should have the files needed for March 2004 to the present.
# If you need dates before this... find them yourself from somehwere else.
# (use CFS forcing instead??)
#src_urls=[
#    "https://ftp.ncep.noaa.gov/data/nccf/com/gfs/prod/"+
#      "gfs.{y}{m:02}{d:02}/{h:02}/gfs.t{h:02}z.pgrb2.1p00.f{f:003}",
#    "https://ncei.noaa.gov/data/global-forecast-system/access/grid-003-1.0-degree/forecast/"+
#      "{y}{m:02}/{y}{m:02}{d:02}/gfs_3_{y}{m:02}{d:02}_{h:02}00_{f:003}.grb2",
#    "https://ncei.noaa.gov/data/global-forecast-system/access/historical/analysis/"+
#      "{y}{m:02}/{y}{m:02}{d:02}/gfsanl_3_{y}{m:02}{d:02}_{h:02}00_{f:003}.grb2",
#    "https://ncei.noaa.gov/data/global-forecast-system/access/historical/analysis/"+
#      "{y}{m:02}/{y}{m:02}{d:02}/gfsanl_3_{y}{m:02}{d:02}_{h:02}00_{f:003}.grb" ]
#
#src_urls=["/work/noaa/marine/yli/soca-shared/DATA/FORCING/2022gfs/gfs.{y}{m:02}{d:02}/{h:02}/atmos/gfs.t{h:02}z.pgrb2.0p25.f{f:003}"]
src_urls=["/work/noaa/hwrf/noscrub/hafs-input/COMGFSv16/gfs.{y}{m:02}{d:02}/{h:02}/atmos/gfs.t{h:02}z.pgrb2.0p25.f{f:003}"]
#src_urls=["/work/noaa/hwrf/noscrub/hafs-input/COMGFS/gfs.{y}{m:02}{d:02}/{h:02}/gfs.t{h:02}z.pgrb2.0p25.f{f:003}"]
#src_urls=["/work/noaa/aoml-hafs1/hskang/gfs025_hpss/2020/gfs.{y}{m:02}{d:02}/{h:02}/gfs.t{h:02}z.pgrb2.0p25.f{f:003}"]
#src_urls=["/work/noaa/aoml-hafs1/hskang/gfs025_hpss/2020v16/gfs.{y}{m:02}{d:02}/{h:02}/gfs.t{h:02}z.pgrb2.0p25.f{f:003}"]

cpwd=os.getcwd()

def proc(date_s, date_e, output, tmp):

    print("Preparing forcing file to cover forcast period of")
    print(" start: ", date_s)
    print(" end:   ", date_e)
    print("")

    # get proper start date just before or on the fcst start
    sdate=date_s

    # setup temporary directory to store downloaded files
    os.makedirs(tmp, exist_ok=True)
    os.chdir(tmp)

    # data to save goes in these variables
    d_dates=[]
    d_lat=None
    d_lon=None
    d_fields=collections.defaultdict(lambda: [])
    d_meta=collections.defaultdict(lambda: {})

    # for each date to obtain:
    gfsinit=sdate-datetime.timedelta(hours=6)
    print(gfsinit)
    #sys.exit()

    ncdate=gfsinit+datetime.timedelta(hours=6)
    d_dates.append(ncdate)
    print(ncdate)

    for x in ( (inst_fields, 3), (flux_fields, 3)):
       fields = x[0]

       file_found = False
       for s in src_urls:
          gfsgrib=s.format(y=gfsinit.year, m=gfsinit.month, d=gfsinit.day, h=gfsinit.hour, f=6)
          if os.path.exists(gfsgrib):
             file_found = True
             print("Exists: ",gfsgrib)
          else:
             raise Exception("file was not found on disk")

          grbs = pygrib.open(gfsgrib)
          for f in fields:
             print(f)
             v_grbs = grbs.select(**f[1])
             if len(v_grbs) != 1:
                [print(g) for g in v_grbs]
                raise Exception("There should be excatly 1 grb field for \""+ f[0]+"\"")
             var=numpy.flipud(v_grbs[0].values)
             d_fields[f[0]].append(var[360:,1000:])
             d_meta[f[0]]['units'] = v_grbs[0]['units']
             d_meta[f[0]]['long_name'] = v_grbs[0]['name']

             if d_lat is None:
                d_lat, d_lon = v_grbs[0].latlons()
                d_lat = numpy.flipud(d_lat)
                d_lat = d_lat[360:,1000:]
                d_lon = d_lon[360:,1000:]

#    for fcst_hr in np.arange(0,54,3):
    gfsinit=sdate
    for fcst_hr in np.arange(3,132,3):
        ncdate=gfsinit+datetime.timedelta(hours=int(fcst_hr))
        d_dates.append(ncdate)
        print(ncdate)

        for x in ( (inst_fields, 3), (flux_fields, 3)):
            fields = x[0]

            file_found=False
            for s in src_urls:
                gfsgrib=s.format(y=gfsinit.year, m=gfsinit.month, d=gfsinit.day, h=gfsinit.hour, f=fcst_hr)
                if os.path.exists(gfsgrib):
                    file_found = True
                    print("Exists: ",gfsgrib)
                else:
                    raise Exception("file was not found on disk")

                grbs = pygrib.open(gfsgrib)
                for f in fields:
                    print(f)
                    if fcst_hr != 0 and (f[0] != flux_fields[0][0] or f[0] != flux_fields[1][0] or f[0] != flux_fields[2][0]):
                       v_grbs = grbs.select(**f[1])
                       if len(v_grbs) != 1:
                           [print(g) for g in v_grbs]
                           raise Exception("There should be excatly 1 grb field for \""+ f[0]+"\"")

                       var=numpy.flipud(v_grbs[0].values)
                       d_fields[f[0]].append(var[360:,1000:])
                       d_meta[f[0]]['units'] = v_grbs[0]['units']
                       d_meta[f[0]]['long_name'] = v_grbs[0]['name']

                       if d_lat is None:
                         d_lat, d_lon = v_grbs[0].latlons()
                         d_lat = numpy.flipud(d_lat)
                         d_lat = d_lat[360:,1000:]
                         d_lon = d_lon[360:,1000:]

                grbs.close()


        # done, next timeslot
        ncdate += datetime.timedelta(hours=6)

    # prepare the output file
    p = os.path.dirname(output)
    if p != '':
        os.makedirs(p, exist_ok=True)
    ncd = nc.Dataset(output, 'w', format="NETCDF4")
    ncd.createDimension('lon', d_lon.shape[1])
    ncd.createDimension('lat', d_lat.shape[0])
    ncd.createDimension('time', None)

    v = ncd.createVariable('lon', 'f4', ('lon',))
    v.standard_name = "longitude"
    v.long_name     = "longitude"
    v.units         = "degrees_east"
    v.axis          = "X"

    v = ncd.createVariable('lat', 'f4', ('lat',))
    v.standard_name = "latitude"
    v.long_name     = "latitude"
    v.units         = "degrees_north"
    v.axis          = "Y"

    basedate = datetime.datetime(1970, 1, 1, tzinfo=ncdate.tzinfo)
    v = ncd.createVariable('time', 'f8', ('time',))
    v.standard_name = "time"
    v.long_name     = "time"
    v.units         = "seconds since "+basedate.strftime("%Y-%m-%d %H:%M:%S")
    v.calendar      = "julian"

    for f in d_fields:
        v = ncd.createVariable(f, 'f4', ('time','lat','lon'), zlib=True, complevel=1)
        for k in d_meta[f]:
            v.setncattr(k, d_meta[f][k])

    # fill in values
    ncd.variables['lat'][:] = d_lat[:,0]
    ncd.variables['lon'][:] = d_lon[0,:]
    for f in d_fields:
        ncd.variables[f][:] = d_fields[f]
    ncd.variables['time'][:] = [ (t-basedate).total_seconds() for t in d_dates]
    ncd.close()


if __name__ == "__main__":
    # get command line args
    parser = argparse.ArgumentParser(
        description="Download the atmospheric surface fields needed to force the ocean model forecast."+
        " These files are pulled from ftp.ncep.noaa.gov if available, and if not the download fallsback"+
        " to nomads.ncdc.noaa.gov. Files are converted from grib2 to netcdf.")
    parser.add_argument('date', help="start date, in any valid date format")
    parser.add_argument('-l','--length', type=int, default=24,
        help="legnth of required forcing in hours (date + length). Default %(default)s")
    parser.add_argument('-o','--output', type=str, default=None,
        help="output filename (Default <date>.nc)")
    parser.add_argument('-t','--tmp', type=str, default="/tmp/prep_forc_gfs",
        help="location to temporarily store the downloaded grib2 files before"+
        " being converted. Default: %(default)s")
    args = parser.parse_args()
    #args.date=parse(args.date)
    args.date=datetime.datetime.strptime(args.date,'%Y%m%d%H')
    print(args.date)
    if args.output is None:
        args.output = args.date.strftime("%Y%m%d%H.nc")

    proc(date_s=args.date, date_e=args.date + datetime.timedelta(hours=args.length),
        output=args.output, tmp=args.tmp)


os.chdir(cpwd)
