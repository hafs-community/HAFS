#! /usr/bin/env python3

# This next line will abort in any version earlier than Python 3.6:
f'This script requires Python 3.6 or newer.'

import time
import subprocess
import contextlib
import os
import tempfile
import getopt
import re
import logging
import datetime
import sys
import cdsapi
import produtil.setup, produtil.fileop, produtil.locking

# Constants
UTILITY_NAME = 'era5_downloader'
VERSION_STRING = '0.0.1'
LOGGING_DOMAIN = UTILITY_NAME
DATASET = 'reanalysis-era5-single-levels'
PRODUCT_TYPE = 'reanalysis'
VARIABLES = [
    '10m_u_component_of_wind', '10m_v_component_of_wind', '2m_dewpoint_temperature',
    '2m_temperature', 'convective_precipitation', 'convective_snowfall',
    'large_scale_precipitation', 'large_scale_snowfall', 'mean_sea_level_pressure',
    'near_ir_albedo_for_diffuse_radiation', 'near_ir_albedo_for_direct_radiation',
    'uv_visible_albedo_for_diffuse_radiation', 'uv_visible_albedo_for_direct_radiation',
    'surface_latent_heat_flux', 'surface_sensible_heat_flux', 
    'surface_solar_radiation_downwards', 'surface_thermal_radiation_downwards',
    'surface_pressure', 'total_precipitation', 'skin_temperature',
    'eastward_turbulent_surface_stress', 'northward_turbulent_surface_stress',
    'surface_net_solar_radiation', 'surface_net_thermal_radiation'
]
FILE_FORMAT = 'netcdf'
CYCLING_INTERVAL = datetime.timedelta(seconds=3600*24)
EPSILON = datetime.timedelta(seconds=5)  # epsilon for time comparison: five seconds

# Non-constant globals:
dayset=set() # list of YYYYMMDD strings
happy=True # False = something failed
filename_format = 'ERA5_%Y%m%d'
swap_latitudes=True

def usage(why=None):
    print(f'''Synopsis: {UTILITY_NAME} [options] day [day [...]]

Downloads the listed days of data. Days can be specified as:
  20210815 = specify one day: August 15, 2021
  20210815-20210819 = specify a range of days: August 15th to 19th, 2021
  2018 = specify an entire year (2018)

Options:
  -q | --quiet = log only warnings and errors
  -v | --verbose = log all messages
  -n | --no-invertlat = do not run "cdo invertlat" on downloaded files
  -F format | --format format = filename format as in strftime(3)
  -i | --invertlat = DO run "cdo inverlat".  This is the default
  --version = print {UTILITY_NAME} {VERSION_STRING}
  --help = this message

Format example: ERA5_%Y%m%d = ERA5_20210815
Script will automatically append ".nc"
''')
    if why:
        sys.stderr.write(f'SCRIPT IS ABORTING BECAUSE: {why}\n')
        return 1
    return 0

# Function that makes the singleton for cdsapi client:
_client = None
def client():
    global _client
    if not _client:
        logger.info('creating cdsapi client')
        _client=cdsapi.Client()
    return _client

# Tell CDO to flip latitudes in a NetCDF file:
def cdo_swap_latitudes(filename_in,filename_out):
    logger.info('Flip latitudes in "'+str(filename_in)+'" and write to "'+str(filename_out)+'"')
    cmd = [ 'cdo', 'invertlat', filename_in, filename_out ]
    logger.info(f'''Run "{'" "'.join(cmd) }"''')
    result = subprocess.run(cmd)
    result.check_returncode()

def quiet_remove(filename):
    with contextlib.suppress(FileNotFoundError):
        os.remove(filename)

# The meat of the program: retrieve a file
def request(when):
    filename_base = when.strftime(filename_format)
    filename_download = filename_base+'_download.nc'
    filename_invert = filename_base+'_invert.nc'
    filename_lock = filename_base+'.lock'
    filename_final = filename_base+'.nc'
    if os.path.exists(filename_final):
        logger.info(filename_final+': already exists. Skipping.')
        return
    with produtil.locking.LockFile(filename_lock,logger):
        try:
            quiet_remove(filename_download)
            quiet_remove(filename_invert)
            logger.info(filename_download+': retrieve '+str(when)+'...')
            request = {
                'product_type': PRODUCT_TYPE,
                'variable': VARIABLES,
                'year': '%04d'%int(when.year),
                'month': [ '%02d'%int(when.month) ],
                'day': [ '%02d'%int(when.day) ],
                'time': [ '%02d'%hour for hour in range(24) ],
                'format': FILE_FORMAT,
            }
            # super-wordy debugging: logger.debug(filename_download+': request is '+str(request))
            client().retrieve(DATASET,request,filename_download)
            filename_copy=filename_download
            if swap_latitudes:
                cdo_swap_latitudes(filename_download,filename_invert)
                filename_copy=filename_invert
            produtil.fileop.deliver_file(filename_copy,filename_final,logger=logger,
                                         keep=False,verify=False,moveok=True,force=True)
            quiet_remove(filename_download)
            quiet_remove(filename_invert)
            quiet_remove(filename_lock)
        except Exception as e:
            quiet_remove(filename_download)
            quiet_remove(filename_invert)
            raise e

# Parse arguments and initialize logging:
log_level = logging.INFO
optlist,args = getopt.getopt(sys.argv[1:],'qveniF:',[
    'version','help','verbose','quiet','invertlat','no-invertlat','format'])
if len(args)<1:
    exit(usage("No arguments provided!"))
for optarg in optlist:
    if optarg[0] in ['-q', '--quiet']:
        log_level = logging.WARNING
    elif optarg[0] in ['-v', '--verbose']:
        log_level = logging.DEBUG
    elif optarg[0] in ['-i', '--invertlat']:
        invertlat = True
    elif optarg[0] in ['-n', '--no-invertlat']:
        invertlat = False
    elif optarg[0] in ['-F', '--format']:
        filename_format = optarg[1]
    elif optarg[0]=='--help':
        exit(usage())
    elif optarg[0]=='--version':
        print(UTILITY_NAME+' '+VERSION_STRING)
        exit(0)
logger = logging.getLogger(LOGGING_DOMAIN)

produtil.setup.setup(level=log_level,send_dbn=False)

# Parse the days. This loop was modified from run_hafs.py:
for arg in args:
    if re.match('\A\d{8}\Z',arg):
        logger.info('single date/time')
        # Single date/time
        dayset.add(arg)
    elif re.match('\A\d{4}\Z',arg):
        logger.info('year')
        # Year
        start=datetime.datetime(int(arg,8),1,1,0,0,0)
        end=datetime.datetime(int(arg,8),12,31,23,59,0)
        now=start
        while now<end+EPSILON:
            dayset.add(now.strftime('%Y%m%d'))
            now+=CYCLING_INTERVAL
    elif re.match('\A\d{8}-\d{8}\Z',arg):
        # Range of date/times
        start=datetime.datetime.strptime(arg[0:8],'%Y%m%d')
        end=datetime.datetime.strptime(arg[9:],'%Y%m%d')
        now=start
        while now<end+EPSILON:
            dayset.add(now.strftime('%Y%m%d'))
            now+=CYCLING_INTERVAL
    else:
        logger.warning('Ignoring invalid argument "'+arg+'"')
        happy=False

# Sort the cycle list so we retrieve in order of increasing date:
daylist = list(dayset)
daylist.sort()
ndays = len(daylist)

if not daylist:
    logger.warning('Nothing to do! Exiting.')
    exit(1)

iloop = 0
while daylist:
    iloop += 1
    day = daylist.pop(0)

    # Turn the day string into a datetime.datetime:
    as_datetime = None
    try:
        as_datetime = datetime.datetime.strptime(day,'%Y%m%d')
    except ValueError as e:
        logger.warning(f'Ignoring invalid day "{day}"',exc_info=e)
        happy=False
        continue

    # Download the file:
    try:
        request(as_datetime)
    except produtil.locking.LockHeld:
        logger.info(f'{day}: lock is held; move on')
        daylist.append(day)
        if iloop>=len(daylist):
            logger.info(f'{day}: sleep for a little while... 30 second snooze...')
            time.sleep(30)
            logger.info(f'{day}: done sleeping.')
            iloop=0
    except Exception as ex: # Unfortunately, cdsapi raises Exception
        happy = False
        logger.error(f'CDSAPI failed to download day {day}: {ex}',exc_info=ex)

# Exit 0 on success, 1 on failure:
exit( 0 if happy else 1 )
