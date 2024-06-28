#! /usr/bin/env python3
################################################################################
# Script Name: hafs_oisst_download.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script downloads OISST input data for CDEPS DOCN component.
################################################################################
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
import random

try:
    import requests
except ImportError as ie:
    sys.stderr.write("""You are missing the request module!
You must install it to run this script.

  pip install request --user
""")

import produtil.setup, produtil.fileop, produtil.locking

# Constants
UTILITY_NAME = 'hafs_oisst_download'
VERSION_STRING = '0.0.1'
LOGGING_DOMAIN = UTILITY_NAME
CYCLING_INTERVAL = datetime.timedelta(seconds=3600*24)
EPSILON = datetime.timedelta(seconds=5)  # epsilon for time comparison: five seconds

# Non-constant globals:
dayset=set() # list of YYYYMMDD strings
happy=True # False = something failed
filename_format = 'oisst-avhrr-v02r01.%Y%m%d'
base_url='https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr'
block_size=65536

def usage(why=None):
    print(f'''Synopsis: {UTILITY_NAME} [options] day [day [...]]

Downloads the listed days of data. Days can be specified as:
  20210815 = specify one day: August 15, 2021
  20210815-20210819 = specify a range of days: August 15th to 19th, 2021
  2018 = specify an entire year (2018)

Options:
  -q | --quiet = log only warnings and errors
  -v | --verbose = log all messages
  -u https:... | --url https:... = base url with no ending /
     default: {base_url}
  -F format | --format format = filename format as in strftime(3)
  -b N | --block-size N = bytes to download in each block (default {block_size})
  --version = print {UTILITY_NAME} {VERSION_STRING}
  --help = this message

Format example: stuffnthings_%Y%m%d = stuffnthings_20210815
Script will automatically append ".nc"
''')
    if why:
        sys.stderr.write(f'SCRIPT IS ABORTING BECAUSE: {why}\n')
        return 1
    return 0

def quiet_remove(filename):
    with contextlib.suppress(FileNotFoundError):
        os.remove(filename)

class RequestFailed(Exception):
    def __init__(self,url,code):
        self.url=str(url)
        self.code=code
    def __str__(self):
        return f'requests.get("{self.url!s}") failed with code {self.code!s}'
    def __repr__(self):
        return f'RequestFailed({self.url!r},{self.code!r})'

# The meat of the program: retrieve a file
def download_one_day(when):
    filename_base = when.strftime(filename_format)
    filename_final = filename_base+'.nc'
    if os.path.exists(filename_final):
        logger.info(filename_final+': already exists. Skipping.')
        return True

    filename_download = filename_base+'_download.nc'
    filename_lock = filename_base+'.lock'
    request = None
    yyyymm="%04d%02d"%(when.year,when.month)
    yyyymmdd="%04d%02d%02d"%(when.year,when.month,when.day)
    url=f'{base_url}/{yyyymm}/oisst-avhrr-v02r01.{yyyymmdd}.nc'
    with produtil.locking.LockFile(filename_lock,logger):
        try:
            if os.path.exists(filename_final):
                logger.info(filename_final+': already exists (after lock). Skipping. Will pick a random date to avoid lock contention..')
                return False
            with open(filename_download,'wb') as downloaded:
                logger.info(filename_download+' <-- '+str(url))
                request = requests.get(url)
                if request.status_code!=200:
                    raise RequestFailed(url,request.status_code)
                for chunk in request.iter_content(block_size):
                    downloaded.write(chunk)
                request.close()
            produtil.fileop.deliver_file(filename_download,filename_final,logger=logger,
                                         keep=False,verify=False,moveok=True,force=True)
            quiet_remove(filename_download)
            quiet_remove(filename_lock)
        except Exception as e:
            quiet_remove(filename_download)
            if request is not None:
                request.close()
            raise e
    return True

# Parse arguments and initialize logging:
log_level = logging.INFO
optlist,args = getopt.getopt(sys.argv[1:],'qveniu:b:F:',[
    'version','help','verbose','quiet','block-size','url','format'])
if len(args)<1:
    exit(usage("No arguments provided!"))
for optarg in optlist:
    if optarg[0] in ['-q', '--quiet']:
        log_level = logging.WARNING
    elif optarg[0] in ['-v', '--verbose']:
        log_level = logging.DEBUG
    elif optarg[0] in ['-F', '--format']:
        filename_format = optarg[1]
    elif optarg[0] in ['-u', '--url' ]:
        base_url=optarg[1]
    elif optarg[0] in ['-b', '--block-size' ]:
        block_size=max(1,int(optarg[1]))
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
        start=datetime.datetime(int(arg,10),1,1,0,0,0)
        end=datetime.datetime(int(arg,10),12,31,23,59,0)
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

pick_randomly = False
iloop = 0
while daylist:
    iloop += 1
    if pick_randomly:
        day = daylist.pop(min(len(daylist)-1,int(random.uniform(0,len(daylist)))))
        logger.info(f'Picked random date {day}')
    else:
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
        pick_randomly = not download_one_day(as_datetime)
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
        logger.error(f'Download failed for {day}: {ex}',exc_info=ex)

# Exit 0 on success, 1 on failure:
exit( 0 if happy else 1 )
