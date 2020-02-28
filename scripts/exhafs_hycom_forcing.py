#! /usr/bin/env python
import os, sys, stat, glob, subprocess, shutil, getpass, fileinput
import logging,sys

USER=getpass.getuser()

import produtil.datastore, produtil.fileop, produtil.config, produtil.setup
from produtil.datastore import Datastore,TASK_CATEGORY,UNSTARTED,COMPLETED
from produtil.fileop import deliver_file, remove_file
import produtil.setup, produtil.log, produtil.dbnalert
import hwrf.launcher
import hwrf_expt
import hafs.launcher
from hwrf.numerics import to_datetime
from produtil.ecflow import set_ecflow_event
import hwrf.hycom, hwrf.config
import nmmbUsh
from nmmbUsh import logger

produtil.setup.setup()

# Long storm ID:
longstormid="{vit[longstormid]}"
'''  # Don't forget the end of line before the '''

def usage(logger):
    logger.critical('Invalid arguments to exhwrf_launch.py.  Aborting.')
    print '''
Usage: exhwrf_launch.py 2014062400 95E case_root /path/to/parm [options]
#  python test.py 2016093018 14L FORECAST /mnt/lfs3/projects/hwrfv3/Dan.Iredell/HyHWRF16wpac/parm > testrun.txt
#  ./run_hwrf.py -f -s sites/ujet-res-hyc2.ent 2016 14E FORECAST 'config.EXPT=HyHWRF16wpac' ../parm/hyhmon.conf 'config.run_gsi=no' 'config.allow_fallbacks=yes'
# python test.py 2016093018 14L FORECAST 'config.EXPT=HyHWRF16wpac' ../../parm/hyhmon.conf 'config.run_gsi=no' 'config.allow_fallbacks=yes'

Mandatory arguments:
  2014062400 -- the cycle to run
  95E -- storm id
  case_root -- FORECAST = real-time mode, HISTORY = retrospective mod
  /path/to/parm -- location of parm directory where standard conf files
      reside

Optional arguments:
section.option=value -- override conf options on the command line
/path/to/file.conf -- additional conf files to parse

Aborting due to incorrect arguments.'''
    sys.exit(2)

logger.info("hycominit2 started")

environ_CONFhafs=os.environ.get('CONFhafs','NO_CONFhafs')
#conf=hafs.launcher.HAFSLauncher().read(environ_CONFhafs)
conf_hafs=hafs.launcher.load(environ_CONFhafs)

#parmdir=os.environ.get('PARMhmon')
#workdir=os.environ.get('WORKhmon')

parmdir=conf_hafs.getloc('PARMhafs','NONE')
workdir=conf_hafs.getloc('WORKhafs','NONE')


conf=produtil.config.from_file(parmdir + "/hafs.conf")
#conf.cycle=os.environ.get('CYCLE')
conf.cycle=conf_hafs.getloc('cycle','NONE')
#expname=conf.get('config','expname')
expname=conf_hafs.getloc('EXPT','NONE')


#with open(workdir + "/get_storm_info/storm_info", 'r') as f:
#    for line in f:
#        if "START_DATE=" in line:
#           START_DATE = line.replace("START_DATE=", "")
#           cycle = START_DATE.strip('\n')
#        if "STORM_ID=" in line:
#           STORM_ID = line.replace("STORM_ID=","")
#           stormid = STORM_ID.strip('\n')

cycle=conf_hafs.getloc('cycle','NONE')
stormid=conf_hafs.getloc('STID','NONE')

os.environ['DATA'] = conf_hafs.getloc('WORKhafs','NONE') 
os.environ['COMINarch'] = conf_hafs.getloc('syndat','NONE')
os.environ['COMOUT'] = conf_hafs.getloc('com','NONE')
os.environ['HISTDATA'] = conf_hafs.getloc('oldcom','NONE')
os.environ['HOMEhmon'] = conf_hafs.getloc('HOMEhafs','NONE')
os.environ['COMINrtofs'] = conf_hafs.getloc('COMrtofs','NONE')
os.environ['COMINgfs'] = conf_hafs.getloc('COMgfs','NONE')
os.environ['MPISERIAL'] = conf_hafs.getloc('MPISERIAL','NONE') 

args=[cycle,stormid,'FORECAST',parmdir + '/hyhafs.conf','config.EXPT='+expname,'config.SUBEXPT='+workdir,'config.run_gsi=no','config.allow_fallbacks=yes']

(case_root,parm,infiles,stid,moreopt) = \
    hwrf.launcher.parse_launch_args(args[1:],logger,usage,parmdir)

fake_stid=None
fakestorm_conf = None
stids = [stid]
cycle=to_datetime(args[0])
moreopts = [moreopt]
logger.info("args[0:],usage")
logger.info(args[0:])
logger.info(usage)
logger.info('Requested storm %s cycle %s case root %s'
    %(stid,cycle.strftime('%Y%m%d%H'),case_root))

filename=workdir+"/hycominit2_state.sqlite3"
remove_file(filename)

global_storm_num = 2
for i,stid in enumerate(stids): 
    if stid != fake_stid:
        logger.info("infiles", infiles)
        logger.info("cycle", cycle)
        logger.info("stid", stid)
        logger.info("moreopts[i]", moreopts[i])
        logger.info("case_root", case_root)
        conf=hwrf.launcher.launch(infiles,cycle,stid,moreopts[i],case_root,
            init_dirs=True,prelaunch=hwrf_expt.prelaunch,
            fakestorm_conf=fakestorm_conf,
            storm_num=global_storm_num)

ds=Datastore(filename,logger=logging.getLogger())#,locking=not readonly)
X = hwrf.hycom.HYCOMInit2(ds,conf,'hycominit2')
X.run()

