#! /usr/bin/env python
import os, sys, stat, glob, subprocess, shutil, getpass, fileinput
import logging,sys

USER=getpass.getuser()
nmmbroot="/scratch3/NCEPDEV/hwrf/save/Dan.Iredell/HyHWRF16wpac"
hwrfush=nmmbroot+"/ush"
hwrfexec=nmmbroot+"/exec"
sys.path.insert(0, hwrfush)
sys.path.append(hwrfexec)
os.environ["PATH"] += os.pathsep + hwrfexec
dirlist=os.environ['PATH'].split(':')
print dirlist
import produtil.datastore, produtil.fileop
from produtil.datastore import Datastore,TASK_CATEGORY,UNSTARTED,COMPLETED
from produtil.fileop import deliver_file, remove_file
import produtil.setup, produtil.log, produtil.dbnalert
import hwrf.launcher
import hwrf_expt
from hwrf.numerics import to_datetime
from produtil.ecflow import set_ecflow_event
import hwrf.hycom, hwrf.config

#LOGFILE_OUT="test.out"

#logging.basicConfig(filename=LOGFILE_OUT,
#                    level=logging.DEBUG,
#                    )
#logger=logging.getLogger(LOGFILE_OUT)
#logger.info("test started")

# Long storm ID:
longstormid="{vit[longstormid]}"
'''  # Don't forget the end of line before the '''

def usage(logger):
    logger.critical('Invalid arguments to exhwrf_launch.py.  Aborting.')
    print '''
Usage: exhwrf_launch.py 2014062400 95E case_root /path/to/parm [options]
#  python test.py 2016093018 14L FORECAST /mnt/lfs3/projects/hwrfv3/Dan.Iredell/HyHWRF16wpac/parm > testrun.txt
#  ./run_hwrf.py -f -s sites/ujet-res-hyc2.ent 2016 14E FORECAST 'config.EXPT=HyHWRF16wpac' ../parm/hyhwrf.conf 'config.run_gsi=no' 'config.allow_fallbacks=yes'
# python test.py 2016093018 14L FORECAST 'config.EXPT=HyHWRF16wpac' ../../parm/hyhwrf.conf 'config.run_gsi=no' 'config.allow_fallbacks=yes'

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


os.environ["TOTAL_TASKS"] = "90"
#os.environ['FIXhwrf']="/scratch3/NCEPDEV/hwrf/save/Keqin.Wu/HyHWRF16wpac/fix"
#os.environ['WORKhwrf']="/scratch3/NCEPDEV/hwrf/save/Keqin.Wu/pytmp/HyHWRF16wpac"

logger=None
LOGFILE_OUT="out.launch"+sys.argv[1]
logging.basicConfig(filename=LOGFILE_OUT,
                    level=logging.DEBUG,
                    )
logger=logging.getLogger(LOGFILE_OUT)
logger.info("test started")

#logger=logging.getLogger('exhwrf_launch')
args=sys.argv[1:]

(case_root,parm,infiles,stid,moreopt) = \
            hwrf.launcher.parse_launch_args(args[1:],logger,usage,"/scratch3/NCEPDEV/hwrf/save/Dan.Iredell/HyHWRF16wpac/parm")

fake_stid=None
fakestorm_conf = None
stids = [stid]
cycle=to_datetime(args[0])
moreopts = [moreopt]
print "args[1:],logger,usage"
print args[1:]
print logger
print usage
logger.info('Requested storm %s cycle %s case root %s'
	%(stid,cycle.strftime('%Y%m%d%H'),case_root))

filename="wrf_state.sqlite3"
remove_file(filename)

global_storm_num = 2
for i,stid in enumerate(stids):
	if stid != fake_stid:
            print "infiles", infiles
            print "cycle", cycle
            print "stid", stid
            print "moreopts[i]", moreopts[i]
            print "case_root", case_root
            conf=hwrf.launcher.launch(infiles,cycle,stid,moreopts[i],case_root,
                                      prelaunch=hwrf_expt.prelaunch,
                                      fakestorm_conf=fakestorm_conf,
                                      storm_num=global_storm_num)

#dstore,conf,section
#conf=hwrf.config.HWRFConfig(None)
#conf.add_section('hycominit')
#for var in ( 'WORKhwrf', 'HOMEhwrf', 'com' ):
#       #expand=conf.getstr('dir',var)  
#       #logger.info('Replace [dir] %s with %s'%(var,expand))
#       conf.set('dir',var,expand)
#print "PARMhycom", os.environ['PARMhycom']


ds=Datastore(filename,logger=logging.getLogger())#,locking=not readonly)
X = hwrf.hycom.HYCOMInit(ds,conf,'hycominit')
X.run()

