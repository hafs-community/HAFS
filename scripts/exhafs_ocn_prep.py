#! /usr/bin/env python3
################################################################################
# Script Name: exhafs_ocn_prep.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS oceanic preprocessing steps to generate HYCOM
#   coupling needed ocean initial condition (IC), open boundary condition (OBC)
#   and atmospheric forcings.
################################################################################
import os, sys, logging

if 'USHhafs' in os.environ:
    sys.path.append(os.environ['USHhafs'])
elif 'HOMEhafs' in os.environ:
    sys.path.append(os.path.join(os.environ['HOMEhafs'],'ush'))
else:
    guess_HOMEhafs=os.path.dirname(os.path.dirname(
            os.path.realpath(__file__)))
    guess_USHhafs=os.path.join(guess_HOMEhafs,'ush')
    sys.path.append(guess_USHhafs)

import produtil.setup, produtil.datastore, produtil.fileop
from produtil.datastore import Datastore
from produtil.fileop import deliver_file, remove_file
from produtil.ecflow import set_ecflow_event
import hafs.launcher, hafs.config, hafs.hycom

produtil.setup.setup()

environ_CONFhafs=os.environ.get('CONFhafs','NO_CONFhafs')
#conf=hafs.launcher.HAFSLauncher().read(environ_CONFhafs)
conf=hafs.launcher.load(environ_CONFhafs)

logger=conf.log('hycominit')
logger.info("hycominit1 started")

DATA=os.environ.get('DATA',conf.getloc('WORKhafs','.')+"/ocn_prep")
fcstlen=conf.getint('config','NHRS',126)
os.environ['MPISERIAL'] = conf.getloc('MPISERIAL','NONE')
os.environ['mpiserial'] = conf.getloc('mpiserial','NONE')

filename=DATA+"/hycominit1_state.sqlite3"
remove_file(filename)
ds=Datastore(filename,logger=logger)

hycominit1workdir=DATA+"/hycominit1"
hycominit1=hafs.hycom.HYCOMInit1(dstore=ds,conf=conf,section='hycominit1',taskname='hycominit1',workdir=hycominit1workdir,fcstlen=fcstlen)

try:
    hycominit1.run()
except:
    logger.critical("FATAL ERROR: hycominit1 failed")
    sys.exit(2)

logger.info("hycominit1 done")

logger.info("hycominit2 started")

filename=DATA+"/hycominit2_state.sqlite3"
remove_file(filename)
ds=Datastore(filename,logger=logger)

hycominit2workdir=DATA+"/hycominit2"
hycominit2=hafs.hycom.HYCOMInit2(dstore=ds,conf=conf,section='hycominit2',taskname='hycominit2',workdir=hycominit2workdir,fcstlen=fcstlen)

try:
    hycominit2.run()
except:
    logger.critical("FATAL ERROR: hycominit2 failed")
    sys.exit(2)

set_ecflow_event('Ocean',logger=logger)

logger.info("hycominit2 done")
