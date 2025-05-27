#! /usr/bin/env python3
################################################################################
# Script Name: exhafs_ocn_post.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS oceanic post-processing steps for HYCOM coupling.
# History:
#      01/2020: Initial version from HWRF/HMON for HAFS HYCOM ocean coupling
#   04/17/2021: Generalize script name for HAFS application/workflow
#   03/20/2023: Finalize for HAFSv1 operational implementation
#   04/19/2024: Improve error handling for HAFSv2
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
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
import hafs.launcher, hafs.config, hafs.hycom

produtil.setup.setup()

environ_CONFhafs=os.environ.get('CONFhafs','NO_CONFhafs')
#conf=hafs.launcher.HAFSLauncher().read(environ_CONFhafs)
conf=hafs.launcher.load(environ_CONFhafs)

logger=conf.log('hycompost')
logger.info("hycompost started")

DATA=os.environ.get('DATA',conf.getloc('WORKhafs','.')+"/ocn_post")
fcstlen=conf.getint('config','NHRS',126)

filename=DATA+"/hycompost_state.sqlite3"
remove_file(filename)
ds=Datastore(filename,logger=logger)

hycompostworkdir=DATA+"/hycompost"
hycompost=hafs.hycom.HYCOMPost(dstore=ds,conf=conf,section='hycompost',workdir=hycompostworkdir,fcstlen=fcstlen)
try:
    hycompost.run()
except:
    logger.critical("FATAL ERROR: hycompost failed")
    sys.exit(2)

logger.info("hycompost done")
