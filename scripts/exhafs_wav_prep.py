#! /usr/bin/env python3
################################################################################
# Script Name: exhafs_wav_prep.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS wave preprocessing steps to generate WW3 coupling
#   needed wave grid, initial condition (IC), boundary condition (BC) and other
#   input forcing files.
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
import hafs.launcher, hafs.config, hafs.ww3

produtil.setup.setup()

environ_CONFhafs=os.environ.get('CONFhafs','NO_CONFhafs')
#conf=hafs.launcher.HAFSLauncher().read(environ_CONFhafs)
conf=hafs.launcher.load(environ_CONFhafs)

logger=conf.log('ww3init')
logger.info("ww3init started")

if not conf.getbool('config','run_wave'):
    logger.info('Wave is disabled. This job need not be run.')
    sys.exit(0)

wave_model=conf.getstr('config','wave_model')
if not wave_model.lower()=='ww3':
    logger.critical('FATAL ERROR: Config file error: unsupported wave model '
                     '%s.'%(repr(wave_model),))
    sys.exit(2)

DATA=os.environ.get('DATA',conf.getloc('WORKhafs','.')+"/wav_prep")
fcstlen=conf.getint('config','NHRS',126)
os.environ['MPISERIAL'] = conf.getloc('MPISERIAL','NONE')
os.environ['mpiserial'] = conf.getloc('mpiserial','NONE')

filename=DATA+"/ww3init_state.sqlite3"
remove_file(filename)
ds=Datastore(filename,logger=logger)

ww3initworkdir=DATA+"/ww3init"
ww3init=hafs.ww3.WW3Init(dstore=ds,conf=conf,section='ww3init',taskname='ww3init',workdir=ww3initworkdir,fcstlen=fcstlen)
try:
    ww3init.run()
except:
    logger.critical("FATAL ERROR: ww3init failed")
    sys.exit(2)

set_ecflow_event('Wave',logger=logger)

logger.info("ww3init done")
