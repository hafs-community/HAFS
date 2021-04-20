#! /usr/bin/env python3

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
hycompost.run()

logger.info("hycompost done")
