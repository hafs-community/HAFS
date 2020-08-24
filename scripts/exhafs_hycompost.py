#! /usr/bin/env python3

import os, sys, logging
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

fcstlen=conf.getint('config','NHRS',126)
filename=conf.getloc('WORKhafs','NONE')+"/hycompost_state.sqlite3"
remove_file(filename)
ds=Datastore(filename,logger=logger)

hycompost=hafs.hycom.HYCOMPost(dstore=ds,conf=conf,section='hycompost',fcstlen=fcstlen)
hycompost.run()

logger.info("hycompost done")
