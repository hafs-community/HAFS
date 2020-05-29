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

logger=conf.log('hycominit1')
logger.info("hycominit1 started")

fcstlen=conf.getint('config','NHRS',126)
filename=conf.getloc('WORKhafs','NONE')+"/hycominit1_state.sqlite3"
remove_file(filename)
ds=Datastore(filename,logger=logger)

hycominit1=hafs.hycom.HYCOMInit1(dstore=ds,conf=conf,section='hycominit1',taskname='hycominit1',fcstlen=fcstlen)
hycominit1.run()

logger.info("hycominit1 done")
