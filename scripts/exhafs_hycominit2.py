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

logger=conf.log('hycominit2')
logger.info("hycominit2 started")

os.environ['MPISERIAL'] = conf.getloc('MPISERIAL','NONE') 
os.environ['mpiserial'] = conf.getloc('mpiserial','NONE') 

filename=conf.getloc('WORKhafs','NONE')+"/hycominit2_state.sqlite3"
remove_file(filename)
ds=Datastore(filename,logger=logger)

hycominit2=hafs.hycom.HYCOMInit2(dstore=ds,conf=conf,section='hycominit2',taskname='hycominit2')
hycominit2.run()

logger.info("hycominit2 done")
