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
hycominit1.run()

logger.info("hycominit1 done")

logger.info("hycominit2 started")

filename=DATA+"/hycominit2_state.sqlite3"
remove_file(filename)
ds=Datastore(filename,logger=logger)

hycominit2workdir=DATA+"/hycominit2"
hycominit2=hafs.hycom.HYCOMInit2(dstore=ds,conf=conf,section='hycominit2',taskname='hycominit2',workdir=hycominit2workdir,fcstlen=fcstlen)
hycominit2.run()

logger.info("hycominit2 done")
