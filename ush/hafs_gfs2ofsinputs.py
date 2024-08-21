#! /usr/bin/env python3
################################################################################
# Script Name: hafs_gfs2ofsinputs.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script generates atmospheric forcing files needed by HYCOM coupling.
# History:
#   05/22/2020: Originally adopted from HWRF/HMON and updated for HAFS.
################################################################################
import os, sys, logging
import produtil.setup
import produtil.fileop
import produtil.datastore
from produtil.fileop import remove_file, make_symlink, deliver_file
from produtil.run import *
import time
produtil.setup.setup(send_dbn=False)

logger=logging.getLogger('gfs2ofsinputs')

# inputs to this script:
# mode - anal or fcst
# flxfile - name of file to be manipulated (without any suffix)
# wgrib2 location
# grb2index location
#
# example
# ./gfs2ofsinputs.py fcst 2016080221.sfcflx /scratch3/NCEPDEV/hwrf/save/Dan.Iredell/HNMMB/exec/hwrf_wgrib2 /scratch3/NCEPDEV/hwrf/save/Dan.Iredell/HNMMB/nwport/util/exec/grb2index

mode=sys.argv[1]
flxfile=sys.argv[2]
wgrib2=alias(exe(sys.argv[3]))
grb2index=alias(exe(sys.argv[4]))

print("inputs are", mode, flxfile, wgrib2, grb2index)

if mode=='anal':
   TYPEx='hour fcst'
else:
   TYPEx='ave'

fields=[
      {"FLUX":'UFLX',   "LEVEL":'surface',           "TYPE":'ave'   },
      {"FLUX":"VFLX",   "LEVEL":"surface",           "TYPE":"ave"   },
      {"FLUX":"TMP",    "LEVEL":'2 m above ground',  "TYPE":"fcst"  },
      {"FLUX":"SPFH",   "LEVEL":'2 m above ground',  "TYPE":"fcst"  },
      {"FLUX":"PRATE",  "LEVEL":"surface",           "TYPE":"ave"   },
      {"FLUX":"UGRD",   "LEVEL":'10 m above ground', "TYPE":"fcst"  },
      {"FLUX":"VGRD",   "LEVEL":'10 m above ground', "TYPE":"fcst"  },
      {"FLUX":"SHTFL",  "LEVEL":"surface",           "TYPE":TYPEx   },
      {"FLUX":"LHTFL",  "LEVEL":"surface",           "TYPE":TYPEx   },
      {"FLUX":"DLWRF",  "LEVEL":"surface",           "TYPE":"ave"   },
      {"FLUX":"ULWRF",  "LEVEL":"surface",           "TYPE":"ave"   },
      {"FLUX":"DSWRF",  "LEVEL":"surface",           "TYPE":"ave"   },
      {"FLUX":"USWRF",  "LEVEL":"surface",           "TYPE":"ave"   },
      {"FLUX":"TMP",    "LEVEL":"surface",           "TYPE":"fcst"  },
      {"FLUX":"PRES",   "LEVEL":"surface",           "TYPE":"fcst"  },
      {"FLUX":"LAND",   "LEVEL":"surface",           "TYPE":"fcst"  }
      ]

remove_file(flxfile+',in3',info=True,logger=logger)
remove_file(flxfile+',in4',info=True,logger=logger)

# Subset flux file:
sfindex=runstr(wgrib2[flxfile+'.in2'],logger=logger)
reindex=''
for flt in fields:
    for line in sfindex.splitlines():
        if line.find(flt['FLUX']) >=0 and \
           line.find(flt['LEVEL']) >=0 and \
           line.find(flt['TYPE']) >=0:
            reindex+=line+'\n'
            logger.info('%s: keep(sf): %s'%(
                    flxfile+'.in2',line.strip()))
logger.info('KEEP(SF):\n'+reindex)
checkrun(wgrib2[flxfile+'.in2',"-i",'-grib',flxfile+'.in3']
         << reindex,logger=logger)

checkrun(wgrib2[flxfile+'.in3',"-new_grid_winds","earth",
   "-new_grid","gaussian","0:1440:0.25","89.75:720",flxfile+'.in4']
   ,logger=logger)
#os.rename(flxfile+'.in4',flxfile) # Before flxfile+'.in4' written, rename executing, throws error on WCOSS2(BT)
#Replacing os.rename with  produtil.fileop.deliver_file(BT)
deliver_file(flxfile+'.in4',flxfile,keep=True,logger=logger)
checkrun(grb2index[flxfile,flxfile+'.idx']) # Giving a non zero exit status(not sure exit status reliable?)
#run(grb2index[flxfile,flxfile+'.idx'])

