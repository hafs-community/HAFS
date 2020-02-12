#! /usr/bin/env python
# need to load "module load intel mvapich2" before running this script
import sys,datetime

nmmbroot="/scratch3/NCEPDEV/hwrf/save/Keqin.Wu/HyHWRF16wpac"
hwrfush=nmmbroot+"/ush"
#hwrfexec=nmmbroot+"/exec"
sys.path.insert(0, hwrfush)
import hwrf.hycom
from produtil.fileop import chdir

spinstart=(2015,10,22,12,0,0)
cyc=(2015,10,27,18,0,0)
#convert to YYYY-MM-DD HH:MM:SS
spinstart =  str(spinstart[0])+"-"+str(spinstart[1])+"-"+str(spinstart[2])+" "+str(spinstart[3])+":"+str(spinstart[4])+":"+str(spinstart[5])
cyc = str(cyc[0])+"-"+str(cyc[1])+"-"+str(cyc[2])+" "+str(cyc[3])+":"+str(cyc[4])+":"+str(cyc[5])
print spinstart
print cyc

#cd to Working directory _WORKDIR_
chdir("/scratch3/NCEPDEV/hwrf/scrub/Keqin.Wu/HNMMB/2015102012_test/20E")
hycom_epoch=datetime.datetime(1900,12,31,0,0,0)
print hycom_epoch
with open('limits','wt') as limitf:
	limitf.write('  %f %f false false  \n'%(
        float(hwrf.hycom.date_normal2hycom(spinstart)),
        float(hwrf.hycom.date_normal2hycom(cyc))))
