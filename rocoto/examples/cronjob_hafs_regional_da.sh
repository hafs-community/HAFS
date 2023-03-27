#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

 # Cold-start from GFS analysis without DA
  ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_noda \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_regional_da_C96s1n4_320x312.conf

 # Warm-start from prior HAFS forecast without DA
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_noda_cycling \
     config.warm_start_opt=2 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_regional_da_C96s1n4_320x312.conf

 # Simple 3DVar DA
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3dvar \
     config.run_gsi=yes config.run_envar=no \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with GDAS ensembles
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3denvar \
     config.run_gsi=yes config.run_envar=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_regional_da_C96s1n4_320x312.conf

  # 3DEnVar with GDAS ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3denvar_fgat \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=no config.ENS_SIZE=0 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with HAFS ensembles (cold-start from GDAS ensembles)
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda \
     config.run_gsi=yes config.run_fgat=no config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with HAFS ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda_fgat \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with self-cycled HAFS enkf ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda_enkf_fgat \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_regional_da_C96s1n4_320x312.conf

#===============================================================================

date

echo 'cronjob done'
