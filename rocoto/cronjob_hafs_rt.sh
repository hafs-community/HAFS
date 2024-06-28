#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no config.archive=none"

#===============================================================================
 # HAFS application regression tests for operational configurations using development computation resources

 # HFSA with two-way atm-ocn coupling and one-way atm-wav coupling
  ./run_hafs.py ${opts} 2020082506-2020082512 13L HISTORY \
      config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_hfsa_dev_ww3 \
      config.NHRS=12 ${scrubopt} \
      ../parm/hfsa_dev_ww3.conf

 # HFSB with two-way atm-ocn coupling
 ./run_hafs.py ${opts} 2020082506-2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_hfsb_dev \
     config.NHRS=12 ${scrubopt} \
     ../parm/hfsb_dev.conf
 
#===============================================================================
 # HAFS application regression tests for experimental regional configurations

 # Regional standalone storm-focused configuration with ESG grid and GFS grib2ab input
 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_atm \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_regional_atm.conf

 # Regional standalone storm-focused configuration with ESG grid and atm-ocn-wav coupling
 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_atm_ocn_wav \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_regional_atm_ocn_wav.conf

 # Regional standalone low-resolution static basin-focused configuration with atm-ocn-wav coupling
 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_C192s1n4_atm_ocn_wav \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_regional_static_C192s1n4_atm_ocn_wav.conf

 # Regional standalone low-resolution static basin-focused configuration with 3DEnVar using GDAS ensembles
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_C192s1n4_atm_3denvar \
     config.NHRS=12 ${scrubopt} \
     ../parm/rt_conf/hafs_regional_static_C192s1n4_atm_3denvar.conf

#===============================================================================
 # HAFS application regression tests for experimental global-nesting configurations

 # Global-nesting storm-focused configuration
 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_atm \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_globnest_atm.conf

 # Global-nesting static basin-focused configuration with GFS grib2ab input
 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_static_atm \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_globnest_static_atm.conf

#===============================================================================

date

echo 'cronjob done'
