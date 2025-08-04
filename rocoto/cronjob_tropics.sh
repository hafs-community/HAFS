#!/bin/sh
set -x
date

HOMEhafs=/scratch3/HFIP/hwrfv3/save/Xu.Lu/HAFS_TROPICS
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no config.archive=none"

#===============================================================================
 # HAFS application regression tests for operational configurations using development computation resources

 # HFSA with two-way atm-ocn coupling and one-way atm-wav coupling
# ./run_hafs.py ${opts} 2023090100-2023090200 10L HISTORY \
#      config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_hfsa_dev \
#      config.NHRS=126 ${scrubopt} config.scrub_work=no config.scrub_com=no \
#      ../parm/hfsa_dev.conf
 confopts="config.EXPT=${EXPT} config.SUBEXPT=TROPIC_Test ../parm/hafsv2p1p1a_h40ensda.conf "
./run_hafs.py ${opts} 2025062518-2025062518 88L HISTORY ${confopts} \
   config.NHRS=12 config.scrub_work=no config.scrub_com=no dir.syndat=/scratch4/HFIP/hwrfv3/save/Xu.Lu/hafsv211_suas/testsyndat

 # HFSB with two-way atm-ocn coupling
# ./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_hfsb_dev \
#     config.NHRS=12 ${scrubopt} \
#     ../parm/hfsb_dev.conf
 
#===============================================================================
 # HAFS application regression tests for experimental regional configurations

 # Regional standalone storm-focused configuration with ESG grid and GFS grib2ab input

date

echo 'cronjob done'
