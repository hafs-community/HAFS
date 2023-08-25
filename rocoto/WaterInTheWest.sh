#! /bin/bash
set -ue

HOMEhafs=$( cd $( dirname ${BASH_SOURCE[0]} ) ; cd .. ; pwd -P )
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc
set -x

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
SUBEXPT=C96
conf=../parm/WaterInTheWestC96.conf
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"
 # 3DEnVar with GDAS ensembles
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT} \
     config.NHRS=12 ${scrubopt} \
     $conf
