#! /bin/bash
set -ue

HOMEhafs=$( cd $( dirname ${BASH_SOURCE[0]} ) ; cd .. ; pwd -P )
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc
set -x

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
SUBEXPT=pygrafC192no
conf=../parm/WaterInTheWestC192.conf
opts="-t -f"

# To enable graphics, uncomment this. But only on Hera!
# conf="$conf config.run_pygraf=yes"

scrubopt="config.scrub_work=no config.scrub_com=no"
 # 3DEnVar with GDAS ensembles
 ./run_hafs.py ${opts} 2023022400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT} \
     config.NHRS=24 ${scrubopt} \
     $conf config.run_pygraf=yes
