#! /bin/sh
set -xue

HOMEhafs=/scratch1/BMC/zrtrr/Samuel.Trahan/westwater/HAFS
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
try=try26
SUBEXPT=${EXPT}_$try
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

 # 3DEnVar with GDAS ensembles
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT} \
     config.NHRS=12 ${scrubopt} \
     ../parm/WaterInTheWest-$try.conf
