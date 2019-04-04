#!/bin/sh
set -xue
date

export EXPT=HAFS
export SUBEXPT=HAFS

export CPU_ACCOUNT=HWRF-T2O
export SCRUB_WORK=YES
export SCRUB_COM=YES
export FETCH_INPUT=YES
export RUN_VORTEXINIT=NO
export RUN_GSI=NO
export RUN_OCEAN=NO
export RUN_WAVE=NO

#export COMINarch=/gpfs/tp1/nco/ops/com/arch/prod/syndat
export COMINarch=/gpfs/hps3/emc/hwrf/noscrub/input/SYNDAT-PLUS
export COMINgfs=/gpfs/dell1/nco/ops/com/gfs/para/
export COMINgdas=/gpfs/dell1/nco/ops/com/gfs/para/
export COMINmsg=/gpfs/hps/nco/ops/com/hur/prod/inphwrf

export CDSAVE=/gpfs/hps3/emc/hwrf/noscrub/${USER}/save
export CDSCRUB=/gpfs/hps2/ptmp/${USER}
export HOMEhafs=${CDSAVE}/${EXPT}
export EXhafs=${HOMEhafs}/scripts
export USHhafs=${HOMEhafs}/ush
export PYTHONPATH=${USHhafs}
export PARMhafs=${HOMEhafs}/parm

cd ${HOMEhafs}/rocoto

#dev="-f"
dev="-s sites/wcoss_cray.ent -f"

 ./run_hafs.py ${dev} 2018 06L HISTORY # Florence
#./run_hafs.py ${dev} 2018083018-2018083100 06L HISTORY # Florence
#./run_hafs.py -t ${dev} 2018100100-2018100206 00L HISTORY # nostorm

date
echo 'cronjob done'
