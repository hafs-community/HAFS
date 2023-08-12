#!/bin/sh
set -x
export LMOD_SH_DBG_ON=0
date

HOMEhafs=/hafs/${USER}/save/H223_ensemble_cloud
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
SUBEXPT=H223_ensemble_cloud_ocn

scrubopt="config.scrub_work=no config.scrub_com=no"

opts="-t -f -s sites/aws_ensemble.ent"

#===============================================================================

 # hafsv0p2a phase2
 confopts="config.EXPT=${EXPT} config.SUBEXPT=H223_ensemble_cloud \
     ../parm/hafs_2023_ensemble_hycom_AL.conf \
     dir.CDSAVE=/hafs/${USER}/save \
     dir.CDNOSCRUB=/hafs/${USER}/noscrub/hafstrak \
     dir.CDSCRUB=/hafs/${USER}/scrub "

for ens in 00
#for ens in  06 07 08
#for ens in 04
#for ens in 09
do

if [ $ens -eq 00 ] ; then
 ./run_hafs.py -t ${opts} 2023080200 00L HISTORY \
     ${confopts} ${scrubopt} \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_${ens} config.ENS=${ens} \
     forecast.do_sppt=.false. forecast.do_shum=.false. forecast.do_skeb=.false. \
     config.NHRS=126 \
     forecast.write_groups=5 forecast.write_tasks_per_group=12 rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_288PE \
     forecast.all_tasks=288 forecast.atm_tasks=240 forecast.ocn_tasks=48 \
     forecast.restart_interval=240 \
     dir.COMgfs=/hafs/hafs_runs/hwrf/noscrub/input/GEFS \
     dir.COMgfs_ocean=/hafs/hafs_runs/hwrf/noscrub/input/COMGFSv16 \
     dir.COMrtofs=/hafs/hafs_runs/hwrf/noscrub/input/COMRTOFSv2 \
     dir.syndat=/hafs/hafs_runs/hwrf/noscrub/input/SYNDAT-PLUS \
     hycominit1.atmos2_dataset=gfs_ocean hycominit2.atmos2_dataset=gfs_ocean
else
 ./run_hafs.py -t ${opts} 2023080200 00L HISTORY \
     ${confopts} ${scrubopt} \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_${ens} config.ENS=${ens} \
     config.NHRS=126 \
     forecast.write_groups=5 forecast.write_tasks_per_group=12 rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_288PE \
     forecast.all_tasks=288 forecast.atm_tasks=240 forecast.ocn_tasks=48 \
     forecast.restart_interval=240 \
     dir.COMgfs=/hafs/hafs_runs/hwrf/noscrub/input/GEFS \
     dir.COMgfs_ocean=/hafs/hafs_runs/hwrf/noscrub/input/COMGFSv16 \
     dir.COMrtofs=/hafs/hafs_runs/hwrf/noscrub/input/COMRTOFSv2 \
     dir.syndat=/hafs/hafs_runs/hwrf/noscrub/input/SYNDAT-PLUS \
     hycominit1.atmos2_dataset=gfs_ocean hycominit2.atmos2_dataset=gfs_ocean
fi

done
#===============================================================================

date

echo 'cronjob done'
