#!/bin/sh
set -x
date

HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/H223_ensemble_cloud
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
SUBEXPT=H223_ensemble_cloud_ocn

scrubopt="config.scrub_work=no config.scrub_com=no"

opts="-t -f -s sites/hera_ensemble.ent"

#===============================================================================

 # hafsv0p2a phase2
 confopts="config.EXPT=${EXPT} config.SUBEXPT=H223_ensemble_cloud \
     ../parm/hafs_2023_ensemble_hycom_AL.conf "

#for ens in 00 01 02 03
for ens in 08
do

if [ $ens -eq 00 ] ; then
 ./run_hafs.py -t ${opts} 2020081918 00L HISTORY \
     ${confopts} ${scrubopt} config.cpu_account=aoml-hafs1 \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_${ens} config.ENS=${ens} \
     forecast.do_sppt=.false. forecast.do_shum=.false. forecast.do_skeb=.false. \
     forecast.write_groups=3 forecast.write_tasks_per_group=20 rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_300PE \
     forecast.all_tasks=288 forecast.atm_tasks=240 forecast.ocn_tasks=48 \
     forecast.restart_interval=240 config.NHRS=126 \
     dir.COMgfs=/scratch1/NCEPDEV/hwrf/noscrub/input/GEFS \
     dir.COMgfs_ocean=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMGFSv16 \
     dir.COMrtofs=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMRTOFSv2 \
     dir.syndat=/scratch1/NCEPDEV/hwrf/noscrub/input/SYNDAT-PLUS \
     hycominit1.atmos2_dataset=gfs_ocean hycominit2.atmos2_dataset=gfs_ocean
else
 ./run_hafs.py -t ${opts} 2020081918 00L HISTORY \
     ${confopts} ${scrubopt} config.cpu_account=aoml-hafs1 \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_${ens} config.ENS=${ens} \
     forecast.write_groups=3 forecast.write_tasks_per_group=20 rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_300PE \
     forecast.all_tasks=288 forecast.atm_tasks=240 forecast.ocn_tasks=48 \
     forecast.restart_interval=240 config.NHRS=126 \
     dir.COMgfs=/scratch1/NCEPDEV/hwrf/noscrub/input/GEFS \
     dir.COMgfs_ocean=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMGFSv16 \
     dir.COMrtofs=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMRTOFSv2 \
     dir.syndat=/scratch1/NCEPDEV/hwrf/noscrub/input/SYNDAT-PLUS \
     hycominit1.atmos2_dataset=gfs_ocean hycominit2.atmos2_dataset=gfs_ocean
fi

done
#===============================================================================

date

echo 'cronjob done'
