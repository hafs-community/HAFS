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
for ens in 02
do

if [ $ens -eq 00 ] ; then
 ./run_hafs.py -t ${opts} 2020081918 00L HISTORY \
     ${confopts} ${scrubopt} config.cpu_account=aoml-hafs1 \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_${ens} config.ENS=${ens} \
     config.do_sppt=.F. config.do_shum=.F. config.do_skeb=.F. \
     forecast.write_groups=3 forecast.write_tasks_per_group=20 rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_300PE \
     forecast.all_tasks=300 forecast.atm_tasks=240 rocotostr.\
     dir.COMgfs=/scratch1/NCEPDEV/hwrf/noscrub/input/GEFS \
     dir.COMgfs_ocean=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMGFSv16 \
     hycominit1.atmos2_dataset=gfs_ocean hycominit2.atmos2_dataset=gfs_ocean
else
 ./run_hafs.py -t ${opts} 2020081918 00L HISTORY \
     ${confopts} ${scrubopt} config.cpu_account=aoml-hafs1 \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_${ens} config.ENS=${ens} \
     forecast.write_groups=3 forecast.write_tasks_per_group=20 rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_300PE \
     forecast.all_tasks=240 forecast.atm_tasks=240 \
     dir.COMgfs=/scratch1/NCEPDEV/hwrf/noscrub/input/GEFS \
     dir.COMgfs_ocean=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMGFSv16 \
     hycominit1.atmos2_dataset=gfs_ocean hycominit2.atmos2_dataset=gfs_ocean
fi

done
#===============================================================================

date

echo 'cronjob done'
