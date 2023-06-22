#!/bin/sh
set -x
date

HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/H223_ensemble_cloud
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})

scrubopt="config.scrub_work=no config.scrub_com=no"

opts="-t -f -s sites/hera_ensemble.ent"

#===============================================================================

 # hafsv0p2a phase2
 confopts="config.EXPT=${EXPT} config.SUBEXPT=H223_ensemble_cloud \
     ../parm/hafs_2023_ensemble_AL.conf \
     config.cpu_account=aoml-hafs1 config.disk_project=hwrf \
     dir.CDSAVE=/scratch1/NCEPDEV/{disk_project}/save/{ENV[USER]} \
     dir.CDNOSCRUB=/scratch1/NCEPDEV/{disk_project}/noscrub/{ENV[USER]}/hafstrak \
     dir.CDSCRUB=/scratch1/NCEPDEV/{disk_project}/scrub/{ENV[USER]}"

#for ens in 00 01 02 03
for ens in 04
do

if [ $ens -eq 00 ] ; then
 ./run_hafs.py -t ${opts} 2020081918 00L HISTORY \
     ${confopts} ${scrubopt} \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_${ens} config.ENS=${ens} \
     config.do_sppt=.F. config.do_shum=.F. config.do_skeb=.F. \
     forecast.write_groups=3 forecast.write_tasks_per_group=20 rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_240PE \
     forecast.all_tasks=240 forecast.atm_tasks=240 \
     dir.COMgfs=/scratch1/NCEPDEV/hwrf/noscrub/input/GEFS
else
 ./run_hafs.py -t ${opts} 2020081918 00L HISTORY \
     ${confopts} ${scrubopt} \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_${ens} config.ENS=${ens} \
     forecast.write_groups=3 forecast.write_tasks_per_group=20 rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_240PE \
     forecast.all_tasks=240 forecast.atm_tasks=240 \
     dir.COMgfs=/scratch1/NCEPDEV/hwrf/noscrub/input/GEFS
fi

done
#===============================================================================

date

echo 'cronjob done'
