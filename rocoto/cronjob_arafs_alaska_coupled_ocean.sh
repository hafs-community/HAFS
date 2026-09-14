#!/bin/sh
set -x
date

export HOMEhafs=${HOMEhafs:-/gpfs/f6/ar-cpu/scratch/Maria.Aristizabal/hafsv2p2p1a_arafs_ocean/rocoto}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
SUBEXPT=ARAFS_alaska_coupled_ocean

#scrubopt="config.scrub_work=no config.scrub_com=no config.archive=none"
scrubopt="config.scrub_work=no config.scrub_com=no"
opts="-t -f -s sites/gaeaC6_ensemble.ent"
#===============================================================================

 # hafsv0p2a phase2
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${H225_ensemble} \
	 ../parm/arafs_coupled_ocean.conf "
#     ../parm/hafs_2024_ensemble_mom6ZI_AL.conf "
#for ens in 00 01 02 03 04 05 11 12 13 14 15 16 17 18 19 20
#for ens in 02	

./run_arafs.py ${opts} 2026091400 00E HISTORY \
     ${confopts} ${scrubopt} config.cpu_account=drsa-hurr3 \
     config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_a config.ENS=99 \
     config.GFSVER=PROD2021 \
     config.run_analysis_merge=no \
     forecast.do_sppt=.false. forecast.do_shum=.false. forecast.do_skeb=.false. \
     forecast.lndp_type=0 \
     rocotostr.FORECAST_RESOURCES=FORECAST_RESOURCES_1260PE \
     forecast.restart_interval=240 config.NHRS=120 config.NOUTHRS=3 config.NBDYHRS=3 \
     config.scrub_work=no config.scrub_com=no config.run_production=yes config.run_emcgraphics=yes


#===============================================================================

date

echo 'cronjob done'

