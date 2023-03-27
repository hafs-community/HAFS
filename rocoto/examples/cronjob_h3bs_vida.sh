#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================
 # h3db_vidacycst: atm_init+atm_vi+fgat+3denvar+anal_merge and cycling storm
 conf_h3bs_vidacycst="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bs_vidacycst \
     config.run_atm_init=yes config.run_atm_init_fgat=yes config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=yes config.run_atm_vi_fgat=yes config.run_atm_vi_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.gsi_d01=yes config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=yes config.run_analysis_merge_ens=no \
     vi.vi_storm_env=pert \
     atm_merge.atm_merge_method=vortexreplace analysis_merge.analysis_merge_method=vortexreplace \
     config.NHRS=126 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.grid_ratio_fv3_regional=1 \
     ../parm/tests/hafsv0p3_regional_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf"

 # h3bs_vida: atm_init+atm_vi+fgat+3denvar+anal_merge and cycling storm perturbation
 conf_h3bs_vida="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bs_vida \
     config.run_atm_init=yes config.run_atm_init_fgat=yes config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=yes config.run_atm_vi_fgat=yes config.run_atm_vi_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.gsi_d01=yes config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=yes config.run_analysis_merge_ens=no \
     vi.vi_storm_env=init \
     atm_merge.atm_merge_method=vortexreplace analysis_merge.analysis_merge_method=vortexreplace \
     config.NHRS=126 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.grid_ratio_fv3_regional=1 \
     ../parm/tests/hafsv0p3_regional_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf"

 # h3bs_dacycst: atm_init+atm_merge+fgat+3denvar+anal_merge and cycling storm
 conf_h3bs_dacycst="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bs_dacycst \
     config.run_atm_init=yes config.run_atm_init_fgat=yes config.run_atm_init_ens=no \
     config.run_atm_merge=yes config.run_atm_merge_fgat=yes config.run_atm_merge_ens=no \
     config.run_atm_vi=no config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.gsi_d01=yes config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=yes config.run_analysis_merge_ens=no \
     vi.vi_storm_env=init \
     atm_merge.atm_merge_method=vortexreplace analysis_merge.analysis_merge_method=vortexreplace \
     config.NHRS=126 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.grid_ratio_fv3_regional=1 \
     ../parm/tests/hafsv0p3_regional_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf"

 # h3bs_dacycdm: atm_init+atm_merge+fgat+3denvar+anal_merge and cycling whole domain
 conf_h3bs_dacycdm="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bs_dacycdm \
     config.run_atm_init=yes config.run_atm_init_fgat=yes config.run_atm_init_ens=no \
     config.run_atm_merge=yes config.run_atm_merge_fgat=yes config.run_atm_merge_ens=no \
     config.run_atm_vi=no config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.gsi_d01=yes config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=yes config.run_analysis_merge_ens=no \
     vi.vi_storm_env=init \
     atm_merge.atm_merge_method=domainmerge analysis_merge.analysis_merge_method=domainmerge \
     config.NHRS=126 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.grid_ratio_fv3_regional=1 \
     ../parm/tests/hafsv0p3_regional_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf"

 # h3bs_vicycst: atm_init+atm_vi+anal_merge and cycling storm
 conf_h3bs_vicycst="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bs_vicycst \
     config.run_atm_init=yes config.run_atm_init_fgat=no config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=yes config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi=no config.run_fgat=no config.run_envar=no \
     config.gsi_d01=no config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=yes config.run_analysis_merge_ens=no \
     vi.vi_storm_env=pert \
     atm_merge.atm_merge_method=vortexreplace analysis_merge.analysis_merge_method=vortexreplace \
     config.NHRS=126 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.grid_ratio_fv3_regional=1 \
     ../parm/tests/hafsv0p3_regional_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf"

 # h3bs_vi: atm_init+atm_vi+anal_merge and cycling storm perturbation
 conf_h3bs_vi="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bs_vi \
     config.run_atm_init=yes config.run_atm_init_fgat=no config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=yes config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi=no config.run_fgat=no config.run_envar=no \
     config.gsi_d01=no config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=yes config.run_analysis_merge_ens=no \
     vi.vi_storm_env=init \
     atm_merge.atm_merge_method=vortexreplace analysis_merge.analysis_merge_method=vortexreplace \
     config.NHRS=126 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.grid_ratio_fv3_regional=1 \
     ../parm/tests/hafsv0p3_regional_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf"

 # h3bs_init: warmstart from the coldstart atm_init (initialized from gfs analysis)
 conf_h3bs_init="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bs_init \
     config.run_atm_init=yes config.run_atm_init_fgat=no config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=no config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi=no config.run_fgat=no config.run_envar=no \
     config.gsi_d01=no config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=no config.run_analysis_merge_ens=no \
     vi.vi_storm_env=init \
     atm_merge.atm_merge_method=vortexreplace analysis_merge.analysis_merge_method=vortexreplace \
     config.NHRS=126 \
     forecast.restart_interval=240 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.grid_ratio_fv3_regional=1 \
     ../parm/tests/hafsv0p3_regional_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf"

 # h3bs: coldstart from gfs analysis
 conf_h3bs="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bs \
     config.run_atm_init=no config.run_atm_init_fgat=no config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=no config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi=no config.run_fgat=no config.run_envar=no \
     config.gsi_d01=no config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=no config.run_analysis_merge_ens=no \
     vi.vi_storm_env=init \
     atm_merge.atm_merge_method=vortexreplace analysis_merge.analysis_merge_method=vortexreplace \
     config.NHRS=126 \
     forecast.restart_interval=240 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.grid_ratio_fv3_regional=1 \
     ../parm/tests/hafsv0p3_regional_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf"

 # Choose the configuration to run
#confopts="${conf_h3bs_vidacycst}"
#confopts="${conf_h3bs_vida}"
#confopts="${conf_h3bs_dacycst}"
#confopts="${conf_h3bs_dacycdm}"
#confopts="${conf_h3bs_vicycst}"
#confopts="${conf_h3bs_vi}"
#confopts="${conf_h3bs_init}"
#confopts="${conf_h3bs}"

 confopts="${conf_h3bs_vida}"
 # Technical testing
 ./run_hafs.py ${opts} 2020082506-2020082512 13L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Storms to run: Laura13L2020, Ida09L2021, Sam18L2021
#./run_hafs.py ${opts} 2020081918-2020082718 13L HISTORY ${confopts}
#./run_hafs.py ${opts} 2021082612-2021083012 09L HISTORY ${confopts}
#./run_hafs.py ${opts} 2021092300-2021100500 18L HISTORY ${confopts}

#===============================================================================

date

echo 'cronjob done'
