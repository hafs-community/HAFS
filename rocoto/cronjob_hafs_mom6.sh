#!/bin/sh
set -x
date

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#source ${HOMEhafs}/ush/hafs_pre_job.sh.inc
# NOAA RDHPCS Jet
HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/HAFS
source /mnt/lfs4/HFIP/hwrfv3/${USER}/HAFS/ush/hafs_pre_job.sh.inc

dev="-s sites/hera.ent -f"
PYTHON3=/apps/intel/intelpython3/bin/python3

#HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

 # Regional static NATL basin-focused configuration with atm-ocn coupling
# ./run_hafs.py ${opts} 2020082512 00L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_atm_ocn \
#     config.NHRS=6 ${scrubopt} \
#     ../parm/hafs_regional_static.conf \
#     ../parm/hafs_hycom.conf

 # Regional static NATL basin-focused configuration with atm-wav coupling
# ./run_hafs.py ${opts} 2020082512 00L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_atm_wav \
#     config.NHRS=6 ${scrubopt} \
#     ../parm/hafs_regional_static.conf \
#     ../parm/hafs_ww3.conf \
#     forecast.cpl_atm_wav=cmeps_2way

## Regional static NATL basin-focused configuration with atm-ocn-wav coupling
#./run_hafs.py ${opts} 2020082512 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_atm_ocn_wav \
#    config.NHRS=6 ${scrubopt} \
#    ../parm/hafs_regional_static.conf \
#    ../parm/hafs_hycom_ww3.conf \
#    forecast.cpl_atm_ocn=cmeps_2way \
#    forecast.cpl_atm_wav=cmeps_2way

 # Regional low-resolution static NATL basin-focused configuration with atm-datm-mom6 coupling
 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_C192s1n4_atm_datm_mom6 \
     config.NHRS=6 ${scrubopt} \
     ../parm/hafs_regional_static_C192s1n4_mom6.conf \
     ../parm/hafs_mom6.conf
#     ../parm/hafs_hycom_ww3.conf \
#     forecast.cpl_atm_ocn=cmeps_2way
#     forecast.cpl_atm_wav=cmeps_2way

 # Regional low-resolution static NATL basin-focused configuration with atm-ocn coupling
# ./run_hafs.py ${opts} 2020082512 13L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_C192s1n4_atm_ocn \
#     config.NHRS=24 ${scrubopt} \
#     ../parm/hafs_regional_static_C192s1n4.conf \
#     ../parm/hafs_mom6.conf
#     ../parm/hafs_hycom_ww3.conf \
#     forecast.cpl_atm_ocn=cmeps_2way
#     forecast.cpl_atm_wav=cmeps_2way

 # Regional low-resolution static NATL basin-focused configuration with atm-ocn-wav coupling
# ./run_hafs.py ${opts} 2019082900 00L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_C192s1n4_atm_ocn_wav \
#     config.NHRS=24 ${scrubopt} \
#     ../parm/hafs_regional_static_C192s1n4.conf \
#     ../parm/hafs_hycom_ww3.conf \
#     forecast.cpl_atm_ocn=cmeps_2way \
#     forecast.cpl_atm_wav=cmeps_2way

 # Regional storm-focused configuration with atm-ocn-wav coupling
# ./run_hafs.py ${opts} 2020082512 13L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_atm_ocn_wav \
#     config.NHRS=6 ${scrubopt} \
#     ../parm/hafs_hycom_ww3.conf \
#     forecast.cpl_atm_ocn=cmeps_2way \
#     forecast.cpl_atm_wav=cmeps_2way

 # Regional storm-focused configuration (atm-only) with GFS grib2ab format IC/BC
# ./run_hafs.py ${opts} 2020082512 13L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_atm_only \
#     config.ictype=gfsgrib2ab_0p25 forecast.nstf_n2=1 \
#     config.NHRS=6 ${scrubopt}

 # Regional low-resolution static NATL basin-focused configuration with 3DEnVar with GDAS ensembles
# ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_da_C192s1n4_3denvar \
#     config.run_gsi=yes config.run_envar=yes \
#     gsi.use_bufr_nr=yes \
#     config.NHRS=12 ${scrubopt} \
#     ../parm/hafs_regional_da_C192s1n4.conf

 # Regional storm-focused moving-nesting configuration with vortex initialization and domain 02 data assimilation
 #   atm_init+atm_vi+fgat+d02_3denvar+anal_merge and cycling storm perturbation
# ./run_hafs.py ${opts} 2020082506-2020082512 13L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_mvnest_vida \
#     config.run_atm_init=yes config.run_atm_init_fgat=yes config.run_atm_init_ens=no \
#     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
#     config.run_atm_vi=yes config.run_atm_vi_fgat=yes config.run_atm_vi_ens=no \
#     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
#     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
#     config.gsi_d01=no config.gsi_d02=yes \
#     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
#     config.run_analysis_merge=yes config.run_analysis_merge_ens=no \
#     vi.vi_storm_env=init \
#     atm_merge.atm_merge_method=vortexreplace analysis_merge.analysis_merge_method=vortexreplace \
#     config.NHRS=12 ${scrubopt} \
#     config.GRID_RATIO_ENS=2 \
#     gsi.use_bufr_nr=yes \
#     gsi.grid_ratio_fv3_regional=1 \
#     ../parm/hafsv0p3_regional_mvnest.conf \
#     ../parm/hafsv0p3_hycom.conf

#===============================================================================

 # Global-nesting static NATL basin-focused configuration (atm-only)
# ./run_hafs.py ${opts} 2020082512 00L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_static \
#     config.NHRS=6 ${scrubopt} \
#     ../parm/hafs_globnest_static.conf

 # Global-nesting storm-focused configuration (atm-only) with GFS grib2ab format IC/BC
# ./run_hafs.py ${opts} 2020082512 13L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_grib2ab \
#     config.ictype=gfsgrib2ab_0p25 forecast.nstf_n2=1 \
#     config.NHRS=6 ${scrubopt} \
#     ../parm/hafs_globnest.conf
#
#===============================================================================

date

echo 'cronjob done'
