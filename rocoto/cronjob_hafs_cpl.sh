#!/bin/sh
set -x
date

# NOAA WCOSS Dell Phase3
#HOMEhafs=/gpfs/dell2/emc/modeling/noscrub/${USER}/save/HAFS
#dev="-s sites/wcoss_dell_p3.ent -f"
#PYTHON3=/usrx/local/prod/packages/python/3.6.3/bin/python3

# NOAA WCOSS Cray
#HOMEhafs=/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS
#dev="-s sites/wcoss_cray.ent -f"
#PYTHON3=/opt/intel/intelpython3/bin/python3

# NOAA RDHPCS Jet
#HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/HAFS
#dev="-s sites/xjet.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
 HOMEhafs=/work/noaa/hwrf/save/${USER}/HAFS
 dev="-s sites/orion.ent -f"
 PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
confopts="forecast.write_dopost=.true. \
         forecast.output_history=.true. \
         config.run_emcgraphics=no \
         config.NHRS=12 \
         config.scrub_work=no \
         config.scrub_com=no"

#===============================================================================
# atm-ocn-wav coupling

 # hafs_a2o2a_a2w2a
 conf_a2o2a_a2w2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o2a_a2w2a \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_2way \
     forecast.cpl_atm_wav=cmeps_2way \
     ${confopts}"

 # hafs_a2o2a_a2w
 conf_a2o2a_a2w="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o2a_a2w \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_2way \
     forecast.cpl_atm_wav=cmeps_1way_1to2 \
     ${confopts}"

 # hafs_a2o_a2w2a
 conf_a2o_a2w2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o_a2w2a \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_1way_1to2 \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     forecast.cpl_atm_wav=cmeps_2way \
     ${confopts}"

 # hafs_a2o_a2w
 conf_a2o_a2w="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o_a2w \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_1way_1to2 \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     forecast.cpl_atm_wav=cmeps_1way_1to2 \
     ${confopts}"

 # hafs_abo_abw
 conf_abo_abw="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_abo_abw \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_sidebyside \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     forecast.cpl_atm_wav=cmeps_sidebyside \
     ${confopts}"

 # hafs_nupoc_abo_abw
 conf_nuopc_abo_abw="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_nuopc_abo_abw \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=nuopc_sidebyside \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     forecast.cpl_atm_wav=nuopc_sidebyside \
     ${confopts}"

 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_a2o2a_a2w2a}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_a2o2a_a2w}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_a2o_a2w2a}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_a2o_a2w}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_abo_abw}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_nuopc_abo_abw}

#===============================================================================
# atm-ocn coupling

 # hafs_a2o2a
 conf_a2o2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o2a \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom.conf \
     forecast.cpl_atm_ocn=cmeps_2way \
     ${confopts}"

 # hafs_a2o
 conf_a2o="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom.conf \
     forecast.cpl_atm_ocn=cmeps_1way_1to2 \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     ${confopts}"

 # hafs_o2a
 conf_o2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_o2a \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom.conf \
     forecast.cpl_atm_ocn=cmeps_1way_2to1 \
     ${confopts}"

 # hafs_abo
 conf_abo="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_abo \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom.conf \
     forecast.cpl_atm_ocn=cmeps_sidebyside \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     ${confopts}"

 # hafs_nuopc_abo
 conf_nuopc_abo="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_nuopc_abo \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom.conf \
     forecast.cpl_atm_ocn=nuopc_sidebyside \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     ${confopts}"

 # hafs_nuopc_a2o2a
 conf_nuopc_a2o2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_nuopc_a2o2a \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_hycom.conf \
     forecast.cpl_atm_ocn=nuopc_bilinear \
     ${confopts}"

 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_a2o2a}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_a2o}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_o2a}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_abo}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_nuopc_abo}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_nuopc_a2o2a}

#===============================================================================
# atm-wav coupling

 # hafs_a2w2a
 conf_a2w2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2w2a \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_2way \
     ${confopts}"

 # hafs_a2w
 conf_a2w="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2w \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_1way_1to2 \
     ${confopts}"

 # hafs_w2a
 conf_w2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_w2a \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_1way_2to1 \
     ${confopts}"

 # hafs_abw
 conf_abw="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_abw \
     ../parm/hafsv0p2aL91_AL.conf \
     ../parm/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_sidebyside \
     ${confopts}"

 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_a2w2a}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_a2w}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_w2a}
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506 00L HISTORY ${conf_abw}

#===============================================================================

date

echo 'cronjob done'
