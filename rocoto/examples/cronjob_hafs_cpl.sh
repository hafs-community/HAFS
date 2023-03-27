#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

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
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_2way \
     forecast.cpl_atm_wav=cmeps_2way \
     ${confopts}"

 # hafs_a2o2a_a2w
 conf_a2o2a_a2w="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o2a_a2w \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_2way \
     forecast.cpl_atm_wav=cmeps_1way_1to2 \
     ${confopts}"

 # hafs_a2o_a2w2a
 conf_a2o_a2w2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o_a2w2a \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_1way_1to2 \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     forecast.cpl_atm_wav=cmeps_2way \
     ${confopts}"

 # hafs_a2o_a2w
 conf_a2o_a2w="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o_a2w \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_1way_1to2 \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     forecast.cpl_atm_wav=cmeps_1way_1to2 \
     ${confopts}"

 # hafs_abo_abw
 conf_abo_abw="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_abo_abw \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_sidebyside \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     forecast.cpl_atm_wav=cmeps_sidebyside \
     ${confopts}"

 # hafs_nupoc_abo_abw
 conf_nuopc_abo_abw="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_nuopc_abo_abw \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=nuopc_sidebyside \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     forecast.cpl_atm_wav=nuopc_sidebyside \
     ${confopts}"

 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_a2o2a_a2w2a}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_a2o2a_a2w}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_a2o_a2w2a}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_a2o_a2w}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_abo_abw}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_nuopc_abo_abw}

#===============================================================================
# atm-ocn coupling

 # hafs_a2o2a
 conf_a2o2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o2a \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom.conf \
     forecast.cpl_atm_ocn=cmeps_2way \
     ${confopts}"

 # hafs_a2o
 conf_a2o="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2o \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom.conf \
     forecast.cpl_atm_ocn=cmeps_1way_1to2 \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     ${confopts}"

 # hafs_o2a
 conf_o2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_o2a \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom.conf \
     forecast.cpl_atm_ocn=cmeps_1way_2to1 \
     ${confopts}"

 # hafs_abo
 conf_abo="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_abo \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom.conf \
     forecast.cpl_atm_ocn=cmeps_sidebyside \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     ${confopts}"

 # hafs_nuopc_abo
 conf_nuopc_abo="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_nuopc_abo \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom.conf \
     forecast.cpl_atm_ocn=nuopc_sidebyside \
     forecast.nstf_n1=2 forecast.nstf_n2=0 \
     forecast.ccpp_suite_regional=FV3_HAFS_v0_gfdlmp_tedmf \
     ${confopts}"

 # hafs_nuopc_a2o2a
 conf_nuopc_a2o2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_nuopc_a2o2a \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_hycom.conf \
     forecast.cpl_atm_ocn=nuopc_bilinear \
     ${confopts}"

 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_a2o2a}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_a2o}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_o2a}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_abo}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_nuopc_abo}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_nuopc_a2o2a}

#===============================================================================
# atm-wav coupling

 # hafs_a2w2a
 conf_a2w2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2w2a \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_2way \
     ${confopts}"

 # hafs_a2w
 conf_a2w="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_a2w \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_1way_1to2 \
     ${confopts}"

 # hafs_w2a
 conf_w2a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_w2a \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_1way_2to1 \
     ${confopts}"

 # hafs_abw
 conf_abw="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_abw \
     ../parm/tests/hafsv0p2aL91_AL.conf \
     ../parm/tests/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_sidebyside \
     ${confopts}"

 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_a2w2a}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_a2w}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_w2a}
 ./run_hafs.py ${opts} 2020082506 00L HISTORY ${conf_abw}

#===============================================================================

date

echo 'cronjob done'
