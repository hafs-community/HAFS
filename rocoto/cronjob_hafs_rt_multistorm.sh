#!/bin/sh
set -x
date

#HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
HOMEhafs=${HOMEhafs:-/scratch2/AOML/aoml-hafs1/Lew.Gramer/hafsv2p1_phase3_multistorm}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no config.archive=none"

if [ 1 == 1 ]; then
 echo "RUNNING MULTISTORM CASE"
 #    dir.COMgfs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMGFSv16 \
 #    dir.COMrtofs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMRTOFSv2 \
 #    ../parm/hfsb_regional_multistorm_ocean_DA_workflow_largedomain_15west_gsi_d02.conf
 
 # # LEAVE atm*nc FILES for Bill: Nadine 15L, Oscar 16L, Kristy 12E
 # ./run_hafs.py ${opts} -M L,E 2024100600 00L HISTORY \
 ./run_hafs.py ${opts} -M L,E 2020082506 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_3storm \
     gsi.use_bufr_nr=yes \
     grid.nest_grids=3 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 config.scrub_work=no config.scrub_com=no \
     ../parm/hfsm_mom6.conf
fi

if [ 1 == 1 ]; then
 echo "RUNNING MULTISTORM OSE CONTROL"
 # # LEAVE atm*nc FILES for Bill: Nadine 15L, Oscar 16L, Kristy 12E
 ./run_hafs.py ${opts} -M L,E -m 10L 2023082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_ose_ctrl \
     gsi.use_bufr_nr=yes \
     grid.nest_grids=3 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 config.scrub_work=no config.scrub_com=no \
     dir.CDSCRUB=/scratch4/AOML/aoml-hafs1/Lew.Gramer/scrub \
     ../parm/hfsm_mom6.conf
fi

#===============================================================================
 # HAFS application regression tests for operational configurations using development computation resources

 # HFSA with two-way atm-ocn coupling and one-way atm-wav coupling
  ./run_hafs.py ${opts} 2020082506-2020082512 13L HISTORY \
      config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_hfsa_dev_ww3 \
      config.NHRS=12 ${scrubopt} \
      ../parm/hfsa_dev_ww3.conf


 # HFSB with two-way atm-ocn coupling
 ./run_hafs.py ${opts} 2020082506-2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_hfsb_dev \
     config.NHRS=12 ${scrubopt} \
     dir.CDSCRUB=/scratch4/AOML/aoml-hafs1/Lew.Gramer/scrub \
     ../parm/hfsb_dev.conf


#===============================================================================
 # HAFS application regression tests for experimental regional configurations

 # Regional standalone storm-focused configuration with ESG grid and GFS grib2ab input
 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_atm \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_regional_atm.conf

 # Regional standalone storm-focused configuration with ESG grid and atm-ocn-wav coupling
 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_atm_ocn_wav \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_regional_atm_ocn_wav.conf

 # Regional standalone low-resolution static basin-focused configuration with atm-ocn-wav coupling
 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_C192s1n4_atm_ocn_wav \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_regional_static_C192s1n4_atm_ocn_wav.conf

 # Regional standalone low-resolution static basin-focused configuration with 3DEnVar using GDAS ensembles
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_C192s1n4_atm_3denvar \
     config.NHRS=12 ${scrubopt} \
     ../parm/rt_conf/hafs_regional_static_C192s1n4_atm_3denvar.conf

#===============================================================================
 # HAFS application regression tests for experimental global-nesting configurations

 # Global-nesting storm-focused configuration
 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_atm \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_globnest_atm.conf

 # Global-nesting static basin-focused configuration with GFS grib2ab input
 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_static_atm \
     config.NHRS=6 ${scrubopt} \
     ../parm/rt_conf/hafs_globnest_static_atm.conf

#===============================================================================

date

echo 'cronjob done'
