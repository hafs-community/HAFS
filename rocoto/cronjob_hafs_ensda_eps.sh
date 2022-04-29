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
 HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/hafsv0p3e_20220429
 dev="-s sites/xjet.ent -f"
 PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
#HOMEhafs=/work/noaa/hwrf/save/${USER}/HAFS
#dev="-s sites/orion.ent -f"
#PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})

#===============================================================================
 # ensda_eps: fgat+3denvar+enkf
 conf_ensda_eps="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_ensda_eps \
     config.run_atm_init=no config.run_atm_init_fgat=no config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=no config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.gsi_d01=yes config.gsi_d02=no \
     config.run_ensda=yes config.ENS_SIZE=40 config.run_enkf=yes \
     config.run_analysis_merge=no config.run_analysis_merge_ens=no \
     atm_merge.atm_merge_method=domainmerge analysis_merge.analysis_merge_method=domainmerge \
     config.NHRS=126 \
     config.GRID_RATIO_ENS=1 \
     gsi.use_bufr_nr=yes \
     ../parm/hafs_2022_regional_ensda_eps_AL.conf"

 confopts="${conf_ensda_eps}"
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 13L HISTORY ${confopts} \
     config.NHRS=12 config.ENS_SIZE=10 config.scrub_work=no config.scrub_com=no

 # Storms to run: Laura13L2020, Ida09L2021, Sam18L2021
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081918-2020082718 13L HISTORY ${confopts}
#${PYTHON3} ./run_hafs.py -t ${dev} 2021082612-2021083012 09L HISTORY ${confopts}
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092300-2021100500 18L HISTORY ${confopts}

#===============================================================================

date

echo 'cronjob done'
