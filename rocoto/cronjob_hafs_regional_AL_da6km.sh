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

#===============================================================================

 # h2db: hafsv0p2a with fgat+3denvar
 confh2db="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_da6km_h2db \
     config.run_atm_init=no config.run_atm_init_fgat=no config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=no config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.gsi_d01=yes config.gsi_d02=no \
     config.run_ensda=no config.ENS_SIZE=40 config.run_enkf=no \
     config.run_analysis_merge=no config.run_analysis_merge_ens=no \
     atm_merge.atm_merge_method=domainmerge analysis_merge.analysis_merge_method=domainmerge \
     config.NHRS=126 \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     gsi.online_satbias=yes \
     ../parm/hafsv0p3_regional_AL_da6km.conf \
     ../parm/hafsv0p3_hycom.conf"

 # Choose the configuration to run
 confopts="${confh2db}"

 # Technical test for 2020082506-2020082512 13L2020
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

## 2021 NATL storm slots
#${PYTHON3} ./run_hafs.py -t ${dev} 2021080100-2021100600 00L HISTORY ${confopts} # 

## 2020 NATL storm slots
#${PYTHON3} ./run_hafs.py -t ${dev} 2020060112-2020060812 00L HISTORY ${confopts} # Slot 1.0: 03L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020062112-2020062406 00L HISTORY ${confopts} # Slot 1.1: 04L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020070400-2020070612 00L HISTORY ${confopts} # Slot 1.2: 05L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020070912-2020071100 00L HISTORY ${confopts} # Slot 1.3: 06L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020072112-2020072618 00L HISTORY ${confopts} # Slot 1.4: 07L, 08L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020072806-2020080500 00L HISTORY ${confopts} # Slot 2: 09L, 10L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081100-2020081612 00L HISTORY ${confopts} # Slot 3: 11L, 12L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081918-2020082718 00L HISTORY ${confopts} # Slot 4: 13L, 14L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020083112-2020090512 00L HISTORY ${confopts} # Slot 5: 15L, 16L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020090612-2020091118 00L HISTORY ${confopts} # Slot 6: 17Lp1, 18Lp1
#${PYTHON3} ./run_hafs.py -t ${dev} 2020091200-2020092300 00L HISTORY ${confopts} # Slot 7: 17Lp2, 18Lp2, 19-24L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020100212-2020101018 00L HISTORY ${confopts} # Slot 8: 25L, 26L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020101618-2020102912 00L HISTORY ${confopts} # Slot 9: 27L, 28L
#${PYTHON3} ./run_hafs.py -t ${dev} 2020101618-2020102912 00L HISTORY ${confopts} # Slot 10: 29-31L # No GFSv16 version input data

## 2019 NATL storm slots
#${PYTHON3} ./run_hafs.py -t ${dev} 2019082406-2019091006 00L HISTORY ${confopts} # Slot 5: 05-08L
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091212-2019092712 00L HISTORY ${confopts} # Slot 6: 09-12L

#===============================================================================

date

echo 'cronjob done'
