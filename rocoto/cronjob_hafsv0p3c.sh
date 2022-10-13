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
 # v0p3c: coldstart from gfs analysis
 conf_v0p3c="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v0p3c \
     config.run_atm_init=no config.run_atm_init_fgat=no config.run_atm_init_ens=no \
     config.run_atm_merge=no config.run_atm_merge_fgat=no config.run_atm_merge_ens=no \
     config.run_atm_vi=no config.run_atm_vi_fgat=no config.run_atm_vi_ens=no \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
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
     ../parm/hafsv0p3c.conf"

 # Choose the configuration to run
 confopts="${conf_v0p3c}"

 # Technical testing
#${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 13L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # 2021 NATL Storms
#${PYTHON3} ./run_hafs.py -t ${dev} 2021052206-2021052318 01L HISTORY ${confopts} # Ana
#${PYTHON3} ./run_hafs.py -t ${dev} 2021061506-2021061518 02L HISTORY ${confopts} # Bill
#${PYTHON3} ./run_hafs.py -t ${dev} 2021061906-2021062200 03L HISTORY ${confopts} # Claudette
#${PYTHON3} ./run_hafs.py -t ${dev} 2021062818-2021062900 04L HISTORY ${confopts} # Danny
#${PYTHON3} ./run_hafs.py -t ${dev} 2021070100-2021070918 05L HISTORY ${confopts} # Elsa
#${PYTHON3} ./run_hafs.py -t ${dev} 2021080918-2021081700 06L HISTORY ${confopts} # Fred
#${PYTHON3} ./run_hafs.py -t ${dev} 2021081312-2021082112 07L HISTORY ${confopts} # Grace
#${PYTHON3} ./run_hafs.py -t ${dev} 2021081600-2021082300 08L HISTORY ${confopts} # Henri
#${PYTHON3} ./run_hafs.py -t ${dev} 2021082612-2021083012 09L HISTORY ${confopts} # Ida
#${PYTHON3} ./run_hafs.py -t ${dev} 2021083012-2021090112 10L HISTORY ${confopts} # Kate
#${PYTHON3} ./run_hafs.py -t ${dev} 2021082912-2021083000 11L HISTORY ${confopts} # Julian
#${PYTHON3} ./run_hafs.py -t ${dev} 2021083118-2021091106 12L HISTORY ${confopts} # Larry
#${PYTHON3} ./run_hafs.py -t ${dev} 2021090900-2021091000 13L HISTORY ${confopts} # Mindy
#${PYTHON3} ./run_hafs.py -t ${dev} 2021091218-2021091500 14L HISTORY ${confopts} # Nicholas
#${PYTHON3} ./run_hafs.py -t ${dev} 2021091718-2021091818 15L HISTORY ${confopts} # Odette part 1
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092112-2021092406 15L HISTORY ${confopts} # Odette part 2
#${PYTHON3} ./run_hafs.py -t ${dev} 2021091906-2021092100 16L HISTORY ${confopts} # Peter part 1
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092112-2021092300 16L HISTORY ${confopts} # Peter part 2
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092618-2021092818 16L HISTORY ${confopts} # Peter part 3
#${PYTHON3} ./run_hafs.py -t ${dev} 2021091918-2021092300 17L HISTORY ${confopts} # Rose
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092300-2021100500 18L HISTORY ${confopts} # Sam
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092418-2021092512 19L HISTORY ${confopts} # Teresa
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092918-2021100412 20L HISTORY ${confopts} # Victor
#${PYTHON3} ./run_hafs.py -t ${dev} 2021103100-2021110706 21L HISTORY ${confopts} # Wanda

 # 2020 NATL storms
#${PYTHON3} ./run_hafs.py -t ${dev} 2020051606-2020051918 01L HISTORY ${confopts} # Arthur
#${PYTHON3} ./run_hafs.py -t ${dev} 2020052700-2020052718 02L HISTORY ${confopts} # Bertha
#${PYTHON3} ./run_hafs.py -t ${dev} 2020060112-2020060812 03L HISTORY ${confopts} # Cristobal hwrfdata_PROD2020HDOBS
#${PYTHON3} ./run_hafs.py -t ${dev} 2020062112-2020062406 04L HISTORY ${confopts} # Dolly
#${PYTHON3} ./run_hafs.py -t ${dev} 2020070400-2020070612 05L HISTORY ${confopts} # Edouard
#${PYTHON3} ./run_hafs.py -t ${dev} 2020070912-2020071100 06L HISTORY ${confopts} # Fay
#${PYTHON3} ./run_hafs.py -t ${dev} 2020072112-2020072518 07L HISTORY ${confopts} # Gonzalo
#${PYTHON3} ./run_hafs.py -t ${dev} 2020072212-2020072618 08L HISTORY ${confopts} # Hanna
#${PYTHON3} ./run_hafs.py -t ${dev} 2020072806-2020080500 09L HISTORY ${confopts} # Isaias
#${PYTHON3} ./run_hafs.py -t ${dev} 2020073018-2020080118 10L HISTORY ${confopts} # Ten
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081100-2020081612 11L HISTORY ${confopts} # Josephine
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081412-2020081600 12L HISTORY ${confopts} # Kyle
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081918-2020082718 13L HISTORY ${confopts} # Laura
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081818-2020082500 14L HISTORY ${confopts} # Marco
#${PYTHON3} ./run_hafs.py -t ${dev} 2020083112-2020090512 15L HISTORY ${confopts} # Omar
#${PYTHON3} ./run_hafs.py -t ${dev} 2020083112-2020090318 16L HISTORY ${confopts} # Nana
#${PYTHON3} ./run_hafs.py -t ${dev} 2020090612-2020091612 17L HISTORY ${confopts} # Paulette part1
#${PYTHON3} ./run_hafs.py -t ${dev} 2020091806-2020092318 17L HISTORY ${confopts} # Paulette part2
#${PYTHON3} ./run_hafs.py -t ${dev} 2020090700-2020091412 18L HISTORY ${confopts} # Rene
#${PYTHON3} ./run_hafs.py -t ${dev} 2020091112-2020091618 19L HISTORY ${confopts} # Sally
#${PYTHON3} ./run_hafs.py -t ${dev} 2020091212-2020092306 20L HISTORY ${confopts} # Teddy
#${PYTHON3} ./run_hafs.py -t ${dev} 2020091318-2020091712 21L HISTORY ${confopts} # Vicky
#${PYTHON3} ./run_hafs.py -t ${dev} 2020091700-2020092218 22L HISTORY ${confopts} # Beta
#${PYTHON3} ./run_hafs.py -t ${dev} 2020091718-2020092100 23L HISTORY ${confopts} # Wilfred
#${PYTHON3} ./run_hafs.py -t ${dev} 2020091818-2020091900 24L HISTORY ${confopts} # Alpha # Do not need to run
#${PYTHON3} ./run_hafs.py -t ${dev} 2020100118-2020100600 25L HISTORY ${confopts} # Gamma
#${PYTHON3} ./run_hafs.py -t ${dev} 2020100312-2020101012 26L HISTORY ${confopts} # Delta
#${PYTHON3} ./run_hafs.py -t ${dev} 2020101618-2020102600 27L HISTORY ${confopts} # Epsilon
#${PYTHON3} ./run_hafs.py -t ${dev} 2020102412-2020102912 28L HISTORY ${confopts} # Zeta

#===============================================================================

date

echo 'cronjob done'
