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
 HOMEhafs=/work/noaa/hwrf/save/${USER}/hafsv0p3_20220412
 dev="-s sites/orion.ent -f"
 PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})

#===============================================================================

 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_h3bp \
     forecast.restart_interval=240 \
     ../parm/hafsv0p3_regional_mvnest.conf \
     ../parm/hafsv0p3_hycom.conf"

# Techincal testing
#${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 13L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no

# 2021 NATL storms
#${PYTHON3} ./run_hafs.py -t ${dev} 2021052000-2021052400 01L HISTORY ${confopts} # Ana
#${PYTHON3} ./run_hafs.py -t ${dev} 2021061400-2021061600 02L HISTORY ${confopts} # Bill
#${PYTHON3} ./run_hafs.py -t ${dev} 2021061712-2021062200 03L HISTORY ${confopts} # Claudette
#${PYTHON3} ./run_hafs.py -t ${dev} 2021062712-2021062906 04L HISTORY ${confopts} # Danny
#${PYTHON3} ./run_hafs.py -t ${dev} 2021063006-2021070918 05L HISTORY ${confopts} # Elsa
#${PYTHON3} ./run_hafs.py -t ${dev} 2021080912-2021082000 06L HISTORY ${confopts} # Fred
#${PYTHON3} ./run_hafs.py -t ${dev} 2021081206-2021082118 07L HISTORY ${confopts} # Grace
#${PYTHON3} ./run_hafs.py -t ${dev} 2021081518-2021082406 08L HISTORY ${confopts} # Henri
#${PYTHON3} ./run_hafs.py -t ${dev} 2021082512-2021083012 09L HISTORY ${confopts} # Ida
#${PYTHON3} ./run_hafs.py -t ${dev} 2021082800-2021090118 10L HISTORY ${confopts} # Kate
#${PYTHON3} ./run_hafs.py -t ${dev} 2021082812-2021083000 11L HISTORY ${confopts} # Julian
#${PYTHON3} ./run_hafs.py -t ${dev} 2021083100-2021091112 12L HISTORY ${confopts} # Larry
#${PYTHON3} ./run_hafs.py -t ${dev} 2021090812-2021091000 13L HISTORY ${confopts} # Mindy
#${PYTHON3} ./run_hafs.py -t ${dev} 2021091200-2021091718 14L HISTORY ${confopts} # Nicholas
#${PYTHON3} ./run_hafs.py -t ${dev} 2021091506-2021092406 15L HISTORY ${confopts} # Odette
#${PYTHON3} ./run_hafs.py -t ${dev} 2021091800-2021092918 16L HISTORY ${confopts} # Peter
#${PYTHON3} ./run_hafs.py -t ${dev} 2021091818-2021092406 17L HISTORY ${confopts} # Rose
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092206-2021100506 18L HISTORY ${confopts} # Sam
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092406-2021092518 19L HISTORY ${confopts} # Teresa
#${PYTHON3} ./run_hafs.py -t ${dev} 2021092818-2021100412 20L HISTORY ${confopts} # Victor
#${PYTHON3} ./run_hafs.py -t ${dev} 2021103012-2021110712 21L HISTORY ${confopts} # Wanda

# 2020 NATL storms
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020051606-2020051918 01L HISTORY ${confopts} # Arthur
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020052700-2020052718 02L HISTORY ${confopts} # Bertha
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020060112-2020060812 03L HISTORY ${confopts} # Cristobal hwrfdata_PROD2020HDOBS
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020062112-2020062406 04L HISTORY ${confopts} # Dolly
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020070400-2020070612 05L HISTORY ${confopts} # Edouard
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020070912-2020071100 06L HISTORY ${confopts} # Fay
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020072112-2020072518 07L HISTORY ${confopts} # Gonzalo
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020072212-2020072618 08L HISTORY ${confopts} # Hanna
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020072806-2020080500 09L HISTORY ${confopts} # Isaias
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020073018-2020080118 10L HISTORY ${confopts} # Ten
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020081100-2020081612 11L HISTORY ${confopts} # Josephine
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020081412-2020081600 12L HISTORY ${confopts} # Kyle
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020081918-2020082718 13L HISTORY ${confopts} # Laura
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020081818-2020082500 14L HISTORY ${confopts} # Marco
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020083112-2020090512 15L HISTORY ${confopts} # Omar
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020083112-2020090318 16L HISTORY ${confopts} # Nana
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020090612-2020091612 17L HISTORY ${confopts} # Paulette part1
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020091806-2020092318 17L HISTORY ${confopts} # Paulette part2
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020090700-2020091412 18L HISTORY ${confopts} # Rene
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020091112-2020091618 19L HISTORY ${confopts} # Sally
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020091212-2020092306 20L HISTORY ${confopts} # Teddy
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020091318-2020091712 21L HISTORY ${confopts} # Vicky
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020091700-2020092218 22L HISTORY ${confopts} # Beta
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020091718-2020092100 23L HISTORY ${confopts} # Wilfred
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020091818-2020091900 24L HISTORY ${confopts} # Alpha # Do not need to run
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020100118-2020100600 25L HISTORY ${confopts} # Gamma
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020100312-2020101012 26L HISTORY ${confopts} # Delta
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020101618-2020102600 27L HISTORY ${confopts} # Epsilon
#${PYTHON3} ./run_hwrf.py -t ${dev} 2020102412-2020102912 28L HISTORY ${confopts} # Zeta

#===============================================================================

date

echo 'cronjob done'
