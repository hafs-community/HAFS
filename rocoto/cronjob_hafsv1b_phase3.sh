#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
#opts="-t -s sites/${WHERE_AM_I:-wcoss2}.ent -f"
opts="-t -f"
#===============================================================================

 #hafsv1b phase3
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v1b_phase3 \
     ../parm/hafsv1b_phase3.conf"

##hafsv1b phase3 72s
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v1b_phase3_72s \
#    ../parm/hafsv1b_phase3_72s.conf"

 # Technical testing for Hurricane Laura
#./run_hafs.py ${opts} 2020082512 13L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

 # 2022 NATL Storms
#./run_hafs.py ${opts} 2022060506-2022060618 01L HISTORY ${confopts} # Alex
#./run_hafs.py ${opts} 2022062718-2022070206 02L HISTORY ${confopts} # Bonnie
#./run_hafs.py ${opts} 2022070206-2022070300 03L HISTORY ${confopts} # Colin
#./run_hafs.py ${opts} 2022082000-2022082006 04L HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2022090112-2022090812 05L HISTORY ${confopts} # Danielle
#./run_hafs.py ${opts} 2022090300-2022091018 06L HISTORY ${confopts} # Earl
#./run_hafs.py ${opts} 2022091412-2022092418 07L HISTORY ${confopts} # Fiona
#./run_hafs.py ${opts} 2022092012-2022092518 08L HISTORY ${confopts} # Gaston
#./run_hafs.py ${opts} 2022092306-2022100106 09L HISTORY ${confopts} # Ian
#./run_hafs.py ${opts} 2022092312-2022092500 10L HISTORY ${confopts} # Hermine
#./run_hafs.py ${opts} 2022092812-2022092912 11L HISTORY ${confopts} # Eleven
#./run_hafs.py ${opts} 2022100418-2022100618 12L HISTORY ${confopts} # Twelve
#./run_hafs.py ${opts} 2022100612-2022100912 13L HISTORY ${confopts} # Julia
#./run_hafs.py ${opts} 2022101118-2022101506 14L HISTORY ${confopts} # Karl
#./run_hafs.py ${opts} 2022103018-2022110506 15L HISTORY ${confopts} # Lisa
#./run_hafs.py ${opts} 2022110106-2022110312 16L HISTORY ${confopts} # Martin
#./run_hafs.py ${opts} 2022110706-2022111100 17L HISTORY ${confopts} # Nicole

 # 2021 NATL Storms
#./run_hafs.py ${opts} 2021052206-2021052318 01L HISTORY ${confopts} # Ana
#./run_hafs.py ${opts} 2021061506-2021061518 02L HISTORY ${confopts} # Bill
#./run_hafs.py ${opts} 2021061906-2021062200 03L HISTORY ${confopts} # Claudette
#./run_hafs.py ${opts} 2021062818-2021062900 04L HISTORY ${confopts} # Danny
#./run_hafs.py ${opts} 2021070100-2021070918 05L HISTORY ${confopts} # Elsa
#./run_hafs.py ${opts} 2021080918-2021081700 06L HISTORY ${confopts} # Fred
#./run_hafs.py ${opts} 2021081312-2021082112 07L HISTORY ${confopts} # Grace
#./run_hafs.py ${opts} 2021081600-2021082300 08L HISTORY ${confopts} # Henri
#./run_hafs.py ${opts} 2021082612-2021083012 09L HISTORY ${confopts} # Ida
#./run_hafs.py ${opts} 2021083012-2021090112 10L HISTORY ${confopts} # Kate
#./run_hafs.py ${opts} 2021082912-2021083000 11L HISTORY ${confopts} # Julian
#./run_hafs.py ${opts} 2021083118-2021091106 12L HISTORY ${confopts} # Larry
#./run_hafs.py ${opts} 2021090900-2021091000 13L HISTORY ${confopts} # Mindy
#./run_hafs.py ${opts} 2021091218-2021091500 14L HISTORY ${confopts} # Nicholas
#./run_hafs.py ${opts} 2021091718-2021091818 15L HISTORY ${confopts} # Odette part 1
#./run_hafs.py ${opts} 2021092112-2021092406 15L HISTORY ${confopts} # Odette part 2
#./run_hafs.py ${opts} 2021091906-2021092100 16L HISTORY ${confopts} # Peter part 1
#./run_hafs.py ${opts} 2021092112-2021092300 16L HISTORY ${confopts} # Peter part 2
#./run_hafs.py ${opts} 2021092618-2021092818 16L HISTORY ${confopts} # Peter part 3
#./run_hafs.py ${opts} 2021091918-2021092300 17L HISTORY ${confopts} # Rose
#./run_hafs.py ${opts} 2021092300-2021100500 18L HISTORY ${confopts} # Sam
#./run_hafs.py ${opts} 2021092418-2021092512 19L HISTORY ${confopts} # Teresa
#./run_hafs.py ${opts} 2021092918-2021100412 20L HISTORY ${confopts} # Victor
#./run_hafs.py ${opts} 2021103100-2021110706 21L HISTORY ${confopts} # Wanda

 # 2020 NATL storms
#./run_hafs.py ${opts} 2020051606-2020051918 01L HISTORY ${confopts} # Arthur
#./run_hafs.py ${opts} 2020052700-2020052718 02L HISTORY ${confopts} # Bertha
#./run_hafs.py ${opts} 2020060112-2020060812 03L HISTORY ${confopts} # Cristobal hwrfdata_PROD2020HDOBS
#./run_hafs.py ${opts} 2020062112-2020062406 04L HISTORY ${confopts} # Dolly
#./run_hafs.py ${opts} 2020070400-2020070612 05L HISTORY ${confopts} # Edouard
#./run_hafs.py ${opts} 2020070912-2020071100 06L HISTORY ${confopts} # Fay
#./run_hafs.py ${opts} 2020072112-2020072518 07L HISTORY ${confopts} # Gonzalo
#./run_hafs.py ${opts} 2020072212-2020072618 08L HISTORY ${confopts} # Hanna
#./run_hafs.py ${opts} 2020072806-2020080500 09L HISTORY ${confopts} # Isaias
#./run_hafs.py ${opts} 2020073018-2020080118 10L HISTORY ${confopts} # Ten
#./run_hafs.py ${opts} 2020081100-2020081612 11L HISTORY ${confopts} # Josephine
#./run_hafs.py ${opts} 2020081412-2020081600 12L HISTORY ${confopts} # Kyle
#./run_hafs.py ${opts} 2020081918-2020082718 13L HISTORY ${confopts} # Laura
#./run_hafs.py ${opts} 2020081818-2020082500 14L HISTORY ${confopts} # Marco
#./run_hafs.py ${opts} 2020083112-2020090512 15L HISTORY ${confopts} # Omar
#./run_hafs.py ${opts} 2020083112-2020090318 16L HISTORY ${confopts} # Nana
#./run_hafs.py ${opts} 2020090612-2020091612 17L HISTORY ${confopts} # Paulette part1
#./run_hafs.py ${opts} 2020091806-2020091818 17L HISTORY ${confopts} # Paulette part2
#./run_hafs.py ${opts} 2020091906-2020092300 17L HISTORY ${confopts} # Paulette part3
#./run_hafs.py ${opts} 2020090700-2020091412 18L HISTORY ${confopts} # Rene
#./run_hafs.py ${opts} 2020091112-2020091618 19L HISTORY ${confopts} # Sally
#./run_hafs.py ${opts} 2020091212-2020092306 20L HISTORY ${confopts} # Teddy
#./run_hafs.py ${opts} 2020091318-2020091712 21L HISTORY ${confopts} # Vicky
#./run_hafs.py ${opts} 2020091700-2020092218 22L HISTORY ${confopts} # Beta
#./run_hafs.py ${opts} 2020091718-2020092100 23L HISTORY ${confopts} # Wilfred
#./run_hafs.py ${opts} 2020091818-2020091900 24L HISTORY ${confopts} # Alpha # Do not need to run
#./run_hafs.py ${opts} 2020100118-2020100600 25L HISTORY ${confopts} # Gamma
#./run_hafs.py ${opts} 2020100312-2020101012 26L HISTORY ${confopts} # Delta
#./run_hafs.py ${opts} 2020101618-2020102600 27L HISTORY ${confopts} # Epsilon
#./run_hafs.py ${opts} 2020102412-2020102912 28L HISTORY ${confopts} # Zeta

date

echo 'cronjob done'
