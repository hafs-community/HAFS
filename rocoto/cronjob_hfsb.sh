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

## HFSB with production computation resources on WCOSS2
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb ../parm/hfsb.conf"
## Technical testing for Hurricane Ida
#./run_hafs.py ${opts} 2021082712-2021082718 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## HFSB with dev computation resources
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_dev ../parm/hfsb_dev.conf"
## Technical testing for Hurricane Ida
#./run_hafs.py ${opts} 2021082712-2021082718 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

#===============================================================================
 # 2023 NATL Storms
#./run_hafs.py ${opts} 2023060218-2023060312 02L HISTORY ${confopts} # Arlene
#./run_hafs.py ${opts} 2023061918-2023062412 03L HISTORY ${confopts} # Bret
#./run_hafs.py ${opts} 2023062206-2023062600 04L HISTORY ${confopts} # Cindy
#./run_hafs.py ${opts} 2023071406-2023071606 05L HISTORY ${confopts} # Don part 1
#./run_hafs.py ${opts} 2023071618-2023072406 05L HISTORY ${confopts} # Don part 2
#./run_hafs.py ${opts} 2023081918-2023082212 06L HISTORY ${confopts} # Gert part 1
#./run_hafs.py ${opts} 2023083106-2023090412 06L HISTORY ${confopts} # Gert part 2
#./run_hafs.py ${opts} 2023082012-2023082112 07L HISTORY ${confopts} # Emily part 1
#./run_hafs.py ${opts} 2023082300-2023082512 07L HISTORY ${confopts} # Emily part 2
#./run_hafs.py ${opts} 2023082018-2023082312 08L HISTORY ${confopts} # Franklin part 1
#./run_hafs.py ${opts} 2023082318-2023090112 08L HISTORY ${confopts} # Franklin part 2
#./run_hafs.py ${opts} 2023082112-2023082212 09L HISTORY ${confopts} # Harold
#./run_hafs.py ${opts} 2023082618-2023090218 10L HISTORY ${confopts} # Idalia
#./run_hafs.py ${opts} 2023082912-2023090118 11L HISTORY ${confopts} # Jose
#./run_hafs.py ${opts} 2023090112-2023090412 12L HISTORY ${confopts} # Katia
#./run_hafs.py ${opts} 2023090512-2023091606 13L HISTORY ${confopts} # Lee part 1
#./run_hafs.py ${opts} 2023091618-2023091712 13L HISTORY ${confopts} # Lee part 2
#./run_hafs.py ${opts} 2023090712-2023091712 14L HISTORY ${confopts} # Margot
#./run_hafs.py ${opts} 2023091512-2023092206 15L HISTORY ${confopts} # Nigel
#./run_hafs.py ${opts} 2023092112-2023092318 16L HISTORY ${confopts} # Ophelia
#./run_hafs.py ${opts} 2023092312-2023100612 17L HISTORY ${confopts} # Philippe
#./run_hafs.py ${opts} 2023092818-2023100200 18L HISTORY ${confopts} # Rina
#./run_hafs.py ${opts} 2023101100-2023101518 19L HISTORY ${confopts} # Sean
#./run_hafs.py ${opts} 2023101818-2023102900 20L HISTORY ${confopts} # Tammy
#./run_hafs.py ${opts} 2023102318-2023102406 21L HISTORY ${confopts} # Twenty-on
#./run_hafs.py ${opts} 2023111618-2023111718 22L HISTORY ${confopts} # Twenty-tw

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
#./run_hafs.py ${opts} 2020091818-2020091900 24L HISTORY ${confopts} # Alpha # No need to run
#./run_hafs.py ${opts} 2020100118-2020100600 25L HISTORY ${confopts} # Gamma
#./run_hafs.py ${opts} 2020100312-2020101012 26L HISTORY ${confopts} # Delta
#./run_hafs.py ${opts} 2020101618-2020102600 27L HISTORY ${confopts} # Epsilon
#./run_hafs.py ${opts} 2020102412-2020102806 28L HISTORY ${confopts} # Zeta # GFS input data missing after 2020102806

#===============================================================================
 # 2023 EPAC storms
#./run_hafs.py ${opts} 2023062718-2023070212 01E HISTORY ${confopts} # Adrian
#./run_hafs.py ${opts} 2023062900-2023070112 02E HISTORY ${confopts} # Beatriz
#./run_hafs.py ${opts} 2023071118-2023071912 03E HISTORY ${confopts} # Calvin
#./run_hafs.py ${opts} 2023072106-2023072206 04E HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2023073118-2023081300 05E HISTORY ${confopts} # Dora
#./run_hafs.py ${opts} 2023080512-2023080712 06E HISTORY ${confopts} # Eugene
#./run_hafs.py ${opts} 2023081218-2023081706 07E HISTORY ${confopts} # Fernanda
#./run_hafs.py ${opts} 2023081400-2023081718 08E HISTORY ${confopts} # Greg
#./run_hafs.py ${opts} 2023081612-2023082018 09E HISTORY ${confopts} # Hilary
#./run_hafs.py ${opts} 2023082700-2023082912 10E HISTORY ${confopts} # Irwin
#./run_hafs.py ${opts} 2023090418-2023091018 11E HISTORY ${confopts} # Jova
#./run_hafs.py ${opts} 2023091518-2023091618 12E HISTORY ${confopts} # Twelve
#./run_hafs.py ${opts} 2023091912-2023092212 13E HISTORY ${confopts} # Kenneth
#./run_hafs.py ${opts} 2023092318-2023092418 14E HISTORY ${confopts} # Fourteen
#./run_hafs.py ${opts} 2023100306-2023101100 15E HISTORY ${confopts} # Lidia
#./run_hafs.py ${opts} 2023100800-2023101000 16E HISTORY ${confopts} # Max
#./run_hafs.py ${opts} 2023101718-2023102306 17E HISTORY ${confopts} # Norma
#./run_hafs.py ${opts} 2023102212-2023102512 18E HISTORY ${confopts} # Otis
#./run_hafs.py ${opts} 2023102818-2023110600 19E HISTORY ${confopts} # Pilar
#./run_hafs.py ${opts} 2023112312-2023112612 20E HISTORY ${confopts} # Ramon

 # 2022 EPAC storms
#./run_hafs.py ${opts} 2022052800-2022053106 01E HISTORY ${confopts} # Agatha
#./run_hafs.py ${opts} 2022061412-2022062012 02E HISTORY ${confopts} # Blas
#./run_hafs.py ${opts} 2022061618-2022062812 03E HISTORY ${confopts} # Celia
#./run_hafs.py ${opts} 2022071212-2022071912 04E HISTORY ${confopts} # Bonnie
#./run_hafs.py ${opts} 2022070918-2022071618 05E HISTORY ${confopts} # Darby
#./run_hafs.py ${opts} 2022071512-2022072112 06E HISTORY ${confopts} # Estelle
#./run_hafs.py ${opts} 2022072606-2022080212 07E HISTORY ${confopts} # Frank
#./run_hafs.py ${opts} 2022072712-2022080318 08E HISTORY ${confopts} # Georgette
#./run_hafs.py ${opts} 2022080612-2022081018 09E HISTORY ${confopts} # Howard
#./run_hafs.py ${opts} 2022081318-2022081618 10E HISTORY ${confopts} # Ivette p1
#./run_hafs.py ${opts} 2022082012-2022082100 10E HISTORY ${confopts} # Ivette p2
#./run_hafs.py ${opts} 2022082112-2022082118 10E HISTORY ${confopts} # Ivette p3
#./run_hafs.py ${opts} 2022090118-2022090400 11E HISTORY ${confopts} # Javier
#./run_hafs.py ${opts} 2022090412-2022090918 12E HISTORY ${confopts} # Kay
#./run_hafs.py ${opts} 2022091518-2022091712 13E HISTORY ${confopts} # Lester
#./run_hafs.py ${opts} 2022091718-2022092012 14E HISTORY ${confopts} # Madeline
#./run_hafs.py ${opts} 2022092118-2022092518 15E HISTORY ${confopts} # Newton p1
#./run_hafs.py ${opts} 2022092718-2022092818 15E HISTORY ${confopts} # Newton p2
#./run_hafs.py ${opts} 2022092900-2022100318 16E HISTORY ${confopts} # Orlene
#./run_hafs.py ${opts} 2022100318-2022100512 17E HISTORY ${confopts} # Paine
#./run_hafs.py ${opts} 2022100918-2022101012 18E HISTORY ${confopts} # Julia
#./run_hafs.py ${opts} 2022102000-2022102318 19E HISTORY ${confopts} # Roslyn

 # 2021 EPAC storms
#./run_hafs.py ${opts} 2021050906-2021051106 01E HISTORY ${confopts} # Andres
#./run_hafs.py ${opts} 2021053018-2021060406 02E HISTORY ${confopts} # Blanca
#./run_hafs.py ${opts} 2021061218-2021061606 03E HISTORY ${confopts} # Carlos
#./run_hafs.py ${opts} 2021061806-2021061918 04E HISTORY ${confopts} # Dolores
#./run_hafs.py ${opts} 2021062606-2021063012 05E HISTORY ${confopts} # Enrique
#./run_hafs.py ${opts} 2021071406-2021072100 06E HISTORY ${confopts} # Felicia
#./run_hafs.py ${opts} 2021071712-2021072012 07E HISTORY ${confopts} # Guillermo
#./run_hafs.py ${opts} 2021073018-2021080606 08E HISTORY ${confopts} # Hilda
#./run_hafs.py ${opts} 2021073018-2021080618 09E HISTORY ${confopts} # Jimena
#./run_hafs.py ${opts} 2021080118-2021080312 10E HISTORY ${confopts} # Ignacio
#./run_hafs.py ${opts} 2021080712-2021081206 11E HISTORY ${confopts} # Kevin
#./run_hafs.py ${opts} 2021081006-2021082012 12E HISTORY ${confopts} # Linda
#./run_hafs.py ${opts} 2021082306-2021082418 13E HISTORY ${confopts} # Marty
#./run_hafs.py ${opts} 2021082518-2021083000 14E HISTORY ${confopts} # Nora
#./run_hafs.py ${opts} 2021090718-2021091100 15E HISTORY ${confopts} # Olaf
#./run_hafs.py ${opts} 2021101006-2021101312 16E HISTORY ${confopts} # Pamela
#./run_hafs.py ${opts} 2021102212-2021102512 17E HISTORY ${confopts} # Rick
#./run_hafs.py ${opts} 2021110412-2021111018 18E HISTORY ${confopts} # Terry
#./run_hafs.py ${opts} 2021110712-2021110912 19E HISTORY ${confopts} # Sandra

 # 2020 EPAC storms
#./run_hafs.py ${opts} 2020042506-2020042612 01E HISTORY ${confopts} # One
#./run_hafs.py ${opts} 2020053012-2020053112 02E HISTORY ${confopts} # Amanda
#./run_hafs.py ${opts} 2020062418-2020062800 03E HISTORY ${confopts} # Boris
#./run_hafs.py ${opts} 2020062912-2020063012 04E HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2020070612-2020071300 05E HISTORY ${confopts} # Cristina
#./run_hafs.py ${opts} 2020071306-2020071418 06E HISTORY ${confopts} # Six
#./run_hafs.py ${opts} 2020072000-2020072106 07E HISTORY ${confopts} # Seven
#./run_hafs.py ${opts} 2020072006-2020072912 08E HISTORY ${confopts} # Douglas
#./run_hafs.py ${opts} 2020080812-2020081300 09E HISTORY ${confopts} # Elida
#./run_hafs.py ${opts} 2020081300-2020081612 10E HISTORY ${confopts} # Ten
#./run_hafs.py ${opts} 2020081518-2020081712 11E HISTORY ${confopts} # Fausto
#./run_hafs.py ${opts} 2020081512-2020082112 12E HISTORY ${confopts} # Genevieve
#./run_hafs.py ${opts} 2020082512-2020082812 13E HISTORY ${confopts} # Hernan
#./run_hafs.py ${opts} 2020082600-2020083012 14E HISTORY ${confopts} # Iselle
#./run_hafs.py ${opts} 2020090506-2020090700 15E HISTORY ${confopts} # Julio
#./run_hafs.py ${opts} 2020091218-2020091618 16E HISTORY ${confopts} # Karina
#./run_hafs.py ${opts} 2020092012-2020092512 17E HISTORY ${confopts} # Lowell
#./run_hafs.py ${opts} 2020092900-2020100618 18E HISTORY ${confopts} # Marie
#./run_hafs.py ${opts} 2020100400-2020101006 19E HISTORY ${confopts} # Norbert p1
#./run_hafs.py ${opts} 2020101112-2020101500 19E HISTORY ${confopts} # Norbert p2
#./run_hafs.py ${opts} 2020110306-2020110600 20E HISTORY ${confopts} # Odalys
#./run_hafs.py ${opts} 2020111718-2020111912 21E HISTORY ${confopts} # Polo

#===============================================================================

date

echo 'cronjob done'
