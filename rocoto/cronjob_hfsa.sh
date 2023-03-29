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

## HFSA with production computation resources on WCOSS2
#opts="-t -s sites/wcoss2_prod.ent -f"
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa ../parm/hfsa.conf"
## Technical testing for Hurricane Ida
#./run_hafs.py ${opts} 2021082712-2021082718 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## HFSA with dev computation resources and one-way wave coupling
#opts="-t -f"
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_dev_ww3 ../parm/hfsa_dev_ww3.conf"
## Technical testing for Hurricane Ida
#./run_hafs.py ${opts} 2021082712-2021082718 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## HFSA with dev computation resources and without wave coupling
#opts="-t -f"
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_dev ../parm/hfsa_dev.conf"
## Technical testing for Hurricane Ida
#./run_hafs.py ${opts} 2021082712-2021082718 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## HFSA with dev computation resources for JTWC storms (no DA and without wave coupling)
#opts="-t -f"
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_jtwc_dev ../parm/hfsa_dev.conf"
## Technical testing for Noru
#./run_hafs.py ${opts} 2022092400-2022092406 18W HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Mandous
#./run_hafs.py ${opts} 2022120812-2022120818 06B HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Darian
#./run_hafs.py ${opts} 2022121900-2022121906 05S HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

#===============================================================================
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
 # 2022 WPAC Storms
#./run_hafs.py ${opts} 2022033018-2022033100 01W HISTORY ${confopts} # One
#./run_hafs.py ${opts} 2022040700-2022041506 02W HISTORY ${confopts} # Malakas
#./run_hafs.py ${opts} 2022040906-2022041218 03W HISTORY ${confopts} # Megi
#./run_hafs.py ${opts} 2022062918-2022070212 04W HISTORY ${confopts} # Chaba
#./run_hafs.py ${opts} 2022063018-2022070900 05W HISTORY ${confopts} # Aere
#./run_hafs.py ${opts} 2022072900-2022080100 06W HISTORY ${confopts} # Songda
#./run_hafs.py ${opts} 2022080100-2022080106 07W HISTORY ${confopts} # Trases
#./run_hafs.py ${opts} 2022080400-2022080406 08W HISTORY ${confopts} # Eight
#./run_hafs.py ${opts} 2022081106-2022081412 09W HISTORY ${confopts} # Meari
#./run_hafs.py ${opts} 2022082206-2022082512 10W HISTORY ${confopts} # Ma-on
#./run_hafs.py ${opts} 2022082206-2022082512 11W HISTORY ${confopts} # Tokage
#./run_hafs.py ${opts} 2022082806-2022090606 12W HISTORY ${confopts} # Hinnamnor
#./run_hafs.py ${opts} 2022083018-2022090100 13W HISTORY ${confopts} # Thirteen
#./run_hafs.py ${opts} 2022090618-2022091600 14W HISTORY ${confopts} # Muifa
#./run_hafs.py ${opts} 2022091018-2022091500 15W HISTORY ${confopts} # Merbok
#./run_hafs.py ${opts} 2022091218-2022091918 16W HISTORY ${confopts} # Nanmado
#./run_hafs.py ${opts} 2022092118-2022092400 17W HISTORY ${confopts} # Talas
#./run_hafs.py ${opts} 2022092200-2022092806 18W HISTORY ${confopts} # Noru
#./run_hafs.py ${opts} 2022092518-2022092900 19W HISTORY ${confopts} # Kulap
#./run_hafs.py ${opts} 2022092806-2022100500 20W HISTORY ${confopts} # Roke
#./run_hafs.py ${opts} 2022101212-2022101506 21W HISTORY ${confopts} # Twenty-on
#./run_hafs.py ${opts} 2022101400-2022101500 22W HISTORY ${confopts} # Sonca
#./run_hafs.py ${opts} 2022101418-2022102012 23W HISTORY ${confopts} # Nesat
#./run_hafs.py ${opts} 2022101800-2022101906 24W HISTORY ${confopts} # Haitang
#./run_hafs.py ${opts} 2022102000-2022102300 25W HISTORY ${confopts} # Twenty-fi
#./run_hafs.py ${opts} 2022102700-2022110312 26W HISTORY ${confopts} # Nalgae
#./run_hafs.py ${opts} 2022103018-2022103118 27W HISTORY ${confopts} # Banyan
#./run_hafs.py ${opts} 2022111212-2022111418 28W HISTORY ${confopts} # Yamaneko
#./run_hafs.py ${opts} 2022121018-2022121218 29W HISTORY ${confopts} # Pakhar

 # 2021 WPAC Storms
#./run_hafs.py ${opts} 2021021706-2021022200 01W HISTORY ${confopts} # Dujuan
#./run_hafs.py ${opts} 2021041312-2021042506 02W HISTORY ${confopts} # Surigae
#./run_hafs.py ${opts} 2021051218-2021051500 03W HISTORY ${confopts} # Three
#./run_hafs.py ${opts} 2021052918-2021060518 04W HISTORY ${confopts} # Choi-wan
#./run_hafs.py ${opts} 2021061200-2021061218 05W HISTORY ${confopts} # Koguma
#./run_hafs.py ${opts} 2021062100-2021062718 06W HISTORY ${confopts} # Champi
#./run_hafs.py ${opts} 2021070418-2021070606 07W HISTORY ${confopts} # Seven
#./run_hafs.py ${opts} 2021070712-2021070712 08W HISTORY ${confopts} # Eight
#./run_hafs.py ${opts} 2021071606-2021072918 09W HISTORY ${confopts} # In-fa
#./run_hafs.py ${opts} 2021071818-2021072406 10W HISTORY ${confopts} # Cempaka
#./run_hafs.py ${opts} 2021072318-2021072818 11W HISTORY ${confopts} # Nepartak
#./run_hafs.py ${opts} 2021080206-2021080600 12W HISTORY ${confopts} # Twelve
#./run_hafs.py ${opts} 2021080218-2021081000 13W HISTORY ${confopts} # Lupit
#./run_hafs.py ${opts} 2021080406-2021080918 14W HISTORY ${confopts} # Mirinae
#./run_hafs.py ${opts} 2021080412-2021080706 15W HISTORY ${confopts} # Nida
#./run_hafs.py ${opts} 2021081012-2021082400 16W HISTORY ${confopts} # Omais
#./run_hafs.py ${opts} 2021090200-2021090318 17W HISTORY ${confopts} # Seventeen
#./run_hafs.py ${opts} 2021090600-2021091306 18W HISTORY ${confopts} # Conson
#./run_hafs.py ${opts} 2021090606-2021091812 19W HISTORY ${confopts} # Chanthu
#./run_hafs.py ${opts} 2021092212-2021100200 20W HISTORY ${confopts} # Mindulle
#./run_hafs.py ${opts} 2021092212-2021092312 21W HISTORY ${confopts} # Twenty-on
#./run_hafs.py ${opts} 2021100706-2021101012 22W HISTORY ${confopts} # Lionrock
#./run_hafs.py ${opts} 2021101000-2021101718 23W HISTORY ${confopts} # Namtheun
#./run_hafs.py ${opts} 2021101006-2021101412 24W HISTORY ${confopts} # Kompasu
#./run_hafs.py ${opts} 2021102400-2021102912 25W HISTORY ${confopts} # Malou
#./run_hafs.py ${opts} 2021102606-2021102700 26W HISTORY ${confopts} # Twenty-si
#./run_hafs.py ${opts} 2021112918-2021120400 27W HISTORY ${confopts} # Nyatoh
#./run_hafs.py ${opts} 2021121300-2021122112 28W HISTORY ${confopts} # Rai
#./run_hafs.py ${opts} 2021121618-2021121706 29W HISTORY ${confopts} # Twenty-ni

#===============================================================================
 # 2022 NIO storms
#./run_hafs.py ${opts} 2022030418-2022030612 01B HISTORY ${confopts} # One
#./run_hafs.py ${opts} 2022050706-2022051112 02B HISTORY ${confopts} # Asani
#./run_hafs.py ${opts} 2022081206-2022081312 03A HISTORY ${confopts} # Three
#./run_hafs.py ${opts} 2022081818-2022082000 04B HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2022102312-2022102418 05B HISTORY ${confopts} # Sitrang
#./run_hafs.py ${opts} 2022120800-2022121000 06B HISTORY ${confopts} # Mandous
#./run_hafs.py ${opts} 2022121500-2022121718 07A HISTORY ${confopts} # Seven

 # 2021 NIO storms
#./run_hafs.py ${opts} 2021030418-2021030612 01A HISTORY ${confopts} # Tauktae
#./run_hafs.py ${opts} 2021050706-2021051112 02B HISTORY ${confopts} # Yaas
#./run_hafs.py ${opts} 2021081206-2021081312 03B HISTORY ${confopts} # Gulab Shaheen
#./run_hafs.py ${opts} 2021081818-2021082000 04B HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2021102312-2021102418 05B HISTORY ${confopts} # Jawad

#===============================================================================
 # 2022 SH storms
#./run_hafs.py ${opts} 2021123112-2022010418 04P HISTORY ${confopts} # Seth
#./run_hafs.py ${opts} 2022010818-2022011406 05P HISTORY ${confopts} # Cody
#./run_hafs.py ${opts} 2022010906-2022011500 06P HISTORY ${confopts} # Tiffany
#./run_hafs.py ${opts} 2022012318-2022012412 07S HISTORY ${confopts} # Ana
#./run_hafs.py ${opts} 2022012700-2022020812 08S HISTORY ${confopts} # Batsirai
#./run_hafs.py ${opts} 2022013118-2022020318 09P HISTORY ${confopts} # Nine
#./run_hafs.py ${opts} 2022020406-2022020712 10S HISTORY ${confopts} # Cliff
#./run_hafs.py ${opts} 2022020900-2022021200 11P HISTORY ${confopts} # Dovi
#./run_hafs.py ${opts} 2022021500-2022021612 12S HISTORY ${confopts} # Dumako
#./run_hafs.py ${opts} 2022021618-2022022418 13S HISTORY ${confopts} # Emnati
#./run_hafs.py ${opts} 2022022506-2022030412 14S HISTORY ${confopts} # Vernon
#./run_hafs.py ${opts} 2022022512-2022030312 15S HISTORY ${confopts} # Anika
#./run_hafs.py ${opts} 2022030206-2022030312 17S HISTORY ${confopts} # Seventeen
#./run_hafs.py ${opts} 2022030300-2022030418 18P HISTORY ${confopts} # Eighteen
#./run_hafs.py ${opts} 2022030800-2022031512 19S HISTORY ${confopts} # Gombe
#./run_hafs.py ${opts} 2022031318-2022031712 20S HISTORY ${confopts} # Billy
#./run_hafs.py ${opts} 2022032012-2022032618 21S HISTORY ${confopts} # Charlotte
#./run_hafs.py ${opts} 2022032312-2022040306 22S HISTORY ${confopts} # Halima
#./run_hafs.py ${opts} 2022040406-2022040418 23P HISTORY ${confopts} # Twenty-th
#./run_hafs.py ${opts} 2022042400-2022042900 24S HISTORY ${confopts} # Jasmine
#./run_hafs.py ${opts} 2022050712-2022051112 25S HISTORY ${confopts} # Karim
#./run_hafs.py ${opts} 2022051812-2022052218 26P HISTORY ${confopts} # Gina
#./run_hafs.py ${opts} 2022072900-2022080100 01S HISTORY ${confopts} # One
#./run_hafs.py ${opts} 2022092618-2022092806 02S HISTORY ${confopts} # Ashley
#./run_hafs.py ${opts} 2022100600-2022100912 03S HISTORY ${confopts} # Balita
#./run_hafs.py ${opts} 2022110318-2022110618 04S HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2022121806-2023010100 05S HISTORY ${confopts} # Darian

 # 2023 SH storms
#./run_hafs.py ${opts} 2023010200-2023010400 06S HISTORY ${confopts} # Ellie
#./run_hafs.py ${opts} 2023010618-2023010806 07P HISTORY ${confopts} # Hale
#./run_hafs.py ${opts} 2023011718-2023013006 08S HISTORY ${confopts} # Cheneso
#./run_hafs.py ${opts} 2023011806-2023011912 09P HISTORY ${confopts} # Irene
#./run_hafs.py ${opts} 2023012018-2023012112 10P HISTORY ${confopts} # Ten

#===============================================================================

date

echo 'cronjob done'
