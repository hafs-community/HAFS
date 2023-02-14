#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/hafsv1_fnl}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
#opts="-t -s sites/${WHERE_AM_I:-wcoss2}.ent -f"
opts="-t -f"
#===============================================================================

 #hafsv1 hfsa
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_jtwc \
     ../parm/hafsv1_hfsa.conf \
     config.run_atm_init_fgat=no config.run_atm_vi_fgat=no \
     config.run_gsi=no config.run_fgat=no config.run_envar=no config.gsi_d02=no"

 # Technical testing
#./run_hafs.py ${opts} 2022092400 18W HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

#./run_hafs.py ${opts} 2022120800 06B HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

#./run_hafs.py ${opts} 2022121812 05S HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

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
