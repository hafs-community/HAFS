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
# HAFSv2.1.1A pre-baseline
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p1p1a_baseline ../parm/hafsv2p1p1a_baseline.conf "
## Technical testing for Helene 09L2024
#./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} \
#   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
#===============================================================================
 # 2024 NATL Storms
#./run_hafs.py ${opts} 2024061800-2024062012 01L HISTORY ${confopts} # Alberto
#./run_hafs.py ${opts} 2024062818-2024070818 02L HISTORY ${confopts} # Beryl
#./run_hafs.py ${opts} 2024063018-2024070100 03L HISTORY ${confopts} # Chris
#./run_hafs.py ${opts} 2024080218-2024080812 04L HISTORY ${confopts} # Debby
#./run_hafs.py ${opts} 2024081118-2024082006 05L HISTORY ${confopts} # Ernesto
#./run_hafs.py ${opts} 2024090818-2024091200 06L HISTORY ${confopts} # Francine
#./run_hafs.py ${opts} 2024091112-2024091806 07L HISTORY ${confopts} # Gordon
#./run_hafs.py ${opts} 2024091518-2024091612 08L HISTORY ${confopts} # Eight
#./run_hafs.py ${opts} 2024092312-2024092712 09L HISTORY ${confopts} # Helene
#./run_hafs.py ${opts} 2024092600-2024093006 10L HISTORY ${confopts} # Isaac
#./run_hafs.py ${opts} 2024092712-2024100100 11L HISTORY ${confopts} # Joyce
#./run_hafs.py ${opts} 2024092918-2024100706 12L HISTORY ${confopts} # Kirk
#./run_hafs.py ${opts} 2024100212-2024101212 13L HISTORY ${confopts} # Leslie
#./run_hafs.py ${opts} 2024100512-2024101012 14L HISTORY ${confopts} # Milton
#./run_hafs.py ${opts} 2024101818-2024102006 15L HISTORY ${confopts} # Nadine
#./run_hafs.py ${opts} 2024101906-2024102212 16L HISTORY ${confopts} # Oscar
#./run_hafs.py ${opts} 2024110118-2024110406 17L HISTORY ${confopts} # Patty
#./run_hafs.py ${opts} 2024110400-2024111012 18L HISTORY ${confopts} # Rafael
#./run_hafs.py ${opts} 2024111318-2024111800 19L HISTORY ${confopts} # Sara

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

#===============================================================================
 # 2024 EPAC storms
#./run_hafs.py ${opts} 2024070412-2024070518 01E HISTORY ${confopts} # Aletta
#./run_hafs.py ${opts} 2024072418-2024072618 02E HISTORY ${confopts} # Bud
#./run_hafs.py ${opts} 2024073112-2024080600 03E HISTORY ${confopts} # Carlotta
#./run_hafs.py ${opts} 2024080312-2024080518 04E HISTORY ${confopts} # Daniel
#./run_hafs.py ${opts} 2024080412-2024080818 05E HISTORY ${confopts} # Emilia
#./run_hafs.py ${opts} 2024080518-2024080712 06E HISTORY ${confopts} # Fabio
#./run_hafs.py ${opts} 2024081812-2024083000 07E HISTORY ${confopts} # Gilma
#./run_hafs.py ${opts} 2024082518-2024082906 08E HISTORY ${confopts} # Hector
#./run_hafs.py ${opts} 2024091212-2024091506 09E HISTORY ${confopts} # Ileana
#./run_hafs.py ${opts} 2024092218-2024092412 10E HISTORY ${confopts} # John part 1
#./run_hafs.py ${opts} 2024092500-2024092718 10E HISTORY ${confopts} # John part 2
#./run_hafs.py ${opts} 2024100118-2024100312 11E HISTORY ${confopts} # Eleven
#./run_hafs.py ${opts} 2024102118-2024102706 12E HISTORY ${confopts} # Kristy
#./run_hafs.py ${opts} 2024110118-2024110306 13E HISTORY ${confopts} # Lane
#./run_hafs.py ${opts} 2024110612-2024110712 14E HISTORY ${confopts} # Fourteen

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

#===============================================================================
 # 2024 WPAC Storms
#./run_hafs.py ${opts} 2024052418-2024053100 01W HISTORY ${confopts} # Ewiniar
#./run_hafs.py ${opts} 2024053100-2024060100 02W HISTORY ${confopts} # Maliksi
#./run_hafs.py ${opts} 2024071500-2024071512 03W HISTORY ${confopts} # Three
#./run_hafs.py ${opts} 2024071918-2024072306 04W HISTORY ${confopts} # Prapiroon
#./run_hafs.py ${opts} 2024071918-2024072518 05W HISTORY ${confopts} # Gaemi
#./run_hafs.py ${opts} 2024080700-2024081300 06W HISTORY ${confopts} # Maria
#./run_hafs.py ${opts} 2024081206-2024081312 07W HISTORY ${confopts} # Son-Tinh
#./run_hafs.py ${opts} 2024081212-2024081806 08W HISTORY ${confopts} # Ampil
#./run_hafs.py ${opts} 2024081300-2024081500 09W HISTORY ${confopts} # Wukong
#./run_hafs.py ${opts} 2024081900-2024082018 10W HISTORY ${confopts} # Jongdari
#./run_hafs.py ${opts} 2024082112-2024090112 11W HISTORY ${confopts} # Shanshan
#./run_hafs.py ${opts} 2024090112-2024090718 12W HISTORY ${confopts} # Yagi
#./run_hafs.py ${opts} 2024090400-2024090700 13W HISTORY ${confopts} # Leepi
#./run_hafs.py ${opts} 2024091000-2024091600 14W HISTORY ${confopts} # Bebinca
#./run_hafs.py ${opts} 2024091618-2024092112 15W HISTORY ${confopts} # Pulasan
#./run_hafs.py ${opts} 2024091806-2024091912 16W HISTORY ${confopts} # Soulik
#./run_hafs.py ${opts} 2024092100-2024092206 17W HISTORY ${confopts} # Seventeen
#./run_hafs.py ${opts} 2024092412-2024092712 18W HISTORY ${confopts} # Cimaron
#./run_hafs.py ${opts} 2024092612-2024100206 19W HISTORY ${confopts} # Jebi
#./run_hafs.py ${opts} 2024092706-2024100318 20W HISTORY ${confopts} # Krathon
#./run_hafs.py ${opts} 2024100606-2024101012 21W HISTORY ${confopts} # Barijat
#./run_hafs.py ${opts} 2024102012-2024102900 22W HISTORY ${confopts} # Trami
#./run_hafs.py ${opts} 2024102500-2024110118 23W HISTORY ${confopts} # Kong-Rey
#./run_hafs.py ${opts} 2024110306-2024111018 24W HISTORY ${confopts} # Yinxing
#./run_hafs.py ${opts} 2024110900-2024111412 25W HISTORY ${confopts} # Man-Yi part 1
#./run_hafs.py ${opts} 2024111500-2024111812 25W HISTORY ${confopts} # Man-Yi part 2
#./run_hafs.py ${opts} 2024110906-2024111318 26W HISTORY ${confopts} # Toraji
#./run_hafs.py ${opts} 2024111100-2024111612 27W HISTORY ${confopts} # Usagi
#./run_hafs.py ${opts} 2024122218-2024122600 28W HISTORY ${confopts} # Pabuk

 # 2023 WPAC Storms
#./run_hafs.py ${opts} 2023042012-2023042206 01W HISTORY ${confopts} # Sanvu
#./run_hafs.py ${opts} 2023052012-2023060306 02W HISTORY ${confopts} # Mawar
#./run_hafs.py ${opts} 2023060600-2023061206 03W HISTORY ${confopts} # Guchol
#./run_hafs.py ${opts} 2023071500-2023071806 04W HISTORY ${confopts} # Talim
#./run_hafs.py ${opts} 2023072112-2023072806 05W HISTORY ${confopts} # Doksuri
#./run_hafs.py ${opts} 2023072706-2023081018 06W HISTORY ${confopts} # Khanun
#./run_hafs.py ${opts} 2023080718-2023081712 07W HISTORY ${confopts} # Lan
#./run_hafs.py ${opts} 2023082306-2023082806 08W HISTORY ${confopts} # Damrey
#./run_hafs.py ${opts} 2023082318-2023090312 09W HISTORY ${confopts} # Saola
#./run_hafs.py ${opts} 2023082812-2023090418 10W HISTORY ${confopts} # Haikui
#./run_hafs.py ${opts} 2023083000-2023090400 11W HISTORY ${confopts} # Kirogi
#./run_hafs.py ${opts} 2023090518-2023090818 12W HISTORY ${confopts} # Yun-Yeung
#./run_hafs.py ${opts} 2023092506-2023092518 13W HISTORY ${confopts} # Thirteen
#./run_hafs.py ${opts} 2023092912-2023101000 14W HISTORY ${confopts} # Koinu
#./run_hafs.py ${opts} 2023100700-2023101412 15W HISTORY ${confopts} # Bolaven
#./run_hafs.py ${opts} 2023101800-2023102100 16W HISTORY ${confopts} # Sanba
#./run_hafs.py ${opts} 2023111212-2023111512 17W HISTORY ${confopts} # Seventeen
#./run_hafs.py ${opts} 2023121718-2023121812 18W HISTORY ${confopts} # Jelawat

#===============================================================================
 # 2024 NIO storms
#./run_hafs.py ${opts} 2024052512-2024052700 01B HISTORY ${confopts} # Remal
#./run_hafs.py ${opts} 2024083000-2024090200 02A HISTORY ${confopts} # Asna
#./run_hafs.py ${opts} 2024102306-2024102518 03B HISTORY ${confopts} # Dana
#./run_hafs.py ${opts} 2024112918-2024120312 04B HISTORY ${confopts} # Fengal

 # 2023 NIO storms
#./run_hafs.py ${opts} 2023051018-2023051418 01B HISTORY ${confopts} # Mocha
#./run_hafs.py ${opts} 2023060600-2023061612 02A HISTORY ${confopts} # Biparjoy
#./run_hafs.py ${opts} 2023060912-2023061000 03B HISTORY ${confopts} # Three
#./run_hafs.py ${opts} 2023073112-2023080112 04B HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2023102006-2023102406 05A HISTORY ${confopts} # Tej
#./run_hafs.py ${opts} 2023102306-2023102500 06B HISTORY ${confopts} # Hamoon
#./run_hafs.py ${opts} 2023111618-2023111712 07B HISTORY ${confopts} # Midhili
#./run_hafs.py ${opts} 2023120300-2023120418 08B HISTORY ${confopts} # Michaung

#===============================================================================

date

echo 'cronjob done'
