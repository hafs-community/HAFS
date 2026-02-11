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
# HAFSv2.2A final configuration
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p2a_final ../parm/hafsv2p2a_final.conf"
## Technical testing for Helene 09L2024
#./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} \
#   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
#===============================================================================
 # 2025 NATL Storms
#./run_hafs.py ${opts} 2025062300-2025062418 01L HISTORY ${confopts} # Andrea
#./run_hafs.py ${opts} 2025062818-2025063000 02L HISTORY ${confopts} # Barry
#./run_hafs.py ${opts} 2025070412-2025070612 03L HISTORY ${confopts} # Chantal
#./run_hafs.py ${opts} 2025080306-2025080618 04L HISTORY ${confopts} # Dexter
#./run_hafs.py ${opts} 2025081006-2025082212 05L HISTORY ${confopts} # Erin
#./run_hafs.py ${opts} 2025082300-2025082800 06L HISTORY ${confopts} # Fernand
#./run_hafs.py ${opts} 2025091600-2025092512 07L HISTORY ${confopts} # Gabrielle
#./run_hafs.py ${opts} 2025092400-2025100106 08L HISTORY ${confopts} # Humberto
#./run_hafs.py ${opts} 2025092612-2025100206 09L HISTORY ${confopts} # Imelda
#./run_hafs.py ${opts} 2025100618-2025101112 10L HISTORY ${confopts} # Jerry
#./run_hafs.py ${opts} 2025100900-2025101012 11L HISTORY ${confopts} # Karen
#./run_hafs.py ${opts} 2025101206-2025101512 12L HISTORY ${confopts} # Lorenzo
#./run_hafs.py ${opts} 2025101812-2025103106 13L HISTORY ${confopts} # Melissa

 # 2024 NATL Storms
#./run_hafs.py ${opts} 2024061712-2024062012 01L HISTORY ${confopts} # Alberto
#./run_hafs.py ${opts} 2024062800-2024070818 02L HISTORY ${confopts} # Beryl
#./run_hafs.py ${opts} 2024062812-2024070100 03L HISTORY ${confopts} # Chris
#./run_hafs.py ${opts} 2024080218-2024080812 04L HISTORY ${confopts} # Debby
#./run_hafs.py ${opts} 2024081118-2024082006 05L HISTORY ${confopts} # Ernesto
#./run_hafs.py ${opts} 2024090806-2024091200 06L HISTORY ${confopts} # Francine
#./run_hafs.py ${opts} 2024091106-2024091806 07L HISTORY ${confopts} # Gordon
#./run_hafs.py ${opts} 2024091500-2024091612 08L HISTORY ${confopts} # Eight
#./run_hafs.py ${opts} 2024092312-2024092712 09L HISTORY ${confopts} # Helene
#./run_hafs.py ${opts} 2024092506-2024093006 10L HISTORY ${confopts} # Isaac
#./run_hafs.py ${opts} 2024092612-2024100100 11L HISTORY ${confopts} # Joyce
#./run_hafs.py ${opts} 2024092906-2024100706 12L HISTORY ${confopts} # Kirk
#./run_hafs.py ${opts} 2024100112-2024101212 13L HISTORY ${confopts} # Leslie
#./run_hafs.py ${opts} 2024100500-2024101012 14L HISTORY ${confopts} # Milton
#./run_hafs.py ${opts} 2024101812-2024102006 15L HISTORY ${confopts} # Nadine
#./run_hafs.py ${opts} 2024101900-2024102212 16L HISTORY ${confopts} # Oscar
#./run_hafs.py ${opts} 2024110118-2024110406 17L HISTORY ${confopts} # Patty
#./run_hafs.py ${opts} 2024110312-2024111012 18L HISTORY ${confopts} # Rafael
#./run_hafs.py ${opts} 2024111318-2024111800 19L HISTORY ${confopts} # Sara

 # 2023 NATL Storms
#./run_hafs.py ${opts} 2023060106-2023060312 02L HISTORY ${confopts} # Arlene
#./run_hafs.py ${opts} 2023061900-2023062412 03L HISTORY ${confopts} # Bret
#./run_hafs.py ${opts} 2023062200-2023062600 04L HISTORY ${confopts} # Cindy
#./run_hafs.py ${opts} 2023071212-2023071606 05L HISTORY ${confopts} # Don part 1
#./run_hafs.py ${opts} 2023071618-2023072406 05L HISTORY ${confopts} # Don part 2
#./run_hafs.py ${opts} 2023081818-2023082212 06L HISTORY ${confopts} # Gert part 1
#./run_hafs.py ${opts} 2023083100-2023090412 06L HISTORY ${confopts} # Gert part 2
#./run_hafs.py ${opts} 2023081818-2023082112 07L HISTORY ${confopts} # Emily part 1
#./run_hafs.py ${opts} 2023082212-2023082512 07L HISTORY ${confopts} # Emily part 2
#./run_hafs.py ${opts} 2023082000-2023090112 08L HISTORY ${confopts} # Franklin
#./run_hafs.py ${opts} 2023082106-2023082212 09L HISTORY ${confopts} # Harold
#./run_hafs.py ${opts} 2023082618-2023090218 10L HISTORY ${confopts} # Idalia
#./run_hafs.py ${opts} 2023082900-2023090118 11L HISTORY ${confopts} # Jose
#./run_hafs.py ${opts} 2023083112-2023090412 12L HISTORY ${confopts} # Katia
#./run_hafs.py ${opts} 2023090500-2023091606 13L HISTORY ${confopts} # Lee part 1
#./run_hafs.py ${opts} 2023091618-2023091712 13L HISTORY ${confopts} # Lee part 2
#./run_hafs.py ${opts} 2023090700-2023091712 14L HISTORY ${confopts} # Margot
#./run_hafs.py ${opts} 2023091418-2023092206 15L HISTORY ${confopts} # Nigel
#./run_hafs.py ${opts} 2023092112-2023092318 16L HISTORY ${confopts} # Ophelia
#./run_hafs.py ${opts} 2023092300-2023100612 17L HISTORY ${confopts} # Philippe
#./run_hafs.py ${opts} 2023092606-2023100200 18L HISTORY ${confopts} # Rina
#./run_hafs.py ${opts} 2023101012-2023101518 19L HISTORY ${confopts} # Sean
#./run_hafs.py ${opts} 2023101712-2023102900 20L HISTORY ${confopts} # Tammy
#./run_hafs.py ${opts} 2023102318-2023102406 21L HISTORY ${confopts} # Twenty-on
#./run_hafs.py ${opts} 2023111618-2023111718 22L HISTORY ${confopts} # Twenty-tw

#===============================================================================
 # 2025 EPAC storms
#./run_hafs.py ${opts} 2025 01E HISTORY ${confopts} # Alvin
#./run_hafs.py ${opts} 2025 02E HISTORY ${confopts} # Barbara
#./run_hafs.py ${opts} 2025 03E HISTORY ${confopts} # Cosme
#./run_hafs.py ${opts} 2025 04E HISTORY ${confopts} # Dalila
#./run_hafs.py ${opts} 2025 05E HISTORY ${confopts} # Erick
#./run_hafs.py ${opts} 2025 06E HISTORY ${confopts} # Flossie
#./run_hafs.py ${opts} 2025 07E HISTORY ${confopts} # Gil
#./run_hafs.py ${opts} 2025 08E HISTORY ${confopts} # Henriette
#./run_hafs.py ${opts} 2025 09E HISTORY ${confopts} # Ivo
#./run_hafs.py ${opts} 2025 10E HISTORY ${confopts} # Juliette
#./run_hafs.py ${opts} 2025 11E HISTORY ${confopts} # Kiko
#./run_hafs.py ${opts} 2025 12E HISTORY ${confopts} # Lorena
#./run_hafs.py ${opts} 2025 13E HISTORY ${confopts} # Mario
#./run_hafs.py ${opts} 2025 14E HISTORY ${confopts} # Narda
#./run_hafs.py ${opts} 2025 15E HISTORY ${confopts} # Octave
#./run_hafs.py ${opts} 2025 16E HISTORY ${confopts} # Priscilla
#./run_hafs.py ${opts} 2025 17E HISTORY ${confopts} # Raymond
#./run_hafs.py ${opts} 2025 18E HISTORY ${confopts} # Sonia

 # 2024 EPAC storms
#./run_hafs.py ${opts} 2024 01E HISTORY ${confopts} # Aletta
#./run_hafs.py ${opts} 2024 02E HISTORY ${confopts} # Bud
#./run_hafs.py ${opts} 2024 03E HISTORY ${confopts} # Carlotta
#./run_hafs.py ${opts} 2024 04E HISTORY ${confopts} # Daniel
#./run_hafs.py ${opts} 2024 05E HISTORY ${confopts} # Emilia
#./run_hafs.py ${opts} 2024 06E HISTORY ${confopts} # Fabio
#./run_hafs.py ${opts} 2024 07E HISTORY ${confopts} # Gilma
#./run_hafs.py ${opts} 2024 08E HISTORY ${confopts} # Hector
#./run_hafs.py ${opts} 2024 09E HISTORY ${confopts} # Ileana
#./run_hafs.py ${opts} 2024 10E HISTORY ${confopts} # John part 1
#./run_hafs.py ${opts} 2024 10E HISTORY ${confopts} # John part 2
#./run_hafs.py ${opts} 2024 11E HISTORY ${confopts} # Eleven
#./run_hafs.py ${opts} 2024 12E HISTORY ${confopts} # Kristy
#./run_hafs.py ${opts} 2024 13E HISTORY ${confopts} # Lane
#./run_hafs.py ${opts} 2024 14E HISTORY ${confopts} # Fourteen

 # 2023 EPAC storms
#./run_hafs.py ${opts} 2023 01E HISTORY ${confopts} # Adrian
#./run_hafs.py ${opts} 2023 02E HISTORY ${confopts} # Beatriz
#./run_hafs.py ${opts} 2023 03E HISTORY ${confopts} # Calvin
#./run_hafs.py ${opts} 2023 04E HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2023 05E HISTORY ${confopts} # Dora
#./run_hafs.py ${opts} 2023 06E HISTORY ${confopts} # Eugene
#./run_hafs.py ${opts} 2023 07E HISTORY ${confopts} # Fernanda
#./run_hafs.py ${opts} 2023 08E HISTORY ${confopts} # Greg
#./run_hafs.py ${opts} 2023 09E HISTORY ${confopts} # Hilary
#./run_hafs.py ${opts} 2023 10E HISTORY ${confopts} # Irwin
#./run_hafs.py ${opts} 2023 11E HISTORY ${confopts} # Jova
#./run_hafs.py ${opts} 2023 12E HISTORY ${confopts} # Twelve
#./run_hafs.py ${opts} 2023 13E HISTORY ${confopts} # Kenneth
#./run_hafs.py ${opts} 2023 14E HISTORY ${confopts} # Fourteen
#./run_hafs.py ${opts} 2023 15E HISTORY ${confopts} # Lidia
#./run_hafs.py ${opts} 2023 16E HISTORY ${confopts} # Max
#./run_hafs.py ${opts} 2023 17E HISTORY ${confopts} # Norma
#./run_hafs.py ${opts} 2023 18E HISTORY ${confopts} # Otis
#./run_hafs.py ${opts} 2023 19E HISTORY ${confopts} # Pilar
#./run_hafs.py ${opts} 2023 20E HISTORY ${confopts} # Ramon

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
