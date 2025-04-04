#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/scratch4/AOML/aoml-hafs1/${USER}/hafsv2p1_phase3_multistorm}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
#opts="-t -s sites/${WHERE_AM_I:-wcoss2}.ent -f"
opts="-t -f"

#===============================================================================
 # HAFSv2.1A baseline configuration
 confopts_a="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p1a_baseline ../parm/hafsv2p1a_baseline.conf"
#    dir.COMrtofs=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMRTOFSv2.5_new" # hera
#    dir.COMrtofs=/lfs/h2/emc/hur/noscrub/hafs-input/COMRTOFSv2.5_new" # wcoss2
#    dir.COMrtofs=/work/noaa/hwrf/noscrub/hafs-input/COMRTOFSv2.5_new" # orion/hercules
#    dir.COMrtofs=/lfs5/HFIP/hwrf-data/hafs-input/COMRTOFSv2.5_new" # jet
#    dir.COMrtofs=/gpfs/f6/drsa-hurr1/world-shared/noscrub/hafs-input/COMRTOFSv2.5_new" # gaeaC6

 # Technical testing
#./run_hafs.py ${opts} 2024092506-2024092512 09L HISTORY ${confopts_a} # Helene

#===============================================================================
 # HAFSv2.1B baseline configuration
 confopts_b="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p1b_baseline ../parm/hafsv2p1b_baseline.conf"
#     dir.COMrtofs=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMRTOFSv2.5_new" # hera
#    dir.COMrtofs=/lfs/h2/emc/hur/noscrub/hafs-input/COMRTOFSv2.5_new" # wcoss2
#    dir.COMrtofs=/work/noaa/hwrf/noscrub/hafs-input/COMRTOFSv2.5_new" # orion/hercules
#    dir.COMrtofs=/lfs5/HFIP/hwrf-data/hafs-input/COMRTOFSv2.5_new" # jet
#    dir.COMrtofs=/gpfs/f6/drsa-hurr1/world-shared/noscrub/hafs-input/COMRTOFSv2.5_new" # gaeaC6

 # Technical testing
#./run_hafs.py ${opts} 2024092506-2024092512 09L HISTORY ${confopts_b} # Helene

 # Coldstart test
./run_hafs.py ${opts} 2024100506 14L HISTORY ${confopts_b} # Milton

#===============================================================================
 # HAFSv2.1M baseline configuration
 confopts_m="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p1b_baseline config.run_hrdgraphics=yes ../parm/hafsv2p1m_baseline.conf"
#    dir.COMrtofs=/scratch1/NCEPDEV/hwrf/noscrub/hafs-input/COMRTOFSv2.5_new" # hera
#    dir.COMrtofs=/lfs/h2/emc/hur/noscrub/hafs-input/COMRTOFSv2.5_new" # wcoss2
#    dir.COMrtofs=/work/noaa/hwrf/noscrub/hafs-input/COMRTOFSv2.5_new" # orion/hercules
#    dir.COMrtofs=/lfs5/HFIP/hwrf-data/hafs-input/COMRTOFSv2.5_new" # jet
#    dir.COMrtofs=/gpfs/f6/drsa-hurr1/world-shared/noscrub/hafs-input/COMRTOFSv2.5_new" # gaeaC6

 # Technical testing
#./run_hafs.py ${opts} 2024092506-2024092512 09L HISTORY ${confopts_b} # Helene

 # Coldstart test
./run_hafs.py ${opts} 2024100506 14L HISTORY ${confopts_b} # Milton


#===============================================================================
 # 2024 NATL/EPac Multistorm Blocks
# AL: Beryl, Chris. EP: Aletta.
#./run_hafs.py ${opts} -M L,E 2024062818-2024070818 00L HISTORY ${confopts_m}

# AL: Helene, Isaac, Joyce. EP: John. NOTE: Block ends before Isaac and Joyce full lifecycles
./run_hafs.py ${opts} -M L,E 2024092218-2024092906 00L HISTORY ${confopts_m}

# AL: Kirk, Leslie, Milton. EP: Eleven.
./run_hafs.py ${opts} -M L,E 2024092918-2024101212 00L HISTORY ${confopts_m}


#===============================================================================
 # 2023 NATL/EPac Multistorm Blocks
# AL: Bret, Cindy.
#./run_hafs.py ${opts} -M L,E 2023061918-2023062600 00L HISTORY ${confopts_m}

# AL: Gert, Emily, Franklin, Harold, Idalia, Jose, Katia. EP: Hilary, Irwin.
#./run_hafs.py ${opts} -M L,E 2023081918-2023090412 00L HISTORY ${confopts_m}

# AL: Lee, Margo, Nigel, Ophelia, Philippe, Rina. EP: Jova, Twelve, Kenneth, Fourteen, Lidia.
#./run_hafs.py ${opts} -M L,E 2023090512-2023100612 00L HISTORY ${confopts_m}


#===============================================================================
 # 2024 NATL Storms
#./run_hafs.py ${opts} 2024061800-2024062012 01L HISTORY ${confopts_b} # Alberto
#./run_hafs.py ${opts} 2024062818-2024070818 02L HISTORY ${confopts_b} # Beryl
#./run_hafs.py ${opts} 2024063018-2024070100 03L HISTORY ${confopts_b} # Chris
#./run_hafs.py ${opts} 2024080218-2024080812 04L HISTORY ${confopts_b} # Debby
#./run_hafs.py ${opts} 2024081118-2024082006 05L HISTORY ${confopts_b} # Ernesto
#./run_hafs.py ${opts} 2024090818-2024091200 06L HISTORY ${confopts_b} # Francine
#./run_hafs.py ${opts} 2024091112-2024091806 07L HISTORY ${confopts_b} # Gordon
#./run_hafs.py ${opts} 2024091518-2024091612 08L HISTORY ${confopts_b} # Eight
#./run_hafs.py ${opts} 2024092312-2024092712 09L HISTORY ${confopts_b} # Helene
#./run_hafs.py ${opts} 2024092600-2024093006 10L HISTORY ${confopts_b} # Isaac
#./run_hafs.py ${opts} 2024092712-2024100100 11L HISTORY ${confopts_b} # Joyce
#./run_hafs.py ${opts} 2024092918-2024100706 12L HISTORY ${confopts_b} # Kirk
#./run_hafs.py ${opts} 2024100212-2024101212 13L HISTORY ${confopts_b} # Leslie
#./run_hafs.py ${opts} 2024100512-2024101012 14L HISTORY ${confopts_b} # Milton
#./run_hafs.py ${opts} 2024101818-2024102006 15L HISTORY ${confopts_b} # Nadine
#./run_hafs.py ${opts} 2024101906-2024102212 16L HISTORY ${confopts_b} # Oscar
#./run_hafs.py ${opts} 2024110118-2024110406 17L HISTORY ${confopts_b} # Patty
#./run_hafs.py ${opts} 2024110400-2024111012 18L HISTORY ${confopts_b} # Rafael
#./run_hafs.py ${opts} 2024111318-2024111800 19L HISTORY ${confopts_b} # Sara

 # 2023 NATL Storms
#./run_hafs.py ${opts} 2023060218-2023060312 02L HISTORY ${confopts_b} # Arlene
#./run_hafs.py ${opts} 2023061918-2023062412 03L HISTORY ${confopts_b} # Bret
#./run_hafs.py ${opts} 2023062206-2023062600 04L HISTORY ${confopts_b} # Cindy
#./run_hafs.py ${opts} 2023071406-2023071606 05L HISTORY ${confopts_b} # Don part 1
#./run_hafs.py ${opts} 2023071618-2023072406 05L HISTORY ${confopts_b} # Don part 2
#./run_hafs.py ${opts} 2023081918-2023082212 06L HISTORY ${confopts_b} # Gert part 1
#./run_hafs.py ${opts} 2023083106-2023090412 06L HISTORY ${confopts_b} # Gert part 2
#./run_hafs.py ${opts} 2023082012-2023082112 07L HISTORY ${confopts_b} # Emily part 1
#./run_hafs.py ${opts} 2023082300-2023082512 07L HISTORY ${confopts_b} # Emily part 2
#./run_hafs.py ${opts} 2023082018-2023082312 08L HISTORY ${confopts_b} # Franklin part 1
#./run_hafs.py ${opts} 2023082318-2023090112 08L HISTORY ${confopts_b} # Franklin part 2
#./run_hafs.py ${opts} 2023082112-2023082212 09L HISTORY ${confopts_b} # Harold
#./run_hafs.py ${opts} 2023082618-2023090218 10L HISTORY ${confopts_b} # Idalia
#./run_hafs.py ${opts} 2023082912-2023090118 11L HISTORY ${confopts_b} # Jose
#./run_hafs.py ${opts} 2023090112-2023090412 12L HISTORY ${confopts_b} # Katia
#./run_hafs.py ${opts} 2023090512-2023091606 13L HISTORY ${confopts_b} # Lee part 1
#./run_hafs.py ${opts} 2023091618-2023091712 13L HISTORY ${confopts_b} # Lee part 2
#./run_hafs.py ${opts} 2023090712-2023091712 14L HISTORY ${confopts_b} # Margot
#./run_hafs.py ${opts} 2023091512-2023092206 15L HISTORY ${confopts_b} # Nigel
#./run_hafs.py ${opts} 2023092112-2023092318 16L HISTORY ${confopts_b} # Ophelia
#./run_hafs.py ${opts} 2023092312-2023100612 17L HISTORY ${confopts_b} # Philippe
#./run_hafs.py ${opts} 2023092818-2023100200 18L HISTORY ${confopts_b} # Rina
#./run_hafs.py ${opts} 2023101100-2023101518 19L HISTORY ${confopts_b} # Sean
#./run_hafs.py ${opts} 2023101818-2023102900 20L HISTORY ${confopts_b} # Tammy
#./run_hafs.py ${opts} 2023102318-2023102406 21L HISTORY ${confopts_b} # Twenty-on
#./run_hafs.py ${opts} 2023111618-2023111718 22L HISTORY ${confopts_b} # Twenty-tw

 # 2022 NATL Storms
#./run_hafs.py ${opts} 2022060506-2022060618 01L HISTORY ${confopts_b} # Alex
#./run_hafs.py ${opts} 2022062718-2022070206 02L HISTORY ${confopts_b} # Bonnie
#./run_hafs.py ${opts} 2022070206-2022070300 03L HISTORY ${confopts_b} # Colin
#./run_hafs.py ${opts} 2022082000-2022082006 04L HISTORY ${confopts_b} # Four
#./run_hafs.py ${opts} 2022090112-2022090812 05L HISTORY ${confopts_b} # Danielle
#./run_hafs.py ${opts} 2022090300-2022091018 06L HISTORY ${confopts_b} # Earl
#./run_hafs.py ${opts} 2022091412-2022092418 07L HISTORY ${confopts_b} # Fiona
#./run_hafs.py ${opts} 2022092012-2022092518 08L HISTORY ${confopts_b} # Gaston
#./run_hafs.py ${opts} 2022092306-2022100106 09L HISTORY ${confopts_b} # Ian
#./run_hafs.py ${opts} 2022092312-2022092500 10L HISTORY ${confopts_b} # Hermine
#./run_hafs.py ${opts} 2022092812-2022092912 11L HISTORY ${confopts_b} # Eleven
#./run_hafs.py ${opts} 2022100418-2022100618 12L HISTORY ${confopts_b} # Twelve
#./run_hafs.py ${opts} 2022100612-2022100912 13L HISTORY ${confopts_b} # Julia
#./run_hafs.py ${opts} 2022101118-2022101506 14L HISTORY ${confopts_b} # Karl
#./run_hafs.py ${opts} 2022103018-2022110506 15L HISTORY ${confopts_b} # Lisa
#./run_hafs.py ${opts} 2022110106-2022110312 16L HISTORY ${confopts_b} # Martin
#./run_hafs.py ${opts} 2022110706-2022111100 17L HISTORY ${confopts_b} # Nicole

#===============================================================================
 # 2024 EPAC storms
#./run_hafs.py ${opts} 2024070412-2024070518 01E HISTORY ${confopts_b} # Aletta
#./run_hafs.py ${opts} 2024072418-2024072618 02E HISTORY ${confopts_b} # Bud
#./run_hafs.py ${opts} 2024073112-2024080600 03E HISTORY ${confopts_b} # Carlotta
#./run_hafs.py ${opts} 2024080312-2024080518 04E HISTORY ${confopts_b} # Daniel
#./run_hafs.py ${opts} 2024080412-2024080818 05E HISTORY ${confopts_b} # Emilia
#./run_hafs.py ${opts} 2024080518-2024080712 06E HISTORY ${confopts_b} # Fabio
#./run_hafs.py ${opts} 2024081812-2024083000 07E HISTORY ${confopts_b} # Gilma
#./run_hafs.py ${opts} 2024082518-2024082906 08E HISTORY ${confopts_b} # Hector
#./run_hafs.py ${opts} 2024091212-2024091506 09E HISTORY ${confopts_b} # Ileana
#./run_hafs.py ${opts} 2024092218-2024092412 10E HISTORY ${confopts_b} # John part 1
#./run_hafs.py ${opts} 2024092500-2024092718 10E HISTORY ${confopts_b} # John part 2
#./run_hafs.py ${opts} 2024100118-2024100312 11E HISTORY ${confopts_b} # Eleven
#./run_hafs.py ${opts} 2024102118-2024102706 12E HISTORY ${confopts_b} # Kristy
#./run_hafs.py ${opts} 2024110118-2024110306 13E HISTORY ${confopts_b} # Lane
#./run_hafs.py ${opts} 2024110612-2024110712 14E HISTORY ${confopts_b} # Fourteen

 # 2023 EPAC storms
#./run_hafs.py ${opts} 2023062718-2023070212 01E HISTORY ${confopts_b} # Adrian
#./run_hafs.py ${opts} 2023062900-2023070112 02E HISTORY ${confopts_b} # Beatriz
#./run_hafs.py ${opts} 2023071118-2023071912 03E HISTORY ${confopts_b} # Calvin
#./run_hafs.py ${opts} 2023072106-2023072206 04E HISTORY ${confopts_b} # Four
#./run_hafs.py ${opts} 2023073118-2023081300 05E HISTORY ${confopts_b} # Dora
#./run_hafs.py ${opts} 2023080512-2023080712 06E HISTORY ${confopts_b} # Eugene
#./run_hafs.py ${opts} 2023081218-2023081706 07E HISTORY ${confopts_b} # Fernanda
#./run_hafs.py ${opts} 2023081400-2023081718 08E HISTORY ${confopts_b} # Greg
#./run_hafs.py ${opts} 2023081612-2023082018 09E HISTORY ${confopts_b} # Hilary
#./run_hafs.py ${opts} 2023082700-2023082912 10E HISTORY ${confopts_b} # Irwin
#./run_hafs.py ${opts} 2023090418-2023091018 11E HISTORY ${confopts_b} # Jova
#./run_hafs.py ${opts} 2023091518-2023091618 12E HISTORY ${confopts_b} # Twelve
#./run_hafs.py ${opts} 2023091912-2023092212 13E HISTORY ${confopts_b} # Kenneth
#./run_hafs.py ${opts} 2023092318-2023092418 14E HISTORY ${confopts_b} # Fourteen
#./run_hafs.py ${opts} 2023100306-2023101100 15E HISTORY ${confopts_b} # Lidia
#./run_hafs.py ${opts} 2023100800-2023101000 16E HISTORY ${confopts_b} # Max
#./run_hafs.py ${opts} 2023101718-2023102306 17E HISTORY ${confopts_b} # Norma
#./run_hafs.py ${opts} 2023102212-2023102512 18E HISTORY ${confopts_b} # Otis
#./run_hafs.py ${opts} 2023102818-2023110600 19E HISTORY ${confopts_b} # Pilar
#./run_hafs.py ${opts} 2023112312-2023112612 20E HISTORY ${confopts_b} # Ramon

 # 2022 EPAC storms
#./run_hafs.py ${opts} 2022052800-2022053106 01E HISTORY ${confopts_b} # Agatha
#./run_hafs.py ${opts} 2022061412-2022062012 02E HISTORY ${confopts_b} # Blas
#./run_hafs.py ${opts} 2022061618-2022062812 03E HISTORY ${confopts_b} # Celia
#./run_hafs.py ${opts} 2022071212-2022071912 04E HISTORY ${confopts_b} # Bonnie
#./run_hafs.py ${opts} 2022070918-2022071618 05E HISTORY ${confopts_b} # Darby
#./run_hafs.py ${opts} 2022071512-2022072112 06E HISTORY ${confopts_b} # Estelle
#./run_hafs.py ${opts} 2022072606-2022080212 07E HISTORY ${confopts_b} # Frank
#./run_hafs.py ${opts} 2022072712-2022080318 08E HISTORY ${confopts_b} # Georgette
#./run_hafs.py ${opts} 2022080612-2022081018 09E HISTORY ${confopts_b} # Howard
#./run_hafs.py ${opts} 2022081318-2022081618 10E HISTORY ${confopts_b} # Ivette p1
#./run_hafs.py ${opts} 2022082012-2022082100 10E HISTORY ${confopts_b} # Ivette p2
#./run_hafs.py ${opts} 2022082112-2022082118 10E HISTORY ${confopts_b} # Ivette p3
#./run_hafs.py ${opts} 2022090118-2022090400 11E HISTORY ${confopts_b} # Javier
#./run_hafs.py ${opts} 2022090412-2022090918 12E HISTORY ${confopts_b} # Kay
#./run_hafs.py ${opts} 2022091518-2022091712 13E HISTORY ${confopts_b} # Lester
#./run_hafs.py ${opts} 2022091718-2022092012 14E HISTORY ${confopts_b} # Madeline
#./run_hafs.py ${opts} 2022092118-2022092518 15E HISTORY ${confopts_b} # Newton p1
#./run_hafs.py ${opts} 2022092718-2022092818 15E HISTORY ${confopts_b} # Newton p2
#./run_hafs.py ${opts} 2022092900-2022100318 16E HISTORY ${confopts_b} # Orlene
#./run_hafs.py ${opts} 2022100318-2022100512 17E HISTORY ${confopts_b} # Paine
#./run_hafs.py ${opts} 2022100918-2022101012 18E HISTORY ${confopts_b} # Julia
#./run_hafs.py ${opts} 2022102000-2022102318 19E HISTORY ${confopts_b} # Roslyn

#===============================================================================
 # 2023 WPAC Storms
#./run_hafs.py ${opts} 2023042012-2023042206 01W HISTORY ${confopts_b} # Sanvu
#./run_hafs.py ${opts} 2023052012-2023060306 02W HISTORY ${confopts_b} # Mawar
#./run_hafs.py ${opts} 2023060600-2023061206 03W HISTORY ${confopts_b} # Guchol
#./run_hafs.py ${opts} 2023071500-2023071806 04W HISTORY ${confopts_b} # Talim
#./run_hafs.py ${opts} 2023072112-2023072806 05W HISTORY ${confopts_b} # Doksuri
#./run_hafs.py ${opts} 2023072706-2023081018 06W HISTORY ${confopts_b} # Khanun
#./run_hafs.py ${opts} 2023080718-2023081712 07W HISTORY ${confopts_b} # Lan
#./run_hafs.py ${opts} 2023082306-2023082806 08W HISTORY ${confopts_b} # Damrey
#./run_hafs.py ${opts} 2023082318-2023090312 09W HISTORY ${confopts_b} # Saola
#./run_hafs.py ${opts} 2023082812-2023090418 10W HISTORY ${confopts_b} # Haikui
#./run_hafs.py ${opts} 2023083000-2023090400 11W HISTORY ${confopts_b} # Kirogi
#./run_hafs.py ${opts} 2023090518-2023090818 12W HISTORY ${confopts_b} # Yun-Yeung
#./run_hafs.py ${opts} 2023092506-2023092518 13W HISTORY ${confopts_b} # Thirteen
#./run_hafs.py ${opts} 2023092912-2023101000 14W HISTORY ${confopts_b} # Koinu
#./run_hafs.py ${opts} 2023100700-2023101412 15W HISTORY ${confopts_b} # Bolaven
#./run_hafs.py ${opts} 2023101800-2023102100 16W HISTORY ${confopts_b} # Sanba
#./run_hafs.py ${opts} 2023111212-2023111512 17W HISTORY ${confopts_b} # Seventeen
#./run_hafs.py ${opts} 2023121718-2023121812 18W HISTORY ${confopts_b} # Jelawat

 # 2022 WPAC Storms
#./run_hafs.py ${opts} 2022033018-2022033100 01W HISTORY ${confopts_b} # One
#./run_hafs.py ${opts} 2022040700-2022041506 02W HISTORY ${confopts_b} # Malakas
#./run_hafs.py ${opts} 2022040906-2022041218 03W HISTORY ${confopts_b} # Megi
#./run_hafs.py ${opts} 2022062918-2022070212 04W HISTORY ${confopts_b} # Chaba
#./run_hafs.py ${opts} 2022063018-2022070900 05W HISTORY ${confopts_b} # Aere
#./run_hafs.py ${opts} 2022072900-2022080100 06W HISTORY ${confopts_b} # Songda
#./run_hafs.py ${opts} 2022080100-2022080106 07W HISTORY ${confopts_b} # Trases
#./run_hafs.py ${opts} 2022080400-2022080406 08W HISTORY ${confopts_b} # Eight
#./run_hafs.py ${opts} 2022081106-2022081412 09W HISTORY ${confopts_b} # Meari
#./run_hafs.py ${opts} 2022082206-2022082512 10W HISTORY ${confopts_b} # Ma-on
#./run_hafs.py ${opts} 2022082206-2022082512 11W HISTORY ${confopts_b} # Tokage
#./run_hafs.py ${opts} 2022082806-2022090606 12W HISTORY ${confopts_b} # Hinnamnor
#./run_hafs.py ${opts} 2022083018-2022090100 13W HISTORY ${confopts_b} # Thirteen
#./run_hafs.py ${opts} 2022090618-2022091600 14W HISTORY ${confopts_b} # Muifa
#./run_hafs.py ${opts} 2022091018-2022091500 15W HISTORY ${confopts_b} # Merbok
#./run_hafs.py ${opts} 2022091218-2022091918 16W HISTORY ${confopts_b} # Nanmado
#./run_hafs.py ${opts} 2022092118-2022092400 17W HISTORY ${confopts_b} # Talas
#./run_hafs.py ${opts} 2022092200-2022092806 18W HISTORY ${confopts_b} # Noru
#./run_hafs.py ${opts} 2022092518-2022092900 19W HISTORY ${confopts_b} # Kulap
#./run_hafs.py ${opts} 2022092806-2022100500 20W HISTORY ${confopts_b} # Roke
#./run_hafs.py ${opts} 2022101212-2022101506 21W HISTORY ${confopts_b} # Twenty-on
#./run_hafs.py ${opts} 2022101400-2022101500 22W HISTORY ${confopts_b} # Sonca
#./run_hafs.py ${opts} 2022101418-2022102012 23W HISTORY ${confopts_b} # Nesat
#./run_hafs.py ${opts} 2022101800-2022101906 24W HISTORY ${confopts_b} # Haitang
#./run_hafs.py ${opts} 2022102000-2022102300 25W HISTORY ${confopts_b} # Twenty-fi
#./run_hafs.py ${opts} 2022102700-2022110312 26W HISTORY ${confopts_b} # Nalgae
#./run_hafs.py ${opts} 2022103018-2022103118 27W HISTORY ${confopts_b} # Banyan
#./run_hafs.py ${opts} 2022111212-2022111418 28W HISTORY ${confopts_b} # Yamaneko
#./run_hafs.py ${opts} 2022121018-2022121218 29W HISTORY ${confopts_b} # Pakhar

#===============================================================================
 # 2023 NIO storms
#./run_hafs.py ${opts} 2023051018-2023051418 01B HISTORY ${confopts_b} # Mocha
#./run_hafs.py ${opts} 2023060600-2023061612 02A HISTORY ${confopts_b} # Biparjoy
#./run_hafs.py ${opts} 2023060912-2023061000 03B HISTORY ${confopts_b} # Three
#./run_hafs.py ${opts} 2023073112-2023080112 04B HISTORY ${confopts_b} # Four
#./run_hafs.py ${opts} 2023102006-2023102406 05A HISTORY ${confopts_b} # Tej
#./run_hafs.py ${opts} 2023102306-2023102500 06B HISTORY ${confopts_b} # Hamoon
#./run_hafs.py ${opts} 2023111618-2023111712 07B HISTORY ${confopts_b} # Midhili
#./run_hafs.py ${opts} 2023120300-2023120418 08B HISTORY ${confopts_b} # Michaung

 # 2022 NIO storms
#./run_hafs.py ${opts} 2022030418-2022030612 01B HISTORY ${confopts_b} # One
#./run_hafs.py ${opts} 2022050706-2022051112 02B HISTORY ${confopts_b} # Asani
#./run_hafs.py ${opts} 2022081206-2022081312 03A HISTORY ${confopts_b} # Three
#./run_hafs.py ${opts} 2022081818-2022082000 04B HISTORY ${confopts_b} # Four
#./run_hafs.py ${opts} 2022102312-2022102418 05B HISTORY ${confopts_b} # Sitrang
#./run_hafs.py ${opts} 2022120800-2022121000 06B HISTORY ${confopts_b} # Mandous
#./run_hafs.py ${opts} 2022121500-2022121718 07A HISTORY ${confopts_b} # Seven

#===============================================================================

date

echo 'cronjob done'
