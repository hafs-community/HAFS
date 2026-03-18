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
# Five cycle testing for Melissa 13L2025
#./run_hafs.py ${opts} 2025102300-2025102400 13L HISTORY ${confopts} \
#   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## Technical testing for NHC basins
## Technical testing for Helene 09L2025
#./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## HFSB with dev computation resources
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_dev ../parm/hfsb_dev.conf"
## Technical testing for Helene 09L2025
#./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Kiko 11E2025
#./run_hafs.py ${opts} 2025083112-2025083118 11E HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Iona 01C2025
#./run_hafs.py ${opts} 2025072700-2025072706 01C HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

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
 # 2025 CPAC storms
#./run_hafs.py ${opts} 2025 01C HISTORY ${confopts} # Iona
#./run_hafs.py ${opts} 2025 02C HISTORY ${confopts} # Keli

#===============================================================================

date

echo 'cronjob done'
