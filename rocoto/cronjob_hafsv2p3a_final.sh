#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
#opts="-t -s sites/${WHERE_AM_I:-wcoss2}.ent -f"
opts="-f"
#===============================================================================
# HAFSv2.3A final configuration
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p3a_final ../parm/hafsv2p3a_final.conf"
##Five cycle testing for Melissa 13L2025
#./run_hafs.py ${opts} 2025102300-2025102400 13L HISTORY ${confopts} \
#   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

#===============================================================================
## HFSA with production computation resources on WCOSS2
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa ../parm/hfsa.conf"
##Five cycle testing for Melissa 13L2025
#./run_hafs.py ${opts} 2025102300-2025102400 13L HISTORY ${confopts} \
#   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## Technical testing for NHC basins
## Technical testing for Helene 09L2025
#./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## HFSA with dev computation resources and one-way wave coupling
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_dev_ww3 ../parm/hfsa_dev_ww3.conf"
## Technical testing for Helene 09L2025
#./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Kiko 11E2025
#./run_hafs.py ${opts} 2025083112-2025083118 11E HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Iona 01C2025
#./run_hafs.py ${opts} 2025072700-2025072706 01C HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## HFSA with dev computation resources and without wave coupling
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_dev ../parm/hfsa_dev.conf"
## Technical testing for Helene 09L2024
#./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

## Technical testing for JTWC basins
## HFSA with dev computation resources for JTWC storms (no DA and without wave coupling)
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_jtwc_dev ../parm/hfsa_dev.conf"
## Technical testing for Ragasa 24W2025
#./run_hafs.py ${opts} 2025091806-2025091812 24W HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Shakhti 02A2025
#./run_hafs.py ${opts} 2025100300-2025100306 02A HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Montha 03B2025
#./run_hafs.py ${opts} 2025102612-2025102618 03B HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Chenge 04S2025
#./run_hafs.py ${opts} 2025101818-2025101900 04S HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
## Technical testing for Urmil 23P2026
#./run_hafs.py ${opts} 2026022518-2026022600 23P HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

#===============================================================================
 # 2025 NATL Storms
#./run_hafs.py ${opts} 2025 01L HISTORY ${confopts} # Andrea
#./run_hafs.py ${opts} 2025 02L HISTORY ${confopts} # Barry
#./run_hafs.py ${opts} 2025 03L HISTORY ${confopts} # Chantal
#./run_hafs.py ${opts} 2025 04L HISTORY ${confopts} # Dexter
#./run_hafs.py ${opts} 2025 05L HISTORY ${confopts} # Erin
#./run_hafs.py ${opts} 2025 06L HISTORY ${confopts} # Fernand
#./run_hafs.py ${opts} 2025 07L HISTORY ${confopts} # Gabrielle
#./run_hafs.py ${opts} 2025 08L HISTORY ${confopts} # Humberto
#./run_hafs.py ${opts} 2025 09L HISTORY ${confopts} # Imelda
#./run_hafs.py ${opts} 2025 10L HISTORY ${confopts} # Jerry
#./run_hafs.py ${opts} 2025 11L HISTORY ${confopts} # Karen
#./run_hafs.py ${opts} 2025 12L HISTORY ${confopts} # Lorenzo
#./run_hafs.py ${opts} 2025 13L HISTORY ${confopts} # Melissa

 # 2024 NATL Storms
#./run_hafs.py ${opts} 2024 01L HISTORY ${confopts} # Alberto
#./run_hafs.py ${opts} 2024 02L HISTORY ${confopts} # Beryl
#./run_hafs.py ${opts} 2024 03L HISTORY ${confopts} # Chris
#./run_hafs.py ${opts} 2024 04L HISTORY ${confopts} # Debby
#./run_hafs.py ${opts} 2024 05L HISTORY ${confopts} # Ernesto
#./run_hafs.py ${opts} 2024 06L HISTORY ${confopts} # Francine
#./run_hafs.py ${opts} 2024 07L HISTORY ${confopts} # Gordon
#./run_hafs.py ${opts} 2024 08L HISTORY ${confopts} # Eight
#./run_hafs.py ${opts} 2024 09L HISTORY ${confopts} # Helene
#./run_hafs.py ${opts} 2024 10L HISTORY ${confopts} # Isaac
#./run_hafs.py ${opts} 2024 11L HISTORY ${confopts} # Joyce
#./run_hafs.py ${opts} 2024 12L HISTORY ${confopts} # Kirk
#./run_hafs.py ${opts} 2024 13L HISTORY ${confopts} # Leslie
#./run_hafs.py ${opts} 2024 14L HISTORY ${confopts} # Milton
#./run_hafs.py ${opts} 2024 15L HISTORY ${confopts} # Nadine
#./run_hafs.py ${opts} 2024 16L HISTORY ${confopts} # Oscar
#./run_hafs.py ${opts} 2024 17L HISTORY ${confopts} # Patty
#./run_hafs.py ${opts} 2024 18L HISTORY ${confopts} # Rafael
#./run_hafs.py ${opts} 2024 19L HISTORY ${confopts} # Sara

 # 2023 NATL Storms
#./run_hafs.py ${opts} 2023 02L HISTORY ${confopts} # Arlene
#./run_hafs.py ${opts} 2023 03L HISTORY ${confopts} # Bret
#./run_hafs.py ${opts} 2023 04L HISTORY ${confopts} # Cindy
#./run_hafs.py ${opts} 2023 05L HISTORY ${confopts} # Don
#./run_hafs.py ${opts} 2023 06L HISTORY ${confopts} # Gert
#./run_hafs.py ${opts} 2023 07L HISTORY ${confopts} # Emily
#./run_hafs.py ${opts} 2023 08L HISTORY ${confopts} # Franklin
#./run_hafs.py ${opts} 2023 09L HISTORY ${confopts} # Harold
#./run_hafs.py ${opts} 2023 10L HISTORY ${confopts} # Idalia
#./run_hafs.py ${opts} 2023 11L HISTORY ${confopts} # Jose
#./run_hafs.py ${opts} 2023 12L HISTORY ${confopts} # Katia
#./run_hafs.py ${opts} 2023 13L HISTORY ${confopts} # Lee
#./run_hafs.py ${opts} 2023 14L HISTORY ${confopts} # Margot
#./run_hafs.py ${opts} 2023 15L HISTORY ${confopts} # Nigel
#./run_hafs.py ${opts} 2023 16L HISTORY ${confopts} # Ophelia
#./run_hafs.py ${opts} 2023 17L HISTORY ${confopts} # Philippe
#./run_hafs.py ${opts} 2023 18L HISTORY ${confopts} # Rina
#./run_hafs.py ${opts} 2023 19L HISTORY ${confopts} # Sean
#./run_hafs.py ${opts} 2023 20L HISTORY ${confopts} # Tammy
#./run_hafs.py ${opts} 2023 21L HISTORY ${confopts} # Twenty-on
#./run_hafs.py ${opts} 2023 22L HISTORY ${confopts} # Twenty-tw

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
#./run_hafs.py ${opts} 2024 10E HISTORY ${confopts} # John
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
 # 2025 WPAC Storms
#./run_hafs.py ${opts} 2025 01W HISTORY ${confopts} # Wutip
#./run_hafs.py ${opts} 2025 02W HISTORY ${confopts} # Sepat
#./run_hafs.py ${opts} 2025 03W HISTORY ${confopts} # Three
#./run_hafs.py ${opts} 2025 04W HISTORY ${confopts} # Mun
#./run_hafs.py ${opts} 2025 05W HISTORY ${confopts} # Danas
#./run_hafs.py ${opts} 2025 06W HISTORY ${confopts} # Nari
#./run_hafs.py ${opts} 2025 07W HISTORY ${confopts} # Seven
#./run_hafs.py ${opts} 2025 08W HISTORY ${confopts} # Eight
#./run_hafs.py ${opts} 2025 09W HISTORY ${confopts} # Wipha
#./run_hafs.py ${opts} 2025 10W HISTORY ${confopts} # Francisco
#./run_hafs.py ${opts} 2025 11W HISTORY ${confopts} # Co-May
#./run_hafs.py ${opts} 2025 12W HISTORY ${confopts} # Krosa
#./run_hafs.py ${opts} 2025 13W HISTORY ${confopts} # Bailu
#./run_hafs.py ${opts} 2025 14W HISTORY ${confopts} # Fourteen
#./run_hafs.py ${opts} 2025 15W HISTORY ${confopts} # Fifteen
#./run_hafs.py ${opts} 2025 16W HISTORY ${confopts} # Podul
#./run_hafs.py ${opts} 2025 17W HISTORY ${confopts} # Seventeen
#./run_hafs.py ${opts} 2025 18W HISTORY ${confopts} # Lingling
#./run_hafs.py ${opts} 2025 19W HISTORY ${confopts} # Kajiki
#./run_hafs.py ${opts} 2025 20W HISTORY ${confopts} # Nongfa
#./run_hafs.py ${opts} 2025 21W HISTORY ${confopts} # Peipah
#./run_hafs.py ${opts} 2025 22W HISTORY ${confopts} # Tapah
#./run_hafs.py ${opts} 2025 23W HISTORY ${confopts} # Mitag
#./run_hafs.py ${opts} 2025 24W HISTORY ${confopts} # Ragasa
#./run_hafs.py ${opts} 2025 25W HISTORY ${confopts} # Neoguri
#./run_hafs.py ${opts} 2025 26W HISTORY ${confopts} # Bualoi
#./run_hafs.py ${opts} 2025 27W HISTORY ${confopts} # Matmo
#./run_hafs.py ${opts} 2025 28W HISTORY ${confopts} # Halong
#./run_hafs.py ${opts} 2025 29W HISTORY ${confopts} # Nakri
#./run_hafs.py ${opts} 2025 30W HISTORY ${confopts} # Fengshen
#./run_hafs.py ${opts} 2025 31W HISTORY ${confopts} # Kalmaegi
#./run_hafs.py ${opts} 2025 32W HISTORY ${confopts} # Fung-Wong
#./run_hafs.py ${opts} 2025 33W HISTORY ${confopts} # Koto
#./run_hafs.py ${opts} 2025 34W HISTORY ${confopts} # Thirtyfour

 # 2024 WPAC Storms
#./run_hafs.py ${opts} 2024 01W HISTORY ${confopts} # Ewiniar
#./run_hafs.py ${opts} 2024 02W HISTORY ${confopts} # Maliksi
#./run_hafs.py ${opts} 2024 03W HISTORY ${confopts} # Three
#./run_hafs.py ${opts} 2024 04W HISTORY ${confopts} # Prapiroon
#./run_hafs.py ${opts} 2024 05W HISTORY ${confopts} # Gaemi
#./run_hafs.py ${opts} 2024 06W HISTORY ${confopts} # Maria
#./run_hafs.py ${opts} 2024 07W HISTORY ${confopts} # Son-Tinh
#./run_hafs.py ${opts} 2024 08W HISTORY ${confopts} # Ampil
#./run_hafs.py ${opts} 2024 09W HISTORY ${confopts} # Wukong
#./run_hafs.py ${opts} 2024 10W HISTORY ${confopts} # Jongdari
#./run_hafs.py ${opts} 2024 11W HISTORY ${confopts} # Shanshan
#./run_hafs.py ${opts} 2024 12W HISTORY ${confopts} # Yagi
#./run_hafs.py ${opts} 2024 13W HISTORY ${confopts} # Leepi
#./run_hafs.py ${opts} 2024 14W HISTORY ${confopts} # Bebinca
#./run_hafs.py ${opts} 2024 15W HISTORY ${confopts} # Pulasan
#./run_hafs.py ${opts} 2024 16W HISTORY ${confopts} # Soulik
#./run_hafs.py ${opts} 2024 17W HISTORY ${confopts} # Seventeen
#./run_hafs.py ${opts} 2024 18W HISTORY ${confopts} # Cimaron
#./run_hafs.py ${opts} 2024 19W HISTORY ${confopts} # Jebi
#./run_hafs.py ${opts} 2024 20W HISTORY ${confopts} # Krathon
#./run_hafs.py ${opts} 2024 21W HISTORY ${confopts} # Barijat
#./run_hafs.py ${opts} 2024 22W HISTORY ${confopts} # Trami
#./run_hafs.py ${opts} 2024 23W HISTORY ${confopts} # Kong-Rey
#./run_hafs.py ${opts} 2024 24W HISTORY ${confopts} # Yinxing
#./run_hafs.py ${opts} 2024 25W HISTORY ${confopts} # Man-Yi
#./run_hafs.py ${opts} 2024 26W HISTORY ${confopts} # Toraji
#./run_hafs.py ${opts} 2024 27W HISTORY ${confopts} # Usagi
#./run_hafs.py ${opts} 2024 28W HISTORY ${confopts} # Pabuk

#===============================================================================
 # 2025 NIO storms
#./run_hafs.py ${opts} 2025 01B HISTORY ${confopts} # One
#./run_hafs.py ${opts} 2025 02A HISTORY ${confopts} # Shakhi
#./run_hafs.py ${opts} 2025 03B HISTORY ${confopts} # Montha
#./run_hafs.py ${opts} 2025 04B HISTORY ${confopts} # Senyar
#./run_hafs.py ${opts} 2025 05B HISTORY ${confopts} # Ditwah

 # 2024 NIO storms
#./run_hafs.py ${opts} 2024 01B HISTORY ${confopts} # Remal
#./run_hafs.py ${opts} 2024 02A HISTORY ${confopts} # Asna
#./run_hafs.py ${opts} 2024 03B HISTORY ${confopts} # Dana
#./run_hafs.py ${opts} 2024 04B HISTORY ${confopts} # Fengal

 # 2023 NIO storms
#./run_hafs.py ${opts} 2023 01B HISTORY ${confopts} # Mocha
#./run_hafs.py ${opts} 2023 02A HISTORY ${confopts} # Biparjoy
#./run_hafs.py ${opts} 2023 03B HISTORY ${confopts} # Three
#./run_hafs.py ${opts} 2023 04B HISTORY ${confopts} # Four
#./run_hafs.py ${opts} 2023 05A HISTORY ${confopts} # Tej
#./run_hafs.py ${opts} 2023 06B HISTORY ${confopts} # Hamoon
#./run_hafs.py ${opts} 2023 07B HISTORY ${confopts} # Midhili
#./run_hafs.py ${opts} 2023 08B HISTORY ${confopts} # Michaung

#===============================================================================

date

echo 'cronjob done'
