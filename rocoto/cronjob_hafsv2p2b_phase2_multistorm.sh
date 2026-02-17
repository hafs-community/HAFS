#!/bin/sh
set -x
date

HOMEhafs=/scratch4/HFIP/hur-aoml/Lew.Gramer/hafsv2p2_phase2_multistorm.FINAL_MERGE
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
#opts="-t -s sites/${WHERE_AM_I:-wcoss2}.ent -f"
opts="-t -f"
#===============================================================================
# HAFSv2.2B phase2 configuration
 confopts="config.EXPT=${EXPT} ../parm/hafsv2p2b_phase2.conf"

# # Technical testing for Helene 09L2024
# # ./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} config.SUBEXPT=${EXPT}_original \
# #    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes

# # Technical testing for Helene 09L2024
# ./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_singlestorm \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Technical testing for Melissa 13L2025
# ./run_hafs.py ${opts} 2025102106-2025102112 13L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_singlestorm \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no #Melissa

# # Technical testing for Helene 09L2024 - FROM SCRATCH
# ./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_singlestorm_REDO \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no

# # Incremental testing for Helene 09L2024 - MULTISTORM
# ./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged \
#    grid.nest_grids=3 ../parm/hafs_multistorm.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Incremental testing for Melissa 13L2025 - MULTISTORM
# ./run_hafs.py ${opts} -M L,E 2025102106-2025102112 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged \
#    grid.nest_grids=3 ../parm/hafs_multistorm.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no

# # Incremental testing for Helene 09L2024 - MULTISTORM
# ./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_OLD_CODE \
#    grid.nest_grids=3 ../parm/hafs_multistorm.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Incremental testing for Melissa 13L2025 - MULTISTORM
# ./run_hafs.py ${opts} -M L,E 2025102106-2025102112 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_OLD_CODE \
#    grid.nest_grids=3 ../parm/hafs_multistorm.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no


# # POST RAMSTROM CODE MERGE TESTS

# # Technical testing for Helene 09L2024
# ./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_singlestorm_NEW_CODE \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Technical testing for Melissa 13L2025
# #./run_hafs.py ${opts} 2025102106-2025102112 13L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_singlestorm_NEW_CODE \
# ./run_hafs.py ${opts} 2025102106-2025102218 13L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_singlestorm_NEW_CODE \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no #Melissa

# # Incremental testing for Helene 09L2024 - MULTISTORM
# ./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_NEW_CODE \
#    grid.nest_grids=3 ../parm/hafs_multistorm.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Incremental testing for Melissa 13L2025 - MULTISTORM
# #./run_hafs.py ${opts} -M L,E 2025102106-2025102112 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_NEW_CODE \
# ./run_hafs.py ${opts} -M L,E 2025102106-2025102218 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_NEW_CODE \
#    grid.nest_grids=3 ../parm/hafs_multistorm.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no

# # Incremental testing for Helene 09L2024 - MULTISTORM *ATLANTIC-ONLY DOMAIN*
# ./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic_NEW_CODE \
#    grid.nest_grids=3 ../parm/hafs_no_vida.conf ../parm/hafs_multistorm_atlantic.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Incremental testing for Melissa 13L2025 - MULTISTORM *ATLANTIC-ONLY DOMAIN*
# #./run_hafs.py ${opts} -M L,E 2025102106-2025102112 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic_NEW_CODE \
# ./run_hafs.py ${opts} -M L,E 2025102106-2025102218 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic_NEW_CODE \
#    grid.nest_grids=3 ../parm/hafs_no_vida.conf ../parm/hafs_multistorm_atlantic.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no

# # Incremental testing for Helene 09L2024 - MULTISTORM *ATLANTIC-ONLY DOMAIN* *WITH VI/DA*
# ./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic_VIDA_NEW_CODE \
#    grid.nest_grids=3 ../parm/hafs_multistorm_atlantic.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Incremental testing for Melissa 13L2025 - MULTISTORM *ATLANTIC-ONLY DOMAIN* *WITH VI/DA*
# #./run_hafs.py ${opts} -M L,E 2025102106-2025102112 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic_VIDA_NEW_CODE \
# ./run_hafs.py ${opts} -M L,E 2025102106-2025102218 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic_VIDA_NEW_CODE \
#    grid.nest_grids=3 ../parm/hafs_multistorm_atlantic.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no



# TESTS OF CLONE OF RAMSTROM *ACTUAL* CODE MERGE

# # Technical testing for Helene 09L2024
# ./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} config.SUBEXPT=${EXPT}_unmerged_singlestorm \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Technical testing for Melissa 13L2025
# ./run_hafs.py ${opts} 2025102106-2025102218 13L HISTORY ${confopts} config.SUBEXPT=${EXPT}_unmerged_singlestorm \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no #Melissa

# # Incremental testing for Helene 09L2024 - MULTISTORM
# ./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_unmerged \
#    grid.nest_grids=3 ../parm/hafs_multistorm.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# # Incremental testing for Melissa 13L2025 - MULTISTORM
# ./run_hafs.py ${opts} -M L,E 2025102106-2025102218 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_unmerged \
#    grid.nest_grids=3 ../parm/hafs_multistorm.conf \
#    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no


# TESTS OF FINAL MERGE OF WORKFLOW with RAMSTROM *ACTUAL* CODE MERGE

# Technical testing for Helene 09L2024
./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_singlestorm \
   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# Technical testing for Melissa 13L2025
./run_hafs.py ${opts} 2025102106-2025102218 13L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_singlestorm \
   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no #Melissa

# Incremental testing for Helene 09L2024 - MULTISTORM
./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged \
   grid.nest_grids=3 ../parm/hafs_multistorm.conf \
   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# Incremental testing for Melissa 13L2025 - MULTISTORM
./run_hafs.py ${opts} -M L,E 2025102106-2025102218 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged \
   grid.nest_grids=3 ../parm/hafs_multistorm.conf \
   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no

# Incremental testing for Helene 09L2024 - MULTISTORM *ATLANTIC-ONLY DOMAIN*
./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic \
   grid.nest_grids=3 ../parm/hafs_no_vida.conf ../parm/hafs_multistorm_atlantic.conf \
   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# Incremental testing for Melissa 13L2025 - MULTISTORM *ATLANTIC-ONLY DOMAIN*
./run_hafs.py ${opts} -M L,E 2025102106-2025102218 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic \
   grid.nest_grids=3 ../parm/hafs_no_vida.conf ../parm/hafs_multistorm_atlantic.conf \
   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no

# Incremental testing for Helene 09L2024 - MULTISTORM *ATLANTIC-ONLY DOMAIN* *WITH VI/DA*
./run_hafs.py ${opts} -M L,E 2024092406-2024092412 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic_VIDA
   grid.nest_grids=3 ../parm/hafs_multistorm_atlantic.conf \
   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no
# Incremental testing for Melissa 13L2025 - MULTISTORM *ATLANTIC-ONLY DOMAIN* *WITH VI/DA*
./run_hafs.py ${opts} -M L,E 2025102106-2025102218 00L HISTORY ${confopts} config.SUBEXPT=${EXPT}_merged_Atlantic_VIDA \
   grid.nest_grids=3 ../parm/hafs_multistorm_atlantic.conf \
   config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_hrdgraphics=yes config.run_emcgraphics=no



#===============================================================================
 # 2025 NATL Storms
#./run_hafs.py ${opts} 2025062306-2025062418 01L HISTORY ${confopts} # Andrea
#./run_hafs.py ${opts} 2025062818-2025063000 02L HISTORY ${confopts} # Barry
#./run_hafs.py ${opts} 2025070418-2025070612 03L HISTORY ${confopts} # Chantal
#./run_hafs.py ${opts} 2025080318-2025080618 04L HISTORY ${confopts} # Dexter
#./run_hafs.py ${opts} 2025081112-2025082212 05L HISTORY ${confopts} # Erin
#./run_hafs.py ${opts} 2025082318-2025082800 06L HISTORY ${confopts} # Fernand
#./run_hafs.py ${opts} 2025091700-2025092500 07L HISTORY ${confopts} # Gabrielle
#./run_hafs.py ${opts} 2025092418-2025100106 08L HISTORY ${confopts} # Humberto
#./run_hafs.py ${opts} 2025092618-2025100206 09L HISTORY ${confopts} # Imelda
#./run_hafs.py ${opts} 2025100706-2025101112 10L HISTORY ${confopts} # Jerry
#./run_hafs.py ${opts} 2025100900-2025101012 11L HISTORY ${confopts} # Karen
#./run_hafs.py ${opts} 2025101212-2025101512 12L HISTORY ${confopts} # Lorenzo
#./run_hafs.py ${opts} 2025102100-2025103106 13L HISTORY ${confopts} # Melissa

 # 2024 NATL Storms
#./run_hafs.py ${opts} 2024061712-2024062012 01L HISTORY ${confopts} # Alberto
#./run_hafs.py ${opts} 2024062818-2024070818 02L HISTORY ${confopts} # Beryl
#./run_hafs.py ${opts} 2024063018-2024070100 03L HISTORY ${confopts} # Chris
#./run_hafs.py ${opts} 2024080218-2024080812 04L HISTORY ${confopts} # Debby
#./run_hafs.py ${opts} 2024081118-2024082006 05L HISTORY ${confopts} # Ernesto
#./run_hafs.py ${opts} 2024090806-2024091200 06L HISTORY ${confopts} # Francine
#./run_hafs.py ${opts} 2024091112-2024091806 07L HISTORY ${confopts} # Gordon
#./run_hafs.py ${opts} 2024091500-2024091612 08L HISTORY ${confopts} # Eight
#./run_hafs.py ${opts} 2024092312-2024092712 09L HISTORY ${confopts} # Helene
#./run_hafs.py ${opts} 2024092506-2024093006 10L HISTORY ${confopts} # Isaac
#./run_hafs.py ${opts} 2024092706-2024100100 11L HISTORY ${confopts} # Joyce
#./run_hafs.py ${opts} 2024092918-2024100706 12L HISTORY ${confopts} # Kirk
#./run_hafs.py ${opts} 2024100212-2024101212 13L HISTORY ${confopts} # Leslie
#./run_hafs.py ${opts} 2024100512-2024101012 14L HISTORY ${confopts} # Milton
#./run_hafs.py ${opts} 2024101818-2024102006 15L HISTORY ${confopts} # Nadine
#./run_hafs.py ${opts} 2024101906-2024102212 16L HISTORY ${confopts} # Oscar
#./run_hafs.py ${opts} 2024110118-2024110406 17L HISTORY ${confopts} # Patty
#./run_hafs.py ${opts} 2024110400-2024111012 18L HISTORY ${confopts} # Rafael
#./run_hafs.py ${opts} 2024111318-2024111800 19L HISTORY ${confopts} # Sara

 # 2023 NATL Storms
#./run_hafs.py ${opts} 2023060212-2023060312 02L HISTORY ${confopts} # Arlene
#./run_hafs.py ${opts} 2023061912-2023062412 03L HISTORY ${confopts} # Bret
#./run_hafs.py ${opts} 2023062206-2023062600 04L HISTORY ${confopts} # Cindy
#./run_hafs.py ${opts} 2023071306-2023071606 05L HISTORY ${confopts} # Don part 1
#./run_hafs.py ${opts} 2023071618-2023072406 05L HISTORY ${confopts} # Don part 2
#./run_hafs.py ${opts} 2023081918-2023082212 06L HISTORY ${confopts} # Gert part 1
#./run_hafs.py ${opts} 2023083106-2023090412 06L HISTORY ${confopts} # Gert part 2
#./run_hafs.py ${opts} 2023082000-2023082112 07L HISTORY ${confopts} # Emily part 1
#./run_hafs.py ${opts} 2023082300-2023082512 07L HISTORY ${confopts} # Emily part 2
#./run_hafs.py ${opts} 2023082012-2023090112 08L HISTORY ${confopts} # Franklin
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
#./run_hafs.py ${opts} 2023092812-2023100200 18L HISTORY ${confopts} # Rina
#./run_hafs.py ${opts} 2023101100-2023101518 19L HISTORY ${confopts} # Sean
#./run_hafs.py ${opts} 2023101812-2023102900 20L HISTORY ${confopts} # Tammy
#./run_hafs.py ${opts} 2023102318-2023102406 21L HISTORY ${confopts} # Twenty-on
#./run_hafs.py ${opts} 2023111618-2023111718 22L HISTORY ${confopts} # Twenty-tw

#===============================================================================
 # 2025 EPAC storms
#./run_hafs.py ${opts} 2025052818-2025053106 01E HISTORY ${confopts} # Alvin
#./run_hafs.py ${opts} 2025060800-2025061018 02E HISTORY ${confopts} # Barbara
#./run_hafs.py ${opts} 2025060812-2025061106 03E HISTORY ${confopts} # Cosme
#./run_hafs.py ${opts} 2025061218-2025061518 04E HISTORY ${confopts} # Dalila
#./run_hafs.py ${opts} 2025061700-2025061918 05E HISTORY ${confopts} # Erick
#./run_hafs.py ${opts} 2025062906-2025070312 06E HISTORY ${confopts} # Flossie
#./run_hafs.py ${opts} 2025073100-2025080306 07E HISTORY ${confopts} # Gil
#./run_hafs.py ${opts} 2025080406-2025081300 08E HISTORY ${confopts} # Henriette
#./run_hafs.py ${opts} 2025080612-2025081106 09E HISTORY ${confopts} # Ivo
#./run_hafs.py ${opts} 2025082418-2025082800 10E HISTORY ${confopts} # Juliette
#./run_hafs.py ${opts} 2025083112-2025091006 11E HISTORY ${confopts} # Kiko
#./run_hafs.py ${opts} 2025090200-2025090500 12E HISTORY ${confopts} # Lorena
#./run_hafs.py ${opts} 2025091118-2025091618 13E HISTORY ${confopts} # Mario
#./run_hafs.py ${opts} 2025092112-2025092818 14E HISTORY ${confopts} # Narda
#./run_hafs.py ${opts} 2025093006-2025100906 15E HISTORY ${confopts} # Octave
#./run_hafs.py ${opts} 2025100412-2025101012 16E HISTORY ${confopts} # Priscilla
#./run_hafs.py ${opts} 2025100912-2025101118 17E HISTORY ${confopts} # Raymond
#./run_hafs.py ${opts} 2025102418-2025102900 18E HISTORY ${confopts} # Sonia

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

date

echo 'cronjob done'
