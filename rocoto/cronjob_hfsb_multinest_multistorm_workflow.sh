#!/bin/sh
set -x
date

#module reset; module purge

# #HOMEhafs=${HOMEhafs:-/work/noaa/hwrf/save/bliu/hafsv1_merge}
# HOMEhafs=${HOMEhafs:-/work2/noaa/aoml-hafs1/lgramer/hafsv1_basin_10x10}
# #HOMEhafs=/work2/noaa/aoml-hafs1/wramstro/basin/HAFS
#HOMEhafs=${HOMEhafs:-/lfs4/HFIP/hur-aoml/Lew.Gramer/src/hafsv1_basin_10x10}
HOMEhafs=${HOMEhafs:-/scratch2/AOML/aoml-hafs1/Lew.Gramer/hafsv1_basin_10x10}

source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
# Lew.Gramer@noaa.gov 2023-11-27
# #opts="-f"
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================
# Example hafs moving nest experiments

#MULTI="-M L,E -m 17L,19L,20L"
MULTI="-m 17L,19L,20L"
##MULTI="-m 17L,19L"
#MULTI="-m 17L,20L"
#MULTI="-m 17L"

# ./run_hafs.py ${opts} 2020091400 19L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsb_regional_5mvnest_storm \
#     config.domlat=20.0 config.domlon=-65.0 \
#     config.NHRS=96 ${scrubopt} \
#     ../parm/hafsb_regional_5mvnest_storm.conf

# ./run_hafs.py ${opts} 2020091400 19L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsb_regional_3mvnest_storm \
#     config.domlat=20.0 config.domlon=-65.0 \
#     config.NHRS=96 ${scrubopt} \
#     ../parm/hafsb_regional_3mvnest_storm.conf

 # ./run_hafs.py ${opts} ${MULTI} 2020091400 00L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multinest_multistorm_ocean \
 #     config.domlat=20.0 config.domlon=-65.0 \
 #     config.run_hrdgraphics=yes \
 #     config.NHRS=126 ${scrubopt} \
 #     ../parm/hfsb_regional_multistorm_ocean_workflow.conf

# COMMENTED OUT 2024-02-02
 # ./run_hafs.py ${opts} -m 17L,19L,20L 2020091400 00L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multinest_multistorm_ocean \
 #     gsi.use_bufr_nr=yes \
 #     config.domlat=20.0 config.domlon=-65.0 \
 #     config.run_hrdgraphics=yes \
 #     config.NHRS=126 ${scrubopt} \
 #     ../parm/hfsb_regional_multistorm_ocean_workflow.conf

 # ./run_hafs.py ${opts} -m 17L 2020091400 00L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multinest_singlestorm_ocean_DA \
 #     gsi.use_bufr_nr=yes \
 #     config.domlat=20.0 config.domlon=-65.0 \
 #     config.run_hrdgraphics=yes \
 #     config.NHRS=126 ${scrubopt} \
 #     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 # ./run_hafs.py ${opts} -m 17L,19L 2020091400 00L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multinest_multistorm_ocean_DA \
 #     gsi.use_bufr_nr=yes \
 #     config.domlat=20.0 config.domlon=-65.0 \
 #     config.run_hrdgraphics=yes \
 #     config.NHRS=126 ${scrubopt} \
 #     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

# # ./run_hafs.py ${opts} -s sites/hera_3storm.ent -m 17L,19L,20L 2020091400 00L HISTORY 
#  ./run_hafs.py ${opts} -m 17L,19L,20L 2020091400 00L HISTORY \
#      config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multinest_3storm_ocean_DA \
#      gsi.use_bufr_nr=yes \
#      config.domlat=20.0 config.domlon=-65.0 \
#      config.run_hrdgraphics=yes \
#      config.NHRS=126 ${scrubopt} \
#      ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf


if [ 0 == 1 ]; then
 ./run_hafs.py ${opts} -m 17L,19L,20L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 20L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_20L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 19L,20L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_19L_20L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 17L,19L,20L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_17L_19L_20L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 17L,18L,19L,20L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_17L_18L_19L_20L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 17L,18L,19L,20L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_noDA_17L_18L_19L_20L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_noDA_workflow.conf

 # Will atm_init_storm1_1 fail? Where? YES! Same place as it did for the 4- and 5-storm cases with *97L*
 ./run_hafs.py ${opts} -m 97L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_97L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 # Will atm_init_storm1_1 fail? Where? YES! Same place as it did for the 4- and 5-storm cases with *16E*
 ./run_hafs.py ${opts} -m 16E 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_16E \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 # Does -M work? How many storms will it start by default? Which ones? Just the four you excpet: 17, 18, 19, 20L
 ./run_hafs.py ${opts} -M L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_minusM_L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 # Multistorm self-cycled DA test
 ./run_hafs.py ${opts} -M L 2020091318-2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_cycledDA_minusM_L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 17L,18L,19L,97L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_17L_18L_19L_97L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 17L,18L,19L,16E 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_17L_18L_19L_16E \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 17L,18L,19L,20L,97L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_17L_18L_19L_20L_97L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 17L,18L,19L,20L,16E 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_17L_18L_19L_20L_16E \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 17L,18L,19L,20L,16E,97L 2020091400 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA_17L_18L_19L_20L_16E_97L \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf
fi

if [ 1 ]; then
 # Self-cycled long block in 2023
 ./run_hafs.py ${opts} -M L 2023083100-2023090218 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_cycledDA_minusM_L_2023 \
     dir.COMgfs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMGFSv16 \
     dir.COMrtofs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMRTOFSv2 \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 # Self-cycled long block in 2023
 ./run_hafs.py ${opts} -M L 2023081900-2023082618 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_cycledDA_minusM_L_2023 \
     dir.COMgfs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMGFSv16 \
     dir.COMrtofs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMRTOFSv2 \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf
fi

if [ 1 ]; then
 ./run_hafs.py ${opts} -m 10L 2023083100-2023090218 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_cycledDA_minusm_10L_2023 \
     dir.COMgfs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMGFSv16 \
     dir.COMrtofs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMRTOFSv2 \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf
fi

if [ 0 == 1 ]; then
 ./run_hafs.py ${opts} -m 08L 2023083100-2023090218 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_cycledDA_minusm_08L_2023 \
     dir.COMgfs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMGFSv16 \
     dir.COMrtofs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMRTOFSv2 \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf

 ./run_hafs.py ${opts} -m 11L 2023083100-2023090218 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_cycledDA_minusm_11L_2023 \
     dir.COMgfs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMGFSv16 \
     dir.COMrtofs=/scratch1/AOML/aoml-hafs1/role.aoml-hafs1/staging/hafs-input/COMRTOFSv2 \
     gsi.use_bufr_nr=yes \
     config.domlat=20.0 config.domlon=-65.0 \
     config.run_hrdgraphics=yes \
     config.NHRS=126 ${scrubopt} \
     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf
fi


 # ./run_hafs.py ${opts} -m 13L,13E,14E 2020082806 00L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multistorm_ocean_DA \
 #     gsi.use_bufr_nr=yes \
 #     config.domlat=20.0 config.domlon=-65.0 \
 #     config.run_hrdgraphics=yes \
 #     config.NHRS=126 ${scrubopt} \
 #     ../parm/hfsb_regional_multistorm_ocean_DA_workflow.conf


 # ./run_hafs.py ${opts} -m 17L 2020091400 00L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multinest_singlestorm_ocean \
 #     config.domlat=20.0 config.domlon=-65.0 \
 #     config.run_hrdgraphics=yes \
 #     config.NHRS=126 ${scrubopt} \
 #     ../parm/hfsb_regional_multistorm_ocean_workflow.conf

 # ./run_hafs.py ${opts} -m 17L 2020091400 00L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multinest_singlestorm_ocean_SPEEDUP \
 #     config.domlat=20.0 config.domlon=-65.0 \
 #     config.run_hrdgraphics=yes \
 #     config.NHRS=126 ${scrubopt} \
 #     ../parm/hfsb_regional_multistorm_ocean_workflow_SPEEDUP.conf

 # ./run_hafs.py ${opts} 2020091400 17L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_regional_multinest_singlestorm_ocean_REGRESSION \
 #     config.run_multistorm=no grid.nest_grids=2 \
 #     config.domlat=20.0 config.domlon=-65.0 \
 #     config.run_hrdgraphics=yes \
 #     config.NHRS=126 ${scrubopt} \
 #     ../parm/hfsb_regional_multistorm_ocean_workflow_SPEEDUP.conf

# ./run_hafs.py ${opts} 2020081718 19L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsb_regional_5mvnest_storm_ocean \
#     config.domlat=20.0 config.domlon=-65.0 \
#     config.NHRS=126 ${scrubopt} \
#     ../parm/hafsb_regional_5mvnest_storm_ocean.conf

# ./run_hafs.py ${opts} 2020091400 19L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsb_regional_2mvnest_storm \
#     config.domlat=20.0 config.domlon=-65.0 \
#     config.NHRS=96 ${scrubopt} \
#     ../parm/hafsb_regional_2mvnest_storm.conf

# ./run_hafs.py ${opts} 2020091400 19L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsb_regional_1mvnest_storm \
#     config.domlat=20.0 config.domlon=-65.0 \
#     config.NHRS=96 ${scrubopt} \
#     ../parm/hafsb_regional_1mvnest_storm.conf


#===============================================================================

date

echo 'cronjob done'
