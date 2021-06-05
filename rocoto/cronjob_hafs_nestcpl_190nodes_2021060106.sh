#!/bin/sh
set -x
date

# NOAA WCOSS Dell Phase3
#HOMEhafs=/gpfs/dell2/emc/modeling/noscrub/${USER}/save/HAFS
#dev="-s sites/wcoss_dell_p3.ent -f"
#PYTHON3=/usrx/local/prod/packages/python/3.6.3/bin/python3

# NOAA WCOSS Cray
#HOMEhafs=/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS
#dev="-s sites/wcoss_cray.ent -f"
#PYTHON3=/opt/intel/intelpython3/bin/python3

# NOAA RDHPCS Jet
HOMEhafs=/mnt/lfs4/HFIP/hur-aoml/${USER}/multi_nests.20210527
dev="-s sites/xjet.ent -f"
## From ORION ORIGINAL CONFIG
#dev="-s sites/xjet_hafsv0p2a.ent -f"
PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
# HOMEhafs=/work/noaa/aoml-hafs1/${USER}/hafs_nestcpl
# dev="-s sites/orion.ent -f"
# PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})

#===============================================================================

# Technical test for HYCOM coupling - Global FV3, HAFS-B nest coupled to tropical HYCOM
 confopts="config.EXPT=${EXPT} config.SUBEXPT=hafs_multi_nests.20210527_nestcpl_190nodes \
     config.scrub_work=no config.scrub_com=no \
     config.NHRS=168 \
     config.run_emcgraphics=yes \
     config.run_hrdgraphics=yes \
     ../parm/hafs_globnest_static_trop_ocn_190nodes.conf \
"

${PYTHON3} ./run_hafs.py -t ${dev} -w hafs_multi_nests.20210527_nestcpl_190nodes_2021060106.xml -d hafs_multi_nests.20210527_nestcpl_190nodes_2021060106.db 2021060106-2021060506 00L HISTORY ${confopts} 

#===============================================================================

date

echo 'cronjob done'
