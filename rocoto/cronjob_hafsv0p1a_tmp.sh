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
 HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/hafsv0p1a_202005
#dev="-s sites/xjet.ent -f"
 dev="-s sites/xjet_hafsv0p1a.ent -f"
 PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
#HOMEhafs=/work/noaa/hwrf/save/${USER}/HAFS
#dev="-s sites/orion.ent -f"
#PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================
# hafs.v0.1a near real-time parallel for 2020 NATL storms
 confopts="config.EXPT=${EXPT} config.SUBEXPT=hafsv0p1acpl_202007 \
           config.run_hrdgraphics=yes \
           config.run_emcgraphics=yes \
           ../parm/hafsv0p1aL91_AL.conf \
           ../parm/hafs_hycom.conf"
# Add this option if pgrb2b GFS BC files do not exist
#          config.bctype=gfsgrib2_0p25 \

#${PYTHON3} ./run_hafs.py -t ${dev} 2020082400 00L HISTORY ${confopts}

#===============================================================================
# hafs.v0.1a near real-time parallel for 2020 EPAC storms
 confopts="config.EXPT=${EXPT} config.SUBEXPT=hafsv0p1acpl_202007 \
           config.run_hrdgraphics=no \
           config.run_emcgraphics=yes \
           ../parm/hafsv0p1aL91_EP.conf \
           ../parm/hafs_hycom.conf"
#${PYTHON3} ./run_hafs.py -t ${dev} 2020072812 00E HISTORY ${confopts}

#===============================================================================
# hafs.v0.1a near real-time parallel for 2020 WPAC storms
 confopts="config.EXPT=${EXPT} config.SUBEXPT=hafsv0p1acpl_202007 \
           config.run_hrdgraphics=no \
           config.run_emcgraphics=yes \
           ../parm/hafsv0p1aL91_WP.conf \
           ../parm/hafs_hycom.conf"
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081100 00W HISTORY ${confopts}

#===============================================================================

date

echo 'cronjob done'
