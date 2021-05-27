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
#HOMEhafs=/mnt/lfs1/HFIP/hwrfv3/${USER}/HAFS
#dev="-s sites/xjet.ent -f"
#dev="-s sites/xjet_hafsv0p1a.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
 HOMEhafs=/work/noaa/hwrf/save/${USER}/HAFS
 dev="-s sites/orion.ent -f"
 PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

 # Cold-start from GFS analysis without DA
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_noda \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # Warm-start from prior HAFS forecast without DA
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_noda_cycling \
     config.warm_start_opt=2 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # Simple 3DVar DA
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3dvar \
     config.run_gsi=yes config.run_envar=no \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with GDAS ensembles
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3denvar \
     config.run_gsi=yes config.run_envar=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSI-based Vortex Relocation (GSIVR) + 3DEnVar with GDAS ensembles
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivr_3denvar \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=no config.run_envar=yes \
     config.run_ensda=no config.ENS_SIZE=0 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with GDAS ensembles + 3hourly FGAT
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3denvar_fgat \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=no config.ENS_SIZE=0 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with HAFS ensembles (cold-start from GDAS ensembles)
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=no config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with HAFS ensembles + 3hourly FGAT
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda_fgat \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + 3DEnVar with HAFS ensembles + 3hourly FGAT
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivr_3densda_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + 3DEnVar with HAFS ensembles + 3hourly FGAT
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivrfgat_3densda_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=no \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + GSIVR_ENS + 3DEnVar with HAFS ensembles (self-cycled through GSIVR_ENS) + 3hourly FGAT
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivrfgatens_3densda_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=yes \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=no \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with self-cycled HAFS enkf ensembles + 3hourly FGAT
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda_enkf_fgat \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + 3DEnVar with self-cycled HAFS enkf ensembles + 3hourly FGAT
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivrfgat_3densda_enkf_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + GSIVR_ENS + 3DEnVar with self-cycled HAFS enkf ensembles + 3hourly FGAT
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivrfgatens_3densda_enkf_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=yes \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=10 config.run_enkf=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + GSIVR_ENS + 3DEnVar with self-cycled dual-resolution HAFS enkf ensembles + 3hourly FGAT
 # On top of the hafsv0p1aL64 configuration without ocean coupling
 # Dual-resolution ENSDA system, 3-km deterministic, 6-km ensembles, same domain coverage
 # Notes: 
 #   * need to increase the number of cores (from 200 to 800) to run the analysis jobs
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsv0p1aL64_full_3densda_dualres \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=yes \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=10 config.run_enkf=yes \
     config.GRID_RATIO_ENS=2 \
     gsi.use_bufr_nr=yes \
     ../parm/hafsv0p1aL64_da_AL.conf \
     config.NHRS=12 ${scrubopt}

## GSIVR + GSIVR_FGAT + GSIVR_ENS + 3DEnVar with self-cycled dual-resolution HAFS enkf ensembles + 3hourly FGAT + ocean coupling
## On top of the hafsv0p1aL64 configuration with ocean coupling;
## Dual-resolution ENSDA system, 3-km deterministic, 6-km ensembles, same domain coverage
## Notes: 
##   * need to increase the number of cores (from 200 to 800) to run the analysis jobs
#${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsv0p1acplL64_full_3densda_dualres \
#    config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=yes \
#    config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
#    config.run_ensda=yes config.ENS_SIZE=10 config.run_enkf=yes \
#    config.GRID_RATIO_ENS=2 \
#    gsi.use_bufr_nr=yes \
#    ../parm/hafsv0p1aL64_da_AL.conf \
#    ../parm/hafs_hycom.conf \
#    config.NHRS=12 ${scrubopt}

#===============================================================================

date

echo 'cronjob done'
