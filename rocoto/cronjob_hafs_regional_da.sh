#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

 # Cold-start from GFS analysis without DA
  ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_noda \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # Warm-start from prior HAFS forecast without DA
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_noda_cycling \
     config.warm_start_opt=2 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # Simple 3DVar DA
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3dvar \
     config.run_gsi=yes config.run_envar=no \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with GDAS ensembles
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3denvar \
     config.run_gsi=yes config.run_envar=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSI-based Vortex Relocation (GSIVR) + 3DEnVar with GDAS ensembles
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivr_3denvar \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=no config.run_envar=yes \
     config.run_ensda=no config.ENS_SIZE=0 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with GDAS ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3denvar_fgat \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=no config.ENS_SIZE=0 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with HAFS ensembles (cold-start from GDAS ensembles)
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=no config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with HAFS ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda_fgat \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + 3DEnVar with HAFS ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivr_3densda_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + 3DEnVar with HAFS ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivrfgat_3densda_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=no \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + GSIVR_ENS + 3DEnVar with HAFS ensembles (self-cycled through GSIVR_ENS) + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivrfgatens_3densda_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=yes \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=no \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # 3DEnVar with self-cycled HAFS enkf ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_3densda_enkf_fgat \
     config.run_gsi_vr=no config.run_gsi_vr_fgat=no config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + 3DEnVar with self-cycled HAFS enkf ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96s1n4_gsivrfgat_3densda_enkf_fgat \
     config.run_gsi_vr=yes config.run_gsi_vr_fgat=yes config.run_gsi_vr_ens=no \
     config.run_gsi=yes config.run_fgat=yes config.run_envar=yes \
     config.run_ensda=yes config.ENS_SIZE=4 config.run_enkf=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

 # GSIVR + GSIVR_FGAT + GSIVR_ENS + 3DEnVar with self-cycled HAFS enkf ensembles + 3hourly FGAT
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
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
 ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
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
# ./run_hafs.py ${opts} 2020082506-2020082512 00L HISTORY \
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
