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
# HFSA based ENSDA configurations

 # Same-res ENSDA
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_ensda \
     ../parm/tests/hafsv1_hfsa_ensda_AL.conf forecast_ens.restart_interval_ens=6"
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Same-res ENSDA with online BC and mixed regional and global ensembles 
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_ensda_olbc_mixens \
     ../parm/tests/hafsv1_hfsa_ensda_AL.conf forecast_ens.restart_interval_ens=6 \
     gsi.online_satbias=yes gsi.l_both_fv3sar_gfs_ens=.true."
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Same-res 4DENSDA
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_4densda \
     ../parm/tests/hafsv1_hfsa_ensda_AL.conf \
     config.NHRS_ENS=9 config.NBDYHRS_ENS=3 \
     gsi.l4densvar=.true. gsi.nhr_obsbin=3"
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Same-res 4DENSDA with online BC and mixed regional and global ensembles
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_4densda_olbc_mixens \
     ../parm/tests/hafsv1_hfsa_ensda_AL.conf \
     config.NHRS_ENS=9 config.NBDYHRS_ENS=3 \
     gsi.l4densvar=.true. gsi.nhr_obsbin=3 \
     gsi.online_satbias=yes gsi.l_both_fv3sar_gfs_ens=.true."
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

#-------------------------------------------------------------------------------

 # Dual-res ENSDA
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_dualres_ensda \
     ../parm/tests/hafsv1_hfsa_dualres_ensda_AL.conf forecast_ens.restart_interval_ens=6"
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Dual-res ENSDA with online BC and mixed regional and global ensembles 
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_dualres_ensda_olbc_mixens \
     ../parm/tests/hafsv1_hfsa_dualres_ensda_AL.conf forecast_ens.restart_interval_ens=6 \
     gsi.online_satbias=yes gsi.l_both_fv3sar_gfs_ens=.true."
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Dual-res 4DENSDA
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_dualres_4densda \
     ../parm/tests/hafsv1_hfsa_dualres_ensda_AL.conf \
     config.NHRS_ENS=9 config.NBDYHRS_ENS=3 \
     gsi.l4densvar=.true. gsi.nhr_obsbin=3"
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Dual-res 4DENSDA with online BC and mixed regional and global ensembles
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_dualres_4densda_olbc_mixens \
     ../parm/tests/hafsv1_hfsa_dualres_ensda_AL.conf \
     config.NHRS_ENS=9 config.NBDYHRS_ENS=3 \
     gsi.l4densvar=.true. gsi.nhr_obsbin=3 \
     gsi.online_satbias=yes gsi.l_both_fv3sar_gfs_ens=.true."
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

#===============================================================================
# HFSB based ENSDA configurations

 # Same-res ENSDA
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_ensda \
     ../parm/tests/hafsv1_hfsb_ensda_AL.conf forecast_ens.restart_interval_ens=6"
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Same-res ENSDA with online BC and mixed regional and global ensembles 
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_ensda_olbc_mixens \
     ../parm/tests/hafsv1_hfsb_ensda_AL.conf forecast_ens.restart_interval_ens=6 \
     gsi.online_satbias=yes gsi.l_both_fv3sar_gfs_ens=.true."
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Same-res 4DENSDA
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_4densda \
     ../parm/tests/hafsv1_hfsb_ensda_AL.conf \
     config.NHRS_ENS=9 config.NBDYHRS_ENS=3 \
     gsi.l4densvar=.true. gsi.nhr_obsbin=3"
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Same-res 4DENSDA with online BC and mixed regional and global ensembles
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_4densda_olbc_mixens \
     ../parm/tests/hafsv1_hfsb_ensda_AL.conf \
     config.NHRS_ENS=9 config.NBDYHRS_ENS=3 \
     gsi.l4densvar=.true. gsi.nhr_obsbin=3 \
     gsi.online_satbias=yes gsi.l_both_fv3sar_gfs_ens=.true."
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

#-------------------------------------------------------------------------------

 # Dual-res ENSDA
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_dualres_ensda \
     ../parm/tests/hafsv1_hfsb_dualres_ensda_AL.conf forecast_ens.restart_interval_ens=6"
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Dual-res ENSDA with online BC and mixed regional and global ensembles 
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_dualres_ensda_olbc_mixens \
     ../parm/tests/hafsv1_hfsb_dualres_ensda_AL.conf forecast_ens.restart_interval_ens=6 \
     gsi.online_satbias=yes gsi.l_both_fv3sar_gfs_ens=.true."
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Dual-res 4DENSDA
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_dualres_4densda \
     ../parm/tests/hafsv1_hfsb_dualres_ensda_AL.conf \
     config.NHRS_ENS=9 config.NBDYHRS_ENS=3 \
     gsi.l4densvar=.true. gsi.nhr_obsbin=3"
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

 # Dual-res 4DENSDA with online BC and mixed regional and global ensembles
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsb_dualres_4densda_olbc_mixens \
     ../parm/tests/hafsv1_hfsb_dualres_ensda_AL.conf \
     config.NHRS_ENS=9 config.NBDYHRS_ENS=3 \
     gsi.l4densvar=.true. gsi.nhr_obsbin=3 \
     gsi.online_satbias=yes gsi.l_both_fv3sar_gfs_ens=.true."
 ./run_hafs.py ${opts} 2020082512-2020082518 00L HISTORY ${confopts} \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no

#===============================================================================

date

echo 'cronjob done'
