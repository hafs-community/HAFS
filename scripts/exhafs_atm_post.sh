#!/bin/sh
################################################################################
# Script Name: exhafs_atm_post.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script calls the ush/hafs_atm_post.sh to run the main HAFS atmospheric
#   post-processing step with UPP, and then subsetting the GFDL vortex tracker
#   needed grib2 records as well as NHC requested grib2 records (if chosen).
################################################################################
set -xe

date

export CDATE=${CDATE:-${YMDH}}
export ENSDA=${ENSDA:-NO}
export ENSID=${ENSID:-000}

cd $DATA

if [ "${ENSDA}" = YES ]; then
  export DATApost=${DATApost:-${DATA:-${WORKhafs}/atm_post${POST_GROUPI}_ens/mem${ENSID}}}
  export INPdir=${WORKhafs}/intercom/forecast_ens/mem${ENSID}
  export COMOUTpost=${COMhafs}/post_ens/mem${ENSID}
  export intercom=${WORKhafs}/intercom/post_ens/mem${ENSID}
  export RESTARTcom=${COMhafs}/${out_prefix}.RESTART_ens/mem${ENSID}
else
  export DATApost=${DATApost:-${DATA:-${WORKhafs}/atm_post${POST_GROUPI}}}
  export INPdir=${WORKhafs}/intercom/forecast
  export COMOUTpost=${COMhafs}
  export intercom=${WORKhafs}/intercom/post
  export RESTARTcom=${COMhafs}/${out_prefix}.RESTART
fi

mkdir -p ${DATApost}
cd ${DATApost}

# Utilize ush/hafs_atm_post.sh for post-processing
${HOMEhafs}/ush/hafs_atm_post.sh
export err=$?; err_chk

cd $DATA

date
