#!/bin/sh
################################################################################
# Script Name: exhafs_forecast.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script calls the ush/hafs_forecast.sh to run the main HAFS forecast
#   with various configurations.
################################################################################
set -xe

date

export CDATE=${CDATE:-${YMDH}}
export ENSDA=${ENSDA:-NO}
export ENSID=${ENSID:-000}

cd $DATA

if [ "${ENSDA}" = YES ]; then
  export DATAfcst=${DATAfcst:-${DATA:-${WORKhafs}/forecast_ens/mem${ENSID}}}
  export FIXgrid=${WORKhafs}/intercom/grid_ens
  export INPdir=${WORKhafs}/intercom/chgres_ens/mem${ENSID}
  export OUTdir=${WORKhafs}/intercom/forecast_ens/mem${ENSID}
  export RESTARTout=${WORKhafs}/intercom/RESTART_ens/mem${ENSID}
else
  export DATAfcst=${DATAfcst:-${DATA:-${WORKhafs}/forecast}}
  export FIXgrid=${WORKhafs}/intercom/grid
  export INPdir=${WORKhafs}/intercom/chgres
  export OUTdir=${WORKhafs}/intercom/forecast
  export RESTARTout=${WORKhafs}/intercom/RESTART
fi

mkdir -p ${DATAfcst}
cd ${DATAfcst}

# Utilize ush/hafs_forecast.sh to configure and run the forecast model
${HOMEhafs}/ush/hafs_forecast.sh
export err=$?; err_chk

cd $DATA

date
