#!/bin/sh
################################################################################
# Script Name: exhafs_atm_init.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs a atmosphere-only one time step forecast, conducts the
#   post-processing, and generates the atcf track files. This is to prepare
#   model restart files and generate storm atcfunix track files, needed for
#   vortex initialization, data assimilation, and forecast model initialization.
################################################################################
set -xe

date

export CDATE=${CDATE:-${YMDH}}
export ENSDA=${ENSDA:-NO}
export ENSID=${ENSID:-000}
export FGAT_MODEL=${FGAT_MODEL:-gfs}
export FGAT_HR=${FGAT_HR:-00}
export RUN_INIT=${RUN_INIT:-YES}

mkdir -p $DATA
cd $DATA

#===============================================================================
# Run one time step atmosphere-only forecast

if [ "${ENSDA}" = YES ]; then
  export DATAfcst=${DATAfcst:-${WORKhafs}/atm_init_ens/mem${ENSID}/forecast}
  export FIXgrid=${FIXgrid:-${WORKhafs}/intercom/grid_ens}
  export INPdir=${INPdir:-${WORKhafs}/intercom/chgres_ens/mem${ENSID}}
  export OUTdir=${OUTdir:-${WORKhafs}/intercom/forecast_init_ens/mem${ENSID}}
  export RESTARTout=${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}
elif [ ${FGAT_MODEL} = gdas ]; then
  export DATAfcst=${DATAfcst:-${WORKhafs}/atm_init_fgat${FGAT_HR}/forecast}
  export FIXgrid=${FIXgrid:-${WORKhafs}/intercom/grid}
  export INPdir=${INPdir:-${WORKhafs}/intercom/chgres_fgat${FGAT_HR}}
  export OUTdir=${OUTdir:-${WORKhafs}/intercom/forecast_init_fgat${FGAT_HR}}
  export RESTARTout=${WORKhafs}/intercom/RESTART_init_fgat${FGAT_HR}
else
  export DATAfcst=${DATAfcst:-${WORKhafs}/atm_init/forecast}
  export FIXgrid=${FIXgrid:-${WORKhafs}/intercom/grid}
  export INPdir=${INPdir:-${WORKhafs}/intercom/chgres}
  export OUTdir=${OUTdir:-${WORKhafs}/intercom/forecast_init}
  export RESTARTout=${WORKhafs}/intercom/RESTART_init
fi

mkdir -p $DATAfcst
cd $DATAfcst

${HOMEhafs}/ush/hafs_forecast.sh
export err=$?; err_chk

cd $DATA

#===============================================================================
# Run one time step atm_post

export TOTAL_TASKS=240
export OMP_THREADS=1
export OMP_NUM_THREADS=1
source ${USHhafs}/hafs_runcmd.sh.inc

if [ ${ENSDA} = YES ]; then
  export DATApost=${WORKhafs}/atm_init_ens/mem${ENSID}/post
  export INPdir=${WORKhafs}/intercom/forecast_init_ens/mem${ENSID}
  export COMOUTpost=${WORKhafs}/intercom/atm_init_ens/mem${ENSID}
  export intercom=${WORKhafs}/intercom/atm_init_ens/mem${ENSID}/post
  export RESTARTcom=""
elif [ ${FGAT_MODEL} = gdas ]; then
  export DATApost=${WORKhafs}/atm_init_fgat${FGAT_HR}/post
  export INPdir=${WORKhafs}/intercom/forecast_init_fgat${FGAT_HR}
  export COMOUTpost=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}
  export intercom=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}/post
  export RESTARTcom=""
else
  export DATApost=${WORKhafs}/atm_init/post
  export INPdir=${WORKhafs}/intercom/forecast_init
  export COMOUTpost=${WORKhafs}/intercom/atm_init
  export intercom=${WORKhafs}/intercom/atm_init/post
  export RESTARTcom=""
fi

mkdir -p $DATApost
cd $DATApost

${HOMEhafs}/ush/hafs_atm_post.sh
export err=$?; err_chk

cd $DATA

#===============================================================================
# one time step product

export TOTAL_TASKS=2
export OMP_THREADS=1
export OMP_NUM_THREADS=1
source ${USHhafs}/hafs_runcmd.sh.inc

if [ ${ENSDA} = YES ]; then
  export DATAprod=${WORKhafs}/atm_init_ens/mem${ENSID}/product
  export INPdir=${WORKhafs}/atm_init_ens/mem${ENSID}/post
  export COMOUTproduct=${WORKhafs}/intercom/atm_init_ens/mem${ENSID}
elif [ ${FGAT_MODEL} = gdas ]; then
  export DATAprod=${WORKhafs}/atm_init_fgat${FGAT_HR}/product
  export INPdir=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}/post
  export COMOUTproduct=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}
else
  export DATAprod=${WORKhafs}/atm_init/product
  export INPdir=${WORKhafs}/intercom/atm_init/post
  export COMOUTproduct=${WORKhafs}/intercom/atm_init
fi

mkdir -p $DATAprod
cd $DATAprod

# Prepare and run ush/hafs_product.sh for parent and nest(s) in parallel
if [ ${gtype} = nest ]; then
  ngrids=$((${nest_grids} + 1))
else
  ngrids=${nest_grids}
fi

rm -f cmdfile_product
# Loop for grids/domains to prepare the cmdfile
for ng in $(seq 1 ${ngrids}); do
  if [[ $ng -eq 1 ]]; then
    neststr=""
    tilestr=".tile1"
  else
    neststr=".nest$(printf '%02d' ${ng})"
    tilestr=".tile$(printf '%d' ${ng})"
  fi
  gridstr=$(echo ${out_gridnames} | cut -d, -f ${ng})
  echo "export neststr=$neststr tilestr=${tilestr} gridstr=${gridstr}; \
        ${HOMEhafs}/ush/hafs_product.sh \
        > ${DATAprod}/run_product.${gridstr}.log 2>&1" >> cmdfile_product
done
chmod +x cmdfile_product
${APRUNC} ${MPISERIAL} -m cmdfile_product
export err=$?; err_chk

# Check if product/tracker run successfully
for ng in $(seq 1 ${ngrids}); do
  if [[ $ng -eq 1 ]]; then
    neststr=""
  else
    neststr=".nest$(printf '%02d' ${ng})"
  fi
  gridstr=$(echo ${out_gridnames} | cut -d, -f ${ng})
  cat ./run_product.${gridstr}.log
  if grep "top of output_all" ./tracker${neststr}/hafs_gettrk.out; then
    echo "INFO: successfully ran run_product.${gridstr}"
  else
    echo "FATAL ERROR: failed running run_product.${gridstr}"
    exit 1
  fi
done

cd $DATA

#===============================================================================
# Cleanup DATA dir
cd ${WORKhafs}
if [ "${KEEPDATA:-YES}" != "YES" ]; then
  rm -rf $DATAinit
fi

date
