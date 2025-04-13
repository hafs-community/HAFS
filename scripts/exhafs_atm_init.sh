#!/bin/sh
################################################################################
# Script Name: exhafs_atm_init.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script conducts forecast and postprocessing steps for a short forecast
#   length. And it generates outputs, products, and restart files, needed for
#   other model initilization processes/steps (e.g., vortex initilization, data
#   assimilation).
################################################################################
set -x -o pipefail

DATAinit=${DATA}

#===============================================================================
# forecast

export DATA=${DATAinit}/forecast
mkdir -p ${DATA}
cd ${DATA}

${HOMEhafs}/scripts/exhafs_forecast.sh
export err=$?; err_chk

#===============================================================================
# post

export TOTAL_TASKS=240
export OMP_NUM_THREADS=1
source ${USHhafs}/hafs_runcmd.sh.inc

export DATA=${DATAinit}/post
mkdir -p ${DATA}
cd ${DATA}

${HOMEhafs}/scripts/exhafs_atm_post.sh
export err=$?; err_chk

#===============================================================================
# product

export OMP_NUM_THREADS=1
export APRUNC=${APRUNS}

export DATA=${DATAinit}/product
mkdir -p ${DATA}
cd ${DATA}

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
        ${HOMEhafs}/scripts/exhafs_product.sh \
        > ${DATA}/run_product.${gridstr}.log 2>&1" >> cmdfile_product
done

chmod +x cmdfile_product
if [ $USE_CFP = "YES" ] ; then
  ncmd=$(cat ./cmdfile_product | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP -n $ncmd_max cfp ./cmdfile_product
else
  ${APRUNC} ${MPISERIAL} -m cmdfile_product
fi
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
  if grep "top of output_all" ./tracker${neststr}/gettrk.out; then
    echo "INFO: successfully ran run_product.${gridstr}"
  else
    echo "FATAL ERROR: failed running run_product.${gridstr}"
    exit 1
  fi
done

#===============================================================================

export DATA=${DATAinit}
cd ${DATA}

date
