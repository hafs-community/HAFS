#!/bin/sh
################################################################################
# Script Name: exhafs_product.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script calls ush/hafs_product.sh to run the GFDL vortex tracker, and
#   produces the stakeholder (NHC/JTWC) needed products (if desired).
################################################################################
set -xe

date

export CDATE=${CDATE:-${YMDH}}

cd $DATA

if [ "${ENSDA}" = YES ]; then
  export DATAprod=${DATAprod:-${DATA:-${WORKhafs}/product_ens/mem${ENSID}}}
  export INPdir=${WORKhafs}/intercom/post_ens/mem${ENSID}
  export COMOUTproduct=${COMhafs}/product_ens/mem${ENSID}
else
  export DATAprod=${DATAprod:-${DATA:-${WORKhafs}/product}}
  export INPdir=${WORKhafs}/intercom/post
  export COMOUTproduct=${COMhafs}
fi

mkdir -p ${DATAprod}
cd ${DATAprod}

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

date
