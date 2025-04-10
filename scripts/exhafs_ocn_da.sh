#!/bin/sh
################################################################################
# Script Name: exhafs_ocn_da.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the mom6 data assimilation step.
################################################################################
set -x -o pipefail

if [[ "$run_ocn_da" != yes ]]; then
  echo "This job should only be run if \$run_ocn_da is yes."
  echo "  \$run_ocn_da=\"$run_ocn_da\""
  echo " -> SCRIPT IS EXITING BECAUSE THIS JOB SHOULD NOT BE RUN <- "
  exit 0
fi

USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}
TMP_DATE=${CDATE:0:8}Z${CDATE:8:2}
export ANA_DATE=$(date -ud "$TMP_DATE")

MOM_res_DIR=${WORKhafs}/atm_init/forecast/RESTART
if [[ -d "${MOM_res_DIR}" && $(ls ${MOM_res_DIR}/*MOM.res* -1q | wc -l) -gt 2 ]]; then
  echo " -> MOM6 restart files from atm_init are available"
  echo " -> Running OCN DA"
else
  echo " -> MOM6 restart files from atm_init are not available" 
  echo " -> Not running OCN DA"
  echo " -> SCRIPT IS EXITING" 
  exit 9
fi

DATA=${DATA:-${WORKhafs}/ocn_da}
mkdir -p ${DATA}
cd $DATA

export WORK3DVAR=${DATA}
mkdir -p ${WORK3DVAR}/restarts
cd ${WORK3DVAR}/restarts

# 1  Combine 5 small MOM.res.nc files into a big MOM.res.nc  ~20GB
ln -sf ${MOM_res_DIR}/${CDATE:0:8}.${CDATE:8:2}0130.MOM*.nc .
cp ${CDATE:0:8}.${CDATE:8:2}0130.MOM.res.nc MOM.res.nc
ncks -A ${CDATE:0:8}.${CDATE:8:2}0130.MOM.res_1.nc MOM.res.nc
ncks -A ${CDATE:0:8}.${CDATE:8:2}0130.MOM.res_2.nc MOM.res.nc
ncks -A ${CDATE:0:8}.${CDATE:8:2}0130.MOM.res_3.nc MOM.res.nc
ncks -A ${CDATE:0:8}.${CDATE:8:2}0130.MOM.res_4.nc MOM.res.nc

# 2  MOM6_3DVAR
cd ${WORK3DVAR}
${USHhafs}/hafs_mom6_3dvar_run.var.sh > ${WORK3DVAR}/hafs_mom6_3dvar_run.var.log

OUTPUT_DIR=${WORK3DVAR}/data_output
if [[ -d "$OUTPUT_DIR" && $(ls $OUTPUT_DIR -1q | wc -l) -lt 2 ]]; then
  echo "3DVAR analysis has NO output files"
    echo "  $OUTPUT_DIR"
        exit 9
fi

# 3  Deliver MOM.res.nc updated by 3DVAR to intercom
cp ./restarts/MOM.res.nc MOM.res.nc
ln -sf ./data_output/ocn.3dvar.an.${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}T${CDATE:8:2}:00:00Z.nc ocn.ana.nc

ncks -A -v Temp,Salt,ave_ssh ./ocn.ana.nc ./TS3D_SSH.nc
ncrename -d zaxis_1,Layer -d yaxis_1,lath -d xaxis_1,lonh ./TS3D_SSH.nc
ncrename -v zaxis_1,Layer -v yaxis_1,lath -v xaxis_1,lonh ./TS3D_SSH.nc
ncks -A -v Layer,lath,lonh ./MOM.res.nc ./TS3D_SSH.nc    # replace dim to be consistent 
ncks -A -v Temp,Salt,ave_ssh ./TS3D_SSH.nc ./MOM.res.nc  # update T, S, SSH from 3DVAR
cp ./MOM.res.nc ${WORKhafs}/intercom/ocn_prep/mom6/. 

cp -p ./TS3D_SSH.nc ${COMhafs}/${out_prefix}.${RUN}.mom6.analysis.nc

cd ${WORK3DVAR}/output
for obs_file in `ls *.3dvar.nc`; do
  cp -p ${obs_file} ${COMhafs}/${out_prefix}.${RUN}.mom6.${obs_file%%.*}.nc
done

