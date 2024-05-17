#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1

# RUN_ENVIR of nco or dev
RUN_ENVIR=${RUN_ENVIR:-${1:-dev}}

cwd=$(pwd)

HOMEhafs=$(pwd)/..
FIXhafs=${HOMEhafs}/fix
FIXversion=20230518
mkdir -p ${FIXhafs}
cd ${FIXhafs}
mkdir -p fix_fv3
if [ ${target} == "wcoss2" ]; then
  FIXROOT=/lfs/h2/emc/hur/noscrub/hafs-fix-files/hafs-${FIXversion}-fix/fix
elif [ ${target} == "hera" ]; then
  FIXROOT=/scratch1/NCEPDEV/hwrf/noscrub/hafs-fix-files/hafs-${FIXversion}-fix/fix
elif [ ${target} == "orion" ] || [ ${target} == "hercules" ]; then
  FIXROOT=/work/noaa/hwrf/noscrub/hafs-fix-files/hafs-${FIXversion}-fix/fix
elif [ ${target} == "jet" ]; then
  FIXROOT=/lfs4/HFIP/hwrf-data/hafs-fix-files/hafs-${FIXversion}-fix/fix
else
  echo "FATAL ERROR: Unknown site " ${target}
  exit 1
fi

for subdir in fix_am fix_aer fix_lut fix_orog fix_fv3_gmted2010 fix_sfc_climo fix_vi fix_hycom fix_mom6 fix_ww3 fix_cdeps;
do
  if [ "${RUN_ENVIR^^}" != "NCO" ]; then
    ln -sf ${FIXROOT}/${subdir} ./
  else
    rsync -av ${FIXROOT}/${subdir} ./
  fi
done

echo 'done'
