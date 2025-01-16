#!/bin/sh

set -xe

YMDH=$(date -ud "$ANA_DATE" +%Y%m%d%H)
TMP_DATE=${YMDH:0:8}Z${YMDH:8:2}
export ANA_DATE=$(date -ud "$TMP_DATE")

echo MOM6-3DVAR ${YMDH}

OUTPUT_DIR=${WORK3DVAR}/output
# skip running the var if it has already been run
if [[ -d "$OUTPUT_DIR" && $(ls $OUTPUT_DIR -1q | wc -l) -gt 0 ]]; then
  echo "VAR analysis has already been created at :"
  echo "  $OUTPUT_DIR"
  echo "done with VAR"
  exit 0
fi

echo run-MOM6-3DVAR ${YMDH}

cd ${WORK3DVAR}
ln -sf ${FIXhafs}/fix_mom6_3dvar/* .

cp ${HOMEhafs}/parm/mom6_3dvar/3dvarbump.yml.tmp . 
YMDHmd=$(date -ud "$ANA_DATE - 24 hours" +%Y%m%d%H )
WINDOWBEGIN=${YMDHmd:0:4}-${YMDHmd:4:2}-${YMDHmd:6:2}T${YMDHmd:8:2}:00:00Z
DATE=${YMDH:0:4}-${YMDH:4:2}-${YMDH:6:2}T${YMDH:8:2}:00:00Z
sed -i "s;WINDOWBEGIN;${WINDOWBEGIN};g" 3dvarbump.yml.tmp
sed -i "s;DATE;${DATE};g" 3dvarbump.yml.tmp
mv 3dvarbump.yml.tmp 3dvarbump.yml

### obs settings
source ${HOMEhafs}/parm/mom6_3dvar/obspath.config
export obs_files_dir=${obs_src_dir}/${YMDH:0:4}/${YMDH:0:8}

mkdir -p obs

ln -sf ${obs_files_dir}/sst_amsr_${YMDH:0:10}.nc ./obs/sst_amsr.nc
ln -sf ${obs_files_dir}/sst_goes_${YMDH:0:10}.nc ./obs/sst_goes.nc
ln -sf ${obs_files_dir}/sst_jpss_${YMDH:0:10}.nc ./obs/sst_jpss.nc
ln -sf ${obs_files_dir}/sst_metop_${YMDH:0:10}.nc ./obs/sst_metop.nc
ln -sf ${obs_files_dir}/sst_npp_${YMDH:0:10}.nc ./obs/sst_npp.nc

ln -sf ${obs_files_dir}/adt_ssh_${YMDH:0:10}.nc ./obs/adt.nc
ln -sf ${obs_files_dir}/sss_salinity_${YMDH:0:10}.nc ./obs/sss.nc
ln -sf ${obs_files_dir}/prof_insitu_${YMDH:0:10}.nc ./obs/prof.nc
 
mkdir -p output

echo "GDASApp-20240731" ${APRUNC}

${APRUNC}  ${EXEChafs}/hafs_jedi.x  soca variational ./3dvarbump.yml 2>&1 | tee hafs_gdas.log
export err=$?; err_chk
