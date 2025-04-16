#!/bin/sh

set -xe

## YMDH=2024111612
YMDH=$(date -ud "$ANA_DATE" +%Y%m%d%H)
TMP_DATE=${YMDH:0:8}Z${YMDH:8:2}
export ANA_DATE=$(date -ud "$TMP_DATE")

echo MOM6-3DVAR ${YMDH}

OUTPUT_DIR=${WORK3DVAR}/data_output
#skip running the var if it has already been run
if [[ -d "$OUTPUT_DIR" && $(ls $OUTPUT_DIR -1q | wc -l) -gt 0 ]]; then
  echo "VAR analysis has already been created at :"
  echo "  $OUTPUT_DIR"
  echo "done with VAR"
exit 0
fi

echo run-MOM6-3DVAR ${YMDH}

cd ${WORK3DVAR}
mkdir -p obs
mkdir -p OUTPUT

YMDHmd=$(date -ud "$ANA_DATE - 24 hours" +%Y%m%d%H )
DATEmd=${YMDHmd:0:4}-${YMDHmd:4:2}-${YMDHmd:6:2}T${YMDHmd:8:2}:00:00Z
DATE0d=${YMDH:0:4}-${YMDH:4:2}-${YMDH:6:2}T${YMDH:8:2}:00:00Z

# 3dvar.yml
cp ${HOMEhafs}/parm/mom6_3dvar/3dvar.yml.tmp .
sed -i "s;DATEmd;${DATEmd};g" 3dvar.yml.tmp
sed -i "s;DATE0d;${DATE0d};g" 3dvar.yml.tmp
sed -i "s;YMDH;${YMDH};g" 3dvar.yml.tmp
sed -i "s;HH;${YMDH:8:2};g" 3dvar.yml.tmp
mv 3dvar.yml.tmp 3dvar.yml

# obs settings
source ${HOMEhafs}/parm/mom6_3dvar/obspath.config
ln -sf ${obs_files_dir}/sst_satellite_${YMDH:0:10}.nc ./obs/.
ln -sf ${obs_files_dir}/adt_ssh_${YMDH:0:10}.nc ./obs/.
ln -sf ${obs_files_dir}/sss_salinity_${YMDH:0:10}.nc ./obs/.
ln -sf ${obs_files_dir}/prof_insitu_${YMDH:0:10}.nc ./obs/.
 
# fix files
ln -sf ${FIXhafs}/fix_mom6_3dvar/* .

mkdir -p data_output

${APRUNC}  ${EXEChafs}/hafs_jedi.x  soca variational ./3dvar.yml 2>&1 | tee hafs_gdas.log

export err=$?; err_chk

