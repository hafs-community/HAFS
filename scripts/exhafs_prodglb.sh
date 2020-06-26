#!/bin/sh

set -xe

TOTAL_TASKS=${TOTAL_TASKS:-1}
NCTSK=${NCTSK:-1}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:-126}
NOUTHRS=${NOUTHRS:-3}

CASE=${CASE:-C768}
CRES=`echo $CASE | cut -c 2-`
res=${res:-$CRES}
gtype=${gtype:-regional}
nest_grids=${nest_grids:-1}

FREGRIDEXEC=${FREGRIDEXEC:-${EXEChafs}/hafs_fregrid.x}
FREGRIDPARAEXEC=${FREGRIDPARAEXEC:-${EXEChafs}/hafs_fregrid_parallel.x}
GETTRKEXEC=${GETTRKEXEC:-${EXEChafs}/hafs_gettrk.x}
TAVEEXEC=${GETTRKEXEC:-${EXEChafs}/hafs_tave.x}
VINTEXEC=${VINTEXEC:-${EXEChafs}/hafs_vint.x}
SUPVITEXEC=${SUPVITEXEC:-${EXEChafs}/hafs_supvit.x}

MPISERIAL=${MPISERIAL:-mpiserial}
NDATE=${NDATE:-ndate}
WGRIB2=${WGRIB2:-wgrib2}
GRB2INDEX=${GRB2INDEX:-grb2index}

WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
INPdir=${INPdir:-${WORKhafs}/forecast}
DATA=${DATA:-${WORKhafs}/prodglb}
COMhafs=${COMhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/com/${CDATE}/${STORMID}}

out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}
trk_atcfunix=${out_prefix}.trak.hafs.atcfunix.glb
all_atcfunix=${out_prefix}.trak.hafs.atcfunix.glb.all

tmp_vital=${WORKhafs}/tmpvit
old_vital=${WORKhafs}/oldvit

DATA_tracker=${DATA}/tracker
mkdir -p ${DATA_tracker}

cd ${DATA_tracker}

#===============================================================================
# Run fregrid_parallel to generate the remapping weight file, if it does not
# exist

nlon=${prodglb_nlon:-3072}
nlat=${prodglb_nlat:-1536}
atmos_tracker_base=${atmos_tracker_base:-atmos_tracker}
atmos_tracker_remap_weight=${atmos_tracker_remap_weight:-/let/prodglb/generate/atmos_tracker_remap_weight.nc}
remap_file='./atmos_tracker_remap_weight.nc'
mosaic_file=${WORKhafs}/intercom/grid/${CASE}/${CASE}_coarse_mosaic.nc
input_mosaic='./C768_coarse_mosaic.nc'
ln -sf ${mosaic_file} ${input_mosaic}
ln -sf ${WORKhafs}/intercom/grid/${CASE}/${CASE}_grid.tile*.nc ./
ln -sf ${WORKhafs}/forecast/${atmos_tracker_base}.*tile*.nc ./

# If atmos_tracker_remap_weight is specified and exists, copy it over directly
if [ -e $atmos_tracker_remap_weight ]; then
  echo "$atmos_tracker_remap_weight is specified and exists."
  echo "Copy $atmos_tracker_remap_weight directly."
  cp -p $atmos_tracker_remap_weight ${remap_file}
# Otherwise, run fregrid_parallel to generate the atmos_tracker_remap_weight
else
  cp -p ${FREGRIDPARAEXEC} ./hafs_fregrid_parallel.x
  ${APRUNC} ./hafs_fregrid_parallel.x --input_mosaic ${input_mosaic} --nlon ${nlon} --nlat ${nlat} --remap_file ${remap_file}
fi

#===============================================================================
# Run fregrid to remap the atmos_tracker netcdf files from the native six
# global tiles to a atmos_tracker_global_remapped.nc file on a regular latlon grid

boundstr=""
#atmos_tracker_fields="grid_xt,grid_yt,time,grid_lont,grid_latt,vort850,UGRDlowest,UGRD850,UGRD700,UGRD500,VGRDlowest,VGRD850,VGRD700,VGRD500,TMP850,TMP500,TMP300,PRMSL,HGT850,HGT700,HGT500,HGT200,RH850,RH500"
atmos_tracker_fields="grid_xt,grid_yt,time,grid_lont,grid_latt,vort850,us,u850,u700,u500,vs,v850,v700,v500,t850,t500,t300,slp,z850,z700,z500,z200,rh850,rh500"

cp -p ${FREGRIDEXEC} ./hafs_fregrid.x
${APRUNS} ./hafs_fregrid.x --input_mosaic ${input_mosaic} --nlon ${nlon} --nlat ${nlat} ${boundstr} \
  --remap_file ${remap_file} --input_file ${atmos_tracker_base} --output_file "${atmos_tracker_base}_global_remapped.nc" \
  --input_dir ./ --scalar_field ${atmos_tracker_fields}

# Deliver atmos_tracker*.nc to COMhafs
for itile in $(seq 1 6)
do
  cp -pL ./${atmos_tracker_base}.tile${itile}.nc ${COMhafs}/${out_prefix}.${atmos_tracker_base}.tile${itile}.nc
done

export ntiles=$((6 + ${nest_grids}))
for itile in $(seq 7 $ntiles)
do
  inest=$(($itile - 5))
  cp -pL ./${atmos_tracker_base}.nest0${inest}.tile${itile}.nc ${COMhafs}/${out_prefix}.${atmos_tracker_base}.nest0${inest}.tile${itile}.nc
done

# Deliver atmos_tracker_global_remapped.nc to COMhafs
cp -pL ./${atmos_tracker_base}_global_remapped.nc ${COMhafs}/${out_prefix}.${atmos_tracker_base}_global_remapped.nc

#===============================================================================
# Run GFDL vortextracker for the global netcdf format input file

# Link the track files and generate the input.fcst_minutes file
if [ -s input.fcst_minutes ] ; then
  rm -f input.fcst_minutes
fi
IFHR=0
FHR=0
FHR3=$( printf "%03d" "$FHR" )
while [ $FHR -le $NHRS ]
do
  echo "FHR3="${FHR3}
  FMIN=$(( ${FHR} * 60 ))
  minstr=$( printf "%5.5d" "$FMIN" )
  IFHR=`expr $IFHR + 1`
  LINE=$( printf "%4d %5d" "$IFHR" "$FMIN" )
  echo "$LINE" >> input.fcst_minutes
  FHR=`expr $FHR + $NOUTHRS`
  FHR3=$( printf "%03d" "$FHR" )
done

rm -f fort.*

# Find and sort active storms for this cycle from known tcvitals file
# This can potentially provide multiple tcvital messages to the tracker,
# so that it can track multiple storms simultaneously.
# *** Currently, tcutil_multistorm_sort.py searches the tcvitals files
# specified in the script. Need to modify it to be able to deal with storm
# message files/dirs, as well as passing in tcvitals files.
${USHhafs}/tcutil_multistorm_sort.py ${CDATE} | cut -c1-96 > allvit

# Prepare the input/output files
cat ${tmp_vital} allvit > input.vitals
#cat ${tmp_vital} > input.vitals

cp input.vitals tcvit_rsmc_storms.txt
ln -sf input.vitals       fort.12
touch fort.14
ln -sf input.fcst_minutes fort.15
ln -sf output.all         fort.61
ln -sf output.atcf        fort.62
ln -sf output.radii       fort.63
ln -sf output.atcfunix    fort.64
ln -sf output.initvitl    fort.65
ln -sf output.atcf_gen    fort.66
ln -sf output.genvitals   fort.67
ln -sf output.atcf_sink   fort.68
ln -sf output.atcf_hfip   fort.69
ln -sf output.cps_parms   fort.71
ln -sf output.structure   fort.72
ln -sf output.fractwind   fort.73
ln -sf output.ike         fort.74
ln -sf output.pdfwind     fort.76

# The product atcf track file
touch ${COMhafs}/${all_atcfunix}
ln -sf ${COMhafs}/${all_atcfunix} output.atcfunix

# Prepare the input namelist
CC=`echo $CDATE | cut -c 1-2`
YY=`echo $CDATE | cut -c 3-4`
MM=`echo $CDATE | cut -c 5-6`
DD=`echo $CDATE | cut -c 7-8`
HH=`echo $CDATE | cut -c 9-10`

cp ${PARMhafs}/product/namelist.gettrk.global_tmp ./
cat namelist.gettrk.global_tmp | sed s/_BCC_/${CC}/ | \
                          sed s/_BYY_/${YY}/ | \
                          sed s/_BMM_/${MM}/ | \
                          sed s/_BDD_/${DD}/ | \
                          sed s/_BHH_/${HH}/ | \
                          sed s/_YMDH_/${CDATE}/ > namelist.gettrk

# Run the vortex tracker gettrk.x
cp -p ${GETTRKEXEC} ./hafs_gettrk.x
#ln -sf ${GETTRKEXEC} ./hafs_gettrk.x
${APRUNS} ./hafs_gettrk.x < namelist.gettrk

# Extract the tracking records for tmpvit
STORMNUM=$(echo ${STORMID} | cut -c1-2)
STORMBS1=$(echo ${STORMID} | cut -c3)
cp ${COMhafs}/${all_atcfunix} ${COMhafs}/${all_atcfunix}.orig
if [ $STORMNUM == "00" ] ; then
norig=`cat ${COMhafs}/${all_atcfunix}.orig |wc -l `
if [ $norig -eq 1 ] ; then
> ${COMhafs}/${all_atcfunix}
else
grep -v "^.., ${STORMNUM}," ${COMhafs}/${all_atcfunix}.orig >  ${COMhafs}/${all_atcfunix}
fi
else
grep "^.., ${STORMNUM}," ${COMhafs}/${all_atcfunix} | grep -E "^${STORMBS1}.,|^.${STORMBS1}," > ${COMhafs}/${trk_atcfunix}
fi

# Deliver track file to NOSCRUB:
mkdir -p ${CDNOSCRUB}/${SUBEXPT}
cp -p ${COMhafs}/${all_atcfunix}.orig ${CDNOSCRUB}/${SUBEXPT}/.
if [ -s ${COMhafs}/${all_atcfunix} ] ; then
cp -p ${COMhafs}/${all_atcfunix} ${CDNOSCRUB}/${SUBEXPT}/.
fi
if [ -s ${COMhafs}/${trk_atcfunix} ] && [ $STORMNUM != "00" ] ; then
cp -p ${COMhafs}/${trk_atcfunix} ${CDNOSCRUB}/${SUBEXPT}/.
fi
#===============================================================================

cd ${DATA}

echo "product job done"

exit
