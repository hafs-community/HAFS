#!/bin/sh
################################################################################
# Script Name: hafs_filter_topo.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script filters atmospheric model topography.
# History:
#   04/12/2019: This script was adopted from UFS_UTILS' fv3gfs_filter_topo.sh.
################################################################################
set -x -o pipefail

#-----------------------------------------------------------------------------------------
#
# Script name: fv3gfs_filter_topo.sh
# -----------
#
# Description: Filters the topography.
# -----------
#
#-----------------------------------------------------------------------------------------

if [ $# -ne 4 ]; then
   echo "FATAL ERROR: Usage: $0 resolution grid_dir orog_dir out_dir"
   exit 1
fi

APRUN=${APRUN:-time}

#if [ $gtype = stretch ] || [ $gtype = regional_gfdl ]; then
if [ $gtype = stretch ] || [ $gtype = regional ] || [ $gtype = regional_gfdl ] || [ $gtype = regional_esg ]; then
  stretch=$stretch_fac
else
  stretch=1.0
fi

res=$1
griddir=$2
orodir=$3
outdir=$4

executable=${FILTERTOPOEXEC:-$exec_dir/hafs_utils_filter_topo.x}

mosaic_grid=C${res}_mosaic.nc
topo_file=oro.C${res}

mkdir -p $outdir
cd $outdir

${NCP} $griddir/$mosaic_grid .
${NCP} $griddir/C${res}_grid.tile?.nc .
for file in $orodir/${topo_file}.tile?.nc ; do
  filebase=$(basename $file)
  if [ ! -e ./${filebase} ]; then
    ${NCP} ${file} ./${filebase}
  fi
done

regional=.false.
if [ $gtype = regional ] || [ $gtype = regional_gfdl ] || [ $gtype = regional_esg ] ; then
  regional=.true.
fi

cat > input.nml <<EOF
&filter_topo_nml
  grid_file = $mosaic_grid
  topo_file = $topo_file
  mask_field = "land_frac"
  regional = $regional
  stretch_fac = $stretch
  res = $res
  /
EOF

${NCP} -p $executable ./hafs_utils_filter_topo.x
${APRUN} ./hafs_utils_filter_topo.x 2>&1 | tee ./filter_topo.log
export err=$?; err_chk

date
