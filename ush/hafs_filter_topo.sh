#!/bin/sh
################################################################################
# Script Name: hafs_filter_topo.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script filters atmospheric model topography.
# History:
#   04/12/2019: This script was adopted from UFS_UTILS' fv3gfs_filter_topo.sh.
################################################################################
#set -eux
set -x

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
   set +x
   echo
   echo "FATAL ERROR: Usage: $0 resolution grid_dir orog_dir out_dir"
   echo
   set -x
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

#executable=$exec_dir/filter_topo
executable=${FILTERTOPOEXEC:-$exec_dir/hafs_filter_topo.x}
if [ ! -s $executable ]; then
  set +x
  echo
  echo "FATAL ERROR: ${executable} does not exist"
  echo
  set -x
  exit 1
fi

mosaic_grid=C${res}_mosaic.nc
topo_file=oro.C${res}

if [ ! -s $outdir ]; then mkdir -p $outdir ;fi
cd $outdir ||exit 8

cp $griddir/$mosaic_grid .
cp $griddir/C${res}_grid.tile?.nc .
cp $orodir/${topo_file}.tile?.nc .
cp $executable .

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

set -o pipefail
${APRUN} $executable 2>&1 | tee ./filter_topo.log
export err=$?; err_chk
set +o pipefail

date
