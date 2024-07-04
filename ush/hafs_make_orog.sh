#!/bin/sh
################################################################################
# Script Name: hafs_make_orog.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the UFS_UTILS orog executable and generates 'oro_data'
#   static topographic statistics files.
# History:
#   04/24/2019: This script was adopted from UFS_UTILS' fv3gfs_make_orog.sh
#   07/03/2024: Sync with latest UFS_UTILS' fv3gfs_make_orog.sh
# Usage:
#  Arguments:
#    res     - "C" Resolution of model grid - 48, 96, 768, etc.
#    tile    - Tile number.
#    griddir - Location of model 'grid' file.
#    outdir  - Location of the model orography file output by
#              the 'orog' program.
#    indir   - Location of input land mask and terrain data.
#    tmpdir  - Location to create the work direcotry
#
#  Input Files:
#    $GRIDFILE                         - The model 'grid' file
#                                        containing georeference info.
#    topography.antarctica.ramp.30s.nc - RAMP terrain data.
#    landcover.umd.30s.nc              - Global land mask data.
#    topography.gmted2010.30s.nc       - Global USGS GMTED 2010
#                                        terrain data.
#
#  Output Files:
#    out.oro.nc - The model orography file (single tile).
#
# Condition codes:
#    0 - Normal termination.
#    1 - Incorrect number of script arguments.
#-------------------------------------------------------------------


################################################################################
set -x -o pipefail

nargv=$#

if [ $nargv -eq 6 ]; then
  res=$1
  tile=$2
  griddir=$3
  outdir=$4
  indir=$5
  tmpdir=$6
else
  echo "FATAL ERROR: Number of arguments must be 6."
  echo "Usage: $0 resolution tile griddir outdir indir tmpdir."
  exit 1
fi

executable=${OROGEXEC:-$exec_dir/hafs_utils_orog.x}

workdir=$tmpdir/C${res}/orog/tile$tile
mkdir -p $workdir $outdir

GRIDFILE="C${res}_grid.tile${tile}.nc"

# Make Orograraphy
echo "GRIDFILE = $GRIDFILE"
echo "workdir = $workdir"
echo "outdir = $outdir"
echo "indir = $indir"

cd $workdir

${NLN} ${indir}/topography.antarctica.ramp.30s.nc .
${NLN} ${indir}/landcover.umd.30s.nc .
${NLN} ${indir}/topography.gmted2010.30s.nc .
${NLN} ${griddir}/$GRIDFILE .

#-------------------------------------------------------------------
# Set up program namelist. The entries are:
#
#  1 - GRIDFILE - model 'grid' file.
#  2 - Logical to output land mask only. When creating a grid
#      for the coupled model ("ocn" resolution is specified)
#      this is true. The mask is then tweaked during the
#      ocean merge step before the 'orog' program is run again
#      (in fv3gfs_ocean_merge.sh) to create the full 'orog'
#      file. When false, the 'orog' program outputs the
#      full orography file.
#  3 - The input file from the ocean merge step. Defaults
#      to 'none' for this script.
#-------------------------------------------------------------------

echo $GRIDFILE > INPS
if [ -z ${ocn+x} ]; then
  echo ".false." >> INPS
else
  echo ".true." >> INPS
fi
echo "none" >> INPS

cat INPS
${NCP} -p $executable ./hafs_utils_orog.x
${SOURCE_PREP_STEP}
${APRUNO} ./hafs_utils_orog.x < INPS 2>&1 | tee ./orog.log
export err=$?; err_chk

outfile=oro.C${res}.tile${tile}.nc
mv ./out.oro.nc $outdir/$outfile
echo "file $outdir/$outfile is created"
echo "Successfully running $executable "

date
