#!/bin/sh
################################################################################
# Script Name: hafs_make_orog.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the UFS_UTILS orog executable and generates 'oro_data'
#   static topographic statistics files.
# History:
#   04/24/2019: This script was adopted from UFS_UTILS' fv3gfs_make_orog.sh
################################################################################
set -x -o pipefail

nargv=$#

inorogexist=0

if [ $nargv -eq 6 ];  then  # lat-lon grid
  lonb=$1
  latb=$2
  outdir=$3
  script_dir=$4
  is_latlon=1
  orogfile="none"
  hist_dir=$5
  TMPDIR=$6
  workdir=$TMPDIR/latlon/orog/latlon_${lonb}x${latb}
elif [ $nargv -eq 7 ]; then  # cubed-sphere grid
  res=$1
  lonb=$1
  latb=$1
  tile=$2
  griddir=$3
  outdir=$4
  script_dir=$5
  is_latlon=0
  orogfile="none"
  hist_dir=$6
  TMPDIR=$7
  workdir=$TMPDIR/C${res}/orog/tile$tile
elif [ $nargv -eq 8 ]; then  # input your own orography files
  res=$1
  lonb=$1
  latb=$1
  tile=$2
  griddir=$3
  outdir=$4
  is_latlon=0
  inputorog=$5
  script_dir=$6
  orogfile=$inputorog
  inorogexist=1
  hist_dir=$7
  TMPDIR=$8
  workdir=$TMPDIR/C${res}/orog/tile$tile
else
  echo "Number of arguments must be 7 or 8 for cubic sphere grid and 6 for lat-lon grid"
  echo "Usage for cubic sphere grid: $0 resolution tile griddir outdir script_dir hist_dir TMPDIR"
  exit 1
fi

indir=$hist_dir
executable=${OROGEXEC:-$exec_dir/hafs_utils_orog.x}

mkdir -p $workdir $outdir

mtnres=1
efac=0
blat=0

if [ $is_latlon -eq 1 ]; then
  OUTGRID="none"
else
  OUTGRID="C${res}_grid.tile${tile}.nc"
fi

# Make Orograraphy
echo "OUTGRID = $OUTGRID"
echo "workdir = $workdir"
echo "outdir = $outdir"
echo "indir = $indir"

cd $workdir

${NLN} ${indir}/thirty.second.antarctic.new.bin fort.15
${NLN} ${indir}/landcover30.fixed .
${NLN} ${indir}/gmted2010.30sec.int  fort.235
if [ $inorogexist -eq 1 ]; then
   ${NCP} $inputorog .
fi

if [ $is_latlon -eq 0 ]; then
   ${NCP} ${griddir}/$OUTGRID .
fi

echo  $mtnres $lonb $latb $efac $blat > INPS
echo $OUTGRID >> INPS
echo $orogfile >> INPS
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

if [ $is_latlon -eq 1 ]; then
   outfile=oro.${lonb}x${latb}.nc
else
   outfile=oro.C${res}.tile${tile}.nc
fi

mv ./out.oro.nc $outdir/$outfile
echo "file $outdir/$outfile is created"
echo "Successfully running $executable "

date
