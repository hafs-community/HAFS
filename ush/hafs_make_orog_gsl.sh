#!/bin/sh
################################################################################
# Script Name: hafs_make_orog_gsl.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the UFS_UTILS orog_gsl executable and generates 'oro_data'
#   static topographic statistics files needed for GSL orographic drag suite.
# History:
#   09/11/2022: This script was adopted from UFS_UTILS' fv3gfs_make_orog_gsl.sh
################################################################################
#-----------------------------------------------------------------------
# Script to run 'orog_gsl' executable, which generates 'oro_data'
# static topographic statistics files needed for GSL orographic
# drag suite.  Source code is gsl_oro_data.f90.
#-----------------------------------------------------------------------
#
set -x -o pipefail

nargv=$#

if [ $nargv -ne 7 ]; then
  echo "Number of arguments must be 7"
  echo "Usage: $0 resolution tile griddir outdir script_dir hist_dir TMPDIR"
  exit 1
fi

res=$1
tile=$2
halo=$3
griddir=$4
outdir=$5
topo_am=$6
TMPDIR=$7
workdir=$TMPDIR/C${res}/orog_gsl/tile$tile

executable=${OROGGSLEXEC:-$exec_dir/hafs_utils_orog_gsl.x}
mkdir -p $workdir $outdir

if [ $halo -eq "-999" ]; then
   OUTGRID="C${res}_grid.tile${tile}.nc"
else
   OUTGRID="C${res}_grid.tile${tile}.halo${halo}.nc"
fi

cd $workdir

${NLN} ${griddir}/$OUTGRID .
${NLN} ${topo_am}/"HGT.Beljaars_filtered.lat-lon.30s_res.nc" .
${NLN} ${topo_am}/"geo_em.d01.lat-lon.2.5m.HGT_M.nc" .

echo $tile > grid_info.dat
echo $res >> grid_info.dat
echo $halo >> grid_info.dat

${NCP} -p $executable ./hafs_utils_orog_gsl.x
${APRUNO} ./hafs_utils_orog_gsl.x < grid_info.dat 2>&1 | tee ./orog_gsl.log
export err=$?; err_chk

mv ./C*oro_data_*.nc $outdir/
echo "*oro_data_ls* and *oro_data_ss* files created"
echo "Successfully running $executable "

date
