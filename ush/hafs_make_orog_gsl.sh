#!/bin/bash
#
#-----------------------------------------------------------------------
# Script to run 'orog_gsl' executable, which generates 'oro_data'
# static topographic statistics files needed for GSL orographic
# drag suite.  Source code is gsl_oro_data.f90.
#-----------------------------------------------------------------------
#

set -eux

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

#executable=$exec_dir/orog_gsl
executable=${OROGGSLEXEC:-$exec_dir/hafs_orog_gsl.x}
if [ ! -s $executable ]; then
  echo "executable does not exist"
  exit 1
fi

if [ ! -s $workdir ]; then mkdir -p $workdir ;fi
if [ ! -s $outdir ]; then mkdir -p $outdir ;fi


if [ $halo -eq "-999" ]; then
   OUTGRID="C${res}_grid.tile${tile}.nc"
else
   OUTGRID="C${res}_grid.tile${tile}.halo${halo}.nc"
fi


cd $workdir

ln -sf ${griddir}/$OUTGRID .
ln -sf ${topo_am}/"HGT.Beljaars_filtered.lat-lon.30s_res.nc" .
ln -sf ${topo_am}/"geo_em.d01.lat-lon.2.5m.HGT_M.nc" .

cp $executable .

echo $tile > grid_info.dat
echo $res >> grid_info.dat
echo $halo >> grid_info.dat
time $executable < grid_info.dat

if [ $? -ne 0 ]; then
  echo "ERROR in running $executable "
  exit 1
else
  mv ./C*oro_data_*.nc $outdir/
  echo "*oro_data_ls* and *oro_data_ss* files created"
  echo "Successfully running $executable "
  exit 0
fi

exit

