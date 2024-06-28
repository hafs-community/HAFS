#!/bin/sh
################################################################################
# Script Name: hafs_make_grid.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script creates HAFS needed atmospheric model grids.
# History:
#   04/24/2019: This script was adopted from UFS_UTILS' grid driver script
#   05/15/2020: Bill Ramstrom, HRD/AOML, Added multiple global-nest capability
#   10/06/2021: Added the capability to support regional nesting configurations
#   10/22/2022: Enabled support for regional ESG grid
################################################################################
set -x -o pipefail

nargv=$#
if [ $nargv -ne 3 -a $nargv -ne 5 -a $nargv -ne 6 -a $nargv -ne 7 -a $nargv -ne 12 -a $nargv -ne 14  ]; then
   echo "number of arguments must be 3, 5 (regular cubic sphere grid), 6 (stretched grid), 7 (regional esg grid) or 12, 14 ( global/regional nested grid)"
   echo "Usage for regular cubic sphere grid: (uses default target_lat, target_lon, retained for backwards compatibility ) "
   echo "  $0 resolution out_dir script_dir"
   echo "Usage for regular cubic sphere grid: "
   echo "  $0 resolution out_dir target_lon target_lat script_dir"
   echo "Usage for stretched grid: "
   echo "  $0 resolution out_dir stetch_fac target_lon target_lat script_dir"
   echo "Usage for regional esg grid: "
   echo "  $0 resolution out_dir target_lon target_lat halop2 pazi script_dir"
   echo "Usage for nested grid: (single nest with parent tile 6, retained for backwards compatibility)"
   echo "  $0 resolution out_dir stetch_fac target_lon target_lat refine_ratio_list istart_nest_list jstart_nest_list iend_nest_list jend_nest_list halo script_dir"
   echo "Usage for nested grid: (single or multiple nests)"
   echo "  $0 resolution out_dir stetch_fac target_lon target_lat num_nests parent_tile_list refine_ratio_list istart_nest_list jstart_nest_list iend_nest_list jend_nest_list halo script_dir"
   echo "FATAL ERROR: $0 wrong usage. Exiting."
   exit 1
fi

APRUN=${APRUN:-time}
export res=$1
export outdir=$2
if [ ! -s $outdir ]; then mkdir -p $outdir ;fi

nx=$(($res * 2))
cd $outdir

if [ $nargv -eq 3 -o $nargv -eq 5 ]; then
  export ntiles=6

  if [ $nargv -eq 3  ]; then
      export script_dir=$3
      export target_opts=""
  else
      export target_lon=$3
      export target_lat=$4
      export script_dir=$5
      export target_opts="--do_schmidt --stretch_factor 1.0 --target_lon ${target_lon} --target_lat ${target_lat}"
  fi

  export executable=${MAKEHGRIDEXEC:-$exec_dir/hafs_utils_make_hgrid.x}
  echo "Making uniform grids:   $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid ${target_opts}"

  $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid ${target_opts}
  export err=$?; err_chk

elif  [ $nargv -eq 6 ]; then
  export stretch_fac=$3
  export target_lon=$4
  export target_lat=$5
  export script_dir=$6
  export ntiles=6
  #export executable=$exec_dir/make_hgrid
  export executable=${MAKEHGRIDEXEC:-$exec_dir/hafs_utils_make_hgrid.x}

  echo "Making stretched grids:   $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid --do_schmidt --stretch_factor ${stretch_fac} --target_lon ${target_lon} --target_lat ${target_lat} "

  $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid --do_schmidt --stretch_factor ${stretch_fac} --target_lon ${target_lon} --target_lat ${target_lat}
  export err=$?; err_chk

elif  [ $nargv -eq 12 -o $nargv -eq 14 ]; then
  export stretch_fac=$3
  export target_lon=$4
  export target_lat=$5

  if  [ $nargv -eq 12 ]; then
      export nest_grids=1
      export parent_tile=6
      export refine_ratio=$6
      export istart_nest=$7
      export jstart_nest=${8}
      export iend_nest=${9}
      export jend_nest=${10}
      export halo=${11}
      export script_dir=${12}
  else
      export nest_grids=$6
      export parent_tile=$7
      export refine_ratio=$8
      export istart_nest=$9
      export jstart_nest=${10}
      export iend_nest=${11}
      export jend_nest=${12}
      export halo=${13}
      export script_dir=${14}
  fi
  if  [ $gtype = regional ]; then
   export ntiles=$(( 0+$nest_grids ))
  else
   export ntiles=$(( 6+$nest_grids ))
  fi
  export executable=${MAKEHGRIDEXEC:-$exec_dir/hafs_utils_make_hgrid.x}

  echo "Making nested grids: $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid --do_schmidt --stretch_factor ${stretch_fac} \
      --target_lon ${target_lon} --target_lat ${target_lat} --nest_grids $nest_grids --parent_tile $parent_tile --refine_ratio $refine_ratio \
      --istart_nest $istart_nest --jstart_nest $jstart_nest --iend_nest $iend_nest --jend_nest $jend_nest --halo $halo --great_circle_algorithm"

  $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid --do_schmidt --stretch_factor ${stretch_fac} \
      --target_lon ${target_lon} --target_lat ${target_lat} --nest_grids $nest_grids --parent_tile $parent_tile --refine_ratio $refine_ratio \
      --istart_nest $istart_nest --jstart_nest $jstart_nest --iend_nest $iend_nest --jend_nest $jend_nest --halo $halo --great_circle_algorithm
  export err=$?; err_chk

elif  [ $nargv -eq 7 -a ${regional_esg:-no} = yes ] ; then
  export target_lon=$3
  export target_lat=$4
  export pazi=$5
  export halop2=$6
  export script_dir=$7
  export executable=${MAKEHGRIDEXEC:-$exec_dir/hafs_utils_regional_esg_grid.x}

  # First, deal with regional esg grid parent or single regional esg grid
  n=1
  inest=$(( ${n} + 1 ))
  idim=$( echo ${idim_nest} | cut -d , -f ${n} )
  jdim=$( echo ${jdim_nest} | cut -d , -f ${n} )
  delx=$( echo ${delx_nest} | cut -d , -f ${n} )
  dely=$( echo ${dely_nest} | cut -d , -f ${n} )
  halop2=${halop2:-$(( halo+2 ))}
  (( lx=idim+halop2*2 ))
  (( ly=jdim+halop2*2 ))
  cat > ./regional_grid.nml << EOF
    &regional_grid_nml
      plon = ${target_lon}
      plat = ${target_lat}
      pazi = ${pazi:--180.}
      delx = ${delx}
      dely = ${dely}
      lx   = -${lx}
      ly   = -${ly}
    /
EOF

  $APRUN $executable
  export err=$?; err_chk
  mv regional_grid.nml C${res}_grid.tile7.nml
  mv regional_grid.nc C${res}_grid.tile7.nc

  # If needed, create a regional esg fine parent grid with its child/nest grid
  # resolution. It covers the regional esg parent grid, which can be subsetted
  # to generate collocated nest grid.
  if [ ${nest_grids} -ge 2 ]; then

  n=2
  delx=$( echo ${delx_nest} | cut -d , -f ${n} )
  dely=$( echo ${dely_nest} | cut -d , -f ${n} )
  refine=$( echo ${refine_ratio} | cut -d , -f ${n} )
  lx=$(( ${lx} * ${refine} ))
  ly=$(( ${ly} * ${refine} ))

  cat > ./regional_grid.nml << EOF
    &regional_grid_nml
      plon = ${target_lon}
      plat = ${target_lat}
      pazi = ${pazi:--180.}
      delx = ${delx}
      dely = ${dely}
      lx   = -${lx}
      ly   = -${ly}
    /
EOF

  $APRUN $executable
  export err=$?; err_chk
  mv regional_grid.nml C${res}_nest1res_grid.tile7.nml
  mv regional_grid.nc C${res}_nest1res_grid.tile7.nc

  # Subset to generate the regional esg nested grids
  for n in $(seq 2 ${nest_grids})
  do

  itile=$((6 + ${n}))
  refine=$( echo ${refine_ratio} | cut -d , -f ${n} )
  istart=$( echo ${istart_nest} | cut -d , -f ${n} )
  jstart=$( echo ${jstart_nest} | cut -d , -f ${n} )
  iend=$( echo ${iend_nest} | cut -d , -f ${n} )
  jend=$( echo ${jend_nest} | cut -d , -f ${n} )
  istart_sub=$(( $(( ${istart}+${halop2}*2-1 )) * ${refine} ))
  jstart_sub=$(( $(( ${jstart}+${halop2}*2-1 )) * ${refine} ))
  iend_sub=$(( $(( ${iend}+${halop2}*2 )) * ${refine} - 1 ))
  jend_sub=$(( $(( ${jend}+${halop2}*2 )) * ${refine} - 1 ))
  ncks -O -d nx,${istart_sub},${iend_sub} \
          -d ny,${jstart_sub},${jend_sub} \
          -d nxp,${istart_sub},$((${iend_sub}+1)) \
          -d nyp,${jstart_sub},$((${jend_sub}+1)) \
       C${res}_nest1res_grid.tile7.nc C${res}_nest1res_grid.tile${itile}.nc
  # Replace the tile value after subsetting
  ncap2 -s 'tile="'tile${itile}'"' C${res}_nest1res_grid.tile${itile}.nc C${res}_grid.tile${itile}.nc

  done

  fi # if [ ${nest_grids} -ge 2 ]; then

fi

#---------------------------------------------------------------------------------------
#export executable=$exec_dir/make_solo_mosaic
export executable=${MAKEMOSAICEXEC:-$exec_dir/hafs_utils_make_solo_mosaic.x}

if [ $gtype = uniform -o $gtype = stretch ] && [ $ntiles -eq 6 ]; then
  $APRUN $executable --num_tiles $ntiles --dir $outdir --mosaic C${res}_mosaic --tile_file C${res}_grid.tile1.nc,C${res}_grid.tile2.nc,C${res}_grid.tile3.nc,C${res}_grid.tile4.nc,C${res}_grid.tile5.nc,C${res}_grid.tile6.nc

elif [ $gtype = nest -a $ntiles -ge 7 ]; then

  file_list="C${res}_grid.tile7.nc"
  for itile in $(seq 8 $ntiles)
  do
    file_list="${file_list},C${res}_grid.tile${itile}.nc"
  done

  # mosaic file for global and nests
  $APRUN $executable --num_tiles $ntiles --dir $outdir --mosaic C${res}_mosaic --tile_file C${res}_grid.tile1.nc,C${res}_grid.tile2.nc,C${res}_grid.tile3.nc,C${res}_grid.tile4.nc,C${res}_grid.tile5.nc,C${res}_grid.tile6.nc,${file_list}
  export err=$?; err_chk

  # mosaic file for coarse grids only
  $APRUN $executable --num_tiles 6 --dir $outdir --mosaic C${res}_coarse_mosaic --tile_file C${res}_grid.tile1.nc,C${res}_grid.tile2.nc,C${res}_grid.tile3.nc,C${res}_grid.tile4.nc,C${res}_grid.tile5.nc,C${res}_grid.tile6.nc
  export err=$?; err_chk

  # mosaic file for nested grids only
  for itile in $(seq 7 $ntiles)
  do
    file_list="C${res}_grid.tile${itile}.nc"
    inest=$(($itile - 5))
    $APRUN $executable --num_tiles 1 --dir $outdir --mosaic C${res}_nested0${inest}_mosaic --tile_file ${file_list}
    export err=$?; err_chk
  done

# regional grid without nests
elif [ $gtype = regional -a $nest_grids -eq 1 ]; then

  $APRUN $executable --num_tiles 1 --dir $outdir --mosaic C${res}_mosaic --tile_file C${res}_grid.tile7.nc
  export err=$?; err_chk

# regional grid with nests
elif [ $gtype = regional -a $nest_grids -gt 1 ]; then

  ntiles=$(( 6+$nest_grids ))
  file_list="C${res}_grid.tile7.nc"
  for itile in $(seq 8 $ntiles)
  do
    file_list="${file_list},C${res}_grid.tile${itile}.nc"
  done

  # create mosaic for regional and nests
  $APRUN $executable --num_tiles $nest_grids --dir $outdir --mosaic C${res}_all_mosaic --tile_file ${file_list}
  export err=$?; err_chk

  # mosaic file for coarse grids only
  $APRUN $executable --num_tiles 1 --dir $outdir --mosaic C${res}_coarse_mosaic --tile_file C${res}_grid.tile7.nc
  export err=$?; err_chk
  $APRUN $executable --num_tiles 1 --dir $outdir --mosaic C${res}_mosaic --tile_file C${res}_grid.tile7.nc
  export err=$?; err_chk

  # mosaic file for nested grids only
  for itile in $(seq 8 $ntiles)
  do
    file_list="C${res}_grid.tile${itile}.nc"
    inest=$(($itile - 6))
    $APRUN $executable --num_tiles 1 --dir $outdir --mosaic C${res}_nested0${inest}_mosaic --tile_file ${file_list}
    export err=$?; err_chk
  done

fi

