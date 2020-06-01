#!/bin/ksh

#
# Updates:
#  May 15, 2020  Bill Ramstrom, HRD/AOML, Added multiple nest capability 
#

set -ax

nargv=$#
if [ $nargv -ne 3 -a $nargv -ne 5 -a $nargv -ne 6 -a $nargv -ne 12 -a $nargv -ne 14  ]; then 
   echo "number of arguments must be 3, 5 (regular cubic sphere grid), 6 (stretched grid), or 12, 14 ( nested grid)"
   echo "Usage for regular cubic sphere grid: (uses default target_lat, target_lon, retained for backwards compatibility ) "
   echo "  $0 resolution out_dir script_dir"
   echo "Usage for regular cubic sphere grid: "
   echo "  $0 resolution out_dir target_lon target_lat script_dir"
   echo "Usage for Stretched grid: "
   echo "  $0 resolution out_dir stetch_fac target_lon target_lat script_dir"
   echo "Usage for Nested grid: (single nest with parent tile 6, retained for backwards compatibility)"
   echo "  $0 resolution out_dir stetch_fac target_lon target_lat refine_ratio_list istart_nest_list jstart_nest_list iend_nest_list jend_nest_list halo script_dir"
   echo "Usage for Nested grid: (single or multiple nests)"
   echo "  $0 resolution out_dir stetch_fac target_lon target_lat num_nests parent_tile_list refine_ratio_list istart_nest_list jstart_nest_list iend_nest_list jend_nest_list halo script_dir"
   exit 1
fi

APRUN=${APRUN:-time}
export res=$1 
export outdir=$2
if [ ! -s $outdir ]; then  mkdir -p $outdir ;fi

nx=`expr $res \* 2 `
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

  export executable=${MAKEHGRIDEXEC:-$exec_dir/hafs_make_hgrid.x}
  if [ ! -s $executable ]; then
    echo "executable does not exist"
    exit 1 
  fi
  echo "Making uniform grids:   $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid ${target_opts}"

  $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid ${target_opts}

elif  [ $nargv -eq 6 ]; then
  export stretch_fac=$3
  export target_lon=$4
  export target_lat=$5
  export script_dir=$6
  export ntiles=6
  #export executable=$exec_dir/make_hgrid
  export executable=${MAKEHGRIDEXEC:-$exec_dir/hafs_make_hgrid.x}
  if [ ! -s $executable ]; then
    echo "executable does not exist"
    exit 1 
  fi

  echo "Making stretched grids:   $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid --do_schmidt --stretch_factor ${stretch_fac} --target_lon ${target_lon} --target_lat ${target_lat} "
 
  $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid --do_schmidt --stretch_factor ${stretch_fac} --target_lon ${target_lon} --target_lat ${target_lat} 

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
  export executable=${MAKEHGRIDEXEC:-$exec_dir/hafs_make_hgrid.x}
  if [ ! -s $executable ]; then
    echo "executable does not exist"
    exit 1 
  fi

  echo "Making nested grids: $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid --do_schmidt --stretch_factor ${stretch_fac} \
      --target_lon ${target_lon} --target_lat ${target_lat} --nest_grids $nest_grids --parent_tile $parent_tile --refine_ratio $refine_ratio \
      --istart_nest $istart_nest --jstart_nest $jstart_nest --iend_nest $iend_nest --jend_nest $jend_nest --halo $halo --great_circle_algorithm"

  $APRUN $executable --grid_type gnomonic_ed --nlon $nx --grid_name C${res}_grid --do_schmidt --stretch_factor ${stretch_fac} \
      --target_lon ${target_lon} --target_lat ${target_lat} --nest_grids $nest_grids --parent_tile $parent_tile --refine_ratio $refine_ratio \
      --istart_nest $istart_nest --jstart_nest $jstart_nest --iend_nest $iend_nest --jend_nest $jend_nest --halo $halo --great_circle_algorithm

fi

if [ $? -ne 0 ]; then
  echo "ERROR in running create C$res grid without halo "
  exit 1
fi


#---------------------------------------------------------------------------------------
#export executable=$exec_dir/make_solo_mosaic
export executable=${MAKEMOSAICEXEC:-$exec_dir/hafs_make_solo_mosaic.x}
if [ ! -s $executable ]; then
  echo "executable does not exist"
  exit 1 
fi

if [ $ntiles -eq 6 ]; then
  $APRUN $executable --num_tiles $ntiles --dir $outdir --mosaic C${res}_mosaic --tile_file C${res}_grid.tile1.nc,C${res}_grid.tile2.nc,C${res}_grid.tile3.nc,C${res}_grid.tile4.nc,C${res}_grid.tile5.nc,C${res}_grid.tile6.nc

elif [ $gtype = nest -a $ntiles -ge 7 ]; then

  file_list="C${res}_grid.tile7.nc"
  for itile in $(seq 8 $ntiles)
  do
    file_list="${file_list},C${res}_grid.tile${itile}.nc"
  done

  # mosaic file for globe and nests
  $APRUN $executable --num_tiles $ntiles --dir $outdir --mosaic C${res}_mosaic --tile_file C${res}_grid.tile1.nc,C${res}_grid.tile2.nc,C${res}_grid.tile3.nc,C${res}_grid.tile4.nc,C${res}_grid.tile5.nc,C${res}_grid.tile6.nc,${file_list}

  # mosaic file for coarse grids only
  $APRUN $executable --num_tiles 6 --dir $outdir --mosaic C${res}_coarse_mosaic --tile_file C${res}_grid.tile1.nc,C${res}_grid.tile2.nc,C${res}_grid.tile3.nc,C${res}_grid.tile4.nc,C${res}_grid.tile5.nc,C${res}_grid.tile6.nc

  # mosaic file for nested grids only
  for itile in $(seq 7 $ntiles)
  do
    file_list="C${res}_grid.tile${itile}.nc"
	inest=$(($itile - 5))
    $APRUN $executable --num_tiles 1 --dir $outdir --mosaic C${res}_nested0${inest}_mosaic --tile_file ${file_list}
  done
#
#special case for the regional grid. For now we have only 1 tile and it is tile 7
#
elif [ $gtype = regional ]; then
  $APRUN $executable --num_tiles 1 --dir $outdir --mosaic C${res}_mosaic --tile_file C${res}_grid.tile7.nc
fi

exit

