#!/bin/ksh

set -xe

TOTAL_TASKS=${TOTAL_TASKS:-4}
NCTSK=${NCTSK:-4}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-6}
OMP_STACKSIZE=${OMP_STACKSIZE:-2048m}
KMP_STACKSIZE=${KMP_STACKSIZE:-1024m}
APRUNS=${APRUNS:-"aprun -b -j1 -n1 -N1 -d1 -cc depth"}
APRUNF=${APRUNF:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth cfp"}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}
export APRUN=time

CDATE=${CDATE:-${YMDH}}
CASE=${CASE:-C768}
CRES=`echo $CASE | cut -c 2-`
export res=${res:-$CRES}
export gtype=${gtype:-regional}           # grid type = uniform, stretch, nest, or stand alone regional

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
COMhafs=${COMhafs:-${COMOUT}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
PARMhafs=${PARMhafs:-${HOMEhafs}/parm}
EXEChafs=${EXEChafs:-${HOMEhafs}/exec}
FIXhafs=${FIXhafs:-${HOMEhafs}/fix}

FIXam=${FIXhafs}/fix_am
FIXorog=${FIXhafs}/fix_orog
FIXfv3=${FIXhafs}/fix_fv3
FIXsfc_climo=${FIXhafs}/fix_sfc_climo

export script_dir=${USHhafs}
export exec_dir=${EXEChafs}
export out_dir=${OUTDIR:-${WORKhafs}/intercom/grid}
export DATA=${DATA:-${WORKhafs}/grid}

export MAKEHGRIDEXEC=${EXEChafs}/hafs_make_hgrid.x
export MAKEMOSAICEXEC=${EXEChafs}/hafs_make_solo_mosaic.x
export FILTERTOPOEXEC=${EXEChafs}/hafs_filter_topo.x
export FREGRIDEXEC=${EXEChafs}/hafs_fregrid.x
export OROGEXEC=${EXEChafs}/hafs_orog.x
export SHAVEEXEC=${EXEChafs}/hafs_shave.x
export SFCCLIMOEXEC=${EXEChafs}/hafs_sfc_climo_gen.x

export MAKEGRIDSSH=${USHhafs}/hafs_make_grid.sh
export MAKEOROGSSH=${USHhafs}/hafs_make_orog.sh
export FILTERTOPOSSH=${USHhafs}/hafs_filter_topo.sh

machine=${WHERE_AM_I:-wcoss_cray} # platforms: wcoss_cray, wcoss_dell_p3, hera, orion, jet

date

export gridfixdir=${gridfixdir:-'/let/hafs_grid/generate/grid'}
# If gridfixdir is specified and exists, use the grid fix files directly
if [ -d $gridfixdir ]; then
  echo "$gridfixdir is specified and exists."
  echo "Copy the grid fix files directly."
  cp -rp $gridfixdir/* ${out_dir}/
  ls ${out_dir}
  exit 0
fi

# Otherwise, generate grid according to the following parameters
#----------------------------------------------------------------
if [ $gtype = uniform ];  then
  echo "creating uniform ICs"
elif [ $gtype = stretch ]; then
  export stretch_fac=${stretch_fac:-1.5}      # Stretching factor for the grid
  export target_lon=${target_lon:--97.5}      # center longitude of the highest resolution tile
  export target_lat=${target_lat:-35.5}       # center latitude of the highest resolution tile
  echo "creating stretched grid"
elif [ $gtype = nest ] || [ $gtype = regional ]; then
  export stretch_fac=${stretch_fac:-1.0001}   # Stretching factor for the grid
  export target_lon=${target_lon:--62.0}      # center longitude of the highest resolution tile
  export target_lat=${target_lat:-22.0}       # center latitude of the highest resolution tile
  # Need for grid types: nest and regional
  export refine_ratio=${refine_ratio:-4}      # Specify the refinement ratio for nest grid
  export istart_nest=${istart_nest:-46}
  export jstart_nest=${jstart_nest:-238}
  export iend_nest=${iend_nest:-1485}
  export jend_nest=${jend_nest:-1287}
  export halo=${halo:-3}                      # halo size to be used in the atmosphere cubic sphere model for the grid tile.
  export halop1=${halop1:-4}                  # halo size that will be used for the orography and grid tile in chgres
  export halo0=${halo0:-0}                    # no halo, used to shave the filtered orography for use in the model
  if [ $gtype = nest ];then
   echo "creating nested grid"
  else
   echo "creating regional grid"
  fi
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest or regional"
  exit 1
fi

#----------------------------------------------------------------
#filter_topo parameters. C48->200km, C96->100km, C192->50km, C384->25km, C768->13km, 
# C1152->8.5km, C1536->6.5km, C2304->4.3km, C3072->3.2km, C4608->2.1km, C6144->1.6km
if [ $CRES -eq 48 ]; then 
 export cd4=0.12;  export max_slope=0.12; export n_del2_weak=4;   export peak_fac=1.1  
elif [ $CRES -eq 96 ]; then 
 export cd4=0.12;  export max_slope=0.12; export n_del2_weak=8;   export peak_fac=1.1  
elif [ $CRES -eq 128 ]; then
 export cd4=0.13;  export max_slope=0.12; export n_del2_weak=8;   export peak_fac=1.1
elif [ $CRES -eq 192 ]; then 
 export cd4=0.15;  export max_slope=0.12; export n_del2_weak=12;  export peak_fac=1.05  
elif [ $CRES -eq 384 ]; then 
 export cd4=0.15;  export max_slope=0.12; export n_del2_weak=12;  export peak_fac=1.0  
elif [ $CRES -eq 768 ]; then 
 export cd4=0.15;  export max_slope=0.12; export n_del2_weak=16;   export peak_fac=1.0  
elif [ $CRES -eq 1152 ]; then 
 export cd4=0.15;  export max_slope=0.16; export n_del2_weak=20;   export peak_fac=1.0  
elif [ $CRES -eq 1536 ]; then 
 export cd4=0.15;  export max_slope=0.24; export n_del2_weak=20;   export peak_fac=1.0  
elif [ $CRES -eq 2304 ]; then 
 export cd4=0.15;  export max_slope=0.27; export n_del2_weak=22;   export peak_fac=1.0  
elif [ $CRES -eq 3072 ]; then 
 export cd4=0.15;  export max_slope=0.30; export n_del2_weak=24;   export peak_fac=1.0  
elif [ $CRES -eq 4608 ]; then 
 export cd4=0.15;  export max_slope=0.33; export n_del2_weak=26;   export peak_fac=1.0  
elif [ $CRES -eq 6144 ]; then 
 export cd4=0.15;  export max_slope=0.36; export n_del2_weak=28;   export peak_fac=1.0  
else
 echo "grid C$CRES not supported, exit"
 exit 1
fi

date
#----------------------------------------------------------------
# Make grid and orography

export grid_dir=$DATA/grid
export orog_dir=$DATA/orog
if [ $gtype = uniform ] || [ $gtype = stretch ] ;  then
  export filter_dir=$DATA/filter_topo
elif [ $gtype = nest ] || [ $gtype = regional ] ;  then
  export filter_dir=$DATA/filter_topo
  export filter_dir=$orog_dir   # nested grid topography will be filtered online
fi
mkdir -p $grid_dir $orog_dir $filter_dir

if [ $gtype = uniform ] || [ $gtype = stretch ] ;  then
  export ntiles=6
  date
  echo "............ execute $MAKEGRIDSSH ................."
  if [ $gtype = uniform ];  then
    ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $script_dir
  elif [ $gtype = stretch ]; then
    ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat $script_dir
  fi
  date
  echo "............ execute $MAKEOROGSSH ................."
  # Run multiple tiles simulatneously for the orography

  echo "${APRUNO} $MAKEOROGSSH $CRES 1 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 2 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 3 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 4 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 5 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 6 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> orog.file1
fi
  chmod u+x $DATA/orog.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog.file1
  ${APRUNF} $DATA/orog.file1
  wait
  #rm $DATA/orog.file1
  date
  echo "............ execute $FILTERTOPOSSH .............."
  $FILTERTOPOSSH $CRES $grid_dir $orog_dir $filter_dir $cd4 $peak_fac $max_slope $n_del2_weak $script_dir $gtype
  echo "Grid and orography files are now prepared"
elif [ $gtype = nest ]; then
  export ntiles=7
  date
  echo "............ execute $MAKEGRIDSSH ................."
  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat $refine_ratio $istart_nest $jstart_nest $iend_nest $jend_nest $halo $script_dir
  date
  echo "............ execute $MAKEOROGSSH ................."
  # Run multiple tiles simulatneously for the orography
  echo "${APRUNO} $MAKEOROGSSH $CRES 1 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 2 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 3 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 4 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 5 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 6 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 7 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> orog.file1
fi
  chmod u+x $DATA/orog.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog.file1
  ${APRUNF} $DATA/orog.file1
  wait
  #rm $DATA/orog.file1
  date
  echo "Grid and orography files are now prepared"
elif [ $gtype = regional ]; then
  # We are now creating only 1 tile and it is tile 7
  export ntiles=1
  tile=7

  # number of parent points
  nptsx=`expr $iend_nest - $istart_nest + 1`
  nptsy=`expr $jend_nest - $jstart_nest + 1`
  # number of compute grid points
  npts_cgx=`expr $nptsx  \* $refine_ratio / 2`
  npts_cgy=`expr $nptsy  \* $refine_ratio / 2`
 
  # figure out how many columns/rows to add in each direction so we have at least 5 halo points
  # for make_hgrid and the orography program
  index=0
  add_subtract_value=0
  while (test "$index" -le "0")
  do
    add_subtract_value=`expr $add_subtract_value + 1`
    iend_nest_halo=`expr $iend_nest + $add_subtract_value`
    istart_nest_halo=`expr $istart_nest - $add_subtract_value`
    newpoints_i=`expr $iend_nest_halo - $istart_nest_halo + 1`
    newpoints_cg_i=`expr $newpoints_i  \* $refine_ratio / 2`
    diff=`expr $newpoints_cg_i - $npts_cgx`
    if [ $diff -ge 10 ]; then 
      index=`expr $index + 1`
    fi
  done
  jend_nest_halo=`expr $jend_nest + $add_subtract_value`
  jstart_nest_halo=`expr $jstart_nest - $add_subtract_value`

  echo "================================================================================== "
  echo "For refine_ratio= $refine_ratio" 
  echo " iend_nest= $iend_nest iend_nest_halo= $iend_nest_halo istart_nest= $istart_nest istart_nest_halo= $istart_nest_halo"
  echo " jend_nest= $jend_nest jend_nest_halo= $jend_nest_halo jstart_nest= $jstart_nest jstart_nest_halo= $jstart_nest_halo"
  echo "================================================================================== "

  echo "............ execute $MAKEGRIDSSH ................."
  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat $refine_ratio $istart_nest_halo $jstart_nest_halo $iend_nest_halo $jend_nest_halo $halo $script_dir

  date
  echo "............ execute $MAKEOROGSSH ................."
  #echo "$MAKEOROGSSH $CRES 7 $grid_dir $orog_dir $script_dir $FIXorog $DATA " >>$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 7 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> orog.file1
fi
  chmod u+x $DATA/orog.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog.file1
  ${APRUNF} $DATA/orog.file1
  wait
  #rm $DATA/orog.file1

  date
  echo "............ execute $FILTERTOPOSSH .............."
  ${APRUNS} $FILTERTOPOSSH $CRES $grid_dir $orog_dir $filter_dir $cd4 $peak_fac $max_slope $n_del2_weak $script_dir $gtype

  echo "............ execute shave to reduce grid and orography files to required compute size .............."
  cd $filter_dir
  # shave the orography file and then the grid file, the echo creates the input file that contains the number of required points
  # in x and y and the input and output file names.This first run of shave uses a halo of 4. This is necessary so that chgres will create BC's 
  # with 4 rows/columns which is necessary for pt.
  echo $npts_cgx $npts_cgy $halop1 \'$filter_dir/oro.${CASE}.tile${tile}.nc\' \'$filter_dir/oro.${CASE}.tile${tile}.shave.nc\' >input.shave.orog
  echo $npts_cgx $npts_cgy $halop1 \'$filter_dir/${CASE}_grid.tile${tile}.nc\' \'$filter_dir/${CASE}_grid.tile${tile}.shave.nc\' >input.shave.grid

  #aprun -n 1 -N 1 -j 1 -d 1 -cc depth $exec_dir/shave.x <input.shave.orog
  #aprun -n 1 -N 1 -j 1 -d 1 -cc depth $exec_dir/shave.x <input.shave.grid
  ${APRUNS} ${SHAVEEXEC} < input.shave.orog
  ${APRUNS} ${SHAVEEXEC} < input.shave.grid

  # Copy the shaved files with the halo of 4
  cp $filter_dir/oro.${CASE}.tile${tile}.shave.nc $out_dir/${CASE}_oro_data.tile${tile}.halo${halop1}.nc
  cp $filter_dir/${CASE}_grid.tile${tile}.shave.nc  $out_dir/${CASE}_grid.tile${tile}.halo${halop1}.nc

  # Now shave the orography file with no halo and then the grid file with a halo of 3. This is necessary for running the model.
  echo $npts_cgx $npts_cgy $halo \'$filter_dir/oro.${CASE}.tile${tile}.nc\' \'$filter_dir/oro.${CASE}.tile${tile}.shave.nc\' >input.shave.orog.halo${halo}
  echo $npts_cgx $npts_cgy $halo \'$filter_dir/${CASE}_grid.tile${tile}.nc\' \'$filter_dir/${CASE}_grid.tile${tile}.shave.nc\' >input.shave.grid.halo${halo}
  ${APRUNS} ${SHAVEEXEC} < input.shave.orog.halo${halo}
  ${APRUNS} ${SHAVEEXEC} < input.shave.grid.halo${halo}

  # Copy the shaved files with the halo of 3
  cp $filter_dir/oro.${CASE}.tile${tile}.shave.nc $out_dir/${CASE}_oro_data.tile${tile}.halo${halo}.nc
  cp $filter_dir/${CASE}_grid.tile${tile}.shave.nc  $out_dir/${CASE}_grid.tile${tile}.halo${halo}.nc

  # Now shave the orography file and then the grid file with a halo of 0. This is handy for running chgres.
  echo $npts_cgx $npts_cgy $halo0 \'$filter_dir/oro.${CASE}.tile${tile}.nc\' \'$filter_dir/oro.${CASE}.tile${tile}.shave.nc\' >input.shave.orog.halo${halo0}
  echo $npts_cgx $npts_cgy $halo0 \'$filter_dir/${CASE}_grid.tile${tile}.nc\' \'$filter_dir/${CASE}_grid.tile${tile}.shave.nc\' >input.shave.grid.halo${halo0}

  ${APRUNS} ${SHAVEEXEC} < input.shave.orog.halo${halo0}
  ${APRUNS} ${SHAVEEXEC} < input.shave.grid.halo${halo0}

  # Copy the shaved files with the halo of 0
  cp $filter_dir/oro.${CASE}.tile${tile}.shave.nc $out_dir/${CASE}_oro_data.tile${tile}.halo${halo0}.nc
  cp $filter_dir/${CASE}_grid.tile${tile}.shave.nc  $out_dir/${CASE}_grid.tile${tile}.halo${halo0}.nc

  echo "Grid and orography files are now prepared"

fi
#----------------------------------------------------------------

# For non-regional grids, copy grid and orography files to output directory.
if [ $gtype != regional ]; then
  echo "Copy grid and orography files to output directory"
  tile=1
  while [ $tile -le $ntiles ]; do
    cp $filter_dir/oro.${CASE}.tile${tile}.nc $out_dir/${CASE}_oro_data.tile${tile}.nc
    cp $grid_dir/${CASE}_grid.tile${tile}.nc  $out_dir/${CASE}_grid.tile${tile}.nc
    tile=`expr $tile + 1 `
  done
fi

# Copy mosaic file(s) to output directory.
cp $grid_dir/${CASE}_*mosaic.nc $out_dir/

#----------------------------------------------------------------
# Make surface static fields - vegetation type, soil type, etc.
#
# For global grids with a nest, the program is run twice.  First
# to create the fields for the six global tiles.  Then to create
# the fields on the high-res nest.  This is done because the
# ESMF libraries can not interpolate to seven tiles at once.
# Note:
# Stand-alone regional grids may be run with any number of
# tasks.  All other configurations must be run with a
# MULTIPLE OF SIX MPI TASKS.

date
input_sfc_climo_dir=${FIXhafs}/fix_sfc_climo
sfc_climo_workdir=$DATA/sfc_climo
sfc_climo_savedir=$out_dir/fix_sfc
mkdir -p $sfc_climo_workdir $sfc_climo_savedir
cd ${sfc_climo_workdir}

if [ $gtype = uniform ] || [ $gtype = stretch ]; then
  GRIDTYPE=NULL
  HALO=${HALO:-0}
  mosaic_file=${out_dir}/${CASE}_mosaic.nc
  the_orog_files='"'${CASE}'_oro_data.tile1.nc","'${CASE}'_oro_data.tile2.nc","'${CASE}'_oro_data.tile3.nc","'${CASE}'_oro_data.tile4.nc","'${CASE}'_oro_data.tile5.nc","'${CASE}'_oro_data.tile6.nc"'
elif [ $gtype = nest ]; then
  # First pass for global-nesting configuration will run the 6 global tiles
  GRIDTYPE=NULL
  HALO=${HALO:-0}
  mosaic_file=$out_dir/${CASE}_coarse_mosaic.nc
  the_orog_files='"'${CASE}'_oro_data.tile1.nc","'${CASE}'_oro_data.tile2.nc","'${CASE}'_oro_data.tile3.nc","'${CASE}'_oro_data.tile4.nc","'${CASE}'_oro_data.tile5.nc","'${CASE}'_oro_data.tile6.nc"'
elif [ $gtype = regional ]; then
  GRIDTYPE=regional
  tile=7
  HALO=$halop1
  ln -fs $out_dir/${CASE}_grid.tile${tile}.halo${HALO}.nc $out_dir/${CASE}_grid.tile${tile}.nc
  ln -fs $out_dir/${CASE}_oro_data.tile${tile}.halo${HALO}.nc $out_dir/${CASE}_oro_data.tile${tile}.nc
  mosaic_file=${out_dir}/${CASE}_mosaic.nc
  the_orog_files='"'${CASE}'_oro_data.tile'${tile}'.nc"'
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest or regional"
  exit 1
fi

cat>./fort.41<<EOF
&config
input_facsf_file="${input_sfc_climo_dir}/facsf.1.0.nc"
input_substrate_temperature_file="${input_sfc_climo_dir}/substrate_temperature.2.6x1.5.nc"
input_maximum_snow_albedo_file="${input_sfc_climo_dir}/maximum_snow_albedo.0.05.nc"
input_snowfree_albedo_file="${input_sfc_climo_dir}/snowfree_albedo.4comp.0.05.nc"
input_slope_type_file="${input_sfc_climo_dir}/slope_type.1.0.nc"
input_soil_type_file="${input_sfc_climo_dir}/soil_type.statsgo.0.05.nc"
input_vegetation_type_file="${input_sfc_climo_dir}/vegetation_type.igbp.0.05.nc"
input_vegetation_greenness_file="${input_sfc_climo_dir}/vegetation_greenness.0.144.nc"
mosaic_file_mdl="${mosaic_file}"
orog_dir_mdl="${out_dir}"
orog_files_mdl=${the_orog_files}
halo=${HALO}
maximum_snow_albedo_method="bilinear"
snowfree_albedo_method="bilinear"
vegetation_greenness_method="bilinear"
/
EOF
more ./fort.41

#APRUNC="srun --ntasks=6 --ntasks-per-node=6 --cpus-per-task=1"
$APRUNC $SFCCLIMOEXEC

rc=$?

if [[ $rc == 0 ]]; then
  if [[ $GRIDTYPE != "regional" ]]; then
    for files in *.nc
    do
      if [[ -f $files ]]; then
        mv $files ${sfc_climo_savedir}/${CASE}.${files}
      fi
    done
  else
    for files in *.halo.nc
    do
      if [[ -f $files ]]; then
        file2=${files%.halo.nc}
        mv $files ${sfc_climo_savedir}/${CASE}.${file2}.halo${HALO}.nc
      fi
    done
    for files in *.nc
    do
      if [[ -f $files ]]; then
        file2=${files%.nc}
        mv $files ${sfc_climo_savedir}/${CASE}.${file2}.halo0.nc
      fi
    done
  fi  # is regional?
else
  exit $rc
fi

if [ $gtype = regional ]; then
  rm -f $out_dir/${CASE}_grid.tile${tile}.nc
  rm -f $out_dir/${CASE}_oro_data.tile${tile}.nc
fi

#----------------------------------------------------------------
# Run for the global nest - tile 7.
# Second pass for global-nesting configuration will run the 7th tile
#----------------------------------------------------------------

if [ $gtype = nest ]; then

export GRIDTYPE=nest
HALO=${HALO:-0}
mosaic_file=$out_dir/${CASE}_nested_mosaic.nc
the_orog_files='"'${CASE}'_oro_data.tile7.nc"'

cat>./fort.41<<EOF
&config
input_facsf_file="${input_sfc_climo_dir}/facsf.1.0.nc"
input_substrate_temperature_file="${input_sfc_climo_dir}/substrate_temperature.1.0.nc"
input_maximum_snow_albedo_file="${input_sfc_climo_dir}/maximum_snow_albedo.0.05.nc"
input_snowfree_albedo_file="${input_sfc_climo_dir}/snowfree_albedo.4comp.0.05.nc"
input_slope_type_file="${input_sfc_climo_dir}/slope_type.1.0.nc"
input_soil_type_file="${input_sfc_climo_dir}/soil_type.statsgo.0.05.nc"
input_vegetation_type_file="${input_sfc_climo_dir}/vegetation_type.igbp.0.05.nc"
input_vegetation_greenness_file="${input_sfc_climo_dir}/vegetation_greenness.0.144.nc"
mosaic_file_mdl="${mosaic_file}"
orog_dir_mdl="${out_dir}"
orog_files_mdl=${the_orog_files}
halo=${HALO}
maximum_snow_albedo_method="bilinear"
snowfree_albedo_method="bilinear"
vegetation_greenness_method="bilinear"
/
EOF
more ./fort.41

#APRUNC="srun --ntasks=6 --ntasks-per-node=6 --cpus-per-task=1"
$APRUNC $SFCCLIMOEXEC

rc=$?

if [[ $rc == 0 ]]; then
  for files in *.nc
  do
    if [[ -f $files ]]; then
      mv $files ${sfc_climo_savedir}/${CASE}.${files}
    fi
  done
else
  exit $rc
fi

fi
# End of run for the global nest - tile 7.
#----------------------------------------------------------------

exit
