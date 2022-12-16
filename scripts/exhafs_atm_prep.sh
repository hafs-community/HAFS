#!/bin/sh

set -xe

NCP=${NCP:-'/bin/cp'}
NLN=${NLN:-'/bin/ln -sf'}
NDATE=${NDATE:-ndate}

TOTAL_TASKS=${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
OMP_STACKSIZE=${OMP_STACKSIZE:-2048m}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}
export APRUN=time

CDATE=${CDATE:-${YMDH}}
cyc=${cyc:-00}
STORM=${STORM:-FAKE}
STORMID=${STORMID:-00L}

export CASE=${CASE:-C768}
export CRES=`echo $CASE | cut -c 2-`
export gtype=${gtype:-regional}
export gridfixdir=${gridfixdir:-'/let/hafs_grid/generate/grid'}
export LEVS=${LEVS:-65}
export istart_nest=${istart_nest:-46}
export jstart_nest=${jstart_nest:-238}
export iend_nest=${iend_nest:-1485}
export jend_nest=${jend_nest:-1287}
export stretch_fac=${stretch_fac:-1.0001}
export target_lon=${target_lon:--62.0}
export target_lat=${target_lat:-22.0}
export refine_ratio=${refine_ratio:-4}
export res=${res:-$CRES}
export halo=${halo:-3}
export halop1=${halop1:-4}
export halo0=${halo0:-0}
export NTRAC=7

export regional_esg=${regional_esg:-no}
export idim_nest=${idim_nest:-1320}
export jdim_nest=${jdim_nest:-1320}
export delx_nest=${delx_nest:-0.03}
export dely_nest=${dely_nest:-0.03}
export halop2=${halop2:-5}
export pazi=${pazi:--180.}

export FIXam=${FIXhafs}/fix_am
export FIXorog=${FIXhafs}/fix_orog
export FIXfv3=${FIXhafs}/fix_fv3
export FIXsfc_climo=${FIXhafs}/fix_sfc_climo

if [ ${regional_esg} = yes ]; then
  export MAKEHGRIDEXEC=${EXEChafs}/hafs_regional_esg_grid.x
else
  export MAKEHGRIDEXEC=${EXEChafs}/hafs_make_hgrid.x
fi
export MAKEMOSAICEXEC=${EXEChafs}/hafs_make_solo_mosaic.x
export FILTERTOPOEXEC=${EXEChafs}/hafs_filter_topo.x
export FREGRIDEXEC=${EXEChafs}/hafs_fregrid.x
export OROGEXEC=${EXEChafs}/hafs_orog.x
export OROGGSLEXEC=${EXEChafs}/hafs_orog_gsl.x
export SHAVEEXEC=${EXEChafs}/hafs_shave.x
export SFCCLIMOEXEC=${EXEChafs}/hafs_sfc_climo_gen.x

export MAKEGRIDSSH=${USHhafs}/hafs_make_grid.sh
export MAKEOROGSSH=${USHhafs}/hafs_make_orog.sh
export MAKEOROGGSLSSH=${USHhafs}/hafs_make_orog_gsl.sh
export FILTERTOPOSSH=${USHhafs}/hafs_filter_topo.sh

export gridfixdir=${gridfixdir:-'/let/hafs_grid/generate/grid'}
export script_dir=${USHhafs}
export exec_dir=${EXEChafs}
export out_dir=${OUTDIR:-${WORKhafs}/intercom/grid}
export DATA=${DATA:-${WORKhafs}/atm_prep}

# If gridfixdir is specified and exists, use the grid fix files directly
if [ -d $gridfixdir ]; then
  echo "$gridfixdir is specified and exists."
  echo "Copy the grid fix files directly."
  cp -r $gridfixdir/* ${out_dir}/
  ls ${out_dir}
  exit
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
elif [ $gtype = nest -o $gtype = regional ]; then
  export stretch_fac=${stretch_fac:-1.0001}   # Stretching factor for the grid
  export target_lon=${target_lon:--62.0}      # center longitude of the highest resolution tile
  export target_lat=${target_lat:-22.0}       # center latitude of the highest resolution tile

  export nest_grids=${nest_grids:-1}
  export parent_tile=${parent_tile:-6}
  export refine_ratio=${refine_ratio:-4}
  export istart_nest=${istart_nest:-46}
  export jstart_nest=${jstart_nest:-238}
  export iend_nest=${iend_nest:-1485}
  export jend_nest=${jend_nest:-1287}

  export halo=${halo:-3}                      # halo size to be used in the atmosphere cubic sphere model for the grid tile.
  export halop1=${halop1:-4}                  # halo size that will be used for the orography and grid tile in chgres
  export halo0=${halo0:-0}                    # no halo, used to shave the filtered orography for use in the model

  export regional_esg=${regional_esg:-no}
  export idim_nest=${idim_nest:-1320}
  export jdim_nest=${jdim_nest:-1320}
  export delx_nest=${delx_nest:-0.03}
  export dely_nest=${dely_nest:-0.03}
  export halop2=${halop2:-5}

  echo "creating grid for gtype of $gtype"
  if [ ${regional_esg} = yes ]; then
    echo "using regional esg grid: ${regional_esg}"
  fi
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest or regional"
  exit 1
fi

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
  if [ ${machine} = "wcoss2" ]; then
     ncmd=$(cat $DATA/orog.file1 | wc -l)
     ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
#     $APRUNCFP  -n $ncmd_max cfp $DATA/orog.file1
     $DATA/orog.file1
  else
     ${APRUNF} $DATA/orog.file1
  fi
  wait
  #rm $DATA/orog.file1

  if [ ${use_orog_gsl:-no} = yes ]; then

  date
  echo "............ execute $MAKEOROGGSLSSH ................."
  # Run multiple tiles simulatneously for the gsl orography
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 1 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog_gsl.file1
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 2 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 3 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 4 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 5 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 6 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> $DATA/orog_gsl.file1
fi
  chmod u+x $DATA/orog_gsl.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog_gsl.file1
  if [ ${machine} = "wcoss2" ]; then
     ncmd=$(cat $DATA/orog_gsl.file1 | wc -l)
     ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
#     $APRUNCFP  -n $ncmd_max cfp $DATA/orog_gsl.file1
     $DATA/orog_gsl.file1
  else
     ${APRUNF} $DATA/orog_gsl.file1
  fi
  wait
  #rm $DATA/orog_gsl.file1

  fi

  date
  echo "............ execute $FILTERTOPOSSH .............."
  $FILTERTOPOSSH $CRES $grid_dir $orog_dir $filter_dir
  echo "Grid and orography files are now prepared"
elif [ $gtype = nest ]; then
  export ntiles=$((6 + ${nest_grids}))
  date
  echo "............ execute $MAKEGRIDSSH ................."
  #${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat $refine_ratio $istart_nest $jstart_nest $iend_nest $jend_nest $halo $script_dir
  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat \
       $nest_grids \
       "$parent_tile" \
       "$refine_ratio" \
       "$istart_nest" \
       "$jstart_nest" \
       "$iend_nest" \
       "$jend_nest" \
       $halo $script_dir
  date
  echo "............ execute $MAKEOROGSSH ................."
  # Run multiple tiles simulatneously for the orography
  echo "${APRUNO} $MAKEOROGSSH $CRES 1 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
  for itile in $(seq 2 $ntiles)
  do
    echo "${APRUNO} $MAKEOROGSSH $CRES ${itile} $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  done
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> orog.file1
fi
  chmod u+x $DATA/orog.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog.file1
  if [ ${machine} = "wcoss2" ]; then
     ncmd=$(cat $DATA/orog.file1 | wc -l)
     ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
#     $APRUNCFP  -n $ncmd_max cfp $DATA/orog.file1
     $DATA/orog.file1
  else
     ${APRUNF} $DATA/orog.file1
  fi
  wait
  #rm $DATA/orog.file1

  if [ ${use_orog_gsl:-no} = yes ]; then

  date
  echo "............ execute $MAKEOROGGSLSSH ................."
  # Run multiple tiles simulatneously for the gsl orography
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 1 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog_gsl.file1
  for itile in $(seq 2 $ntiles)
  do
    echo "${APRUNO} $MAKEOROGGSLSSH $CRES ${itile} -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  done
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> $DATA/orog_gsl.file1
fi
  chmod u+x $DATA/orog_gsl.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog_gsl.file1
  if [ ${machine} = "wcoss2" ]; then
     ncmd=$(cat $DATA/orog_gsl.file1 | wc -l)
     ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
#     $APRUNCFP  -n $ncmd_max cfp $DATA/orog_gsl.file1
     $DATA/orog_gsl.file1
  else
     ${APRUNF} $DATA/orog.file1
  fi
  wait
  #rm $DATA/orog_gsl.file1

  fi

  date
  echo "Grid and orography files are now prepared"

# regional grid with nests
elif [ $gtype = regional ] && [ ${nest_grids} -gt 1 ]; then

  export ntiles=$((6 + ${nest_grids}))
  echo "............ execute $MAKEGRIDSSH ................."

  if [ ${regional_esg:-no} = yes ] ; then

  echo "creating regional esg grid"
  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $target_lon $target_lat $pazi $halop2 $script_dir

  else

  #${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat $refine_ratio $istart_nest $jstart_nest $iend_nest $jend_nest $halo $script_dir
  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat \
       $nest_grids \
       "$parent_tile" \
       "$refine_ratio" \
       "$istart_nest" \
       "$jstart_nest" \
       "$iend_nest" \
       "$jend_nest" \
       $halo $script_dir

  fi

  date
  echo "............ execute $MAKEOROGSSH ................."
  # Run multiple tiles simulatneously for the orography
  echo "${APRUNO} $MAKEOROGSSH $CRES 7 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
  for itile in $(seq 8 $ntiles)
  do
    echo "${APRUNO} $MAKEOROGSSH $CRES ${itile} $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  done
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> orog.file1
fi
  chmod u+x $DATA/orog.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog.file1
  if [ ${machine} = "wcoss2" ]; then
     ncmd=$(cat $DATA/orog.file1 | wc -l)
     ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
#     $APRUNCFP  -n $ncmd_max cfp $DATA/orog.file1
     $DATA/orog.file1
  else
     ${APRUNF} $DATA/orog.file1
  fi
  wait
  #rm $DATA/orog.file1

  if [ ${use_orog_gsl:-no} = yes ]; then

  date
  echo "............ execute $MAKEOROGGSLSSH ................."
  # Run multiple tiles simulatneously for the gsl orography
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 7 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog_gsl.file1
  for itile in $(seq 8 $ntiles)
  do
    echo "${APRUNO} $MAKEOROGGSLSSH $CRES ${itile} -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  done
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> $DATA/orog_gsl.file1
fi
  chmod u+x $DATA/orog_gsl.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog_gsl.file1
  if [ ${machine} = "wcoss2" ]; then
     ncmd=$(cat $DATA/orog_gsl.file1 | wc -l)
     ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
#     $APRUNCFP  -n $ncmd_max cfp $DATA/orog_gsl.file1
     $DATA/orog_gsl.file1
  else
     ${APRUNF} $DATA/orog_gsl.file1
  fi
  wait
  #rm $DATA/orog_gsl.file1

  fi

  date
  echo "Grid and orography files are now prepared"

fi

if [ $gtype = regional ]; then
  # We are now creating only 1 tile and it is tile 7
  export ntiles=1
  tile=7

  # number of parent points
  iend_nest=`echo $iend_nest | cut -d , -f 1`
  istart_nest=`echo $istart_nest | cut -d , -f 1`
  jend_nest=`echo $jend_nest | cut -d , -f 1`
  jstart_nest=`echo $jstart_nest | cut -d , -f 1`
  refine_ratio=`echo $refine_ratio | cut -d , -f 1`

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
  if [ ${regional_esg:-no} = yes ] ; then

  if [ ${nest_grids} -eq 1 ] ; then
    echo "Creating regional esg grid"
    ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $target_lon $target_lat $pazi $halop2 $script_dir
  else
    echo "Regional esg grid parent already generated. No need to generate again."
  fi

  else

  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat $refine_ratio $istart_nest_halo $jstart_nest_halo $iend_nest_halo $jend_nest_halo $halo $script_dir

  fi

  date
  echo "............ execute $MAKEOROGSSH ................."
  #echo "$MAKEOROGSSH $CRES 7 $grid_dir $orog_dir $script_dir $FIXorog $DATA " >$DATA/orog.file1
  echo "${APRUNO} $MAKEOROGSSH $CRES 7 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> orog.file1
fi
  chmod u+x $DATA/orog.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog.file1
  if [ ${machine} = "wcoss2" ]; then
     ncmd=$(cat $DATA/orog.file1 | wc -l)
     ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
#     $APRUNCFP  -n $ncmd_max cfp $DATA/orog.file1
     $DATA/orog.file1
  else
     ${APRUNF} $DATA/orog.file1
  fi
  wait
  #rm $DATA/orog.file1

  date
  echo "............ execute $FILTERTOPOSSH .............."
  ${APRUNS} $FILTERTOPOSSH $CRES $grid_dir $orog_dir $filter_dir

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

  if [ ${use_orog_gsl:-no} = yes ]; then

  date
  echo "............ execute $MAKEOROGGSLSSH ................."
  echo "${APRUNO} $MAKEOROGGSLSSH $CRES 7 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog_gsl.file1
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> $DATA/orog_gsl.file1
fi
  chmod u+x $DATA/orog_gsl.file1
  #aprun -j 1 -n 4 -N 4 -d 6 -cc depth cfp $DATA/orog_gsl.file1
  if [ ${machine} = "wcoss2" ]; then
     ncmd=$(cat $DATA/orog_gsl.file1 | wc -l)
     ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
#     $APRUNCFP  -n $ncmd_max cfp $DATA/orog_gsl.file1
     $DATA/orog_gsl.file1
  else
     ${APRUNF} $DATA/orog_gsl.file1
  fi
  wait
  #rm $DATA/orog_gsl.file1

  cp $orog_dir/C${res}_oro_data_*.tile${tile}*.nc $out_dir/  # gsl drag suite oro_data files

  fi

  echo "Grid and orography files are now prepared"

fi
#----------------------------------------------------------------

# Copy mosaic file(s) to output directory.
cp $grid_dir/${CASE}_*mosaic.nc $out_dir/

# For non-regional grids, copy grid and orography files to output directory.
if [ $gtype = uniform -o $gtype = stretch -o $gtype = nest ]; then
  echo "Copy grid and orography files to output directory"
  tile=1
  ntiles=`expr ${nest_grids} + 6`
  while [ $tile -le $ntiles ]; do
    cp $filter_dir/oro.${CASE}.tile${tile}.nc $out_dir/${CASE}_oro_data.tile${tile}.nc
    if [ ${use_orog_gsl:-no} = yes ]; then
      cp $orog_dir/${CASE}_oro_data_ls.tile${tile}.nc $out_dir/${CASE}_oro_data_ls.tile${tile}.nc
      cp $orog_dir/${CASE}_oro_data_ss.tile${tile}.nc $out_dir/${CASE}_oro_data_ss.tile${tile}.nc
    fi
    cp $grid_dir/${CASE}_grid.tile${tile}.nc  $out_dir/${CASE}_grid.tile${tile}.nc
    tile=`expr $tile + 1 `
  done
fi

if [ $gtype = regional -a $nest_grids -gt 1 ]; then
  cp -p $out_dir/${CASE}_all_mosaic.nc $out_dir/${CASE}_mosaic.nc
  echo "Copy grid and orography files to output directory"
  tile=8
  ntiles=`expr ${nest_grids} + 6`
  while [ $tile -le $ntiles ]; do
    cp $filter_dir/oro.${CASE}.tile${tile}.nc $out_dir/${CASE}_oro_data.tile${tile}.nc
    if [ ${use_orog_gsl:-no} = yes ]; then
      cp $orog_dir/${CASE}_oro_data_ls.tile${tile}.nc $out_dir/${CASE}_oro_data_ls.tile${tile}.nc
      cp $orog_dir/${CASE}_oro_data_ss.tile${tile}.nc $out_dir/${CASE}_oro_data_ss.tile${tile}.nc
    fi
    cp $grid_dir/${CASE}_grid.tile${tile}.nc  $out_dir/${CASE}_grid.tile${tile}.nc
    tile=`expr $tile + 1 `
  done
fi

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
  if [ $nest_grids -gt 1 ];  then
    mosaic_file=${out_dir}/${CASE}_coarse_mosaic.nc
  else
    mosaic_file=${out_dir}/${CASE}_mosaic.nc
  fi
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
if [[ ! -e ./hafs_sfc_climo_gen.x ]]; then
  cp -p $SFCCLIMOEXEC ./hafs_sfc_climo_gen.x
fi
$APRUNC ./hafs_sfc_climo_gen.x
#$APRUNC $SFCCLIMOEXEC

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
# Run for the global or regional nested tiles
# Second pass for global-nesting or regional-nesting configuration will run the 7+th/8+th tiles
#----------------------------------------------------------------

if [ $gtype = nest -o $nest_grids -gt 1 ];  then

ntiles=$(( ${nest_grids} + 6 ))
export GRIDTYPE=nest
HALO=0

if [ $gtype = regional ]; then
  stile=8
else
  stile=7
fi

for itile in $(seq $stile $ntiles)
do

inest=$(($itile + 2 - $stile))
mosaic_file=$out_dir/${CASE}_nested0${inest}_mosaic.nc
the_orog_files='"'${CASE}'_oro_data.tile'${itile}'.nc"'

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
if [[ ! -e ./hafs_sfc_climo_gen.x ]]; then
  cp -p $SFCCLIMOEXEC ./hafs_sfc_climo_gen.x
fi
$APRUNC ./hafs_sfc_climo_gen.x
#$APRUNC $SFCCLIMOEXEC

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

done

fi
# End of run for the global or regional nested tiles.
#----------------------------------------------------------------

exit
