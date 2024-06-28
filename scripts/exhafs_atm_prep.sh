#!/bin/sh
################################################################################
# Script Name: exhafs_atm_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS atmopsheric preprocessing steps to generate the
#   model grid and geographical files (topography, surface climatology, etc.)
################################################################################
set -x -o pipefail

if [[ ${ATM_PREP_MVNEST:-NO} != YES ]]; then

# Deterministic or ensemble
if [ ${ENSDA:-NO} = YES ]; then
  export CASE=${CASE_ENS:-C768}
  export gtype=${gtype_ens:-regional}
  export gridfixdir=${gridfixdir_ens:-'/let/hafs_grid/generate/grid_ens'}
  export LEVS=${LEVS_ENS:-65}
  export istart_nest=${istart_nest_ens:-46}
  export jstart_nest=${jstart_nest_ens:-238}
  export iend_nest=${iend_nest_ens:-1485}
  export jend_nest=${jend_nest_ens:-1287}
  export stretch_fac=${stretch_fac_ens:-1.0001}
  export target_lon=${target_lon_ens:--62.0}
  export target_lat=${target_lat_ens:-22.0}
  export refine_ratio=${refine_ratio_ens:-4}
  export regional_esg=${regional_esg_ens:-no}
  export idim_nest=${idim_nest_ens:-1320}
  export jdim_nest=${jdim_nest_ens:-1320}
  export delx_nest=${delx_nest_ens:-0.03}
  export dely_nest=${dely_nest_ens:-0.03}
  export halop2=${halop2_ens:-5}
  export OUTDIR=${OUTDIR:-${WORKhafs}/intercom/grid_ens/${CASE}}
else
  export CASE=${CASE:-C768}
  export gtype=${gtype:-regional}
  export LEVS=${LEVS:-65}
  export istart_nest=${istart_nest:-46}
  export jstart_nest=${jstart_nest:-238}
  export iend_nest=${iend_nest:-1485}
  export jend_nest=${jend_nest:-1287}
  export stretch_fac=${stretch_fac:-1.0001}
  export target_lon=${target_lon:--62.0}
  export target_lat=${target_lat:-22.0}
  export refine_ratio=${refine_ratio:-4}
  export regional_esg=${regional_esg:-no}
  export idim_nest=${idim_nest:-1320}
  export jdim_nest=${jdim_nest:-1320}
  export delx_nest=${delx_nest:-0.03}
  export dely_nest=${dely_nest:-0.03}
  export halop2=${halop2:-5}
  export OUTDIR=${OUTDIR:-${WORKhafs}/intercom/grid/${CASE}}
fi

else # Moving nest is enabled

if [[ "${is_moving_nest}" = *".true."* ]]; then
  export CASE=${CASE_mvnest1res:-C768}
  export gtype=${gtype_mvnest1res:-regional}
  export gridfixdir=${gridfixdir_mvnest1res:-'/let/hafs_grid/generate/grid_mvnest1res'}
  export LEVS=${LEVS_ENS:-65}
  export stretch_fac=${stretch_fac_mvnest1res:-1.0001}
  export target_lon=${target_lon_mvnest1res:--62.0}
  export target_lat=${target_lat_mvnest1res:-22.0}
  export refine_ratio=${refine_ratio_mvnest1res:-4}
  export nest_grids=${nest_grids_mvnest1res:-1}
  export parent_grid_num=${parent_grid_num_mvnest1res:-1}
  export parent_tile=${parent_tile_mvnest1res:-1}
  export istart_nest=${istart_nest_mvnest1res:-46}
  export jstart_nest=${jstart_nest_mvnest1res:-238}
  export iend_nest=${iend_nest_mvnest1res:-1485}
  export jend_nest=${jend_nest_mvnest1res:-1287}
  export regional_esg=${regional_esg_mvnest1res:-no}
  export idim_nest=${idim_nest_mvnest1res:-3960}
  export jdim_nest=${jdim_nest_mvnest1res:-3960}
  export delx_nest=${delx_nest_mvnest1res:-0.01}
  export dely_nest=${dely_nest_mvnest1res:-0.01}
  export halop2=${halop2_mvnest1res:-15}
  export OUTDIR=${WORKhafs}/intercom/grid_mvnest1res/${CASE}
fi

fi

export CRES=$(echo $CASE | cut -c 2-)
export res=${res:-$CRES}
export halo=${halo:-3}
export halop1=${halop1:-4}
export halo0=${halo0:-0}
export NTRAC=7
export pazi=${pazi:--180.}

export FIXam=${FIXhafs}/fix_am
export FIXorog=${FIXhafs}/fix_orog
export FIXfv3=${FIXhafs}/fix_fv3
export FIXsfc_climo=${FIXhafs}/fix_sfc_climo

if [ ${regional_esg} = yes ]; then
  export MAKEHGRIDEXEC=${EXEChafs}/hafs_utils_regional_esg_grid.x
else
  export MAKEHGRIDEXEC=${EXEChafs}/hafs_utils_make_hgrid.x
fi
export MAKEMOSAICEXEC=${EXEChafs}/hafs_utils_make_solo_mosaic.x
export FILTERTOPOEXEC=${EXEChafs}/hafs_utils_filter_topo.x
export FREGRIDEXEC=${EXEChafs}/hafs_utils_fregrid.x
export OROGEXEC=${EXEChafs}/hafs_utils_orog.x
export OROGGSLEXEC=${EXEChafs}/hafs_utils_orog_gsl.x
export SHAVEEXEC=${EXEChafs}/hafs_utils_shave.x
export SFCCLIMOEXEC=${EXEChafs}/hafs_utils_sfc_climo_gen.x

export MAKEGRIDSSH=${USHhafs}/hafs_make_grid.sh
export MAKEOROGSSH=${USHhafs}/hafs_make_orog.sh
export MAKEOROGGSLSSH=${USHhafs}/hafs_make_orog_gsl.sh
export FILTERTOPOSSH=${USHhafs}/hafs_filter_topo.sh

export gridfixdir=${gridfixdir:-'/let/hafs_grid/generate/grid'}
export script_dir=${USHhafs}
export exec_dir=${EXEChafs}
export out_dir=${OUTDIR:-${WORKhafs}/intercom/grid}
export DATA=${DATA:-${WORKhafs}/atm_prep}
mkdir -p ${out_dir}

# If gridfixdir is specified and exists, use the grid fix files directly
if [ -d $gridfixdir ]; then
  echo "$gridfixdir is specified and exists."
  echo "Copy the grid fix files directly."
  ${NCP} -r $gridfixdir/* ${out_dir}/
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
  echo "FATAL ERROR: please specify grid type with 'gtype' as uniform, stretch, nest or regional"
  exit 1
fi

#----------------------------------------------------------------
# Make grid and orography

export grid_dir=$DATA/grid
export orog_dir=$DATA/orog
if [ $gtype = uniform ] || [ $gtype = stretch ];  then
  export filter_dir=$DATA/filter_topo
elif [ $gtype = nest ] || [ $gtype = regional ];  then
# export filter_dir=$DATA/filter_topo
  export filter_dir=$orog_dir   # nested grid topography will be filtered online
fi
mkdir -p $grid_dir $orog_dir $filter_dir

if [ $gtype = uniform ] || [ $gtype = stretch ];  then
  export ntiles=6
  date
  echo "............ execute $MAKEGRIDSSH ................."
  if [ $gtype = uniform ];  then
    ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $script_dir
    export err=$?; err_chk
  elif [ $gtype = stretch ]; then
    ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat $script_dir
    export err=$?; err_chk
  fi
  date
  echo "............ execute $MAKEOROGSSH ................."
  # Run multiple tiles simulatneously for the orography
  echo "${APRUN} $MAKEOROGSSH $CRES 1 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
  echo "${APRUN} $MAKEOROGSSH $CRES 2 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUN} $MAKEOROGSSH $CRES 3 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUN} $MAKEOROGSSH $CRES 4 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUN} $MAKEOROGSSH $CRES 5 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  echo "${APRUN} $MAKEOROGSSH $CRES 6 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ] || [ "$machine" = hercules ]; then
    echo 'wait' >> orog.file1
  fi
  chmod u+x $DATA/orog.file1
  time $DATA/orog.file1
  wait
  #rm $DATA/orog.file1

  if [ ${use_orog_gsl:-no} = yes ]; then

  date
  echo "............ execute $MAKEOROGGSLSSH ................."
  # Run multiple tiles simulatneously for the gsl orography
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 1 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog_gsl.file1
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 2 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 3 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 4 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 5 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 6 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ] || [ "$machine" = hercules ]; then
    echo 'wait' >> $DATA/orog_gsl.file1
  fi
  chmod u+x $DATA/orog_gsl.file1
  time $DATA/orog_gsl.file1
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
  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat \
       $nest_grids \
       "$parent_tile" \
       "$refine_ratio" \
       "$istart_nest" \
       "$jstart_nest" \
       "$iend_nest" \
       "$jend_nest" \
       $halo $script_dir
  export err=$?; err_chk
  date
  echo "............ execute $MAKEOROGSSH ................."
  # Run multiple tiles simulatneously for the orography
  echo "${APRUN} $MAKEOROGSSH $CRES 1 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
  for itile in $(seq 2 $ntiles); do
    echo "${APRUN} $MAKEOROGSSH $CRES ${itile} $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  done
  if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ] || [ "$machine" = hercules ]; then
    echo 'wait' >> orog.file1
  fi
  chmod u+x $DATA/orog.file1
  time $DATA/orog.file1
  wait
  #rm $DATA/orog.file1

  if [ ${use_orog_gsl:-no} = yes ]; then

  date
  echo "............ execute $MAKEOROGGSLSSH ................."
  # Run multiple tiles simulatneously for the gsl orography
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 1 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog_gsl.file1
  for itile in $(seq 2 $ntiles); do
    echo "${APRUN} $MAKEOROGGSLSSH $CRES ${itile} -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  done
  if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ] || [ "$machine" = hercules ]; then
    echo 'wait' >> $DATA/orog_gsl.file1
  fi
  chmod u+x $DATA/orog_gsl.file1
  time $DATA/orog_gsl.file1
  wait
  #rm $DATA/orog_gsl.file1

  fi

  date
  echo "Grid and orography files are now prepared"

# regional grid with nests
elif [ $gtype = regional ] && [ ${nest_grids} -gt 1 ]; then

  export ntiles=$((6 + ${nest_grids}))
  echo "............ execute $MAKEGRIDSSH ................."

  if [ ${regional_esg:-no} = yes ]; then

  echo "creating regional esg grid"
  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $target_lon $target_lat $pazi $halop2 $script_dir
  export err=$?; err_chk

  else

  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat \
       $nest_grids \
       "$parent_tile" \
       "$refine_ratio" \
       "$istart_nest" \
       "$jstart_nest" \
       "$iend_nest" \
       "$jend_nest" \
       $halo $script_dir
  export err=$?; err_chk

  fi

  date
  echo "............ execute $MAKEOROGSSH ................."
  # Run multiple tiles simulatneously for the orography
  echo "${APRUN} $MAKEOROGSSH $CRES 7 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
  for itile in $(seq 8 $ntiles); do
    echo "${APRUN} $MAKEOROGSSH $CRES ${itile} $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog.file1
  done
  if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ] || [ "$machine" = hercules ]; then
    echo 'wait' >> orog.file1
  fi
  chmod u+x $DATA/orog.file1
  time $DATA/orog.file1
  wait
  #rm $DATA/orog.file1

  if [ ${use_orog_gsl:-no} = yes ]; then

  date
  echo "............ execute $MAKEOROGGSLSSH ................."
  # Run multiple tiles simulatneously for the gsl orography
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 7 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog_gsl.file1
  for itile in $(seq 8 $ntiles); do
    echo "${APRUN} $MAKEOROGGSLSSH $CRES ${itile} -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >>$DATA/orog_gsl.file1
  done
  if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ] || [ "$machine" = hercules ]; then
    echo 'wait' >> $DATA/orog_gsl.file1
  fi
  chmod u+x $DATA/orog_gsl.file1
  time $DATA/orog_gsl.file1
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
  iend_nest=$(echo $iend_nest | cut -d , -f 1)
  istart_nest=$(echo $istart_nest | cut -d , -f 1)
  jend_nest=$(echo $jend_nest | cut -d , -f 1)
  jstart_nest=$(echo $jstart_nest | cut -d , -f 1)
  refine_ratio=$(echo $refine_ratio | cut -d , -f 1)

  nptsx=$(($iend_nest - $istart_nest + 1))
  nptsy=$(($jend_nest - $jstart_nest + 1))
  # number of compute grid points
  npts_cgx=$(($nptsx * $refine_ratio / 2))
  npts_cgy=$(($nptsy * $refine_ratio / 2))

  # figure out how many columns/rows to add in each direction so we have at least 5 halo points
  # for make_hgrid and the orography program
  index=0
  add_subtract_value=0
  while (test "$index" -le "0"); do
    add_subtract_value=$(($add_subtract_value + 1))
	iend_nest_halo=$(($iend_nest + $add_subtract_value))
	istart_nest_halo=$(($istart_nest - $add_subtract_value))
	newpoints_i=$(($iend_nest_halo - $istart_nest_halo + 1))
	newpoints_cg_i=$(($newpoints_i * $refine_ratio / 2))
	diff=$(($newpoints_cg_i - $npts_cgx))
    if [ $diff -ge 10 ]; then
      index=$(($index + 1))
    fi
  done
  jend_nest_halo=$(($jend_nest + $add_subtract_value))
  jstart_nest_halo=$(($jstart_nest - $add_subtract_value))

  echo "================================================================================== "
  echo "For refine_ratio= $refine_ratio"
  echo " iend_nest= $iend_nest iend_nest_halo= $iend_nest_halo istart_nest= $istart_nest istart_nest_halo= $istart_nest_halo"
  echo " jend_nest= $jend_nest jend_nest_halo= $jend_nest_halo jstart_nest= $jstart_nest jstart_nest_halo= $jstart_nest_halo"
  echo "================================================================================== "

  echo "............ execute $MAKEGRIDSSH ................."
  if [ ${regional_esg:-no} = yes ]; then

  if [ ${nest_grids} -eq 1 ]; then
    echo "Creating regional esg grid"
    ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $target_lon $target_lat $pazi $halop2 $script_dir
    export err=$?; err_chk
  else
    echo "Regional esg grid parent already generated. No need to generate again."
  fi

  else

  ${APRUNS} $MAKEGRIDSSH $CRES $grid_dir $stretch_fac $target_lon $target_lat $refine_ratio $istart_nest_halo $jstart_nest_halo $iend_nest_halo $jend_nest_halo $halo $script_dir
  export err=$?; err_chk

  fi

  date
  echo "............ execute $MAKEOROGSSH ................."
  echo "${APRUN} $MAKEOROGSSH $CRES 7 $grid_dir $orog_dir $script_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog.file1
  if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ] || [ "$machine" = hercules ]; then
    echo 'wait' >> orog.file1
  fi
  chmod u+x $DATA/orog.file1
  time $DATA/orog.file1
  wait
  #rm $DATA/orog.file1

  date
  echo "............ execute $FILTERTOPOSSH .............."
  ${APRUN} $FILTERTOPOSSH $CRES $grid_dir $orog_dir $filter_dir
  export err=$?; err_chk

  echo "............ execute shave to reduce grid and orography files to required compute size .............."
  cd $filter_dir
  # shave the orography file and then the grid file, the echo creates the input file that contains the number of required points
  # in x and y and the input and output file names.This first run of shave uses a halo of 4. This is necessary so that chgres will create BC's
  # with 4 rows/columns which is necessary for pt.
  echo $npts_cgx $npts_cgy $halop1 \'$filter_dir/oro.${CASE}.tile${tile}.nc\' \'$filter_dir/oro.${CASE}.tile${tile}.shave.nc\' >input.shave.orog
  echo $npts_cgx $npts_cgy $halop1 \'$filter_dir/${CASE}_grid.tile${tile}.nc\' \'$filter_dir/${CASE}_grid.tile${tile}.shave.nc\' >input.shave.grid

  ${APRUNS} ${SHAVEEXEC} < input.shave.orog
  export err=$?; err_chk
  ${APRUNS} ${SHAVEEXEC} < input.shave.grid
  export err=$?; err_chk

  # Copy the shaved files with the halo of 4
  ${NCP} $filter_dir/oro.${CASE}.tile${tile}.shave.nc $out_dir/${CASE}_oro_data.tile${tile}.halo${halop1}.nc
  ${NCP} $filter_dir/${CASE}_grid.tile${tile}.shave.nc  $out_dir/${CASE}_grid.tile${tile}.halo${halop1}.nc

  # Now shave the orography file with no halo and then the grid file with a halo of 3. This is necessary for running the model.
  echo $npts_cgx $npts_cgy $halo \'$filter_dir/oro.${CASE}.tile${tile}.nc\' \'$filter_dir/oro.${CASE}.tile${tile}.shave.nc\' >input.shave.orog.halo${halo}
  echo $npts_cgx $npts_cgy $halo \'$filter_dir/${CASE}_grid.tile${tile}.nc\' \'$filter_dir/${CASE}_grid.tile${tile}.shave.nc\' >input.shave.grid.halo${halo}
  ${APRUNS} ${SHAVEEXEC} < input.shave.orog.halo${halo}
  export err=$?; err_chk
  ${APRUNS} ${SHAVEEXEC} < input.shave.grid.halo${halo}
  export err=$?; err_chk

  # Copy the shaved files with the halo of 3
  ${NCP} $filter_dir/oro.${CASE}.tile${tile}.shave.nc $out_dir/${CASE}_oro_data.tile${tile}.halo${halo}.nc
  ${NCP} $filter_dir/${CASE}_grid.tile${tile}.shave.nc  $out_dir/${CASE}_grid.tile${tile}.halo${halo}.nc

  # Now shave the orography file and then the grid file with a halo of 0. This is handy for running chgres.
  echo $npts_cgx $npts_cgy $halo0 \'$filter_dir/oro.${CASE}.tile${tile}.nc\' \'$filter_dir/oro.${CASE}.tile${tile}.shave.nc\' >input.shave.orog.halo${halo0}
  echo $npts_cgx $npts_cgy $halo0 \'$filter_dir/${CASE}_grid.tile${tile}.nc\' \'$filter_dir/${CASE}_grid.tile${tile}.shave.nc\' >input.shave.grid.halo${halo0}

  ${APRUNS} ${SHAVEEXEC} < input.shave.orog.halo${halo0}
  export err=$?; err_chk
  ${APRUNS} ${SHAVEEXEC} < input.shave.grid.halo${halo0}
  export err=$?; err_chk

  # Copy the shaved files with the halo of 0
  ${NCP} $filter_dir/oro.${CASE}.tile${tile}.shave.nc $out_dir/${CASE}_oro_data.tile${tile}.halo${halo0}.nc
  ${NCP} $filter_dir/${CASE}_grid.tile${tile}.shave.nc  $out_dir/${CASE}_grid.tile${tile}.halo${halo0}.nc

  if [ ${use_orog_gsl:-no} = yes ]; then

  date
  echo "............ execute $MAKEOROGGSLSSH ................."
  echo "${APRUN} $MAKEOROGGSLSSH $CRES 7 -999 $grid_dir $orog_dir $FIXorog $DATA ${BACKGROUND}" >$DATA/orog_gsl.file1
  if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ] || [ "$machine" = hercules ]; then
    echo 'wait' >> $DATA/orog_gsl.file1
  fi
  chmod u+x $DATA/orog_gsl.file1
  time $DATA/orog_gsl.file1
  wait
  #rm $DATA/orog_gsl.file1

  ${NCP} $orog_dir/C${res}_oro_data_*.tile${tile}*.nc $out_dir/  # gsl drag suite oro_data files

  fi

  echo "Grid and orography files are now prepared"

fi
#----------------------------------------------------------------

# Copy mosaic file(s) to output directory.
${NCP} $grid_dir/${CASE}_*mosaic.nc $out_dir/

# For non-regional grids, copy grid and orography files to output directory.
if [ $gtype = uniform -o $gtype = stretch -o $gtype = nest ]; then
  echo "Copy grid and orography files to output directory"
  tile=1
  ntiles=$((${nest_grids} + 6))
  while [ $tile -le $ntiles ]; do
    ${NCP} $filter_dir/oro.${CASE}.tile${tile}.nc $out_dir/${CASE}_oro_data.tile${tile}.nc
    if [ ${use_orog_gsl:-no} = yes ]; then
      ${NCP} $orog_dir/${CASE}_oro_data_ls.tile${tile}.nc $out_dir/${CASE}_oro_data_ls.tile${tile}.nc
      ${NCP} $orog_dir/${CASE}_oro_data_ss.tile${tile}.nc $out_dir/${CASE}_oro_data_ss.tile${tile}.nc
    fi
    ${NCP} $grid_dir/${CASE}_grid.tile${tile}.nc $out_dir/${CASE}_grid.tile${tile}.nc
	tile=$(($tile + 1))
  done
fi

if [ $gtype = regional -a $nest_grids -gt 1 ]; then
  ${NCP} -p $out_dir/${CASE}_all_mosaic.nc $out_dir/${CASE}_mosaic.nc
  echo "Copy grid and orography files to output directory"
  tile=8
  ntiles=$((${nest_grids} + 6))
  while [ $tile -le $ntiles ]; do
    ${NCP} $filter_dir/oro.${CASE}.tile${tile}.nc $out_dir/${CASE}_oro_data.tile${tile}.nc
    if [ ${use_orog_gsl:-no} = yes ]; then
      ${NCP} $orog_dir/${CASE}_oro_data_ls.tile${tile}.nc $out_dir/${CASE}_oro_data_ls.tile${tile}.nc
      ${NCP} $orog_dir/${CASE}_oro_data_ss.tile${tile}.nc $out_dir/${CASE}_oro_data_ss.tile${tile}.nc
    fi
    ${NCP} $grid_dir/${CASE}_grid.tile${tile}.nc $out_dir/${CASE}_grid.tile${tile}.nc
	tile=$(($tile + 1))
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
  ${NLN} $out_dir/${CASE}_grid.tile${tile}.halo${HALO}.nc $out_dir/${CASE}_grid.tile${tile}.nc
  ${NLN} $out_dir/${CASE}_oro_data.tile${tile}.halo${HALO}.nc $out_dir/${CASE}_oro_data.tile${tile}.nc
  if [ $nest_grids -gt 1 ];  then
    mosaic_file=${out_dir}/${CASE}_coarse_mosaic.nc
  else
    mosaic_file=${out_dir}/${CASE}_mosaic.nc
  fi
  the_orog_files='"'${CASE}'_oro_data.tile'${tile}'.nc"'
else
  echo "FATAL ERROR: Please specify grid type with 'gtype' as uniform, stretch, nest or regional"
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
input_vegetation_type_file="${input_sfc_climo_dir}/vegetation_type.viirs.igbp.0.05.nc"
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

if [[ ! -e ./hafs_utils_sfc_climo_gen.x ]]; then
  ${NCP} -p $SFCCLIMOEXEC ./hafs_utils_sfc_climo_gen.x
fi
${SOURCE_PREP_STEP}
${APRUNC} ./hafs_utils_sfc_climo_gen.x 2>&1 | tee ./sfc_climo_gen.log
export err=$?; err_chk

if [[ $GRIDTYPE != "regional" ]]; then
  for files in *.nc; do
    if [[ -f $files ]]; then
      mv $files ${sfc_climo_savedir}/${CASE}.${files}
    fi
  done
else
  for files in *.halo.nc; do
    if [[ -f $files ]]; then
      file2=${files%.halo.nc}
      mv $files ${sfc_climo_savedir}/${CASE}.${file2}.halo${HALO}.nc
    fi
  done
  for files in *.nc; do
    if [[ -f $files ]]; then
      file2=${files%.nc}
      mv $files ${sfc_climo_savedir}/${CASE}.${file2}.halo0.nc
    fi
  done
fi  # is regional?

if [ $gtype = regional ]; then
  rm -f $out_dir/${CASE}_grid.tile${tile}.nc
  rm -f $out_dir/${CASE}_oro_data.tile${tile}.nc
fi

#----------------------------------------------------------------
# Run for the global or regional nested tiles
# Second pass for global-nesting or regional-nesting configuration will run the 7+th/8+th tiles
#----------------------------------------------------------------

if [ $gtype = nest -o $nest_grids -gt 1 ]; then

ntiles=$((${nest_grids} + 6))
export GRIDTYPE=nest
HALO=0

if [ $gtype = regional ]; then
  stile=8
else
  stile=7
fi

for itile in $(seq $stile $ntiles); do

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
input_vegetation_type_file="${input_sfc_climo_dir}/vegetation_type.viirs.igbp.0.05.nc"
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

if [[ ! -e ./hafs_utils_sfc_climo_gen.x ]]; then
  ${NCP} -p $SFCCLIMOEXEC ./hafs_utils_sfc_climo_gen.x
fi
${SOURCE_PREP_STEP}
${APRUNC} ./hafs_utils_sfc_climo_gen.x 2>&1 | tee ./sfc_climo_gen_tile${itile}.log
export err=$?; err_chk

for files in *.nc; do
  if [[ -f $files ]]; then
    mv $files ${sfc_climo_savedir}/${CASE}.${files}
  fi
done

done

fi
# End of run for the global or regional nested tiles.
#----------------------------------------------------------------

exit
