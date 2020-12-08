#!/bin/sh
set -xeu

build_dir=`pwd`

CP='cp -rp'

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build.sh

#------------------------------------
# install forecast
#------------------------------------
$Build_forecast && {
  ${CP} hafs_forecast.fd/NEMS/exe/NEMS.x             ../exec/hafs_forecast.x
}

#------------------------------------
# install utils
#------------------------------------
$Build_utils && {
  ${CP} hafs_utils.fd/exec/global_chgres             ../exec/hafs_chgres.x
  ${CP} hafs_utils.fd/exec/chgres_cube               ../exec/hafs_chgres_cube.x
  ${CP} hafs_utils.fd/exec/orog                      ../exec/hafs_orog.x
  ${CP} hafs_utils.fd/exec/sfc_climo_gen             ../exec/hafs_sfc_climo_gen.x
# ${CP} hafs_utils.fd/exec/regional_grid             ../exec/hafs_regional_grid.x
  ${CP} hafs_utils.fd/exec/make_hgrid                ../exec/hafs_make_hgrid.x
  ${CP} hafs_utils.fd/exec/make_solo_mosaic          ../exec/hafs_make_solo_mosaic.x
  ${CP} hafs_utils.fd/exec/fregrid                   ../exec/hafs_fregrid.x
  ${CP} hafs_utils.fd/exec/filter_topo               ../exec/hafs_filter_topo.x
  ${CP} hafs_utils.fd/exec/shave                     ../exec/hafs_shave.x
}

#------------------------------------
# install post
#------------------------------------
$Build_post && {
  ${CP} hafs_post.fd/exec/ncep_post                  ../exec/hafs_post.x
}

#------------------------------------
# install vortextracker
#------------------------------------
$Build_vortextracker && {
  ${CP} hafs_vortextracker.fd/exec/gettrk.x          ../exec/hafs_gettrk.x
  ${CP} hafs_vortextracker.fd/exec/tave.x            ../exec/hafs_tave.x
  ${CP} hafs_vortextracker.fd/exec/vint.x            ../exec/hafs_vint.x
  ${CP} hafs_vortextracker.fd/exec/supvit.x          ../exec/hafs_supvit.x
}

#------------------------------------
# install tools
#------------------------------------
$Build_tools && {
  ${CP} hafs_tools.fd/exec/tempdrop_sonde.x          ../exec/hafs_tempdrop_sonde.x
  ${CP} hafs_tools.fd/exec/obs_to_bufr.x             ../exec/hafs_obs_to_bufr.x
}

#------------------------------------
# install gsi
#------------------------------------
$Build_gsi && {
  ${CP} hafs_gsi.fd/exec/global_gsi.x                ../exec/hafs_gsi.x
  ${CP} hafs_gsi.fd/exec/global_enkf.x               ../exec/hafs_enkf.x
  ${CP} hafs_gsi.fd/exec/adderrspec.x                ../exec/hafs_adderrspec.x
  ${CP} hafs_gsi.fd/exec/adjustps.x                  ../exec/hafs_adjustps.x
  ${CP} hafs_gsi.fd/exec/calc_increment_ens.x        ../exec/hafs_calc_increment_ens.x
  ${CP} hafs_gsi.fd/exec/calc_increment_serial.x     ../exec/hafs_calc_increment_serial.x
  ${CP} hafs_gsi.fd/exec/getnstensmeanp.x            ../exec/hafs_getnstensmeanp.x
  ${CP} hafs_gsi.fd/exec/getsfcensmeanp.x            ../exec/hafs_getsfcensmeanp.x
  ${CP} hafs_gsi.fd/exec/getsfcnstensupdp.x          ../exec/hafs_getsfcnstensupdp.x
  ${CP} hafs_gsi.fd/exec/getsigensmeanp_smooth.x     ../exec/hafs_getsigensmeanp_smooth.x
  ${CP} hafs_gsi.fd/exec/getsigensstatp.x            ../exec/hafs_getsigensstatp.x
  ${CP} hafs_gsi.fd/exec/gribmean.x                  ../exec/hafs_gribmean.x
  ${CP} hafs_gsi.fd/exec/nc_diag_cat.x               ../exec/hafs_nc_diag_cat.x
  ${CP} hafs_gsi.fd/exec/nc_diag_cat_serial.x        ../exec/hafs_nc_diag_cat_serial.x
  ${CP} hafs_gsi.fd/exec/oznmon_horiz.x              ../exec/hafs_oznmon_horiz.x
  ${CP} hafs_gsi.fd/exec/oznmon_time.x               ../exec/hafs_oznmon_time.x
  ${CP} hafs_gsi.fd/exec/radmon_angle.x              ../exec/hafs_radmon_angle.x
  ${CP} hafs_gsi.fd/exec/radmon_bcoef.x              ../exec/hafs_radmon_bcoef.x
  ${CP} hafs_gsi.fd/exec/radmon_bcor.x               ../exec/hafs_radmon_bcor.x
  ${CP} hafs_gsi.fd/exec/radmon_time.x               ../exec/hafs_radmon_time.x
  ${CP} hafs_gsi.fd/exec/recenternemsiop_hybgain.x   ../exec/hafs_recenternemsiop_hybgain.x
  ${CP} hafs_gsi.fd/exec/recentersigp.x              ../exec/hafs_recentersigp.x
  ${CP} hafs_gsi.fd/exec/test_nc_unlimdims.x         ../exec/hafs_test_nc_unlimdims.x
}


echo;echo " .... Install system finished .... "

exit 0
