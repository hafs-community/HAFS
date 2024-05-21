#!/bin/sh
set -xeu

cwd=$(pwd)

CP='/bin/cp -rp'

#------------------------------------
# INCLUDE PARTIAL BUILD
#------------------------------------

. ./partial_build.sh.inc

#------------------------------------
# install forecast
#------------------------------------
$Build_forecast && {
  ${CP} hafs_forecast.fd/tests/fv3_hafs_mom6.exe                     ../exec/hafs_forecast_mom6.x
  ${CP} hafs_forecast.fd/tests/fv3_hafs_hycom.exe                    ../exec/hafs_forecast_hycom.x
}

#------------------------------------
# install utils
#------------------------------------
$Build_utils && {
  ${CP} hafs_utils.fd/exec/chgres_cube                               ../exec/hafs_utils_chgres_cube.x
  ${CP} hafs_utils.fd/exec/orog                                      ../exec/hafs_utils_orog.x
  ${CP} hafs_utils.fd/exec/orog_gsl                                  ../exec/hafs_utils_orog_gsl.x
  ${CP} hafs_utils.fd/exec/sfc_climo_gen                             ../exec/hafs_utils_sfc_climo_gen.x
  ${CP} hafs_utils.fd/exec/global_equiv_resol                        ../exec/hafs_utils_global_equiv_resol.x
  ${CP} hafs_utils.fd/exec/regional_esg_grid                         ../exec/hafs_utils_regional_esg_grid.x
  ${CP} hafs_utils.fd/exec/make_hgrid                                ../exec/hafs_utils_make_hgrid.x
  ${CP} hafs_utils.fd/exec/make_solo_mosaic                          ../exec/hafs_utils_make_solo_mosaic.x
  ${CP} hafs_utils.fd/exec/fregrid                                   ../exec/hafs_utils_fregrid.x
  ${CP} hafs_utils.fd/exec/filter_topo                               ../exec/hafs_utils_filter_topo.x
  ${CP} hafs_utils.fd/exec/shave                                     ../exec/hafs_utils_shave.x
}

#------------------------------------
# install post
#------------------------------------
$Build_post && {
  ${CP} hafs_post.fd/exec/upp.x                                      ../exec/hafs_post.x
}

#------------------------------------
# install tracker
#------------------------------------
$Build_tracker && {
  ${CP} hafs_tracker.fd/exec/gettrk.x                                ../exec/hafs_tracker_gettrk.x
  ${CP} hafs_tracker.fd/exec/tave.x                                  ../exec/hafs_tracker_tave.x
  ${CP} hafs_tracker.fd/exec/vint.x                                  ../exec/hafs_tracker_vint.x
  ${CP} hafs_tracker.fd/exec/supvit.x                                ../exec/hafs_tracker_supvit.x
}

#------------------------------------
# install tools
#------------------------------------
$Build_tools && {
  ${CP} hafs_tools.fd/exec/hafs_nhc_products.x                       ../exec/hafs_tools_nhc_products.x
  ${CP} hafs_tools.fd/exec/hafs_obs_preproc.x                        ../exec/hafs_tools_obs_preproc.x
  ${CP} hafs_tools.fd/exec/mpiserial.x                               ../exec/hafs_tools_mpiserial.x
  ${CP} hafs_tools.fd/exec/hafs_change_prepbufr_qm_typ.x             ../exec/hafs_tools_change_prepbufr_qm_typ.x
  ${CP} hafs_tools.fd/exec/hafs_change_prepbufr_qm_in_circle.x       ../exec/hafs_tools_change_prepbufr_qm_in_circle.x
  ${CP} hafs_tools.fd/exec/hafs_change_prepbufr_rm_typ_in_circle.x   ../exec/hafs_tools_change_prepbufr_rm_typ_in_circle.x
  ${CP} hafs_tools.fd/exec/hafs_datool.x                             ../exec/hafs_tools_datool.x
  ${CP} hafs_tools.fd/exec/hafs_vi_create_trak_guess.x               ../exec/hafs_tools_vi_create_trak_guess.x
  ${CP} hafs_tools.fd/exec/hafs_vi_create_trak_init.x                ../exec/hafs_tools_vi_create_trak_init.x
  ${CP} hafs_tools.fd/exec/hafs_vi_split.x                           ../exec/hafs_tools_vi_split.x
  ${CP} hafs_tools.fd/exec/hafs_vi_anl_pert.x                        ../exec/hafs_tools_vi_anl_pert.x
  ${CP} hafs_tools.fd/exec/hafs_vi_anl_combine.x                     ../exec/hafs_tools_vi_anl_combine.x
  ${CP} hafs_tools.fd/exec/hafs_vi_anl_enhance.x                     ../exec/hafs_tools_vi_anl_enhance.x
  ${CP} hafs_tools.fd/exec/hafs_vi_anl_bogus.x                       ../exec/hafs_tools_vi_anl_bogus.x
}

#------------------------------------
# install gsi
#------------------------------------
$Build_gsi && {
  ${CP} hafs_gsi.fd/install/bin/gsi.x                                ../exec/hafs_gsi.x
  ${CP} hafs_gsi.fd/install/bin/enkf.x                               ../exec/hafs_gsi_enkf.x
}

#------------------------------------
# install hycom_utils
#------------------------------------
$Build_hycom_utils && {
  ${CP} hafs_hycom_utils.fd/exec/hafs_get_rtofs                      ../exec/hafs_hycom_utils_get_rtofs.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv2data3z                   ../exec/hafs_hycom_utils_archv2data3z.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv2data2d                   ../exec/hafs_hycom_utils_archv2data2d.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv3z2nc                     ../exec/hafs_hycom_utils_archv3z2nc.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_gfs2ofs2                       ../exec/hafs_hycom_utils_gfs2ofs2.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_rtofs_subregion                ../exec/hafs_hycom_utils_rtofs_subregion.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv2restart                  ../exec/hafs_hycom_utils_archv2restart.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_restart2restart                ../exec/hafs_hycom_utils_restart2restart.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_timeinterp_forcing             ../exec/hafs_hycom_utils_timeinterp_forcing.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv2ncdf3z                   ../exec/hafs_hycom_utils_archv2ncdf3z.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv2ncdf2d                   ../exec/hafs_hycom_utils_archv2ncdf2d.x
}

#------------------------------------
# install ww3_utils
#------------------------------------
$Build_ww3_utils && {
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_grid                     ../exec/hafs_ww3_grid.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_strt                     ../exec/hafs_ww3_strt.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_prep                     ../exec/hafs_ww3_prep.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_outf                     ../exec/hafs_ww3_outf.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_outp                     ../exec/hafs_ww3_outp.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_grib                     ../exec/hafs_ww3_grib.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_gint                     ../exec/hafs_ww3_gint.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_bound                    ../exec/hafs_ww3_bound.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_prnc                     ../exec/hafs_ww3_prnc.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_ounf                     ../exec/hafs_ww3_ounf.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_ounp                     ../exec/hafs_ww3_ounp.x
}


echo;echo " .... Install system finished .... "

exit
