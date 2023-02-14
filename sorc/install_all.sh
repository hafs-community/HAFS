#!/bin/sh
set -xeu

cwd=$(pwd)

CP='/bin/cp -rp'

#------------------------------------
# INCLUDE PARTIAL BUILD
#------------------------------------

. ./partial_build.sh

#------------------------------------
# install forecast
#------------------------------------
$Build_forecast && {
  ${CP} hafs_forecast.fd/tests/fv3_32bit.exe         ../exec/hafs_forecast.x
}

#------------------------------------
# install utils
#------------------------------------
$Build_utils && {
  ${CP} hafs_utils.fd/exec/chgres_cube               ../exec/hafs_chgres_cube.x
  ${CP} hafs_utils.fd/exec/orog                      ../exec/hafs_orog.x
  ${CP} hafs_utils.fd/exec/orog_gsl                  ../exec/hafs_orog_gsl.x
  ${CP} hafs_utils.fd/exec/sfc_climo_gen             ../exec/hafs_sfc_climo_gen.x
  ${CP} hafs_utils.fd/exec/global_equiv_resol        ../exec/hafs_global_equiv_resol.x
  ${CP} hafs_utils.fd/exec/regional_esg_grid         ../exec/hafs_regional_esg_grid.x
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
  ${CP} hafs_post.fd/exec/upp.x                      ../exec/hafs_post.x
}

#------------------------------------
# install tracker
#------------------------------------
$Build_tracker && {
  ${CP} hafs_tracker.fd/exec/gettrk.x          ../exec/hafs_gettrk.x
  ${CP} hafs_tracker.fd/exec/tave.x            ../exec/hafs_tave.x
  ${CP} hafs_tracker.fd/exec/vint.x            ../exec/hafs_vint.x
  ${CP} hafs_tracker.fd/exec/supvit.x          ../exec/hafs_supvit.x
}

#------------------------------------
# install tools
#------------------------------------
$Build_tools && {
  ${CP} hafs_tools.fd/exec/hafs_nhc_products.x                     ../exec/hafs_nhc_products.x
  ${CP} hafs_tools.fd/exec/hafs_analysis_update.x                  ../exec/hafs_analysis_update.x
  ${CP} hafs_tools.fd/exec/hafs_obs_preproc.x                      ../exec/hafs_obs_preproc.x
  ${CP} hafs_tools.fd/exec/mpiserial.x                             ../exec/hafs_mpiserial.x
  ${CP} hafs_tools.fd/exec/hafs_change_prepbufr_qm_typ.x           ../exec/hafs_change_prepbufr_qm_typ.x
  ${CP} hafs_tools.fd/exec/hafs_change_prepbufr_qm_in_circle.x     ../exec/hafs_change_prepbufr_qm_in_circle.x
  ${CP} hafs_tools.fd/exec/hafs_change_prepbufr_rm_typ_in_circle.x ../exec/hafs_change_prepbufr_rm_typ_in_circle.x
  ${CP} hafs_tools.fd/exec/hafs_datool.x                           ../exec/hafs_datool.x
  ${CP} hafs_tools.fd/exec/hafs_vi_create_trak_guess.x             ../exec/hafs_vi_create_trak_guess.x
  ${CP} hafs_tools.fd/exec/hafs_vi_create_trak_init.x              ../exec/hafs_vi_create_trak_init.x
  ${CP} hafs_tools.fd/exec/hafs_vi_split.x                         ../exec/hafs_vi_split.x
  ${CP} hafs_tools.fd/exec/hafs_vi_anl_pert.x                      ../exec/hafs_vi_anl_pert.x
  ${CP} hafs_tools.fd/exec/hafs_vi_anl_combine.x                   ../exec/hafs_vi_anl_combine.x
  ${CP} hafs_tools.fd/exec/hafs_vi_anl_enhance.x                   ../exec/hafs_vi_anl_enhance.x
  ${CP} hafs_tools.fd/exec/hafs_vi_anl_bogus.x                     ../exec/hafs_vi_anl_bogus.x
}

#------------------------------------
# install gsi
#------------------------------------
$Build_gsi && {
  ${CP} hafs_gsi.fd/install/bin/gsi.x                              ../exec/hafs_gsi.x
  ${CP} hafs_gsi.fd/install/bin/enkf.x                             ../exec/hafs_enkf.x
}

#------------------------------------
# install hycom_utils
#------------------------------------
$Build_hycom_utils && {
  ${CP} hafs_hycom_utils.fd/exec/hafs_get_rtofs                 ../exec/hafs_get_rtofs.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv2data3z              ../exec/hafs_archv2data3z.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv2data2d              ../exec/hafs_archv2data2d.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv3z2nc                ../exec/hafs_archv3z2nc.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_gfs2ofs2                  ../exec/hafs_gfs2ofs2.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_rtofs_subregion           ../exec/hafs_rtofs_subregion.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_archv2restart             ../exec/hafs_archv2restart.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_restart2restart           ../exec/hafs_restart2restart.x
  ${CP} hafs_hycom_utils.fd/exec/hafs_timeinterp_forcing        ../exec/hafs_timeinterp_forcing.x
}

#------------------------------------
# install ww3_utils
#------------------------------------
$Build_ww3_utils && {
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_grid                ../exec/hafs_ww3_grid.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_strt                ../exec/hafs_ww3_strt.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_prep                ../exec/hafs_ww3_prep.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_outf                ../exec/hafs_ww3_outf.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_outp                ../exec/hafs_ww3_outp.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_trck                ../exec/hafs_ww3_trck.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_grib                ../exec/hafs_ww3_grib.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_gspl                ../exec/hafs_ww3_gspl.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_gint                ../exec/hafs_ww3_gint.x
# ${CP} hafs_forecast.fd/WW3/model/exec/gx_outf                 ../exec/hafs_gx_outf.x
# ${CP} hafs_forecast.fd/WW3/model/exec/gx_outp                 ../exec/hafs_gx_outp.x
# ${CP} hafs_forecast.fd/WW3/model/exec/ww3_systrk              ../exec/hafs_ww3_systrk.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_bound               ../exec/hafs_ww3_bound.x
# ${CP} hafs_forecast.fd/WW3/model/exec/ww3_shel                ../exec/hafs_ww3_shel.x
# ${CP} hafs_forecast.fd/WW3/model/exec/ww3_multi               ../exec/hafs_ww3_multi.x
# ${CP} hafs_forecast.fd/WW3/model/exec/ww3_sbs1                ../exec/hafs_ww3_sbs1.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_prnc                ../exec/hafs_ww3_prnc.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_ounf                ../exec/hafs_ww3_ounf.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_ounp                ../exec/hafs_ww3_ounp.x
  ${CP} hafs_forecast.fd/WW3/model/exec/ww3_bounc               ../exec/hafs_ww3_bounc.x
}


echo;echo " .... Install system finished .... "

exit
