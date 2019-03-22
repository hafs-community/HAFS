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
# install forecast
#------------------------------------
${CP} hafs_forecast.fd/NEMS/exe/NEMS.x ../exec/hafs_forecast.x

#------------------------------------
# install gsi
#------------------------------------
${CP} hafs_gsi.fd/exec/global_gsi.x ../exec/hafs_gsi.x
${CP} hafs_gsi.fd/exec/global_enkf.x ../exec/hafs_gsi.x

#------------------------------------
# install post
#------------------------------------
${CP} hafs_post.fd/exec/ncep_post ../exec/hafs_post.x

#------------------------------------
# install chgres
#------------------------------------
${CP} ../fv3gfs/exec/global_chgres ../exec/hafs_chgres.x

#------------------------------------
# install orog
#------------------------------------
${CP} ../fv3gfs/exec/orog.x ../exec/hafs_orog.x

#------------------------------------
# install fre-nctools
#------------------------------------
${CP} ../fv3gfs/exec/make_hgrid                  ../exec/hafs_make_hgrid.x
${CP} ../fv3gfs/exec/make_hgrid_parallel         ../exec/hafs_make_hgrid_parallel.x
${CP} ../fv3gfs/exec/make_solo_mosaic            ../exec/hafs_make_solo_mosaic.x
${CP} ../fv3gfs/exec/fregrid                     ../exec/hafs_fregrid.x
${CP} ../fv3gfs/exec/fregrid_parallel            ../exec/hafs_fregrid_parallel.x
${CP} ../fv3gfs/exec/filter_topo                 ../exec/hafs_filter_topo.x
${CP} ../fv3gfs/exec/shave.x                     ../exec/hafs_shave.x

echo;echo " .... Install system finished .... "

exit 0
