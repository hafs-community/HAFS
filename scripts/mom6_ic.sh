#!/bin/sh
# prepare mom6 ic
#
# scripts: /work/noaa/hwrf/save/maristiz/scripts_to_prep_MOM6/RTOFS_IC/get_netcdf_rtofs_HAT10_IC.sh
# input files: ${HOMErtofs}/rtofs.$Year$Month$Day/rtofs_glo.t00z.${type}${Hour}.archv.a.tgz
# output files: /work/noaa/hwrf/save/maristiz/scripts_to_prep_MOM6/RTOFS_IC/2020082512/rtofs_HAT10_20200825_f12_TS_ic.nc
# output files: /work/noaa/hwrf/save/maristiz/scripts_to_prep_MOM6/RTOFS_IC/2020082512/rtofs_HAT10_20200825_f12_SSH_ic.nc
# output files: /work/noaa/hwrf/save/maristiz/scripts_to_prep_MOM6/RTOFS_IC/2020082512/rtofs_HAT10_20200825_f12_UV_ic.nc
# 
cp /scratch1/NCEPDEV/hwrf/noscrub/Bin.Li/hafs-ic-bc/rtofs_HAT10_20200825_f12_TS_ic.nc .
cp /scratch1/NCEPDEV/hwrf/noscrub/Bin.Li/hafs-ic-bc/rtofs_HAT10_20200825_f12_SSH_ic.nc .

exit
