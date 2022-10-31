#!/bin/bash
#

module load nco/4.9.3

#rm RESTART_IN/MOM*
#rm RESTART_IN/rtofs*
#rm RESTART_IN/hycom*
#rm RESTART_IN/tmp*

envars+=("DATE")
YMDH=$(date -ud "$DATE" +%Y%m%d%H )

#YMDH=2022092212

export HOMEwork=/work/noaa/marine/yli/RTOFS_IC # Tools and HAT10 settings
cd ${HOMEwork}/${YMDH}

#export YMDH=${YMDH:-2020082512}
ff=${ff:-rtofs_HAT10_${YMDH:0:8}_f${YMDH:8:2}_UV}

#ncrename -d Depth,Layer -d Latitude,lath -d Longitude,lonh -d MT,Time -v Depth,Layer -v Latitude,lath -v Longitude,lonh -v MT,Time ${ff}_IC.nc ${ff}_Layer.nc

rm ${ff}_Layer.nc
ncrename -d Depth,Layer -d Latitude,lath -d Longitude,lonh -d MT,Time -v Depth,Layer -v Latitude,lath -v Longitude,lonh -v MT,Time ${ff}.nc ${ff}_Layer.nc

cp -f ${ff}_Layer.nc hycom_3d.nc

#cp -f ./MOM.res.nc RESTART_IN/MOM.res.nc

rm MOM.res.nc
#cp /work/noaa/hwrf/save/maristiz/MOM6_examples_NOAA_GFDL/ocean_only_HAT10/RESTART_IN/MOM.res.nc .
cp /work/noaa/marine/yli/RTOFS_MOM6/hycom_netcdf/data_netcdf/hat10_IC/MOM.res.nc .

#cp /work/noaa/hwrf/save/maristiz/scripts_to_prep_MOM6/RTOFS_IC/2020082512/MOM.res.nc .

#cd RESTART_IN
cp MOM.res.nc   MOM.res.nc.obc
#cd ../

ncks -O -F -d latq,2,634 -d lonq,2,1136 MOM.res.nc.obc MOM.res.nc

#exit

ncks -O -F -v u -d lonq,1,1 MOM.res.nc.obc tmp1_u.nc
ncks -O -F -v v -d latq,1,1 MOM.res.nc.obc tmp1_v.nc

#
# to make double precision, multiply *1.0
#
ncap2 -O -s lonh=lonh*1.0 hycom_3d.nc hycom_3d.nc
ncap2 -O -s lath=lath*1.0 hycom_3d.nc hycom_3d.nc
ncap2 -O -s Layer=Layer*1.0 hycom_3d.nc hycom_3d.nc
ncap2 -O -s u=u*1.0 hycom_3d.nc hycom_3d.nc
ncap2 -O -s v=v*1.0 hycom_3d.nc hycom_3d.nc
#
# extract u & v 
#
ncks -O -v u hycom_3d.nc tmp2_u.nc
ncks -O -v v hycom_3d.nc tmp2_v.nc

ncrename -O -d lonh,lonq -v lonh,lonq tmp2_u.nc tmp2_u.nc 
ncrename -O -d lath,latq -v lath,latq tmp2_v.nc tmp2_v.nc 

ncks -A -C -v lonq MOM.res.nc tmp2_u.nc
ncks -A -C -v latq MOM.res.nc tmp2_v.nc

ncpdq -O -a lonq,lath,Layer,Time tmp1_u.nc tmp1_u.nc
ncpdq -O -a lonq,lath,Layer,Time tmp2_u.nc tmp2_u.nc

ncpdq -O -a latq,lonh,Layer,Time tmp1_v.nc tmp1_v.nc
ncpdq -O -a latq,lonh,Layer,Time tmp2_v.nc tmp2_v.nc

ncrcat -O tmp1_u.nc tmp2_u.nc tmp_u.nc
ncrcat -O tmp1_v.nc tmp2_v.nc tmp_v.nc

ncpdq -O -a Time,Layer,lath,lonq tmp_u.nc tmp_u.nc
ncpdq -O -a Time,Layer,latq,lonh tmp_v.nc tmp_v.nc

ncks -A -C -v Time,Layer,lath,lonq,u tmp_u.nc tmp_uv.nc
ncks -A -C -v latq,lonh,v tmp_v.nc tmp_uv.nc

cp tmp_uv.nc test_uv.nc                     
#
# values on the land grids set to be zero
#
ncap2 -s 'where (u > 100.0 ) u=0.0' test_uv.nc test_uv_u0.nc
ncap2 -s 'where (v > 100.0 ) v=0.0' test_uv_u0.nc test_uv_00.nc

mv test_uv_00.nc ${ff}_ic.nc
#

