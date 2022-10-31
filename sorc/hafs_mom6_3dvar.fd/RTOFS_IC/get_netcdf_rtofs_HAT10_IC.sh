#!/bin/sh

##set -x

############################### User input ################################
#export YMDH=${YMDH:-2020082512}
#export YMDH=${YMDH:-2020072012}
#export YMDH=2020071512

envars+=("DATE")
YMDH=$(date -ud "$DATE" +%Y%m%d%H )

export HOMEwork=/work/noaa/marine/yli/RTOFS_IC  # running scripts
export HOMEhat10=/work/noaa/marine/yli/RTOFS_IC # Tools and HAT10 settings                    
export HOMEhafs=/work/noaa/hwrf/save/maristiz/hafs_develop_202112 # global RTOFS depth and grid files
export HOMErtofs=/work/noaa/hwrf/noscrub/hafs-input/COMRTOFSv2    # Daily RTOFS

############################### End user input ################################

cd ${HOMEwork}

# Make experiment folder

if [[ -d ${YMDH} ]]; then
rm -rf ${YMDH}
fi

mkdir ${YMDH} 
cd ${YMDH} 

# Load modules
module purge
export MACHINE=orion.intel
source /work/noaa/hwrf/save/maristiz/HAT10_MOM6_from_HeeSook/expdir_OBC3/soca-science/configs/machine/machine.orion.intel
module load nco/4.9.3

# Link global RTOFS depth and grid files
rm -f regional*
ln -s ${HOMEhafs}/fix/fix_hycom/rtofs_glo.navy_0.08.regional.depth.a regional.depth.a
ln -s ${HOMEhafs}/fix/fix_hycom/rtofs_glo.navy_0.08.regional.depth.b regional.depth.b

ln -s ${HOMEhafs}/fix/fix_hycom/rtofs_glo.navy_0.08.regional.grid.a regional.grid.a
ln -s ${HOMEhafs}/fix/fix_hycom/rtofs_glo.navy_0.08.regional.grid.b regional.grid.b

#exit

Year=${YMDH:0:4}
echo $Year
Month=${YMDH:4:2}
echo $Month
Day=${YMDH:6:2}
echo $Day
Hour=${YMDH:8:2}
echo $Hour

if [ "${Hour}" == "00" ]
then
    type=${type:-n}
    echo ${type}
else
    type=${type:-f}
    echo ${type}
fi

# Link global RTOFS analysis or forecast files
rm -f archv_in.[ab]
ln -sf ${HOMErtofs}/rtofs.$Year$Month$Day/rtofs_glo.t00z.${type}${Hour}.archv.a.tgz
tar -xpvzf rtofs_glo.t00z.${type}${Hour}.archv.a.tgz
ln -sf rtofs_glo.t00z.${type}${Hour}.archv.a archv_in.a
ln -sf ${HOMErtofs}/rtofs.$Year$Month$Day/rtofs_glo.t00z.${type}${Hour}.archv.b archv_in.b

# Define output file names
outnc_UV=${outnc_UV:-rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_UV.nc}
outnc_TS=${outnc_TS:-rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS.nc}
outnc_2d=${outnc_2d:-rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH.nc}

export CDF033=./${outnc_UV}
export CDF034=./${outnc_TS}
export CDF038=./${outnc_2d}

rm $CDF033 $CDF034 $CDF038

# run HYCOM-tools executables to produce IC netcdf files
${HOMEhat10}/archv2ncdf3z < ${HOMEhat10}/ncdf3z_rtofs_3d_hat10_IC.in

${HOMEhat10}/archv2ncdf2d < ${HOMEhat10}/ncdf3z_rtofs_SSH_hat10_IC.in

# SSH file
# Rename variables so they match MOM6 variable name
ncrename -d Latitude,lath -d Longitude,lonh -d MT,time -v Latitude,lath -v Longitude,lonh -v MT,time -v ssh,ave_ssh rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH.nc  rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH_mom6names.nc

# Convert variable to double precission?
ncap2 -O -s ave_ssh=ave_ssh*1.0 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH_mom6names.nc rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH_ic.nc

# _Fillvalues set to zero
ncatted -a _FillValue,ave_ssh,o,f,0.0 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH_ic.nc

#ncks -3 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH.nc SSH_classic.nc
#ncrename -d Latitude,lath -d Longitude,lonh -d MT,time -v Latitude,lath -v Longitude,lonh -v MT,time -v ssh,ave_ssh SSH_classic.nc rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH_mom6names.nc
#ncks -4 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH_mom6names.nc rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH_ic.nc

# TS file
# Change format to netcdf3 format
ncks -3 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS.nc TS_classic.nc

# Rename variables so they match MOM6 variable name
ncrename -d Depth,depth -d Latitude,lath -d Longitude,lonh -d MT,time -v Depth,depth -v Latitude,lath -v Longitude,lonh -v MT,time -v pot_temp,Temp -v salinity,Salt TS_classic.nc rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS_mom6names.nc

# Change format to netcdf4 format
ncks -4 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS_mom6names.nc rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS_ic.nc

# _Fillvalues set to zero
#
#ncatted -a _FillValue,ave_ssh,d,f,0.0 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH.nc
#ncatted -a _FillValue,ave_ssh,o,f,0.0 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH_ic.nc
#ncap2 -s 'where (ave_ssh > 100.0 ) ave_ssh=0.0' rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_SSH.nc

#ncatted -a _FillValue,Temp,o,f,0.0 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS_ic.nc
#ncap2 -s 'where (Temp > 100.0 ) Temp=0.0' rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS.nc 

#ncatted -a _FillValue,Salt,o,f,0.0 rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS_ic.nc
#ncap2 -s 'where (Salt > 100.0 ) Salt=0.0' rtofs_HAT10_${Year}${Month}${Day}_${type}${Hour}_TS.nc 

