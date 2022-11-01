#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
# prep.bkg.coldstart.sh
#   Generate initial conditions from the closest date found from CFSv2 files.
#   Download and prep the file, and run a very short spinup run.
#================================================================================

EOF

# TODO, for older dates use the CFSR  url instead!

# default environment varaibles
MAX_SEARCH_DAYS=${MAX_SEARCH_DAYS:-10}
SRC_URL=${SRC_URL:-https://www.ncei.noaa.gov/data/climate-forecast-system/access\
/operational-analysis/6-hourly-ocean/%Y/%Y%m/%Y%m%d/cdas1.t12z.ocngrbh01.grib2}

# Required environment variables:
envars+=("BKGRST_DIR")
envars+=("DATE")
envars+=("MAX_SEARCH_DAYS")
envars+=("SRC_URL")
envars+=("FORC_DIR")
envars+=("WORK_DIR")

# make sure required env vars exist
set +u
for v in ${envars[@]}; do
    if [[ -z "${!v}" ]]; then
        echo "ERROR: env var $v is not set."; exit 1
    fi
    printf "%-25s %s\n" " $v " "${!v}"
done
set -u
echo ""

USE_CFS=0   #USE CFS

if [[ $USE_CFS == 1 ]]; then

# Download the most recent CFSv2 ocean to the date chosen
found=0
offset=0
while [[ $offset -lt "$MAX_SEARCH_DAYS" ]]; do
    # $offset days before $DATE
    ocean_file=$(date -d "$DATE - $offset day" "+$SRC_URL" -u)
    echo "trying $ocean_file..."
    wget -q $ocean_file && found=1
    [[ $found == 1 ]] &&  break

    # $offset days after $DATE
    if [[ $offset != 0 ]]; then
        ocean_file=$(date -d "$DATE + $offset day" "+$SRC_URL" -u)
        echo "trying $ocean_file..."
        wget -q $ocean_file && found=1
        [[ $found == 1 ]] &&  break
    fi

    offset=$((offset+1))
done

if [[ $found == 0 ]]; then
    echo "ERROR: no nearby CFSv2 ocean file found for the given date"
    exit 1
fi


# Convert the file to the correct format needed by MOM                                                                                                                        #--------------------------------------------------------------------------------                                                                                             echo "Converting CFSv2 ocean file to netCDF format..."
# TODO configurable size depending on model fcst resolution and domain??
cat > lonlat <<EOF
gridtype = lonlat
xsize    = 720
ysize    = 360
xfirst   = -179.75
xinc     = 0.5
yfirst   = -89.75
yinc     = 0.5
EOF

cdo \
    -Q -L -f nc4\
    -subc,273.15 \
    -chname,pt,ptemp_an\
    -select,name=pt\
    cdas1.*.grib2 ocean.T.nc.tmp
cdo \
    -Q -invertlev\
    -fillmiss\
    -remapbil,lonlat\
    ocean.T.nc.tmp ocean.T.nc

cdo \
    -Q -L -f nc \
    -mulc,1000 \
    -chname,s,s_an\
    -select,name=s\
    cdas1.*.grib2 ocean.S.nc.tmp
cdo\
    -Q -invertlev\
    -fillmiss\
    -remapbil,lonlat\
    ocean.S.nc.tmp ocean.S.nc

rm *.tmp
fi   # USE CFS

USE_RTOFS=1  ## USE RTOFS
if [[ $USE_RTOFS == 1 ]]; then

export HOMEwork=/work/noaa/marine/yli/RTOFS_IC

$HOMEwork/get_netcdf_rtofs_HAT10_IC.sh
$HOMEwork/get_UV_Cgrids.sh

YMDH=$(date -ud "$DATE" +%Y%m%d%H )
ln -sf $HOMEwork/${YMDH}/rtofs_HAT10_${YMDH:0:8}_f${YMDH:8:2}_TS_ic.nc rtofs_ts_ic.nc
ln -sf $HOMEwork/${YMDH}/rtofs_HAT10_${YMDH:0:8}_f${YMDH:8:2}_SSH_ic.nc rtofs_ssh_ic.nc
ln -sf $HOMEwork/${YMDH}/rtofs_HAT10_${YMDH:0:8}_f${YMDH:8:2}_UV_ic.nc rtofs_uv_ic.nc

fi            # End of USE RTOFS

# download the required forcing
# TODO allow for variable length forecasts
ymdh=$(date -d "$DATE - $FCST_LEN hour" -u +"%Y%m%d%H")
export FORC_DIR=$FORC_DIR/${ymdh:0:8} # eh, this is messy
mkdir -p forc
(
    export DATE=$(date -d "$DATE - $FCST_LEN hour" -u)
    cd forc
    $SUBSCRIPTS_DIR/prep.forc.sh
)

# run a very short forecast to generate restart files
mkdir -p fcst
(
    export MODEL_IC_DIR=$(readlink -f .)
    cd fcst
    export FCST_RESTART=0
    export MODEL_RST_DIR_IN=NONE
    export MODEL_RST_DIR_OUT=$BKGRST_DIR
    export FCST_START_TIME=$(date -d "$DATE - $FCST_LEN hour" -u)
    export FCST_RST_OFST=$FCST_LEN
    $SUBSCRIPTS_DIR/run.fcst.sh
)
