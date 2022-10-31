#!/bin/bash

NCPATH=/work/noaa/marine/yli/RTOFS_OBS/NC

START_YMDH=20220921Z06
END_YMDH=2022092600
DH=6

date_YMDH=$(date -ud "$START_YMDH")
echo $date_YMDH

################ SST ####################

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )

platlist=(amsr goes npp)
#platlist=(jpss metop)

while [ "$YMDH" -le "$END_YMDH" ]; do

echo $YMDH

for plat in ${platlist[@]}; do

mkdir -p DATA/${plat}/${YMDH:0:4}/${YMDH:0:8}

cd DATA/${plat}/${YMDH:0:4}/${YMDH:0:8}
ln -sf ${NCPATH}/sst_${plat}_${YMDH}.nc sst_${plat}_${YMDH}.nc
cd -

done # platforms

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )
DH=$(($DH+6))

done # time loop

################ adt ####################

DH=6

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )

platlist=(ssh)

while [ "$YMDH" -le "$END_YMDH" ]; do

echo $YMDH

for plat in ${platlist[@]}; do

mkdir -p DATA/${plat}/${YMDH:0:4}/${YMDH:0:8}

cd DATA/${plat}/${YMDH:0:4}/${YMDH:0:8}
ln -sf ${NCPATH}/adt_${plat}_${YMDH}.nc adt_${plat}_${YMDH}.nc
cd -

done # platforms

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )
DH=$(($DH+6))

done # time loop

################ insitu ####################

DH=6

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )

platlist=(profile)

while [ "$YMDH" -le "$END_YMDH" ]; do

echo $YMDH

for plat in ${platlist[@]}; do

mkdir -p DATA/${plat}/${YMDH:0:4}/${YMDH:0:8}

cd DATA/${plat}/${YMDH:0:4}/${YMDH:0:8}
ln -sf ${NCPATH}/insitu_${plat}_${YMDH}.nc insitu_${plat}_${YMDH}.nc
cd -

done # platforms

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )
DH=$(($DH+6))

done # time loop

