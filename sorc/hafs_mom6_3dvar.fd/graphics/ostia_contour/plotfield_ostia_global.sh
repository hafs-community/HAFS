#!/bin/bash

START_YMD=20201025
  END_YMD=20201026

startdate=$(date -I -d "$START_YMD") || exit -1
enddate=$(date -I -d "$END_YMD")     || exit -1

d="$startdate"
YMD=${START_YMD}

while [ "$YMD" -le "$END_YMD" ]; do
echo $YMD

#cat isaias2020latlon.txt |grep $YMD > latlon.txt
#cat dorian2019latlon.txt |grep $YMD > latlon.txt
cat Zeta2020_latlon.txt |grep $YMD > latlon.txt

rm ostia.nc
ln -s /work/noaa/ng-godas/marineda/validation/OSTIA/${YMD}/${YMD}120000-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB-v02.0-fv02.0.nc ostia.nc

./plotfield_ostia_global -f ./ostia.nc -s horizontal -y plot.yaml -d ${YMD}12

d=$(date -I -d "$d + 1 day")
YMD=$(date -d "$d" +%Y%m%d)

done  # day loop

mkdir PNG
mv *.png PNG/.

