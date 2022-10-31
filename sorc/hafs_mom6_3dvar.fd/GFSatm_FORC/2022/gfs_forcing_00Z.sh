#!/bin/bash

START_YMD=20220921
  END_YMD=20220926

startdate=$(date -I -d "$START_YMD") || exit -1
enddate=$(date -I -d "$END_YMD")     || exit -1

d="$startdate"
YMD=${START_YMD}

while [ "$YMD" -le "$END_YMD" ]; do
echo YMD=$YMD

python gfs_forcing_00Z.py ${YMD} -o atm_${YMD}.nc -t ../00Z/${YMD}

d=$(date -I -d "$d + 1 day")
YMD=$(date -d "$d" +%Y%m%d)

done    # day loop

#exit

mv ../00Z/* .

