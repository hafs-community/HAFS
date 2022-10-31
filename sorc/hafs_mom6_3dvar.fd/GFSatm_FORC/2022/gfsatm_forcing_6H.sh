#!/bin/bash

START_YMDH=2022092300
  END_YMDH=2022092318
DH=6

tmp_YMDH=${START_YMDH:0:8}Z${START_YMDH:8:2}
date_YMDH=$(date -ud "$tmp_YMDH")

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )

while [ "$YMDH" -le "$END_YMDH" ]; do

echo YMDH=$YMDH

python3 gfsatm_forcing_6H.py ${YMDH:0:8}${YMDH:8:2} -o atm_${YMDH}.nc -t ./${YMDH:0:8}

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )
DH=$(($DH+6))

done    # day loop

exit

