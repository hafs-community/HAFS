#!/bin/bash

module load hpss

YMDpath=20201102
#           /NCEPDEV/emc-ocean/5year/emc.ncodapa/nco_parallel
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/nco_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/profile/*.profile
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/nco_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/ssh/*.ssh
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/nco_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/metop/*.metop
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/nco_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/viirs/*.jpss
exit

YMDpath=20200822
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/profile/*.profile
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/ssh/*.ssh
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/metop/*.metop
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/viirs/*.jpss

exit

START_YMDH=20200815Z00
END_YMDH=2020082000
DH=24
mkdir data

date_YMDH=$(date -ud "$START_YMDH")
echo $date_YMDH

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )

while [ "$YMDH" -le "$END_YMDH" ]; do

echo ${YMDpath} ${YMDH}

#htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/profile/${YMDH}.profile
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/nco_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/profile/${YMDH}.profile

cd data
ln -s ../ocnqc/profile/${YMDH}.profile .
cd -

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )
DH=$(($DH+24))
exit
done

exit

htar -tv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/nco_parallel/ncoda.20201028/ocnqc.tar > ncoda.20210812.table
htar -tv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel3/ncoda.20210812/ocnqc.tar > ncoda.20210812.table
htar -tv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20201024/ocnqc.tar > ncoda.20201024.table

htar -tv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20200821/ocnqc.tar > ncoda.20200821.table

cd SCP

scp -r 202010* yongzuo@Orion-login-1.HPC.MsState.Edu:/work/noaa/ng-godas/yli/plot112/rtofs_profile/data

scp -r 20210* yongzuo@Orion-login-1.HPC.MsState.Edu:/work/noaa/marine/yli/soca-shared/DATA/obs/RTOFSobs4MOM6_24h/rtofs_profile/data

htar -tv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/rtofs.20200808/rtofs.ncoda.tar > rtofs.20200808.table
scp -r 20200* yongzuo@Orion-login-3.HPC.MsState.Edu:/work/noaa/ng-godas/yli/plot112/rtofs_profile/data

module load hpss
