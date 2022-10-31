#!/bin/bash

module load hpss

#YMD=20210101
#YMDpath=20210113
#htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/goes/${YMD}*.goes
#exit

#YMDpath=20210116
#YMDpath=20210128

#htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar
#exit

#YMDpath=20200630
#htar -tv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar
#exit

#htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20200908/ocnqc.tar
#htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20200922/ocnqc.tar
#htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20201006/ocnqc.tar
#htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20201020/ocnqc.tar
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/nco_parallel/ncoda.20201103/ocnqc.tar
exit

## xv every 14 days
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20200630/ocnqc.tar
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20200714/ocnqc.tar
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20200728/ocnqc.tar
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20200811/ocnqc.tar
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.20200825/ocnqc.tar

#YMDpath=20200616
YMDpath=20200630
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/profile/*.profile
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/ssh/*.ssh
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/sss/*.sss
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/amsr/*.amsr
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/goes/*.goes
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/metop/*.metop
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/himawari/*.himawari
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/viirs/*.jpss
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/viirs/*.npp
exit

#YMD=20201231
#YMDpath=20210113
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/profile/${YMD}*.profile
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/ssh/${YMD}*.ssh
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/sss/${YMD}*.sss
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/amsr/${YMD}*.amsr
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/goes/${YMD}*.goes
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/metop/${YMD}*.metop
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/himawari/${YMD}*.himawari
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/viirs/${YMD}*.jpss
htar -xv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.${YMDpath}/ocnqc.tar ocnqc/viirs/${YMD}*.npp
exit

htar -tv -f /NCEPDEV/emc-ocean/5year/emc.ncodapa/emc_parallel/ncoda.20210128/ocnqc.tar > ncoda.20210128.table

exit

scp -r 202010* yongzuo@Orion-login-1.HPC.MsState.Edu:/work/noaa/ng-godas/yli/plot112/rtofs_profile/data
scp yongzuo@Orion-login-1.HPC.MsState.Edu:/work/noaa/ng-godas/yli/plot112/rtofs_sst/

scp -r 20210* yongzuo@Orion-login-1.HPC.MsState.Edu:/work/noaa/marine/yli/soca-shared/DATA/obs/RTOFSobs4MOM6_24h/rtofs_profile/data

module load hpss
