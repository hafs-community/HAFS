#!/bin/bash

export HOMEwork=/scratch2/NCEPDEV/marine/Yongzuo.Li/RTOFS2IODA_24H

START_YMDH=2020060112
  END_YMDH=2020060112

TMP_YMDH=${START_YMDH:0:8}Z${START_YMDH:8:2}
date_YMDH=$(date -ud "$TMP_YMDH")
DH=24

SKIP=0
if [[ $SKIP == 0 ]]; then
######################### convert RTOFS SST into IODA v2 ############

platlist=(amsr goes himawari metop npp jpss)

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )

winstart=$(date -ud "$date_YMDH - 12 hours" +%Y%m%d%H )
winend=$(date -ud "$date_YMDH + 12 hours" +%Y%m%d%H )

while [ "$YMDH" -le "$END_YMDH" ]; do

echo $winstart $YMDH $winend

rm window.txt
echo ${winstart}00 > window.txt
echo ${winend}00 >> window.txt

rm sst.bin sst.txt

for plat in ${platlist[@]}; do

platx=${plat}
if [[ ${plat} == "jpss" ]] || [[ ${plat} == "npp" ]]; then
platx="viirs"
fi
echo ${platx}

ocnqcpath=${HOMEwork}/ocnqc/${platx}

rm sst.bin sst.txt sst_all.txt
if [[ ${plat} == "jpss" ]] || [[ ${plat} == "npp" ]] || [[ ${plat} == "metop" ]]; then
nn=-15
fi

if [[ ${plat} == "amsr" ]] || [[ ${plat} == "goes" ]] || [[ ${plat} == "himawari" ]]; then
nn=-18
fi

while [ "${nn}" -le "12" ]; do

MM=$(($DH-24+${nn}))
YMDHmXh=$(date -ud "$date_YMDH + ${MM} hours" +%Y%m%d%H )

if [[ -f ${ocnqcpath}/${YMDHmXh}.${plat} ]]; then

 ls -l ${ocnqcpath}/${YMDHmXh}.${plat}
ln -sf ${ocnqcpath}/${YMDHmXh}.${plat} sst.bin

${HOMEwork}/rtofs_obs_read read_sst

if [[ -f sst.txt ]]; then
cat sst.txt >> sst_all.txt
rm sst.txt
fi

fi

if [[ ${plat} == "jpss" ]] || [[ ${plat} == "npp" ]] || [[ ${plat} == "metop" ]]; then
nn=$((${nn}+3))
fi

if [[ ${plat} == "amsr" ]] || [[ ${plat} == "goes" ]] || [[ ${plat} == "himawari" ]]; then
nn=$((${nn}+6))
fi

done # nn every 3 or 6 hrs loop

mv sst_all.txt sst.txt
echo start NETCDF

${HOMEwork}/rtofs_ascii2iodav2.py -i sst.txt -v sea_surface_temperature -o ./sst_${plat}_${YMDH:0:8}.nc

mv sst.txt ./data/sst.txt_${plat}

done # plat loop

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )

DHm12h=$(($DH-12))
winstart=$(date -ud "$date_YMDH + $DHm12h hours" +%Y%m%d%H )
DHp12h=$(($DH+12))
winend=$(date -ud "$date_YMDH + $DHp12h hours" +%Y%m%d%H )

DH=$(($DH+24))

done # day time loop

if [[ ! -d NC ]]; then
mkdir NC
fi
mv sst_*.nc NC/.

#exit

######################### convert RTOFS SSH into IODA v2 ############

DH=24
platlist=(ssh)
ocnqcpath=${HOMEwork}/ocnqc/ssh

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )
today=${YMDH:0:8}00

YMDHm1d=$(date -ud "$date_YMDH - 19 hours" +%Y%m%d%H )
yesterday=${YMDHm1d:0:8}00

winstart=$(date -ud "$date_YMDH - 12 hours" +%Y%m%d%H )
winend=$(date -ud "$date_YMDH + 12 hours" +%Y%m%d%H )

while [ "$YMDH" -le "$END_YMDH" ]; do

today=${YMDH:0:8}00
yesterday=${YMDHm1d:0:8}00
echo $winstart $YMDH $winend

for plat in ${platlist[@]}; do

rm window.txt
echo ${winstart}0000 > window.txt
echo ${winend}0000 >> window.txt

rm ssh.bin ssh.txt
if [[ ${YMDH:8:2} == "00" ]]; then
ls -l ${ocnqcpath}/${yesterday}.${plat}
ln -sf ${ocnqcpath}/${yesterday}.${plat} ssh.bin
${HOMEwork}/rtofs_obs_read read_ssh

if [[ -f ssh.txt ]]; then
mv ssh.txt sshab.txt
fi

fi

ls -l ${ocnqcpath}/${today}.${plat}
ln -sf ${ocnqcpath}/${today}.${plat} ssh.bin
${HOMEwork}/rtofs_obs_read read_ssh
if [[ -f ssh.txt ]]; then
cat ssh.txt >> sshab.txt
fi

mv sshab.txt ssh.txt
echo start NETCDF

${HOMEwork}/rtofs_ascii2iodav2.py -i ssh.txt -v absolute_dynamic_topography -o ./adt_${plat}_${YMDH:0:8}.nc
mv ssh.txt data/.

done # plat loop

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )

DHH=$(($DH-12))
YMDHm1d=$(date -ud "$date_YMDH + $DHH hours" +%Y%m%d%H )

DHm3h=$(($DH-12))
winstart=$(date -ud "$date_YMDH + $DHm3h hours" +%Y%m%d%H )

DHp3h=$(($DH+12))
winend=$(date -ud "$date_YMDH + $DHp3h hours" +%Y%m%d%H )

DH=$(($DH+24))

done # day loop

mv adt_*.nc NC/.

################### convert RTOFS profile into IODA v2 ##############

DH=24
platlist=(profile)

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )
today=${YMDH:0:8}00

YMDHm1d=$(date -ud "$date_YMDH - 19 hours" +%Y%m%d%H )
yesterday=${YMDHm1d:0:8}00

winstart=$(date -ud "$date_YMDH - 12 hours" +%Y%m%d%H )
winend=$(date -ud "$date_YMDH + 12 hours" +%Y%m%d%H )

while [ "$YMDH" -le "$END_YMDH" ]; do

today=${YMDH:0:8}00
yesterday=${YMDHm1d:0:8}00
echo $winstart $YMDH $winend

for plat in ${platlist[@]}; do

rm window.txt
echo ${winstart}00 > window.txt
echo ${winend}00 >> window.txt

rm profile.bin profile.txt
if [[ ${YMDH:8:2} == "00" ]]; then
ls -l ${HOMEwork}/ocnqc/profile/${yesterday}.${plat}
ln -sf ${HOMEwork}/ocnqc/profile/${yesterday}.${plat} profile.bin
${HOMEwork}/rtofs_obs_read read_profile
if [[ -f profile.txt ]]; then
mv profile.txt profileab.txt
fi
fi

ls -l ${HOMEwork}/ocnqc/profile/${today}.${plat}
ln -sf ${HOMEwork}/ocnqc/profile/${today}.${plat} profile.bin
${HOMEwork}/rtofs_obs_read read_profile

if [[ -f profile.txt ]]; then
cat profile.txt >> profileab.txt
fi

mv profileab.txt profile.txt
echo start NETCDF

${HOMEwork}/rtofs_ascii2iodav2.py -i profile.txt -v sea_water_temperature -o ./insitu_${plat}_${YMDH:0:8}.nc
mv profile.txt ./data/.

done # plat loop

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )

DHH=$(($DH-12))
YMDHm1d=$(date -ud "$date_YMDH + $DHH hours" +%Y%m%d%H )

DHm3h=$(($DH-12))
winstart=$(date -ud "$date_YMDH + $DHm3h hours" +%Y%m%d%H )

DHp3h=$(($DH+12))
winend=$(date -ud "$date_YMDH + $DHp3h hours" +%Y%m%d%H )

DH=$(($DH+24))

done # day loop

mv insitu_profile_*.nc NC/.

fi # SKIP

################### convert RTOFS SSS into IODA v2 ##############

DH=24
platlist=(sss)

YMDH=$(date -ud "$date_YMDH " +%Y%m%d%H )
today=${YMDH:0:8}00

YMDHm1d=$(date -ud "$date_YMDH - 19 hours" +%Y%m%d%H )
yesterday=${YMDHm1d:0:8}00

winstart=$(date -ud "$date_YMDH - 12 hours" +%Y%m%d%H )
winend=$(date -ud "$date_YMDH + 12 hours" +%Y%m%d%H )

while [ "$YMDH" -le "$END_YMDH" ]; do

today=${YMDH:0:8}00
yesterday=${YMDHm1d:0:8}00
echo $winstart $YMDH $winend

for plat in ${platlist[@]}; do

rm window.txt
echo ${winstart}00 > window.txt
echo ${winend}00 >> window.txt

rm sss.bin sss.txt
if [[ ${YMDH:8:2} == "00" ]]; then
ls -l ${HOMEwork}/ocnqc/sss/${yesterday}.${plat}
ln -sf ${HOMEwork}/ocnqc/sss/${yesterday}.${plat} sss.bin
${HOMEwork}/rtofs_obs_read read_sss
if [[ -f sss.txt ]]; then
mv sss.txt sssab.txt
fi
fi

ls -l ${HOMEwork}/ocnqc/sss/${today}.${plat}
ln -sf ${HOMEwork}/ocnqc/sss/${today}.${plat} sss.bin
${HOMEwork}/rtofs_obs_read read_sss

if [[ -f profile.txt ]]; then
cat sss.txt >> sssab.txt
fi

mv sssab.txt sss.txt
echo start NETCDF

${HOMEwork}/rtofs_ascii2iodav2.py -i sss.txt -v sea_surface_salinity -o ./sss_salinity_${YMDH:0:8}.nc
mv sss.txt data/.

done # plat loop

YMDH=$(date -ud "$date_YMDH + $DH hours" +%Y%m%d%H )

DHH=$(($DH-12))
YMDHm1d=$(date -ud "$date_YMDH + $DHH hours" +%Y%m%d%H )

DHm3h=$(($DH-12))
winstart=$(date -ud "$date_YMDH + $DHm3h hours" +%Y%m%d%H )

DHp3h=$(($DH+12))
winend=$(date -ud "$date_YMDH + $DHp3h hours" +%Y%m%d%H )

DH=$(($DH+24))

done # day loop

mv sss_salinity_*.nc NC/.

##################### END ###############################

if [[ ! -d data ]]; then
   mkdir data
fi
mv *.bin *.txt ./data/.

