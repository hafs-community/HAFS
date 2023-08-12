#!/bin/sh

set -xe

CDATE=${CDATE:-${YMDH}}
ENS=${ENS:-99}
YMD=$(echo ${CDATE} | cut -c1-8)
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)
hh=$(echo ${CDATE} | cut -c9-10)
cycle=$hh
chr=$ENS

module load aws-utils

GEFS_dir=${DATA:-${WORKhafs}/input/GEFS}
GFS_dir=${DATA:-${WORKhafs}/input/COMGFSv16}
RTOFS_dir=${DATA:-${WORKhafs}/input/COMRTOFSv2}
SYNDAT_dir=${DATA:-${WORKhafs}/input/SYNDAT-PLUS}

rm -fr $GEFS_dir $GFS_dir $RTOFS_dir
mkdir -p $GEFS_dir
mkdir -p $GFS_dir
mkdir -p $RTOFS_dir
mkdir -p $SYNDAT_dir

#getting SYNDAT
cd $SYNDAT_dir
aws s3 cp s3://noaa-hfip-none-ca-hafs-ens/SYNDAT-PLUS/syndat_tcvitals.${yr} .

#getting GEFS input
s3_cp_file_pfx="aws s3 cp --no-sign-request s3://noaa-gefs-pds/gefs."${YMD}
s3_cp_dir_pfx="aws s3 cp --recursive --no-sign-request s3://noaa-gefs-pds/gefs."${YMD}

dir_date=${CDATE}
cd $GEFS_dir

mkdir -p $dir_date
cd $dir_date
        fn_pfx="gec00"
        if [ $ENS -ne "00" ]; then
                fn_pfx="gep"${chr}
        fi
        cd ${GEFS_dir}/${dir_date}
        mkdir -p $ENS
        cd ${ENS}
for nhr in 0 6 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126
do
  if [ $nhr -eq 0 ]; then
   fhr='00'$nhr
   lhr='0'$nhr
  elif [ $nhr -lt 10 ]; then
   fhr='00'$nhr
   lhr='0'$nhr
  elif [ $nhr -lt 100 ]; then
   fhr='0'$nhr
   lhr=$nhr
  else
   fhr=$nhr
   lhr=$nhr
  fi

  cmd="${s3_cp_file_pfx}/${cycle}/atmos/pgrb2ap5/${fn_pfx}.t${cycle}z.pgrb2a.0p50.f${fhr} ${fn_pfx}.t${cycle}z.pgrb2af${lhr}"
  #echo "$cmd"
  ret=$(${cmd})
  cmd="${s3_cp_file_pfx}/${cycle}/atmos/pgrb2bp5/${fn_pfx}.t${cycle}z.pgrb2b.0p50.f${fhr} ${fn_pfx}.t${cycle}z.pgrb2bf${lhr}"
  #echo "$cmd"
  ret=$(${cmd})
  wait
done

#getting GFS input
CDATEm6=`ndate -6 $CDATE`
YMDm6=`echo $CDATEm6 |cut -c1-8`
cyclem6=`echo $CDATEm6 |cut -c9-10`
dir_date="gfs."${YMD}
dir_datem6="gfs."${YMDm6}
s3_cp_file_pfx="aws s3 cp --no-sign-request s3://noaa-gfs-bdp-pds/gfs."${YMD}
s3_cp_dir_pfx="aws s3 cp --recursive --no-sign-request s3://noaa-gfs-bdp-pds/gfs."${YMD}
s3_cp_file_pfxm6="aws s3 cp --no-sign-request s3://noaa-gfs-bdp-pds/gfs."${YMDm6}
s3_cp_dir_pfxm6="aws s3 cp --recursive --no-sign-request s3://noaa-gfs-bdp-pds/gfs."${YMDm6}

mkdir -p ${GFS_dir}/${dir_date}
mkdir -p ${GFS_dir}/${dir_datem6}

cd ${GFS_dir}/${dir_date}
fn_pfx="gfs"
mkdir -p $cycle/atmos
cd ${cycle}/atmos
for nhr in {0..129..3}
do
  if [ $nhr -eq 0 ]; then
   fhr='00'$nhr
  elif [ $nhr -lt 10 ]; then
   fhr='00'$nhr
  elif [ $nhr -lt 100 ]; then
   fhr='0'$nhr
  else
   fhr=$nhr
  fi
        cmd="${s3_cp_file_pfx}/${cycle}/atmos/${fn_pfx}.t${cycle}z.pgrb2.0p25.f${fhr} ${fn_pfx}.t${cycle}z.pgrb2.0p25.f${fhr}"
        #echo "$cmd"
        ret=$(${cmd})
        cmd="${s3_cp_file_pfx}/${cycle}/atmos/${fn_pfx}.t${cycle}z.pgrb2b.0p25.f${fhr} ${fn_pfx}.t${cycle}z.pgrb2b.0p25.f${fhr}"
        #echo "$cmd"
        ret=$(${cmd})
        wait
done

cd ${GFS_dir}/${dir_datem6}
fn_pfx="gfs"
mkdir -p $cyclem6/atmos
cd ${cyclem6}/atmos
for nhr in {0..6..3}
do
  if [ $nhr -eq 0 ]; then
   fhr='00'$nhr
  elif [ $nhr -lt 10 ]; then
   fhr='00'$nhr
  elif [ $nhr -lt 100 ]; then
   fhr='0'$nhr
  else
   fhr=$nhr
  fi
        cmd="${s3_cp_file_pfxm6}/${cyclem6}/atmos/${fn_pfx}.t${cyclem6}z.pgrb2.0p25.f${fhr} ${fn_pfx}.t${cyclem6}z.pgrb2.0p25.f${fhr}"
        #echo "$cmd"
        ret=$(${cmd})
        cmd="${s3_cp_file_pfxm6}/${cyclem6}/atmos/${fn_pfx}.t${cyclem6}z.pgrb2b.0p25.f${fhr} ${fn_pfx}.t${cyclem6}z.pgrb2b.0p25.f${fhr}"
        #echo "$cmd"
        ret=$(${cmd})
        wait
done
#Getting RTOFS input
dir_date="rtofs."${YMD}
s3_cp_file_pfx="aws s3 cp --no-sign-request s3://noaa-nws-rtofs-pds/rtofs."${YMD}
s3_cp_dir_pfx="aws s3 cp --recursive --no-sign-request s3://noaa-nws-rtofs-pds/rtofs."${YMD}

mkdir -p ${RTOFS_dir}/$dir_date

cd ${RTOFS_dir}/$dir_date

# download data for different cycles
if [ $cycle == 00 ] ; then 
        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.n00.archv.b ."
        ret=$(${cmd})
#ZZ        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.n00.archv.a.tgz ."
        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.n00.archv.a ."
        ret=$(${cmd})
        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.n00.restart.b ."
        ret=$(${cmd})
#ZZ        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.n00.restart.a.tgz ."
        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.n00.restart.a ."
        ret=$(${cmd})
else
#ZZfor cyc in 06 12 18
#ZZdo
        #cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.f${cyc}.archv.a ."
        #ret=$(${cmd})
        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.n00.restart.b ."
        ret=$(${cmd})
        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.f${cycle}.archv.b ."
        ret=$(${cmd})
        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.n00.restart.a ."
        ret=$(${cmd})
#ZZ        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.f${cycle}.archv.a.tgz ."
        cmd="${s3_cp_file_pfx}/rtofs_glo.t00z.f${cycle}.archv.a ."
        ret=$(${cmd})
#ZZdone
fi
echo "done downloading input file"

exit
