#!/bin/sh

set -xe

NCP=${NCP:-'/bin/cp'}
NLN=${NLN:-'/bin/ln -sf'}
NDATE=${NDATE:-ndate}

TOTAL_TASKS=${TOTAL_TASKS:-144}
NCTSK=${NCTSK:-24}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
APRUNS=${APRUNS:-"aprun -b -j1 -n1 -N1 -d1 -cc depth"}
APRUNF=${APRUNF:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth cfp"}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

export MP_LABELIO=yes

CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:-126}
NOUTHRS=${NOUTHRS:-3}
NOUTMOM6=${NOUTMOM6:-1}

POSTEXEC=${POSTEXEC:-${EXEChafs}/hafs_post.x}
MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
MPPNCCOMBINE=${MPPNCCOMBINE:-${EXEChafs}/hafs_mppnccombine.x}
WGRIB2=${WGRIB2:-wgrib2}
GRB2INDEX=${GRB2INDEX:-grb2index}

WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
COMhafs=${COMhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/com/${CDATE}/${STORMID}}
FIXcrtm=${FIXcrtm:-${FIXhafs}/hafs-crtm-2.3.0}
intercom=${intercom:-${WORKhafs}/intercom/post}
SENDCOM=${SENDCOM:-YES}

COMOUTpost=${COMOUTpost:-${COMhafs}}
DATA=${DATA:-${WORKhafs}/ocn_post}

IFHR=0
FHR=0
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )

FHR_MOM6=$NOUTMOM6

# Loop for forecast hours
while [ $FHR -le $NHRS ];
do

#cd ${DATA}
cd ${COMhafs}

# do post processing here

NEWDATE=`${NDATE} +${FHR_MOM6} $CDATE`
YYYY=`echo $NEWDATE | cut -c1-4`
MM=`echo $NEWDATE | cut -c5-6`
DD=`echo $NEWDATE | cut -c7-8`
HH=`echo $NEWDATE | cut -c9-10`
if [ -f ${WORKhafs}/forecast/ocn_${YYYY}_${MM}_${DD}_${HH}.nc ]; then
   #cp ${WORKhafs}/forecast/ocn_${YYYY}_${MM}_${DD}_${HH}.nc ${COMhafs}/ocn_${YYYY}_${MM}_${DD}_${HH}.nc
   cp ${WORKhafs}/forecast/ocn_${YYYY}_${MM}_${DD}_${HH}.nc .
fi

IFHR=`expr $IFHR + 1`
FHR=`expr $FHR + $NOUTHRS`
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )
FHR_MOM6=`expr $FHR_MOM6 + $NOUTHRS`

done
# End loop for forecast hours
exit
