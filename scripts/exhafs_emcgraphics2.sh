#!/bin/sh

set -xe

date

YMDH=${YMDH:-2019082900}
STORM=${STORM:-NATL}
storm=${STORM,,}
STORMID=${STORMID:-00L}
stormid=${STORMID,,}
stormModel=${stormModel:-HAFS}
is6Hr=${is6Hr:-False}
#figTimeLevels=$(seq 0 42)
trackOn=${trackOn:-True}

modelLabels='(/"BEST","'$stormModel'","HWRF","HMON","AVNO","OFCL"/)'
modelColors='(/"black","cyan2","purple","green2","blue","red"/)'
modelMarkers='(/17,18,18,18,18,18/)'

export HOMEgraph=${HOMEgraph:-/mnt/lfs4/HFIP/hwrfv3/${USER}/hafs_graphics}
export USHgraph=${USHgraph:-${HOMEgraph}/ush}
export DRIVERDOMAIN=${USHgraph}/driverDomain.sh
export PLOTATCF=${USHgraph}/plotATCF.sh

export COMhafs=${COMhafs:-/hafs/com/${YMDH}/${STORMID}}
export WORKgraph=${WORKgraph:-${COMhafs}/../../../${YMDH}/${STORMID}/emc_graphics}
export COMgraph=${COMgraph:-${COMhafs}/emc_graphics}

export machine=${WHERE_AM_I:-wcoss_cray} # platforms: wcoss_cray, wcoss_dell_p3, hera, orion, jet
export TOTAL_TASKS=${TOTAL_TASKS:-${SLURM_NTASKS:-480}}
export NCTSK=${NCTSK:-16}
export NCNODE=${NCNODE:-16}
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}

mkdir -p ${WORKgraph}
cd ${WORKgraph}

#==============================================================================
# Plot ATCF track and intensity figures after the product job is done
#==============================================================================

atcfFile=${CDNOSCRUB}/${SUBEXPT}/${storm}${stormid}.${YMDH}.trak.hafs.atcfunix.all.orig

# Wait for atcfFile under ${CDNOSCRUB}/${SUBEXPT}
n=1
while [ $n -le 600 ]
do
  if [ ! -f ${atcfFile} ] ; then
    echo "${atcfFile} not ready, sleep 60"
    sleep 60s
  else
    echo "${atcfFile} exist, do graphics"
    sleep 1s
    break
  fi
  n=$(( n+1 ))
done

cd ${WORKgraph}

atcfFile=${COMhafs}/${storm}${stormid}.${YMDH}.trak.hafs.atcfunix.all

if [ -f ${atcfFile} ]; then
  atcfFile=${atcfFile}
elif [ -f ${atcfFile%.all} ]; then
  atcfFile=${atcfFile%.all}
else
  echo "File ${atcfFile} does not exist"
  echo 'SCRIPT WILL EXIT'
  exit 1
fi

array=$( sh ${USHgraph}/getStormNames.sh ${atcfFile} ${YMDH} )
echo $array

# Loop for all storms
for stormnmid in ${array[@]}
do
  stormnmid=`echo ${stormnmid} | tr '[A-Z]' '[a-z]' `
  STORMNMID=`echo ${stormnmid} | tr '[a-z]' '[A-Z]' `
  STORMNM=${STORMNMID:0:-3}
  stormnm=${STORMNM,,}
  STID=${STORMNMID: -3}
  stid=${STID,,}
  STORMNUM=${STID:0:2}
  BASIN1C=${STID: -1}
  basin1c=${BASIN1C,,}
  yyyy=`echo ${YMDH} | cut -c1-4`

  if [ ${BASIN1C} != ${STORMID: -1} ]; then
    continue
  fi

if [ ${basin1c} = 'l' ]; then
  basin2c='al'
  BASIN2C='AL'
  BASIN='NATL'
elif [ ${basin1c} = 'e' ]; then
  basin2c='ep'
  BASIN2C='EP'
  BASIN='EPAC'
elif [ ${basin1c} = 'c' ]; then
  basin2c='cp'
  BASIN2C='CP'
  BASIN='CPAC'
elif [ ${basin1c} = 'w' ]; then
  basin2c='wp'
  BASIN2C='WP'
  BASIN='WPAC'
elif [ ${basin1c} = 's' ] || [ ${basin1c} = 'p'  ]; then
  basin2c='sh'
  BASIN2C='SH'
  BASIN='SH'
elif [ ${basin1c} = 'a' ] || [ ${basin1c} = 'b'  ]; then
  basin2c='io'
  BASIN2C='IO'
  BASIN='NIO'
else
  echo "WRONG BASIN DESIGNATION basin1c=${basin1c}"
  echo 'SCRIPT WILL EXIT'
  exit 1
fi

  archbase="${COMgraph}/figures"
  archdir="${archbase}/RT${yyyy}_${BASIN}/${STORMNM}${STID}/${STORMNM}${STID}.${YMDH}"
  storm_atcfFile=${WORKgraph}/${stormnm}${stid}.${YMDH}.trak.hafs.atcfunix
  grep "^${BASIN2C}, ${STORMNUM}," ${atcfFile} > ${storm_atcfFile}

  if [ -f ${storm_atcfFile} ]; then
    echo "${storm_atcfFile} present, will proceed"
    # make the track and intensity plots
    sh ${HOMEgraph}/ush/plotATCF.sh ${STORMNM} ${STID} ${YMDH} ${stormModel} ${storm_atcfFile} ${ADECKgraph} ${BDECKgraph} ${HOMEgraph}/ush/ncl ${WORKgraph} ${archdir} ${modelLabels} ${modelColors} ${modelMarkers}
  else
    echo "${storm_atcfFile} NOT PRESENT. SKIP."
  fi
done

date

#
IFHR=0
FHR=0
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )

#Generate the cmdfile
cmdfile="cmdfile"
rm -f $cmdfile
touch $cmdfile

# Loop for forecast hours
while [ $FHR -le $NHRS ];
do

cd ${WORKgraph}

NEWDATE=`${NDATE} +${FHR} $CDATE`
YYYY=`echo $NEWDATE | cut -c1-4`
MM=`echo $NEWDATE | cut -c5-6`
DD=`echo $NEWDATE | cut -c7-8`
HH=`echo $NEWDATE | cut -c9-10`

# Wait for post output
n=1
while [ $n -le 600 ]
do
  if [ -f ${WORKhafs}/forecast/postf${FHR3} ] || [ -f ${atcfFile} ] ; then
    echo "${WORKhafs}/forecast/postf${FHR3} and ${atcfFile} exist, do graphics"
    break
  else
    echo "${WORKhafs}/forecast/postf${FHR3} or ${atcfFile} not ready, sleep 60"
    sleep 60s
  fi
  n=$(( n+1 ))
done

figTimeLevels=${IFHR}

#==============================================================================
# For the Whole Synoptic Domain
#==============================================================================

figScriptAll=( \
  "fv3_Mslp_10m_Wind_plot.ncl" \
  "fv3_Surface_Temp_Mslp_Wind_plot.ncl" \
  "fv3_Reflectivity_plot.ncl" \
  "fv3_Standard_Layer_Temp_Ght_Wind_plot.ncl" \
  "fv3_Standard_Layer_Vort_Ght_Wind_plot.ncl" \
  "fv3_Standard_Layer_Streamlines_plot.ncl" \
  "fv3_Standard_Layer_RH_Ght_Wind_plot.ncl" \
  "fv3_Standard_Layer_Vort_Ght_Wind_plot.ncl" \
  "fv3_Standard_Layer_Vort_Ght_Wind_plot.ncl" \
  "fv3_Precip_mslp_thickness_plot.ncl" \
  "fv3_Wind_Shear_plot.ncl" \
  )

figNameAll=( \
  "mslp.10m_wind" \
  "surface.temp.mslp.wind" \
  "reflectivity" \
  "850mb.temp.ght.wind" \
  "850mb.vort.hgt.wind" \
  "850mb.wind" \
  "700mb.rh.hgt.wind" \
  "500mb.vort.hgt.wind" \
  "200mb.vort.hgt.wind" \
  "precip.mslp.thk" \
  "wind.shear" \
  )

standardLayerAll=( \
  1003 \
  1003 \
  1003 \
  850 \
  850 \
  850 \
  700 \
  500 \
  200 \
  1003 \
  850 \
  )

nscripts=${#figScriptAll[*]}

isStormDomain=False

for((i=0;i<${nscripts};i++));
do

echo ${figScriptAll[$i]} ${figNameAll[$i]} ${standardLayerAll[$i]}

for figTimeLevel in ${figTimeLevels};
do
  echo "${APRUNS} ${DRIVERDOMAIN} $stormModel $STORM $STORMID $YMDH $isStormDomain $is6Hr ${trackOn} ${figScriptAll[$i]} ${figNameAll[$i]} ${standardLayerAll[$i]} $figTimeLevel $figTimeLevel > ${WORKgraph}/$STORM$STORMID.$YMDH.${figNameAll[$i]}.f${FHR3}.log 2>&1 ${BACKGROUND}" >> $cmdfile
done

done

#==============================================================================
# For the Inner Storm Domain(s)
#==============================================================================

figScriptAll=( \
  "fv3_Mslp_10m_Wind_plot.ncl" \
  "fv3_Surface_Temp_Mslp_Wind_plot.ncl" \
  "fv3_Reflectivity_plot.ncl" \
  "fv3_Standard_Layer_Vort_Ght_Wind_plot.ncl" \
  "fv3_Standard_Layer_Streamlines_plot.ncl" \
  "fv3_Standard_Layer_RH_Ght_Wind_plot.ncl" \
  "fv3_Standard_Layer_Vort_Ght_Wind_plot.ncl" \
  "fv3_Standard_Layer_Vort_Ght_Wind_plot.ncl" \
  "fv3_Standard_Layer_TempAno_plot.ncl" \
  )

figNameAll=( \
  "storm.mslp.10m_wind" \
  "storm.surface.temp.mslp.wind" \
  "storm.reflectivity" \
  "storm.850mb.vort.hgt.wind" \
  "storm.850mb.wind" \
  "storm.700mb.rh.hgt.wind" \
  "storm.500mb.vort.hgt.wind" \
  "storm.200mb.vort.hgt.wind" \
  "storm.200mb.tempano" \
  )

standardLayerAll=( \
  1003 \
  1003 \
  1003 \
  850 \
  850 \
  700 \
  500 \
  200 \
  200 \
  )

nscripts=${#figScriptAll[*]}

isStormDomain=True

for((i=0;i<${nscripts};i++));
do

echo ${figScriptAll[$i]} ${figNameAll[$i]} ${standardLayerAll[$i]}

for figTimeLevel in ${figTimeLevels};
do
  echo "${APRUNS} ${DRIVERDOMAIN} $stormModel $STORM $STORMID $YMDH $isStormDomain $is6Hr ${trackOn} ${figScriptAll[$i]} ${figNameAll[$i]} ${standardLayerAll[$i]} $figTimeLevel $figTimeLevel > ${WORKgraph}/$STORM$STORMID.$YMDH.${figNameAll[$i]}.f${FHR3}.log 2>&1 ${BACKGROUND}" >> $cmdfile
done

done

date

cd ${WORKgraph}

IFHR=`expr $IFHR + 1`
FHR=`expr $FHR + $NOUTHRS`
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )

done
# End loop for forecast hours

#==============================================================================

if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> $cmdfile
fi
chmod u+x ./$cmdfile
${APRUNF} ./$cmdfile

wait

echo "graphics job done"

exit
