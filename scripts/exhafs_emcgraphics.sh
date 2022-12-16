#!/bin/sh

set -xe

date

YMDH=${YMDH:-2019082900}
STORM=${STORM:-NATL}
storm=${STORM,,}
STORMID=${STORMID:-00L}
stormid=${STORMID,,}

export NOUTHRS=${NOUTHRS:-3}
export run_ocean=${run_ocean:-no}

stormModel=${stormModel:-HAFS}
#figTimeLevels=$(seq 0 42)
trackOn=${trackOn:-False}

modelLabels="['BEST','OFCL','${stormModel}','HWRF','HMON','AVNO']"
modelColors="['black','red','cyan','purple','green','blue']"
modelMarkers="['hr','.','.','.','.','.']"
modelMarkerSizes="[18,15,15,15,15,15]"
nset=""

stormname=${STORM}
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]' `
stormid=`echo ${stormid} | tr '[A-Z]' '[a-z]' `
STORMNAME=`echo ${stormname} | tr '[a-z]' '[A-Z]' `
stormname=`echo ${stormname} | tr '[A-Z]' '[a-z]' `

stormnmid=`echo ${stormname}${stormid} | tr '[A-Z]' '[a-z]' `
STORMNMID=`echo ${stormnmid} | tr '[a-z]' '[A-Z]' `
STORMNM=${STORMNMID:0:-3}
stormnm=${STORMNM,,}
STID=${STORMNMID: -3}
stid=${STID,,}
STORMNUM=${STID:0:2}
BASIN1C=${STID: -1}
basin1c=${BASIN1C,,}
yyyy=`echo ${YMDH} | cut -c1-4`

export HOMEgraph=${HOMEgraph:-/mnt/lfs4/HFIP/hwrfv3/${USER}/hafs_graphics}
export USHgraph=${USHgraph:-${HOMEgraph}/ush}
export DRIVERATMOS=${USHgraph}/driverAtmos.sh
export DRIVEROCEAN=${USHgraph}/driverOcean.sh
export PLOTATCF=${USHgraph}/python/ATCF/plotATCF.sh

export COMhafs=${COMhafs:-/hafs/com/${YMDH}/${STORMID}}
export WORKgraph=${WORKgraph:-${COMhafs}/../../../${YMDH}/${STORMID}/emc_graphics}
export COMgraph=${COMgraph:-${COMhafs}/emc_graphics}

export machine=${WHERE_AM_I:-wcoss2} # platforms: wcoss2, hera, orion, jet
export TOTAL_TASKS=${TOTAL_TASKS:-${SLURM_NTASKS:-480}}
export NCTSK=${NCTSK:-16}
export NCNODE=${NCNODE:-16}
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}

if [ ${machine} = jet ]; then
  export ADECKgraph=${ADECKgraph:-/mnt/lfs4/HFIP/hwrf-data/hwrf-input/abdeck/aid}
  export BDECKgraph=${BDECKgraph:-/mnt/lfs4/HFIP/hwrf-data/hwrf-input/abdeck/btk}
  export cartopyDataDir=${cartopyDataDir:-/mnt/lfs4/HFIP/hwrfv3/local/share/cartopy}
elif [ ${machine} = hera ]; then
  export ADECKgraph=${ADECKgraph:-/scratch1/NCEPDEV/hwrf/noscrub/input/abdeck/aid}
  export BDECKgraph=${BDECKgraph:-/scratch1/NCEPDEV/hwrf/noscrub/input/abdeck/btk}
  export cartopyDataDir=${cartopyDataDir:-/scratch1/NCEPDEV/hwrf/noscrub/local/share/cartopy}
elif [ ${machine} = orion ]; then
  export ADECKgraph=${ADECKgraph:-/work/noaa/hwrf/noscrub/input/abdeck/aid}
  export BDECKgraph=${BDECKgraph:-/work/noaa/hwrf/noscrub/input/abdeck/btk}
  export cartopyDataDir=${cartopyDataDir:-/work/noaa/hwrf/noscrub/local/share/cartopy}
elif [ ${machine} = wcoss2 ]; then
  export ADECKgraph=${ADECKgraph:-/lfs/h2/emc/hur/noscrub/input/abdeck/aid}
  export BDECKgraph=${BDECKgraph:-/lfs/h2/emc/hur/noscrub/input/abdeck/btk}
  export cartopyDataDir=${cartopyDataDir:-/lfs/h2/emc/hur/noscrub/local/share/cartopy}
else
  export ADECKgraph=${ADECKgraph:-/your/abdeck/aid}
  export BDECKgraph=${BDECKgraph:-/your/abdeck/btk}
  export cartopyDataDir=${cartopyDataDir:-/your/local/share/cartopy}
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

mkdir -p ${WORKgraph}
cd ${WORKgraph}

IFHR=0
FHR=0
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )
NHR3=$( printf "%03d" "$NHRS" )

# Loop for forecast hours
while [ $FHR -le $NHRS ];
do

cd ${WORKgraph}

NEWDATE=`${NDATE} +${FHR} $CDATE`
YYYY=`echo $NEWDATE | cut -c1-4`
MM=`echo $NEWDATE | cut -c5-6`
DD=`echo $NEWDATE | cut -c7-8`
HH=`echo $NEWDATE | cut -c9-10`

# Check if graphics has processed this forecast hour previously
if [ -s ${WORKhafs}/forecast/graphf${FHR3} ] && \
   [ ${WORKhafs}/forecast/graphf${FHR3} -nt ${WORKhafs}/forecast/postf${FHR3} ] ; then

echo "graph message ${WORKhafs}/forecast/graphf${FHR3} exist and newer than ${WORKhafs}/forecast/postf${FHR3}"
echo "skip graphics for forecast hour ${FHR3} valid at ${NEWDATE}"

# Otherwise run graphics for this forecast hour
else

atcfFile=${COMhafs}/${stormid}.${YMDH}.hafs.trak.atcfunix.all
prodlog=${WORKhafs}/product/run_product.grid02.log
FHRN=`expr $FHR + $NOUTHRS`
STRFHRN="New forecast hour:$( printf "%5d" "$FHRN" ):00"
STRDONE="top of output_all"

# Wait for post and product output
n=1
while [ $n -le 600 ]
do
  if [ -f ${WORKhafs}/forecast/postf${FHR3} ] && [ -f ${atcfFile} ] ; then
    echo "${WORKhafs}/forecast/postf${FHR3} and ${atcfFile} exist"
    if grep -q "$STRDONE" ${prodlog} || grep -q "$STRFHRN" ${prodlog} ; then
      echo "GFDL tracker succeeded or has processed this time level, do graphics."
      sleep 1s
      break
	else
      echo "GFDL tracker has not processed this time level, sleep 60"
      sleep 60s
    fi
  else
    echo "${WORKhafs}/forecast/postf${FHR3} or ${atcfFile} not ready, sleep 60"
    sleep 60s
  fi
  n=$(( n+1 ))
done

figTimeLevels=${IFHR}

#Generate the cmdfile
cmdfile="cmdfile_atmos.${FHR3}"
rm -f $cmdfile
touch $cmdfile

#==============================================================================
# Plot ATCF track and intensity figures
#==============================================================================

# Produce these figures only if product job is still running (not done yet).
if [ ${IFHR} -eq 0 ] || [ ! -s ${CDNOSCRUB}/${SUBEXPT}/${stormid}.${YMDH}.hafs.trak.atcfunix.all ]; then

atcfFile=${COMhafs}/${stormid}.${YMDH}.hafs.trak.atcfunix.all

cd ${WORKgraph}

if [ -f ${atcfFile} ]; then
  echo "${atcfFile} present, will proceed"
  # make the track and intensity plots
  sh ${PLOTATCF} ${STORMNM} ${STID} ${YMDH} ${stormModel} ${COMhafs} ${ADECKgraph} ${BDECKgraph} ${HOMEgraph}/ush/python ${WORKgraph} ${archdir} ${modelLabels} ${modelColors} ${modelMarkers} ${modelMarkerSizes} ${nset}
else
  echo "${atcfFile} NOT PRESENT. SKIP."
fi

fi

date

#==============================================================================
# For the atmos figures
#==============================================================================
for stormDomain in grid01 grid02; do

if [ ${stormDomain} = "grid01" ]; then
  figScriptAll=( \
    plot_mslp_wind10m.py \
    plot_tsfc_mslp_wind10m.py \
    plot_t2m_mslp_wind10m.py \
    plot_heatflux_wind10m.py \
    plot_shtflux_wind10m.py \
    plot_lhtflux_wind10m.py \
    plot_precip_mslp_thk.py \
    plot_reflectivity.py \
    plot_850mb_200mb_vws.py \
    plot_rhmidlev_hgt_wind.py \
    plot_temp_hgt_wind.py \
    plot_temp_hgt_wind.py \
    plot_temp_hgt_wind.py \
    plot_temp_hgt_wind.py \
    plot_rh_hgt_wind.py \
    plot_rh_hgt_wind.py \
    plot_rh_hgt_wind.py \
    plot_rh_hgt_wind.py \
    plot_vort_hgt_wind.py \
    plot_vort_hgt_wind.py \
    plot_vort_hgt_wind.py \
    plot_vort_hgt_wind.py \
    plot_streamline_wind.py \
    )
  levAll=( \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    850 \
    700 \
    500 \
    200 \
    850 \
    700 \
    500 \
    200 \
    850 \
    700 \
    500 \
    200 \
    850 \
    )
elif [ ${stormDomain} = "grid02" ]; then
  figScriptAll=( \
    plot_mslp_wind10m.py \
    plot_tsfc_mslp_wind10m.py \
    plot_t2m_mslp_wind10m.py \
    plot_heatflux_wind10m.py \
    plot_shtflux_wind10m.py \
    plot_lhtflux_wind10m.py \
    plot_precip_mslp_thk.py \
    plot_reflectivity.py \
    plot_rhmidlev_hgt_wind.py \
    plot_temp_hgt_wind.py \
    plot_temp_hgt_wind.py \
    plot_temp_hgt_wind.py \
    plot_temp_hgt_wind.py \
    plot_rh_hgt_wind.py \
    plot_rh_hgt_wind.py \
    plot_rh_hgt_wind.py \
    plot_rh_hgt_wind.py \
    plot_vort_hgt_wind.py \
    plot_vort_hgt_wind.py \
    plot_vort_hgt_wind.py \
    plot_vort_hgt_wind.py \
    plot_streamline_wind.py \
    plot_tempanomaly_hgt_wind.py \
    )
  levAll=( \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    1003 \
    850 \
    700 \
    500 \
    200 \
    850 \
    700 \
    500 \
    200 \
    850 \
    700 \
    500 \
    200 \
    850 \
    200 \
    )
fi
nscripts=${#figScriptAll[*]}

for((i=0;i<${nscripts};i++)); do
  fhhh="f${FHR3}"
  echo ${figScriptAll[$i]} ${levAll[$i]} ${fhhh}
  echo "time ${DRIVERATMOS} $stormModel $STORM $STORMID $YMDH $stormDomain ${figScriptAll[$i]} ${levAll[$i]} ${fhhh} > ${WORKgraph}/$STORM$STORMID.$YMDH.${stormDomain}.${figScriptAll[$i]%.*}.${fhhh}.log 2>&1" >> $cmdfile
done

done

#==============================================================================

chmod u+x ./$cmdfile
if [ ${machine} = "wcoss2" ]; then
  ncmd=$(cat ./$cmdfile | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP  -n $ncmd_max cfp ./$cmdfile
else
  ${APRUNC} ${MPISERIAL} ./$cmdfile
fi

date

cd ${WORKgraph}

# Write out the graphics done message file
echo 'done' > ${WORKhafs}/forecast/graphf${FHR3}

fi
# End if for checking if graphics has processed this forecast hour previously

IFHR=`expr $IFHR + 1`
FHR=`expr $FHR + $NOUTHRS`
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )

done
# End loop for forecast hours

#==============================================================================
# Plot ATCF track and intensity figures after the product job is done
#==============================================================================

atcfFile=${CDNOSCRUB}/${SUBEXPT}/${stormid}.${YMDH}.hafs.trak.atcfunix.all

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

if [ -f ${atcfFile} ]; then
  echo "${atcfFile} present, will proceed"
  # make the track and intensity plots
  sh ${PLOTATCF} ${STORMNM} ${STID} ${YMDH} ${stormModel} ${COMhafs} ${ADECKgraph} ${BDECKgraph} ${HOMEgraph}/ush/python ${WORKgraph} ${archdir} ${modelLabels} ${modelColors} ${modelMarkers} ${modelMarkerSizes} ${nset}
else
  echo "${atcfFile} NOT PRESENT. SKIP."
fi

date

#==============================================================================
# For the ocean figures
#==============================================================================

if [ ${run_ocean} = yes ];  then

cd ${WORKgraph}

# Wait for hycompost and product output
atcfFile=${CDNOSCRUB}/${SUBEXPT}/${stormid}.${YMDH}.hafs.trak.atcfunix.all
n=1
while [ $n -le 600 ]
do
  if [ -f ${COMhafs}/${stormid}.${YMDH}.hafs.hycom.3z.f${NHR3}.nc ] && [ -f ${atcfFile} ] ; then
    echo "${COMhafs}/${stormid}.${YMDH}.hafs.hycom.3z.f${NHR3}.nc and ${atcfFile} exist"
    sleep 1s
    break
  else
    echo "${COMhafs}/${stormid}.${YMDH}.hafs.hycom.3z.f${NHR3}.nc or ${atcfFile} not ready, sleep 60"
    sleep 60s
  fi
  n=$(( n+1 ))
done

#Generate the cmdfile
cmdfile='cmdfile_ocean'
rm -f $cmdfile
touch $cmdfile

figScriptAll=( \
  "plot_sst.py" \
  "plot_sss.py" \
  "plot_mld.py" \
  "plot_ohc.py" \
  "plot_z20.py" \
  "plot_z26.py" \
  "plot_storm_sst.py" \
  "plot_storm_sss.py" \
  "plot_storm_mld.py" \
  "plot_storm_ohc.py" \
  "plot_storm_z20.py" \
  "plot_storm_z26.py" \
  "plot_storm_tempz40m.py" \
  "plot_storm_tempz70m.py" \
  "plot_storm_tempz100m.py" \
  "plot_storm_wvelz40m.py" \
  "plot_storm_wvelz70m.py" \
  "plot_storm_wvelz100m.py" \
  )

nscripts=${#figScriptAll[*]}

trackOn=True

for((i=0;i<${nscripts};i++));
do
  echo ${figScriptAll[$i]}
  echo "${APRUNS} ${DRIVEROCEAN} $stormModel $STORM $STORMID $YMDH $trackOn ${figScriptAll[$i]} > ${WORKgraph}/$STORM$STORMID.$YMDH.${figScriptAll[$i]%.*}.log 2>&1" >> $cmdfile
done

chmod u+x ./$cmdfile
if [ ${machine} = "wcoss2" ]; then
  ncmd=$(cat ./$cmdfile | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP  -n $ncmd_max cfp ./$cmdfile
else
  ${APRUNC} ${MPISERIAL} ./$cmdfile
fi

fi
#==============================================================================

date

echo "graphics job done"

exit
