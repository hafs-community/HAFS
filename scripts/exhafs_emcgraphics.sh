#!/bin/sh
################################################################################
# Script Name: exhafs_emcgraphics.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script generates EMC graphics through hafs_graphcs.
################################################################################
#
set -xe

date

YMDH=${YMDH:-2019082900}
STORM=${STORM:-NATL}
storm=${STORM,,}
STORMID=${STORMID:-00L}
stormid=${STORMID,,}

export NOUTHRS=${NOUTHRS:-3}
export run_ocean=${run_ocean:-no}

stormModel=${stormModel:-${RUN^^}}
#figTimeLevels=$(seq 0 42)
trackOn=${trackOn:-False}

modelLabels="['BEST','OFCL','${stormModel}','HWRF','AVNO']"
modelColors="['black','red','cyan','#FF80FF','blue']"
modelMarkers="['hr','.','.','.','.']"
modelMarkerSizes="[18,15,15,15,15]"
nset=""

stormname=${STORM}
STORMID=$(echo ${stormid} | tr '[a-z]' '[A-Z]')
stormid=$(echo ${stormid} | tr '[A-Z]' '[a-z]')
STORMNAME=$(echo ${stormname} | tr '[a-z]' '[A-Z]')
stormname=$(echo ${stormname} | tr '[A-Z]' '[a-z]')

stormnmid=$(echo ${stormname}${stormid} | tr '[A-Z]' '[a-z]')
STORMNMID=$(echo ${stormnmid} | tr '[a-z]' '[A-Z]')
STORMNM=${STORMNMID:0:-3}
stormnm=${STORMNM,,}
STID=${STORMNMID: -3}
stid=${STID,,}
STORMNUM=${STID:0:2}
BASIN1C=${STID: -1}
basin1c=${BASIN1C,,}
yyyy=$(echo ${YMDH} | cut -c1-4)

export HOMEgraph=${HOMEgraph:-/${HOMEhafs}/sorc/hafs_graphics.fd/emc_graphics}
export USHgraph=${USHgraph:-${HOMEgraph}/ush}
export DRIVERATMOS=${USHgraph}/driverAtmos.sh
export DRIVEROCEAN=${USHgraph}/driverOcean.sh
export PLOTATCF=${USHgraph}/python/ATCF/plotATCF.sh

export COMhafs=${COMhafs:-/hafs/com/${YMDH}/${STORMID}}
export WORKgraph=${WORKgraph:-${COMhafs}/../../../${YMDH}/${STORMID}/emc_graphics}
export COMgraph=${COMgraph:-${COMhafs}/emc_graphics}

export ADECKgraph=${ADECKhafs:?}
export BDECKgraph=${BDECKhafs:?}
export SYNDAThafs=${SYNDAThafs:?}
export cartopyDataDir=${cartopyDataDir:?}

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

intercompost=${WORKhafs}/intercom/post
intercomgraph=${WORKhafs}/intercom/emc_graphics
mkdir -p ${WORKgraph} ${intercomgraph}
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

NEWDATE=$(${NDATE} +${FHR} $CDATE)
YYYY=$(echo $NEWDATE | cut -c1-4)
MM=$(echo $NEWDATE | cut -c5-6)
DD=$(echo $NEWDATE | cut -c7-8)
HH=$(echo $NEWDATE | cut -c9-10)

# Check if graphics has processed this forecast hour previously
if [ -s ${intercomgraph}/graphf${FHR3} ] && \
   [ ${intercomgraph}/graphf${FHR3} -nt ${intercompost}/postf${FHR3} ]; then

echo "graph message ${intercomgraph}/graphf${FHR3} exist and newer than ${intercompost}/postf${FHR3}"
echo "skip graphics for forecast hour ${FHR3} valid at ${NEWDATE}"

# Otherwise run graphics for this forecast hour
else

atcfFile=${COMhafs}/${stormid}.${YMDH}.${RUN}.trak.atcfunix.all
prodlog=${WORKhafs}/product/run_product.storm.log
FHRN=$(($FHR + $NOUTHRS))
STRFHRN="New forecast hour:$( printf "%5d" "$FHRN" ):00"
STRDONE="top of output_all"

# Wait for post and product output
n=1
while [ $n -le 600 ]; do
  if [ -f ${intercompost}/postf${FHR3} ] && [ -f ${atcfFile} ]; then
    echo "${intercompost}/postf${FHR3} and ${atcfFile} exist"
    if grep -q "$STRDONE" ${prodlog} || grep -q "$STRFHRN" ${prodlog}; then
      echo "GFDL tracker succeeded or has processed this time level, do graphics."
      sleep 1s
      break
	else
      echo "GFDL tracker has not processed this time level, sleep 60s"
      sleep 60s
    fi
  else
    echo "${intercompost}/postf${FHR3} or ${atcfFile} not ready, sleep 60s"
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
if [ ${IFHR} -eq 0 ] || [ ! -s ${CDNOSCRUB}/${SUBEXPT}/${stormid}.${YMDH}.${RUN}.trak.atcfunix.all ]; then

atcfFile=${COMhafs}/${stormid}.${YMDH}.${RUN}.trak.atcfunix.all

cd ${WORKgraph}

if [ -f ${atcfFile} ]; then
  echo "${atcfFile} present, will proceed"
  # make the track and intensity plots
  sh ${PLOTATCF} ${STORMNM} ${STID} ${YMDH} ${stormModel} ${COMhafs} ${ADECKgraph} ${BDECKgraph} \
     ${HOMEgraph}/ush/python ${WORKgraph} ${archdir} \
     ${modelLabels} ${modelColors} ${modelMarkers} ${modelMarkerSizes} ${nset}
else
  echo "${atcfFile} NOT PRESENT. SKIP."
fi

fi

date

#==============================================================================
# For the atmos figures
#==============================================================================
for stormDomain in parent storm; do

if [ ${stormDomain} = "parent" ]; then
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
elif [ ${stormDomain} = "storm" ]; then
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
  echo "time ${DRIVERATMOS} $stormModel $STORM $STORMID $YMDH $stormDomain ${figScriptAll[$i]} ${levAll[$i]} ${fhhh} \
        > ${WORKgraph}/$STORM$STORMID.$YMDH.${stormDomain}.${figScriptAll[$i]%.*}.${fhhh}.log 2>&1" >> $cmdfile
done

if [ ${satpost} = .true. ]; then
  figScriptAll=( \
    plot_goes_ir13.py \
    plot_goes_wv9.py \
    plot_ssmisf17_mw37ghz.py \
    plot_ssmisf17_mw91ghz.py \
    )
  levAll=( \
    1003 \
    1003 \
    1003 \
    1003 \
    )
  nscripts=${#figScriptAll[*]}
  for((i=0;i<${nscripts};i++)); do
    fhhh="f${FHR3}"
    echo ${figScriptAll[$i]} ${levAll[$i]} ${fhhh}
    echo "time ${DRIVERATMOS} $stormModel $STORM $STORMID $YMDH $stormDomain ${figScriptAll[$i]} ${levAll[$i]} ${fhhh} \
          > ${WORKgraph}/$STORM$STORMID.$YMDH.${stormDomain}.${figScriptAll[$i]%.*}.${fhhh}.log 2>&1" >> $cmdfile
  done
fi

done

#==============================================================================

chmod u+x ./$cmdfile
${APRUNC} ${MPISERIAL} -m ./$cmdfile
export err=$?; err_chk

date

#==============================================================================

cd ${WORKgraph}

# Write out the graphics done message file
echo 'done' > ${intercomgraph}/graphf${FHR3}

fi
# End if for checking if graphics has processed this forecast hour previously

IFHR=$(($IFHR + 1))
FHR=$(($FHR + $NOUTHRS))
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )

done
# End loop for forecast hours

#==============================================================================
# Plot ATCF track and intensity figures after the product job is done
#==============================================================================

atcfFile=${CDNOSCRUB}/${SUBEXPT}/${stormid}.${YMDH}.${RUN}.trak.atcfunix.all

# Wait for atcfFile under ${CDNOSCRUB}/${SUBEXPT}
n=1
while [ $n -le 600 ]; do
  if [ ! -f ${atcfFile} ]; then
    echo "${atcfFile} not ready, sleep 60s"
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
  sh ${PLOTATCF} ${STORMNM} ${STID} ${YMDH} ${stormModel} ${COMhafs} ${ADECKgraph} ${BDECKgraph} \
     ${HOMEgraph}/ush/python ${WORKgraph} ${archdir} \
     ${modelLabels} ${modelColors} ${modelMarkers} ${modelMarkerSizes} ${nset}
else
  echo "${atcfFile} NOT PRESENT. SKIP."
fi

date

#==============================================================================
# Plot some atmosphere figures after the product and output jobs are done
#==============================================================================

cd ${WORKgraph}

# Wait for product and output
atcfFile=${CDNOSCRUB}/${SUBEXPT}/${stormid}.${YMDH}.${RUN}.trak.atcfunix.all
n=1
while [ $n -le 600 ]; do
  if [ -f ${COMhafs}/${stormid}.${YMDH}.${RUN}.parent.swath.grb2.idx ] && [ -f ${atcfFile} ]; then
    echo "${COMhafs}/${stormid}.${YMDH}.${RUN}.parent.swath.grb2.idx and ${atcfFile} exist"
    sleep 1s
    break
  else
    echo "${COMhafs}/${stormid}.${YMDH}.${RUN}.parent.swath.grb2.idx or ${atcfFile} not ready, sleep 60s"
    sleep 60s
  fi
  n=$(( n+1 ))
done

#Generate the cmdfile
cmdfile='cmdfile_output'
rm -f $cmdfile
touch $cmdfile

fhhhAll=$(seq -f "f%03g" 0 $NOUTHRS $NHRS)
# Loop for forecast hours
for fhhh in ${fhhhAll}; do

for stormDomain in storm; do

if [ ${stormDomain} = "storm" ]; then
  figScriptAll=( \
    plot_crs_sn_wind.py \
    plot_crs_sn_rh_tempanomaly.py \
    plot_crs_sn_reflectivity.py \
    plot_crs_we_wind.py \
    plot_crs_we_rh_tempanomaly.py \
    plot_crs_we_reflectivity.py \
    plot_azimuth_wind.py \
    plot_azimuth_tempanomaly.py \
    plot_azimuth_rh_q.py \
    plot_azimuth_reflectivity.py \
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
    )
fi

nscripts=${#figScriptAll[*]}

for((i=0;i<${nscripts};i++)); do
  echo ${figScriptAll[$i]} ${levAll[$i]}
  echo "time ${DRIVERATMOS} $stormModel $STORM $STORMID $YMDH $stormDomain ${figScriptAll[$i]} ${levAll[$i]} $fhhh > ${WORKgraph}/$STORM$STORMID.$YMDH.${stormDomain}.${figScriptAll[$i]%.*}.${fhhh}.log 2>&1 || exit 0" >> $cmdfile
done

done

done
# End loop for forecast hours

chmod u+x ./$cmdfile
${APRUNC} ${MPISERIAL} -m ./$cmdfile
export err=$?; err_chk

date

#==============================================================================
# For the ocean figures
#==============================================================================

if [ ${run_ocean} = yes ]; then
	
IFHR=0
FHR=0
FHR3=$( printf "%03d" "$FHR" )

# Loop for forecast hours
while [ $FHR -le $NHRS ];
do

cd ${WORKgraph}

if [ ${ocean_model,,} = hycom ] && [[ $(($FHR%2)) -ne 0 ]]; then
    echo "Forecast hour f${FHR3} does not exist"
    FHR=$(($FHR + $NOUTHRS))
    FHR3=$( printf "%03d" "$FHR" )
    continue
fi

#Generate the cmdfile
cmdfile="cmdfile_ocean.${FHR3}"
rm -f $cmdfile
touch $cmdfile

figScriptAll=( \
  plot_sst.py \
  plot_sss.py \
  plot_mld.py \
  plot_ohc.py \
  plot_z20.py \
  plot_z26.py \
  plot_storm_sst.py \
  plot_storm_sss.py \
  plot_storm_mld.py \
  plot_storm_ohc.py \
  plot_storm_z20.py \
  plot_storm_z26.py \
  plot_storm_tempz40m.py \
  plot_storm_tempz70m.py \
  plot_storm_tempz100m.py \
  plot_storm_wvelz40m.py \
  plot_storm_wvelz70m.py \
  plot_storm_wvelz100m.py \
  plot_storm_crs_sn_temp.py \
  plot_storm_crs_trk_temp.py \
  plot_storm_crs_we_temp.py \
  )

nscripts=${#figScriptAll[*]}
TRACKON="yes"

for((i=0;i<${nscripts};i++)); do
  fhhh="f${FHR3}"
  if [ ${ocean_model,,} = mom6 ] && [ ${figScriptAll[$i]: 11:5} = wvelz ]; then
     echo "Vertical velocity plots for MOM6 are not being produced yet."
  else
     echo ${figScriptAll[$i]} ${fhhh}
     echo "time ${DRIVEROCEAN} $stormModel $STORM $STORMID $YMDH $TRACKON ${figScriptAll[$i]} $fhhh \
	> ${WORKgraph}/$STORM$STORMID.$YMDH.${figScriptAll[$i]%.*}.${fhhh}.log 2>&1" >> $cmdfile
  fi
done

chmod u+x ./$cmdfile

${APRUNC} ${MPISERIAL} -m ./$cmdfile
export err=$?; err_chk

IFHR=$(($IFHR + 1))
FHR=$(($FHR + $NOUTHRS))
FHR3=$( printf "%03d" "$FHR" )

done #End loop for forecast hours

fi #[ ${run_ocean} = yes ]; then

#==============================================================================
# For the wave figures
#==============================================================================

if [ ${run_wave} = yes ]; then

cd ${WORKgraph}

# Wait for wave_post and product output
atcfFile=${CDNOSCRUB}/${SUBEXPT}/${stormid}.${YMDH}.${RUN}.trak.atcfunix.all
n=1
while [ $n -le 600 ]; do
  if [ -f ${COMhafs}/${stormid}.${YMDH}.${RUN}.ww3.grb2.idx ] && [ -f ${atcfFile} ]; then
    echo "${COMhafs}/${stormid}.${YMDH}.${RUN}.ww3.grb2.idx and ${atcfFile} exist"
    sleep 1s
    break
  else
    echo "${COMhafs}/${stormid}.${YMDH}.${RUN}.ww3.grb2.idx or ${atcfFile} not ready, sleep 60s"
    sleep 60s
  fi
  n=$(( n+1 ))
done

#Generate the cmdfile
cmdfile='cmdfile_wave'
rm -f $cmdfile
touch $cmdfile

fhhhAll=$(seq -f "f%03g" 0 $NOUTHRS $NHRS)
# Loop for forecast hours
for fhhh in ${fhhhAll}; do

for stormDomain in parent storm; do

if [ ${stormDomain} = "parent" ]; then
  figScriptAll=( \
    plot_wave_hs.py \
    plot_wave_tm.py \
    plot_wave_tp.py \
    )
  levAll=( \
    1003 \
    1003 \
    1003 \
    )
elif [ ${stormDomain} = "storm" ]; then
  figScriptAll=( \
    plot_wave_hs.py \
    plot_wave_tm.py \
    plot_wave_tp.py \
    )
  levAll=( \
    1003 \
    1003 \
    1003 \
    )
fi

nscripts=${#figScriptAll[*]}

for((i=0;i<${nscripts};i++)); do
  echo ${figScriptAll[$i]} ${levAll[$i]}
  echo "time ${DRIVERATMOS} $stormModel $STORM $STORMID $YMDH $stormDomain ${figScriptAll[$i]} ${levAll[$i]} $fhhh > ${WORKgraph}/$STORM$STORMID.$YMDH.${stormDomain}.${figScriptAll[$i]%.*}.${fhhh}.log 2>&1" >> $cmdfile
done

done

done

chmod u+x ./$cmdfile
${APRUNC} ${MPISERIAL} -m ./$cmdfile
export err=$?; err_chk

date

fi # if [ ${run_wave} = yes ]; then

#==============================================================================

date

echo "graphics job done"

exit
