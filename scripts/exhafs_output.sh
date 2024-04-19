#!/bin/sh
################################################################################
# Script Name: exhafs_output.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script delivers some HAFS output files, sends a notification email (if
#   desired), and generate the HAFS swath grib2 files.
################################################################################
set -x -o pipefail

export MP_LABELIO=yes

CDATE=${CDATE:-${YMDH}}

out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}
output_grid=${output_grid:-rotated_latlon}

EMAIL_SDM=${EMAIL_SDM:-NO}
SENDCOM=${SENDCOM:-YES}

DATA=${DATA:-${WORKhafs}/output}
mkdir -p ${DATA}

cd ${DATA}

# Email SDM about AFOS if run by NCO on WCOSS2
afosfile=${COMhafs}/${out_prefix}.${RUN}.afos
if [ "${EMAIL_SDM^^}" = "YES" ] && [ -s ${afosfile} ]; then
  MAILFROM=${MAILFROM:-"nco.spa@noaa.gov"}
  MAILTO=${MAILTO:-"sdm@noaa.gov"}
  subject="${cyc}Z ${RUN^^} Output for ${basinname:-} Tropical System ${STORM} (${STORMID^^})"
  mail.py -s "${subject}" -v "${MAILTO}" < ${afosfile}
  export err=$?; err_chk
fi

# Deliver track file to NOSCRUB if not run by NCO
if [ ${RUN_ENVIR^^} != "NCO" ]; then
  trk_atcfunix=${out_prefix}.${RUN}.trak.atcfunix
  all_atcfunix=${out_prefix}.${RUN}.trak.atcfunix.all
  mkdir -p ${CDNOSCRUB:?}/${SUBEXPT:?}
  if [ -s ${COMhafs}/${all_atcfunix} ]; then
    ${NCP} -p ${COMhafs}/${all_atcfunix} ${CDNOSCRUB}/${SUBEXPT}/.
  fi
  if [ -s ${COMhafs}/${trk_atcfunix} ] && [ "${STORMID:0:2}" != "00" ]; then
    ${NCP} -p ${COMhafs}/${trk_atcfunix} ${CDNOSCRUB}/${SUBEXPT}/.
  fi
  if [ -s ${COMhafs}/${out_prefix}.${RUN}.trak.patcf ]; then
    ${NCP} -p ${COMhafs}/${out_prefix}.${RUN}.trak.patcf ${CDNOSCRUB}/${SUBEXPT}/.
  fi
fi

#===============================================================================
# Produce swath grib2 products for peak gust, precipitation rate from
# accumulated total precipitation, peak 10 m wind, convective precipitation
# rate from accumulated convective precipitation, peak updraft, peak downdraft,
# updraft helicity in the lower troposphere (2000-5000 m), updraft helicity
# near the surface (0-3000 m).
# From Lew.Gramer@noaa.gov, 2023-01-19

# Currently swath grib2 products are based on the parent domain grib2 output only
if [ ${swathpost:-.true.} = .true. ]; then

DATA_SWATH=${DATA}/swath
rm -rf ${DATA_SWATH}
mkdir -p ${DATA_SWATH}
cd ${DATA_SWATH}

# currently only deal with parent domain)
gridstr=$(echo ${out_gridnames} | cut -d, -f 1)

swath_grb2file=${out_prefix}.${RUN}.${gridstr}.swath.grb2
swath_grb2indx=${out_prefix}.${RUN}.${gridstr}.swath.grb2.idx
rm -f ${swath_grb2file} ${swath_grb2indx}
# temporary grib2 files
GUSTF="./gust.grb2"
APCPF="./apcp.grb2"
PRATEF="./prate.grb2"
WINDMAXF="./windmax.grb2"
ACPCPF="./acpcp.grb2"
CPRATF="./cprat.grb2"
MAXUVVF="./maxuvv.grb2"
MAXDVVF="./maxdvv.grb2"
UPHLSFCF="./uphlsfc.grb2"
UPHLTROF="./uphltro.grb2"
TMPFILE="./tmpfile.grb2"
TMPFILE2="./tmpfile2.grb2"
rm -f ${GUSTF} ${APCPF} ${PRATEF} ${WINDMAXF} ${ACPCPF} ${CPRATF} ${MAXUVVF} ${MAXDVVF} ${UPHLSFCF} ${UPHLTROF}

# Deal with forecast F000
DHR=$NOUTHRS
IFHR=0
FHR=0
FHR3=$(printf "%03d" "$FHR")
grb2file=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.grb2
swath_grb2fhhh=${out_prefix}.${RUN}.${gridstr}.swath.f${FHR3}.grb2

# Replace UNKNOWN values in GRB2 messages of interest with 0; convert PCP accs to PRATEs
${WGRIB2} ${COMhafs}/${grb2file} -match '(:GUST:)'           	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:GUST:10-10 m above ground:0-$((IFHR*DHR)) hour max fcst::Wind Speed (Gust) [m/s]:" \
    -grib_out ${GUSTF}
export err=$?; err_chk
${WGRIB2} ${COMhafs}/${grb2file} | grep ":APCP:" | head -n 1 | ${WGRIB2} -i ${COMhafs}/${grb2file} 	-rpn "0:swap:merge" -grib_out ${APCPF}
export err=$?; err_chk
${WGRIB2} ${APCPF} -match '(:APCP:)'            	-rpn "0:*" \
    -set_metadata_str "0:0:d=+0hr:PRATE:atmos col:0-$((IFHR*DHR)) hour ave fcst::Precipitation Rate [kg/m^2/s]:" \
    -grib_out ${PRATEF}
export err=$?; err_chk
${WGRIB2} ${COMhafs}/${grb2file} -match '(:WIND.*max)'       	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:WIND:10-10 m above ground:0-$((IFHR*DHR)) hour max fcst::Wind Speed [m/s]:" \
    -grib_out ${WINDMAXF}
export err=$?; err_chk
${WGRIB2} ${COMhafs}/${grb2file} | grep ":ACPCP:" | head -n 1 | ${WGRIB2} -i ${COMhafs}/${grb2file} -rpn "0:swap:merge" -grib_out ${ACPCPF}
export err=$?; err_chk
${WGRIB2} ${ACPCPF} -match '(:ACPCP:)'            	-rpn "0:*" \
    -set_metadata_str "0:0:d=+0hr:CPRAT:atmos col:0-$((IFHR*DHR)) hour ave fcst::Convective Precipitation Rate [kg/m^2/s]:" \
    -grib_out ${CPRATF}
export err=$?; err_chk
${WGRIB2} ${COMhafs}/${grb2file} -match '(:MAXUVV:)'       	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:DZDT:0-400 mb above ground:0-$((IFHR*DHR)) hour max fcst::Vertical Velocity (Geometric) [m/s]:" \
    -grib_out ${MAXUVVF}
export err=$?; err_chk
${WGRIB2} ${COMhafs}/${grb2file} -match '(:MAXDVV:)'       	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:DZDT:0-400 mb above ground:0-$((IFHR*DHR)) hour min fcst::Vertical Velocity (Geometric) [m/s]:" \
    -grib_out ${MAXDVVF}
export err=$?; err_chk
${WGRIB2} ${COMhafs}/${grb2file} -match '(:MXUPHL:5000-2000 m)' 	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:UPHL:5000-2000 m above ground:0-0 hour max fcst::Updraft Helicity [m^2/s^2]:" \
    -grib_out ${UPHLTROF}
export err=$?; err_chk
${WGRIB2} ${COMhafs}/${grb2file} -match '(:MXUPHL:3000-0 m)'   	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:UPHL:3000-0 m above ground:0-0 hour max fcst::Updraft Helicity [m^2/s^2]:" \
    -grib_out ${UPHLSFCF}
export err=$?; err_chk

#cat ${GUSTF} ${PRATEF} ${WINDMAXF} ${CPRATF} ${MAXUVVF} ${MAXDVVF} ${UPHLTROF} ${UPHLSFCF} >> ${swath_grb2fhhh}

# Loop for other forecast hours
IFHR=$(($IFHR + 1))
FHR=$(($FHR + $NOUTHRS))
FHR3=$(printf "%03d" "$FHR")

while [ $FHR -le $NHRS ]; do
  grb2file=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.grb2
  swath_grb2fhhh=${out_prefix}.${RUN}.${gridstr}.swath.f${FHR3}.grb2
  # MAXIMIZE, MINIMIZE, or ACCUMULATE current 3-hourly values with
  # persistent intermediate swath files (e.g., GUSTF) for each variable.
  # GUST - peak wind gust
  # Replace UNKNOWN values with 0, change variable name, metadata
  ${WGRIB2} ${COMhafs}/${grb2file} -match '(:GUST:)'         	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:GUST:10-10 m above ground:0-$((IFHR*DHR)) hour max fcst::Wind Speed (Gust) [m/s]:" \
      -grib_out ${TMPFILE}
  export err=$?; err_chk
  # Max with previous swath intermediate file
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${GUSTF} -rpn "rcl_1:max" -grib_out ${TMPFILE2}
  export err=$?; err_chk
  rm ${TMPFILE}
  mv ${TMPFILE2} ${GUSTF}
  # PRATE - accumulated total precipitation
  ${WGRIB2} ${COMhafs}/${grb2file} | grep ":APCP:" | head -n 1 | ${WGRIB2} -i ${COMhafs}/${grb2file} -rpn "0:swap:merge" -grib_out ${TMPFILE}
  export err=$?; err_chk
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${APCPF} -rpn "rcl_1:+" -grib_out ${TMPFILE2}
  export err=$?; err_chk
  rm ${TMPFILE}
  mv ${TMPFILE2} ${APCPF}
  ${WGRIB2} ${APCPF} -match '(:APCP:)'            	-rpn "$((IFHR*DHR*3600)):/" \
      -set_metadata_str "0:0:d=+0hr:PRATE:atmos col:0-$((IFHR*DHR)) hour ave fcst::Precipitation Rate [kg/m^2/s]:" \
      -grib_out ${PRATEF}
  export err=$?; err_chk
  # WINDmax - peak 10 m wind
  ${WGRIB2} ${COMhafs}/${grb2file} -match '(:WIND.*max)'     	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:WIND:10-10 m above ground:0-$((IFHR*DHR)) hour max fcst::Wind Speed [m/s]:" \
      -grib_out ${TMPFILE}
  export err=$?; err_chk
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${WINDMAXF} -rpn "rcl_1:max" -grib_out ${TMPFILE2}
  export err=$?; err_chk
  rm ${TMPFILE}
  mv ${TMPFILE2} ${WINDMAXF}
  # CPRAT - accumulated convective precipitation
  ${WGRIB2} ${COMhafs}/${grb2file} | grep ":ACPCP:" | head -n 1 | ${WGRIB2} -i ${COMhafs}/${grb2file} -rpn "0:swap:merge" -grib_out ${TMPFILE}
  export err=$?; err_chk
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${ACPCPF} -rpn "rcl_1:+" -grib_out ${TMPFILE2}
  export err=$?; err_chk
  rm ${TMPFILE}
  mv ${TMPFILE2} ${ACPCPF}
  ${WGRIB2} ${ACPCPF} -match '(:ACPCP:)'            	-rpn "$((IFHR*DHR*3600)):/" \
      -set_metadata_str "0:0:d=+0hr:CPRAT:atmos col:0-$((IFHR*DHR)) hour ave fcst::Convective Precipitation Rate [kg/m^2/s]:" \
      -grib_out ${CPRATF}
  export err=$?; err_chk
  # DZDTmax - peak updrafts
  ${WGRIB2} ${COMhafs}/${grb2file} -match '(:MAXUVV:)'       	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:DZDT:0-400 mb above ground:0-$((IFHR*DHR)) hour max fcst::Vertical Velocity (Geometric) [m/s]:" \
      -grib_out ${TMPFILE}
  export err=$?; err_chk
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${MAXUVVF} -rpn "rcl_1:max" -grib_out ${TMPFILE2}
  export err=$?; err_chk
  rm ${TMPFILE}
  mv ${TMPFILE2} ${MAXUVVF}
  # DZDTmin - peak downdrafts
  ${WGRIB2} ${COMhafs}/${grb2file} -match '(:MAXDVV:)'       	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:DZDT:0-400 mb above ground:0-$((IFHR*DHR)) hour min fcst::Vertical Velocity (Geometric) [m/s]:" \
      -grib_out ${TMPFILE}
  export err=$?; err_chk
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${MAXDVVF} -rpn "rcl_1:min" -grib_out ${TMPFILE2}
  export err=$?; err_chk
  rm ${TMPFILE}
  mv ${TMPFILE2} ${MAXDVVF}
  # UPHLtro - updraft helicity in the lower troposphere
  ${WGRIB2} ${COMhafs}/${grb2file} -match '(:MXUPHL:5000-2000 m)' 	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:UPHL:5000-2000 m above ground:$(((IFHR-1)*DHR))-$((IFHR*DHR)) hour max fcst::Updraft Helicity [m^2/s^2]:" \
      -grib_out ${TMPFILE}
  export err=$?; err_chk
  mv ${TMPFILE} ${UPHLTROF}
  # UPHLsfc - updraft helicity in the surface layer
  ${WGRIB2} ${COMhafs}/${grb2file} -match '(:MXUPHL:3000-0 m)'   	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:UPHL:3000-0 m above ground:$(((IFHR-1)*DHR))-$((IFHR*DHR)) hour max fcst::Updraft Helicity [m^2/s^2]:" \
      -grib_out ${TMPFILE}
  export err=$?; err_chk
  mv ${TMPFILE} ${UPHLSFCF}
 #cat ${GUSTF} ${PRATEF} ${WINDMAXF} ${CPRATF} ${MAXUVVF} ${MAXDVVF} ${UPHLTROF} ${UPHLSFCF} > ${swath_grb2fhhh}
  cat ${GUSTF} ${PRATEF} ${WINDMAXF} ${CPRATF} ${MAXUVVF} ${MAXDVVF} ${UPHLTROF} ${UPHLSFCF} >> ${swath_grb2file}

  # next forecast hour
  IFHR=$(($IFHR + 1))
  FHR=$(($FHR + $NOUTHRS))
  FHR3=$(printf "%03d" "$FHR")
done

# Compress swath grib2 file to save disk space
${WGRIB2} ${swath_grb2file} -set_grib_type c2 -grib_out ${swath_grb2file}.c2
export err=$?; err_chk
mv ${swath_grb2file}.c2 ${swath_grb2file}
# Generate the index file for the swath grib2 file
${WGRIB2} -s ${swath_grb2file} > ${swath_grb2indx}
export err=$?; err_chk

# Deliver to COMhafs
if [ $SENDCOM = YES ]; then
  mkdir -p ${COMhafs}
  mv ${swath_grb2file} ${COMhafs}/
  if [ "${SENDDBN^^}" = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_GB2 $job ${COMhafs}/${swath_grb2file}
  fi
  mv ${swath_grb2indx} ${COMhafs}/
  if [ "${SENDDBN^^}" = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_GB2_WIDX $job ${COMhafs}/${swath_grb2indx}
  fi
fi

fi

#===============================================================================

cd ${DATA}

