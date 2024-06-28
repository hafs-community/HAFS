#!/bin/sh
################################################################################
# Script Name: exhafs_atm_unpost.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script cleans up the HAFS atmopheric, wave and oceanic post-processing
#   products and files in com and intercom.
################################################################################
set -x -o pipefail

CDATE=${CDATE:-${YMDH}}

out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}
DATA=${DATA:-${WORKhafs}/unpost}

mkdir -p ${DATA}
cd ${DATA}

if [ "${POST_CLEANUP^^}" = "YES" ]; then

# Remove atm_post com and intercom output
rm -f ${COMhafs}/${out_prefix}.${RUN}.*.atm.f???.grb2*
rm -f ${COMhafs}/${out_prefix}.${RUN}.*.sat.f???.grb2*
rm -f ${COMhafs}/${out_prefix}.${RUN}.trak.patcf
rm -f ${WORKhafs}/intercom/post/${out_prefix}.${RUN}.*.trk.f???.grb2*
rm -f ${WORKhafs}/intercom/post/post*f???

# Remove ocn_post com output
if [ ${run_ocean} = yes ]; then
  rm -f ${COMhafs}/${out_prefix}.${RUN}.hycom.*.f???.nc
  rm -f ${COMhafs}/${out_prefix}.${RUN}.mom6.*.f???.nc
  rm -f ${WORKhafs}/intercom/ocn_post/ocnpost*f???
fi

# Remove wav_post com output
if [ ${run_wave} = yes ]; then
  rm -f ${COMhafs}/${out_prefix}.${RUN}.ww3.grb2*
  rm -f ${COMhafs}/${out_prefix}.${RUN}.ww3*.tar
  rm -f ${COMhafs}/${out_prefix}.${RUN}.out_???.ww3
  rm -f ${COMhafs}/${out_prefix}.${RUN}.ww3*.nc
fi

# Remove product com output
atcfunix=${COMhafs}/${out_prefix}.${RUN}.trak.atcfunix
atcfglobal=${COMhafs}/global/tracks.atcfunix.${CDATE:2:2}
if [ -s ${atcfunix} ] && [ -s ${atcfglobal} ]; then
  atcftmp=$(head -n 1 ${atcfunix} | cut -c1-29)
  if [[ "${atcftmp}" = *"${CDATE},"*"${RUN}," ]]; then
    sed -i -e "/${atcftmp}/d" ${atcfglobal}
  fi
fi
atcfncep=${COMhafs}/atcf/${hafsbasin2,,}${STORMID:0:2}${CDATE:0:4}/ncep_a${hafsbasin2,,}${STORMID:0:2}${CDATE:0:4}.dat
rm -f ${atcfncep}
rm -f ${COMhafs}/${out_prefix}.${RUN}.*trak.atcfunix*
rm -f ${COMhafs}/${out_prefix}.${RUN}.stats.tpc
rm -f ${COMhafs}/${out_prefix}.${RUN}.grib.stats.short
rm -f ${COMhafs}/${out_prefix}.${RUN}.afos

# Remove output com output
rm -f ${COMhafs}/${out_prefix}.${RUN}.*.swath.grb2*

# Remove gempak com output
rm -f ${COMhafs}/gempak/${STORMID,,}/${RUN}*_${STORMID,,}
rm -f ${COMhafs}/gempak/${STORMID,,}/meta/${RUN}*_${STORMID,,}*
rm -f ${WORKhafs}/intercom/gempak/*.done

fi # if [ "${POST_CLEANUP^^}" = "YES" ]; then

cd ${DATA}
