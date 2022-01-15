#!/bin/sh

set -xe

FGAT_MODEL=${FGAT_MODEL:-gfs}
FGAT_HR=${FGAT_HR:-00}

export merge_method=${merge_method:-vortexreplace}
export RESTARTsrc=${RESTARTsrc:-"${COMhafs}/RESTART_analysis"}
export RESTARTdst=${RESTARTdst:-"${COMhafs}/RESTART_init"}
export RESTARTmrg=${RESTARTmrg:-"${COMhafs}/RESTART_analysis_merge"}

TOTAL_TASKS=${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

# Utilities
NDATE=${NDATE:-ndate}
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
export DATOOL=${DATOOL:-${EXEChafs}/hafs_datool.x}

PDY=`echo $CDATE | cut -c1-8`
cyc=`echo $CDATE | cut -c9-10`
yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`
hh=`echo $CDATE | cut -c9-10`

mkdir -p ${RESTARTmrg}
${NCP} -rp ${RESTARTdst}/* ${RESTARTmrg}/

if [ -d ${RESTARTsrc} ] || [ -L ${RESTARTsrc} ] ; then

if [ ${FGAT_HR} = 03 ]; then
  tcvital=${WORKhafs}/tm03vit
elif [ ${FGAT_HR} = 06 ]; then
  tcvital=${WORKhafs}/tmpvit
elif [ ${FGAT_HR} = 09 ]; then
  tcvital=${WORKhafs}/tp03vit
else
  tcvital=${WORKhafs}/tmpvit
fi
if [ ${merge_method} = vortexreplace ]; then
  MERGE_CMD="${DATOOL} vortexreplace --tcvital=${tcvital} --vortexradius=800:900"
elif [ ${merge_method} = mergedomain ]; then
  MERGE_CMD="${DATOOL} remap"
else
  echo "Error: unsupported merge_method: ${merge_method}"
  exit 1
fi
rm -f cmdfile_datool_merge
#for var in fv_core.res.tile1 fv_tracer.res.tile1 fv_srf_wnd.res.tile1 sfc_data phy_data; 
for var in fv_core.res.tile1 fv_tracer.res.tile1 fv_srf_wnd.res.tile1 sfc_data; 
do
  cat >> cmdfile_datool_merge << EOF
  time ${MERGE_CMD} \
    --in_grid=${RESTARTsrc}/grid_spec.nc \
    --out_grid=${RESTARTmrg}/grid_spec.nc \
    --in_file=${RESTARTsrc}/${PDY}.${cyc}0000.${var}.nc \
    --out_file=${RESTARTmrg}/${PDY}.${cyc}0000.${var}.nc \
    > datool.${var}.log
EOF
done

chmod +x cmdfile_datool_merge
${APRUNC} ${MPISERIAL} -m cmdfile_datool_merge
cat datool.*.log

else

echo "RESTARTsrc: ${RESTARTsrc} does not exist"
echo "RESTARTmrg is the same as RESTARTdst"

fi

exit
