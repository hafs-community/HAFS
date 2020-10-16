#!/bin/sh

set -xe

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export FIXcrtm=${FIXcrtm:-${FIXhafs}/hwrf-crtm-2.2.6}
export COMgfs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}

export RUN_GSI=${RUN_GSI:-NO}
export RUN_GSI_VR=${RUN_GSI_VR:-NO}

#export hybrid_3denvar_gdas=${hybrid_3denvar_gdas:-yes}
export hybrid_3denvar_gdas=no

TOTAL_TASKS=${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`

NDATE=${NDATE:-ndate}
NCP='/bin/cp'
NLN='ln -sf'

CDATEprior=`${NDATE} -6 $CDATE`
yrprior=`echo ${CDATEprior} | cut -c1-4`
mnprior=`echo ${CDATEprior} | cut -c5-6`
dyprior=`echo ${CDATEprior} | cut -c7-8`
hhprior=`echo ${CDATEprior} | cut -c9-10`
PDYprior=`echo ${CDATEprior} | cut -c1-8`

export COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
export WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

if [ ! ${RUN_GSI_VR} = "YES" ]; then
  echo "RUN_GSI_VR: ${RUN_GSI_VR} is not YES"
  echo "Do nothing. Exiting"
  exit
fi

if [ ! -s ${COMhafsprior}/storm1.holdvars.txt ] && [ ! -s ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  echo "Prior cycle does not exist. No need to run gsi_vr for the first cycle."
  echo "Do nothing. Exiting"
  exit
fi

#---------------------------------------------- 
# Sample the synthetic data around the storm from the previous cycle's model forecast
# And relcoate the synthetic data according to the observed (tcvitals) storm location 
#---------------------------------------------- 

# Copy the input first guess files
export RESTARTinp=${RESTARTinp:-${COMhafsprior}/RESTART}

${NCP} ${RESTARTinp}/oro_data.nc ./fv3_oro_data
${NCP} ${RESTARTinp}/atmos_static.nc ./fv3_atmos_static
${NCP} ${RESTARTinp}/grid_spec.nc ./fv3_grid_spec

${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.coupler.res ./coupler.res
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.nc ./fv3_akbk
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.sfc_data.nc ./fv3_sfcdata
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv3_dynvars
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv3_tracer

${NLN} ${COMhafsprior}/${STORM,,}${STORMID,,}.${CDATEprior}.trak.hafs.atcfunix.all ./hafs.atcfunix
# Find and sort active storms for this cycle from known tcvitals file
# This can potentially provide multiple tcvital messages to the tracker, 
# so that it can track multiple storms simultaneously.
# *** Currently, tcutil_multistorm_sort.py searches the tcvitals files
# specified in the script. Need to modify it to be able to deal with storm
# message files/dirs, as well as passing in tcvitals files.
${USHhafs}/tcutil_multistorm_sort.py ${CDATE} | cut -c1-96 > allvit

${USHhafs}/hafs_opptcv_format.py --ncep_trkr_filename ./hafs.atcfunix --tcv_filename ./allvit --output_filename ./synobs_vr.info

cat synobs_vr.info

# Proceed with the GSI based vortex relocation if synobs_vr.info is non-empty.
if [ -s ./synobs_vr.info ]; then

# Link the neede files
${NLN} ${PARMgsi}/prepobs_prep.bufrtable ./
${NLN} ${PARMgsi}/bufrinfo.json ./

# Prepare the namelist
${NCP} ${PARMgsi}/obs-preproc.input.tmp ./
analdate="${yr}-${mn}-${dy}_${cyc}:00:00"

sed -e "s/_analdate_/${analdate}/g" \
    -e "s/_grid_ratio_/${refine_ratio:-4}/g" \
    obs-preproc.input.tmp > obs-preproc.input

#-------------------------------------------------------------------
# Link and run the executable
#-------------------------------------------------------------------
OBSPREPROCEXEC=${OBSPREPROCEXEC:-${EXEChafs}/hafs_obs_preproc.x}
${NCP} -p ${OBSPREPROCEXEC} ./hafs_obs_preproc.x

${APRUNS} ./hafs_obs_preproc.x 1> ./hafs_obs_preproc.out 2>&1

#---------------------------------------------- 
# Run GSI to assimilate the relocated synthetic data to produce the analysis 
#---------------------------------------------- 

#---------------------------------------------- 
# Link all the necessary fix files
#---------------------------------------------- 
${NLN} ${PARMgsi}/anavinfo_hafs_L${LEVS:-65} ./anavinfo
${NLN} ${PARMgsi}/hwrf_convinfo.txt_vr ./convinfo
${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats
${NLN} ${PARMgsi}/hwrf_nam_errtable.r3dv_vr ./errtable

# Link GFS/GDAS input and observation files
${NLN} ./synobs_vr.prepbufr ./prepbufr

#---------------------------------------------- 
# Prepare gsiparm.anl
#---------------------------------------------- 
#${NCP} ${PARMgsi}/gsiparm.anl.tmp_vr ./gsiparm.anl.tmp
${NCP} ${PARMgsi}/gsiparm.anl.tmp ./gsiparm.anl.tmp

sed -e "s/_L_HYB_ENS_/${L_HYB_ENS:-.false.}/g" \
    -e "s/_N_ENS_/${N_ENS:-80}/g" \
    -e "s/_GRID_RATIO_FV3_REGIONAL_/${refine_ratio:-4}/g" \
    gsiparm.anl.tmp > gsiparm.anl

#-------------------------------------------------------------------
# Link the executable and run the analysis
#-------------------------------------------------------------------
ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_gsi.x}
${NCP} -p ${ANALYSISEXEC} ./hafs_gsi.x

${APRUNC} ./hafs_gsi.x 1>stdout 2>&1
cat stdout

fi # end if [ -s ./synobs_vr.info ]; then

#-------------------------------------------------------------------
# Deliver files to COM
#-------------------------------------------------------------------
export RESTARTout=${RESTARTout:-${COMhafs}/RESTART_analysis_vr}
mkdir -p ${RESTARTout}

${NCP} ./fv3_oro_data ${RESTARTout}/oro_data.nc
${NCP} ./fv3_atmos_static ${RESTARTout}/atmos_static.nc
${NCP} ./fv3_grid_spec ${RESTARTout}/grid_spec.nc

${NCP} ./coupler.res ${RESTARTout}/${PDY}.${cyc}0000.coupler.res
${NCP} ./fv3_akbk ${RESTARTout}/${PDY}.${cyc}0000.fv_core.res.nc
${NCP} ./fv3_sfcdata ${RESTARTout}/${PDY}.${cyc}0000.sfc_data.nc
${NCP} ./fv3_srfwnd ${RESTARTout}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc
${NCP} ./fv3_dynvars ${RESTARTout}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
${NCP} ./fv3_tracer ${RESTARTout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc

exit

