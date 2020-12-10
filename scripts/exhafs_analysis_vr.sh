#!/bin/sh

set -xe

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export FIXcrtm=${FIXcrtm:-${FIXhafs}/hwrf-crtm-2.2.6}
export COMgfs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}

export RUN_GSI=${RUN_GSI:-NO}
export RUN_GSI_VR=${RUN_GSI_VR:-NO}
export HAFS_ENS=${HAFS_ENS:-NO}
export FGAT_OPT=${FGAT_OPT:-NO}

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

if [ ${FGAT_OPT} = YES ]; then 
 CDATE_pre=`${NDATE} -6 $CDATE`
 CDATE_aft="$CDATE"
 if [ ${FGAT_ID} = 03 ]; then
  CDATE_tmp=`${NDATE} -3 $CDATE`
 fi
 if [ ${FGAT_ID} = 09 ]; then
  CDATE_tmp=`${NDATE} +3 $CDATE`
 fi
 cyc_fgat=`echo ${CDATE_tmp} | cut -c9-10`
fi

export COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
export WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}

if [ ! ${RUN_GSI_VR} = "YES" ]; then
  echo "RUN_GSI_VR: ${RUN_GSI_VR} is not YES"
  echo "Do nothing. Exiting"
  exit
fi

if [ "${HAFS_ENS}" = YES ]; then
 if [ ! -s ${COMhafsprior}/storm1.holdvars.txt ] && [ ! -s ${COMhafsprior}/RESTART_ENS/member${VR_ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  echo "Prior cycle does not exist. No need to run gsi_vr for the first cycle."
  echo "Do nothing. Exiting"
  exit
 fi
else
 if [ ! -s ${COMhafsprior}/storm1.holdvars.txt ] && [ ! -s ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  echo "Prior cycle does not exist. No need to run gsi_vr for the first cycle."
  echo "Do nothing. Exiting"
  exit
 fi
 if [ ${FGAT_OPT} = YES ]; then
  if [ ! -s ${COMhafsprior}/storm1.holdvars.txt ] && [ ! -s ${COMhafsprior}/RESTART/${PDY}.${cyc_fgat}0000.fv_core.res.tile1.nc ]; then
   echo "Prior cycle FGAT files does not exist. No need to run gsi_vr for the first cycle."
   echo "Do nothing. Exiting"
   exit
  fi
 fi
fi

#---------------------------------------------- 
# Sample the synthetic data around the storm from the previous cycle's model forecast
# And relcoate the synthetic data according to the observed (tcvitals) storm location 
#---------------------------------------------- 

# Copy the input first guess files
if [ "${HAFS_ENS}" = YES ]; then
  export RESTARTinp=${RESTARTinp:-${COMhafsprior}/RESTART_ENS/member${VR_ENSID}}
  ${NCP} ${RESTARTinp}/oro_data.nc ./fv3_oro_data
  ${NCP} ${RESTARTinp}/atmos_static.nc ./fv3_atmos_static
  ${NCP} ${RESTARTinp}/grid_spec.nc ./fv3_grid_spec
  ${NCP} ${RESTARTinp}/coupler.res ./coupler.res
  ${NCP} ${RESTARTinp}/fv_core.res.nc ./fv3_akbk
  ${NCP} ${RESTARTinp}/sfc_data.nc ./fv3_sfcdata
  ${NCP} ${RESTARTinp}/fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
  ${NCP} ${RESTARTinp}/fv_core.res.tile1.nc ./fv3_dynvars
  ${NCP} ${RESTARTinp}/fv_tracer.res.tile1.nc ./fv3_tracer
  grep "HAFS, 006" ${COMhafsprior}/${STORM,,}${STORMID,,}.${CDATEprior}.trak.hafs.atcfunix.all > ./hafs.atcfunix
else
  export RESTARTinp=${RESTARTinp:-${COMhafsprior}/RESTART}

  ${NCP} ${RESTARTinp}/oro_data.nc ./fv3_oro_data
  ${NCP} ${RESTARTinp}/atmos_static.nc ./fv3_atmos_static
  ${NCP} ${RESTARTinp}/grid_spec.nc ./fv3_grid_spec
 if [ ${FGAT_OPT} = YES ]; then #OU added for FGAT
  ${NCP} ${RESTARTinp}/${PDY}.${cyc_fgat}0000.coupler.res ./coupler.res
  ${NCP} ${RESTARTinp}/${PDY}.${cyc_fgat}0000.fv_core.res.nc ./fv3_akbk
  ${NCP} ${RESTARTinp}/${PDY}.${cyc_fgat}0000.sfc_data.nc ./fv3_sfcdata
  ${NCP} ${RESTARTinp}/${PDY}.${cyc_fgat}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
  ${NCP} ${RESTARTinp}/${PDY}.${cyc_fgat}0000.fv_core.res.tile1.nc ./fv3_dynvars
  ${NCP} ${RESTARTinp}/${PDY}.${cyc_fgat}0000.fv_tracer.res.tile1.nc ./fv3_tracer
  grep "HAFS, 0${FGAT_ID}" ${COMhafsprior}/${STORM,,}${STORMID,,}.${CDATEprior}.trak.hafs.atcfunix.all > ./hafs.atcfunix
 else
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.coupler.res ./coupler.res
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.nc ./fv3_akbk
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.sfc_data.nc ./fv3_sfcdata
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv3_dynvars
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv3_tracer

# Extract the 6-hr forecast atcf records from the prior cycle
  ${NLN} ${COMhafsprior}/${STORM,,}${STORMID,,}.${CDATEprior}.trak.hafs.atcfunix.all ./hafs.atcfunix_prior
  grep ", HAFS, 006," ./hafs.atcfunix_prior | grep ",  34, NEQ," > ./hafs.atcfunix
 fi
fi
cat ./hafs.atcfunix

# Find and sort active storms for this cycle from known tcvitals file
# This can potentially provide multiple tcvital messages to the tracker, 
# so that it can track multiple storms simultaneously.
# *** Currently, tcutil_multistorm_sort.py searches the tcvitals files
# specified in the script. Need to modify it to be able to deal with storm
# message files/dirs, as well as passing in tcvitals files.
if [ ${FGAT_OPT} = YES ]; then #OU added for FGAT
 ${USHhafs}/tcutil_multistorm_sort.py ${CDATE_pre} | cut -c1-96 > allvit_pre
 ${USHhafs}/tcutil_multistorm_sort.py ${CDATE_aft} | cut -c1-96 > allvit_aft
 if [ ${FGAT_ID} = 03 ]; then
  ${USHhafs}/tcutil_interpolate.py allvit_pre allvit_aft 0 | cut -c1-96 > allvit
 else
  ${USHhafs}/tcutil_interpolate.py allvit_pre allvit_aft 1 | cut -c1-96 > allvit
 fi
else
 ${USHhafs}/tcutil_multistorm_sort.py ${CDATE} | cut -c1-96 > allvit
fi
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

# Diagnostic files
# if requested, link GSI diagnostic file directories for use later
if [ "${HAFS_ENS}" = YES ]; then
 if [ -d ${COMhafs}/analysis_vr_diags_ens/ ]; then
   echo "${COMhafs}/analysis_vr_diags_ens/ exists"
 else
   mkdir ${COMhafs}/analysis_vr_diags_ens/
 fi
 export DIAG_DIR=${DIAG_DIR:-${COMhafs}/analysis_vr_diags_ens/member${VR_ENSID}}
else
 if [ "FGAT_OPT" = YES ]; then
  export DIAG_DIR=${DIAG_DIR:-${COMhafs}/analysis_vr_diags_${FGAT_ID}}
 else
  export DIAG_DIR=${DIAG_DIR:-${COMhafs}/analysis_vr_diags}
 fi
fi
if [ ${GENDIAG:-YES} = "YES" ] ; then
   if [ ${lrun_subdirs:-.true.} = ".true." ] ; then
      if [ -d $DIAG_DIR ]; then
      rm -rf $DIAG_DIR
      fi
      npe_m1="$(($TOTAL_TASKS-1))"
      for pe in $(seq 0 1 $npe_m1); do
        pedir="dir."$(printf %04i $pe)
        mkdir -p $DIAG_DIR/$pedir
        $NLN $DIAG_DIR/$pedir $pedir
      done
   else
      echo "FATAL ERROR: lrun_subdirs must be true. lrun_subdirs=$lrun_subdirs"
      exit 2
   fi
fi

#---------------------------------------------- 
# Prepare gsiparm.anl
#---------------------------------------------- 
#${NCP} ${PARMgsi}/gsiparm.anl.tmp_vr ./gsiparm.anl.tmp
${NCP} ${PARMgsi}/gsiparm.anl.tmp ./gsiparm.anl.tmp

sed -e "s/_L_HYB_ENS_/${L_HYB_ENS:-.false.}/g" \
    -e "s/_N_ENS_/${N_ENS:-80}/g" \
    -e "s/_G_RATIO_/${G_RATIO:-1}/g" \
    -e "s/_REO_ENS_/${REO_ENS:-1}/g" \
    -e "s/_GRID_RATIO_FV3_REGIONAL_/${refine_ratio:-4}/g" \
    gsiparm.anl.tmp > gsiparm.anl

#-------------------------------------------------------------------
# Link the executable and run the analysis
#-------------------------------------------------------------------
ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_gsi.x}
${NCP} -p ${ANALYSISEXEC} ./hafs_gsi.x

${APRUNC} ./hafs_gsi.x 1>stdout 2>&1
cat stdout

${NCP} -p ./stdout ${COMhafs}/${out_prefix}.analysis_vr.stdout

# Cat runtime output files.
cat fort.2* > ${COMhafs}/${out_prefix}.analysis_vr.gsistat

fi # end if [ -s ./synobs_vr.info ]; then

#-------------------------------------------------------------------
# Deliver files to COM
#-------------------------------------------------------------------
if [ "${HAFS_ENS}" = YES ]; then
  export RESTARTout=${RESTARTout:-${COMhafs}/RESTART_analysis_vr_ens/member${VR_ENSID}}
else
  export RESTARTout=${RESTARTout:-${COMhafs}/RESTART_analysis_vr}
fi
mkdir -p ${RESTARTout}

${NCP} ./fv3_oro_data ${RESTARTout}/oro_data.nc
${NCP} ./fv3_atmos_static ${RESTARTout}/atmos_static.nc
${NCP} ./fv3_grid_spec ${RESTARTout}/grid_spec.nc

if [ ${FGAT_OPT} = YES ]; then #OU added for FGAT
 ${NCP} ./coupler.res ${RESTARTout}/${PDY}.${cyc_fgat}0000.coupler.res
 ${NCP} ./fv3_akbk ${RESTARTout}/${PDY}.${cyc_fgat}0000.fv_core.res.nc
 ${NCP} ./fv3_sfcdata ${RESTARTout}/${PDY}.${cyc_fgat}0000.sfc_data.nc
 ${NCP} ./fv3_srfwnd ${RESTARTout}/${PDY}.${cyc_fgat}0000.fv_srf_wnd.res.tile1.nc
 ${NCP} ./fv3_dynvars ${RESTARTout}/${PDY}.${cyc_fgat}0000.fv_core.res.tile1.nc
 ${NCP} ./fv3_tracer ${RESTARTout}/${PDY}.${cyc_fgat}0000.fv_tracer.res.tile1.nc
else
 ${NCP} ./coupler.res ${RESTARTout}/${PDY}.${cyc}0000.coupler.res
 ${NCP} ./fv3_akbk ${RESTARTout}/${PDY}.${cyc}0000.fv_core.res.nc
 ${NCP} ./fv3_sfcdata ${RESTARTout}/${PDY}.${cyc}0000.sfc_data.nc
 ${NCP} ./fv3_srfwnd ${RESTARTout}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc
 ${NCP} ./fv3_dynvars ${RESTARTout}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
 ${NCP} ./fv3_tracer ${RESTARTout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
fi
exit

