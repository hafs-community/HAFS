#!/bin/sh

set -xe

if [ ${ENSDA} = YES ]; then
  export NHRS=${NHRS_ENS:-126}
  export NBDYHRS=${NBDYHRS_ENS:-3}
  export NOUTHRS=${NOUTHRS_ENS:-3}
  export CASE=${CASE_ENS:-C768}
  export CRES=`echo $CASE | cut -c 2-`
  export gtype=${gtype_ens:-regional}
  export LEVS=${LEVS_ENS:-65}
else
  export NHRS=${NHRS:-126}
  export NBDYHRS=${NBDYHRS:-3}
  export NOUTHRS=${NOUTHRS:-3}
  export CASE=${CASE:-C768}
  export CRES=`echo $CASE | cut -c 2-`
  export gtype=${gtype:-regional}
  export LEVS=${LEVS:-65}
fi

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export FIXcrtm=${FIXcrtm:-${FIXhafs}/hafs-crtm-2.3.0}
export COMgfs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export COMINhafs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export DONST=${DONST:-"NO"}
export use_bufr_nr=${use_bufr_nr:-no}
export grid_ratio_fv3_regional=${grid_ratio_fv3_regional:-1}
export s_ens_h=${s_ens_h:-150}
export s_ens_v=${s_ens_v:--0.5}
export out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}

export RUN_GSI_VR=${RUN_GSI_VR:-NO}
export RUN_GSI_VR_FGAT=${RUN_GSI_VR_FGAT:-NO}
export RUN_GSI_VR_ENS=${RUN_GSI_VR_ENS:-NO}
export RUN_GSI=${RUN_GSI:-NO}
export RUN_FGAT=${RUN_FGAT:-NO}
export FGAT=${FGAT:-NO}
export RUN_ENVAR=${RUN_ENVAR:-NO}
export RUN_ENSDA=${RUN_ENSDA:-NO}
export ENSDA=${ENSDA:-NO}
export GRID_RATIO_ENS=${GRID_RATIO_ENS:-1}

export RUN_ENVAR=NO
#export RUN_ENSDA=NO

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
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export CATEXEC=${CATEXEC:-${EXEChafs}/hafs_ncdiag_cat.x}
export MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
export COMPRESS=${COMPRESS:-gzip}
export UNCOMPRESS=${UNCOMPRESS:-gunzip}

if [ $GFSVER = PROD2021 ]; then
  export atmos="atmos/"
  export USE_GFS_NEMSIO=.false.
  export USE_GFS_NCIO=.true.
  GSUFFIX=${GSUFFIX:-.nc}
elif [ $GFSVER = PROD2019 ]; then
  export atmos=""
  export USE_GFS_NEMSIO=.true.
  export USE_GFS_NCIO=.false.
  GSUFFIX=${GSUFFIX:-.nemsio}
else
  export atmos=""
  export USE_GFS_NEMSIO=.true.
  export USE_GFS_NCIO=.false.
  GSUFFIX=${GSUFFIX:-.nemsio}
fi

# Diagnostic files options
export netcdf_diag=${netcdf_diag:-".true."}
export binary_diag=${binary_diag:-".false."}

yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`

CDATEprior=`${NDATE} -6 $CDATE`
yrprior=`echo ${CDATEprior} | cut -c1-4`
mnprior=`echo ${CDATEprior} | cut -c5-6`
dyprior=`echo ${CDATEprior} | cut -c7-8`
hhprior=`echo ${CDATEprior} | cut -c9-10`
cycprior=`echo ${CDATEprior} | cut -c9-10`
PDYprior=`echo ${CDATEprior} | cut -c1-8`

if [ ${FGAT} = "YES" ]; then
 CDATEtm06=`${NDATE} -6 $CDATE`
 CDATEtp06=`${NDATE} +6 $CDATE`
 if [ ${FGAT_HR} = "03" ]; then
  CDATEfgat=`${NDATE} -3 $CDATE`
 fi
 if [ ${FGAT_HR} = "09" ]; then
  CDATEfgat=`${NDATE} +3 $CDATE`
 fi
 PDYfgat=`echo ${CDATEfgat} | cut -c1-8`
 cycfgat=`echo ${CDATEfgat} | cut -c9-10`
fi

export COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
export WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

if [ ${RUN_GSI_VR} = "NO" ] && [ ${RUN_GSI_VR_FGAT} = "NO" ] && [ ${RUN_GSI_VR_ENS} = "NO" ]; then
  echo "None of RUN_GSI_VR, RUN_GSI_VR_FGAT, RUN_GSI_VR_ENS is YES"
  echo "$RUN_GSI_VR, $RUN_GSI_VR_FGAT, $RUN_GSI_VR_ENS"
  echo "Do nothing. Exiting"
  exit
fi

if [ ${RUN_GSI_VR_ENS} = "YES" ] && [ "${ENSDA}" = YES ]; then
 if [ ! -s ${COMhafsprior}/storm1.holdvars.txt ] && [ ! -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  echo "Prior cycle does not exist. No need to run gsi_vr for the first cycle."
  echo "Do nothing. Exiting"
  exit
 fi
elif [ ${RUN_GSI_VR_FGAT} = "YES" ] && [ ${FGAT} = YES ]; then
 if [ ! -s ${COMhafsprior}/storm1.holdvars.txt ] && [ ! -s ${COMhafsprior}/RESTART/${PDYfgat}.${cycfgat}0000.fv_core.res.tile1.nc ]; then
  echo "Prior cycle FGAT files does not exist. No need to run gsi_vr for the first cycle."
  echo "Do nothing. Exiting"
  exit
 fi
else
 if [ ! -s ${COMhafsprior}/storm1.holdvars.txt ] && [ ! -s ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  echo "Prior cycle does not exist. No need to run gsi_vr for the first cycle."
  echo "Do nothing. Exiting"
  exit
 fi
fi

#---------------------------------------------- 
# Sample the synthetic data around the storm from the previous cycle's model forecast
# And relcoate the synthetic data according to the observed (tcvitals) storm location 
#---------------------------------------------- 

# Copy the input first guess files
if [ "${ENSDA}" = "YES" ]; then
  export RESTARTinp=${RESTARTinp:-${COMhafsprior}/RESTART_ens/mem${ENSID}}
  ${NCP} ${RESTARTinp}/oro_data.nc ./fv3_oro_data
  ${NCP} ${RESTARTinp}/atmos_static.nc ./fv3_atmos_static
  ${NCP} ${RESTARTinp}/grid_spec.nc ./fv3_grid_spec
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.coupler.res ./coupler.res
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.nc ./fv3_akbk
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.sfc_data.nc ./fv3_sfcdata
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv3_dynvars
  ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv3_tracer
  ${NLN} ${COMhafsprior}/product_ens/mem${ENSID}/${STORM,,}${STORMID,,}.${CDATEprior}.trak.hafs.atcfunix.all ./hafs.atcfunix_prior
  grep ", HAFS, 006," ./hafs.atcfunix_prior | grep ",  34, NEQ," > ./hafs.atcfunix
  export RESTARTanl=${RESTARTanl:-${COMhafs}/RESTART_analysis_vr_ens/mem${ENSID}}
else
  export RESTARTinp=${RESTARTinp:-${COMhafsprior}/RESTART}
  ${NCP} ${RESTARTinp}/oro_data.nc ./fv3_oro_data
  ${NCP} ${RESTARTinp}/atmos_static.nc ./fv3_atmos_static
  ${NCP} ${RESTARTinp}/grid_spec.nc ./fv3_grid_spec
 if [ ${FGAT} = "YES" ]; then
  ${NCP} ${RESTARTinp}/${PDYfgat}.${cycfgat}0000.coupler.res ./coupler.res
  ${NCP} ${RESTARTinp}/${PDYfgat}.${cycfgat}0000.fv_core.res.nc ./fv3_akbk
  ${NCP} ${RESTARTinp}/${PDYfgat}.${cycfgat}0000.sfc_data.nc ./fv3_sfcdata
  ${NCP} ${RESTARTinp}/${PDYfgat}.${cycfgat}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
  ${NCP} ${RESTARTinp}/${PDYfgat}.${cycfgat}0000.fv_core.res.tile1.nc ./fv3_dynvars
  ${NCP} ${RESTARTinp}/${PDYfgat}.${cycfgat}0000.fv_tracer.res.tile1.nc ./fv3_tracer
  ${NLN} ${COMhafsprior}/${STORM,,}${STORMID,,}.${CDATEprior}.trak.hafs.atcfunix.all ./hafs.atcfunix_prior
  grep ", HAFS, 0${FGAT_HR}," ./hafs.atcfunix_prior | grep ",  34, NEQ," > ./hafs.atcfunix
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
  export RESTARTanl=${RESTARTanl:-${COMhafs}/RESTART_analysis_vr}
fi
cat ./hafs.atcfunix

# Find and sort active storms for this cycle from known tcvitals file
# This can potentially provide multiple tcvital messages to the tracker, 
# so that it can track multiple storms simultaneously.
# *** Currently, tcutil_multistorm_sort.py searches the tcvitals files
# specified in the script. Need to modify it to be able to deal with storm
# message files/dirs, as well as passing in tcvitals files.
if [ ${FGAT} = "YES" ]; then
 ${USHhafs}/tcutil_multistorm_sort.py ${CDATEtm06} | cut -c1-96 > allvit_pre
 ${USHhafs}/tcutil_multistorm_sort.py ${CDATE} | cut -c1-96 > allvit_aft
 if [ ${FGAT_HR} = "03" ]; then
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

mkdir -p ${RESTARTanl}

# Stat files
if [ ${FGAT} = "YES" ]; then
  RADSTAT=${RADSTAT:-${RESTARTanl}/${PDYfgat}.${cycfgat}0000.analysis.radstat}
  GSISTAT=${GSISTAT:-${RESTARTanl}/${PDYfgat}.${cycfgat}0000.analysis.gsistat}
  PCPSTAT=${PCPSTAT:-${RESTARTanl}/${PDYfgat}.${cycfgat}0000.analysis.pcpstat}
  CNVSTAT=${CNVSTAT:-${RESTARTanl}/${PDYfgat}.${cycfgat}0000.analysis.cnvstat}
  OZNSTAT=${OZNSTAT:-${RESTARTanl}/${PDYfgat}.${cycfgat}0000.analysis.oznstat}
  GSISOUT=${GSISOUT:-${RESTARTanl}/${PDYfgat}.${cycfgat}0000.analysis.gsisout}
else
  RADSTAT=${RADSTAT:-${RESTARTanl}/${PDY}.${cyc}0000.analysis.radstat}
  GSISTAT=${GSISTAT:-${RESTARTanl}/${PDY}.${cyc}0000.analysis.gsistat}
  PCPSTAT=${PCPSTAT:-${RESTARTanl}/${PDY}.${cyc}0000.analysis.pcpstat}
  CNVSTAT=${CNVSTAT:-${RESTARTanl}/${PDY}.${cyc}0000.analysis.cnvstat}
  OZNSTAT=${OZNSTAT:-${RESTARTanl}/${PDY}.${cyc}0000.analysis.oznstat}
  GSISOUT=${GSISOUT:-${RESTARTanl}/${PDY}.${cyc}0000.analysis.gsisout}
fi

# Obs diag
RUN_SELECT=${RUN_SELECT:-"NO"}
USE_SELECT=${USE_SELECT:-"NO"}
USE_RADSTAT=${USE_RADSTAT:-"NO"}
SELECT_OBS=${SELECT_OBS:-${COMhafs}/obsinput.tar}
#GENDIAG=${GENDIAG:-"YES"}
GENDIAG=${GENDIAG:-"NO"}
DIAG_SUFFIX=${DIAG_SUFFIX:-""}
if [ $netcdf_diag = ".true." ] ; then
   DIAG_SUFFIX="${DIAG_SUFFIX}.nc4"
fi
DIAG_COMPRESS=${DIAG_COMPRESS:-"YES"}
DIAG_TARBALL=${DIAG_TARBALL:-"YES"}
USE_MPISERIAL=${USE_MPISERIAL:-"YES"}
USE_CFP=${USE_CFP:-"NO"}
CFP_MP=${CFP_MP:-"NO"}
nm=""
if [ $CFP_MP = "YES" ]; then
  nm=0
fi

export DIAG_DIR=${DIAG_DIR:-./analysis_diags}
REMOVE_DIAG_DIR=${REMOVE_DIAG_DIR:-"NO"}

# Set script / GSI control parameters
lrun_subdirs=${lrun_subdirs:-".true."}

#---------------------------------------------- 
# Link all the necessary fix files
#---------------------------------------------- 
anavinfo=${PARMgsi}/hafs_anavinfo.tmp
sed -e "s/_LEV_/${npz:-64}/g" \
    -e "s/_LP1_/${LEVS:-65}/g" \
    ${anavinfo} > ./anavinfo
${NLN} ${PARMgsi}/hafs_convinfo.txt_vr ./convinfo
${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats
${NLN} ${PARMgsi}/hafs_nam_errtable.r3dv_vr ./errtable

# Link GFS/GDAS input and observation files
${NLN} ./synobs_vr.prepbufr ./prepbufr

# Diagnostic files
# if requested, link GSI diagnostic file directories for use later
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
${NCP} ${PARMgsi}/gsiparm.anl.tmp ./

sed -e "s/_MITER_/${MITER:-2}/g" \
    -e "s/_NITER_/${NITER:-50}/g" \
    -e "s/_USE_GFS_NEMSIO_/${USE_GFS_NEMSIO:-.true.}/g" \
    -e "s/_USE_GFS_NCIO_/${USE_GFS_NCIO:-.false.}/g" \
    -e "s/_NETCDF_DIAG_/${netcdf_diag:-.true.}/g" \
    -e "s/_BINARY_DIAG_/${binary_diag:-.false.}/g" \
    -e "s/_LREAD_OBS_SAVE_/${LREAD_OBS_SAVE:-.false.}/g" \
    -e "s/_LREAD_OBS_SKIP_/${LREAD_OBS_SKIP:-.false.}/g" \
    -e "s/_ENS_NSTARTHR_/${ENS_NSTARTHR:-6}/g" \
    -e "s/_LWRITE_PREDTERMS_/${LWRITE_PREDTERMS:-.false.}/g" \
    -e "s/_LWRITE_PEAKWT_/${LWRITE_PEAKWT:-.false.}/g" \
    -e "s/_REDUCE_DIAG_/${REDUCE_DIAG:-.false.}/g" \
    -e "s/_L_HYB_ENS_/${L_HYB_ENS:-.false.}/g" \
    -e "s/_N_ENS_/${N_ENS:-80}/g" \
    -e "s/_S_ENS_H_/${s_ens_h:-150}/g" \
    -e "s/_S_ENS_V_/${s_ens_v:--0.5}/g" \
    -e "s/_BETA_S0_/${BETA_S0:-0.2}/g" \
    -e "s/_GRID_RATIO_ENS_/${GRID_RATIO_ENS:-1}/g" \
    -e "s/_REGIONAL_ENSEMBLE_OPTION_/${REGIONAL_ENSEMBLE_OPTION:-1}/g" \
    -e "s/_GRID_RATIO_FV3_REGIONAL_/${grid_ratio_fv3_regional:-1}/g" \
    gsiparm.anl.tmp > gsiparm.anl

#-------------------------------------------------------------------
# Link the executable and run the analysis
#-------------------------------------------------------------------
ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_gsi.x}
${NCP} -p ${ANALYSISEXEC} ./hafs_gsi.x

${APRUNC} ./hafs_gsi.x 1> stdout 2>&1
cat stdout

${NCP} -p ./stdout ${GSISOUT}

# Cat runtime output files.
cat fort.2* > ${GSISTAT}

# If requested, create obsinput tarball from obs_input.* files
if [ ${RUN_SELECT:-NO} = "YES" ]; then
  echo $(date) START tar obs_input
  rm -f ./obsinput.tar
  ${NLN} $SELECT_OBS ./obsinput.tar
  tar -cvf obsinput.tar obs_input.*
  echo $(date) END tar obs_input
fi

# If requested, generate diagnostic files
if [ $GENDIAG = "YES" ] ; then

   # Set up lists and variables for various types of diagnostic files.
   ntype=3

   diagtype[0]="conv conv_gps conv_ps conv_pw conv_q conv_sst conv_t conv_tcp conv_uv conv_spd"
   diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
   diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a gome_metop-b omi_aura mls30_aura ompsnp_npp ompstc8_npp gome_metop-c"
   diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_f16 ssmis_f17 ssmis_f18 ssmis_f19 ssmis_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 seviri_m11 cris_npp cris-fsr_npp cris-fsr_n20 atms_npp atms_n20 hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b avhrr_metop-b avhrr_n18 avhrr_n19 avhrr_metop-a amsr2_gcom-w1 gmi_gpm saphir_meghat ahi_himawari8 abi_g16 abi_g17 amsua_metop-c mhs_metop-c iasi_metop-c avhrr_metop-c"

   diaglist[0]=listcnv
   diaglist[1]=listpcp
   diaglist[2]=listozn
   diaglist[3]=listrad

   diagfile[0]=$CNVSTAT
   diagfile[1]=$PCPSTAT
   diagfile[2]=$OZNSTAT
   diagfile[3]=$RADSTAT

   numfile[0]=0
   numfile[1]=0
   numfile[2]=0
   numfile[3]=0

   # Set diagnostic file prefix based on lrun_subdirs variable
   if [ $lrun_subdirs = ".true." ]; then
      prefix=" dir.*/"
   else
      prefix="pe*"
   fi

   if [ $USE_CFP = "YES" -o $USE_MPISERIAL = "YES" ]; then
      [[ -f ./diag.sh ]] && rm ./diag.sh
      [[ -f ./mp_diag.sh ]] && rm ./mp_diag.sh
      cat > ./diag.sh << EOFdiag
#!/bin/sh
lrun_subdirs=\$1
binary_diag=\$2
type=\$3
loop=\$4
string=\$5
CDATE=\$6
DIAG_COMPRESS=\$7
DIAG_SUFFIX=\$8
if [ \$lrun_subdirs = ".true." ]; then
   prefix=" dir.*/"
else
   prefix="pe*"
fi
file=diag_\${type}_\${string}.\${CDATE}\${DIAG_SUFFIX}
if [ \$binary_diag = ".true." ]; then
   cat \${prefix}\${type}_\${loop}* > \$file
else
   $CATEXEC -o \$file \${prefix}\${type}_\${loop}*
fi
if [ \$DIAG_COMPRESS = "YES" ]; then
   $COMPRESS \$file
fi
EOFdiag
      chmod 755 ./diag.sh
   fi

   # Collect diagnostic files as a function of loop and type.
   # Loop over first and last outer loops to generate innovation
   # diagnostic files for indicated observation types (groups)
   #
   # NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
   #        loop 03 will contain innovations with respect to
   #        the analysis.  Creation of o-a innovation files
   #        is triggered by write_diag(3)=.true.  The setting
   #        write_diag(1)=.true. turns on creation of o-g
   #        innovation files.

   loops="01 03"
   for loop in $loops; do
      case $loop in
         01) string=ges;;
         03) string=anl;;
          *) string=$loop;;
      esac
      echo $(date) START loop $string >&2
      n=-1
      while [ $((n+=1)) -le $ntype ] ;do
         for type in $(echo ${diagtype[n]}); do
            count=$(ls ${prefix}${type}_${loop}* 2>/dev/null | wc -l)
            if [ $count -gt 1 ]; then
               if [ $USE_CFP = "YES" ]; then
                  echo "$nm ./diag.sh $lrun_subdirs $binary_diag $type $loop $string $CDATE $DIAG_COMPRESS $DIAG_SUFFIX" | tee -a ./mp_diag.sh
          if [ ${CFP_MP:-"NO"} = "YES" ]; then
              nm=$((nm+1))
          fi
               elif [ $USE_MPISERIAL = "YES" ]; then
                  echo "$nm ./diag.sh $lrun_subdirs $binary_diag $type $loop $string $CDATE $DIAG_COMPRESS $DIAG_SUFFIX" | tee -a ./mp_diag.sh
               else
                  if [ $binary_diag = ".true." ]; then
                     cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
                  else
                     $CATEXEC -o diag_${type}_${string}.${CDATE}${DIAG_SUFFIX} ${prefix}${type}_${loop}*
                  fi
               fi
               echo "diag_${type}_${string}.${CDATE}*" >> ${diaglist[n]}
               numfile[n]=$(expr ${numfile[n]} + 1)
            elif [ $count -eq 1 ]; then
                cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
                if [ $DIAG_COMPRESS = "YES" ]; then
            $COMPRESS diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
                fi
                echo "diag_${type}_${string}.${CDATE}*" >> ${diaglist[n]}
                numfile[n]=$(expr ${numfile[n]} + 1)
            fi
         done
      done
      echo $(date) END loop $string >&2
   done

   # We should already be in $DATA, but extra cd to be sure.
   cd $DATA

   # If requested, compress diagnostic files
   if [ $DIAG_COMPRESS = "YES" -a $USE_CFP = "NO" -a $USE_MPISERIAL = "NO" ]; then
      echo $(date) START $COMPRESS diagnostic files >&2
      for file in $(ls diag_*${CDATE}${DIAG_SUFFIX}); do
         $COMPRESS $file
      done
      echo $(date) END $COMPRESS diagnostic files >&2
   fi

   if [ $USE_CFP = "YES" ] ; then
      chmod 755 ./mp_diag.sh
      ncmd=$(cat ./mp_diag.sh | wc -l)
      if [ $ncmd -gt 0 ]; then
         ncmd_max=$((ncmd < npe_node_max ? ncmd : npe_node_max))
         APRUNCFP_DIAG=$(eval echo $APRUNCFP)
         $APRUNCFP_DIAG ./mp_diag.sh
         export ERR=$?
         export err=$ERR
         $ERRSCRIPT || exit 3
      fi
   fi

   if [ $USE_MPISERIAL = "YES" ] ; then
      chmod 755 ./mp_diag.sh
      ${APRUNC} ${MPISERIAL} -m ./mp_diag.sh
   fi

   # If requested, create diagnostic file tarballs
   if [ $DIAG_TARBALL = "YES" ]; then
      echo $(date) START tar diagnostic files >&2
      n=-1
      while [ $((n+=1)) -le $ntype ] ;do
         TAROPTS="-uvf"
         if [ ! -s ${diagfile[n]} ]; then
            TAROPTS="-cvf"
         fi
         if [ ${numfile[n]} -gt 0 ]; then
            tar $TAROPTS ${diagfile[n]} $(cat ${diaglist[n]})
            export ERR=$?
            export err=$ERR
            $ERRSCRIPT || exit 4
         fi
      done

      # Restrict CNVSTAT
      #chmod 750 $CNVSTAT
      #${CHGRP_CMD} $CNVSTAT

      # Restrict RADSTAT
      #chmod 750 $RADSTAT
      #${CHGRP_CMD} $RADSTAT

      echo $(date) END tar diagnostic files >&2
   fi
fi # End diagnostic file generation block - if [ $GENDIAG = "YES" ]

# If no processing error, remove $DIAG_DIR
if [[ "$REMOVE_DIAG_DIR" = "YES" && "$err" = "0" ]]; then
    rm -rf $DIAG_DIR
fi

fi # end if [ -s ./synobs_vr.info ]; then

${NCP} ./fv3_oro_data ${RESTARTanl}/oro_data.nc
${NCP} ./fv3_atmos_static ${RESTARTanl}/atmos_static.nc
${NCP} ./fv3_grid_spec ${RESTARTanl}/grid_spec.nc

if [ ${FGAT} = "YES" ]; then
 ${NCP} ./coupler.res ${RESTARTanl}/${PDYfgat}.${cycfgat}0000.coupler.res
 ${NCP} ./fv3_akbk ${RESTARTanl}/${PDYfgat}.${cycfgat}0000.fv_core.res.nc
 ${NCP} ./fv3_sfcdata ${RESTARTanl}/${PDYfgat}.${cycfgat}0000.sfc_data.nc
 ${NCP} ./fv3_srfwnd ${RESTARTanl}/${PDYfgat}.${cycfgat}0000.fv_srf_wnd.res.tile1.nc
 ${NCP} ./fv3_dynvars ${RESTARTanl}/${PDYfgat}.${cycfgat}0000.fv_core.res.tile1.nc
 ${NCP} ./fv3_tracer ${RESTARTanl}/${PDYfgat}.${cycfgat}0000.fv_tracer.res.tile1.nc
else
 ${NCP} ./coupler.res ${RESTARTanl}/${PDY}.${cyc}0000.coupler.res
 ${NCP} ./fv3_akbk ${RESTARTanl}/${PDY}.${cyc}0000.fv_core.res.nc
 ${NCP} ./fv3_sfcdata ${RESTARTanl}/${PDY}.${cyc}0000.sfc_data.nc
 ${NCP} ./fv3_srfwnd ${RESTARTanl}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc
 ${NCP} ./fv3_dynvars ${RESTARTanl}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
 ${NCP} ./fv3_tracer ${RESTARTanl}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
fi

exit

