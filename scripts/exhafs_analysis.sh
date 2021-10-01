#!/bin/sh

set -xe

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export FIXcrtm=${FIXcrtm:-${FIXhafs}/hafs-crtm-2.3.0}
export COMgfs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export COMINhafs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export DONST=${DONST:-"NO"}
export LEVS=${LEVS:-65}
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
export ONLINE_SATBIAS=${ONLINE_SATBIAS:-NO}

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

if [ ${RUN_FGAT} = YES ]; then
 CDATEtm03=`${NDATE} -3 $CDATE`
 PDYtm03=`echo ${CDATEtm03} | cut -c1-8`
 cyctm03=`echo ${CDATEtm03} | cut -c9-10`
 CDATEtm02=`${NDATE} -2 $CDATE`
 PDYtm02=`echo ${CDATEtm02} | cut -c1-8`
 cyctm02=`echo ${CDATEtm02} | cut -c9-10`
 CDATEtm01=`${NDATE} -1 $CDATE`
 PDYtm01=`echo ${CDATEtm01} | cut -c1-8`
 cyctm01=`echo ${CDATEtm01} | cut -c9-10`
 CDATEtp03=`${NDATE} +3 $CDATE`
 PDYtp03=`echo ${CDATEtp03} | cut -c1-8`
 cyctp03=`echo ${CDATEtp03} | cut -c9-10`
 CDATEtp02=`${NDATE} +2 $CDATE`
 PDYtp02=`echo ${CDATEtp02} | cut -c1-8`
 cyctp02=`echo ${CDATEtp02} | cut -c9-10`
 CDATEtp01=`${NDATE} +1 $CDATE`
 PDYtp01=`echo ${CDATEtp01} | cut -c1-8`
 cyctp01=`echo ${CDATEtp01} | cut -c9-10`
fi

export COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
export WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

if [ ! ${RUN_GSI} = "YES" ]; then
  echo "RUN_GSI: ${RUN_GSI} is not YES"
  echo "Do nothing. Exiting"
  exit
fi

if [ ! -s ${COMhafsprior}/storm1.holdvars.txt ] && [ ! -s ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  echo "Prior cycle does not exist. No need to run gsi for the first cycle."
  echo "Do nothing. Exiting"
  exit
fi

# Copy the first guess files
if [ ${RUN_GSI_VR} = "YES" ]; then
  RESTARTinp=${COMhafs}/RESTART_analysis_vr
else
  RESTARTinp=${COMhafsprior}/RESTART
fi

export RESTARTanl=${RESTARTanl:-${COMhafs}/RESTART_analysis}
mkdir -p ${RESTARTanl}

# We should already be in $DATA, but extra cd to be sure.
cd $DATA

if [ ${RUN_FGAT} = "YES" ]; then
  if [ ${RUN_GSI_VR_FGAT} = "YES" ]; then
    RESTARTinp_fgat=${COMhafs}/RESTART_analysis_vr
  else
    RESTARTinp_fgat=${COMhafsprior}/RESTART
  fi
  ${NLN} ${RESTARTinp_fgat}/${PDYtm03}.${cyctm03}0000.coupler.res ./coupler.res_03
  ${NLN} ${RESTARTinp_fgat}/${PDYtm03}.${cyctm03}0000.fv_core.res.nc ./fv3_akbk_03
  ${NLN} ${RESTARTinp_fgat}/${PDYtm03}.${cyctm03}0000.sfc_data.nc ./fv3_sfcdata_03
  ${NLN} ${RESTARTinp_fgat}/${PDYtm03}.${cyctm03}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd_03
  ${NLN} ${RESTARTinp_fgat}/${PDYtm03}.${cyctm03}0000.fv_core.res.tile1.nc ./fv3_dynvars_03
  ${NLN} ${RESTARTinp_fgat}/${PDYtm03}.${cyctm03}0000.fv_tracer.res.tile1.nc ./fv3_tracer_03
  ${NLN} ${RESTARTinp_fgat}/${PDYtp03}.${cyctp03}0000.coupler.res ./coupler.res_09
  ${NLN} ${RESTARTinp_fgat}/${PDYtp03}.${cyctp03}0000.fv_core.res.nc ./fv3_akbk_09
  ${NLN} ${RESTARTinp_fgat}/${PDYtp03}.${cyctp03}0000.sfc_data.nc ./fv3_sfcdata_09
  ${NLN} ${RESTARTinp_fgat}/${PDYtp03}.${cyctp03}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd_09
  ${NLN} ${RESTARTinp_fgat}/${PDYtp03}.${cyctp03}0000.fv_core.res.tile1.nc ./fv3_dynvars_09
  ${NLN} ${RESTARTinp_fgat}/${PDYtp03}.${cyctp03}0000.fv_tracer.res.tile1.nc ./fv3_tracer_09
fi

if [ ${RUN_ENVAR} = "YES" ]; then

export L_HYB_ENS=.true.
if [ ${RUN_ENSDA} = "YES" ]; then
  export N_ENS=${ENS_SIZE:-2}
  export BETA_S0=${BETA_S0:-0.0}
  export GRID_RATIO_ENS=${GRID_RATIO_ENS}
  export REGIONAL_ENSEMBLE_OPTION=5
  for mem in $(seq -f '%03g' 1 ${N_ENS})
  do
    #if [ ${RUN_GSI_VR_ENS} = "YES" ]; then
    #  RESTARTens=${COMhafs}/RESTART_analysis_vr_ens/mem${mem}
    #  RESTARTens=${WORKhafs}/intercom/RESTART_analysis_vr_ens/mem${mem}
    #else
      RESTARTens=${COMhafsprior}/RESTART_ens/mem${mem}
    #fi
    ${NLN} ${RESTARTens}/${PDY}.${cyc}0000.coupler.res ./fv3SAR06_ens_mem${mem}-coupler.res
    ${NLN} ${RESTARTens}/${PDY}.${cyc}0000.fv_core.res.nc ./fv3SAR06_ens_mem${mem}-fv3_akbk
    ${NLN} ${RESTARTens}/${PDY}.${cyc}0000.sfc_data.nc ./fv3SAR06_ens_mem${mem}-fv3_sfcdata
    ${NLN} ${RESTARTens}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv3SAR06_ens_mem${mem}-fv3_srfwnd
    ${NLN} ${RESTARTens}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv3SAR06_ens_mem${mem}-fv3_dynvars
    ${NLN} ${RESTARTens}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv3SAR06_ens_mem${mem}-fv3_tracer
    if [ ! -s ./fv3_ens_grid_spec ]; then
      ${NLN} ${RESTARTens}/grid_spec.nc ./fv3_ens_grid_spec
    fi
  done
else
  export N_ENS=80
  export BETA_S0=${BETA_S0:-0.2}
  export GRID_RATIO_ENS=1
  export REGIONAL_ENSEMBLE_OPTION=1
# Link ensemble members
  mkdir -p ensemble_data
  ENKF_SUFFIX="s"
  GSUFFIX=${GSUFFIX:-.nemsio}
  fhrs="06"
  for fhh in $fhrs; do
  rm -f filelist${fhh}
  for mem in $(seq -f '%03g' 1 ${N_ENS}); do
    if [ $USE_GFS_NEMSIO = .true. ]; then
    if [ -s ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}s${GSUFFIX:-.nemsio} ]; then
      ${NLN} ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}s${GSUFFIX:-.nemsio} ./ensemble_data/enkfgdas.${PDYprior}${hhprior}.atmf0${fhh}_ens_${mem}
    elif [ -s ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${GSUFFIX:-.nemsio} ]; then
      ${NLN} ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${GSUFFIX:-.nemsio} ./ensemble_data/enkfgdas.${PDYprior}${hhprior}.atmf0${fhh}_ens_${mem}
    fi
    fi
    if [ $USE_GFS_NCIO = .true. ]; then
    if [ -s ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}s${GSUFFIX:-.nc} ]; then
      ${NLN} ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}s${GSUFFIX:-.nc} ./ensemble_data/enkfgdas.${PDYprior}${hhprior}.atmf0${fhh}_ens_${mem}
    elif [ -s ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${GSUFFIX:-.nc} ]; then
      ${NLN} ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${GSUFFIX:-.nc} ./ensemble_data/enkfgdas.${PDYprior}${hhprior}.atmf0${fhh}_ens_${mem}
    fi
    fi
    echo "./ensemble_data/enkfgdas.${PDYprior}${hhprior}.atmf0${fhh}_ens_${mem}" >> filelist${fhh}
  done
  done
fi

fi # endif ${RUN_ENVAR}

# Copy first guess files
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.coupler.res ./coupler.res
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.nc ./fv3_akbk
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.sfc_data.nc ./fv3_sfcdata
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv3_dynvars
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv3_tracer

${NCP} ${RESTARTinp}/oro_data.nc ./fv3_oro_data
${NCP} ${RESTARTinp}/atmos_static.nc ./fv3_atmos_static
${NCP} ${RESTARTinp}/grid_spec.nc ./fv3_grid_spec

# Stat files
RADSTAT=${RADSTAT:-${RESTARTanl}/analysis.radstat}
GSISTAT=${GSISTAT:-${RESTARTanl}/analysis.gsistat}
PCPSTAT=${PCPSTAT:-${RESTARTanl}/analysis.pcpstat}
CNVSTAT=${CNVSTAT:-${RESTARTanl}/analysis.cnvstat}
OZNSTAT=${OZNSTAT:-${RESTARTanl}/analysis.oznstat}
GSISOUT=${GSISOUT:-${RESTARTanl}/analysis.gsisout}

# Obs diag
RUN_SELECT=${RUN_SELECT:-"NO"}
USE_SELECT=${USE_SELECT:-"NO"}
USE_RADSTAT=${USE_RADSTAT:-"NO"}
SELECT_OBS=${SELECT_OBS:-${COMhafs}/obsinput.tar}
GENDIAG=${GENDIAG:-"YES"}
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
${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats
#${NLN} ${PARMgsi}/nam_global_satangbias.txt ./satbias_angle
${NLN} ${PARMgsi}/hafs_satinfo.txt ./satinfo
#checkgfs $NLN $RADCLOUDINFO cloudy_radiance_info.txt
${NLN} ${PARMgsi}/atms_beamwidth.txt ./atms_beamwidth.txt
anavinfo=${PARMgsi}/hafs_anavinfo.tmp
sed -e "s/_LEV_/${npz:-64}/g" \
    -e "s/_LP1_/${LEVS:-65}/g" \
    ${anavinfo} > ./anavinfo
${NLN} ${PARMgsi}/hafs_convinfo.txt ./convinfo
#checkgfs $NLN $vqcdat       vqctp001.dat
#checkgfs $NLN $INSITUINFO   insituinfo
${NLN} ${PARMgsi}/global_ozinfo.txt ./ozinfo
${NLN} ${PARMgsi}/nam_global_pcpinfo.txt ./pcpinfo
#checkgfs $NLN $AEROINFO     aeroinfo
${NLN} ${PARMgsi}/global_scaninfo.txt ./scaninfo
#checkgfs $NLN $HYBENSINFO   hybens_info
${NLN} ${PARMgsi}/hafs_nam_errtable.r3dv ./errtable

${NLN} ${PARMgsi}/prepobs_prep.bufrtable ./prepobs_prep.bufrtable
${NLN} ${PARMgsi}/bufrtab.012 ./bftab_sstphr

# Link CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
  ${NLN} ${FIXcrtm}/fix-4-hafs/${file}.SpcCoeff.bin ./
  ${NLN} ${FIXcrtm}/fix-4-hafs/${file}.TauCoeff.bin ./
done

${NLN} ${FIXcrtm}/EmisCoeff/IR_Water/Big_Endian/Nalli.IRwater.EmisCoeff.bin ./Nalli.IRwater.EmisCoeff.bin
${NLN} ${FIXcrtm}/EmisCoeff/IR_Ice/SEcategory/Big_Endian/NPOESS.IRice.EmisCoeff.bin ./NPOESS.IRice.EmisCoeff.bin
${NLN} ${FIXcrtm}/EmisCoeff/IR_Snow/SEcategory/Big_Endian/NPOESS.IRsnow.EmisCoeff.bin ./NPOESS.IRsnow.EmisCoeff.bin
${NLN} ${FIXcrtm}/EmisCoeff/IR_Land/SEcategory/Big_Endian/NPOESS.IRland.EmisCoeff.bin ./NPOESS.IRland.EmisCoeff.bin
${NLN} ${FIXcrtm}/EmisCoeff/VIS_Ice/SEcategory/Big_Endian/NPOESS.VISice.EmisCoeff.bin ./NPOESS.VISice.EmisCoeff.bin
${NLN} ${FIXcrtm}/EmisCoeff/VIS_Land/SEcategory/Big_Endian/NPOESS.VISland.EmisCoeff.bin ./NPOESS.VISland.EmisCoeff.bin
${NLN} ${FIXcrtm}/EmisCoeff/VIS_Snow/SEcategory/Big_Endian/NPOESS.VISsnow.EmisCoeff.bin ./NPOESS.VISsnow.EmisCoeff.bin
${NLN} ${FIXcrtm}/EmisCoeff/VIS_Water/SEcategory/Big_Endian/NPOESS.VISwater.EmisCoeff.bin ./NPOESS.VISwater.EmisCoeff.bin
${NLN} ${FIXcrtm}/EmisCoeff/MW_Water/Big_Endian/FASTEM6.MWwater.EmisCoeff.bin ./FASTEM6.MWwater.EmisCoeff.bin
${NLN} ${FIXcrtm}/AerosolCoeff/Big_Endian/AerosolCoeff.bin ./AerosolCoeff.bin
${NLN} ${FIXcrtm}/CloudCoeff/Big_Endian/CloudCoeff.bin ./CloudCoeff.bin

# If requested, link (and if tarred, de-tar obsinput.tar) into obs_input.* files
if [ ${USE_SELECT:-NO} = "YES" ]; then
   rm -f obs_input.*
   nl=$(file $SELECT_OBS | cut -d: -f2 | grep tar | wc -l)
   if [ $nl -eq 1 ]; then
      rm -f obsinput.tar
      $NLN $SELECT_OBS obsinput.tar
      tar -xvf obsinput.tar
      rm -f obsinput.tar
   else
      for filetop in $(ls $SELECT_OBS/obs_input.*); do
         fileloc=$(basename $filetop)
         $NLN $filetop $fileloc
      done
   fi
fi

if [ ${USE_SELECT:-NO} != "YES" ]; then  #regular  run

# Link GFS/GDAS input and observation files
COMIN_OBS=${COMIN_OBS:-${COMgfs}/gfs.$PDY/$cyc/${atmos}}
OPREFIX=${OPREFIX:-"gfs.t${cyc}z."}
OSUFFIX=${OSUFFIX:-""}
PREPQC=${PREPQC:-${COMIN_OBS}/${OPREFIX}prepbufr${OSUFFIX}}
PREPQCPF=${PREPQCPF:-${COMIN_OBS}/${OPREFIX}prepbufr.acft_profiles${OSUFFIX}}
NSSTBF=${NSSTBF:-${COMIN_OBS}/${OPREFIX}nsstbufr${OSUFFIX}}
SATWND=${SATWND:-${COMIN_OBS}/${OPREFIX}satwnd.tm00.bufr_d${OSUFFIX}}
OSCATBF=${OSCATBF:-${COMIN_OBS}/${OPREFIX}oscatw.tm00.bufr_d${OSUFFIX}}
RAPIDSCATBF=${RAPIDSCATBF:-${COMIN_OBS}/${OPREFIX}rapidscatw.tm00.bufr_d${OSUFFIX}}
GSNDBF=${GSNDBF:-${COMIN_OBS}/${OPREFIX}goesnd.tm00.bufr_d${OSUFFIX}}
GSNDBF1=${GSNDBF1:-${COMIN_OBS}/${OPREFIX}goesfv.tm00.bufr_d${OSUFFIX}}
B1HRS2=${B1HRS2:-${COMIN_OBS}/${OPREFIX}1bhrs2.tm00.bufr_d${OSUFFIX}}
B1MSU=${B1MSU:-${COMIN_OBS}/${OPREFIX}1bmsu.tm00.bufr_d${OSUFFIX}}
B1HRS3=${B1HRS3:-${COMIN_OBS}/${OPREFIX}1bhrs3.tm00.bufr_d${OSUFFIX}}
B1HRS4=${B1HRS4:-${COMIN_OBS}/${OPREFIX}1bhrs4.tm00.bufr_d${OSUFFIX}}
B1AMUA=${B1AMUA:-${COMIN_OBS}/${OPREFIX}1bamua.tm00.bufr_d${OSUFFIX}}
B1AMUB=${B1AMUB:-${COMIN_OBS}/${OPREFIX}1bamub.tm00.bufr_d${OSUFFIX}}
B1MHS=${B1MHS:-${COMIN_OBS}/${OPREFIX}1bmhs.tm00.bufr_d${OSUFFIX}}
ESHRS3=${ESHRS3:-${COMIN_OBS}/${OPREFIX}eshrs3.tm00.bufr_d${OSUFFIX}}
ESAMUA=${ESAMUA:-${COMIN_OBS}/${OPREFIX}esamua.tm00.bufr_d${OSUFFIX}}
ESAMUB=${ESAMUB:-${COMIN_OBS}/${OPREFIX}esamub.tm00.bufr_d${OSUFFIX}}
ESMHS=${ESMHS:-${COMIN_OBS}/${OPREFIX}esmhs.tm00.bufr_d${OSUFFIX}}
HRS3DB=${HRS3DB:-${COMIN_OBS}/${OPREFIX}hrs3db.tm00.bufr_d${OSUFFIX}}
AMUADB=${AMUADB:-${COMIN_OBS}/${OPREFIX}amuadb.tm00.bufr_d${OSUFFIX}}
AMUBDB=${AMUBDB:-${COMIN_OBS}/${OPREFIX}amubdb.tm00.bufr_d${OSUFFIX}}
MHSDB=${MHSDB:-${COMIN_OBS}/${OPREFIX}mhsdb.tm00.bufr_d${OSUFFIX}}
AIRSBF=${AIRSBF:-${COMIN_OBS}/${OPREFIX}airsev.tm00.bufr_d${OSUFFIX}}
IASIBF=${IASIBF:-${COMIN_OBS}/${OPREFIX}mtiasi.tm00.bufr_d${OSUFFIX}}
ESIASI=${ESIASI:-${COMIN_OBS}/${OPREFIX}esiasi.tm00.bufr_d${OSUFFIX}}
IASIDB=${IASIDB:-${COMIN_OBS}/${OPREFIX}iasidb.tm00.bufr_d${OSUFFIX}}
AMSREBF=${AMSREBF:-${COMIN_OBS}/${OPREFIX}amsre.tm00.bufr_d${OSUFFIX}}
AMSR2BF=${AMSR2BF:-${COMIN_OBS}/${OPREFIX}amsr2.tm00.bufr_d${OSUFFIX}}
GMI1CRBF=${GMI1CRBF:-${COMIN_OBS}/${OPREFIX}gmi1cr.tm00.bufr_d${OSUFFIX}}
SAPHIRBF=${SAPHIRBF:-${COMIN_OBS}/${OPREFIX}saphir.tm00.bufr_d${OSUFFIX}}
SEVIRIBF=${SEVIRIBF:-${COMIN_OBS}/${OPREFIX}sevcsr.tm00.bufr_d${OSUFFIX}}
AHIBF=${AHIBF:-${COMIN_OBS}/${OPREFIX}ahicsr.tm00.bufr_d${OSUFFIX}}
ABIBF=${ABIBF:-${COMIN_OBS}/${OPREFIX}gsrcsr.tm00.bufr_d${OSUFFIX}}
CRISBF=${CRISBF:-${COMIN_OBS}/${OPREFIX}cris.tm00.bufr_d${OSUFFIX}}
ESCRIS=${ESCRIS:-${COMIN_OBS}/${OPREFIX}escris.tm00.bufr_d${OSUFFIX}}
CRISDB=${CRISDB:-${COMIN_OBS}/${OPREFIX}crisdb.tm00.bufr_d${OSUFFIX}}
CRISFSBF=${CRISFSBF:-${COMIN_OBS}/${OPREFIX}crisf4.tm00.bufr_d${OSUFFIX}}
ESCRISFS=${ESCRISFS:-${COMIN_OBS}/${OPREFIX}escrsf.tm00.bufr_d${OSUFFIX}}
CRISFSDB=${CRISFSDB:-${COMIN_OBS}/${OPREFIX}crsfdb.tm00.bufr_d${OSUFFIX}}
ATMSBF=${ATMSBF:-${COMIN_OBS}/${OPREFIX}atms.tm00.bufr_d${OSUFFIX}}
ESATMS=${ESATMS:-${COMIN_OBS}/${OPREFIX}esatms.tm00.bufr_d${OSUFFIX}}
ATMSDB=${ATMSDB:-${COMIN_OBS}/${OPREFIX}atmsdb.tm00.bufr_d${OSUFFIX}}
SSMITBF=${SSMITBF:-${COMIN_OBS}/${OPREFIX}ssmit.tm00.bufr_d${OSUFFIX}}
SSMISBF=${SSMISBF:-${COMIN_OBS}/${OPREFIX}ssmisu.tm00.bufr_d${OSUFFIX}}
SBUVBF=${SBUVBF:-${COMIN_OBS}/${OPREFIX}osbuv8.tm00.bufr_d${OSUFFIX}}
OMPSNPBF=${OMPSNPBF:-${COMIN_OBS}/${OPREFIX}ompsn8.tm00.bufr_d${OSUFFIX}}
OMPSTCBF=${OMPSTCBF:-${COMIN_OBS}/${OPREFIX}ompst8.tm00.bufr_d${OSUFFIX}}
GOMEBF=${GOMEBF:-${COMIN_OBS}/${OPREFIX}gome.tm00.bufr_d${OSUFFIX}}
OMIBF=${OMIBF:-${COMIN_OBS}/${OPREFIX}omi.tm00.bufr_d${OSUFFIX}}
MLSBF=${MLSBF:-${COMIN_OBS}/${OPREFIX}mls.tm00.bufr_d${OSUFFIX}}
OMPSLPBF=${OMPSLPBF:-${COMIN_OBS}/${OPREFIX}ompslp.tm00.bufr_d${OSUFFIX}}
SMIPCP=${SMIPCP:-${COMIN_OBS}/${OPREFIX}spssmi.tm00.bufr_d${OSUFFIX}}
TMIPCP=${TMIPCP:-${COMIN_OBS}/${OPREFIX}sptrmm.tm00.bufr_d${OSUFFIX}}
GPSROBF=${GPSROBF:-${COMIN_OBS}/${OPREFIX}gpsro.tm00.bufr_d${OSUFFIX}}
TCVITL=${TCVITL:-${COMIN_OBS}/${OPREFIX}syndata.tcvitals.tm00}
B1AVHAM=${B1AVHAM:-${COMIN_OBS}/${OPREFIX}avcsam.tm00.bufr_d${OSUFFIX}}
B1AVHPM=${B1AVHPM:-${COMIN_OBS}/${OPREFIX}avcspm.tm00.bufr_d${OSUFFIX}}
##HDOB=${HDOB:-${COMIN_OBS}/${OPREFIX}hdob.tm00.bufr_d${OSUFFIX}}

# Observational data
if [ -s $PREPQC ]; then
  $NCP -Lp $PREPQC     prepbufr
else
  touch prepbufr
fi
#$NLN $PREPQC           prepbufr
##$NLN $PREPQCPF         prepbufr_profl
$NLN $SATWND           satwndbufr
##$NLN $OSCATBF          oscatbufr
##$NLN $RAPIDSCATBF      rapidscatbufr
##$NLN $GSNDBF           gsndrbufr
$NLN $GSNDBF1          gsnd1bufr
##$NLN $B1HRS2           hirs2bufr
##$NLN $B1MSU            msubufr
$NLN $B1HRS3           hirs3bufr
$NLN $B1HRS4           hirs4bufr
$NLN $B1AMUA           amsuabufr
##$NLN $B1AMUB           amsubbufr
$NLN $B1MHS            mhsbufr
$NLN $ESHRS3           hirs3bufrears
$NLN $ESAMUA           amsuabufrears
##$NLN $ESAMUB           amsubbufrears
#$NLN $ESMHS            mhsbufrears
$NLN $HRS3DB           hirs3bufr_db
##$NLN $AMUADB           amsuabufr_db
##$NLN $AMUBDB           amsubbufr_db
#$NLN $MHSDB            mhsbufr_db
$NLN $SBUVBF           sbuvbufr
$NLN $OMPSNPBF         ompsnpbufr
$NLN $OMPSTCBF         ompstcbufr
$NLN $GOMEBF           gomebufr
$NLN $OMIBF            omibufr
$NLN $MLSBF            mlsbufr
##$NLN $SMIPCP           ssmirrbufr
##$NLN $TMIPCP           tmirrbufr
$NLN $AIRSBF           airsbufr
$NLN $IASIBF           iasibufr
$NLN $ESIASI           iasibufrears
$NLN $IASIDB           iasibufr_db
##$NLN $AMSREBF          amsrebufr
$NLN $AMSR2BF          amsr2bufr
$NLN $GMI1CRBF         gmibufr
$NLN $SAPHIRBF         saphirbufr
$NLN $SEVIRIBF         seviribufr
$NLN $CRISBF           crisbufr
$NLN $ESCRIS           crisbufrears
$NLN $CRISDB           crisbufr_db
$NLN $CRISFSBF         crisfsbufr
$NLN $ESCRISFS         crisfsbufrears
$NLN $CRISFSDB         crisfsbufr_db
$NLN $ATMSBF           atmsbufr
$NLN $ESATMS           atmsbufrears
$NLN $ATMSDB           atmsbufr_db
##$NLN $SSMITBF          ssmitbufr
$NLN $SSMISBF          ssmisbufr
$NLN $GPSROBF          gpsrobufr
$NLN $TCVITL           tcvitl
$NLN $B1AVHAM          avhambufr
$NLN $B1AVHPM          avhpmbufr
##$NLN $AHIBF            ahibufr
##$NLN $ABIBF            abibufr
##$NLN $HDOB             hdobbufr

##[[ $DONST = "YES" ]] && $NLN $NSSTBF nsstbufr

if [[ ${use_bufr_nr:-no} = "yes" ]]; then

if [ -s ${PREPQC}.nr ]; then
  $NCP -L ${PREPQC}.nr    prepbufr
fi
# $NLN ${PREPQC}.nr    prepbufr
  $NLN ${SAPHIRBF}.nr  saphirbufr
##[[ $DONST = "YES" ]] && $NLN /dev/null nsstbufr

fi

# HAFS specific observations
# Use updated prepbufr if exists
if [ -s ${WORKhafs}/intercom/obs_proc/hafs.prepbufr ]; then
  ${NCP} ${WORKhafs}/intercom/obs_proc/hafs.prepbufr prepbufr
fi
# cat tempdrop.prepbufr with drifting correction into prepbufr
if [ -s ${WORKhafs}/intercom/obs_proc/tempdrop.prepbufr ]; then
  cat ${WORKhafs}/intercom/obs_proc/tempdrop.prepbufr >> prepbufr
fi
COMINhafs_obs=${COMINhafs_obs:-${COMINhafs}/hafs.$PDY/$cyc/${atmos}}
${NLN} ${COMINhafs_obs}/hafs.t${cyc}z.hdob.tm00.bufr_d            hdobbufr
${NLN} ${COMINhafs_obs}/hafs.t${cyc}z.nexrad.tm00.bufr_d          l2rwbufr
${NLN} ${COMINhafs_obs}/hafs.t${cyc}z.tldplr.tm00.bufr_d          tldplrbufr

fi #USE_SELECT

# Workflow will read from previous cycles for satbias predictors if ONLINE_SATBIAS is set to yes
if [ ${ONLINE_SATBIAS} = "YES" ]; then
  if [ ! -s ${COMhafsprior}/RESTART_analysis/satbias_hafs_out ] && [ ! -s ${COMhafsprior}/RESTART_analysis/satbias_hafs_pc.out ]; then
    echo "Prior cycle satbias data does not exist. Grabbing satbias data from GDAS"
    ${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias           satbias_in
    ${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_pc        satbias_pc
  elif [ -s ${COMhafsprior}/RESTART_analysis/satbias_hafs_out ] && [ -s ${COMhafsprior}/RESTART_analysis/satbias_hafs_pc.out ]; then
    ${NLN} ${COMhafsprior}/RESTART_analysis/satbias_hafs_out            satbias_in
    ${NLN} ${COMhafsprior}/RESTART_analysis/satbias_hafs_pc.out         satbias_pc
  else
    echo "ERROR: Either source satbias_in or source satbias_pc does not exist. Exiting script."
    exit 2
  fi
else
  ${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias           satbias_in
  ${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_pc        satbias_pc
fi

#
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_air       satbias_air

#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.atmf003.nemsio  gfs_sigf03
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.atmf006.nemsio  gfs_sigf06
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.atmf009.nemsio  gfs_sigf09

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

if [ ${HX_ONLY:-NO} != "YES" ]; then

${NCP} ./fv3_oro_data ${RESTARTanl}/oro_data.nc
${NCP} ./fv3_atmos_static ${RESTARTanl}/atmos_static.nc
${NCP} ./fv3_grid_spec ${RESTARTanl}/grid_spec.nc

${NCP} ./coupler.res ${RESTARTanl}/${PDY}.${cyc}0000.coupler.res
${NCP} ./fv3_akbk ${RESTARTanl}/${PDY}.${cyc}0000.fv_core.res.nc
${NCP} ./fv3_sfcdata ${RESTARTanl}/${PDY}.${cyc}0000.sfc_data.nc
${NCP} ./fv3_srfwnd ${RESTARTanl}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc
${NCP} ./fv3_dynvars ${RESTARTanl}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
${NCP} ./fv3_tracer ${RESTARTanl}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc

fi

# If requested, generate diagnostic files
if [ $GENDIAG = "YES" ] ; then

   # Set up lists and variables for various types of diagnostic files.
   ntype=3

   diagtype[0]="conv conv_gps conv_ps conv_pw conv_q conv_sst conv_t conv_tcp conv_uv conv_spd conv_rw"
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

# Save satbias data for next cycle
if [ ${ONLINE_SATBIAS} = "YES" ]; then
  ${NCP} satbias_out  $RESTARTanl/satbias_hafs_out
  ${NCP} satbias_pc.out  $RESTARTanl/satbias_hafs_pc.out
fi

# If no processing error, remove $DIAG_DIR
if [[ "$REMOVE_DIAG_DIR" = "YES" && "$err" = "0" ]]; then
    rm -rf $DIAG_DIR
fi

exit

