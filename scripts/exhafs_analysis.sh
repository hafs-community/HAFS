#!/bin/sh

set -xe

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export FIXcrtm=${FIXcrtm:-${FIXhafs}/hwrf-crtm-2.2.6}
export COMgfs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export COMINhafs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export DONST=${DONST:-"NO"}

export RUN_GSI=${RUN_GSI:-NO}
export RUN_GSI_VR=${RUN_GSI_VR:-NO}
export hybrid_3denvar_gdas=${hybrid_3denvar_gdas:-yes}

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

out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}

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
if [ ${RUN_GSI_VR} = "YES" ] && [ -s ${COMhafs}/RESTART_analysis_vr/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  RESTARTinp=${COMhafs}/RESTART_analysis_vr
else
  RESTARTinp=${COMhafsprior}/RESTART
fi

${NCP} ${RESTARTinp}/oro_data.nc ./fv3_oro_data
${NCP} ${RESTARTinp}/atmos_static.nc ./fv3_atmos_static
${NCP} ${RESTARTinp}/grid_spec.nc ./fv3_grid_spec

${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.coupler.res ./coupler.res
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.nc ./fv3_akbk
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.sfc_data.nc ./fv3_sfcdata
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv3_dynvars
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv3_tracer

if [ $hybrid_3denvar_gdas = yes ]; then

export L_HYB_ENS=.true.
export N_ENS=80
# Link ensemble members
mkdir -p ensemble_data
ENKF_SUFFIX="s"
GSUFFIX=${GSUFFIX:-.nemsio}
fhrs="06"
for fhh in $fhrs; do
  rm -f filelist${fhh}
  for mem in $(seq -f '%03g' 1 ${N_ENS}); do
    ${NLN} ${COMgfs}/enkfgdas.${PDYprior}/${hhprior}/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${ENKF_SUFFIX:-s}${GSUFFIX:-.nemsio} ./ensemble_data/enkfgdas.${PDYprior}${hhprior}.atmf0${fhh}_ens_${mem}
    echo "./ensemble_data/enkfgdas.${PDYprior}${hhprior}.atmf0${fhh}_ens_${mem}" >> filelist${fhh}
  done
done

fi	

#---------------------------------------------- 
# Link all the necessary fix files
#---------------------------------------------- 
${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats
#${NLN} ${PARMgsi}/nam_global_satangbias.txt ./satbias_angle
${NLN} ${PARMgsi}/hwrf_satinfo.txt ./satinfo
#checkgfs $NLN $RADCLOUDINFO cloudy_radiance_info.txt
${NLN} ${PARMgsi}/atms_beamwidth.txt ./atms_beamwidth.txt
${NLN} ${PARMgsi}/anavinfo_hafs_L${LEVS:-65} ./anavinfo
${NLN} ${PARMgsi}/hwrf_convinfo.txt ./convinfo
#checkgfs $NLN $vqcdat       vqctp001.dat
#checkgfs $NLN $INSITUINFO   insituinfo
${NLN} ${PARMgsi}/global_ozinfo.txt ./ozinfo
${NLN} ${PARMgsi}/nam_global_pcpinfo.txt ./pcpinfo
#checkgfs $NLN $AEROINFO     aeroinfo
${NLN} ${PARMgsi}/global_scaninfo.txt ./scaninfo
#checkgfs $NLN $HYBENSINFO   hybens_info
${NLN} ${PARMgsi}/hwrf_nam_errtable.r3dv ./errtable

${NLN} ${PARMgsi}/prepobs_prep.bufrtable ./prepobs_prep.bufrtable
${NLN} ${PARMgsi}/bufrtab.012 ./bftab_sstphr

# Link CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
  ${NLN} ${FIXcrtm}/fix-4-hwrf/${file}.SpcCoeff.bin ./
  ${NLN} ${FIXcrtm}/fix-4-hwrf/${file}.TauCoeff.bin ./
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

# Link GFS/GDAS input and observation files
COMIN_OBS=${COMgfs}/gfs.$PDY/$cyc
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
$NLN $PREPQC           prepbufr
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

if [[ ${USE_BUFR_NR} = "yes" ]]; then
  $NLN ${PREPQC}.nr    prepbufr
  $NLN ${SAPHIRBF}.nr  saphirbufr
fi

# HAFS specific observations
COMINhafs_obs=${COMINhafs}/hafs.$PDY/$cyc
${NLN} ${COMINhafs_obs}/hafs.t${cyc}z.hdob.tm00.bufr_d            hdobbufr
${NLN} ${COMINhafs_obs}/hafs.t${cyc}z.nexrad.tm00.bufr_d          l2rwbufr
${NLN} ${COMINhafs_obs}/hafs.t${cyc}z.tldplr.tm00.bufr_d          tldplrbufr

#
${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.abias           satbias_in
${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.abias_pc        satbias_pc
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.abias_air       satbias_air

#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.atmf003.nemsio  gfs_sigf03
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.atmf006.nemsio  gfs_sigf06
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.atmf009.nemsio  gfs_sigf09

# Diagnostic files
# if requested, link GSI diagnostic file directories for use later
export DIAG_DIR=${DIAG_DIR:-${COMhafs}/analysis_diags}
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

sed -e "s/_L_HYB_ENS_/${L_HYB_ENS:-.false.}/g" \
    -e "s/_N_ENS_/${N_ENS:-80}/g" \
    -e "s/_GRID_RATIO_FV3_REGIONAL_/${refine_ratio:-4}/g" \
    gsiparm.anl.tmp > gsiparm.anl

#-------------------------------------------------------------------
# Link the executable and run the analysis
#-------------------------------------------------------------------
ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_gsi.x}
${NCP} -p ${ANALYSISEXEC} ./hafs_gsi.x

${APRUNC} ./hafs_gsi.x 1> stdout 2>&1
cat stdout

#-------------------------------------------------------------------
# Deliver files to COM
#-------------------------------------------------------------------

${NCP} -p ./stdout ${COMhafs}/${out_prefix}.analysis.stdout

# Cat runtime output files.
cat fort.2* > ${COMhafs}/${out_prefix}.analysis.gsistat

export RESTARTout=${RESTARTout:-${COMhafs}/RESTART_analysis}
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

