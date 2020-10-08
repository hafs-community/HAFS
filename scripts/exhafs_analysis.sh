#!/bin/sh

set -xe

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export FIXcrtm=${FIXcrtm:-${FIXhafs}/hwrf-crtm-2.2.6}
export COMgfs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}

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


#---------------------------------------------- 
# Link all the necessary fix files
#---------------------------------------------- 
${NLN} ${PARMgsi}/anavinfo_hafs_L${LEVS:-65} ./anavinfo
${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats

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
#${NLN} ${PARMgsi}/nam_global_satangbias.txt ./satbias_angle
${NLN} ${PARMgsi}/hwrf_satinfo.txt ./satinfo
${NLN} ${PARMgsi}/global_scaninfo.txt ./scaninfo
${NLN} ${PARMgsi}/nam_global_pcpinfo.txt ./pcpinfo
${NLN} ${PARMgsi}/global_ozinfo.txt ./ozinfo
${NLN} ${PARMgsi}/hwrf_convinfo.txt ./convinfo
${NLN} ${PARMgsi}/hwrf_nam_errtable.r3dv ./errtable
${NLN} ${PARMgsi}/atms_beamwidth.txt ./atms_beamwidth.txt
${NLN} ${PARMgsi}/prepobs_prep.bufrtable ./prepobs_prep.bufrtable
${NLN} ${PARMgsi}/bufrtab.012 ./bftab_sstphr

# Link CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
  ${NLN} ${FIXcrtm}/fix-4-hwrf/${file}.SpcCoeff.bin ./
  ${NLN} ${FIXcrtm}/fix-4-hwrf/${file}.TauCoeff.bin ./
done

# Link GFS/GDAS input and observation files
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.ssmisu.tm00.bufr_d               ssmisbufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.ssmisu.tm00.bufr_d               ssmisbufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.sevcsr.tm00.bufr_d               seviribufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.satwnd.tm00.bufr_d               satwndbufr
if [ -s ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.saphir.tm00.bufr_d ] ; then
  ${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.saphir.tm00.bufr_d             saphirbufr
elif [ -s ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.saphir.tm00.bufr_d.nr ] ; then
  ${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.saphir.tm00.bufr_d.nr          saphirbufr
else
  echo "No saphirbufr file"
fi
if [ -s ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.prepbufr ] ; then
  ${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.prepbufr                       prepbufr
elif [ -s ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.prepbufr.nr ] ; then
  ${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.prepbufr.nr                    prepbufr
else
  echo "No prepbufr file, create a link anyway"
  ${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.prepbufr.nr                    prepbufr
fi
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.1bmhs.tm00.bufr_d                mhsbufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.mtiasi.tm00.bufr_d               iasibufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.1bhrs4.tm00.bufr_d               hirs4bufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.eshrs3.tm00.bufr_d               hirs3bufrears
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.goesfv.tm00.bufr_d               gsnd1bufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.gpsro.tm00.bufr_d                gpsrobufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.crisf4.tm00.bufr_d               crisfsbufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.cris.tm00.bufr_d                 crisbufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.atms.tm00.bufr_d                 atmsbufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.esamua.tm00.bufr_d               amsuabufrears
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.1bamua.tm00.bufr_d               amsuabufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.airsev.tm00.bufr_d               airsbufr

# Need to change/update the file names
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.hdob.tm00.bufr_d                 hdobbufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.nexrad.tm00.bufr_d               l2rwbufr
${NLN} ${COMgfs}/gfs.$PDY/$cyc/gfs.t${cyc}z.tldplr.tm00.bufr_d               tldplrbufr

${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.abias           satbias_in
${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.abias_pc        satbias_pc
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.atmf003.nemsio  gfs_sigf03
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.atmf006.nemsio  gfs_sigf06
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/gdas.t${hhprior}z.atmf009.nemsio  gfs_sigf09

# Copy the first guess files
${NCP} ${WORKhafsprior}/forecast/grid_spec.nc ./fv3_grid_spec

${NCP} ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.coupler.res ./coupler.res
${NCP} ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_core.res.nc ./fv3_akbk
${NCP} ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.sfc_data.nc ./fv3_sfcdata
${NCP} ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
${NCP} ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv3_dynvars
${NCP} ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv3_tracer

if [ $hybrid_3denvar_gdas = yes ]; then
export L_HYB_ENS=.true.
export N_ENS=80
for mem in $(seq -f '%03g' 1 ${N_ENS})
do
  ${NLN} ${COMgfs}/enkfgdas.$PDYprior/${hhprior}/sfg_${CDATEprior}_fhr06s_mem${mem} ./
done
/bin/ls -1 sfg_${CDATEprior}_fhr06s_mem??? > filelist06
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

${APRUNC} ./hafs_gsi.x 1>stdout 2>&1
cat stdout
export err=$?

#-------------------------------------------------------------------
# Deliver files to COM
#-------------------------------------------------------------------
mkdir -p ${COMhafs}/RESTART_analysis
${NCP} ./coupler.res ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.coupler.res
${NCP} ./fv3_akbk ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_core.res.nc
${NCP} ./fv3_sfcdata ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.sfc_data.nc
${NCP} ./fv3_srfwnd ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc
${NCP} ./fv3_dynvars ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_core.res.tile1.nc
${NCP} ./fv3_tracer  ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc

exit $err

