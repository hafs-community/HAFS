#!/bin/sh
# exhafs_enkf.sh: Depend upon the value of ldo_enscalc_option, this script can
# perform the EnKF mean, update, and recenter functions by running the GSI's
# enkf.x in different modes. 
# ldo_enscalc_option=1: enkf_mean, calculate the ensemble mean 
# ldo_enscalc_option=0: enkf_update, condut the EnKF analysis to update the ensemble members 
# ldo_enscalc_option=2: enkf_recenter, recenter the ensemble memmber analysis around the deterministic EnVar analysis 

set -xe

export HDF5_USE_FILE_LOCKING=FALSE #avoild recenter's error "NetCDF: HDF error"
export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}

TOTAL_TASKS=${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

NDATE=${NDATE:-ndate}
NCP='/bin/cp'
NLN='ln -sf'

if [ $GFSVER = PROD2021 ]; then
  export atmos="atmos/"
elif [ $GFSVER = PROD2019 ]; then
  export atmos=""
else
  export atmos=""
fi

export ldo_enscalc_option=${ldo_enscalc_option:-1}
export nens=${ENS_SIZE:-40}

# Diagnostic files options
netcdf_diag=${netcdf_diag:-".true."}
binary_diag=${binary_diag:-".false."}

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

export COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
export WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

if [ ${RUN_GSI_VR_ENS} = YES ]; then
  export RESTARTens_inp=${COMhafs}/RESTART_analysis_vr_ens
  #export RESTARTens_anl=${COMhafs}/RESTART_analysis_vr_ens_anl
else
  export RESTARTens_inp=${COMhafsprior}/RESTART_ens
  #export RESTARTens_anl=${COMhafs}/RESTART_ens_anl
fi

export RESTARTens_anl=${COMhafs}/RESTART_analysis_ens
mkdir -p ${RESTARTens_anl}

DATA=${DATA:-${WORKhafs}/enkf_mean}
mkdir -p ${DATA}
cd ${DATA}

if [ $ldo_enscalc_option -ne 2 ]; then # enkf_mean or enkf_update
  memstr="mem001"
  ensmean="ensmean"
  cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${ensmean}_tracer
  cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${ensmean}_dynvars
  #dynvar_list="delp,DZ,phis,T,u,ua,v,va,W"
  dynvar_list=$(ncks --trd -m fv3sar_tile1_${ensmean}_dynvars | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort | grep -v 'Time' | grep -v 'axis' | paste -sd "," -)
  #tracer_list="cld_amt,graupel,ice_wat,liq_wat,o3mr,rainwat,snowwat,sphum"
  tracer_list=$(ncks --trd -m fv3sar_tile1_${ensmean}_tracer  | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort | grep -v 'Time' | grep -v 'axis' | paste -sd "," -)
  ncrename -d yaxis_1,yaxis_2 -v yaxis_1,yaxis_2 fv3sar_tile1_${ensmean}_tracer
  # somehow the yaxis_2 get default values, the following line is a temporary workaround
  ncks --no-abc -A -v yaxis_2 fv3sar_tile1_${ensmean}_dynvars fv3sar_tile1_${ensmean}_tracer
  ncks -A -v ${tracer_list} fv3sar_tile1_${ensmean}_tracer fv3sar_tile1_${ensmean}_dynvars
  mv fv3sar_tile1_${ensmean}_dynvars fv3sar_tile1_${ensmean}_dynvartracer
  cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.nc fv3sar_tile1_akbk.nc
  cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc fv3_sfcdata
  cp ${RESTARTens_inp}/${memstr}/grid_spec.nc fv3sar_tile1_grid_spec.nc

  for memstr in $(seq -f 'mem%03g' 1 $nens)
  do
    cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${memstr}_dynvars 
    cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${memstr}_tracer
    ncrename -d yaxis_1,yaxis_2 -v yaxis_1,yaxis_2 fv3sar_tile1_${memstr}_tracer
    # somehow the yaxis_2 get default values, the following line is a temporary workaround
    ncks --no-abc -A -v yaxis_2 fv3sar_tile1_${memstr}_dynvars fv3sar_tile1_${memstr}_tracer
    ncks -A -v $tracer_list fv3sar_tile1_${memstr}_tracer fv3sar_tile1_${memstr}_dynvars
    mv fv3sar_tile1_${memstr}_dynvars fv3sar_tile1_${memstr}_dynvartracer
  done
else # enkf_recenter
  ${NCP} ${RESTARTens_anl}/ensmean/grid_spec.nc fv3sar_tile1_grid_spec.nc
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_core.res.nc fv3sar_tile1_akbk.nc
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.coupler.res coupler.res
  ${NCP} ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3_dynvars
  ${NCP} ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3_tracer
  #dynvar_list="delp,DZ,phis,T,u,ua,v,va,W"
  dynvar_list=$(ncks --trd -m fv3_dynvars | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort | grep -v 'Time' | grep -v 'axis' | paste -sd "," -)
  #tracer_list="cld_amt,graupel,ice_wat,liq_wat,o3mr,rainwat,snowwat,sphum"
  tracer_list=$(ncks --trd -m fv3_tracer  | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort | grep -v 'Time' | grep -v 'axis' | paste -sd "," -)
  ncrename -d yaxis_1,yaxis_2 -v yaxis_1,yaxis_2 fv3_tracer
  # somehow the yaxis_2 get default values, the following line is a temporary workaround
  ncks --no-abc -A -v yaxis_2 fv3_dynvars fv3_tracer
  ncks -A -v ${tracer_list} fv3_tracer fv3_dynvars 
  # mem001 from deterministic EnVar analysis
  # The assumption is that the deterministic and ensemble domains are identical but with different grid resolutions
  # Also, the grid spacing ratio between the deterministic and ensemble domains is 1:GRID_RATIO_ENS, where GRID_RATIO_ENS can be 1, 2, 3, 4, 5, etc.
  if [ ${GRID_RATIO_ENS:-1} -eq 1 ]; then
    # the deterministic and ensemble domains are identical and with the same grid resolution
    mv fv3_dynvars fv3sar_tile1_mem001_dynvartracer
  else 
    # subset every GRID_RATIO_ENS grid points to obtain the deterministic EnVar analysis in ensemble resolution
    ncks -d xaxis_1,,,${GRID_RATIO_ENS} -d xaxis_2,,,${GRID_RATIO_ENS} \
         -d yaxis_1,,,${GRID_RATIO_ENS} -d yaxis_2,,,${GRID_RATIO_ENS} \
         fv3_dynvars fv3sar_tile1_mem001_dynvartracer
  fi
  # 
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_dynvartracer fv3sar_tile1_ensmean_dynvartracer

  let "nens0 = $nens"
  let "nens = $nens + 1"
  for imem in $(seq 1 $nens0)
  do
    memchar="mem"$(printf %03i $imem)
    let "memin = $imem + 1"
    meminchar="mem"$(printf %03i $memin)
    ${NCP} ${RESTARTens_anl}/${memchar}/anl_dynvartracer_${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${meminchar}_dynvartracer
  done
fi

if [ $ldo_enscalc_option -eq 0 ]; then # enkf_update

  memstr="ensmean"
  RADSTAT=${RESTARTens_anl}/${memstr}/analysis.radstat
  CNVSTAT=${RESTARTens_anl}/${memstr}/analysis.cnvstat
  tar -xvf $RADSTAT
  tar -xvf $CNVSTAT

for memstr in $(seq -f "mem%03g" 1 $nens)
do
  RADSTAT=${RESTARTens_anl}/${memstr}/analysis.radstat
  CNVSTAT=${RESTARTens_anl}/${memstr}/analysis.cnvstat
  tar -xvf $RADSTAT
  tar -xvf $CNVSTAT
done

for gzfile in $(/bin/ls diag*ges*.gz) 
do
  gzip -d  $gzfile
  rm -f $gzfile
done

fi

${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats
#checkgfs $NLN $RADCLOUDINFO cloudy_radiance_info.txt
${NLN} ${PARMgsi}/atms_beamwidth.txt ./atms_beamwidth.txt
#${NLN} ${PARMgsi}/anavinfo_hafs_L${LEVS:-65} ./anavinfo
#checkgfs $NLN $vqcdat       vqctp001.dat
#checkgfs $NLN $INSITUINFO   insituinfo
${NLN} ${PARMgsi}/nam_global_pcpinfo.txt ./pcpinfo
#checkgfs $NLN $AEROINFO     aeroinfo
#checkgfs $NLN $HYBENSINFO   hybens_info
${NLN} ${PARMgsi}/hwrf_nam_errtable.r3dv ./errtable

if [ $ldo_enscalc_option -eq 1 -o $ldo_enscalc_option -eq 2 ]; then # enkf_mean or enkf_recenter
  #anavinfo=${PARMgsi}/anavinfo_hafs_enkf_ensmean_L${LEVS:-65}
  #anavinfo=${PARMgsi}/anavinfo_hafs_enkf_L${LEVS:-65}
  anavinfo=${PARMgsi}/anavinfo_hafs_enkf_tmp
else # enkf_update
  #anavinfo=${PARMgsi}/anavinfo_hafs_enkf_L${LEVS:-65}
  anavinfo=${PARMgsi}/anavinfo_hafs_enkf_tmp
fi
#${NCP} ${anavinfo} ./anavinfo
sed -e "s/_LEV_/${npz:-64}/g" \
    -e "s/_LP1_/${LEVS:-65}/g" \
    ${anavinfo} > ./anavinfo

${NLN} ${PARMgsi}/hwrf_satinfo.txt ./satinfo
${NLN} ${PARMgsi}/global_scaninfo.txt ./scaninfo
#${NLN} ${PARMgsi}/nam_global_satangbias.txt ./satbias_angle
${NLN} ${PARMgsi}/global_ozinfo.txt ./ozinfo
${NLN} ${PARMgsi}/hwrf_convinfo.txt ./convinfo

${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias           satbias_in
${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_pc        satbias_pc
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_air       satbias_air

# Make enkf namelist
cat > enkf.nml << EOFnml
&nam_enkf
   datestring="$CDATE",datapath="./",
   analpertwtnh=0.85,analpertwtsh=0.85,analpertwttr=0.85,
   covinflatemax=1.e2,covinflatemin=1,pseudo_rh=.true.,iassim_order=0,
   corrlengthnh=400,corrlengthsh=400,corrlengthtr=400,
   lnsigcutoffnh=0.5,lnsigcutoffsh=0.5,lnsigcutofftr=0.5,
   lnsigcutoffpsnh=0.5,lnsigcutoffpssh=0.5,lnsigcutoffpstr=0.5,
   lnsigcutoffsatnh=0.5,lnsigcutoffsatsh=0.5,lnsigcutoffsattr=0.5,
   obtimelnh=1.e30,obtimelsh=1.e30,obtimeltr=1.e30,
   saterrfact=1.0,numiter=1,
   sprd_tol=1.e30,paoverpb_thresh=0.98,
   nlons=$((${npx_ens:-$npx}-1)),nlats=$((${npy_ens:-$npy}-1)),nlevs=${npz_ens:-$npz},nanals=${nens},
   deterministic=.true.,sortinc=.true.,lupd_satbiasc=.false.,
   reducedgrid=.true.,readin_localization=.false.,
   use_gfs_nemsio=.false.,use_gfs_ncio=.false.,imp_physics=11,lupp=.false.,
   univaroz=.false.,adp_anglebc=.true.,angord=4,use_edges=.false.,emiss_bc=.true.,
   letkf_flag=.false.,nobsl_max=10000,denkf=.false.,getkf=.false.,
   nhr_anal=6,nhr_state=6,use_qsatensmean=.false.,
   lobsdiag_forenkf=.false.,
   write_spread_diag=.false.,
   modelspace_vloc=.false.,
   use_correlated_oberrs=.false.,
   netcdf_diag=${netcdf_diag},cnvw_option=.false.,
   paranc=.false.,write_fv3_incr=.false.,
   incvars_to_zero='NONE',
   smoothparm=-1,
   ldo_enscalc_option=${ldo_enscalc_option},
/

&satobs_enkf
   sattypes_rad(1) = 'amsua_n15',     dsis(1) = 'amsua_n15',
   sattypes_rad(2) = 'amsua_n18',     dsis(2) = 'amsua_n18',
   sattypes_rad(3) = 'amsua_n19',     dsis(3) = 'amsua_n19',
   sattypes_rad(4) = 'amsub_n16',     dsis(4) = 'amsub_n16',
   sattypes_rad(5) = 'amsub_n17',     dsis(5) = 'amsub_n17',
   sattypes_rad(6) = 'amsua_aqua',    dsis(6) = 'amsua_aqua',
   sattypes_rad(7) = 'amsua_metop-a', dsis(7) = 'amsua_metop-a',
   sattypes_rad(8) = 'airs_aqua',     dsis(8) = 'airs_aqua',
   sattypes_rad(9) = 'hirs3_n17',     dsis(9) = 'hirs3_n17',
   sattypes_rad(10)= 'hirs4_n19',     dsis(10)= 'hirs4_n19',
   sattypes_rad(11)= 'hirs4_metop-a', dsis(11)= 'hirs4_metop-a',
   sattypes_rad(12)= 'mhs_n18',       dsis(12)= 'mhs_n18',
   sattypes_rad(13)= 'mhs_n19',       dsis(13)= 'mhs_n19',
   sattypes_rad(14)= 'mhs_metop-a',   dsis(14)= 'mhs_metop-a',
   sattypes_rad(15)= 'goes_img_g11',  dsis(15)= 'imgr_g11',
   sattypes_rad(16)= 'goes_img_g12',  dsis(16)= 'imgr_g12',
   sattypes_rad(17)= 'goes_img_g13',  dsis(17)= 'imgr_g13',
   sattypes_rad(18)= 'goes_img_g14',  dsis(18)= 'imgr_g14',
   sattypes_rad(19)= 'goes_img_g15',  dsis(19)= 'imgr_g15',
   sattypes_rad(20)= 'avhrr_n18',     dsis(20)= 'avhrr3_n18',
   sattypes_rad(21)= 'avhrr_metop-a', dsis(21)= 'avhrr3_metop-a',
   sattypes_rad(22)= 'avhrr_n19',     dsis(22)= 'avhrr3_n19',
   sattypes_rad(23)= 'amsre_aqua',    dsis(23)= 'amsre_aqua',
   sattypes_rad(24)= 'ssmis_f16',     dsis(24)= 'ssmis_f16',
   sattypes_rad(25)= 'ssmis_f17',     dsis(25)= 'ssmis_f17',
   sattypes_rad(26)= 'ssmis_f18',     dsis(26)= 'ssmis_f18',
   sattypes_rad(27)= 'ssmis_f19',     dsis(27)= 'ssmis_f19',
   sattypes_rad(28)= 'ssmis_f20',     dsis(28)= 'ssmis_f20',
   sattypes_rad(29)= 'sndrd1_g11',    dsis(29)= 'sndrD1_g11',
   sattypes_rad(30)= 'sndrd2_g11',    dsis(30)= 'sndrD2_g11',
   sattypes_rad(31)= 'sndrd3_g11',    dsis(31)= 'sndrD3_g11',
   sattypes_rad(32)= 'sndrd4_g11',    dsis(32)= 'sndrD4_g11',
   sattypes_rad(33)= 'sndrd1_g12',    dsis(33)= 'sndrD1_g12',
   sattypes_rad(34)= 'sndrd2_g12',    dsis(34)= 'sndrD2_g12',
   sattypes_rad(35)= 'sndrd3_g12',    dsis(35)= 'sndrD3_g12',
   sattypes_rad(36)= 'sndrd4_g12',    dsis(36)= 'sndrD4_g12',
   sattypes_rad(37)= 'sndrd1_g13',    dsis(37)= 'sndrD1_g13',
   sattypes_rad(38)= 'sndrd2_g13',    dsis(38)= 'sndrD2_g13',
   sattypes_rad(39)= 'sndrd3_g13',    dsis(39)= 'sndrD3_g13',
   sattypes_rad(40)= 'sndrd4_g13',    dsis(40)= 'sndrD4_g13',
   sattypes_rad(41)= 'sndrd1_g14',    dsis(41)= 'sndrD1_g14',
   sattypes_rad(42)= 'sndrd2_g14',    dsis(42)= 'sndrD2_g14',
   sattypes_rad(43)= 'sndrd3_g14',    dsis(43)= 'sndrD3_g14',
   sattypes_rad(44)= 'sndrd4_g14',    dsis(44)= 'sndrD4_g14',
   sattypes_rad(45)= 'sndrd1_g15',    dsis(45)= 'sndrD1_g15',
   sattypes_rad(46)= 'sndrd2_g15',    dsis(46)= 'sndrD2_g15',
   sattypes_rad(47)= 'sndrd3_g15',    dsis(47)= 'sndrD3_g15',
   sattypes_rad(48)= 'sndrd4_g15',    dsis(48)= 'sndrD4_g15',
   sattypes_rad(49)= 'iasi_metop-a',  dsis(49)= 'iasi_metop-a',
   sattypes_rad(50)= 'seviri_m08',    dsis(50)= 'seviri_m08',
   sattypes_rad(51)= 'seviri_m09',    dsis(51)= 'seviri_m09',
   sattypes_rad(52)= 'seviri_m10',    dsis(52)= 'seviri_m10',
   sattypes_rad(53)= 'seviri_m11',    dsis(53)= 'seviri_m11',
   sattypes_rad(54)= 'amsua_metop-b', dsis(54)= 'amsua_metop-b',
   sattypes_rad(55)= 'hirs4_metop-b', dsis(55)= 'hirs4_metop-b',
   sattypes_rad(56)= 'mhs_metop-b',   dsis(56)= 'mhs_metop-b',
   sattypes_rad(57)= 'iasi_metop-b',  dsis(57)= 'iasi_metop-b',
   sattypes_rad(58)= 'avhrr_metop-b', dsis(58)= 'avhrr3_metop-b',
   sattypes_rad(59)= 'atms_npp',      dsis(59)= 'atms_npp',
   sattypes_rad(60)= 'atms_n20',      dsis(60)= 'atms_n20',
   sattypes_rad(61)= 'cris_npp',      dsis(61)= 'cris_npp',
   sattypes_rad(62)= 'cris-fsr_npp',  dsis(62)= 'cris-fsr_npp',
   sattypes_rad(63)= 'cris-fsr_n20',  dsis(63)= 'cris-fsr_n20',
   sattypes_rad(64)= 'gmi_gpm',       dsis(64)= 'gmi_gpm',
   sattypes_rad(65)= 'saphir_meghat', dsis(65)= 'saphir_meghat',
   sattypes_rad(66)= 'amsua_metop-c', dsis(66)= 'amsua_metop-c',
   sattypes_rad(67)= 'mhs_metop-c',   dsis(67)= 'mhs_metop-c',
   sattypes_rad(68)= 'ahi_himawari8', dsis(68)= 'ahi_himawari8',
   sattypes_rad(69)= 'abi_g16',       dsis(69)= 'abi_g16',
   sattypes_rad(70)= 'abi_g17',       dsis(70)= 'abi_g17',
/

&ozobs_enkf
   sattypes_oz(1) = 'sbuv2_n16',
   sattypes_oz(2) = 'sbuv2_n17',
   sattypes_oz(3) = 'sbuv2_n18',
   sattypes_oz(4) = 'sbuv2_n19',
   sattypes_oz(5) = 'omi_aura',
   sattypes_oz(6) = 'gome_metop-a',
   sattypes_oz(7) = 'gome_metop-b',
   sattypes_oz(8) = 'mls30_aura',
   sattypes_oz(9) = 'ompsnp_npp',
   sattypes_oz(10) = 'ompstc8_npp',
/

&nam_fv3
   fv3fixpath="./",nx_res=$((${npx_ens:-$npx}-1)),ny_res=$((${npy_ens:-$npy}-1)),ntiles=1,
/

EOFnml

ENKFEXEC=${ENKFEXEC:-$HOMEhafs/exec/hafs_enkf.x}
cp -p $ENKFEXEC ./enkf.x

# In case there are memory issues to run enkf_mean/enkf_recenter for all
# variables, can consider separating the enkf control variables into three
# parts
#if [ $ldo_enscalc_option -ne 0  ]; then # enkf_mean or enkf_recenter
#  let i=1
#  #for infile in $(/bin/ls ${anavinfo}_p*)
#  for infile in $(/bin/ls ${anavinfo})
#  do
#    #${NCP} $infile anavinfo
#    sed -e "s/_LEV_/${npz:-64}/g" \
#        -e "s/_LP1_/${LEVS:-65}/g" \
#        ${infile} > ./anavinfo
#    ${APRUNC}  ./enkf.x < enkf.nml > stdout_p${i} 2>&1
#    let i=i+1
#  done
#else
#  ${APRUNC} ./enkf.x < enkf.nml > stdout 2>&1
#fi

${APRUNC} ./enkf.x < enkf.nml > stdout 2>&1

if [ $ldo_enscalc_option -eq 0 ]; then # enkf_update
  for memstr in $(seq -f "mem%03g" 1 $nens)
  do
    mkdir -p ${RESTARTens_anl}/${memstr}
    ${NCP} fv3sar_tile1_${memstr}_dynvartracer ${RESTARTens_anl}/${memstr}/anl_dynvartracer_${PDY}.${cyc}0000.fv_core.res.tile1.nc  
  done
elif  [ $ldo_enscalc_option -eq 1 ]; then # enkf_mean 
  memstr="ensmean"
  mkdir -p ${RESTARTens_anl}/${memstr}
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_core.res.tile1.nc ${RESTARTens_anl}/${memstr}/
  ncks -A -v $dynvar_list fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/${memstr}/
  ncks -A -v $tracer_list fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ncks --no-abc -O -x -v yaxis_2 ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ${NCP} fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_dynvartracer # used by recenter 
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/${memstr}/
  #${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.phy_data.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/grid_spec.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/oro_data.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/atmos_static.nc ${RESTARTens_anl}/${memstr}/
elif [ $ldo_enscalc_option -eq 2 ]; then # enkf_recenter
  mkdir -p ${RESTARTens_anl}/anlmean
  cp ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_core.res.tile1.nc ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_core.res.tile1.nc
  ncks -A -v $dynvar_list fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_core.res.tile1.nc
  cp ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ncks -A -v $tracer_list fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ncks --no_abc -O -x -v yaxis_2 ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  #cp ${COMhafs}/RESTART_analysis/{*grid_spec.nc,*sfc_data.nc,*coupler.res,gfs_ctrl.nc,fv_core.res.nc,*bndy*} ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/grid_spec.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/oro_data.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/atmos_static.nc ${RESTARTens_anl}/anlmean/

  for imem in $(seq 2 $nens)
  do
    memstr="mem"$(printf %03i $imem)
    memout="mem"$(printf %03i $(($imem-1)))
    mkdir -p ${RESTARTens_anl}/${memout}
    cp ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
    ncks -A -v $dynvar_list fv3sar_tile1_${memstr}_dynvartracer ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
    cp ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
    ncks -A -v $tracer_list fv3sar_tile1_${memstr}_dynvartracer ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
    ncks --no_abc -O -x -v yaxis_2 ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
    #cp ${COMhafs}/RESTART_analysis/{*grid_spec.nc,*sfc_data.nc,*coupler.res,gfs_ctrl.nc,fv_core.res.nc,*bndy*} ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/grid_spec.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/oro_data.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/atmos_static.nc ${RESTARTens_anl}/${memout}/
  done
else
  echo "Wrong ldo_enscalc_option: $ldo_enscalc_option"
fi

exit
