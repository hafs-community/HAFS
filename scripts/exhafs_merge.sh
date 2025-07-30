#!/bin/sh
################################################################################
# Script Name: exhafs_merge.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs hafs_datool to merge atmospheric restart files. It
#   supports the merge_type of analysis or init with the merge_method of
#   domainmerge or vortexreplace.
# History:
#   01/12/2022: Enable cycling the storm region only or cycling the whole domain
#               from the prior HAFS forecast cycle
#   02/26/2022: Enable handling the regional moving nesting configuration
#   03/09/2023: Improvements for HAFSv1 operational implementation
#   04/20/2024: Improve error handling and stdout/stderr redirection for HAFSv2
#   07/12/2024: Add 3DIAU related capabilities for HAFS regional configuration
#   12/16/2024: Add Fourier wave-number filtering capabilities for DA increments
#   06/30/2024: Merge with HFSM (Multistorm) workflow changes
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
################################################################################
set -x -o pipefail

# # Suggested by Raghu Reddy (RDHPCS) to diagnose potential slow nodes... (Lew.Gramer@noaa.gov 2025-06-25)
# pdsh -w "$SLURM_JOB_NODELIST" /home/role.regress/S2/Testsuite/STREAM/check-node.sh

FGAT_MODEL=${FGAT_MODEL:-gfs}
FGAT_HR=${FGAT_HR:-00}

MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_tools_mpiserial.x}
DATOOL=${DATOOL:-${EXEChafs}/hafs_tools_datool.x}
SENDCOM=${SENDCOM:-YES}

# Merge analysis or init
if [ ${MERGE_TYPE} = analysis ]; then

    merge_method=${analysis_merge_method:-vortexreplace}
    # Deterministic or ensemble
    if [ "${ENSDA}" = YES ]; then
	if [ -d ${WORKhafs}/intercom/RESTART_analysis_ens/mem${ENSID} ]; then
	    RESTARTsrc=${WORKhafs}/intercom/RESTART_analysis_ens/mem${ENSID}
	elif [ -d ${WORKhafs}/intercom/RESTART_vi_ens/mem${ENSID} ]; then
	    RESTARTsrc=${WORKhafs}/intercom/RESTART_vi_ens/mem${ENSID}
	else
	    echo "FATAL ERROR: RESTARTsrc does not exist"
	    exit 1
	fi
	RESTARTdst=${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}
	RESTARTmrg=${WORKhafs}/intercom/RESTART_analysis_merge_ens/mem${ENSID}
	RESTARTcom=${COMhafs}/${out_prefix}.RESTART_analysis_merge_ens/mem${ENSID}
    else
	if [ -e ${WORKhafs}/intercom/RESTART_analysis ]; then
	    RESTARTsrc=${WORKhafs}/intercom/RESTART_analysis
	elif [ -e ${WORKhafs}/intercom/RESTART_vi ]; then
	    RESTARTsrc=${WORKhafs}/intercom/RESTART_vi
	    # Lew.Gramer@noaa.gov 2024-03-01
	elif [ -e ${WORKhafs}/intercom/RESTART_init ]; then
	    RESTARTsrc=${WORKhafs}/intercom/RESTART_init
	    #LJG
	else
	    echo "FATAL ERROR: RESTARTsrc does not exist"
	    exit 1
	fi
	RESTARTdst=${WORKhafs}/intercom/RESTART_init
	RESTARTmrg=${WORKhafs}/intercom/RESTART_analysis_merge
	RESTARTcom=${COMhafs}/${out_prefix}.RESTART_analysis_merge
    fi #if [ "${ENSDA}" = YES ]; then

    #if [ ${MERGE_TYPE} = analysis ]; then
elif [ ${MERGE_TYPE} = init ]; then

    merge_method=${atm_merge_method:-vortexreplace}
    if [ ${FGAT_MODEL} = gdas ]; then
	RESTARTsrc=${COMOLD}/${old_out_prefix}.RESTART
	RESTARTdst=${WORKhafs}/intercom/RESTART_init_fgat${FGAT_HR}
	RESTARTmrg=${WORKhafs}/intercom/RESTART_merge_fgat${FGAT_HR}
	# Ghassan.Alaka@noaa.gov, 2024-07-12: CDATE could be undefined, but YMDH always is.
	#CDATE=$(${NDATE} $(awk "BEGIN {print ${FGAT_HR}-6}") $CDATE)
	CDATE=$(${NDATE} $(awk "BEGIN {print ${FGAT_HR}-6}") ${CDATE:-${YMDH}})
	# GJA
    else
	RESTARTsrc=${COMOLD}/${old_out_prefix}.RESTART
	RESTARTdst=${WORKhafs}/intercom/RESTART_init
	RESTARTmrg=${WORKhafs}/intercom/RESTART_merge
    fi

    #if [ ${MERGE_TYPE} = analysis ]; then
    #elif [ ${MERGE_TYPE} = init ]; then
else

    echo "FATAL ERROR: unsupported MERGE_TYPE: ${MERGE_TYPE}"
    exit 1

fi # if [ ${MERGE_TYPE} = analysis ]; then ... elif ... else

CDATE=${CDATE:-$YMDH}
ymd=$(echo $CDATE | cut -c1-8)
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)
hh=$(echo $CDATE | cut -c9-10)

# Ghassan.Alaka@noaa.gov, 2024-07-12: always parse multistorm_sids into an array if running a multistorm
if [ ${RUN_MULTISTORM} == "YES" ]; then
    echo "DEBUG:: multistorm_sids=${multistorm_sids} (non-array)"
    #multistorm_sids=( $(echo ${multistorm_sids} | sed 's/,/ /g') ) #GJA: convert multistorm_sids to an array
    # Lew.Gramer@noaa.gov per JiaYi.Peng@noaa.gov: bug fix for merge of storms[3-N]
    multistorm_sids=`echo ${multistorm_sids} | tr ',' ' '`
    echo "DEBUG:: multistorm_sids=${multistorm_sids}"
fi
# GJA

DATA=${DATA:-${WORKhafs}/merge}

cd ${DATA}

mkdir -p ${RESTARTmrg}
${NCP} -rp ${RESTARTdst}/* ${RESTARTmrg}/

if [ -d ${RESTARTsrc} ] || [ -L ${RESTARTsrc} ]; then

    if [ ${FGAT_HR} = 03 ]; then
	tcvital=${WORKhafs}/intercom/launch/tm03vit
    elif [ ${FGAT_HR} = 06 ]; then
	tcvital=${WORKhafs}/intercom/launch/tmpvit
    elif [ ${FGAT_HR} = 09 ]; then
	tcvital=${WORKhafs}/intercom/launch/tp03vit
    else
	tcvital=${WORKhafs}/intercom/launch/tmpvit
    fi
    if [ ${merge_method} = vortexreplace ]; then
	MERGE_CMD="${APRUNC} ${DATOOL} vortexreplace --tcvital=${tcvital} --infile_date=${ymd}.${hh}0000 --vortexradius=650:700"
    elif [ ${merge_method} = domainmerge ]; then
	MERGE_CMD="${APRUNC} ${DATOOL} remap"
    else
	echo "FATAL ERROR: unsupported merge_method: ${merge_method}"
	exit 1
    fi

    # Regional single domain configuration
    if [[ $nest_grids -eq 1 ]]; then

	#for var in fv_core.res.tile1 fv_tracer.res.tile1 fv_srf_wnd.res.tile1 sfc_data phy_data; do
	for var in fv_core.res.tile1 fv_tracer.res.tile1 fv_srf_wnd.res.tile1 sfc_data; do
	    in_grid=${RESTARTsrc}/grid_spec.nc
	    out_grid=${RESTARTmrg}/grid_spec.nc
	    in_file=${RESTARTsrc}/${ymd}.${hh}0000.${var}.nc
	    out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nc
	    if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
		   [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
		echo "FATAL ERROR: Missing in/out_grid or in/out_file"
		exit 1
	    fi
	    ${MERGE_CMD} \
		--in_grid=${in_grid} \
		--out_grid=${out_grid} \
		--in_file=${in_file} \
		--out_file=${out_file} 2>&1 | tee ./merge_regional_${var}.log
	    export err=$?; err_chk
	done

	# Lew.Gramer@noaa.gov 2024-01-25
	# Regional with one or more nest(s) configuration
	# The following steps are needed
	#   Step 1: merge srcd01 into dstd02 (for atm_merge) or merge srcd02 into srcd01 (for analysis_merge)
	#   Step 2: merge srcd01 into dstd01
	#   Step 3: merge srcd02 into dstd02
	#elif [[ $nest_grids -eq 2 ]]; then
	#if [[ $nest_grids -eq 1 ]]; then
        
    # Lew.Gramer@noaa.gov 2025-06-30
    elif [[ $nest_grids -ge 2 ]]; then

	RESTARTtmp=${DATA}/RESTARTtmp
	mkdir -p ${RESTARTtmp}

	if [ ${MERGE_TYPE} = analysis ]; then

	    if [ ${RUN_MULTISTORM} == "YES" ] && [ "${STORMID^^}" == "00L" ]; then
		#echo "DEBUG:: multistorm_sids=${multistorm_sids} (non-array)"
		#multistorm_sids=( $(echo ${multistorm_sids} | sed 's/,/ /g') ) #GJA: convert multistorm_sids to an array
		#echo "DEBUG:: multistorm_sids=${multistorm_sids}"
		
		# Step 1: merge each src0[2-N] nest analysis into src01 parent domain (for analysis_merge)
		${NCP} -rp ${RESTARTdst}/* ${RESTARTtmp}/
		tileno=2
		for sid in ${multistorm_sids} ; do
		    RESTARTNESTtmp=${DATA}/RESTARTtmp${sid}
		    mkdir -p ${RESTARTNESTtmp}
		    echo "DEBUG:: RESTARTNESTtmp=${RESTARTNESTtmp}"
		    
		    # Ghassan.Alaka@noaa.gov 2024-07-05
		    # Update the MERGE Command by pointing to the appropriate TCVitals file
		    # Merge the src0[2-N] nest analyses directly into dst01 (RESTARTmrg) 
		    WORKhafs_nest=${WORKhafs/00L/${sid}/}
		    tcvital_nest=${WORKhafs_nest}/tmpvit
		    if [ ${merge_method} = vortexreplace ]; then
			MERGE_CMD_NEST="${APRUNC} ${DATOOL} vortexreplace --tcvital=${tcvital_nest} --infile_date=${ymd}.${hh}0000 --vortexradius=650:700"
		    else
			MERGE_CMD_NEST="${MERGE_CMD}"
		    fi
		    if [ -e ${WORKhafs_nest}/intercom/RESTART_analysis ]; then
			RESTARTNESTsrc=${WORKhafs_nest}/intercom/RESTART_analysis
		    elif [ -e ${WORKhafs_nest}/intercom/RESTART_vi ]; then
			RESTARTNESTsrc=${WORKhafs_nest}/intercom/RESTART_vi
		    elif [ -e ${WORKhafs_nest}/intercom/RESTART_init ]; then
			RESTARTNESTsrc=${WORKhafs_nest}/intercom/RESTART_init
		    else
			echo "FATAL ERROR: RESTARTNESTsrc does not exist"
			exit 1
		    fi
		    #RESTARTNESTdst=${RESTARTdst/00L/${sid}/}
		    
		    #${NCP} -rp ${RESTARTNESTsrc}/* ${RESTARTNESTtmp}/
		    # Lew.Gramer@noaa.gov 2025-03-25: fix to accommodate JiaYi's fix!
		    for srcf in ${RESTARTNESTsrc}/*; do
			dstf=$(basename ${srcf} | sed -s s/nest02/nest0${tileno}/ | sed -s s/tile2/tile${tileno}/)
			echo "DEBUG:: ${NCP} -rp ${srcf} ${RESTARTNESTtmp}/${dstf}"
			${NCP} -rp ${srcf} ${RESTARTNESTtmp}/${dstf}
		    done
		    # LJG
		    find ${RESTARTdst} \( ! -name "*nest0[0-9]*" \) -exec ${NCP} -rp {} ${RESTARTNESTtmp}/ \;
		    in_grid=${RESTARTNESTtmp}/grid_mspec.nest0${tileno}_${yr}_${mn}_${dy}_${hh}.tile${tileno}.nc
		    #out_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
		    out_grid=${RESTARTmrg}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
		    
		    for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
			in_file=${RESTARTNESTtmp}/${ymd}.${hh}0000.${var}.nest0${tileno}.tile${tileno}.nc
			if [[ $var = sfc_data ]]; then
			    #out_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nc
			    out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nc
			else
			    #out_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.tile1.nc
			    out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.tile1.nc
			fi
			if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
			       [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
			    echo "FATAL ERROR: Missing in/out_grid or in/out_file"
			    exit 1
			fi
			#${MERGE_CMD}
			${MERGE_CMD_NEST} \
			    --in_grid=${in_grid} \
			    --out_grid=${out_grid} \
			    --in_file=${in_file} \
			    --out_file=${out_file}
			status=$?; [[ $status -ne 0 ]] && exit $status
		    done
		    #GJA
		    tileno=$((tileno+1))
		    #for sid in ${multistorm_sids} ; do
		done
		
		#LJG
		#if [ ${RUN_MULTISTORM} == "YES" ] && [ "${STORMID^^}" == "00L" ]; then
	    else
		# Step 1: merge srcd02 into srcd01 (for analysis_merge)
		${NCP} -rp ${RESTARTsrc}/* ${RESTARTtmp}/
		for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
		    in_grid=${RESTARTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
		    out_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
		    in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
		    if [[ $var = sfc_data ]]; then
			out_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nc
		    else
			out_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.tile1.nc
		    fi
		    if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
			   [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
			echo "FATAL ERROR: Missing in/out_grid or in/out_file"
			exit 1
		    fi
		    ${MERGE_CMD} \
			--in_grid=${in_grid} \
			--out_grid=${out_grid} \
			--in_file=${in_file} \
			--out_file=${out_file} 2>&1 | tee ./merge_analysis_step1_${var}.log
		    export err=$?; err_chk
		    #for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
		done
		#if [ ${RUN_MULTISTORM} == "YES" ] && [ "${STORMID^^}" == "00L" ]; then ... else
	    fi

	    #if [ ${MERGE_TYPE} = analysis ]; then
	elif [ ${MERGE_TYPE} = init ]; then

	    # Step 1: merge srcd02 into srcd01 (for atm_merge)
	    ${RLN} ${RESTARTsrc}/* ${RESTARTtmp}/
	    for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
		in_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
		out_grid=${RESTARTmrg}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
		if [[ $var = sfc_data ]]; then
		    in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nc
		else
		    in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.tile1.nc
		fi
		out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
		if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
		       [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
		    echo "FATAL ERROR: Missing in/out_grid or in/out_file"
		    exit 1
		fi
		${MERGE_CMD} \
		    --in_grid=${in_grid} \
		    --out_grid=${out_grid} \
		    --in_file=${in_file} \
		    --out_file=${out_file} 2>&1 | tee ./merge_init_step1_${var}.log
		export err=$?; err_chk
		#for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
	    done

	    #if [ ${MERGE_TYPE} = analysis ]; then
	    #elif [ ${MERGE_TYPE} = init ]; then
	else
	    echo "FATAL ERROR: unsupported MERGE_TYPE: ${MERGE_TYPE}"
	    exit 1
	fi

	# Step 2: merge srcd01 into dstd01
	# Ghassan.Alaka@noaa.gov 2024-07-05
	# Only do this step for real storms
	# For multistorm, don't merge srcd01 (from storm-centric pre-processing) into dstd01 (domain-centric forecast domain)
	#if [ ${RUN_MULTISTORM} == "NO" ]; then
	if [ "${STORMID}" != "00L" ]; then
	    for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
		in_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
		out_grid=${RESTARTmrg}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
		if [[ $var = sfc_data ]]; then
		    in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nc
		    out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nc
		else
		    in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.tile1.nc
		    out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.tile1.nc
		fi
		if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
		       [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
		    echo "FATAL ERROR: Missing in/out_grid or in/out_file"
		    exit 1
		fi
		${MERGE_CMD} \
		    --in_grid=${in_grid} \
		    --out_grid=${out_grid} \
		    --in_file=${in_file} \
		    --out_file=${out_file} 2>&1 | tee ./merge_init_step2_${var}.log
		export err=$?; err_chk
		#for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
	    done
	fi

	# Step 3: merge srcd02 into dstd02
	# Lew.Gramer@noaa.gov 2024-01-22
	# Ghassan.Alaka@noaa.gov, 2024-07-12: add dependence on SID=00L
	if [ ${RUN_MULTISTORM} == "YES" ] && [ "${STORMID^^}" == "00L" ]; then
	    # GJA
	    tileno=2
	    for sid in ${multistorm_sids} ; do
		RESTARTNESTtmp=${DATA}/RESTARTtmp${sid}
                # Lew.Gramer@noaa.gov 2025-06-30
		# Ghassan.Alaka@noaa.gov 2024-07-05
		WORKhafs_nest=${WORKhafs/00L/${sid}/}
		tcvital_nest=${WORKhafs_nest}/tmpvit
		if [ ${merge_method} = vortexreplace ]; then
		    MERGE_CMD_NEST="${APRUNC} ${DATOOL} vortexreplace --tcvital=${tcvital_nest} --infile_date=${ymd}.${hh}0000 --vortexradius=650:700"
		else
		    MERGE_CMD_NEST="${MERGE_CMD}"
		fi
                # LJG
		for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
		    # in_grid=${RESTARTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
		    # out_grid=${RESTARTmrg}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
		    # in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
		    # out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
		    # Lew.Gramer@noaa.gov 2025-03-25: fix to accommodate JiaYi's fix!
		    #in_grid=${RESTARTNESTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
		    in_grid=${RESTARTNESTtmp}/grid_mspec.nest0${tileno}_${yr}_${mn}_${dy}_${hh}.tile${tileno}.nc
		    out_grid=${RESTARTmrg}/grid_mspec.nest0${tileno}_${yr}_${mn}_${dy}_${hh}.tile${tileno}.nc
		    #in_file=${RESTARTNESTtmp}/${PDY}.${cyc}0000.${var}.nest02.tile2.nc
		    in_file=${RESTARTNESTtmp}/${PDY}.${cyc}0000.${var}.nest0${tileno}.tile${tileno}.nc
		    out_file=${RESTARTmrg}/${PDY}.${cyc}0000.${var}.nest0${tileno}.tile${tileno}.nc
		    # LJG
		    if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
			   [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
			echo "FATAL ERROR: Missing in/out_grid or in/out_file. Exiting..."
			exit 1
		    fi
		    #${MERGE_CMD}
		    ${MERGE_CMD_NEST} \
			--in_grid=${in_grid} \
			--out_grid=${out_grid} \
			--in_file=${in_file} \
			--out_file=${out_file} 2>&1 | tee ./merge_init_step3_${var}.log
		    export err=$?; err_chk
		    #for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
		done
		# GJA
		tileno=$((tileno+1))
		#for sid in ${multistorm_sids} ; do
	    done
	else
	    # LJG

	    for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
		in_grid=${RESTARTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
		out_grid=${RESTARTmrg}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
		in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
		out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
		if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
		       [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
		    echo "FATAL ERROR: Missing in/out_grid or in/out_file"
		    exit 1
		fi
		${MERGE_CMD} \
		    --in_grid=${in_grid} \
		    --out_grid=${out_grid} \
		    --in_file=${in_file} \
		    --out_file=${out_file}
		status=$?; [[ $status -ne 0 ]] && exit $status

	    done
	    # Lew.Gramer@noaa.gov 2024-01-22
	fi
	# LJG

    else
	echo "FATAL ERROR: only support nest_grids = 1 or >=2"
	echo "FATAL ERROR: nest_grids = $nest_grids"
	exit 1
    fi

else

    echo "RESTARTsrc: ${RESTARTsrc} does not exist"
    echo "RESTARTmrg is the same as RESTARTdst"

fi

if [ ${RUN_GSI} = "YES" ] && [ ${GSI_D02} = "YES" ]; then

    # Lew.Gramer@noaa.gov 2025-06-30
    # Step 4: Calculate d02 increments for IAU
    # Extract vmax from tcvitals (m/s)
    ${NCP} ${WORKhafs}/intercom/launch/tmpvit tcvitals
    vmax_vit=$(cat tcvitals | cut -c68-69 | bc -l)
    # LJG
    export err=$?; err_chk
    if [ ${vmax_vit} -gt ${fwd_vmax_threshold:-33} ]; then
      wave_num=${fwd_wave_number:-2}
    else
      wave_num=-999
    fi
    
    if [ ${iau_regional:-.false.} = ".true." ] || [ ${wave_num} -gt "-99" ]; then
      RESTARTbkg=${WORKhafs}/intercom/RESTART_vi
      in_grid=${RESTARTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
      iau_fwd_command="${APRUNO} ${DATOOL} fftw_iau --vars=u:v:delp:DZ:T:sphum"
      if [ -s ./tcvitals ]; then
        iau_fwd_command="${iau_fwd_command} --tcvital=./tcvitals"
      fi
      if [ -s ${in_grid} ]; then
        iau_fwd_command="${iau_fwd_command} --in_grid=${in_grid}"
      fi
      if [ ${iau_regional} = ".true." ]; then
        iau_fwd_command="${iau_fwd_command} --out_file=./analysis_inc_nest02.nc"
      fi
      if [ ${wave_num} -gt "-99" -a ${wave_num} -lt "99" ]; then
        iau_fwd_command="${iau_fwd_command} --wave_num=${wave_num}"
      fi
      #for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
      for var in fv_core.res fv_tracer.res; do
        ${iau_fwd_command} \
             --bg_file=${RESTARTbkg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc \
             --an_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc 2>&1 | tee ./analysis_fftw_iau.${var}.log
        export err=$?; err_chk
      done
      if [ ${iau_regional} = ".true." ]; then
          ${NCP} -rp ./analysis_inc_nest02.nc ${RESTARTmrg}/
          # Replace d02 restart files
          for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
              in_file=${RESTARTbkg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
              out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
              mrg_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.merge.nc
              ${NCP} -rp ${out_file} ${mrg_file}
              ${NCP} -rp ${in_file} ${out_file}
          done
      fi
  fi #if [ ${iau_regional:-.false.} = ".true." ] || [ ${wave_num} -gt "-99" ]; then

fi

if [ ${MERGE_TYPE} = analysis ] && [ $SENDCOM = YES ]; then
    mkdir -p ${RESTARTcom}
    ${NCP} -rp ${RESTARTmrg}/* ${RESTARTcom}/
fi

# WDR 2025 04 08 these appear to be duplicates from above, detached from if blocks
#else
#    echo "FATAL ERROR: only support nest_grids = 1 or 2"
#    echo "FATAL ERROR: nest_grids = $nest_grids"
#    exit 1
#fi
#
#else
#
#    echo "RESTARTsrc: ${RESTARTsrc} does not exist"
#    echo "RESTARTmrg is the same as RESTARTdst"
#
#fi

date
