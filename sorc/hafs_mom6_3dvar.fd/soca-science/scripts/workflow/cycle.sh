#!/bin/bash
#
# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# cycling script for SOCA experiments.
#
# A very simple workflow that calls all steps of the DA cycle sequentially.
# Run this script directly from the experiment directory to start the job submission.
#
# This script will resubmit itself to workload managers and exit if there is not enough
# time remaining for the current job (based on an averge cycle runtime kept track
# of by this script). If a job fails in the middle of a cycle, each of the
# substeps *should* be smart enough to skip over parts that had already
# been completed.
#================================================================================
set -eu

#====================================================================================
# environment variables that the user could specify before running this script
#====================================================================================

# optional path to config file, default is automatically found based on current directory
EXP_CONFIG_FILE=${EXP_CONFIG_FILE:-}

# The name of the machine, corresponding to a file in configs/machine/machine.*
# User can specify either variable below, MACHINE was left here for backward compatibility,
# but I figured SOCA_SCIENCE_MACHINE can be set in the .bashrc and not run the risk of interfering
# with other things.
SOCA_SCIENCE_MACHINE=${SOCA_SCIENCE_MACHINE:-}
MACHINE=${MACHINE:-$SOCA_SCIENCE_MACHINE}

# if machine above is "custom", a this custom machine configuration file is used
MACHINE_CONFIG_FILE=${MACHINE_CONFIG_FILE:-}

# if =T verbose output is sent to the screen instead of to a log file.
VERBOSE=${VERBOSE:-F}

#====================================================================================



# print header
# (yeah, it looks messed up here, but trust me that echo prints it out correctly)
echo '#================================================================================'
echo '#================================================================================'
echo '#              _______.  ______     ______      ___      '
echo '#             /       | /  __  \   /      |    /   \     '
echo '#            |   (----`|  |  |  | |  ,----'"'"'   /  ^  \    '
echo '#             \   \    |  |  |  | |  |       /  /_\  \   '
echo '#         .----)   |   |  `--'"'"'  | |  `----. /  _____  \  '
echo '#         |_______/     \______/   \______|/__/     \__\ '
echo '#'
echo '#            Sea-Ice, Ocean, and Coupled Assimilation'
echo "#                       JCSDA, $(date +"%Y")"
echo '#================================================================================'
echo '#================================================================================'
echo ''

# "It's a trap!!" -Admiral Ackbar
function abort {
  if [ "$?" -gt 0 ]; then
    echo ""
    echo "ERROR: abnormal termination"
  fi
}
trap abort EXIT


# determine the experiment directory,
# and make sure cycle.sh and exp.config are in the current working directory
[[ ! -f "cycle.sh" || ! -f "exp.config" ]] && (
    printf "ERROR: exp.config and a link to cycle.sh must be within the experiment "
    printf "direcoty. You MUST change to that directory before running cycle.sh"
    exit 1
)
export EXP_DIR=$(readlink -f $(pwd) )


# find the original directory this script is contained in, even if it
# means following symlinks (.i.e find $SOCA_SCIENCE_DIR)
src=$(pwd)/cycle.sh
[[ ! -f "$src" ]] && src="${BASH_SOURCE[0]}"
while [ -h "$src" ]; do # resolve $src until the file is no longer a symlink
    dir="$( cd -P "$( dirname "$src" )" >/dev/null 2>&1 && pwd )"
    src="$(readlink "$src")"
    # if $src was a relative symlink, we need to resolve it relative to the path
    # where the symlink file was located
    [[ $src != /* ]] && src="$dir/$src"
done
workflow_src_dir="$( cd -P "$( dirname "$src" )" >/dev/null 2>&1 && pwd )"
SOCA_SCIENCE_DIR=$(readlink -f $workflow_src_dir/../../)
printf "%-30s %s\n" "SOCA_SCIENCE_DIR" "$SOCA_SCIENCE_DIR"


# set some other directories based on the $SOCA_SCIENCE_DIR
export MODEL_DEFAULT_CFGS_DIR=$SOCA_SCIENCE_DIR/configs/model
export MODEL_DEFAULT_SCRIPT_DIR=$SOCA_SCIENCE_DIR/scripts/workflow/model
export SOCA_DEFAULT_CFGS_DIR=$SOCA_SCIENCE_DIR/configs/soca
export SUBSCRIPTS_DIR=$SOCA_SCIENCE_DIR/scripts/workflow/subscripts
export FORC_SCRIPTS_DIR=$SOCA_SCIENCE_DIR/scripts/forc
export OBS_SCRIPT_DIR=$SOCA_SCIENCE_DIR/scripts/obs
export R2D2_CFG_DIR=$SOCA_SCIENCE_DIR/configs/r2d2

# read the machine-specific configuration file. Given by $MACHINE_CONFIG_FILE
# if $MACHINE=custom, otherwise use a predefined file for the machine given by
# $MACHINE. Because these modules could also be sourced at build time, the
# $SOCA_SCIENCE_RUNTIME flag is used to let the machine config file know we are
# running a cycle and therefore possibly load different modules
export SOCA_SCIENCE_RUNTIME=T
if [[ "$MACHINE" == "" ]]; then
    echo "ERROR: environment variable MACHINE must be set"
    exit 1
elif [[ ! "$MACHINE" == custom ]]; then
    MACHINE_CONFIG_FILE=$SOCA_SCIENCE_DIR/configs/machine/machine.$MACHINE
elif [[ -z "$MACHINE_CONFIG_FILE" ]]; then
    echo "ERROR: if MACHINE=custom, MACHINE_CONFIG_FILE must be specified"
    exit 1
fi
printf "%-30s %s\n" "MACHINE" "$MACHINE"
printf "%-30s %s\n" "MACHINE_CONFIG_FILE" "$MACHINE_CONFIG_FILE"
set +u
export MACHINE_CONFIG_FILE
source $MACHINE_CONFIG_FILE
set -u

# assign default values to some of the environement variables
export DA_ATMHOFX_ENABLED=F # No atm h(x)
export OBS_LIST_ATM=""      # Atm list of obs is empty by default
export DA_VARIABLES_OCN='tocn, socn, ssh, hocn, sw, lhf, shf, lw, us'
export DA_VARIABLES_ICE='cicen, hicen, hsnon'
export DA_REGIONAL_ENABLED=F
export DA_CHKPT_WITH_MODEL=T
export DA_SAVE_INC=T
export DA_OBGC_ENABLED=F
export DA_DIAGB_ENABLED=T
export DA_DIAGB_DIRAC_STEP=10
export DA_PERTURBATION_MODEL='letkf'
export DA_ENSBDIAG_ENABLED=F
export DA_DUALRES_ENABLE=F
export DA_DUALRES_GENSUBGEOM=F
export DA_DUALRES_GENSUBGEOMLOC=""
export DA_DUALRES_SKIP=2
export R2D2_DB_DIR=" "
export R2D2_ENABLED=F
export R2D2_PROVIDER="jcsda_soca"
export R2D2_EXP="soca_ctest"
export R2D2_INSTALL="None"

# stuff for running custom user-defined steps within the workflow
export CUSTOM_STEPS_PREPREP=()
export CUSTOM_STEPS_PREP=()
export CUSTOM_STEPS_RUN=()
export CUSTOM_STEPS_POST=()
function run_custom {
    i=0
    steps=CUSTOM_STEPS_${1^^}[@]
    [[ ! -v $steps ]] && return
    steps=("${!steps}")
    for step in "${steps[@]}"; do
        i=$((i+1))
        step_name=${1}.custom_$i
        printf "%-30s" "Running $step_name ..."
        log_file=$LOG_DIR_CYCLE/$step_name
        (   export WORK_DIR=$SCRATCH_DIR_CYCLE/$step_name
            rm -rf $WORK_DIR
            mkdir -p $WORK_DIR
            cd $WORK_DIR

            if [[ "$VERBOSE" =~ [yYtT1] ]]; then
                $step
            else
                time $step &> $log_file
            fi
        ) || { printf "ERROR in $step_name, exit code $?\n check $log_file"; exit 1; }
    done
}

# read the experiment-specific configuration file given by $EXP_CONFIG_FILE.
# if thta variable is not specified assume the configuration file is given on the
# or otherwise is the current working directory
if [[ -z "$EXP_CONFIG_FILE" ]]; then
    if [[ $# -gt 0 ]]; then
        EXP_CONFIG_FILE=$(readlink -f "$1")
    else
        EXP_CONFIG_FILE=$(readlink -f .)/exp.config
    fi
fi
printf "%-30s %s\n" "EXP_DIR" "$EXP_DIR"
printf "%-30s %s\n" "EXP_CONFIG_FILE" "$EXP_CONFIG_FILE"
set -a
source $EXP_CONFIG_FILE
set +a

# Sometimes the soca-science bin dir is different from soca bin dir
# (this likely should only be happening with TravisCI!)
export SOCA_SCIENCE_BIN_DIR=${SOCA_SCIENCE_BIN_DIR:-$SOCA_BIN_DIR}

# concatenate ocean and atmosphere observations
export OBS_LIST="$OBS_LIST_OCN $OBS_LIST_ATM"

# make sure the logging directory is setup
LOG_DIR=$EXP_DIR/logs
mkdir -p $LOG_DIR
printf "%-30s %s\n" "LOG_DIR" "$LOG_DIR"
printf "%-30s %s\n" "VERBOSE" "$VERBOSE"

# load the workload manager settings
. $SOCA_SCIENCE_DIR/scripts/workflow/workload_manager/wm.${WORKLOAD_MANAGER}.sh
wm_init

# convenience wrapper for each subscript used within the loops later
function run_step {
    [[ "$#" != 1 ]] && ( echo "ERROR: run_step must be called with one arg" && exit 1)
    printf "%-30s" "Running $1 ..."
    script=$SUBSCRIPTS_DIR/$1.sh
    log_file=$LOG_DIR_CYCLE/$1
    if [[ ! -f "$script" ]]; then
        printf "\nERROR unable to find file $script"
        exit 1
    fi
    (   export WORK_DIR=$SCRATCH_DIR_CYCLE/$1
        rm -rf $WORK_DIR
        mkdir -p $WORK_DIR
        cd $WORK_DIR

        if [[ "$VERBOSE" =~ [yYtT1] ]]; then
            $script
        else
            time $script &> $log_file
        fi
    ) || { printf "ERROR in $1, exit code $?\n check $log_file"; exit 1; }
}


# what parts of the da cycle do we need to run?
export DA_ENABLED=F
export DA_HOFX_ENABLED=F
export DA_LETKF_ENABLED=F
export DA_LETKF_RECENTER=F
export DA_RECENTER_CHKPT=F
export DA_VAR_ENABLED=F
export DA_FCST_ENABLED=T
export DA_ENSFCST_ENABLED=F
case "$DA_MODE" in
    prep) ;;

    noda)
        [[ "$OBS_ENABLED" =~ [yYtT1] ]] && DA_HOFX_ENABLED=T
        [[ "$DA_NODA_FCST_ENABLED" =~ [nNfF0] ]] && DA_FCST_ENABLED=F ;;

    3dvar)
        DA_ENABLED=T
        DA_VAR_ENABLED=T     ;;

    letkf)
        DA_ENABLED=T
        DA_ENSFCST_ENABLED=T
        DA_RECENTER_CHKPT=T
        DA_LETKF_ENABLED=T   ;;

    3dhyb)
        DA_ENABLED=T
        DA_ENSFCST_ENABLED=T
        if [[ $DA_PERTURBATION_MODEL == 'letkf' ]]; then
            DA_LETKF_ENABLED=T
            DA_LETKF_RECENTER=T
            DA_RECENTER_CHKPT=T
        else
            DA_RECENTER_CHKPT=T
        fi
        DA_VAR_ENABLED=T     ;;

    *)  printf "ERROR: unkown \$DA_MODE \"$DA_MODE\""
        exit 1   ;;
esac

# check consistency of some of the settings
[[ "$DA_ENABLED" == "T" && ! "$OBS_ENABLED" =~ [yYtT1] ]] && (
    echo "ERROR: Trying do data assimilation without observations enabled."
    echo "ERROR: Check \$OBS_ENABLED in exp.config."
    exit 1
)
[[ ! "$DA_MODE" == "prep" && "$DA_FCST_ENABLED" == F &&  "$FORC_ENABLED" =~ [yYtT1] ]] && (
    echo "ERROR: Running in HofX mode with no forecast enabled..."
    echo "       But you also have forcing enabled, this is probably not what"
    echo "       you want. Disable \$FORC_ENABLED first."
    exit 1
)


#================================================================================
#================================================================================
# Start of loop
#================================================================================
#================================================================================
export TIMEFORMAT='%1Rs'
cycle_avg_count=0
cycle_avg_runtime=0
while true; do
    cycle_start=$(date +%s)

    # determine where the experiment left off and if this is the first cycle
    CYCLE_STATUS_FILE=$EXP_DIR/cycle_status
    if [[ ! -e $CYCLE_STATUS_FILE ]]; then
        CYCLE_START_DATE=$(date -ud "$EXP_START_DATE")
        FCST_RESTART=0
    else
        CYCLE_START_DATE=$(cat $CYCLE_STATUS_FILE)
        FCST_RESTART=1
    fi

    # FCST_LEN= 66 & 60 hours for free forecast
    if [[ "$FREE_FCST" =~ [yYtT1] ]]; then

       if [[ ! -e $CYCLE_STATUS_FILE ]]; then
       export FCST_LEN=${FREE_FCST_LEN[0]}
       echo YONGZUO $FREE_FCST $FCST_LEN

       fi

       if [[ -e $CYCLE_STATUS_FILE ]]; then
       export FCST_LEN=${FREE_FCST_LEN[1]}
       echo Yuhua $FREE_FCST $FCST_LEN
       fi

    fi

    # are we done with the experiment?
    if [[ $(date -ud "$CYCLE_START_DATE" +%Y%m%d%H) -gt \
          $(date -ud "$EXP_END_DATE" +%Y%m%d%H) ]]; then
        echo "Done with the experiment."
        exit 0
    fi

    # resubmit this job, if out of time, depending on the workload manager
    # (don't check this until cycle has run at least once, obviously)
    if [[ "$cycle_avg_count" -gt 0 ]]; then
        end_time=$(date -d "$(wm_checktime)" +%s)
        rem_time=$((end_time-cycle_start))
        min_time=$(bc <<< "$cycle_avg_runtime * 1.2 / 1") # fudge factor of 20%
        echo "${rem_time} seconds remaining for current job"
        if [[ "$rem_time" -lt "$min_time" ]]; then
            echo "almost out of time, resubmitting a new job"
            wm_submitjob
            exit 0
        fi
    fi

    echo ""
    echo ""
    echo "********************************************************************************"
    echo "********************************************************************************"
    echo "**   Starting cycle for $CYCLE_START_DATE"
    echo "********************************************************************************"
    echo "********************************************************************************"

    # determine derived variables involving the date
    export ANA_DATE=$(date -ud "$CYCLE_START_DATE") # remove?
    ymd=$(date -ud "$ANA_DATE" +%Y%m%d)
    ymdh=$(date -ud "$ANA_DATE" +%Y%m%d%H)
    export DATE=$ANA_DATE
    export NEXT_DATE=$(date -ud "$ANA_DATE + $FCST_LEN hours")
    next_ymdh=$(date -ud "$NEXT_DATE" +%Y%m%d%H)

    # directories that depend on the cycle
    SCRATCH_DIR_CYCLE=$SCRATCH_DIR/$ymdh
    LOG_DIR_CYCLE=$LOG_DIR/$ymdh
    mkdir -p $SCRATCH_DIR_CYCLE
    mkdir -p $LOG_DIR_CYCLE

    # final output directories
    export ANA_DIR=$EXP_DIR/ana/${ymdh:0:4}/$ymdh
    export BKG_DIR=$EXP_DIR/bkg/${ymdh:0:4}/$ymdh
    export INCR_DIR=$EXP_DIR/incr/${ymdh:0:4}/$ymdh
    export DIAGB_DIR=$EXP_DIR/bmat/${ymdh:0:4}/$ymdh
    export BKGRST_DIR=$EXP_DIR/rst/$ymdh/ctrl
    export BKGRST_DIR_NEXT=$EXP_DIR/rst/$next_ymdh/ctrl
    export BKGRST_ENS_DIR=$EXP_DIR/rst/$ymdh/ens
    export BKGRST_ENS_DIR_NEXT=$EXP_DIR/rst/$next_ymdh/ens

    export OBS_OUT_DIR=$EXP_DIR/obs_out/${ymdh:0:4}/$ymdh
    export OBS_ATM_OUT_DIR=$EXP_DIR/obs_atm_out/${ymdh:0:4}/$ymdh
    export SOCA_STATIC_DIR=$EXP_DIR/static
    export DIAG_DIR=$EXP_DIR/diag/${ymdh:0:4}/$ymdh

    # temporary output directories for individual subscripts
    export ANA_ENS_DIR=$SCRATCH_DIR_CYCLE/ana_ens
    export ANARST_DIR=$SCRATCH_DIR_CYCLE/ana_rst/ctrl
    export DIAGB_TMP_DIR=$SCRATCH_DIR_CYCLE/bmat
    export INCR_TMP_DIR=$SCRATCH_DIR_CYCLE/incr/ctrl
    export DIAG_TMP_DIR=$SCRATCH_DIR_CYCLE/diag
    export ANARST_ENS_DIR=$SCRATCH_DIR_CYCLE/ana_rst/ens
    export FORC_DIR=$SCRATCH_DIR_CYCLE/forc
    export OBS_DIR=$SCRATCH_DIR_CYCLE/obs
    export OBS_OUT_CTRL_DIR=$SCRATCH_DIR_CYCLE/obs_out/ctrl
    export OBS_ATM_OUT_CTRL_DIR=$SCRATCH_DIR_CYCLE/obs_atm_out/ctrl
    export OBS_OUT_ENS_DIR=$SCRATCH_DIR_CYCLE/obs_out/ens

    # convenience function for ending the current cycle, because it's called in
    # more than one location
    function end_cycle {
        # done with this day of the cycle, cleanup and prepare for the next cycle
        echo "$NEXT_DATE" > $CYCLE_STATUS_FILE

        # update statistics on average cycle runtime
        cycle_end=$(date +%s)
        cycle_runtime=$((cycle_end-cycle_start ))
        cycle_avg_runtime=$(((cycle_avg_runtime*cycle_avg_count+cycle_runtime)/(cycle_avg_count+1) ))
        cycle_avg_count=$((cycle_avg_count+1))
        echo "Cycle runtime:     $cycle_runtime seconds"
        echo "Cycle runtime avg: $cycle_avg_runtime seconds"
    }


    #===========================================================================
    # prep
    #===========================================================================
    # custom pre-prep steps
    run_custom preprep

    # forcing
    [[ "$FORC_ENABLED" =~ [yYtT1] ]] && run_step prep.forc

    # observations
    [[ "$OBS_ENABLED" =~ [yYtT1] ]] && run_step prep.obs

    # if running in "prep" mode (i.e. just downloading the input data)
    #  cleanup and quit here
    if [[ "$DA_MODE" == "prep" ]]; then
        rm -r $SCRATCH_DIR_CYCLE
        end_cycle
        continue
    fi

    # background restart (single deterministic)
    run_step prep.bkgrst

    # static b / grid init
    run_step prep.soca

    # background (ensemble)
    [[ "$DA_ENSFCST_ENABLED" =~ [yYtT1] ]] && run_step prep.bkgrst.ens

    # Diagonal of ensemble B-matrix
    [[ "$DA_ENSBDIAG_ENABLED" =~ [yYtT1] ]] && run_step prep.bdiag.ens

    # custom prep steps
    run_custom prep


    #===========================================================================
    # run
    #===========================================================================

    # Atmospheric hofx
    # TODO only works with GEOS, make it work with UFS
    [[ "$DA_ATMHOFX_ENABLED" =~ [yYtT1] ]] && run_step run.atmhofx

    # Data Assimilation methods
    if [[ "$DA_ENABLED" =~ [yYtT1] ]]; then

        # 3DVAR/EnVAR
        [[ "$DA_VAR_ENABLED" =~ [yYtT1] ]] && run_step run.var

        # LETKF
        [[ "$DA_LETKF_ENABLED" =~ [yYtT1] ]] && run_step run.letkf

        # RECENTER ONLY
        [[ "$DA_RECENTER_CHKPT" =~ [yYtT1] ]] && run_step run.recenter.checkpoint

        # TODO perturb output if not doing LETKF but still doing EnVAR
    else
        # Not doing any DA, just link ANARST to BKGRST
        mkdir -p $(dirname $ANARST_DIR)
        ln -sf $BKGRST_DIR $ANARST_DIR
    fi

    # hofx
    # TODO this section is a bit of a hack, should be cleaned up
    [[ "$DA_HOFX_ENABLED" =~ [yYtT1] ]] && (
        # annoyingly, the background source is different depending on
        #  whether the forecast has been generated as part of this cycle
        if [[ "$DA_FCST_ENABLED" =~ [yYtT1] ]]; then
            # assume we are pulling from full restart files
            ocn_file=MOM.res.nc
            ice_file=cice.res.nc
        else
            # assume we are pulling from reduced backgorund files
            ocn_file=ocn.bkg.$ymdh.nc
            ice_file=ice.bkg.$ymdh.nc
        fi
        export BKG_FILE=$BKGRST_DIR/$ocn_file
        export SEAICE_BKG_FILE=$BKGRST_DIR/$ice_file

        run_step run.hofx
    )

    # forecasts
    [[ "$DA_FCST_ENABLED" =~ [yYtT1] ]] && (
        export FCST_START_TIME=$ANA_DATE

        # control member
        export MODEL_RST_DIR_IN=$ANARST_DIR
        export MODEL_RST_DIR_OUT=$BKGRST_DIR_NEXT
        run_step run.fcst

        # ensemble members
        if [[ "$DA_ENSFCST_ENABLED" =~ [yYtT1] ]]; then
            for ens in $(seq -f "%03g" $DA_ENS_SIZE); do
                export MODEL_RST_DIR_IN=$ANARST_ENS_DIR/$ens
                export MODEL_RST_DIR_OUT=$BKGRST_ENS_DIR_NEXT/$ens

                # forcing
                if [[ "$FORC_ENABLED_ENS" =~ [yYtT1] ]]; then
                    export FORC_SRC=$FORC_SRC_ENS/$ens
                fi

                # stochastic physics
                if [[ "$MODEL_CFG_ENABLED_ENS" =~ [yYtT1] ]]; then
                    export MODEL_CFG_DIR=$MODEL_CFG_DIR_ENS/$ens
                fi

                run_step run.fcst
            done
        fi
    )

    # custom run steps
    run_custom run


    #===========================================================================
    # post
    #===========================================================================
    # obs stats
    [[ "$OBS_ENABLED" =~ [yYtT1] ]] && run_step post.obs

    # state compression
    [[ "$DA_FCST_ENABLED" =~ [yYtT1] ]] && run_step post.state

    # custom post steps
    run_custom post

    # run cleanup
    run_step post.cleanup
    #rm -r $SCRATCH_DIR_CYCLE

    #===========================================================================
    #===========================================================================

    end_cycle
done
