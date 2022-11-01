#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# The SLURM workload manager

function wm_checktime {
    # ask SLURM what the end time is for this job
    echo $(squeue -h -j $SLURM_JOB_ID -o %e)
}

function wm_submitjob {
    # resubmit this script via SLURM
    opt=""
    JOB_NPES=${JOB_NPES:-" "}
    JOB_OPTS=${JOB_OPTS:-" "}
    JOB_NODES=${JOB_NODES:-" "}
    JOB_QOS=${JOB_QOS:-" "}
    JOB_PARTITION=${JOB_PARTITION:-" "}
    [[ "$JOB_OPTS" != " " ]] && opt="$opt $JOB_OPTS"
    [[ "$JOB_QOS" != " " ]] && opt="$opt --qos=$JOB_QOS"
    [[ "$JOB_PARTITION" != " " ]] && opt="$opt --partition=$JOB_PARTITION"
    [[ "$JOB_NPES" != " " ]] && opt="$opt --ntasks=$JOB_NPES"
    [[ "$JOB_NODES" != " " ]] && opt="$opt --nodes=$JOB_NODES"
    sbatch $opt --time=$JOB_TIME -A $JOB_ACCT -J $JOB_NAME \
        -o $LOG_DIR/slurm.log $EXP_DIR/cycle.sh
}

function wm_init {
    # make sure the user has provided the required SLURM related environment variables
    echo ""
    echo "Using the SLURM workload manager"

    JOB_NPES=${JOB_NPES:-" "}
    JOB_NODES=${JOB_NODES:-" "}
    JOB_OPTS=${JOB_OPTS:-" "}
    JOB_QOS=${JOB_QOS:-" "}
    JOB_PARTITION=${JOB_PARTITION:-" "}

    envars=()
    envars+=("JOB_NAME")
    envars+=("JOB_NODES")
    envars+=("JOB_NPES")
    envars+=("JOB_QOS")
    envars+=("JOB_PARTITION")
    envars+=("JOB_TIME")
    envars+=("JOB_ACCT")

    set +u
    for v in ${envars[@]}; do
        [[ -z "${!v}" ]] && (echo "ERROR: env var $v is not set."; exit 1)
        printf "%-30s %s\n" "$v " "${!v}"
    done
    set -u
    echo ""

    if [[ "$JOB_NODES" == " " && "$JOB_NPES" == " " ]]; then
	    echo "ERROR: either \$JOB_NODES or \$JOB_NPES must be set"
    fi

    SLURM_JOB_ID=${SLURM_JOB_ID:-}
    if [[ "$SLURM_JOB_ID" == "" ]]; then
        echo "Relaunching this job under SLURM."
        wm_submitjob
        exit 0
    fi
}
