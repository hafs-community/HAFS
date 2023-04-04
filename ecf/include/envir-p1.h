# envir-p1.h
export job=${job:-$PBS_JOBNAME}
export jobid=${jobid:-$job.$PBS_JOBID}

export RUN_ENVIR=nco
export envir=%ENVIR%
export MACHINE_SITE=%MACHINE_SITE%
export RUN=%RUN%

if [ -n "%SENDCANNEDDBN:%" ]; then export SENDCANNEDDBN=${SENDCANNEDDBN:-%SENDCANNEDDBN:%}; fi
export SENDCANNEDDBN=${SENDCANNEDDBN:-"NO"}

if [[ "$envir" == prod && "$SENDDBN" == YES ]]; then
    export eval=%EVAL:NO%
    if [ $eval == YES ]; then export SIPHONROOT=${UTILROOT}/para_dbn
    else export SIPHONROOT=/lfs/h1/ops/prod/dbnet_siphon
    fi
    if [ "$PARATEST" == YES ]; then export SIPHONROOT=${UTILROOT}/fakedbn; export NODBNFCHK=YES; fi
else
    export SIPHONROOT=${UTILROOT}/fakedbn
fi
export SIPHONROOT=${UTILROOT}/fakedbn
export DBNROOT=$SIPHONROOT

if [[ ! " prod para test " =~ " ${envir} " && " ops.prod ops.para " =~ " $(whoami) " ]]; then err_exit "ENVIR must be prod, para, or test [envir-p1.h]"; fi

# Developer configuration
PTMP=/lfs/h2/emc/ptmp
PSLOT=${hafs_ver:-hafs.v1.0.0}
SID=${SID:-13L}
export COMROOT=${PTMP}/${USER}/${PSLOT}/para/com
export COMPATH=${PTMP}/${USER}/${PSLOT}/para/com/hafs
export COMOUT_PREP="$(compath.py obsproc/v1.1.0)"
if [ -n "%PDY:%" ]; then
  export PDY=${PDY:-%PDY:%}
  export CDATE=${PDY}%CYC:%
fi
export DATAROOT=/lfs/h2/emc/stmp/${USER}/hafs
mkdir -p ${DATAROOT} # ${COMhafs}
export COMINgfs=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/com/gfs/v16.3
export COMINgdas=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/com/gfs/v16.3
export COMINrtofs=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/com/rtofs/v2.3
export COMINobs=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/com/obsproc/v1.1
export DCOMROOT=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/dcom
export COMINobsproc=/lfs/h1/ops/prod/com/obsproc/v1.1/%RUN:%.${PDY}/%CYC:%/atmos

