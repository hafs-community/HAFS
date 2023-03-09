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

PTMP=/lfs/h2/emc/ptmp
PSLOT=${hafs_ver:-hafs.v1.0.0}
#SUBEXPT=${SUBEXPT:-hafs.v1.0.0_hfsa}
SID=${SID:-13L}

export COMROOT=${PTMP}/${USER}/${PSLOT}/para/com
#### export COMPATH=${PTMP}/${USER}/${PSLOT}/para/com/gfs:${PTMP}/${USER}/${PSLOT}/para/com/obsproc
#### Requested by NCO to point to NCO's obsproc COM location starting CDATE=2022101112
#export COMPATH=${PTMP}/${USER}/${PSLOT}/para/com/gfs:/lfs/h1/ops/para/com/obsproc
export COMPATH=${PTMP}/${USER}/${PSLOT}/para/com/hafs

#export COMhafs="$(compath.py hafs/${hafs_ver})"/${SUBEXPT}/${CDATE}/${SID}
export COMOUT_PREP="$(compath.py obsproc/v1.1.0)"
if [ -n "%PDY:%" ]; then
  export PDY=${PDY:-%PDY:%}
  export CDATE=${PDY}%CYC:%
fi

export DATAROOT=/lfs/h2/emc/stmp/${USER}/hafs
#echo $ROTDIR/%RUN:%.${PDY}/%CYC:%/atmos
mkdir -p ${DATAROOT} # ${COMhafs}

export COMINgfs=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/com/gfs/v16.3
export COMINgdas=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/com/gfs/v16.3
export COMINrtofs=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/com/rtofs/v2.3
export COMINobs=/lfs/h2/emc/hafstemp/CANNED_input_for_HAFS/com/obsproc/v1.1


#### export COMINobsproc=/lfs/h2/emc/global/noscrub/emc.global/dump/%RUN:%.${PDY}/%CYC:%
#### export COMINobsproc=/lfs/h2/emc/ptmp/${USER}/${PSLOT}/para/com/obsproc/v1.1/%RUN:%.${PDY}/%CYC:%/atmos
export COMINobsproc=/lfs/h1/ops/prod/com/obsproc/v1.1/%RUN:%.${PDY}/%CYC:%/atmos
#### tcvital assignment
#### export COMINtcvital=$COMINobsproc

#### Emergency production switch check
####   If production is not dogwood, the production switch is in place. The parallel should
####   stop.
prod_machine_Current=`grep primary /lfs/h1/ops/prod/config/prodmachinefile|awk 'BEGIN { FS = ":" } ; { print $2 }'`
echo "Current production machine is $prod_machine_Current"
if [[ "$prod_machine_Current" == cactus ]]; then
  err_exit "Production switch is in place. All parallel jobs set to fail."
fi
