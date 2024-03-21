################################################################################
# Script Name: hafs_runcmd.sh.inc
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script sets job node/core, mpi/omp environment, job run commands, as
#   well as some commonly used commands and environment variables. The main
#   purpose to help HAFS application to easily support multiple platforms.
################################################################################

# Set job node/core, mpi/omp environment
export TOTAL_TASKS=${TOTAL_TASKS:-1}
export NCTSK=${NCTSK:-1}
export OMP_NUM_THREADS=${OMP_THREADS:-${OMP_NUM_THREADS:-24}}
export PURE_OMP_THREADS=${PURE_OMP_THREADS:-${OMP_NUM_THREADS:-24}}
export NCNODE=${NCNODE:-$(($NCTSK * $OMP_NUM_THREADS))}
export KMP_AFFINITY=disabled
export KMP_STACKSIZE=${KMP_STACKSIZE:-2048m}
#export OMP_STACKSIZE=${OMP_STACKSIZE:-2048m}
# Avoild "NetCDF: HDF error"
export HDF5_USE_FILE_LOCKING=FALSE

# Set job run commands
# APRUN:  command to run general jobs
# APRUNS: command to run serial jobs
# APRUNO: command to run pure openmp jobs
# APRUNC: command to run pure mpi or mpi+omp jobs
# APRUNF: command to run multi jobs from a command file
# BACKGROUND: "" or "&" to implement the APRUNF capability on different platforms
if [ "$machine" = wcoss2 ]; then
  export APRUN="time"
  export APRUNS="time"
  export APRUNO="mpiexec -n 1 --cpu-bind core --depth ${OMP_NUM_THREADS}"
  export APRUNC="mpiexec -n ${TOTAL_TASKS} --cpu-bind core --depth ${OMP_NUM_THREADS}"
  export APRUNF="mpiexec -n ${TOTAL_TASKS} cfp "
  export APRUNCFP="mpiexec"
  export BACKGROUND=""
elif [ "$machine" = jet ] || [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = hercules ]; then
  export APRUN="time"
  export APRUNS="srun --mem=0 --ntasks=1 --nodes=1 --ntasks-per-node=1 --cpus-per-task=1"
  export APRUNO="srun --mem=0 --exclusive --ntasks=1 --nodes=1 --ntasks-per-node=${NCTSK} --cpus-per-task=${PURE_OMP_THREADS}"
  export APRUNC="srun --mem=0 --ntasks=${TOTAL_TASKS} --ntasks-per-node=${NCTSK} --cpus-per-task=${OMP_NUM_THREADS}"
 #export APRUNF="srun --mem=0 --ntasks=${TOTAL_TASKS} --ntasks-per-node=${NCTSK} --cpus-per-task=${OMP_NUM_THREADS} --multi-prog"
  export APRUNF="time"
  export APRUNCFP="time"
  export BACKGROUND="&"
else
  echo "WARNING: unknown platform. Guessing the job run commands..."
  export APRUN="time"
  export APRUNS="time"
  export APRUNO="time"
  export APRUNC="mpirun"
  export APRUNF="${MPISERIAL:-mpiserial}"
  export APRUNCFP="time"
  export BACKGROUND=""
fi

# Some commonly used commands
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export TAR=${TAR:-tar}
export NDATE=${NDATE:-ndate}
export WGRIB2=${WGRIB2:-wgrib2}
export GRB2INDEX=${GRB2INDEX:-grb2index}
export COMPRESS=${COMPRESS:-gzip}
export UNCOMPRESS=${UNCOMPRESS:-gunzip}
export MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}

#==============================================================================
