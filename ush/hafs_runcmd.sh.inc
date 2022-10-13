#==============================================================================
# This script sets job node/core, mpi/omp envionment, and run commands.
#==============================================================================

# Set job node/core, mpi/omp environment
export TOTAL_TASKS=${TOTAL_TASKS:-1}
export NCTSK=${NCTSK:-1}
export OMP_NUM_THREADS=${OMP_THREADS:-${OMP_NUM_THREADS:-24}}
export PURE_OMP_THREADS=${PURE_OMP_THREADS:-${OMP_NUM_THREADS:-24}}
export NCNODE=${NCNODE:-$(($NCTSK * $OMP_NUM_THREADS))}
export KMP_AFFINITY=disabled
export KMP_STACKSIZE=${KMP_STACKSIZE:-2048m}
#export OMP_STACKSIZE=${OMP_STACKSIZE:-2048m}

# Set job run commands
# APRUNS: command to run serial jobs
# APRUNO: command to run pure openmp jobs
# APRUNC: command to run pure mpi or mpi+omp jobs
# APRUNF: command to run multi jobs from a command file
# BACKGROUND: "" or "&" to implement the APRUNF capability on different platforms
if [ "$machine" = wcoss_cray ]; then
  #export NODES=${NODES:-1}
  export APRUNS="aprun -b -j1 -n1 -N1 -d1 -cc depth"
  #export APRUNO="aprun -b -j1 -n1 -N1 -d${PURE_OMP_THREADS} -cc depth"
  export APRUNO="time"
  export APRUNC="aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"
  export APRUNF="aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth cfp"
  export BACKGROUND=""
elif [ "$machine" = wcoss2 ]; then                                                                 
  #export NODES=${NODES:-1}                                                                          
  export APRUNS="time"                                                 
  export APRUNO="time"                                                                               
  export APRUNC="mpiexec -n ${TOTAL_TASKS} --cpu-bind core --depth ${OMP_NUM_THREADS}"             
  export APRUNF="mpiexec -n ${TOTAL_TASKS} cfp "
  export APRUNCFP="mpiexec"
  export APRUNO="time"
elif [ "$machine" = wcoss_dell_p3 ]; then
  export APRUNS="time"
  export APRUNO="time"
  export APRUNC="mpirun -n ${TOTAL_TASKS}"
  export APRUNF="mpirun -n ${TOTAL_TASKS} cfp"
  export BACKGROUND=""
elif [ "$machine" = jet ] || [ "$machine" = hera ] || [ "$machine" = orion ]; then
  export APRUNS="srun --ntasks=1 --nodes=1 --ntasks-per-node=1 --cpus-per-task=1"
  export APRUNO="srun --exclusive --ntasks=1 --nodes=1 --ntasks-per-node=${NCTSK} --cpus-per-task=${PURE_OMP_THREADS}"
  #export APRUNO="srun --ntasks=1 --nodes=1 --ntasks-per-node=${NCTSK} --cpus-per-task=${PURE_OMP_THREADS}"
  export APRUNC="srun --ntasks=${TOTAL_TASKS} --ntasks-per-node=${NCTSK} --cpus-per-task=${OMP_NUM_THREADS}"
  #export APRUNF="srun --ntasks=${TOTAL_TASKS} --ntasks-per-node=${NCTSK} --cpus-per-task=${OMP_NUM_THREADS} --multi-prog"
  export APRUNF="time"
  export BACKGROUND="&"
else
  echo "WARNING: unknown platform. Guessing the job run commands..."
  export APRUNS="time"
  export APRUNO="time"
  export APRUNC="mpirun"
  export APRUNF=${MPISERIAL:-mpiserial}
  export BACKGROUND=""
fi

# Print out environment 
env

#==============================================================================
