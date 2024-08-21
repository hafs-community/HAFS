#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

script_dir=${cwd}
cd ${script_dir}

export RT_COMPILER="intel"
source "${script_dir}/hafs_forecast.fd/tests/detect_machine.sh"
source "${script_dir}/hafs_forecast.fd/tests/module-setup.sh"

# Default settings
  
ww3switch="model/bin/switch_meshcap"

finalexecdir="$( pwd -P )/hafs_forecast.fd/WW3/model/exec"

# Check final exec folder exists
if [[ ! -d "${finalexecdir}" ]]; then
  mkdir -p ${finalexecdir}
fi

#Determine machine and load modules
module use "${script_dir}/hafs_forecast.fd/modulefiles"
module load "ufs_${MACHINE_ID}.${RT_COMPILER}"

#Set WW3 directory, switch, prep and post exes 
cd hafs_forecast.fd/WW3 || exit 1
WW3_DIR=$( pwd -P )
export WW3_DIR
export SWITCHFILE="${WW3_DIR}/${ww3switch}"

# Build exes for prep jobs and post jobs:
prep_exes="ww3_grid ww3_prep ww3_prnc ww3_grid ww3_strt ww3_bound ww3_prnc"
post_exes="ww3_outp ww3_outf ww3_outp ww3_gint ww3_ounf ww3_ounp ww3_grib"

#create build directory: 
path_build="${WW3_DIR}/build_SHRD"
mkdir -p "${path_build}" || exit 1
cd "${path_build}" || exit 1
echo "Forcing a SHRD build" 

cat "${SWITCHFILE}" > "${path_build}/tempswitch"

sed -e "s/DIST/SHRD/g"\
    -e "s/OMPG / /g"\
    -e "s/OMPH / /g"\
    -e "s/MPIT / /g"\
    -e "s/MPI / /g"\
    -e "s/B4B / /g"\
    -e "s/PDLIB / /g"\
    -e "s/NOGRB/NCEP2/g"\
       "${path_build}/tempswitch" > "${path_build}/switch"
rm "${path_build}/tempswitch"

echo "Switch file is ${path_build}/switch with switches:" 
cat "${path_build}/switch"

#Build executables: 
BUILD_TYPE=${BUILD_MODE}
cmake "${WW3_DIR}" -DSWITCH="${path_build}/switch" -DCMAKE_INSTALL_PREFIX=install -DCMAKE_BUILD_TYPE=${BUILD_TYPE}
rc=$?
if (( rc != 0 )); then
  echo "Fatal error in cmake."
  exit "${rc}"
fi
make -j 8 
rc=$?
if (( rc != 0 )); then
  echo "Fatal error in make."
  exit "${rc}"
fi
make install 
if (( rc != 0 )); then
  echo "Fatal error in make install."
  exit "${rc}" 
fi

# Copy to top-level exe directory
for prog in ${prep_exes} ${post_exes}; do
  cp "${path_build}/install/bin/${prog}" "${finalexecdir}/"
  rc=$?
  if (( rc != 0 )); then
    echo "FATAL: Unable to copy ${path_build}/${prog} to ${finalexecdir} (Error code ${rc})"
    exit "${rc}"
  fi
done

#clean-up build directory:
echo "executables are in ${finalexecdir}" 
echo "cleaning up ${path_build}" 
rm -rf "${path_build}"

