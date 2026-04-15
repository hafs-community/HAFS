set -xeu

machine=wcoss2

#Supports Debug or Release modes for the build
BUILD_TYPE=${BUILD_TYPE:-RELEASE}

#Explicitly pass to linker that executable stack is not needed
USE_NOEXECSTACK=${USE_NOEXECSTACK:-ON}
export USE_NOEXECSTACK

cwd=$(pwd)

module reset
module use ../sorc/hafs_jedi.fd/modulefiles
module load HDAS/${machine}.intel
module list

export HAFS_UTILS_SORC=${cwd}/hafs_tools.fd/sorc
cd ${HAFS_UTILS_SORC}

if [ -d "${HAFS_UTILS_SORC}/build" ]; then
   rm -rf ${HAFS_UTILS_SORC}/build
fi
mkdir ${HAFS_UTILS_SORC}/build
cd ${HAFS_UTILS_SORC}/build

cmake ../hafs_obs_preproc -DCMAKE_Fortran_COMPILER=${FC} -DCMAKE_C_COMPILER=${CC} -DUSE_NOEXECSTACK=${USE_NOEXECSTACK} -DBUILD_TYPE=${BUILD_TYPE}

make all VERBOSE=3
make install

cd ..

if [ -d "${HAFS_UTILS_SORC}/build" ]; then
   rm -rf ${HAFS_UTILS_SORC}/build
fi
mkdir ${HAFS_UTILS_SORC}/build
cd ${HAFS_UTILS_SORC}/build

cmake ../hafs_datool -DCMAKE_Fortran_COMPILER=${FC} -DCMAKE_C_COMPILER=${CC} -DBUILD_TYPE=${BUILD_TYPE}
make all VERBOSE=3
make install
