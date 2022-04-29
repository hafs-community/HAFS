#!/bin/bash --posix

################################################################################
#### UNIX Script Documentation Block
##
## Script name:         build_hafs_utils.sh
##
## Script description:  Compile HAFS libraries and utility applications.
##
## Author:              Henry R. Winterbottom 
##
## Date:                2020-02-03
##
## Abstract:            This script compiles the HAFS local libraries
##                      (ext_libs) source codes and all utility applications.
##
## Script history log:  
##
## 2020-02-03  Henry R. Winterbottom -- Original version.
##
## Usage: build_hafs_utils.sh
##
##   Imported Shell Variables:
##
##   Exported Shell Variables:
##
## Remarks:
##
##   Condition codes:
##
##      0 - no problem encountered
##     >0 - some problem encountered
##
## Attributes:
##
##   Language: POSIX shell
##   Machine: IBM SP
##
################################################################################
##
## CMake based build: Biju Thomas 2021-01-04
## Cleaning hafs_tools.fd:   Biju Thomas 2021-03-23
##      * Removing sources that are not needed or no longer used
##      * Merging internal libraries into a single folder with a single driver script
##      * Ported hafs_change_prepbufr under hafs_tools.fd from HWRF (2021-06-07)
## Added hafs_datool & hafs_vi to CMake based build: Biju Thomas 2022-01-25
#################################################################################

set -x -e

#----

# FUNCTION:

# _hafsutils_analysis_update.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility analysis-update
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_analysis_update (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application 
    if [[ $target = "wcoss_cray" ]]; then
        cmake ../hafs_analysis_update -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc
    else
        cmake ../hafs_analysis_update -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc
    fi

    # Build the analysis-update application.

    make all

    # Move the analysis-update application executable to the HAFS
    # utility application executables path.
    make install
}

#----

# FUNCTION:

# _hafsutils_obs_preproc.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility obs-preproc
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_obs_preproc (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application
    if [[ $target = "wcoss_cray" ]]; then
       cmake ../hafs_obs_preproc -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc
    else
       cmake ../hafs_obs_preproc -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc
    fi

    # Build the obs-preproc application.
    make all

    # Move the analysis-update application executable to the HAFS
    # utility application executables path.
    make install
}

#----

# FUNCTION:

# _hafsutils_change_prepbufr.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility change_prepbufr
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_change_prepbufr (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application
    if [[ $target = "wcoss_cray" ]]; then
       cmake ../hafs_change_prepbufr -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc
    else
       cmake ../hafs_change_prepbufr -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc
    fi

    # Build the hafs_change_prepbufr application.
    make all

    # Move the hafs_change_prepbufr application executable to the HAFS
    # utility application executables path.
    make install
}


#----

# FUNCTION:

# _hafsutils_datool.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility datool
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_datool (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application
    # BUILD_TYPE supports RELEASE OR DEBUG MODE
    if [[ $target = "wcoss_cray" ]]; then
       cmake ../hafs_datool -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc -DBUILD_TYPE=RELEASE
    else
       cmake ../hafs_datool -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -DBUILD_TYPE=RELEASE
    fi

    # Build the hafs_datool application.
    make all VERBOSE=3

    # Move the hafs_datool application executable to the HAFS
    # utility application executables path.
    make install
}

#----

# FUNCTION:

# _hafsutils_vi.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility vi
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_vi (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application
    # BUILD_TYPE supports RELEASE OR DEBUG MODE
    if [[ $target = "wcoss_cray" ]]; then
       cmake ../hafs_vi -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc -DBUILD_TYPE=RELEASE
    else
       cmake ../hafs_vi -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -DBUILD_TYPE=RELEASE
    fi

    # Build the hafs_vi application.
    make all VERBOSE=3

    # Move the hafs_vi application executable to the HAFS
    # utility application executables path.
    make install
}

#----


# FUNCTION:

# build_hafsutils.sh

# DESCRIPTION:

# This function builds all source code and/or libraries for the HAFS
# utility applications.

build_hafsutils (){

    # Create a directory to contain all configure, make, and
    # installation logs.

    mkdir -p ${HAFS_UTILS_SORC}/logs
    
    # Define the top-level directory for all HAFS utility libraries
    # collected from external sources.

    export EXT_LIBS=${HAFS_UTILS_EXTLIBS}
    
    # Build the analysis-update application.

    _hafsutils_analysis_update

    # Build the obs-preproc application.

    _hafsutils_obs_preproc

    # Build the change_prepbufr application.

    _hafsutils_change_prepbufr
  
     # Build the datool application.

    _hafsutils_datool
    
     # Build the vi application
    _hafsutils_vi

}

#----

# FUNCTION:

# _extlib_fftw.sh

# DESCRIPTION:

# This function configures, builds, and installs the FFTW
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_extlib_fftw (){

    # Move to the working directory for the FFTW application/library.

    cd ${HAFS_UTILS_EXTLIBS}/fftw

    # Define the local environment variables for the FFTW compilation.

    PREFIX=${HAFS_UTILS_EXTLIBS}
    export F77=`which ifort`
    export CC=`which gcc`

    # Configure the compile-time environment for the FFTW application
    # build.

#    make clean

    ./configure --prefix=${PREFIX} --disable-doc

    # Build the FFTW application.

    make

    # Install the FFTW application.

    make install
}

#----

# FUNCTION:

# _extlib_shtns.sh

# DESCRIPTION:

# This function configures, builds, and installs the SHTNS
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_extlib_shtns (){

    # Move to the working directory for the SHTNS application/library.

    cd ${HAFS_UTILS_EXTLIBS}/shtns

    # Define the local environment variables for the SHTNS
    # compilation.

    export LDFLAGS=-L${HAFS_UTILS_EXTLIBS}/lib
    PREFIX=${HAFS_UTILS_EXTLIBS}

    # Configure the compile-time environment for the SHTNS application
    # build.

    ./configure --prefix=${PREFIX}

    # Build the SHTNS application.

    make

    # Install the SHTNS application.

    make install
}

#----

# FUNCTION:

# build_extlibs.sh

# DESCRIPTION:

# This function builds all applications and/or libraries required for
# the HAFS utility applications.

build_extlibs (){

    # Create a directory to contain all configure, make, and
    # installation logs.

    mkdir -p ${HAFS_UTILS_EXTLIBS}/logs

    # clean previous build

    cd ${HAFS_UTILS_EXTLIBS}
    make clean

    # Build the FFTW application.

    _extlib_fftw

    # Build the SHTNS application.

    _extlib_shtns


    # Create a directory to contain all configure, make, and
    # installation logs.

    mkdir -p ${HAFS_UTILS_EXTLIBS}/logs

    # Move to the working directory for the HAFS utility libraries.

    cd ${HAFS_UTILS_EXTLIBS}

    # Build all utility libraries.

    _setup_compiler
    make
}

#----

# FUNCTION:

# setup_hafs_utils_build.sh

# DESCRIPTION:

# This function configures the compile-time environment for the HAFS
# utility application compilations and builds.

setup_hafs_utils_build (){

    # Define the top-level directory for all HAFS utility application
    # source codes; this includes the library paths.

    export HAFS_UTILS_SORC=`pwd`

    # Define a working directory to contain all HAFS utility
    # application executables.

    export HAFS_UTILS_EXEC=${HAFS_UTILS_SORC}/../exec

    # Define the top-level directory for all HAFS utility external
    # libraries.

    export HAFS_UTILS_EXTLIBS=${HAFS_UTILS_SORC}/hafs_extlibs

    if [ -d "${HAFS_UTILS_EXTLIBS}/lib" ]; then rm -Rf ${HAFS_UTILS_EXTLIBS}/lib; fi
    if [ -d "${HAFS_UTILS_EXTLIBS}/bin" ]; then rm -Rf ${HAFS_UTILS_EXTLIBS}/bin; fi
    if [ -d "${HAFS_UTILS_EXTLIBS}/include" ]; then rm -Rf ${HAFS_UTILS_EXTLIBS}/include; fi
    if [ -d "${HAFS_UTILS_EXTLIBS}/logs" ]; then rm -Rf ${HAFS_UTILS_EXTLIBS}/logs; fi
    
    # Create a working directory to contain all HAFS utility
    # application executables.

    mkdir -p ${HAFS_UTILS_EXEC}
}

#----

# FUNCTION:

# setup_compiler.sh

# DESCRIPTION:

# Define all compilers specific to the HAFS utility application
# builds.

_setup_compiler (){
    export AR=/usr/bin/ar
    export MKDIR=/bin/mkdir
    export MV=/bin/mv
    export RANLIB=/usr/bin/ranlib
    export RM=/bin/rm
    export CC=gcc
    export F77=ifort
    export FC=ifort
    ##export MPIFC=mpif90 (Moved to build_tools.sh)

# Define all compiler flags for the EXT-libs applications.

    export EXT_LIBS_CCFLAGS="-O3"
    export EXT_LIBS_DEBUG=""
    export EXT_LIBS_FCFLAGS="-O3 -mcmodel=large -convert big_endian"

# Define all compiler flags for the analysis-update application.

    export ANALYSIS_UPDATE_DEBUG=""
    export ANALYSIS_UPDATE_FCFLAGS="-O3 -heap-arrays -mkl=sequential -convert big_endian -assume byterecl -DLINUX"

# Define all compiler flags for the obs-preproc application.

    export OBS_PREPROC_DEBUG=""
    export OBS_PREPROC_FCFLAGS="-O3 -fp-model precise -assume byterecl -convert big_endian"
}

#----

script_name=`basename "$0"`
start_date=`date`
echo "START ${script_name}: ${start_date}"

# (1) Configure the local environment for the HAFS utility application
#     compilations.

setup_hafs_utils_build

# (2) Build all libraries specific to numerical weather prediction
#     (EXT) applications.

build_extlibs

# (3) Build all HAFS utility applications.

build_hafsutils

export ERR=$?
export err=${ERR}
stop_date=`date`
echo "STOP ${script_name}: ${stop_date}"

exit ${err}
