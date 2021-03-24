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
    cmake ../hafs_analysis_update -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc

    # Build the analysis-update application.

    make all >& ${HAFS_UTILS_SORC}/logs/make.analysis-update.log

    # Move the analysis-update application executable to the HAFS
    # utility application executables path.
    make install >& ${HAFS_UTILS_SORC}/logs/install.analysis-update.log
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
    cmake ../hafs_obs_preproc -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc

    # Build the obs-preproc application.
    make all >& ${HAFS_UTILS_SORC}/logs/make.obs-preproc.log

    # Move the analysis-update application executable to the HAFS
    # utility application executables path.
    make install >& ${HAFS_UTILS_SORC}/logs/install.obs-preproc.log
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

    ./configure --prefix=${PREFIX} --disable-doc >& ${HAFS_UTILS_EXTLIBS}/logs/configure.fftw.log

    # Build the FFTW application.

    make >& ${HAFS_UTILS_EXTLIBS}/logs/make.fftw.log

    # Install the FFTW application.

    make install >& ${HAFS_UTILS_EXTLIBS}/logs/install.fftw.log
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

    ./configure --prefix=${PREFIX} >& ${HAFS_UTILS_EXTLIBS}/logs/configure.shtns.log

    # Build the SHTNS application.

    make >& ${HAFS_UTILS_EXTLIBS}/logs/make.shtns.log

    # Install the SHTNS application.

    make install >& ${HAFS_UTILS_EXTLIBS}/logs/install.shtns.log
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
    make >& ${HAFS_UTILS_EXTLIBS}/logs/make.ext-libs.log
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
