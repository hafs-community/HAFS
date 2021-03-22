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
##                      (nwp_libs) source codes and all utility applications.
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
##
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

# _hafsutils_da_utils.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility da-utils
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_da_utils (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application
    cmake ../hafs_da_utils -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc

    # Build the da-utils application.
    make VERBOSE=1 >& ${HAFS_UTILS_SORC}/logs/make.da-utils.log

    # Move the analysis-update application executable to the HAFS
    # utility application executables path.
    make install >& ${HAFS_UTILS_SORC}/logs/install.da-utils.log
}

#----

# FUNCTION:

# _hafsutils_file_check.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility file-check
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_file_check (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application
    cmake ../hafs_file_check -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc

    # Build the file-check application.
    make all >& ${HAFS_UTILS_SORC}/logs/make.file-check.log

    # Move the analysis-update application executable to the HAFS
    # utility application executables path.
    make install >& ${HAFS_UTILS_SORC}/logs/install.file-check.log
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

# _hafsutils_post_utils.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility post-utils
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_post_utils (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application
    cmake ../hafs_post_utils -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc

    # Build the post-utils application.
    make all >& ${HAFS_UTILS_SORC}/logs/make.post-utils.log

    # Move the post-utils application executable to the HAFS utility
    # application executables path.
    make install >& ${HAFS_UTILS_SORC}/logs/install.post-utils.log
}

#----

# FUNCTION:

# _hafsutils_tc_diagnostics.sh

# DESCRIPTION:

# This function compiles and install the HAFS utility tc-diagnostics
# application.

# NOTE:

# This function should never be called directly by the user and is for
# internal use only within this script.

_hafsutils_tc_diagnostics (){

    # Remove the build dir if it exists from previous build
    if [ -d "${HAFS_UTILS_SORC}/build" ]; then
       rm -rf ${HAFS_UTILS_SORC}/build
    fi

    # Create a build directory for a fresh build
    mkdir ${HAFS_UTILS_SORC}/build

    cd ${HAFS_UTILS_SORC}/build

    # Generate makefile using CMake for the application
    cmake ../hafs_tc_diagnostics -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc

    # Build the tc-diagnostics application.
    make all >& ${HAFS_UTILS_SORC}/logs/make.tc-diagnostics.log

    # Move the tc-diagnostics application executable to the HAFS
    # utility application executables path.
    make install >& ${HAFS_UTILS_SORC}/logs/install.tc-diagnostics.log
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
    
    # Define the top-level directory for all HAFS utility application
    # compile-time libraries.

    export NWP_LIBS=${HAFS_UTILS_NWPLIBS}

    # Build the analysis-update application.

    _hafsutils_analysis_update

##    # Build the da-utils application.

##    _hafsutils_da_utils

##    # Build the file-check application.

##    _hafsutils_file_check
    
    # Build the obs-preproc application.

    _hafsutils_obs_preproc

##    # Build the post-utils application.

##    _hafsutils_post_utils

##    # Build the tc-diagnostics application.

##    _hafsutils_tc_diagnostics
}

#----

# FUNCTION:

# build_nwplibs.sh

# DESCRIPTION:

# This function builds all applications and/or libraries required for
# the HAFS utility applications.

build_nwplibs (){

    # Define the top-level directory for all HAFS utility libraries.

    export HAFS_UTILS_NWPLIBS=${HAFS_UTILS_SORC}/hafs_nwplibs

    # Create a directory to contain all configure, make, and
    # installation logs.

    mkdir -p ${HAFS_UTILS_NWPLIBS}/logs

    # Move to the working directory for the HAFS utility libraries.

    cd ${HAFS_UTILS_NWPLIBS}

    # Build all utility libraries.

    make >& ${HAFS_UTILS_NWPLIBS}/logs/make.nwp-libs.log
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
    
    # Create a working directory to contain all HAFS utility
    # application executables.

    mkdir -p ${HAFS_UTILS_EXEC}

    # Load all modules and compile-time environment variables for the
    # HAFS utility applications build.

#   . ${MODULES}
# Define all compilers specific to the HAFS utility application
# builds.

export AR=/usr/bin/ar
export CC=gcc
export F77=ifort
export FC=ifort
export MKDIR=/bin/mkdir
##export MPIFC=mpif90 (Moved to build_tools.sh)
export MV=/bin/mv
export RANLIB=/usr/bin/ranlib
export RM=/bin/rm

# Define all compiler flags for the NWP-libs applications.

export NWP_LIBS_CCFLAGS="-O3"
export NWP_LIBS_DEBUG=""
export NWP_LIBS_FCFLAGS="-O3 -mcmodel=large -convert big_endian"

# Define all compiler flags for the analysis-update application.

export ANALYSIS_UPDATE_DEBUG=""
export ANALYSIS_UPDATE_FCFLAGS="-O3 -heap-arrays -mkl=sequential -convert big_endian -assume byterecl -DLINUX"

# Define all compiler flags for the da-utils application.

export DA_UTILS_DEBUG=""
export DA_UTILS_FCFLAGS="-O3 -heap-arrays -mcmodel=large -integer-size 32 -real-size 32"

# Define all compiler flags for the file-check application.

export FILE_CHECK_DEBUG=""
export FILE_CHECK_FCFLAGS="-O3 -mkl=sequential -assume byterecl"

# Define all compiler flags for the obs-preproc application.

export OBS_PREPROC_DEBUG=""
export OBS_PREPROC_FCFLAGS="-O3 -fp-model precise -assume byterecl -convert big_endian"

# Define all compiler flags for the post-utils application.

export POST_UTILS_DEBUG=""
export POST_UTILS_FCFLAGS="-O3 -mkl=sequential -convert big_endian -assume byterecl -DLINUX"

# Define all compiler flags for the tc-diagnostics application.

export TC_DIAGNOSTICS_DEBUG=""
export TC_DIAGNOSTICS_FCFLAGS="-O3 -heap-arrays -mkl=sequential -assume byterecl -mcmodel=large -DLINUX"

}

#----

script_name=`basename "$0"`
start_date=`date`
echo "START ${script_name}: ${start_date}"

# (1) Configure the local environment for the HAFS utility application
#     compilations.

setup_hafs_utils_build

# (2) Build all libraries specific to numerical weather prediction
#     (NWP) applications.

build_nwplibs

# (3) Build all HAFS utility applications.

build_hafsutils

export ERR=$?
export err=${ERR}
stop_date=`date`
echo "STOP ${script_name}: ${stop_date}"

exit ${err}
