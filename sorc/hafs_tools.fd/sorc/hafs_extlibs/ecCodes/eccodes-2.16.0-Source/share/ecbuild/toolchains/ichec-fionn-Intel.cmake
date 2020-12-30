####################################################################
# ARCHITECTURE
####################################################################


####################################################################
# COMPILER
####################################################################

include(CMakeForceCompiler)

CMAKE_FORCE_C_COMPILER       ( icc  Intel )
CMAKE_FORCE_CXX_COMPILER     ( icpc  Intel )
CMAKE_FORCE_Fortran_COMPILER ( ifort Intel )

####################################################################
# OpenMP FLAGS
####################################################################

set( OMP_C_FLAGS             "-openmp -openmp-threadprivate=compat" )
set( OMP_CXX_FLAGS           "-openmp -openmp-threadprivate=compat" )
set( OMP_Fortran_FLAGS       "-openmp -openmp-threadprivate=compat" )

####################################################################
# COMMON FLAGS
####################################################################

# for diagnostics:
#  -diag-enable=vec -diag-file -Winline

set( ECBUILD_C_FLAGS       "-fp-speculation=strict -fp-model precise -traceback")
set( ECBUILD_CXX_FLAGS     "-fp-speculation=strict -fp-model precise -traceback" )
set( ECBUILD_Fortran_FLAGS "-fp-speculation=strict -fp-model source  -convert big_endian -assume byterecl -traceback -fpe0" )

####################################################################
# BIT REPRODUCIBLE FLAGS
####################################################################

set( ECBUILD_C_FLAGS_BIT        "-O2 -xAVX -finline-function -finline-limit=500" )
set( ECBUILD_CXX_FLAGS_BIT      "-O2 -xAVX -finline-function -finline-limit=500" )
set( ECBUILD_Fortran_FLAGS_BIT  "-O2 -xAVX -finline-function -finline-limit=500 -align array64byte" )

####################################################################
# DEBUG FLAGS
####################################################################

set( ECBUILD_C_FLAGS_DEBUG        "-O0 -g -traceback -fp-trap=common" )
set( ECBUILD_CXX_FLAGS_DEBUG      "-O0 -g -traceback -fp-trap=common" )
# -check all implies -check bounds
set( ECBUILD_Fortran_FLAGS_DEBUG  "-O0 -g -traceback -warn all -heap-arrays -fpe-all=0 -fpe:0 -check all" )

####################################################################
# LINK FLAGS
####################################################################

set( ECBUILD_C_LINK_FLAGS        "-Wl,-Map,load.map -Wl,--as-needed" )
set( ECBUILD_CXX_LINK_FLAGS      "-Wl,-Map,load.map -Wl,--as-needed" )
set( ECBUILD_Fortran_LINK_FLAGS  "-Wl,-Map,load.map -Wl,--as-needed" )

###################################################################
# 
# Serial versions of these packages (need to specify intel_mpi versions? )
###################################################################

set( FFTW_PATH    "/ichec/packages/fftw/intel/3.3.4")
set( NETCDF_PATH  "/ichec/packages/netcdf/intel/4.4.0")
set( HDF5_PATH    "/ichec/packages/hdf5/intel/1.8.16")
