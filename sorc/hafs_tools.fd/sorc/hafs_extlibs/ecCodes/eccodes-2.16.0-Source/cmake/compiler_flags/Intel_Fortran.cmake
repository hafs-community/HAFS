# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( Fortran_AUTOMATIC_ARRAYS_LIMIT 32768 )  # (32 kb)
math( EXPR Fortran_AUTOMATIC_ARRAYS_LIMIT_KB "${Fortran_AUTOMATIC_ARRAYS_LIMIT}/1024" )

set( Fortran_FLAG_STACK_ARRAYS     "-no-heap-arrays" )
set( Fortran_FLAG_AUTOMATIC_ARRAYS "-heap-arrays ${Fortran_AUTOMATIC_ARRAYS_LIMIT_KB}" )

set( CMAKE_Fortran_FLAGS_RELEASE        "-O3 -DNDEBUG -unroll -inline ${Fortran_FLAG_AUTOMATIC_ARRAYS}" CACHE STRING "Release Fortran flags"                 FORCE )
set( CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG ${Fortran_FLAG_AUTOMATIC_ARRAYS}"              CACHE STRING "Release-with-debug-info Fortran flags" FORCE )
set( CMAKE_Fortran_FLAGS_BIT            "-O2 -DNDEBUG -unroll -inline ${Fortran_FLAG_AUTOMATIC_ARRAYS}" CACHE STRING "Bit-reproducible Fortran flags"        FORCE )
set( CMAKE_Fortran_FLAGS_DEBUG          "-O0 -g -traceback ${Fortran_FLAG_AUTOMATIC_ARRAYS} -check all" CACHE STRING "Debug Fortran flags"                   FORCE )
set( CMAKE_Fortran_FLAGS_PRODUCTION     "-O3 -g ${Fortran_FLAG_AUTOMATIC_ARRAYS}"                       CACHE STRING "Production Fortran compiler flags"     FORCE )

# "-check all" implies "-check bounds"
