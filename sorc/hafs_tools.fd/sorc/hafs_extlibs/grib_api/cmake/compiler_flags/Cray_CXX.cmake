# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( CMAKE_CXX_FLAGS_RELEASE        "-O3 -hfp3 -hscalar3 -hvector3 -DNDEBUG"        CACHE STRING "Release C++ flags"                 FORCE )
set( CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -hfp1 -Gfast -DNDEBUG"                     CACHE STRING "Release-with-debug-info C++ flags" FORCE )
set( CMAKE_CXX_FLAGS_PRODUCTION     "-O2 -hfp1 -G2"                                 CACHE STRING "Production C++ flags"              FORCE )
set( CMAKE_CXX_FLAGS_BIT            "-O2 -hfp1 -G2 -hflex_mp=conservative -DNDEBUG" CACHE STRING "Bit-reproducible C++ flags"        FORCE )
set( CMAKE_CXX_FLAGS_DEBUG          "-O0 -G0"                                       CACHE STRING "Debug CXX flags"                   FORCE )
