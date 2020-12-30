# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( CMAKE_C_FLAGS_RELEASE        "-O3 -DNDEBUG"    CACHE STRING "C compiler flags for Release builds"          FORCE )
set( CMAKE_C_FLAGS_BIT            "-O2 -DNDEBUG"    CACHE STRING "C compiler flags for Bit-reproducible builds" FORCE )
set( CMAKE_C_FLAGS_DEBUG          "-O0 -g"          CACHE STRING "C compiler flags for Debug builds"            FORCE )
set( CMAKE_C_FLAGS_PRODUCTION     "-O3 -g"          CACHE STRING "C compiler flags for Production builds."      FORCE )
set( CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG" CACHE STRING "C compiler flags for RelWithDebInfo builds."  FORCE )
