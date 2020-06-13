# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#Sets:
# RPCGEN_FOUND           = prcgen was found
# RPCGEN_EXECUTABLE      = the executable rpcgen

if( DEFINED RPCGEN_PATH )
    find_program( RPCGEN_EXECUTABLE NAMES rpcgen PATHS ${RPCGEN_PATH} PATH_SUFFIXES bin NO_DEFAULT_PATH )
endif()
find_program( RPCGEN_EXECUTABLE NAMES rpcgen )

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args( RPCGEN  DEFAULT_MSG RPCGEN_EXECUTABLE )
