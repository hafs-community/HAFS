# (C) Copyright 2019- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

if( NOT ECBUILD_PROJECT_${CMAKE_CURRENT_SOURCE_DIR} )
set( ECBUILD_PROJECT_${CMAKE_CURRENT_SOURCE_DIR} TRUE )

########################################################################################################
# compatibility with ecbuild 2

if ( NOT DEFINED ECBUILD_2_COMPAT_VALUE )
    set( ECBUILD_2_COMPAT_VALUE ON )
endif()
if ( NOT DEFINED ECBUILD_2_COMPAT_DEPRECATE_VALUE )
    set( ECBUILD_2_COMPAT_DEPRECATE_VALUE OFF )
endif()

option( ECBUILD_2_COMPAT "Keep compatibility with ecbuild 2" ${ECBUILD_2_COMPAT_VALUE} )
option( ECBUILD_2_COMPAT_DEPRECATE "Emit deprecation warnings in the compatibility layer" ${ECBUILD_2_COMPAT_DEPRECATE_VALUE} )

if( ECBUILD_2_COMPAT_DEPRECATE )
    set( CMAKE_WARN_DEPRECATED ON )
endif()

########################################################################################################

include( ecbuild_project )

########################################################################################################

endif()
