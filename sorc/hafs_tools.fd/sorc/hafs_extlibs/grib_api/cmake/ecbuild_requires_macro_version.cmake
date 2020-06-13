# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_requires_macro_version
# ==============================
#
# Check that the ecBuild version satisfied a given minimum version or fail. ::
#
#   ecbuild_requires_macro_version( <minimum-version> )
#
##############################################################################

macro( ecbuild_requires_macro_version req_vrs )

	if( ECBUILD_MACRO_VERSION VERSION_LESS ${req_vrs} )
		ecbuild_critical( "${PROJECT_NAME} needs ecbuild macro version >= ${req_vrs}" )
	endif()

endmacro()
