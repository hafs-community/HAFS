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
# ecbuild_add_cxx11_flags
# =======================
#
# Add cxx11 flags to CXX compilation flags. ::
#
#   ecbuild_add_cxx11_flags()
#
# This macro uses macros from http://github.com/UCL/GreatCMakeCookOff.
#
##############################################################################

macro( ecbuild_add_cxx11_flags )

	# if( CMAKE_COMPILER_IS_GNUCXX )
	# 	if( CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.7 )
	# 		ecbuild_add_cxx_flags("-std=c++0x")
	# 	else()
	# 		ecbuild_add_cxx_flags("-std=c++11")
	# 	endif()
	# endif()

	include( ${ECBUILD_MACROS_DIR}/contrib/GreatCMakeCookOff/AddCPP11Flags.cmake )

endmacro( ecbuild_add_cxx11_flags )
