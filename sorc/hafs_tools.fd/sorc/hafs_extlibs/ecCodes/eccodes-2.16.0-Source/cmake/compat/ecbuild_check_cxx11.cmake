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
# ecbuild_check_cxx11
# ===================
#
# REMOVED
##############################################################################

function(ecbuild_check_cxx11)

  if(ECBUILD_COMPAT_DEPRECATE)
    ecbuild_deprecate("The ecbuild_check_cxx11 has been removed. Please use "
      "CMake facilities for C++11 features")
  endif()

endfunction(ecbuild_check_cxx11)
