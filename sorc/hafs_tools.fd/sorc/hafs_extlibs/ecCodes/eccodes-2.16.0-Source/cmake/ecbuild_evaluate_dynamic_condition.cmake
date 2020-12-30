# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


##############################################################################
#.rst:
#
# ecbuild_evaluate_dynamic_condition
# ==================================
#
# Add a CMake configuration option, which may depend on a list of packages. ::
#
#   ecbuild_evaluate_dynamic_condition( condition outVariable )
#
# Options
# -------
# condition A list of boolean statements like OPENSSL_FOUND AND ENABLE_OPENSSL
#
function(ecbuild_evaluate_dynamic_condition _conditions _outVar)
  if( DEFINED ${_conditions})
    if(${${_conditions}})
      set( ${_outVar} TRUE )
    else()
      set( ${_outVar} FALSE )
    endif()
  else()
    set( ${_outVar} TRUE )
  endif()
  ecbuild_debug("ecbuild_evaluate_dynamic_condition(${_outVar}): checking condition '${${_conditions}}' -> ${${_outVar}}")
  set( ${_outVar} ${${_outVar}} PARENT_SCOPE )
endfunction()
