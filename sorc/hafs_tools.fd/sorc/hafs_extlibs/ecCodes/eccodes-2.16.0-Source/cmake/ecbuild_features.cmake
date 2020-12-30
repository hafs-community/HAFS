# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Internal macros to handle CMake features

include( FeatureSummary )

# Write list of enabled features to CMake variable ${OUT}
macro( ecbuild_enabled_features OUT )
    get_property( ${OUT}  GLOBAL PROPERTY ENABLED_FEATURES )
endmacro()

# Write list of disabled features to CMake variable ${OUT}
macro( ecbuild_disabled_features OUT )
    get_property( ${OUT}  GLOBAL PROPERTY DISABLED_FEATURES )
endmacro()

# Enable the feature ${_name} (add to enabled features, remove from disabled)
function( ecbuild_enable_feature _name )

  get_property( _enabled_features  GLOBAL PROPERTY ENABLED_FEATURES )
  get_property( _disabled_features GLOBAL PROPERTY DISABLED_FEATURES )

  if( _disabled_features )
    list( REMOVE_ITEM _disabled_features ${_name} )
  endif()

  list( APPEND _enabled_features ${_name} )
  list( REMOVE_DUPLICATES _enabled_features )

  set_property(GLOBAL PROPERTY ENABLED_FEATURES  "${_enabled_features}" )
  set_property(GLOBAL PROPERTY DISABLED_FEATURES "${_disabled_features}" )

endfunction()

# Disable the feature ${_name} (add to disabled features, remove from enabled)
function( ecbuild_disable_feature _name )

  get_property( _enabled_features  GLOBAL PROPERTY ENABLED_FEATURES )
  get_property( _disabled_features GLOBAL PROPERTY DISABLED_FEATURES )

  if( _enabled_features )
    list( REMOVE_ITEM _enabled_features ${_name} )
  endif()

  list( APPEND _disabled_features ${_name} )
  list( REMOVE_DUPLICATES _disabled_features )

  set_property(GLOBAL PROPERTY ENABLED_FEATURES  "${_enabled_features}" )
  set_property(GLOBAL PROPERTY DISABLED_FEATURES "${_disabled_features}" )

endfunction()
