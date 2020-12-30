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
# ecbuild_add_option
# ==================
#
# Add a CMake configuration option, which may depend on a list of packages. ::
#
#   ecbuild_add_option( FEATURE <name>
#                       [ DEFAULT ON|OFF ]
#                       [ DESCRIPTION <description> ]
#                       [ REQUIRED_PACKAGES <package1> [<package2> ...] ]
#                       [ CONDITION <condition> ]
#                       [ ADVANCED ] [ NO_TPL ] )
#
# Options
# -------
#
# FEATURE : required
#   name of the feature / option
#
# DEFAULT : optional, defaults to ON
#   if set to ON, the feature is enabled even if not explicitly requested
#
# DESCRIPTION : optional
#   string describing the feature (shown in summary and stored in the cache)
#
# REQUIRED_PACKAGES : optional
#   list of packages required to be found for this feature to be enabled
#
#   The package specification can have one of two forms. Either ::
#
#     "<package> [ <version> ... ]"
#
#   to search for a given package using the CMake ``find_package`` mechanism.
#   The entire specification must be enclosed in quotes and is passed on
#   verbatim. Any options of ``find_package`` are supported.
#
#   The other specification must start with ``PROJECT`` like this ::
#
#     "PROJECT <name> [ VERSION <version> ... ]"
#
#   and is used to search for an ecBuild project via ``ecbuild_use_package``.
#   The entire specification must be enclosed in quotes and is passed on
#   verbatim. Any options of ``ecbuild_use_package`` are supported.
#
#   .. note::
#
#     Arguments inside the package string that require quoting need to use the
#     `bracket argument syntax`_ introduced in CMake 3.0 since
#     regular quotes even when escaped are swallowed by the CMake parser.
#
#     Alternatively, the name of a CMake variable containing the string can be
#     passed, which will be expanded by ``ecbuild_find_package``: ::
#
#       set( ECCODES_FAIL_MSG
#            "grib_api can be used instead (select with -DENABLE_ECCODES=OFF)" )
#       ecbuild_add_option( FEATURE ECCODES
#                           DESCRIPTION "Use eccodes instead of grib_api"
#                           REQUIRED_PACKAGES "PROJECT eccodes REQUIRED FAILURE_MSG ECCODES_FAIL_MSG"
#                           DEFAULT ON )
#
# CONDITION : optional
#   conditional expression which must evaluate to true for this option to be
#   enabled (must be valid in a CMake ``if`` statement)
#
# ADVANCED : optional
#   mark the feature as advanced
#
# NO_TPL : optional
#   do not add any ``REQUIRED_PACKAGES`` to the list of third party libraries
#
# Usage
# -----
#
# Features with ``DEFAULT OFF`` need to be explcitly enabled by the user with
# ``-DENABLE_<FEATURE>=ON``. If a feature is enabled, all ``REQUIRED_PACKAGES``
# are found and ``CONDITION`` is met, ecBuild sets the variable
# ``HAVE_<FEATURE>`` to ``ON``. This is the variable to use to check for the
# availability of the feature.
#
# If a feature is explicitly enabled but the required packages are not found,
# configuration fails. This only applies when configuring from *clean cache*.
# With an already populated cache, use ``-DENABLE_<FEATURE>=REQUIRE`` to make
# the feature a required feature (this cannot be done via the CMake GUI).
#
# .. _bracket argument syntax: https://cmake.org/cmake/help/latest/manual/cmake-language.7.html#bracket-argument
#
##############################################################################

macro( ecbuild_add_option )

  set( options ADVANCED NO_TPL )
  set( single_value_args FEATURE DEFAULT DESCRIPTION TYPE PURPOSE )
  if( ECBUILD_2_COMPAT )
    set( multi_value_args REQUIRED_PACKAGES CONDITION )
  else()
    set( multi_value_args CONDITION )
  endif()

  cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if( _p_UNPARSED_ARGUMENTS )
    ecbuild_critical("Unknown keywords given to ecbuild_add_option(): \"${_p_UNPARSED_ARGUMENTS}\"")
  endif()

  # check FEATURE parameter

  if( NOT _p_FEATURE  )
    ecbuild_critical("The call to ecbuild_add_option() doesn't specify the FEATURE.")
  endif()

  # check DEFAULT parameter

  if( NOT DEFINED _p_DEFAULT )
    set( _p_DEFAULT ON )
  else()
    if( NOT _p_DEFAULT MATCHES "[Oo][Nn]" AND NOT _p_DEFAULT MATCHES "[Oo][Ff][Ff]" )
      ecbuild_critical("In macro ecbuild_add_option(), DEFAULT is either ON or OFF: \"${_p_DEFAULT}\"")
    endif()
  endif()
  ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): defaults to ${_p_DEFAULT}")

  if( _p_PURPOSE  )
    ecbuild_deprecate( "ecbuild_add_option: argument PURPOSE is ignored and will be removed in a future release." )
  endif()
  if( _p_TYPE  )
    ecbuild_deprecate( "ecbuild_add_option: argument TYPE is ignored and will be removed in a future release." )
  endif()

  # check CONDITION parameter
  ecbuild_evaluate_dynamic_condition( _p_CONDITION _${_p_FEATURE}_condition  )

  # Disable deprecation warnings until end of macro, because "ENABLE_<FEATURE>" may already have been
  #   marked with "ecbuild_mark_compat()" in a bundle.
  if( ECBUILD_2_COMPAT )
    set( DISABLE_ECBUILD_DEPRECATION_WARNINGS_orig ${DISABLE_ECBUILD_DEPRECATION_WARNINGS} )
    set( DISABLE_ECBUILD_DEPRECATION_WARNINGS ON )
  endif()

  # Check if user explicitly enabled/disabled the feature in cache
  get_property( _in_cache CACHE ENABLE_${_p_FEATURE} PROPERTY VALUE SET )

  # A feature set to REQUIRE is always treated as explicitly enabled
  if( ENABLE_${_p_FEATURE} MATCHES "REQUIRE" )
    set( ENABLE_${_p_FEATURE} ON CACHE BOOL "" FORCE )
    ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): ENABLE_${_p_FEATURE} was required")
    set( ${_p_FEATURE}_user_provided_input 1 CACHE BOOL "" FORCE )
  elseif( NOT ENABLE_${_p_FEATURE} STREQUAL "" AND _in_cache )
    ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): ENABLE_${_p_FEATURE}=${ENABLE_${_p_FEATURE}} was found in cache")
    set( ${_p_FEATURE}_user_provided_input 1 CACHE BOOL "" )
  else()
    ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): ENABLE_${_p_FEATURE} not found in cache")
    set( ${_p_FEATURE}_user_provided_input 0 CACHE BOOL "" )
  endif()

  mark_as_advanced( ${_p_FEATURE}_user_provided_input )


  # define the option -- for cmake GUI

  option( ENABLE_${_p_FEATURE} "${_p_DESCRIPTION}" ${_p_DEFAULT} )
  get_property( _feature_desc GLOBAL PROPERTY _CMAKE_${_p_FEATURE}_DESCRIPTION )
  if( _feature_desc )
    add_feature_info( ${_p_FEATURE} ENABLE_${_p_FEATURE} "${_feature_desc}, ${PROJECT_NAME}: ${_p_DESCRIPTION}" )
  else()
    add_feature_info( ${_p_FEATURE} ENABLE_${_p_FEATURE} "${PROJECT_NAME}: ${_p_DESCRIPTION}" )
  endif()

  ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): defining option ENABLE_${_p_FEATURE} '${_p_DESCRIPTION}' ${_p_DEFAULT}")
  ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): ENABLE_${_p_FEATURE}=${ENABLE_${_p_FEATURE}}")

  if( ENABLE_${_p_FEATURE} )
    ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): feature enabled")

    set( HAVE_${_p_FEATURE} 1 )

    if( _${_p_FEATURE}_condition )

      ### search for dependent packages

      set( _failed_to_find_packages )  # clear variable
      if( ECBUILD_2_COMPAT )
        if( ECBUILD_2_COMPAT_DEPRECATE AND _p_REQUIRED_PACKAGES )
          ecbuild_deprecate("Keyword REQUIRED_PACKAGES of ecbuild_add_option is deprecated, "
            "please include the package and use CONDITION \${package}_FOUND instead")
        endif()

        foreach( pkg ${_p_REQUIRED_PACKAGES} )
          ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for dependent package ${pkg}")

          if(${_p_NO_TPL})
            set(_no_tpl NO_TPL)
          else()
            set(_no_tpl)
          endif()
          ecbuild_compat_require(pkgname ${pkg} ${_no_tpl} FEATURE "${_p_FEATURE}" DESCRIPTION "${_p_DESCRIPTION}")

          # we have feature if all required packages were FOUND
          if( ${pkgname}_FOUND )
            ecbuild_info( "Found package ${pkgname} required for feature ${_p_FEATURE}" )
          else()
            ecbuild_info( "Could NOT find package ${pkgname} required for feature ${_p_FEATURE} -- ${${pkgname}_HELP_MSG}" )
            set( HAVE_${_p_FEATURE} 0 )
            list( APPEND _failed_to_find_packages ${pkgname} )
          endif()

        endforeach()
      endif()
    else( _${_p_FEATURE}_condition )
      set( HAVE_${_p_FEATURE} 0 )
    endif( _${_p_FEATURE}_condition )

    # FINAL CHECK

    if( HAVE_${_p_FEATURE} )

      ecbuild_enable_feature( ${_p_FEATURE} )

      ecbuild_info( "Feature ${_p_FEATURE} enabled" )

    else() # if user provided input and we cannot satisfy FAIL otherwise WARN

      ecbuild_disable_feature( ${_p_FEATURE} )

      if( ${_p_FEATURE}_user_provided_input )
        if( NOT _${_p_FEATURE}_condition )
          string(REPLACE ";" " " _condition_msg "${_p_CONDITION}")
          ecbuild_critical( "Feature ${_p_FEATURE} cannot be enabled -- following condition was not met: ${_condition_msg}" )
        else()
          ecbuild_critical( "Feature ${_p_FEATURE} cannot be enabled -- following required packages weren't found: ${_failed_to_find_packages}" )
        endif()
      else()
        if( NOT _${_p_FEATURE}_condition )
          string(REPLACE ";" " " _condition_msg "${_p_CONDITION}")
          ecbuild_info( "Feature ${_p_FEATURE} was not enabled (also not requested) -- following condition was not met: ${_condition_msg}" )
        else()
          ecbuild_info( "Feature ${_p_FEATURE} was not enabled (also not requested) -- following required packages weren't found: ${_failed_to_find_packages}" )
        endif()
        set( ENABLE_${_p_FEATURE} OFF )
        ecbuild_disable_feature( ${_p_FEATURE} )
      endif()

    endif()

  else()

    ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): feature disabled")
    set( HAVE_${_p_FEATURE} 0 )
    ecbuild_disable_feature( ${_p_FEATURE} )

  endif()


  if( ${_p_ADVANCED} )
    mark_as_advanced( ENABLE_${_p_FEATURE} )
  endif()

  set( ${PROJECT_NAME}_HAVE_${_p_FEATURE} ${HAVE_${_p_FEATURE}} )

  if(ECBUILD_2_COMPAT)
    set(ENABLE_${_p_FEATURE} ${ENABLE_${_p_FEATURE}})
    ecbuild_mark_compat(ENABLE_${_p_FEATURE} "HAVE_${_p_FEATURE} or ${PROJECT_NAME}_HAVE_${_p_FEATURE}")

    if (NOT "${PROJECT_NAME_CAPS}" STREQUAL "${PROJECT_NAME}")
      ecbuild_declare_compat( ${PROJECT_NAME_CAPS}_HAVE_${_p_FEATURE} ${PROJECT_NAME}_HAVE_${_p_FEATURE})
    endif()
  endif()

  if( ECBUILD_2_COMPAT )
    set( DISABLE_ECBUILD_DEPRECATION_WARNINGS ${DISABLE_ECBUILD_DEPRECATION_WARNINGS_orig} )
  endif()

endmacro( ecbuild_add_option  )
