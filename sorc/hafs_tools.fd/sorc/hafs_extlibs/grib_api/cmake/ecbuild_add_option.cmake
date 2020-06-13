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
  set( multi_value_args  REQUIRED_PACKAGES CONDITION )

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
  if( DEFINED _p_CONDITION )
    set(_feature_condition_file "${CMAKE_CURRENT_BINARY_DIR}/set_${_p_FEATURE}_condition.cmake")
    file( WRITE  ${_feature_condition_file} "  if( ")
    foreach( term ${_p_CONDITION} )
      file( APPEND ${_feature_condition_file} " ${term}")
    endforeach()
    file( APPEND ${_feature_condition_file} " )\n    set(_${_p_FEATURE}_condition TRUE)\n  else()\n    set(_${_p_FEATURE}_condition FALSE)\n  endif()\n")
    include( ${_feature_condition_file} )
    ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): checking condition ${_p_CONDITION} -> ${_${_p_FEATURE}_condition}")
  else()
    set( _${_p_FEATURE}_condition TRUE )
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
      foreach( pkg ${_p_REQUIRED_PACKAGES} )
        ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for dependent package ${pkg}")

        string(REPLACE " " ";" pkglist ${pkg}) # string to list

        list( GET pkglist 0 pkgname )

        if( pkgname STREQUAL "PROJECT" )  # if 1st entry is PROJECT, then we are looking for a ecbuild project
          set( pkgproject 1 )
          list( GET pkglist 1 pkgname )
          # Use feature description as package description if there is none
          list( FIND pkglist DESCRIPTION __description )
          if( __description LESS 0 )
            ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): no description for ${pkgname}, using feature description '${_p_DESCRIPTION}'")
            list( APPEND pkglist DESCRIPTION "${_p_DESCRIPTION}" )
          endif()
        else()                            # else 1st entry is package name
          set( pkgproject 0 )
        endif()

        # ecbuild_debug_var( pkg )
        # ecbuild_debug_var( pkglist )
        # ecbuild_debug_var( pkgname )

        string( TOUPPER ${pkgname} pkgUPPER )
        string( TOLOWER ${pkgname} pkgLOWER )

        set( __help_msg "Provide ${pkgname} location with -D${pkgUPPER}_PATH=/..." )
        if( ${pkgname}_FOUND OR ${pkgUPPER}_FOUND OR ${pkgLOWER}_FOUND )

          ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): ${pkgname} has already been found")
          set( ${pkgname}_already_found 1 )

        else()

          if( pkgproject )

            ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for ecbuild project ${pkgname} - ecbuild_use_package( ${pkglist} )")
            ecbuild_use_package( ${pkglist} )

          else()

            if( pkgname STREQUAL "LAPACK" )
              ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for LAPACK - ecbuild_find_package( NAME ${pkglist} )")
              ecbuild_find_package( NAME ${pkglist} )
              if( HAVE_LAPACK AND TARGET lapack )
                ecbuild_debug( "LAPACK found as CMake target lapack" )
                set( LAPACK_LIBRARIES lapack )
              endif()
            elseif( pkgname STREQUAL "MPI" )
              set( _find_args ${pkglist} )
              list( REMOVE_ITEM _find_args "MPI" )
              ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for MPI - ecbuild_find_mpi( ${_find_args} )")
              ecbuild_find_mpi( ${_find_args} )
            elseif( pkgname STREQUAL "OMP" )
              set( _find_args ${pkglist} )
              list( REMOVE_ITEM _find_args "OMP" )
              if( NOT ENABLE_${_p_FEATURE} )
                list( APPEND _find_args STUBS )
              endif()
              ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for OpenMP - ecbuild_find_omp( ${_find_args} )")
              ecbuild_find_omp( ${_find_args} )
            elseif( pkgname STREQUAL "Python" OR pkgname STREQUAL "PYTHON" )
              set( _find_args ${pkglist} )
              list( REMOVE_ITEM _find_args ${pkgname} )
              ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for Python - ecbuild_find_python( ${_find_args} )")
              ecbuild_find_python( ${_find_args} )
              set( __help_msg "Specify the location of the Python interpreter with -DPYTHON_EXECUTABLE=/..." )
            elseif( pkgname STREQUAL "LEXYACC" )
              set( _find_args ${pkglist} )
              list( REMOVE_ITEM _find_args ${pkgname} )
              ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for lex-yacc - ecbuild_find_lexyacc( ${_find_args} )")
              ecbuild_find_lexyacc( ${_find_args} )
            else()
              ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): searching for package ${pkgname} - find_package( ${pkglist} )")
              find_package( ${pkglist} )
            endif()

          endif()

        endif()

        # ecbuild_debug_var( ${pkgname}_FOUND  )
        # ecbuild_debug_var( ${pkgLOWER}_FOUND )
        # ecbuild_debug_var( ${pkgUPPER}_FOUND )

        # we have feature if all required packages were FOUND

        if( ${pkgname}_FOUND OR ${pkgUPPER}_FOUND OR ${pkgLOWER}_FOUND )
          ecbuild_info( "Found package ${pkgname} required for feature ${_p_FEATURE}" )

          # append to list of third-party libraries (to be forward to other packages )
          # unless the NO_TPL option was given
          if( NOT _p_NO_TPL )
            ecbuild_debug("ecbuild_add_option(${_p_FEATURE}): appending ${pkgname} to ${PROJECT_NAME_CAPS}_TPLS")
            list( APPEND ${PROJECT_NAME_CAPS}_TPLS ${pkgname} )
            list( REMOVE_DUPLICATES ${PROJECT_NAME_CAPS}_TPLS )
          endif()

        else()
          ecbuild_info( "Could NOT find package ${pkgname} required for feature ${_p_FEATURE} -- ${__help_msg}" )
          set( HAVE_${_p_FEATURE} 0 )
          list( APPEND _failed_to_find_packages ${pkgname} )
        endif()

      endforeach()
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

  set( ${PROJECT_NAME_CAPS}_HAVE_${_p_FEATURE} ${HAVE_${_p_FEATURE}} )

endmacro( ecbuild_add_option  )
