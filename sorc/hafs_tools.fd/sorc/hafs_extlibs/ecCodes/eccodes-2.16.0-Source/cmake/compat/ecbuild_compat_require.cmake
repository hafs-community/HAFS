# (C) Copyright 2019- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_compat_require
# ======================
#
# Emulate the behaviour of the REQUIRED_PACKAGES option of ecbuild_add_option ::
#
#   ecbuild_compat_require(<namevar> <package>
#                          [FEATURE <name>]
#                          [DESCRIPTION <description>])
#
# Options
# -------
# <namevar> : required
#   variable to retrieve the name of the package
#
# <package> : required
#   string describing the package to be imported
#
# FEATURE : optional
#   name of the feature / option requiring the package
#
# DESCRIPTION : optional
#   string describing the feature (used if the package has none)
#
# Usage
# -----
#
# This function is primarily for use in ecbuild_add_option. Do NOT use it in
# new code. The <package> specification should be a string containing valid
# arguments to either ecbuild_use_package or find_package.
#
# Output variables
# ----------------
#
# * <namevar> will be set to the required package name <pname>,
# * <pname>_FOUND will be set if the package has been found,
# * <pname>_HELP_MSG will contain some help if the package has not been found.
#
##############################################################################

macro(ecbuild_compat_require out_name pkg)
  set(options NO_TPL)
  set(single_value_args FEATURE DESCRIPTION)
  cmake_parse_arguments(_p "${options}" "${single_value_args}" "" ${ARGN})

  if( _p_UNPARSED_ARGUMENTS )
    ecbuild_critical("Unknown keywords given to ecbuild_compat_require(): \"${_p_UNPARSED_ARGUMENTS}\"")
  endif()

  string(REPLACE " " ";" pkglist ${pkg}) # string to list

  list(GET pkglist 0 pkgname)

  if(pkgname STREQUAL "PROJECT")  # if 1st entry is PROJECT, then we are looking for an ecbuild project
    set(pkgproject 1)
    list(GET pkglist 1 pkgname)

    list(FIND pkglist DESCRIPTION __description)
    if( __description LESS 0 )
      ecbuild_debug("ecbuild_compat_require(${pkgname}): no package description found, using provided description '${_p_DESCRIPTION}'")
      list(APPEND pkglist DESCRIPTION "${_p_DESCRIPTION}")
    endif()
  else()                          # else 1st entry is the package name
    set(pkgproject 0)
  endif()

  string(TOUPPER ${pkgname} pkgUPPER)
  string(TOLOWER ${pkgname} pkgLOWER)

  # export the package name
  set(${out_name} ${pkgname})

  set(${pkgname}_HELP_MSG "Provide ${pkgname} location with -D${pkgUPPER}_PATH=/...")
  if(${pkgname}_FOUND OR ${pkgUPPER}_FOUND OR ${pkgLOWER}_FOUND)

    ecbuild_debug("ecbuild_compat_require(${pkgname}): ${pkgname} has already been found")

  else()

    if(pkgproject)

      ecbuild_debug("ecbuild_compat_require(${pkgname}): calling ecbuild_use_package(${pkglist})")
      ecbuild_use_package(${pkglist})

    else()

      if(pkgname STREQUAL "LAPACK")
        ecbuild_debug("ecbuild_compat_require(${pkgname}): searching for LAPACK - ecbuild_find_package(NAME ${pkglist})")
        ecbuild_find_package(NAME ${pkglist})
        if(HAVE_LAPACK AND TARGET lapack)
          ecbuild_debug("LAPACK found as CMake target lapack")
          set(LAPACK_LIBRARIES lapack)
        endif()

      elseif(pkgname STREQUAL "MPI")
        set(_find_args ${pkglist})
        list(REMOVE_ITEM _find_args "MPI")
        ecbuild_debug("ecbuild_compat_require(${pkgname}): searching for MPI - ecbuild_find_mpi(${_find_args})")
        ecbuild_find_mpi(${_find_args})

      elseif(pkgname STREQUAL "OMP")
        set(_find_args ${pkglist})
        list(REMOVE_ITEM _find_args "OMP")
        if(NOT ENABLE_${_p_FEATURE})
          list(APPEND _find_args STUBS)
        endif()
        ecbuild_debug("ecbuild_compat_require(${pkgname}): searching for OpenMP - ecbuild_find_omp(${_find_args})")
        ecbuild_find_omp(${_find_args})

      elseif(pkgname STREQUAL "Python" OR pkgname STREQUAL "PYTHON")
        set(_find_args ${pkglist})
        list(REMOVE_ITEM _find_args ${pkgname})
        ecbuild_debug("ecbuild_compat_require(${pkgname}): searching for Python - ecbuild_find_python(${_find_args})")
        ecbuild_find_python(${_find_args})
        set(${pkgname}_HELP_MSG "Specify the location of the Python interpreter with -DPYTHON_EXECUTABLE=/...")

      elseif(pkgname STREQUAL "LEXYACC")
        set(_find_args ${pkglist})
        list(REMOVE_ITEM _find_args ${pkgname})
        ecbuild_debug("ecbuild_compat_require(${pkgname}): searching for lex-yacc - ecbuild_find_lexyacc(${_find_args})")
        ecbuild_find_lexyacc(${_find_args})

      else()
        ecbuild_debug("ecbuild_compat_require(${pkgname}): searching for package ${pkgname} - find_package(${pkglist})")

        list(FIND pkglist "VERSION" _ver_found)
        if(NOT _ver_found EQUAL -1)
          # XXX: This is an invalid syntax used in IFS (that gets short-circuited with ecbuild 2.x
          # because the packages are already found)
          set(_pkglist_old ${pkglist})
          list(REMOVE_ITEM pkglist "VERSION")

          string(REPLACE ";" " " _pkglist_old "${_pkglist_old}")
          string(REPLACE ";" " " _pkglist_new "${pkglist}")
          ecbuild_warn("Removing unexpected VERSION keyword from REQUIRED_PACKAGES: \
                        either use 'PROJECT ${_pkglist_old}' or '${_pkglist_new}'")
        endif()

        find_package(${pkglist})

      endif()

    endif()

  endif()

  if(${pkgname}_FOUND OR ${pkgUPPER}_FOUND OR ${pkgLOWER}_FOUND)
    set(${pkgname}_FOUND 1) # make sure this one is defined for consistency

    # append to list of third-party libraries (to be forward to other packages )
    # unless the NO_TPL option was given
    if(NOT _p_NO_TPL)
      ecbuild_debug("ecbuild_compat_require(${pkgname}): appending ${pkgname} to ${PROJECT_NAME_CAPS}_TPLS")
      list(APPEND ${PROJECT_NAME_CAPS}_TPLS ${pkgname})
      list(REMOVE_DUPLICATES ${PROJECT_NAME_CAPS}_TPLS)
    endif()

  endif()
endmacro()
