# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_target_flags
# ====================
#
# Override compiler flags for a given target. ::
#
#   ecbuild_target_flags( <target> <c_flags> <cxx_flags> <fortran_flags> )
#
# Required arguments:
#
# :target:        Target name
# :c_flags:       Target specific C flags (can be empty)
# :cxx_flags:     Target specific CXX flags (can be empty)
# :fortran_flags: Target specific Fortran flags (can be empty)
#
# There are 3 cases, only the first applicable case takes effect:
#
# 1.  Use custom rules from user specified ``ECBUILD_COMPILE_FLAGS`` file and
#     append target specific flags.
#
# 2.  Use JSON rules from user specified ``ECBUILD_SOURCE_FLAGS`` file and
#     append target specific flags.
#
# 3.  Only the target specific flags are applied to all matching source files.
#
##############################################################################

function( ecbuild_target_flags target c_flags cxx_flags fortran_flags )

  get_property( languages GLOBAL PROPERTY ENABLED_LANGUAGES )

  foreach( lang ${languages} )

    string( TOLOWER ${lang} l )

    if( ${target}_${l}_srcs )

      # 1) Override compile flags from user specified CMake file
      if( ECBUILD_COMPILE_FLAGS )

        # Project specific flags for current language and optionally build type
        set( pflags "${${PNAME}_${lang}_FLAGS} ${${PNAME}_${lang}_FLAGS_${CMAKE_BUILD_TYPE_CAPS}}" )

        foreach( src ${${target}_${l}_srcs} )
          get_property( oflags SOURCE ${src} PROPERTY OVERRIDE_COMPILE_FLAGS )
          get_property( oflags_btype SOURCE ${src} PROPERTY OVERRIDE_COMPILE_FLAGS_${CMAKE_BUILD_TYPE_CAPS} )
          # Override compile flags for source file?
          if( oflags OR oflags_btype )
            set_source_files_properties( ${src} PROPERTIES COMPILE_FLAGS "${oflags} ${oflags_btype}" )
            ecbuild_debug( "ecbuild_target_flags(${target}): overriding flags for ${src} with '${oflags} ${oflags_btype}'" )
          # Otherwise append source file specific flags to project specific and target specific flags
          else()
            get_property( flags SOURCE ${src} PROPERTY COMPILE_FLAGS )
            get_property( flags_btype SOURCE ${src} PROPERTY COMPILE_FLAGS_${CMAKE_BUILD_TYPE_CAPS} )
            set_source_files_properties( ${src} PROPERTIES COMPILE_FLAGS "${pflags} ${${l}_flags} ${flags} ${flags_btype}" )
            ecbuild_debug( "ecbuild_target_flags(${target}): setting flags for ${src} to '${pflags} ${${l}_flags} ${flags} ${flags_btype}'" )
          endif()
        endforeach()

      # 2) Override compile flags from user specified JSON file
      elseif( ECBUILD_SOURCE_FLAGS )
        ecbuild_source_flags( ${target}_${lang}_SOURCE_FLAGS
                              ${target}_${l}
                              "${${l}_flags}"
                              "${${target}_${l}_srcs}" )

        ecbuild_debug("ecbuild_target_flags(${target}): setting source file ${lang} flags from ${${target}_${lang}_SOURCE_FLAGS}")
        include( ${${target}_${lang}_SOURCE_FLAGS} )

      # 3) Use target specific compile flags
      elseif( ${l}_flags )

        set_source_files_properties( ${${target}_${l}_srcs} PROPERTIES COMPILE_FLAGS "${${l}_flags}" )
        ecbuild_debug("ecbuild_target_flags(${target}): setting flags for '${${target}_${l}_srcs}' to '${${l}_flags}'")

      endif()
    endif()

  endforeach()

endfunction()
