# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

###################################################################################################
# enable C to use in system introspection

if( NOT CMAKE_C_COMPILER_LOADED )
  enable_language( C )
  ecbuild_compiler_flags( C )
endif()

############################################################################################
# try to get compiler version if cmake did not

if( NOT CMAKE_C_COMPILER_VERSION )

    set( EC_COMPILER_VERSION "?.?" )

    if( CMAKE_C_COMPILER_ID MATCHES "GNU" OR CMAKE_C_COMPILER_ID MATCHES "Intel" )
        exec_program( ${CMAKE_C_COMPILER}
                      ARGS ${CMAKE_C_COMPILER_ARG1} -dumpversion
                      OUTPUT_VARIABLE EC_COMPILER_VERSION )

        string(REGEX REPLACE "([0-9])\\.([0-9])(\\.([0-9]))?" "\\1.\\2"  EC_COMPILER_VERSION ${EC_COMPILER_VERSION} )
    endif()

    if( CMAKE_C_COMPILER_ID MATCHES "Clang" )
        exec_program( ${CMAKE_C_COMPILER}
                      ARGS ${CMAKE_C_COMPILER_ARG1} --version
                      OUTPUT_VARIABLE EC_COMPILER_VERSION )

        string(REGEX REPLACE ".*clang version ([0-9])\\.([0-9])(\\.([0-9]))?.*" "\\1.\\2" EC_COMPILER_VERSION ${EC_COMPILER_VERSION} )
    endif()

    if( CMAKE_C_COMPILER_ID MATCHES "SunPro" )
        exec_program( ${CMAKE_C_COMPILER}
                      ARGS ${CMAKE_C_COMPILER_ARG1} -V
                      OUTPUT_VARIABLE EC_COMPILER_VERSION )

        string(REGEX REPLACE ".*([0-9]+)\\.([0-9]+).*" "\\1.\\2" EC_COMPILER_VERSION ${EC_COMPILER_VERSION} )
    endif()

    if( CMAKE_C_COMPILER_ID MATCHES "XL" )
        exec_program( ${CMAKE_C_COMPILER}
                      ARGS ${CMAKE_C_COMPILER_ARG1} -qversion
                      OUTPUT_VARIABLE EC_COMPILER_VERSION )

        string(REGEX REPLACE ".*V([0-9]+)\\.([0-9]+).*" "\\1.\\2" EC_COMPILER_VERSION ${EC_COMPILER_VERSION} )

    endif()

    if( NOT EC_COMPILER_VERSION STREQUAL "?.?" )
        set(CMAKE_C_COMPILER_VERSION "${EC_COMPILER_VERSION}" )
    endif()

endif()

############################################################################################
# enable warnings

if( CMAKE_COMPILER_IS_GNUCC )

    ecbuild_add_c_flags("-pipe") # use pipe for faster compilation

    if( ENABLE_WARNINGS )
        ecbuild_add_c_flags("-Wall")
        # ecbuild_add_c_flags("-pedantic")
        # ecbuild_add_c_flags("-Wextra")
    endif()

endif()

if( CMAKE_COMPILER_IS_GNUCXX )

   ecbuild_add_cxx_flags("-pipe") # use pipe for faster compilation

    if( ENABLE_WARNINGS )
        ecbuild_add_cxx_flags("-Wall")
        #    ecbuild_add_cxx_flags("-Wextra")
    endif()

endif()

if( ENABLE_WARNINGS AND CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
  ecbuild_add_fortran_flags("-warn all")
endif()

############################################################################################
# compiler dependent fixes

# For Cray compilers add "-Wl,-Bdynamic" at very end of linker commands, in order to produce dynamic executables by default

if( "${CMAKE_C_COMPILER_ID}" STREQUAL "Cray" )
  set( CMAKE_C_LINK_EXECUTABLE "<CMAKE_C_COMPILER> <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS>  -o <TARGET> <LINK_LIBRARIES> -Wl,-Bdynamic" )
endif()

if( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Cray" )
  set( CMAKE_CXX_LINK_EXECUTABLE "<CMAKE_CXX_COMPILER> <FLAGS> <CMAKE_CXX_LINK_FLAGS> <LINK_FLAGS> <OBJECTS>  -o <TARGET> <LINK_LIBRARIES> -Wl,-Bdynamic" )
endif()

if( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Cray" )
  set(CMAKE_Fortran_LINK_EXECUTABLE "<CMAKE_Fortran_COMPILER> <CMAKE_Fortran_LINK_FLAGS> <LINK_FLAGS> <FLAGS> <OBJECTS>  -o <TARGET> <LINK_LIBRARIES> -Wl,-Bdynamic" )
endif()

############################################################################################
# Fortran compiler specific flags
# if( NOT HAVE_SINGLE_PRECISION )
#  if(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
#      ecbuild_add_fortran_flags("-r8")
#  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
#      # NOTE that if we add -fdefault-real-8 then we NEED -fdefault-double-8 to avoid quadmath
#      ecbuild_add_fortran_flags("-fdefault-real-8 -fdefault-double-8")
#  endif()
# endif()
