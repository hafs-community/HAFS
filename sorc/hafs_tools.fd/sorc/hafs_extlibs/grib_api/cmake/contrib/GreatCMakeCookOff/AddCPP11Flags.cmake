include(CheckCXXCompilerFlag)

# On older cmake versions + newer compilers, 
# the given version of CheckCXXCompilerFlags does not quite work.
if(CMAKE_VERSION VERSION_LESS 2.8.9)
  macro (CHECK_CXX_COMPILER_FLAG _FLAG _RESULT)
     set(SAFE_CMAKE_REQUIRED_DEFINITIONS "${CMAKE_REQUIRED_DEFINITIONS}")
     set(CMAKE_REQUIRED_DEFINITIONS "${_FLAG}")
     CHECK_CXX_SOURCE_COMPILES("int main() { return 0;}" ${_RESULT}
       # Some compilers do not fail with a bad flag
       FAIL_REGEX "command line option .* is valid for .* but not for C\\\\+\\\\+" # GNU
       FAIL_REGEX "unrecognized .*option"                     # GNU
       FAIL_REGEX "unknown .*option"                          # Clang
       FAIL_REGEX "ignoring unknown option"                   # MSVC
       FAIL_REGEX "warning D9002"                             # MSVC, any lang
       FAIL_REGEX "option.*not supported"                     # Intel
       FAIL_REGEX "invalid argument .*option"                 # Intel
       FAIL_REGEX "ignoring option .*argument required"       # Intel
       FAIL_REGEX "[Uu]nknown option"                         # HP
       FAIL_REGEX "[Ww]arning: [Oo]ption"                     # SunPro
       FAIL_REGEX "command option .* is not recognized"       # XL
       FAIL_REGEX "not supported in this configuration; ignored"       # AIX
       FAIL_REGEX "File with unknown suffix passed to linker" # PGI
       FAIL_REGEX "WARNING: unknown flag:"                    # Open64
       )
     set (CMAKE_REQUIRED_DEFINITIONS "${SAFE_CMAKE_REQUIRED_DEFINITIONS}")
  endmacro ()
endif(CMAKE_VERSION VERSION_LESS 2.8.9)

if( CXX11_FLAG )

  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX11_FLAG}")

else()

  if(MINGW)
    LIST( APPEND CHECK_CXX11_FLAGS
      "-std=gnu++11"
      "-std=gnu++0x"
    )
  endif(MINGW)

  LIST( APPEND CHECK_CXX11_FLAGS
    "-std=c++11"
    "-hstd=c++11"
    "-std=c++0x"
  )

  set( __N 1 )
  foreach( _cxx11_flag ${CHECK_CXX11_FLAGS} )
    check_cxx_compiler_flag( ${_cxx11_flag} has_cxx11_flag_${__N} )
    if( has_cxx11_flag_${__N} )
      set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${_cxx11_flag}" )
      break()
    endif()
    math( EXPR __N '${__N}+1' )
  endforeach()

endif()

if(MSVC) 
  set(MSWINDOBE TRUE)
  add_definitions(/EHsc)
  # Wd4251 stops MSCrapWare from issuing meaningless warnings. Seems Microsoft engineers don't grok
  # dynamic libraries yet. Or templates. Or both acting alone or together. In any case, issuing
  # warning sure is easier on them than fixing  their OS.
  # Unfortunately, it does disable warnings that may be of interest. Possibly. 
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /D_VARIADIC_MAX=10 /wd4251")
endif(MSVC)

set(PROJECT_USES_CPP11 True CACHE INTERNAL "Uses c++11.")
