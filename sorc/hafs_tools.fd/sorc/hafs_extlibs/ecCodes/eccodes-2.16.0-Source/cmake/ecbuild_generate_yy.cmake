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
# ecbuild_generate_yy
# ===================
#
# Process lex/yacc files. ::
#
#   ecbuild_generate_yy( YYPREFIX <prefix>
#                        YACC <file>
#                        LEX <file>
#                        DEPENDANT <file1> [ <file2> ... ]
#                        [ SOURCE_DIR <dir> ]
#                        [ OUTPUT_DIRECTORY <dir> ]
#                        [ YACC_TARGET <file> ]
#                        [ LEX_TARGET <file> ]
#                        [ YACC_FLAGS <flags> ]
#                        [ LEX_FLAGS <flags> ]
#                        [ BISON_FLAGS <flags> ]
#                        [ FLEX_FLAGS <flags> ] )
#
# Options
# -------
#
# YYPREFIX : required
#   prefix to use for file and function names
#
# YACC : required
#   base name of the yacc source file (without .y extension)
#
# LEX : required
#   base name of the lex source file (without .l extension)
#
# DEPENDANT : required
#  list of files which depend on the generated lex and yacc target files
#  At least one should be an existing source file (not generated itself).
#
# SOURCE_DIR : optional, defaults to CMAKE_CURRENT_SOURCE_DIR
#   directory where yacc and lex source files are located
#
# OUTPUT_DIRECTORY : optional, defaults to CMAKE_CURRENT_BINARY_DIR
#   output directory for yacc and lex target files
#
# YACC_TARGET : optional, defaults to YACC
#   base name of the generated yacc target file (without .c extension)
#
# LEX_TARGET : optional, defaults to LEX
#   base name of the generated lex target file (without .c extension)
#
# YACC_FLAGS : optional, defaults to -t
#   flags to pass to yacc executable
#
# LEX_FLAGS : optional
#   flags to pass to lex executable
#
# BISON_FLAGS : optional, defaults to -t
#   flags to pass to bison executable
#
# FLEX_FLAGS : optional, defaults to -l
#   flags to pass to flex executable
#
##############################################################################

macro( ecbuild_generate_yy )

  ecbuild_find_lexyacc() # find [ yacc|byson ] and [ lex|flex ]

  ecbuild_find_perl( REQUIRED )

  set( options )
  set( single_value_args YYPREFIX YACC LEX SOURCE_DIR OUTPUT_DIRECTORY YACC_TARGET LEX_TARGET LEX_FLAGS YACC_FLAGS FLEX_FLAGS BISON_FLAGS )
  set( multi_value_args  DEPENDANT )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_generate_yy(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if( NOT _PAR_YYPREFIX  )
    ecbuild_critical("The call to ecbuild_generate_yy() doesn't specify the YYPREFIX.")
  endif()

  if( NOT _PAR_YACC  )
    ecbuild_critical("The call to ecbuild_generate_yy() doesn't specify the YACC file.")
  endif()

  if( NOT _PAR_LEX  )
    ecbuild_critical("The call to ecbuild_generate_yy() doesn't specify the LEX file.")
  endif()

  if( NOT _PAR_DEPENDANT )
    ecbuild_critical("The call to ecbuild_generate_yy() doesn't specify the DEPENDANT files.")
  endif()

  set( BASE ${_PAR_YYPREFIX}_${_PAR_YACC} )

  ## default flags

  if( NOT _PAR_LEX_FLAGS )
    set( _PAR_LEX_FLAGS "" )
  endif()

  if( NOT _PAR_FLEX_FLAGS )
    set( _PAR_FLEX_FLAGS "-l" )
  endif()

  if( NOT _PAR_YACC_FLAGS )
    set( _PAR_YACC_FLAGS "-t" )
  endif()

  if( NOT _PAR_BISON_FLAGS )
    set( _PAR_BISON_FLAGS "-t" )
  endif()

  if( NOT _PAR_YACC_TARGET )
    set ( _PAR_YACC_TARGET ${_PAR_YACC} )
  endif()

  if ( NOT _PAR_LEX_TARGET )
    set ( _PAR_LEX_TARGET ${_PAR_LEX} )
  endif()

  if( NOT _PAR_SOURCE_DIR )
    set( _PAR_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} )
  endif()

  if( NOT _PAR_OUTPUT_DIRECTORY )
    set( _PAR_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )
  else()
    file( MAKE_DIRECTORY ${_PAR_OUTPUT_DIRECTORY} )
  endif()

  set( ${BASE}yy_tmp_target ${_PAR_OUTPUT_DIRECTORY}/${_PAR_YACC_TARGET}.tmp.c )
  set( ${BASE}yh_tmp_target ${_PAR_OUTPUT_DIRECTORY}/${_PAR_YACC_TARGET}.tmp.h )
  set( ${BASE}yl_tmp_target ${_PAR_OUTPUT_DIRECTORY}/${_PAR_LEX_TARGET}.tmp.c )

  set( ${BASE}yy_target ${_PAR_OUTPUT_DIRECTORY}/${_PAR_YACC_TARGET}.c )
  set( ${BASE}yh_target ${_PAR_OUTPUT_DIRECTORY}/${_PAR_YACC_TARGET}.h )
  set( ${BASE}yl_target ${_PAR_OUTPUT_DIRECTORY}/${_PAR_LEX_TARGET}.c )

  if( BISON_FOUND )
    bison_target( ${BASE}_parser ${_PAR_SOURCE_DIR}/${_PAR_YACC}.y ${${BASE}yy_tmp_target} COMPILE_FLAGS "${_PAR_BISON_FLAGS}" )
  else()
    yacc_target( ${BASE}_parser ${_PAR_SOURCE_DIR}/${_PAR_YACC}.y ${${BASE}yy_tmp_target} COMPILE_FLAGS "${_PAR_YACC_FLAGS}" )
  endif()

  if( FLEX_FOUND )
    flex_target( ${BASE}_scanner ${_PAR_SOURCE_DIR}/${_PAR_LEX}.l ${${BASE}yl_tmp_target} COMPILE_FLAGS "${_PAR_FLEX_FLAGS}" )
    add_flex_bison_dependency(${BASE}_scanner ${BASE}_parser)
  else()
    lex_target( ${BASE}_scanner ${_PAR_SOURCE_DIR}/${_PAR_LEX}.l ${${BASE}yl_tmp_target} COMPILE_FLAGS "${_PAR_LEX_FLAGS}" )
    add_lex_yacc_dependency(${BASE}_scanner ${BASE}_parser)
  endif()

  set_source_files_properties(${${BASE}yy_tmp_target} GENERATED)
  set_source_files_properties(${${BASE}yh_tmp_target} GENERATED)
  set_source_files_properties(${${BASE}yl_tmp_target} GENERATED)

  add_custom_command(OUTPUT  ${${BASE}yy_target}
    COMMAND ${CMAKE_COMMAND} -E copy ${${BASE}yy_tmp_target} ${${BASE}yy_target}
    COMMAND ${PERL_EXECUTABLE} -pi -e 's/yy/${_PAR_YYPREFIX}/g' ${${BASE}yy_target}
    COMMAND ${PERL_EXECUTABLE} -pi -e 's/\\.tmp\\.c/\\.c/g' ${${BASE}yy_target}
    DEPENDS ${${BASE}yy_tmp_target}
    )

  add_custom_command(OUTPUT  ${${BASE}yh_target}
    COMMAND ${CMAKE_COMMAND} -E copy ${${BASE}yh_tmp_target} ${${BASE}yh_target}
    COMMAND ${PERL_EXECUTABLE} -pi -e 's/yy/${_PAR_YYPREFIX}/g' ${${BASE}yh_target}
    COMMAND ${PERL_EXECUTABLE} -pi -e 's/\\.tmp\\.h/\\.h/g' ${${BASE}yh_target}
    DEPENDS ${${BASE}yh_tmp_target}
    )

  add_custom_command(OUTPUT  ${${BASE}yl_target}
    COMMAND ${CMAKE_COMMAND} -E copy ${${BASE}yl_tmp_target} ${${BASE}yl_target}
    COMMAND ${PERL_EXECUTABLE} -pi -e 's/yy/${_PAR_YYPREFIX}/g' ${${BASE}yl_target}
    COMMAND ${PERL_EXECUTABLE} -pi -e 's/\\.tmp\\.c/\\.c/g' ${${BASE}yl_target}
    DEPENDS ${${BASE}yl_tmp_target}
    )

  set_source_files_properties(${${BASE}yy_target} GENERATED)
  set_source_files_properties(${${BASE}yh_target} GENERATED)
  set_source_files_properties(${${BASE}yl_target} GENERATED)

  foreach( file ${_PAR_DEPENDANT} )
    if( NOT IS_ABSOLUTE ${file})
      set( file ${_PAR_SOURCE_DIR}/${file} )
    endif()
    set_source_files_properties( ${file} PROPERTIES
        OBJECT_DEPENDS "${${BASE}yy_target};${${BASE}yh_target};${${BASE}yl_target}" )
  endforeach()

endmacro( ecbuild_generate_yy )
