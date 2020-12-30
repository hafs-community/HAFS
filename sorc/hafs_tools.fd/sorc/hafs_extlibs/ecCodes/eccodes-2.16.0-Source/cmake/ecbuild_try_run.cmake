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
# ecbuild_try_run
# ===============
#
# Try compiling and then running some code. ::
#
#   ecbuild_try_run( <run_result_var> <compile_result_var>
#                    <bindir> <srcfile>
#                    [ CMAKE_FLAGS <flag> [ <flag> ... ] ]
#                    [ COMPILE_DEFINITIONS <def> [ <def> ... ] ]
#                    [ LINK_LIBRARIES <lib> [ <lib> ... ] ]
#                    [ COMPILE_OUTPUT_VARIABLE <var> ]
#                    [ RUN_OUTPUT_VARIABLE <var> ]
#                    [ OUTPUT_VARIABLE <var> ]
#                    [ ARGS <arg> [ <arg> ... ] ] )
#
# Try compiling a ``<srcfile>``.  Returns ``TRUE`` or ``FALSE`` for success
# or failure in ``<compile_result_var>``.  If the compile succeeded, runs the
# executable and returns its exit code in ``<run_result_var>``.  If the
# executable was built, but failed to run, then ``<run_result_var>`` will be
# set to ``FAILED_TO_RUN``.  See the CMake ``try_compile`` command for
# information on how the test project is constructed to build the source file.
#
# Options
# -------
#
# CMAKE_FLAGS : optional
#   Specify flags of the form ``-DVAR:TYPE=VALUE`` to be passed to
#   the ``cmake`` command-line used to drive the test build.
#
#   The example in CMake's ``try_compile`` shows how values for variables
#   ``INCLUDE_DIRECTORIES``, ``LINK_DIRECTORIES``, and ``LINK_LIBRARIES``
#   are used.
#
# COMPILE_DEFINITIONS : optional
#   Specify ``-Ddefinition`` arguments to pass to ``add_definitions``
#   in the generated test project.
#
# COMPILE_OUTPUT_VARIABLE : optional
#   Report the compile step build output in a given variable.
#
# LINK_LIBRARIES : optional
#   Specify libraries to be linked in the generated project.
#   The list of libraries may refer to system libraries and to
#   Imported Targets from the calling project.
#
#   If this option is specified, any ``-DLINK_LIBRARIES=...`` value
#   given to the ``CMAKE_FLAGS`` option will be ignored.
#
# OUTPUT_VARIABLE : optional
#   Report the compile build output and the output from running the executable
#   in the given variable.  This option exists for legacy reasons.  Prefer
#   ``COMPILE_OUTPUT_VARIABLE`` and ``RUN_OUTPUT_VARIABLE`` instead.
#
# RUN_OUTPUT_VARIABLE : optional
#   Report the output from running the executable in a given variable.
#
# Other Behavior Settings
# -----------------------
#
# Set the ``CMAKE_TRY_COMPILE_CONFIGURATION`` variable to choose
# a build configuration.
#
# Behavior when Cross Compiling
# -----------------------------
#
# When cross compiling, the executable compiled in the first step
# usually cannot be run on the build host.  The ``try_run`` command checks
# the ``CMAKE_CROSSCOMPILING`` variable to detect whether CMake is in
# cross-compiling mode.  If that is the case, it will still try to compile
# the executable, but it will not try to run the executable unless the
# ``CMAKE_CROSSCOMPILING_EMULATOR`` variable is set.  Instead it will create
# cache variables which must be filled by the user or by presetting them in
# some CMake script file to the values the executable would have produced if
# it had been run on its actual target platform. These cache entries are:
#
# ``<RUN_RESULT_VAR>``
#   Exit code if the executable were to be run on the target platform.
#
# ``<RUN_RESULT_VAR>__TRYRUN_OUTPUT``
#   Output from stdout and stderr if the executable were to be run on
#   the target platform.  This is created only if the
#   ``RUN_OUTPUT_VARIABLE`` or ``OUTPUT_VARIABLE`` option was used.
#
# In order to make cross compiling your project easier, use ``try_run``
# only if really required.  If you use ``try_run``, use the
# ``RUN_OUTPUT_VARIABLE`` or ``OUTPUT_VARIABLE`` options only if really
# required.  Using them will require that when cross-compiling, the cache
# variables will have to be set manually to the output of the executable.
# You can also "guard" the calls to ``try_run`` with an ``if`` block checking
# the ``CMAKE_CROSSCOMPILING`` variable and provide an easy-to-preset
# alternative for this case.
#
##############################################################################

# This is an API compatible version of try_run which ignores output on stderr

if( CMAKE_VERSION VERSION_LESS 2.8.12 )

ecbuild_deprecate( "ecbuild_try_run falls back to try_run on CMake < 2.8.12 (ECBUILD-341)" )

macro( ecbuild_try_run )
  try_run( ${ARGV} )
endmacro()

else()

function( ecbuild_try_run RUN_RESULT_VAR COMPILE_RESULT_VAR BINDIR SRCFILE )

  set( options )
  set( single_value_args COMPILE_OUTPUT_VARIABLE RUN_OUTPUT_VARIABLE OUTPUT_VARIABLE )
  set( multi_value_args  CMAKE_FLAGS COMPILE_DEFINITIONS LINK_LIBRARIES ARGS )

  cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if( _p_UNPARSED_ARGUMENTS )
    ecbuild_critical("Unknown keywords given to ecbuild_try_run(): \"${_p_UNPARSED_ARGUMENTS}\"")
  endif()

  if( CMAKE_EXE_LINKER_FLAGS )
    set( _p_LINK_LIBRARIES "${_p_LINK_LIBRARIES} ${CMAKE_EXE_LINKER_FLAGS}" )
  endif()

  # Build argument list for try_compile
  set( _opts "" )
  foreach( _opt CMAKE_FLAGS COMPILE_DEFINITIONS LINK_LIBRARIES  )
    if( _p_${_opt} )
      list( APPEND _opts ${_opt} "${_p_${_opt}}" )
    endif()
  endforeach()

  ecbuild_debug( "ecbuild_try_run: Compiling ${SRCFILE} in ${BINDIR}" )
  try_compile( _compile_res ${BINDIR} ${SRCFILE}
               OUTPUT_VARIABLE _compile_out
               COPY_FILE ${CMAKE_CURRENT_BINARY_DIR}/${SRCFILE}.bin COPY_FILE_ERROR _compile_err
               ${_opts} )

  if( _compile_out )
    ecbuild_debug( "ecbuild_try_run: compilation output"
      "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
      "\n${_compile_out}"
      "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" )
  endif()
  if( _compile_err )
    ecbuild_debug( "ecbuild_try_run: compilation errors"
      "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
      "\n${_compile_err}"
      "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" )
  endif()

  # FIXME: how do we handle cross compilation mode? (CMAKE_CROSSCOMPILING)

  if( _compile_res )

    ecbuild_debug( "ecbuild_try_run: Running ${CMAKE_CURRENT_BINARY_DIR}/${SRCFILE}.bin in ${BINDIR}" )
    execute_process( COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${SRCFILE}.bin WORKING_DIRECTORY ${BINDIR}
                     RESULT_VARIABLE _run_res
                     OUTPUT_VARIABLE _run_out ERROR_VARIABLE _run_err )

    if( _p_RUN_OUTPUT_VARIABLE )
      set( ${_p_RUN_OUTPUT_VARIABLE} ${_run_out} )
    endif()
    if( _run_out )
      ecbuild_debug( "ecbuild_try_run: run output"
        "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        "\n${_run_out}"
        "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" )
    endif()
    if( _run_err )
      ecbuild_debug( "ecbuild_try_run: run errors"
        "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        "\n${_run_err}"
        "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" )
    endif()

  else()
    set( ${RUN_RESULT_VAR} "FAILED_TO_RUN" PARENT_SCOPE )
    ecbuild_debug( "ecbuild_try_run: Compilation of ${SRCFILE} in ${BINDIR} failed!" )
  endif()

  set( ${COMPILE_RESULT_VAR} ${_compile_res} PARENT_SCOPE )
  set( ${RUN_RESULT_VAR} ${_run_res} PARENT_SCOPE )
  if( _p_COMPILE_OUTPUT_VARIABLE )
    set( ${_p_COMPILE_OUTPUT_VARIABLE} ${_compile_out} PARENT_SCOPE )
  endif()
  if( _p_RUN_OUTPUT_VARIABLE )
    set( ${_p_RUN_OUTPUT_VARIABLE} ${_run_out} PARENT_SCOPE )
  endif()
  if( _p_OUTPUT_VARIABLE )
    set( ${_p_OUTPUT_VARIABLE} "${_compile_out}\n${_run_out}" PARENT_SCOPE )
  endif()

endfunction()

endif()
