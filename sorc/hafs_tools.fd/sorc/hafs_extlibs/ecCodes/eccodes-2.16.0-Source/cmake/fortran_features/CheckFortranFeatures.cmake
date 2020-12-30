###############################################################################
# checks
set(Fortran_FEATURE_CHECK_DIR ${CMAKE_CURRENT_LIST_DIR} CACHE INTERNAL "fortran file directory")

MACRO(fortran_check_single_feature FEATURE_NAME FEATURE_NUMBER RESULT_VAR)
  IF (NOT DEFINED ${RESULT_VAR})
    SET(_bindir "${CMAKE_BINARY_DIR}/fortran_feature_tests/fortran_${FEATURE_NAME}")

    IF (${FEATURE_NUMBER})
      SET(_SRCFILE_BASE ${Fortran_FEATURE_CHECK_DIR}/${FEATURE_NAME}-N${FEATURE_NUMBER})
      SET(_LOG_NAME "\"${FEATURE_NAME}\" (N${FEATURE_NUMBER})")
    ELSE (${FEATURE_NUMBER})
      SET(_SRCFILE_BASE ${Fortran_FEATURE_CHECK_DIR}/${FEATURE_NAME})
      SET(_LOG_NAME "\"${FEATURE_NAME}\"")
    ENDIF (${FEATURE_NUMBER})
    ecbuild_info("Checking Fortran support for ${_LOG_NAME}")

    SET(_SRCFILE "${_SRCFILE_BASE}.F90")
    SET(_SRCFILE_FAIL "${_SRCFILE_BASE}_fail.F90")
    SET(_SRCFILE_FAIL_COMPILE "${_SRCFILE_BASE}_fail_compile.F90")

    IF (CROSS_COMPILING)
      try_compile(${RESULT_VAR} "${_bindir}" "${_SRCFILE}")
      IF (${RESULT_VAR} AND EXISTS ${_SRCFILE_FAIL})
        try_compile(${RESULT_VAR} "${_bindir}_fail" "${_SRCFILE_FAIL}")
      ENDIF (${RESULT_VAR} AND EXISTS ${_SRCFILE_FAIL})
    ELSE (CROSS_COMPILING)
      ecbuild_try_run(_RUN_RESULT_VAR _COMPILE_RESULT_VAR
          "${_bindir}" "${_SRCFILE}")
      IF (_COMPILE_RESULT_VAR AND NOT _RUN_RESULT_VAR)
        SET(${RESULT_VAR} TRUE)
      ELSE (_COMPILE_RESULT_VAR AND NOT _RUN_RESULT_VAR)
        SET(${RESULT_VAR} FALSE)
      ENDIF (_COMPILE_RESULT_VAR AND NOT _RUN_RESULT_VAR)
      IF (${RESULT_VAR} AND EXISTS ${_SRCFILE_FAIL})
        ecbuild_try_run(_RUN_RESULT_VAR _COMPILE_RESULT_VAR
            "${_bindir}_fail" "${_SRCFILE_FAIL}")
        IF (_COMPILE_RESULT_VAR AND _RUN_RESULT_VAR)
          SET(${RESULT_VAR} TRUE)
        ELSE (_COMPILE_RESULT_VAR AND _RUN_RESULT_VAR)
          SET(${RESULT_VAR} FALSE)
        ENDIF (_COMPILE_RESULT_VAR AND _RUN_RESULT_VAR)
      ENDIF (${RESULT_VAR} AND EXISTS ${_SRCFILE_FAIL})
    ENDIF (CROSS_COMPILING)
    IF (${RESULT_VAR} AND EXISTS ${_SRCFILE_FAIL_COMPILE})
      try_compile(_TMP_RESULT "${_bindir}_fail_compile" "${_SRCFILE_FAIL_COMPILE}")
      IF (_TMP_RESULT)
        SET(${RESULT_VAR} FALSE)
      ELSE (_TMP_RESULT)
        SET(${RESULT_VAR} TRUE)
      ENDIF (_TMP_RESULT)
    ENDIF (${RESULT_VAR} AND EXISTS ${_SRCFILE_FAIL_COMPILE})

    IF (${RESULT_VAR})
      ecbuild_info("Checking Fortran support for ${_LOG_NAME} -- works")
    ELSE (${RESULT_VAR})
      ecbuild_info("Checking Fortran support for ${_LOG_NAME} -- not supported")
    ENDIF (${RESULT_VAR})
    SET(${RESULT_VAR} ${${RESULT_VAR}} CACHE INTERNAL "Fortran support for ${_LOG_NAME}")
  ENDIF (NOT DEFINED ${RESULT_VAR})
ENDMACRO(fortran_check_single_feature)

# Find list of all features
function(fortran_find_all_features outvar)
  FILE(GLOB ALL_Fortran_FEATURE_FILES "${Fortran_FEATURE_CHECK_DIR}/*.F90")
  set(OUTPUT_VARIABLES)
  foreach(filename ${ALL_Fortran_FEATURE_FILES})
    get_filename_component(filename ${filename} NAME_WE)
    string(REGEX REPLACE "_fail_compile" "" filename "${filename}")
    string(REGEX REPLACE "_fail" "" filename "${filename}")
    string(REGEX REPLACE "-N[0-9]*" "" filename "${filename}")
    set(OUTPUT_VARIABLES ${OUTPUT_VARIABLES} ${filename})
  endforeach()
  list(REMOVE_DUPLICATES OUTPUT_VARIABLES)
  set(${outvar} ${OUTPUT_VARIABLES} PARENT_SCOPE)
endfunction()

# Parses input and separates into arguments before REQUIRED and after REQUIRED.
# Arguments before REQUIRED are OPTIONALS.
# Arguments after REQUIRED are REQUIRED.
# If no arguments, then sets output OPTIONALS to ALLFEATURES.
function(parse_input_features ALLFEATURES OPTIONALS REQUIRED ERRORS)

  if("${ARGN}" STREQUAL "")
    set(${OPTIONALS} ${ALLFEATURES} PARENT_SCOPE)
    set(${REQUIRED} "" PARENT_SCOPE)
  else()
    set(REQUIRED_FEATURES)
    set(OPTIONAL_FEATURES)
    set(UNKNOWN_FEATURES)
    set(result_type OPTIONAL_FEATURES)
    foreach(feature ${ARGN})
      if(${feature} STREQUAL "REQUIRED")
        set(result_type REQUIRED_FEATURES)
      else()
        list(FIND ALLFEATURES ${feature} feature_was_found)

        if(feature_was_found EQUAL -1)
          list(APPEND UNKNOWN_FEATURES ${feature})
        else()
          list(APPEND ${result_type} ${feature})
        endif()

      endif(${feature} STREQUAL "REQUIRED")
    endforeach()

    set(${OPTIONALS} ${OPTIONAL_FEATURES} PARENT_SCOPE)
    set(${REQUIRED} ${REQUIRED_FEATURES} PARENT_SCOPE)
    set(${ERRORS} ${UNKNOWN_FEATURES} PARENT_SCOPE)
  endif("${ARGN}" STREQUAL "")
endfunction(parse_input_features)

# Figures out name and number of feature
# then calls macro that does the work
macro(_figure_out_fortran_feature current_feature)
  # Find set of files that match current_feature, excepting _fail and _fail_compile.
  file(GLOB ALL_FEATURE_FILES "${Fortran_FEATURE_CHECK_DIR}/${current_feature}*.F90")
  foreach(filename ${ALL_FEATURE_FILES})
    if(filename MATCHES "_fail")
      list(REMOVE_ITEM ALL_FEATURE_FILES ${filename})
    endif()
  endforeach()

  list(LENGTH ALL_FEATURE_FILES NFILES)
  if(NOT ${NFILES} EQUAL 1)
    ecbuild_critical("[Fortran] Expected to find only one feature. Found ${NFILES} -- ${ALL_FEATURE_FILES}.")
  endif(NOT ${NFILES} EQUAL 1)

  # Now we know which file corresponds to option.
  get_filename_component(basename ${ALL_FEATURE_FILES} NAME_WE)
  # If has feature number, extract it
  set(number "")
  if(basename MATCHES "-N[0-9]*$")
    string(REGEX REPLACE "${current_feature}-N" "" number "${basename}")
  endif()
  # Then call macro
  string(TOUPPER ${current_feature} UPPER_OPTIONAL)
  set(VARNAME HAS_Fortran_${UPPER_OPTIONAL})
  fortran_check_single_feature(${current_feature} "${number}" ${VARNAME})
endmacro(_figure_out_fortran_feature)

function(fortran_feature_check)

  # find all features
  fortran_find_all_features(ALL_Fortran_FEATURES)

  # Parses input to this function.
  parse_input_features("${ALL_Fortran_FEATURES}" OPTIONALS REQUIRED ERRORS ${ARGN})
  if(NOT ${ERRORS} STREQUAL "")
    ecbuild_info("[Fortran] The following features are unknown: ${ERRORS}.")
  endif()

  # Check optional features
  foreach(current_feature ${OPTIONALS})
    _figure_out_fortran_feature(${current_feature})
  endforeach(current_feature ${ARGN})

  # Check required features
  foreach(current_feature ${REQUIRED})
    _figure_out_fortran_feature(${current_feature})
    string(TOUPPER ${current_feature} UPPER_REQUIRED)
    set(VARNAME HAS_Fortran_${UPPER_REQUIRED})
    if(NOT ${VARNAME})
      ecbuild_critical("[Fortran] Required feature ${current_feature} is not available.")
    endif(NOT ${VARNAME})
  endforeach(current_feature ${REQUIRED})

endfunction(fortran_feature_check)
