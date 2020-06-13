# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
# macro for adding a link to library on a development system

set( EC_ALL_EXES "" CACHE INTERNAL "" )
set( EC_ALL_LIBS "" CACHE INTERNAL "" )

macro( ecbuild_link_exe TARGET  FILENAME FILEPATH )

    if( DEFINED EC_LINK_DIR )
       add_custom_target(${TARGET}_link
          COMMAND ${CMAKE_COMMAND} -E make_directory ${EC_LINK_DIR}
          COMMAND ${CMAKE_COMMAND} -E make_directory ${EC_LINK_DIR}/bin
          COMMAND ${CMAKE_COMMAND} -E remove ${EC_LINK_DIR}/bin/${FILENAME}
          COMMAND ${CMAKE_COMMAND} -E create_symlink ${FILEPATH} ${EC_LINK_DIR}/bin/${FILENAME}
          DEPENDS ${TARGET}
          COMMENT "link ${EC_LINK_DIR}/bin/${FILENAME}" )
    endif()

    set( EC_ALL_EXES ${EC_ALL_EXES} ${TARGET} CACHE INTERNAL "" )

endmacro( ecbuild_link_exe  )

###############################################################################
# macro for adding a link to library on a development system

macro( ecbuild_link_lib  TARGET FILENAME FILEPATH )

    if( DEFINED EC_LINK_DIR )
       add_custom_target(${TARGET}_link
          COMMAND ${CMAKE_COMMAND} -E make_directory ${EC_LINK_DIR}
          COMMAND ${CMAKE_COMMAND} -E make_directory ${EC_LINK_DIR}/lib
          COMMAND ${CMAKE_COMMAND} -E remove ${EC_LINK_DIR}/lib/${FILENAME}
          COMMAND ${CMAKE_COMMAND} -E create_symlink ${FILEPATH} ${EC_LINK_DIR}/lib/${FILENAME}
          DEPENDS ${TARGET}
          COMMENT "link ${EC_LINK_DIR}/lib/${FILENAME}" )
    endif()

    set( EC_ALL_LIBS ${EC_ALL_LIBS} ${TARGET} CACHE INTERNAL "" )

endmacro( ecbuild_link_lib  )

############################################################################################
# define make links target

macro( ecbuild_define_links_target )

  if( DEFINED EC_LINK_DIR )

    foreach( lib ${EC_ALL_LIBS} )
      list( APPEND ec_link_libs ${lib}_link )
    endforeach()
    foreach( exe ${EC_ALL_EXES} )
      list( APPEND ec_link_exes ${exe}_link )
    endforeach()

    add_custom_target( links DEPENDS ${ec_link_libs} ${ec_link_exes} )

     #   ecbuild_debug_var( EC_ALL_EXES )
     #   ecbuild_debug_var( ec_link_exes )

     #  ecbuild_debug_var( EC_ALL_LIBS )
     #   ecbuild_debug_var( ec_link_libs )

  endif()

endmacro(ecbuild_define_links_target)
