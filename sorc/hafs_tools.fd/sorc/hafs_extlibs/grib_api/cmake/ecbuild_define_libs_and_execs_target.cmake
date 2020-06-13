# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

set( EC_ALL_EXES "" CACHE INTERNAL "" )
set( EC_ALL_LIBS "" CACHE INTERNAL "" )

############################################################################################
# define libs and execs targets

macro( ecbuild_define_libs_and_execs_targets )

  add_custom_target( libs )

  if( EC_ALL_LIBS )
    add_dependencies( libs ${EC_ALL_LIBS} )
  endif()

  add_custom_target( execs )

  if( EC_ALL_EXECS )
    add_dependencies( execs ${EC_ALL_EXES} )
  endif()

endmacro(ecbuild_define_libs_and_execs_targets)
