# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
# check endiness

function(ecbuild_test_endiness)

    test_big_endian( _BIG_ENDIAN )

    if( _BIG_ENDIAN )
        set( EC_BIG_ENDIAN    1 )
        set( EC_LITTLE_ENDIAN 0 )
    else()
        set( EC_BIG_ENDIAN    0 )
        set( EC_LITTLE_ENDIAN 1 )
    endif()

  set( EC_BIG_ENDIAN    ${EC_BIG_ENDIAN}    PARENT_SCOPE )
  set( EC_LITTLE_ENDIAN ${EC_LITTLE_ENDIAN} PARENT_SCOPE )

endfunction(ecbuild_test_endiness)

