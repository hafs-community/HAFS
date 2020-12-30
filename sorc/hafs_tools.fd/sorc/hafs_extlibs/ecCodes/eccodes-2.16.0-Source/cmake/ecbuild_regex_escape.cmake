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
# ecbuild_regex_escape
# ====================
#
# Escape regular expression special characters from the input string. ::
#
#   ecbuild_regex_escape(<string> <output_variable>)
#
##############################################################################
function(ecbuild_regex_escape input outvar)

    string(REGEX REPLACE "[][.*+?|()\\^$]" "\\\\\\0" output "${input}")
    set(${outvar} "${output}" PARENT_SCOPE)

endfunction(ecbuild_regex_escape)
