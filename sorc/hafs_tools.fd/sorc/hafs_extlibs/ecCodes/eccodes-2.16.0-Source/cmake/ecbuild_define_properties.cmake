# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
# define custom properties

############################################################################################
# source properties:

# Custom property to determine if compiler flags have been applied yet:
define_property( SOURCE
  PROPERTY CUSTOM_FLAGS
  BRIEF_DOCS "Custom compiler flags have been applied to source file"
  FULL_DOCS "Compiler flags have been applied to the source file, using custom CMake rules. Assists processing of sources that are used by multiple targets. Treated as Boolean." )
