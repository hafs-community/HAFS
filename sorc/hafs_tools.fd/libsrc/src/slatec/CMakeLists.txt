#=======================================================================

#$$$ CMAKEFILE DOCUMENTATION BLOCK

# Copyright (C) 2018 Henry R. Winterbottom

# Email: henry.winterbottom@noaa.gov

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

# Review the README, within the top-level directory, which provides
# relevant instructions and (any) references cited by algorithms
# within this software suite.

#=======================================================================

# CMake pre-requisites

include_directories(${NWP_LIBS_SOURCE_DIR}/src/slatec)
set (LLIBS "m")
set (TARGET "slatec")

# CMake compiler flags

set (CMAKE_Fortran_FLAGS "-O3 -g -convert big_endian")

# CMake source codes declarations

file (GLOB ftn_objs *.f)
set (FTN_OBJS ${ftn_objs})

# CMake build and install instructions

add_library (${TARGET} ${FTN_OBJS})
install (TARGETS ${TARGET} DESTINATION ${INSTALL_PATH})

