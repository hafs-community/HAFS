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

cmake_minimum_required (VERSION 2.6)
project (NWP_LIBS)
enable_language (Fortran C)

# CMake build module and library directories

set (CMAKE_Fortran_MODULE_DIRECTORY ${NWP_LIBS_SOURCE_DIR}/../include)
set (INSTALL_PATH ${NWP_LIBS_SOURCE_DIR}/../lib)

# CMake library build instructions

add_subdirectory (src/diagsread)
add_subdirectory (src/fson)
add_subdirectory (src/gridprojs)
add_subdirectory (src/kdtree)
add_subdirectory (src/slatec)
add_subdirectory (src/slint)
add_subdirectory (src/sondelib)
add_subdirectory (src/spherepack)
add_subdirectory (src/spline)

# CMake directory permissions and information

set (DIRECTORY_PERMISSIONS "OWNER_WRITE" "OWNER_READ" "OWNER_EXECUTE" "GROUP_READ" "GROUP_EXECUTE" "WORLD_READ" "WORLD_EXECUTE")
install (DIRECTORY DESTINATION ${INSTALL_PATH} DIRECTORY_PERMISSIONS ${DIRECTORY_PERMISSIONS})
