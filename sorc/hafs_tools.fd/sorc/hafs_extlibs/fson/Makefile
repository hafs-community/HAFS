#=======================================================================

#$$$ MAKEFILE DOCUMENTATION BLOCK

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

# General rules

include ./../configure.ext-libs

# Source files

FSRC_FILES = fson_string_m.f90 \
	     fson_value_m.f90 \
	     fson_path_m.f90 \
	     fson.f90

# Object files

OBJ_FILES = ${FSRC_FILES:.f90=.o}

# Libraries

LIB = libfson.a

# Make calls

$(LIB): $(OBJ_FILES)
	$(AR) $(ARFLAGS) $@ $(OBJ_FILES)
	$(RANLIB) $@

install:
	$(MV) *.mod ./../include
	$(MV) $(LIB) ./../lib

clean: 
	$(RM) *.mod *.o >& /dev/null

# Compiling rules

.SUFFIXES:
.SUFFIXES: .f90 .o
.f90.o:
	$(FC) $(FCFLAGS) -g -convert big_endian -mcmodel=large -c $<
