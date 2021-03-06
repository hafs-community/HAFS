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

FSRC_FILES   = geomag.f
F90SRC_FILES = wmm_interface.F90

# Object files

OBJ_FILES = $(FSRC_FILES:.f=.o) \
            $(F90SRC_FILES:.F90=.o)

# Libraries

LIB = libwmm.a

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
.SUFFIXES: .f .F90 .o
.f.o:
	$(FC) $(FCFLAGS) -c $<
.F90.o:
	$(FC) $(FCFLAGS) -c $<
