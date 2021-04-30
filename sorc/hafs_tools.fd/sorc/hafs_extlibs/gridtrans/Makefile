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

FSRC_FILES   = 
F90SRC_FILES = gridtrans_interface.F90 \
               gridtrans_fv3_interface.F90 

# Object files

OBJ_FILES = ${FSRC_FILES:.f=.o} \
            $(F90SRC_FILES:.F90=.o)

# Libraries

LIB = libgridtrans.a

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
	$(FC) $(FCFLAGS) $(DEBUG) -c $<
.F90.o:
	$(FC) $(FCFLAGS) $(DEBUG) -c $<
