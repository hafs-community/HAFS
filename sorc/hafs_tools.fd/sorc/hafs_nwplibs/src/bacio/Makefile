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

include ../../configure.nwp-libs

# Source files

CSRC_FILES = bacio.c \
	     byteswap.c

FSRC_FILES = baciof.f \
	     bafrio.f \
	     chk_endianc.f 

# Object files

OBJ_FILES = ${CSRC_FILES:.c=.o} \
	    ${FSRC_FILES:.f=.o}

# Libraries

LIB = libbacio.a

# Make calls

$(LIB): $(OBJ_FILES)
	$(AR) $(ARFLAGS) $@ $(OBJ_FILES)
	$(RANLIB) $@

install:
	$(MV) *.mod ../../include
	$(MV) $(LIB) ../../lib

clean: 
	$(RM) *.mod *.o >& /dev/null

# Dependencies

baciof.o : baciof.f baciof.h 
bafrio.o : bafrio.f 
chk_endianc.o : chk_endianc.f 
bacio.o : bacio.c clib.h 
byteswap.o : byteswap.c 
baciof.o : baciof.h 

# Compiling rules

.SUFFIXES:
.SUFFIXES: .f .c .o
.f.o:
	$(FC) $(FCFLAGS) -c -mcmodel=medium -FR $<

.c.o:
	$(CC) $(CFLAGS) -c -DUNDERSCORE $<
