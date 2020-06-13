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

SHELL=/bin/sh

# Machine dependent compile options

MAKE_CONF = Makefile.conf
MAKE_DEPEND = Makefile.dependency
include $(MAKE_CONF)
include $(MAKE_DEPEND)
include configure.tc-diagnostics

# Make calls

"" :
	@$(MAKE) -f Makefile all

clean:
	cd ./tc_mpi; make clean; cd ../
	- $(RM) $(EXEC) *.o *.mod

# Source files

SRCS = $(SRCSF77) $(SRCSF90)

# Object files

OBJS    = $(SRCSF90:.F90=.o) $(SRCSF77:.f=.o)
EXTOBJS = ./tc_mpi/pcmin_2013.o

# Compiling rules

.SUFFIXES :
.SUFFIXES : .F90 .f .c .o

.F90.o  :
	$(MPIFC) $(FCFFLAGS) $(LIBS) $(INCS) -c $<

.f.o  :
	$(F77) $(FCFFLAGS) $(LIBS) $(INCS) -c $<


# Compiler

all: PCMIN $(EXEC)

PCMIN:
	cd ./tc_mpi; make all; cd ../

$(EXEC): $(OBJS)
	 $(LD) $(LDFLAGS) $(OBJS) $(EXTOBJS) main.F90 $(LIBS) $(INCS) -lm -o ./$(EXEC) 
