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

FSRC_FILES   = advec.f alf.f divec.f dives.f divgc.f divgs.f \
	       gaqd.f geo2math.f gradec.f grades.f gradgc.f gradgs.f \
	       helmsph.f hrfft.f idivec.f idives.f idivgc.f idivgs.f \
	       idvtec.f idvtes.f idvtgc.f idvtgs.f igradec.f igrades.f \
	       igradgc.f igradgs.f ihgeod.f isfvpec.f isfvpes.f isfvpgc.f \
	       isfvpgs.f islapec.f islapes.f islapgc.f islapgs.f ivlapec.f \
	       ivlapes.f ivlapgc.f ivlapgs.f ivrtec.f ivrtes.f ivrtgc.f \
               ivrtgs.f lfim.f lfin.f lfp.f lfpt.f sfvpec.f \
	       sfvpes.f sfvpgc.f sfvpgs.f shaec.f shaes.f shagc.f \
               shags.f shallow.f shigc.f shigs.f shpe.f shpg.f \
	       shsec.f shses.f shsgc.f shsgs.f slapec.f slapes.f \
	       slapgc.f slapgs.f sphcom.f sshifte.f trssph.f trvsph.f \
	       vhaec.f vhaes.f vhagc.f vhags.f vhsec.f vhses.f \
               vhsgc.f vhsgs.f visequ.f visgau.f visgeo.f vlapec.f \
               vlapes.f vlapgc.f vlapgs.f vrtec.f vrtes.f vrtgc.f \
               vrtgs.f vshifte.f vsurf.f vtsec.f vtses.f vtsgc.f \
	       vtsgs.f
F90SRC_FILES = sphere.f90 

# Object files

OBJ_FILES = $(FSRC_FILES:.f=.o) \
            $(F90SRC_FILES:.f90=.o)

# Libraries

LIB = libspherepack.a

# Make calls

$(LIB): $(OBJ_FILES)
	$(AR) $(ARFLAGS) $@ $(OBJ_FILES)
	$(RANLIB) $@

install:
	$(MV) *.mod ../../include
	$(MV) $(LIB) ../../lib

clean: 
	$(RM) *.mod *.o >& /dev/null

# Compiling rules

.SUFFIXES:
.SUFFIXES: .f .f90 .o
.f.o:
	$(FC) $(FCFLAGS) -c $<
.f90.o:
	$(FC) $(FCFLAGS) -c $<
