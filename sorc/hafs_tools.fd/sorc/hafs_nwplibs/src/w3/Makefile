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

CSRC_FILES = mova2i.c

FSRC_FILES = aea.f errexit.f errmsg.f fparsei.f \
	     fparser.f gbyte.f gbytec.f gbytes.f \
	     gbytesc.f getbit.f getgb.f getgb1.f \
	     getgb1r.f getgb1re.f getgb1s.f getgbe.f \
	     getgbeh.f getgbem.f getgbemh.f getgbemn.f \
	     getgbemp.f getgbep.f getgbex.f getgbexm.f \
	     getgbh.f getgbm.f getgbmh.f getgbmp.f \
	     getgbp.f getgi.f getgir.f gtbits.f \
	     idsdef.f instrument.f iw3jdn.f iw3pds.f \
	     iw3unp29.f ixgb.f lengds.f makwmo.f \
	     mkfldsep.f pdsens.f pdseup.f putgb.f \
	     putgbe.f putgben.f putgbex.f putgbn.f \
	     q9ie32.f r63w72.f sbyte.f sbytec.f \
	     sbytes.f sbytesc.f skgb.f w3ai00.f \
	     w3ai01.f w3ai08.f w3ai15.f w3ai18.f \
	     w3ai19.f w3ai24.f w3ai38.f w3ai39.f \
	     w3aq15.f w3as00.f w3difdat.f w3doxdat.f \
	     w3fi01.f w3fi04.f w3fi58.f w3fi59.f \
	     w3fi62.f w3fi63.f w3fi64.f w3fi68.f \
	     w3fi69.f w3fi71.f w3fi72.f w3fi73.f \
	     w3fi74.f w3fi75.f w3fi76.f w3fi82.f \
	     w3fi83.f w3fi88.f w3fi92.f w3fp11.f \
	     w3fp12.f w3fp13.f w3fs13.f w3fs15.f \
	     w3fs21.f w3fs26.f w3ft32.f w3locdat.f \
	     w3movdat.f w3nogds.f w3pradat.f w3reddat.f \
	     w3tagb.f w3trnarg.f w3unpk77.f w3utcdat.f \
	     w3valdat.f w3ymdh4.f xmovex.f xstore.f

# Object files

OBJ_FILES  = ${FSRC_FILES:.f=.o}  \
	     $(CSRC_FILES:.c=.o)

# Libraries

LIB = libw3.a

# Make calls

$(LIB): $(OBJ_FILES)
	$(AR) $(ARFLAGS) $@ $(OBJ_FILES)
	$(RANLIB) $@

install:
	$(MV) $(LIB) ../../lib

clean: 
	$(RM) *.o >& /dev/null

# Compiling rules

.SUFFIXES:
.SUFFIXES: .f .c .o
.f.o:
	$(FC) $(FCFLAGS) -c $<
.c.o:
	$(CC) $(CFLAGS) -c -DLINUX $<
