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

include configure.ext-libs 

# Libraries and modules

SUBDIRS = fson gridprojs gridtrans kdtree shtns_interface slatec slint sondelib spline wmm

# Make calls

all: 
	$(MKDIR) -p ./lib ./include
	@for dir in $(SUBDIRS); do \
		( cd ./$$dir; echo "Making $@ in `pwd`" ; make >& ./../logs/make.$$dir; make install >& ./../logs/install.$$dir; cd ./) ; \
	done

clean: 
	$(RM) -rf ./lib/*.a ./include/*.mod
	@for dir in $(SUBDIRS); do \
		( cd ./$$dir; rm -rf lib*.a install.$$dir make.$$dir; make clean >& clean.$$dir ; cd ./) ; \
	done
