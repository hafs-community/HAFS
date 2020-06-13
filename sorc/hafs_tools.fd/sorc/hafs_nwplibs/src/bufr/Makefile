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

CSRC_FILES = bort_exit.c ccbfl.c cmpia.c cobfl.c crbmg.c cread.c cwbmg.c \
	     icvidx.c nummtb.c rbytes.c restd.c stseq.c wrdesc.c

FSRC_FILES = adn30.f atrcpt.f bfrini.f blocks.f bort.f bort2.f \
	     bvers.f cadn30.f capit.f chekstab.f chrtrn.f chrtrna.f \
	     cktaba.f closbf.f closmg.f cmpmsg.f cmsgini.f cnved4.f \
	     conwin.f copybf.f copymg.f copysb.f cpbfdx.f cpdxmm.f \
	     cpymem.f cpyupd.f datebf.f datelen.f digit.f drfini.f \
	     drstpl.f dumpbf.f dxdump.f dxinit.f dxmini.f elemdx.f \
	     errwrt.f getabdb.f getbmiss.f getlens.f getntbe.f gets1loc.f \
	     gettagpr.f gettbh.f getvalnb.f getwin.f i4dy.f ibfms.f \
	     icbfms.f ichkstr.f icmpdx.f icopysb.f idn30.f idxmsg.f \
	     ifbget.f ifxy.f igetdate.f igetfxy.f igetntbi.f igetntbl.f \
	     igetsc.f igettdi.f inctab.f invcon.f invmrg.f invtag.f \
	     invwin.f iok2cpy.f ipkm.f ipks.f ireadmg.f ireadmm.f \
	     ireadns.f ireadsb.f irev.f ishrdx.f isize.f istdesc.f \
	     iupb.f iupbs01.f iupbs3.f iupm.f iupvs01.f jstchr.f \
	     jstnum.f lcmgdf.f lmsg.f lstjpb.f makestab.f maxout.f \
	     mesgbc.f mesgbf.f minimg.f mrginv.f msgfull.f msgini.f \
	     msgupd.f msgwrt.f mtinfo.f mvb.f nemock.f nemtab.f \
	     nemtba.f nemtbax.f nemtbb.f nemtbd.f nenubd.f nevn.f \
	     newwin.f nmsub.f nmwrd.f numbck.f numtab.f numtbd.f \
	     nvnwin.f nwords.f nxtwin.f openbf.f openbt.f openmb.f \
	     openmg.f pad.f padmsg.f parstr.f parusr.f parutg.f \
	     pkb.f pkbs1.f pkc.f pkftbv.f pktdd.f pkvs01.f \
	     posapx.f rcstpl.f rdbfdx.f rdcmps.f rdmemm.f rdmems.f \
	     rdmgsb.f rdmsgb.f rdmsgw.f rdmtbb.f rdmtbd.f rdtree.f \
	     rdusdx.f readdx.f readerme.f readlc.f readmg.f readmm.f \
	     readmt.f readns.f reads3.f readsb.f rewnbf.f rjust.f \
	     rsvfvm.f rtrcpt.f seqsdx.f setblock.f setbmiss.f sntbbe.f \
	     sntbde.f status.f stbfdx.f stdmsg.f stndrd.f stntbi.f \
	     stntbia.f strcln.f strcpt.f string.f strnum.f strsuc.f \
	     tabent.f tabsub.f trybump.f ufbcnt.f ufbcpy.f ufbcup.f \
	     ufbdmp.f ufbevn.f ufbget.f ufbin3.f ufbint.f ufbinx.f \
	     ufbmem.f ufbmex.f ufbmms.f ufbmns.f ufbovr.f ufbpos.f \
	     ufbqcd.f ufbqcp.f ufbrep.f ufbrms.f ufbrp.f ufbrw.f \
	     ufbseq.f ufbsp.f ufbstp.f ufbtab.f ufbtam.f ufdump.f \
	     upb.f upbb.f upc.f upds3.f upftbv.f ups.f \
	     uptdd.f usrtpl.f valx.f wrcmps.f wrdlen.f wrdxtb.f \
	     writcp.f writdx.f writlc.f writsa.f writsb.f wrtree.f \
	     wtstat.f

# Object files

OBJ_FILES  = $(CSRC_FILES:.c=.o) $(FSRC_FILES:.f=.o)

# Libraries

LIB = libbufr.a

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
.SUFFIXES: .c .f .o
.f.o:
	$(FC) $(FCFLAGS) -fPIC -real-size 64 -c $<
.c.o:
	$(CC) $(CCFLAGS) -fPIC -DUNDERSCORE -c $<
