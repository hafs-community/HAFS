include ../../configure.hycom
#
SRCS=   flush.f90 constants.f90 horiz_interp.f90 mod_hytime.f90 mod_flags.f90 \
	mod_hycomio1.f90 mod_dump.f90 mod_grib2io.f90 mod_geom.f90 intp.f90 cd.f90

OBJS=	${SRCS:.f90=.o}

LIBS += -L../../libs -lofs_mods $(W3NCO_LIB4) $(BACIO_LIB4) $(G2_LIB4) $(W3EMC_LIB4) -L$(JASPERLIB) -ljasper $(PNG_LIB) -lz

INC = -I../../libs/src/ofs_mods
INC += -I$(G2_INC4)
FFLAGS+= $(INC)

CMD   =		../../exec/hafs_gfs2ofs2
FC	= $(SFC)
F90	= $(SF90)

# actual rules which make uses to build swathps.
#
all:	$(CMD)

$(CMD):		$(OBJS) 
	$(SFC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

#$(OBJS):	$(SRCS) $(INC)
#	$(SFC) $(FFLAGS) -c $<

# Make the profiled version of the command and call it swathps.prof
#
$(CMD).prof:	$(OBJS)
	$(SFC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS) *.mod $(CMD)

copy_summ:
	cp summary.o.hold summary.o

allclean:	clean all

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
