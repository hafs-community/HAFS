include ../../configure.hycom
#F90=$(SF90)
#FC=$(SF90)

INC += -I$(G2_INC4)
FFLAGS+= -I$(INC)

OBJS    = getgds2.o
CMD    = ../../exec/hmon_getgds2

$(CMD):        $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) $(CMD)

