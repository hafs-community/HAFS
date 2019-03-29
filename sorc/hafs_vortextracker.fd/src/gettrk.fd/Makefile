include ../../configure.trk

LIBS = ${LIBS_GETTRK}
INCMOD = ${INC_GETTRK}

OBJS = gettrk_modules.o module_waitfor.o cwaitfor.o # all non-main objects
EXEC = ../../exec/gettrk.x

all: $(EXEC)

$(EXEC):      $(OBJS) 
	$(FC) $(FFLAGS) ${INCMOD} $(LDFLAGS) gettrk_main.f $(OBJS) $(LIBS) -o $(EXEC)

.f.o:
	$(FC) $(FFLAGS) ${INCMOD} -c $< 

.PHONY: clean

clean:
	$(RM) *.o  *.mod
