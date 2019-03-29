include ../../configure.trk

EXEC = ../../exec/tave.x

all: $(EXEC)

$(EXEC):      tave.o 
	$(FC) $(FFLAGS) ${INC} $(LDFLAGS) tave.o $(LIBS) -o $(EXEC)

.f.o:
	$(FC) $(FFLAGS) ${INC} -c $< 

.PHONY: clean

clean:
	$(RM) *.o  *.mod
