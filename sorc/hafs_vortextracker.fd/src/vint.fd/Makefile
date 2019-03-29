include ../../configure.trk

EXEC = ../../exec/vint.x

all: $(EXEC)

$(EXEC):      vint.o 
	$(FC) $(FFLAGS) ${INC} $(LDFLAGS) vint.o $(LIBS) -o $(EXEC)

.f.o:
	$(FC) $(FFLAGS) ${INC} -c $< 

.PHONY: clean

clean:
	$(RM) *.o  *.mod
