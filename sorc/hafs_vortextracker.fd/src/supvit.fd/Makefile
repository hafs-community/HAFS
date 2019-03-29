include ../../configure.trk

EXEC = ../../exec/supvit.x

OBJ = supvit_modules.o supvit_main.o
all: $(EXEC)

$(EXEC): $(OBJ)
	$(FC) $(FFLAGS) ${INC} $(LDFLAGS) $(OBJ) $(LIBS) -o $(EXEC)

.f.o:
	$(FC) $(FFLAGS) ${INC} -c $< 

.PHONY: clean

clean:
	$(RM) *.o  *.mod
