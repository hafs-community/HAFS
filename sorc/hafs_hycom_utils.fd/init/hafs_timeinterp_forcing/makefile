include ../../configure.hycom

FC     = $(DM_FC)
F90    = $(SF90)

LIBS    += -L../../libs -lofs_mods

INC = ../../libs/src/ofs_mods
FFLAGS+= -I$(INC)

OBJ  =     get_abfld1.o
ALLOBJ = $(MODS) $(OBJ)

.SUFFIXES: .c .f .F .F90 .f90 .o

CMD	= ../../exec/hafs_timeinterp_forcing

$(CMD): $(ALLOBJ)  timeinterp_forcing.o
	$(FC) $(LDFLAGS) -o $@ timeinterp_forcing.o $(ALLOBJ) $(LIBS) 

.PHONY: clean

clean:
	$(RM) $(ALLOBJ) $(CMD) *.mod *.o


