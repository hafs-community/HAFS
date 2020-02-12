include ../../configure.hycom

EXE=../../exec/hmon_get_rtofs
MPF90=$(DM_F90) $(FFLAGS) -I../../libs/

all: $(EXE)
clean: 
	rm -f *.o *.mod a.out *~
bare: clean
	rm -f $(EXE)

################################

module_get_rtofs.o: module_get_rtofs.f90
	$(MPF90) -o module_get_rtofs.o -c module_get_rtofs.f90

$(EXE): get_rtofs.f90 module_get_rtofs.o
	$(MPF90) -o $(EXE) get_rtofs.f90 module_get_rtofs.o -L../../libs/ -lsia -lrt
