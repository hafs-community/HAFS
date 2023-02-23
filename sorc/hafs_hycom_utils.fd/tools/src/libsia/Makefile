LIB_sia = libsia.a
include ../../../configure.hycom

# Compilers:
F90=$(SF90) $(FFLAGS)
MPF90=$(DM_F90) $(FFLAGS)
CC=$(SCC)

# Test programs:
# EXES=stat_test time_test glob_test dir_test file_test \
     copy_test tree_test make_sia_const path_test subprocess_test \
     make_make_sia_const worklist_test

EXES=make_sia_const make_make_sia_const

# Library object files:
LIBOBJ = \
        sia_cmdarg.o \
        sia_const.o \
        sia_dir.o \
        sia_fileop.o \
        sia_glob.o \
        sia_stat.o \
        sia_subprocess.o \
        sia_time.o \
        sia_treewalk.o \
	sia_mpi_worklist.o \
        sia_usrgrp.o \
        sia_c_dir.o \
        sia_c_fileop.o \
        sia_c_glob.o \
        sia_c_stat.o \
        sia_c_subprocess.o \
        sia_c_time.o \
        sia_c_usrgrp.o

all: $(EXES) libsia.a
clean: 
	rm -f *.o *.mod a.out *~
bare: clean
	rm -f $(EXES) sia_const.f90 make_sia_const.c *.a
	rm -f ../../libsia.a ../../sia*.mod

################################

worklist_test: libsia.a worklist_test.f90
	$(MPF90) -o worklist_test worklist_test.f90 -L. -lsia -lrt

sia_mpi_worklist.o: sia_mpi_worklist.f90
	$(MPF90) -c -o sia_mpi_worklist.o sia_mpi_worklist.f90

################################

hyhwrf_get_rtofs: hyhwrf_get_rtofs.f90 libsia.a
	$(MPF90) -o hyhwrf_get_rtofs hyhwrf_get_rtofs.f90 -L. -lsia -lrt

mpiuntar: mpiuntar.f90 libsia.a
	$(MPF90) -o mpiuntar mpiuntar.f90 -L. -lsia -lrt

################################

libsia.a: $(LIBOBJ)
	ar cr libsia.a $(LIBOBJ)
	cp -p *.mod libsia.a ../../.

################################

ndate: ndate.f90 libsia.a
	$(F90) -o ndate ndate.f90 -L. -lsia -lrt

################################

path_test: path_test.f90 libsia.a
	$(F90) -o path_test path_test.f90 -L. -lsia

sia_cmdarg.o: sia_cmdarg.f90
	$(F90) -c -o sia_cmdarg.o sia_cmdarg.f90

################################

subprocess_test: subprocess_test.f90 libsia.a
	$(F90) -o subprocess_test subprocess_test.f90 -L. -lsia -lrt

closefrom.o: closefrom.c
	$(CC) -c -o closefrom.o closefrom.c

sia_c_subprocess.o: sia_c_subprocess.c
	$(CC) -c -o sia_c_subprocess.o sia_c_subprocess.c

sia_subprocess.o: sia_subprocess.f90 sia_const.o
	$(F90) -c -o sia_subprocess.o sia_subprocess.f90

################################

sia_const.f90: make_sia_const
	./make_sia_const > sia_const.f90

sia_const.o: sia_const.f90
	$(F90) -c sia_const.f90 -o sia_const.o

make_sia_const: make_sia_const.c
	$(CC) -o make_sia_const make_sia_const.c

make_sia_const.c: make_sia_const.c.in make_make_sia_const
	./make_make_sia_const < make_sia_const.c.in > make_sia_const.c

make_make_sia_const: make_make_sia_const.c
	$(CC) -o make_make_sia_const make_make_sia_const.c

##############################

sia_treewalk.o: sia_treewalk.f90 sia_stat.o sia_dir.o sia_fileop.o sia_time.o sia_usrgrp.o sia_const.o
	$(F90) -c sia_treewalk.f90 -o sia_treewalk.o

tree_test: tree_test.f90 libsia.a
	$(F90) -o tree_test tree_test.f90 -L. -lsia -lrt

##############################

sia_fileop.o: sia_fileop.f90
	$(F90) -c sia_fileop.f90 -o sia_fileop.o

sia_c_fileop.o: sia_c_fileop.c
	$(CC) -c sia_c_fileop.c -o sia_c_fileop.o

file_test: file_test.f90 libsia.a
	$(F90) -o file_test file_test.f90 sia_c_fileop.o sia_fileop.o

copy_test: copy_test.f90 libsia.a
	$(F90) -o copy_test copy_test.f90 -L. -lsia

##############################

sia_glob.o: sia_glob.f90 sia_const.o
	$(F90) -c sia_glob.f90 -o sia_glob.o

sia_c_glob.o: sia_c_glob.c
	$(CC) -c sia_c_glob.c -o sia_c_glob.o

glob_test: glob_test.f90 libsia.a
	$(F90) -o glob_test glob_test.f90 -L. -lsia -lrt

##############################

sia_usrgrp.o: sia_usrgrp.f90
	$(F90) -c sia_usrgrp.f90 -o sia_usrgrp.o

sia_c_usrgrp.o: sia_c_usrgrp.c
	$(CC) -c sia_c_usrgrp.c -o sia_c_usrgrp.o

sia_stat.o: sia_stat.f90 sia_const.o
	$(F90) -c sia_stat.f90 -o sia_stat.o

sia_c_stat.o: sia_c_stat.c
	$(CC) -c sia_c_stat.c -o sia_c_stat.o

stat_test.o: stat_test.f90 libsia.a
	$(F90) -c stat_test.f90 -o stat_test.o

stat_test: stat_test.o libsia.a
	$(F90) -o stat_test stat_test.o -L. -lsia -lrt

##############################

sia_time.o: sia_time.f90
	$(F90) -c sia_time.f90 -o sia_time.o

sia_c_time.o: sia_c_time.c
	$(CC) -c sia_c_time.c -o sia_c_time.o

time_test.o: time_test.f90 libsia.a
	$(F90) -c time_test.f90 -o time_test.o

time_test: time_test.o libsia.a
	$(F90) -o time_test time_test.o -L. -lsia -lrt

##############################

sia_c_dir.o: sia_c_dir.c
	$(CC) -c sia_c_dir.c -o sia_c_dir.o

sia_dir.o: sia_dir.f90
	$(F90) -c sia_dir.f90 -o sia_dir.o

dir_test: dir_test.f90 libsia.a
	$(F90) -o dir_test dir_test.f90 -L. -lsia
