# This is a Makefile include that should be used by the per-program or
# per-library Makefiles to compile and link code that uses OpenMP but
# not MPI.

FFLAGS+=$(OMP_FFLAGS)
LDFLAGS+=$(OMP_FFLAGS)
FC=$(SFC)
F90=$(SF90)
CC=$(SCC)
