include ../configure.vi
include ../pure-openmp.inc

SRCS =	modules.f90 split.f90 BOUND.f90 fft99.f90 sig_p_convt.f90 \
	SEPAR.f90 WNLIT.f90 FDUMP.f90 H12.f90 I1MACH.f90 J4SAVE.f90 XGETUA.f90 \
	WNLSM.f90 WNNLS.f90 XERABT.f90 XERCTL.f90 XERPRT.f90 XERROR.f XERRWV.f90 \
	XERSAV.f90 srotm.f90 srotmg.f90 amatrix.f90 rodist.f90 landcmsk.f90 \
        create_rel_domain.f90

OBJS =	modules.o split.o BOUND.o fft99.o sig_p_convt.o \
	SEPAR.o WNLIT.o FDUMP.o H12.o I1MACH.o J4SAVE.o XGETUA.o \
	WNLSM.o WNNLS.o XERABT.o XERCTL.o XERPRT.o XERROR.o XERRWV.o \
	XERSAV.o srotm.o srotmg.o amatrix.o rodist.o landcmsk.o \
        create_rel_domain.o

# Requires either essl or blas library.
LIBS  = $(ESSLLIB)
EXEC  =  ../../../exec/hafs_vi_split.x

$(EXEC): $(OBJS)
	$(SFC) $(FFLAGS) $(FFLAGS_DOUBLE) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

.f.o:
	$(SFC) $(FFLAGS) $(FFLAGS_DOUBLE) -c $<

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod

distclean: clean
	$(RM) $(EXEC)

