include ../configure.vi
include ../pure-openmp.inc

SRCS =	modules.f split.f BOUND.f fft99.f sig_p_convt.f \
	SEPAR.f WNLIT.f FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f \
	WNLSM.f WNNLS.f XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f \
	XERSAV.f srotm.f srotmg.f amatrix.f rodist.f landcmsk.f \
        create_rel_domain.f

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

