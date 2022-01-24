include ../configure.vi
include ../pure-openmp.inc

OBJS	= anl_combine.o split_sym_asy.o correct_mat.o interp_coef_agrid.o \
	  fill_nmm_gridg.o

EXEC	= ../../../exec/hafs_vi_anl_combine.x

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS)

distclean: clean
	$(RM) $(EXEC)

