include ../configure.vi
include ../pure-openmp.inc

OBJS	= anl_pert.o correct_mat.o fill_nmm_gridg.o grads.o

EXEC	= ../../../exec/hafs_vi_anl_pert.x

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS)

distclean: clean
	$(RM) $(EXEC)

