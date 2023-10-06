include ../configure.vi
include ../pure-openmp.inc

OBJS	= anl_enhance.o convert_axi_xy.o correct_mat_2.o interp_coef_agrid.o

EXEC	= ../../../exec/hafs_vi_anl_enhance.x

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS)

distclean: clean
	$(RM) $(EXEC)

