include ../configure.vi

OBJS    = create_trak_init.o
EXEC    = ../../../exec/hafs_vi_create_trak_init.x

$(EXEC): $(OBJS)
	$(SFC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean

clean:
	$(RM) $(OBJS)

distclean: clean
	$(RM) $(EXEC)

