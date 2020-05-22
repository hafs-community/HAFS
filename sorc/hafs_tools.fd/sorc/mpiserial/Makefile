
include ./configure.hafs

OBJS =	mpiserial.o

LIBS  = 
EXEC  =  ../../exec/mpiserial.x

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(DM_CC) $(DM_LDFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.o

