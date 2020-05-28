include ../../configure.hycom

FC=$(SFC)
#
# If it is invoked by the command line
#	make -f makefile
#
CMD =      ../../exec/hafs_restart2restart
EXEC = 	../../exec


OBJS=        hycom_subset.o parse.o

OBJST=       

LIBS =        

.SUFFIXES: .c .F .f .o
#
# rules.
#

all: 	$(CMD)

$(CMD):      $(OBJST) $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJST) $(OBJS) $(LIBS)

install:
	-mv $(CMD) $(EXEC)

clean :
	-rm -f *.o $(CMD)
      
