EXEC = create_stats_short.exe 

OBJS = create_stats_short.f90 DISTSP.f ATAN2D.f COSD.f SIND.f

FC =           ifort 
LDFLAGS =
FFLAGS =         -O2 

all: $(EXEC)

$(EXEC): $(OBJS)
	$(FC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f *.exe 

