EXEC = ${EXECpath}/obs_to_bufr.x

MODS = kinds_interface.o \
       constants_interface.o \
       variable_interface.o \
       namelist_interface.o \
       diagnostics_interface.o \
       time_methods_interface.o \
       math_methods_interface.o \
       meteo_methods_interface.o \
       bufrio_interface.o \
       json_interface.o \
       netcdf_interface.o \
       fileio_interface.o \
       interpolation_interface.o \
       observation_interface.o \
       obs_to_bufr_interface.o

SUBS =

OBJS = $(MODS) $(SUBS) main.o

INC = -I${DEPLIBS_PATH}/include ${NETCDF_INCLUDE}
LIBS = -L${DEPLIBS_PATH}/lib -lfson -lslatec -lslint -lspline ${NETCDF_LDFLAGS} ${BUFR_LDFLAGS}
FFLAGS += $(INC)

$(EXEC): main.F90 $(MODS) $(SUBS)
	${FC} $(FFLAGS) main.F90 $(MODS) $(SUBS) -o $(@) $(LIBS)

.PHONY: clean

.SUFFIXES: .F90 .o

clean:
	rm $(MODS) $(SUBS) *.mod

.F90.o:
	${FC} $(FFLAGS) -c $*.F90
