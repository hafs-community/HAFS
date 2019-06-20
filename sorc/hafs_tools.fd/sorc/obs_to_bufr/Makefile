EXEC = ../../exec/obs_to_bufr.x

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

INC = -I${UTILS_INC} ${NETCDF_INCLUDE} ${HDF5_INCLUDE}
LIBS = -L${UTILS_LIBDIR} -lfson -lslatec -lslint -lspline ${BUFR_LDFLAGS} \
       ${NETCDF_LDFLAGS} ${HDF5_LDFLAGS} ${Z_LIB}
FFLAGS += $(INC)

$(EXEC): main.F90 $(MODS) $(SUBS)
	${FC} $(FFLAGS) main.F90 $(MODS) $(SUBS) -o $(@) $(LIBS)

.PHONY: clean

.SUFFIXES: .F90 .o

clean:
	rm *.o *.mod

.F90.o:
	${FC} $(FFLAGS) -c $*.F90
