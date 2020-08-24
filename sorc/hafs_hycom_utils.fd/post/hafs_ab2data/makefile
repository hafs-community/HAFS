include ../../configure.hycom
FC=$(SFC)

SHELL         = /bin/sh

.SUFFIXES: 
.SUFFIXES: .c .F .f .o

LIBS+=   -L../../libs -lofs_mods $(W3NCO_LIB4) $(W3EMC_LIB4) $(BACIO_LIB4) \
	 $(NETCDF_LDFLAGS) $(NETCDF_LDFLAGS_C) $(HDF5_LDFLAGS) -lz
FFLAGS+= $(NETCDF_INCLUDE) -I../../libs/src/ofs_mods -I.

MODS     =	mod_plot.o mod_xc.o mod_za.o          wtime.o
MODB     =	mod_plot.o mod_xc.o mod_za.o mod_zb.o wtime.o
EXEC     =      ../../exec

programs=  ../../exec/hafs_archv2data3z \
	   ../../exec/hafs_archv2data2d \
	   ../../exec/hafs_archv3z2nc

all: ${programs}

clean:
	/bin/rm -f *.o *.a mod_plot.mod  mod_xc.mod    mod_za.mod    mod_zb.mod \
	${programs}

install:
	mv -f ${programs} ${EXEC}

DATAX_OBJ = 	bigrid.o  bigrd1.o blkin.o    \
		extrct.o  extrot.o fordate.o  \
                getdat.o  getdtm.o getdepth.o \
		          indxi.o  indxj.o    \
                pakk.o    poisnd.o psmoo.o    \
		sbmerg.o  zebra.o  zh.o       \
                dum_gks.o  \
		getdat4rtofs.o 

DATAH_OBJ = 	$(DATAX_OBJ) horout_nc.o
DATAZ_OBJ = 	$(DATAH_OBJ) layer2z.o

NCDFH_OBJ = 	$(DATAX_OBJ) horout_nc.o
NCDFZ_OBJ = 	$(NCDFH_OBJ) layer2z.o

GRIB_OBJ = 	ab2grib.o gribit.o grib_maps.o

#THERM_OBJ=      feistel.o
THERM_OBJ=      pot2pot0.o

#
# --- executables.
#

../../exec/hafs_archv2data3z:  $(MODB) archv2data3z.o  $(NCDFZ_OBJ) $(GRIB_OBJ) $(THERM_OBJ)
	$(FC) $(LDFLAGS) -o $@  archv2data3z.o  $(MODB) $(NCDFZ_OBJ) $(GRIB_OBJ) $(THERM_OBJ) $(LIBS)

../../exec/hafs_archv2data2d:  $(MODB) archv2data2d.o  $(NCDFZ_OBJ) $(GRIB_OBJ) $(THERM_OBJ)
	$(FC) $(LDFLAGS) -o $@  archv2data2d.o  $(MODB) $(NCDFZ_OBJ) $(GRIB_OBJ) $(THERM_OBJ) $(LIBS)

../../exec/hafs_archv3z2nc:  $(MODB) archv3z2nc.o  $(NCDFZ_OBJ) $(GRIB_OBJ) $(THERM_OBJ)
	$(FC) $(LDFLAGS) -o $@  archv3z2nc.o  $(MODB) $(NCDFZ_OBJ) $(GRIB_OBJ) $(THERM_OBJ) $(LIBS)

archv2data2d.o:  archv2data2d.f  mod_plot.o mod_za.o
archv2data2d.o:  archv2data2d.f  mod_plot.o mod_za.o
archv3z2nc.o:  archv3z2nc.f  mod_plot.o mod_za.o

bigrd1.o:   bigrd1.f      mod_plot.o
bigrid.o:   bigrid.f      mod_plot.o
blkin.o:    blkin.f
dum_gks.o:  dum_gks.f
extrct.o:   extrct.f
extrot.o:   extrot.f
fordate.o:  fordate.f
getdat4rtofs.o:   getdat4rtofs.f      mod_plot.o mod_za.o
getdat.o:   getdat.f      mod_plot.o mod_za.o
getdepth.o: getdepth.f    mod_plot.o mod_za.o
getdtm.o:   getdtm.f      mod_plot.o mod_za.o
horout_nc.o:   horout_nc.f      mod_plot.o mod_xc.o mod_zb.o
horout_nc.o: horout_nc.f  mod_plot.o mod_xc.o mod_zb.o grib_block.h locale.inc clib.inc 
indxi.o:    indxi.f       mod_plot.o
indxj.o:    indxj.f       mod_plot.o
layer2z.o:  layer2z.f
pakk.o:     pakk.f
poisnd.o:   poisnd.f      mod_plot.o
prtmsk.o:   prtmsk.f
psmoo.o:    psmoo.f       mod_plot.o
putdat.o:   putdat.f      mod_plot.o mod_za.o
ab2grib.o:  ab2grib.f     
gribit.o:   gribit.f
sbmerg.o:   sbmerg.f      mod_plot.o
wtime.o:    wtime.F
zebra.o:    zebra.f

mod_plot.o: mod_plot.F
mod_xc.o:   mod_xc.F   
mod_za.o:   mod_za.F   mod_xc.o
mod_zb.o:   mod_zb.F

zh.o:       zh.F zh_sun.f zh_t3e.f zh_ymp.f
