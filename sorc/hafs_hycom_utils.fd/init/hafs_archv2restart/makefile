include ../../configure.hycom

.SUFFIXES: 
.SUFFIXES: .c .F .f .o

FC            =	$(DM_FC)
CC            =	$(DM_CC)
LD            =	$(FC)

RM            = \rm -f
CMD	= ../../exec/hafs_archv2restart
EXEC	= ../../exec

all:    $(CMD) 

install:
	mv -f $(CMD) $(EXEC)

clean:
	-rm -f *.o *.mod $(CMD)

MODS     =	mod_plot.o mod_xc.o mod_za.o               wtime.o
MODB     =	mod_plot.o mod_xc.o mod_za.o mod_zb.o      wtime.o
MODR     =	mod_plot.o mod_xc.o mod_za.o mod_restart.o wtime.o

DATAX_OBJ = 	bigrid.o  bigrd1.o blkin.o    \
		buoflx.o                      \
		extrct.o  extrot.o fordate.o  \
                getdat.o  getdtm.o getdepth.o \
		          indxi.o  indxj.o    \
                pakk.o    poisnd.o psmoo.o    \
		sbmerg.o  zebra.o  zh.o       \
                dum_gks.o

DATAH_OBJ = 	$(DATAX_OBJ) horout.o
DATAR_OBJ = 	$(DATAX_OBJ) putdat.o
DATAZ_OBJ = 	$(DATAH_OBJ) layer2z.o mixlay.o

NCDFH_OBJ = 	$(DATAX_OBJ) horout_nc.o
NCDFZ_OBJ = 	$(NCDFH_OBJ) layer2z.o mixlay.o

ARCHM_OBJ = 	bigrid.o blkin.o  \
		extrct.o extrot.o \
                getdat.o getdtm.o getdepth.f putdat.o \
		indxi.o  indxj.o  \
		zebra.o  zh.o     dum_gks.o \
		pakk.o   
# 		pakk_micom.o   

ARCHV_OBJ = 	bigrid.o blkin.o  \
		extrct.o extrot.o fordate.o \
                getdat.o getdtm.o getdepth.o putdat.o \
		indxi.o  indxj.o  pakk.o  \
		zebra.o   zh.o    dum_gks.o 

ARCHV_SRC = 	hycomarchv.f micomarchv.f \
                trim_archv.f archv2restart.f remap_archv \
                archv2data2d.f archv2data2t.f archv2data3z.f archv2datasf.f archv2datasfl.f archv2restart.f putdat.f

#
# --- executables.
#

$(CMD):         $(MODR) archv2restart.o   $(ARCHV_OBJ)
	$(FC) $(LDFLAGS) -o $@  archv2restart.o $(MODR) $(ARCHV_OBJ)


archv2restart.o:    archv2restart.f    mod_plot.o mod_za.o

bigrd1.o:   bigrd1.f      mod_plot.o
bigrid.o:   bigrid.f      mod_plot.o
blkin.o:    blkin.f
buoflx.o:   buoflx.f
dum_gks.o:  dum_gks.f
extrct.o:   extrct.f
extrot.o:   extrot.f
fordate.o:  fordate.f
getdat.o:   getdat.f      mod_plot.o mod_za.o
getdepth.o: getdepth.f    mod_plot.o mod_za.o
getdtm.o:   getdtm.f      mod_plot.o mod_za.o
horout.o:   horout.f      mod_plot.o mod_xc.o mod_zb.o
horout_nc.o: horout_nc.f  mod_plot.o mod_xc.o mod_zb.o
indxi.o:    indxi.f       mod_plot.o
indxj.o:    indxj.f       mod_plot.o
layer2z.o:  layer2z.f
mixlay.o:   mixlay.f
pakk.o:     pakk.f
poisnd.o:   poisnd.f      mod_plot.o
prtmsk.o:   prtmsk.f
psmoo.o:    psmoo.f       mod_plot.o
putdat.o:   putdat.f      mod_plot.o mod_za.o
sbmerg.o:   sbmerg.f      mod_plot.o
wtime.o:    wtime.F
zebra.o:    zebra.f

mod_plot.o: mod_plot.F
mod_xc.o:   mod_xc.F   
mod_za.o:   mod_za.F   mod_xc.o
mod_zb.o:   mod_zb.F

zh.o:       zh.F zh_sun.f zh_t3e.f zh_ymp.f
