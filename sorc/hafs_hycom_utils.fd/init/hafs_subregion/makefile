include ../../configure.hycom
FC            = $(SFC)

LDFLAGS        =  -Wl,-noinhibit-exec -i_dynamic 
CPPFLAGS       =  -P -D$(BYTE_ORDER) -DWRF -DLINUX -DSERIAL_IO \
                  -I$(WRF_DIR)/inc

RM            = \rm -f
#programs      = hmon_isubregion2avg hmon_isubregion2 hmon_subregion1 \
#		hmon_sub_topog1 hmon_sub_topog2 hmon_sub_grid1 hmon_sub_grid2
programs      = ../../exec/hafs_rtofs_subregion
EXEC          = ../../exec

.SUFFIXES: 
.SUFFIXES: .c .F .f .o

all:	$(programs)

install: 
	-mv $(programs) $(EXEC)

clean:
	-rm -f *.o *.mod  $(programs)

MODS = mod_xc.o mod_za.o mod_zb.o wtime.o zh.o

# select one of the programs to make,
#  e.g. make subregion ARCH=sun >& Make_subregion


$(EXEC)/hafs_rtofs_subregion:       $(MODS) subregion.o         
	$(FC) $(LDFLAGS) subregion.o  $(MODS) -o $@


mod_xc.o:   mod_xc.F

mod_za.o:   mod_za.F mod_xc.o

mod_zb.o:   mod_za.F mod_xc.o

zh.o:       zh.F zh_sun.f zh_t3e.f zh_ymp.f
