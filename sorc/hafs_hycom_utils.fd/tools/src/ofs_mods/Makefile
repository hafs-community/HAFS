LIB_ofs = libofs_mods.a 
include ../../../configure.hycom

FC=$(DM_FC)

.SUFFIXES: .o .f .F

OBJS1 = zh.o mod_xc.o mod_za.o mod_zb.o wtime.o

SRC_F = $(OBJS1:.o=.F)

#CPPFLAGS+= -DIFC

$(LIB_ofs): $(OBJS1)
	$(FC) $(FFLAGS) $(FFLAGS_SINGLE) -c $(SRC_F)
	$(AR) $(ARFLAGS) $(LIB_ofs) $(OBJS1)
	$(CP) $(LIB_ofs) ../..

clean:
	$(RM) $(OBJS1) *.mod *.a



mod_xc.o:   mod_xc.F

mod_za.o:   mod_za.F mod_xc.o

mod_zb.o:   mod_za.F mod_xc.o

wtime.o:    wtime.F

zh.o:       zh.F zh_sun.f zh_t3e.f zh_ymp.f
