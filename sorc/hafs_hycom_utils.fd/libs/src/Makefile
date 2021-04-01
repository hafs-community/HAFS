SHELL=/bin/sh
# HYCOM LIBS makefile

SUBDIRS = ofs_mods libsia


all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
	done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	     make $@) ; \
	done

