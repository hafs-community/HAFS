SHELL=/bin/sh
# HYCOM LIBS makefile

SUBDIRS = src


all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ) \
	done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	     make $@) ; \
	done

