# Must include configure.hycom here or CPPFLAGS and CPP are overrided
# as "" on AIX.
include ../configure.hycom

SHELL=/bin/sh
#HYCOM forecast makefile

SUBDIRS = hafs_forecast

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; ./run_makefile.sh ); \
	done

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
	done

