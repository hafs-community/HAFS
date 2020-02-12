# Must include configure.hycom here or CPPFLAGS and CPP are overrided
# as "" on AIX.
include ../configure.hycom

SHELL=/bin/sh
#HYCOM post makefile

SUBDIRS = hafs_ab2data

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
	done

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
	done

