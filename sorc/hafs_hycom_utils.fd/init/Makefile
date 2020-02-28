# Must include configure.hycom here or CPPFLAGS and CPP are overrided
# as "" on AIX.
include ../configure.hycom

SHELL=/bin/sh
#HYCOM init makefile

SUBDIRS = \
	hafs_get_rtofs \
	hafs_archv2restart \
	hafs_gfs2ofs2 \
	hafs_restart2restart \
	hafs_subregion \
	hafs_timeinterp_forcing  

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
	done

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	   make $@) ; \
	done

