SHELL=/bin/sh
# HYCOM makefile
#------------
# Include machine dependent compile & load options
#------------
include ./configure.hycom

SUBDIRS = libs init post
LIBDIRS = libs

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
        done

#library: $(LIBDIRS)
#	for dir in $(LIBDIRS); do \
#	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
#	done

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
             make $@ ); \
	done
