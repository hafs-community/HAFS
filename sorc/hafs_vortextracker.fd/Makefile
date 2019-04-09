SHELL=/bin/sh
# Vortex Tracker makefile
#------------
# Include machine dependent compile & load options
#------------
include ./configure.trk

SUBDIRS = src

all: $(SUBDIRS)

$(SUBDIRS)::
	cd $@ ; make

clean::
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
             make $@ ); \
        done
	$(RM) exec/*.x
