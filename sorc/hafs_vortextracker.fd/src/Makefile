SHELL=/bin/sh
# Vortex Tracker makefile
#------------
# Include machine dependent compile & load options
#------------

SUBDIRS = vint.fd tave.fd supvit.fd gettrk.fd 

all: $(SUBDIRS)

$(SUBDIRS)::
	cd $@ ; make

clean::
	@for dir in $(SUBDIRS); do                                       \
           ( cd $$dir; echo "Making $@ in `pwd`" ;                       \
             $(MAKE) $@ );                                               \
        done
