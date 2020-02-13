SHELL=/bin/sh

SUBDIRS = obs_to_bufr \
          tempdrop_sonde \
          mpiserial

##SUBDIRS = mpiserial 

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
	done

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
	     make $@) ; \
	done
