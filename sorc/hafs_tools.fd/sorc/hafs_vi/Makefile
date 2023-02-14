SHELL=/bin/sh
include ./configure.vi

SUBDIRS = create_trak_guess create_trak_init split anl_pert anl_combine anl_enhance anl_bogus

all: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make ); \
        done

cleanall: clean

clean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
             make $@ ); \
	done

distclean: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
             make $@ ); \
	done
