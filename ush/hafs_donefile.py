#! /usr/bin/env python3

##@namespace ush.hafs_donefile
# A simple script that makes a "donefile" that indicates the
# completion of one ensemble member's work, before archiving and
# scrubbing.

import sys
import produtil.setup, produtil.log

def main():
    """!Main program.  Sets up the produtil package and logs a message
    about cycle completion."""
    produtil.setup.setup()
    filename=' '.join(sys.argv[1:])
    produtil.log.jlogger.info('MAKE DONEFILE: '+filename)
    try:
        with open(filename,'wt') as f:
            f.write('Cycle is complete\n')
    except Exception as e:
        produtil.log.jlogger.error('%s: %s'%(filename,str(e)),
                                   exc_info=True)

if __name__=='__main__': main()
