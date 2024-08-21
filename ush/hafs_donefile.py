#! /usr/bin/env python3
################################################################################
# Script Name: hafs_completion.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script makes a "donefile" that indicates the completion of the
#   forecast and post-processing jobs, before the archive and scrub jobs.
################################################################################
##@namespace ush.hafs_donefile
# A simple script that makes a "donefile" that indicates the
# completion of one ensemble member's work, before archiving and
# scrubbing.

import sys, os, glob

if 'USHhafs' in os.environ:
    sys.path.append(os.environ['USHhafs'])
elif 'HOMEhafs' in os.environ:
    sys.path.append(os.path.join(os.environ['HOMEhafs'],'ush'))
else:
    guess_HOMEhafs=os.path.dirname(os.path.dirname(
            os.path.realpath(__file__)))
    guess_USHhafs=os.path.join(guess_HOMEhafs,'ush')
    sys.path.append(guess_USHhafs)

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
