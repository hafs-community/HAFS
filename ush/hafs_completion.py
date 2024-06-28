#! /usr/bin/env python3
################################################################################
# Script Name: hafs_completion.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script logs the completion of an HAFS forecast cycle.
################################################################################
##@namespace ush.hafs_completion
# A simple script that logs the completion of an HAFS forecast cycle.
#
# This script writes the message "HAFS CYCLE COMPLETED" followed by the
# command line arguments, to the produtil.log.jlogger.  This is called by
# the special Rocoto "completion" job that is used to tell Rocoto to stop
# running jobs for the forecast cycle.

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
    produtil.log.jlogger.info('HAFS CYCLE COMPLETED: '+(' '.join(sys.argv[1:])))

if __name__=='__main__': main()
