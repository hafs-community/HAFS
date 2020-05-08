"""!This module contains utilities for plugging tcutil into the Rocoto
workflow manager."""

##@var __all__
# List of symbols exported by "from tcutil.rocoto import *"
__all__=['cycles_as_entity']

import io, random
import tcutil.numerics

from tcutil.numerics import to_datetime, to_timedelta

##@var epsilon
# An epsilon value for time equality comparisons
epsilon=to_timedelta(5)

##@var six_hours
# A datetime.timedelta that represents +6 hours
six_hours=to_timedelta(6*3600)

def entity_quote(string):
    """!Returns a copy of the string with & " < % and > replaced with
    their respective XML entities &#38; &#34; &#60; &#37; and &#62;
    @param string the string to parse.
    @returns a new string with proper replacements"""
    return string.replace('&','&#38;') \
                 .replace('"','&#34;') \
                 .replace('<','&#60;') \
                 .replace('%','&#37;') \
                 .replace('>','&#62;')

def sanity_check_failed(logger,ex):
    """!Logs information about a failure of a sanity check routine.
    The failure is in exception ex, and the "logger" argument must be
    a logging.Logger.
    @param ex the failure information
    @param logger a logging.Logger for log messages"""
    logger.critical('Sanity check failed.',exc_info=True)
    logger.info(
        'tcutil SANITY CHECK FAILED.  Cannot run this configuration.\n'
        'Check paths and conf files.  See earlier messages for details.')

def cycles_as_entity(cycleset):
    """!Returns a set of Rocoto XML <cycledef> tags to add to an XML
    file.  The tags will define the list of cycles specified in the
    cycleset argument.  That argument must be a set of datetime
    objects.
    @param cycleset an iterable of cycles to convert.  These can be
      anything accepted by tcutil.numerics.to_datetime()"""
    cycles=list(cycleset)
    cycles = [ to_datetime(x) for x in cycles ]
    cycles.sort()
    ctream=io.StringIO()
    first=cycles[0]
    last=cycles[0]
    sent=cycles[0]-six_hours
    for cycle in cycles:
        if to_datetime(cycle) > to_datetime(last)+six_hours+epsilon:
            # Found a break in the cycles
            writeme='<cycledef>%s00 %s00 06:00:00</cycledef> ' \
                %(first.strftime('%Y%m%d%H'),
                  last.strftime('%Y%m%d%H'))
            ctream.write(writeme)
            sent=last
            first=cycle
            last=cycle
        else:
            last=cycle
    if sent+epsilon < last:
        # Need to send the last group of cycles
        writeme='<cycledef>%s00 %s00 06:00:00</cycledef> ' \
                     %(first.strftime('%Y%m%d%H'),
                       last.strftime('%Y%m%d%H'))
        ctream.write(writeme)
    ctream.write('\n')

    out=str(ctream.getvalue())
    ctream.close()
    return out

