#! /usr/bin/env python3

import logging, os, sys, re, functools
import produtil.setup
import tcutil.numerics, tcutil.revital

##@namespace ush.tcutil_multistorm_sort_test
# A test script for the multistorm sort function.  This script orders storms by
# some internal priority information defined by hrd_multistorm_sorter().
# Call like so:
# @code{.sh}
#  tcutil_multistorm_sort.py YYYYMMDDHH list of storms and basins
# @endcode
#
# Where:
# * YYYYMMDDHH --- the ten digit date and hour for the cycle of interest
# * stormid=priority --- 19L=3, 18E=1, 03A=5, etc.  A storm to run for
#   that cycle and a user-specified priority for the storm.
# * a basin --- L, E, C, etc.  All storms will be run from that basin.

def oldcmp(a,b):
    if a<b: return -1
    if a>b: return 1
    return 0

def hrd_multistorm_sorter(a,b):
    """!A drop-in replacement for "cmp" that can be used for sorting or
    comparison.  Returns -1 if a<b, 1 if a>b or 0 if a=b.  Decision is
    made in this order:

        User priority (a.userprio): lower (priority 1) is "more
        important" than higher numbers (priority 9999 is fill value).

        Invest vs. non-invest: invest is less important

        wind: stronger wind is more important than weaker wind

        North Atlantic (L) storms: farther west is more important

        North East Pacific (E) storms: farther East is more important

    If all of the above values are equal, 0 is returned."""
    a_userprio=getattr(a,'userprio',9999)
    b_userprio=getattr(b,'userprio',9999)
    a_invest=1 if (a.stormname=='INVEST') else 0
    b_invest=1 if (b.stormname=='INVEST') else 0
    a_basin=1  if (a.basin1=='E') else 0
    b_basin=1  if (b.basin1=='E') else 0

    c = oldcmp(a_userprio,b_userprio) or\
        oldcmp(a_invest,b_invest) or\
        oldcmp(a_basin,b_basin) or\
        -oldcmp(a.wmax,b.wmax) or\
        (a.basin1=='L' and b.basin1=='L' and oldcmp(a.lon,b.lon)) or \
        (a.basin1=='E' and b.basin1=='E' and -oldcmp(a.lon,b.lon))
    return c

hrd_multistorm_keygen=functools.cmp_to_key(hrd_multistorm_sorter)

##@var vitfiles
# List of known tcvitals file locations.  Each is intended to be sent
# through a datetime.datetime.format to get the final filename.
vitfiles=[
    '/gpfs/dell1/nco/ops/com/gfs/prod/syndat/syndat_tcvitals.%Y',
    '/lfs3/HFIP/hwrf-data/hwrf-input/SYNDAT-PLUS/syndat_tcvitals.%Y',
    '/lfs4/HFIP/hwrf-data/hwrf-input/SYNDAT-PLUS/syndat_tcvitals.%Y',
    '/work/noaa/hwrf/noscrub/input/SYNDAT-PLUS/syndat_tcvitals.%Y',
    '/scratch1/NCEPDEV/hwrf/noscrub/input/SYNDAT-PLUS/syndat_tcvitals.%Y']

def main(args):
    """!Set up logging, reads vitals, outputs storm list."""

    # Set up logging, disable dbn alerts:
    produtil.setup.setup(send_dbn=False,ologlevel=99,jloglevel=99,
                         eloglevel=logging.INFO)
    logger=logging.getLogger('tcutil_multistorm_sort.py')
    strcycle=args[1]
    cyc=tcutil.numerics.to_datetime(strcycle)
    YMDH=cyc.strftime('%Y%m%d%H')
    basins=''
    userprios=dict()

    for arg in args[2:]:
        if len(arg)==1:
            if 'LECWPQSAB'.find(arg.upper())<0:
                logger.error('Invalid basin %s'%(arg,))
                exit(2)
            basins=basins+arg.upper()
            continue
        m=re.match('(\d\d[LECWPQSAB])=(\d+)',arg.upper())
        if not m:
            logger.error('Unrecognized argument: '+arg)
            exit(2)
        (storm,userprio)=m.groups()
        userprios[storm]=int(userprio)

    rv=tcutil.revital.Revital(logger=logger)
    rv.readfiles([ cyc.strftime(v) for v in vitfiles ],
                  raise_all=False)
    #rv.renumber(threshold=14)
    #rv.renumber(unrenumber=True)
    rv.delete_invest_duplicates()
    rv.clean_up_vitals()
    rv.discard_except(lambda v: v.YMDH==YMDH)
    if basins:
        logger.info('Only keep basins: <%s>'%(basins,))
        rv.discard_except(lambda v: v.basin1 in basins)
    rv.clean_up_vitals()
    for v in rv:
        if v.stormid3 in userprios:
            userprio=int(userprios[v.stormid3])
            logger.info('User priority for %s is %d'%(v.stormid3,userprio))
            setattr(v,'userprio',userprio)

    rv.sort_by_function(hrd_multistorm_keygen)
    for v in rv:
        print(v.as_tcvitals())

if __name__=='__main__': main(sys.argv)
