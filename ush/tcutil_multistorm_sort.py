#! /usr/bin/env python3
################################################################################
# Script Name: tcutil_multistorm_sort.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script provides the multistorm sort function.
# History:
#   04/21/2023: Adapted from HWRF and improved for HAFS.
#   10/10/2026: Add geospatial bounding box filtering and reverse order options.
################################################################################

import logging, os, sys, re, functools
import produtil.setup
import tcutil.numerics, tcutil.revital

##@namespace ush.tcutil_multistorm_sort_test
# A test script for the multistorm sort function.  This script orders storms by
# some internal priority information defined by hrd_multistorm_sorter().
# Call like so:
# @code{.sh}
#  tcutil_multistorm_sort.py [options] YYYYMMDDHH list of storms and basins
# @endcode
#
# Where:
# * YYYYMMDDHH --- the ten digit date and hour for the cycle of interest
# * stormid=priority --- 19L=3, 18E=1, 03A=5, etc.  A storm to run for
#   that cycle and a user-specified priority for the storm.
# * a basin --- L, E, C, etc.  All storms will be run from that basin.
#
# Options:
# * -b, --bbox LONMIN,LONMAX,LATMIN,LATMAX --- Filter storms within a geographic
#   bounding box. Storms outside this box are excluded from output.
#   Example: -b -100,0,-10,50 (Atlantic region)
# * -r, --reverse --- Print storms in reverse order (low priority to high priority).
#   Default is high priority to low priority.
#
# Examples:
# @code{.sh}
#   tcutil_multistorm_sort.py 2023091400 19L=3 18E=1 L E
#   tcutil_multistorm_sort.py 2023091400 -b -100,0,-10,50 19L=3 L
#   tcutil_multistorm_sort.py 2023091400 --bbox 120,180,0,40 18E=1 E
#   tcutil_multistorm_sort.py 2023091400 -r 19L=3 L
#   tcutil_multistorm_sort.py 2023091400 -b -100,0,-10,50 -r 19L=3 L
# @endcode

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

def in_bbox(storm, bbox, logger=None):
    """!Check if a storm is within a geographic bounding box.

    @param storm tcutil.storminfo.StormInfo object
    @param bbox tuple of (lonmin, lonmax, latmin, latmax)
    @param logger optional logging.Logger for debug messages
    @return True if storm is within bbox, False otherwise
    """
    if bbox is None:
        return True

    lonmin, lonmax, latmin, latmax = bbox
    storm_lon = storm.lon
    storm_lat = storm.lat

    # Check if within latitude bounds
    lat_ok = latmin <= storm_lat <= latmax

    # Check if within longitude bounds
    # Handle cases where bbox crosses the antimeridian (180/-180 boundary)
    if lonmin <= lonmax:
        # Normal case: lonmin < lonmax (e.g., -100 to 0)
        lon_ok = lonmin <= storm_lon <= lonmax
    else:
        # Antimeridian crossing case: lonmin > lonmax (e.g., 120 to -120)
        # Storm is valid if it's either >= lonmin OR <= lonmax
        lon_ok = storm_lon >= lonmin or storm_lon <= lonmax

    if logger:
        logger.debug(
            '%s at (lon=%g, lat=%g): lat_ok=%s, lon_ok=%s, in_bbox=%s'%(
                storm.stormid3, storm_lon, storm_lat, lat_ok, lon_ok, lat_ok and lon_ok))

    return lat_ok and lon_ok

def parse_bbox(bbox_str, logger=None):
    """!Parse a bounding box string into a tuple.

    @param bbox_str string in format "LONMIN,LONMAX,LATMIN,LATMAX"
    @param logger optional logging.Logger for log messages
    @return tuple (lonmin, lonmax, latmin, latmax) or None on error
    """
    if bbox_str is None:
        return None

    try:
        parts = bbox_str.split(',')
        if len(parts) != 4:
            if logger:
                logger.error('Bounding box must have 4 values (LONMIN,LONMAX,LATMIN,LATMAX)')
            return None
        lonmin = float(parts[0])
        lonmax = float(parts[1])
        latmin = float(parts[2])
        latmax = float(parts[3])
        # Validate latitude bounds
        if not (-90 <= latmin <= 90 and -90 <= latmax <= 90):
            if logger:
                logger.error('Latitude values must be between -90 and 90')
            return None
        if latmin > latmax:
            if logger:
                logger.error('latmin (%g) must be <= latmax (%g)'%(latmin, latmax))
            return None
        if logger:
            logger.info('Bounding box: lon=[%g,%g], lat=[%g,%g]'%(
                lonmin, lonmax, latmin, latmax))
        return (lonmin, lonmax, latmin, latmax)
    except ValueError as e:
        if logger:
            logger.error('Invalid bounding box format: %s'%(str(e),))
        return None

def main(args):
    """!Set up logging, reads vitals, outputs storm list."""

    # Set up logging, disable dbn alerts:
    produtil.setup.setup(send_dbn=False,ologlevel=99,jloglevel=99,
                         eloglevel=logging.INFO)
    logger=logging.getLogger('tcutil_multistorm_sort.py')
    # Parse command-line options
    bbox = None
    reverse_order = False
    remaining_args = []
    i = 1
    while i < len(args):
        arg = args[i]
        if arg in ('-b', '--bbox'):
            # Next argument should be the bbox string
            if i + 1 >= len(args):
                logger.error('FATAL ERROR: -b/--bbox requires a value')
                sys.exit(2)
            i += 1
            bbox_str = args[i]
            bbox = parse_bbox(bbox_str, logger)
            if bbox is None:
                logger.error('FATAL ERROR: Invalid bounding box: %s'%(bbox_str,))
                sys.exit(2)
            logger.info('Geographic bounding box filter enabled')
        elif arg in ('-r', '--reverse'):
            reverse_order = True
            logger.info('Output order: low priority to high priority (reversed)')
        elif arg.startswith('-'):
            logger.error('FATAL ERROR: Unknown option: %s'%(arg,))
            sys.exit(2)
        else:
            remaining_args.append(arg)
        i += 1
    # Check we have at least the cycle
    if len(remaining_args) < 1:
        logger.error('FATAL ERROR: Cycle (YYYYMMDDHH) is required')
        sys.exit(2)
    strcycle = remaining_args[0]
    cyc = tcutil.numerics.to_datetime(strcycle)
    YMDH = cyc.strftime('%Y%m%d%H')
    basins = ''
    userprios = dict()

    for arg in remaining_args[1:]:
        if len(arg) == 1:
            if 'LECWPQSAB'.find(arg.upper()) < 0:
                logger.error('FATAL ERROR: Invalid basin %s'%(arg,))
                sys.exit(2)
            basins = basins + arg.upper()
            continue
        m = re.match(r'(\d\d[LECWPQSAB])=(\d+)', arg.upper())
        if not m:
            logger.error('FATAL ERROR: Unrecognized argument: '+arg)
            sys.exit(2)
        (storm, userprio) = m.groups()
        userprios[storm] = int(userprio)

    rv = tcutil.revital.Revital(logger=logger)

    if 'SYNDAThafs' in os.environ:
        vitfiles = [os.environ['SYNDAThafs']+'/syndat_tcvitals.%Y',]
    else:
        logger.error('FATAL ERROR: cannot find the needed environment variable of SYNDAThafs.')
        sys.exit(2)

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

    # Apply geographic bounding box filter if specified
    if bbox is not None:
        logger.info('Applying geographic bounding box filter...')
        rv.discard_except(lambda v: in_bbox(v, bbox, logger))
        logger.info('After bbox filter: %d storm(s) remaining'%(len(list(rv)),))

    rv.clean_up_vitals()
    for v in rv:
        if v.stormid3 in userprios:
            userprio = int(userprios[v.stormid3])
            logger.info('User priority for %s is %d'%(v.stormid3, userprio))
            setattr(v, 'userprio', userprio)

#   rv.sort_by_function(hrd_multistorm_keygen)
    rv.sort_by_function(hrd_multistorm_sorter)

    # Convert to list and optionally reverse
    storm_list = list(rv)
    if reverse_order:
        storm_list.reverse()
        logger.info('Storm list reversed: low priority to high priority')

    for v in storm_list:
        print(v.as_tcvitals())

if __name__=='__main__': main(sys.argv)

