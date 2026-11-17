#! /usr/bin/env python3
################################################################################
# Script Name: hafs_bdeck_to_tcvitals.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script generates TC vitals records from ATCF best track (b-deck)
#   files, for cases without an archived TC vitals file (e.g., storms before
#   the syndat_tcvitals archive starts). One record is written for each
#   synoptic time (00, 06, 12 and 18 UTC) of the best track.
#
#   Older best track files only hold the position, maximum wind, minimum
#   pressure and storm type. The other TC vitals parameters are then derived
#   or estimated as follows (the vortex initialization uses all of them
#   except for the storm motion):
#     - Storm motion: from the best track positions 6 h before and after
#       (one-sided at the ends of the track).
#     - Radius of maximum wind: best track value if available, otherwise
#       Willoughby et al. (2006, MWR, eq. 7a):
#         RMW = 46.4 exp(-0.0155 Vmax + 0.0169 |lat|)  [km, m/s, degrees]
#     - Pressure and radius of the outermost closed isobar: best track values
#       if available, otherwise the --poci and --roci defaults. The pressure
#       is kept at least --min-dp hPa above the minimum pressure.
#     - 34-kt wind radii: best track values if available, otherwise missing
#       (-999), in which case the vortex initialization does not use them.
#     - Storm depth: best track value if available, otherwise from the
#       maximum wind: S (shallow) below 34 kt, M (medium) below 64 kt,
#       and D (deep) from 64 kt.
#   These estimates are no observations: check them, in particular the radii
#   for storms known to be unusually small or large.
# History:
#   11/17/2026: Initial version for the HAFS reanalysis subproject
# Usage:
#   ./hafs_bdeck_to_tcvitals.py [-o syndat_tcvitals.YYYY] bal041992.dat [...]
#   The output is written to stdout without -o, and appended to an existing
#   file with -o (use one file per year, named syndat_tcvitals.YYYY).
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
################################################################################
# This next line will abort in any version earlier than Python 3.6:
f'This script requires Python 3.6 or newer.'

import sys
import math
import argparse
import logging
import datetime

UTILITY_NAME = 'hafs_bdeck_to_tcvitals'
logger = logging.getLogger(UTILITY_NAME)

KT2MS = 0.514444
NM2KM = 1.852
REARTH = 6371.2  # km

# ATCF basin to the TC vitals basin letter and forecast center
BASINS = {
    'AL': ('L', 'NHC'), 'EP': ('E', 'NHC'), 'CP': ('C', 'NHC'),
    'WP': ('W', 'JTWC'), 'IO': ('B', 'JTWC'), 'SH': ('S', 'JTWC'),
    'SL': ('Q', 'NHC'), 'LS': ('Q', 'NHC'),
}
# Subregion (ATCF field 23) for the basins holding more than one
SUBBASINS = {'A': 'A', 'B': 'B', 'S': 'S', 'P': 'P'}

################################################################################

def to_int(text, default=0):
    try:
        return int(text.strip())
    except ValueError:
        return default

def parse_latlon(text):
    """Converts an ATCF latitude or longitude (tenths of degrees with the
    hemisphere, e.g., 254N or 775W) to degrees north or east."""
    text = text.strip()
    value = float(text[:-1])/10.
    if text[-1] in 'SW':
        value = -value
    elif text[-1] not in 'NE':
        raise ValueError(f'Invalid latitude/longitude: {text}')
    return value

def read_bdeck(filename):
    """Reads an ATCF best track file. Returns a dict that maps the storm
    identifier (e.g., '04L') to the list of its fixes sorted by time, where
    each fix is a dict."""
    storms = {}
    with open(filename, 'rt') as fileobj:
        for lineno, line in enumerate(fileobj, 1):
            fields = [f.strip() for f in line.rstrip('\n').split(',')]
            if len(fields) < 10 or not fields[0]:
                continue
            get = lambda i: fields[i] if i < len(fields) else ''
            try:
                basin = fields[0].upper()
                if basin not in BASINS:
                    raise ValueError(f'unknown basin {basin}')
                letter = BASINS[basin][0]
                if basin in ('IO', 'SH'):
                    letter = SUBBASINS.get(get(22).upper(), letter)
                when = datetime.datetime.strptime(fields[2], '%Y%m%d%H')
                minutes = to_int(fields[3])
                if fields[4].upper() != 'BEST' or to_int(fields[5]) != 0:
                    continue
                # tcvitals only exist for the synoptic times
                if when.hour % 6 != 0 or minutes != 0:
                    logger.debug(f'{filename}:{lineno}: skipped the '
                                 f'non-synoptic fix {fields[2]}{minutes:02d}')
                    continue
                stid = f'{to_int(fields[1]):02d}{letter}'
                fixes = storms.setdefault(stid, {})
                fix = fixes.setdefault(when, dict(
                    when=when, center=BASINS[basin][1], stid=stid,
                    lat=parse_latlon(fields[6]), lon=parse_latlon(fields[7]),
                    vmax_kt=to_int(fields[8]), pmin=to_int(fields[9]),
                    type=get(10).upper(), poci=to_int(get(17)),
                    roci_nm=to_int(get(18)), rmw_nm=to_int(get(19)),
                    name=get(27).upper(), depth=get(28).upper()[:1],
                    r34_nm=None))
                # Newer files have one line per wind radii threshold
                if to_int(get(11)) == 34:
                    code = get(12).upper()
                    radii = [to_int(get(i)) for i in range(13, 17)]
                    if code == 'AAA':
                        radii = [radii[0]]*4
                    elif code != 'NEQ':
                        radii = None
                    fix['r34_nm'] = radii
            except (ValueError, IndexError) as e:
                raise ValueError(f'{filename}:{lineno}: cannot parse the '
                                 f'best track line: {e}')
    return {stid: [fixes[when] for when in sorted(fixes)]
            for stid, fixes in storms.items()}

def motion(first, last):
    """Returns the direction of motion (degrees clockwise from north, 1-360)
    and speed (m/s) between two fixes, or (None, None) if undefined."""
    hours = (last['when'] - first['when']).total_seconds()/3600.
    if hours <= 0. or hours > 24.:
        return None, None
    lat1, lat2 = math.radians(first['lat']), math.radians(last['lat'])
    dlon = math.radians((last['lon'] - first['lon'] + 180.) % 360. - 180.)
    a = math.sin((lat2-lat1)/2.)**2 + \
        math.cos(lat1)*math.cos(lat2)*math.sin(dlon/2.)**2
    dist = 2.*REARTH*math.asin(min(1., math.sqrt(a)))
    y = math.sin(dlon)*math.cos(lat2)
    x = math.cos(lat1)*math.sin(lat2) - \
        math.sin(lat1)*math.cos(lat2)*math.cos(dlon)
    direction = math.degrees(math.atan2(y, x)) % 360.
    if round(direction) == 0:
        direction = 360.
    return direction, dist*1000./(hours*3600.)

def storm_motion(fixes, i):
    """Storm motion at fix i from the fixes before and after it."""
    before = fixes[i-1] if i > 0 else fixes[i]
    after = fixes[i+1] if i < len(fixes)-1 else fixes[i]
    for first, last in ((before, after), (fixes[i], after),
                        (before, fixes[i])):
        direction, speed = motion(first, last)
        if direction is not None:
            return direction, speed
    return None, None

def rmw_willoughby(vmax_ms, lat):
    """Radius of maximum wind (km) from Willoughby et al. (2006), eq. 7a."""
    return 46.4*math.exp(-0.0155*vmax_ms + 0.0169*abs(lat))

def depth_from_vmax(vmax_kt):
    if vmax_kt < 34:
        return 'S'
    elif vmax_kt < 64:
        return 'M'
    return 'D'

def tcvitals_line(fix, direction, speed, poci_default, roci_default, min_dp):
    """Formats one fix as a TC vitals record (the same layout as
    tcutil.storminfo.StormInfo.as_tcvitals)."""
    def cint(x):
        return int(abs(round(x)))
    vmax_ms = fix['vmax_kt']*KT2MS
    pmin = fix['pmin'] if fix['pmin'] > 0 else -999
    poci = fix['poci'] if fix['poci'] > 0 else poci_default
    if pmin > 0:
        poci = max(poci, pmin + min_dp)
    roci = fix['roci_nm']*NM2KM if fix['roci_nm'] > 0 else roci_default
    if fix['rmw_nm'] > 0:
        rmw = fix['rmw_nm']*NM2KM
    else:
        rmw = rmw_willoughby(vmax_ms, fix['lat'])
    if fix['r34_nm'] and max(fix['r34_nm']) > 0:
        r34 = [cint(r*NM2KM) if r > 0 else -999 for r in fix['r34_nm']]
    else:
        r34 = [-999]*4
    depth = fix['depth'] if fix['depth'] in ('S', 'M', 'D') else \
        depth_from_vmax(fix['vmax_kt'])
    name = fix['name'] or 'NAMELESS'
    lat, lon = fix['lat'], (fix['lon'] + 180.) % 360. - 180.
    return '%-4s %-3s %-9s %s %03d%s %04d%s %03d %03d %04d ' \
        '%04d %04d %02d %03d %04d %04d %04d %04d %s' % (
            fix['center'][:4], fix['stid'], name[:9],
            fix['when'].strftime('%Y%m%d %H%M'),
            min(900, cint(lat*10.)), 'N' if lat > 0 else 'S',
            min(3600, cint(lon*10.)), 'E' if lon > 0 else 'W',
            -99 if direction is None else min(360, cint(direction)),
            -99 if speed is None else min(999, cint(speed*10.)),
            pmin, min(1100, cint(poci)), min(9999, cint(roci)),
            min(99, cint(vmax_ms)), min(999, max(1, cint(rmw))),
            r34[0], r34[1], r34[2], r34[3], depth)

def main(argv=None):
    parser = argparse.ArgumentParser(
        description='Generate TC vitals records from ATCF best track '
        '(b-deck) files.')
    parser.add_argument('bdeck', nargs='+', help='best track file(s)')
    parser.add_argument('-o', '--output',
                        help='output file, appended to if it exists '
                        '(default: stdout)')
    parser.add_argument('--poci', type=float, default=1012.,
                        help='pressure in hPa of the outermost closed isobar '
                        'if not in the best track (default: %(default)s)')
    parser.add_argument('--roci', type=float, default=300.,
                        help='radius in km of the outermost closed isobar '
                        'if not in the best track (default: %(default)s)')
    parser.add_argument('--min-dp', type=float, default=2.,
                        help='minimum difference in hPa between the pressure '
                        'of the outermost closed isobar and the minimum '
                        'pressure (default: %(default)s)')
    parser.add_argument('-v', '--verbose', action='store_true')
    args = parser.parse_args(argv)

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format='%(asctime)s %(name)s %(levelname)s: %(message)s')

    lines = []
    for filename in args.bdeck:
        storms = read_bdeck(filename)
        if not storms:
            raise ValueError(f'{filename}: no best track fixes found.')
        for stid in sorted(storms):
            fixes = storms[stid]
            estimated = not any(f['rmw_nm'] > 0 for f in fixes)
            for i, fix in enumerate(fixes):
                direction, speed = storm_motion(fixes, i)
                lines.append(tcvitals_line(fix, direction, speed, args.poci,
                                           args.roci, args.min_dp))
            logger.info(f'{filename}: {len(fixes)} records for {stid} '
                        f'{fixes[0]["name"]} {fixes[0]["when"]:%Y%m%d%H}-'
                        f'{fixes[-1]["when"]:%Y%m%d%H}' +
                        ('; the storm size parameters are estimates.'
                         if estimated else '.'))
    text = ''.join(line + '\n' for line in lines)
    if args.output:
        with open(args.output, 'at') as fileobj:
            fileobj.write(text)
    else:
        sys.stdout.write(text)
    return 0

if __name__ == '__main__':
    try:
        sys.exit(main())
    except (ValueError, OSError) as e:
        logger.error(f'FATAL ERROR: {e}')
        sys.exit(1)
