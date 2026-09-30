#! /usr/bin/env python3
################################################################################
# Script Name: tcutil_generate_vitals.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script is a wrapper around the tcutil.storminfo and tcutil.revital
#   modules. It can read in tcvitals files from known locations and manipulate
#   them in various ways. It can also generate forecast cycle vitals files
#   (tmpvit, oldvit, tm03vit, tp03vit) similar to hafs.launcher.HAFSLauncher.gen_vitals().
# History:
#   04/03/2023: Adapted from HWRF and improved for HAFS.
#   09/30/2026: Enhanced to support forecast cycle tcvitals generation.
################################################################################
##@namespace ush.tcutil_generate_vitals
# A utility script for tcvitals manipulation, a wrapper around tcutil.revital
#
# This script is a wrapper around the tcutil.storminfo and tcutil.revital
# modules.  It can read in tcvitals files from known locations and
# manipulate them in various ways.  Call like so:
# @code{.sh}
# tcutil_generate_vitals.py [options] stormid years [LOG renumberlog [/path/to/syndat/ ]]
# @endcode
#
# Where:
# * stormid --- the three character storm ID (ie.: 12L is Katrina)
# * years --- a single argument with a space-separated list of years.
#      Typically only one year is provided but multiple is allowed.
# * LOG renumberlog --- The renumberlog is an output file with information
#      about how renumbering was done.  It must be preceded by a non-empty
#      argument (such as the word "LOG").
# * /path/to/syndat/ --- a directory with tcvitals data.  If this is not
#      specified, the script will try to guess where the data resides.
#
# Options:
# * -v --- verbosity
# * -n --- disable renumbering of invest cycles
# * -H --- enable HHS output format.  Do not use.
# * -c CYCLE --- generate forecast cycle vitals for a specific cycle (YYYYMMDDHH)
# * -o OUTDIR --- output directory for cycle vitals files (tmpvit, oldvit, tm03vit, tp03vit)
# * -i INTERVAL --- cycling interval in hours (default: 6)
#
# Environment Variables
# * $CASE_ROOT=HISTORY --- retrospective runs
# * $CASE_ROOT=FORECAST --- real-time runs
# * $SYNDAThafs --- optional to specify the directory with tcvitals data
# * $COMINarch --- if you are NCO, this is the path to the tcvitals
# * $COMINmsg --- if you are NCO, this is where the message files reside

import logging, sys, os, getopt, collections
import tcutil.revital
import tcutil.rocoto
import tcutil.storminfo
import tcutil.numerics
from datetime import datetime, timedelta

##@var logger
# Logging domain used for this script
logger=None

# Set up logging.  We customize things to look nice.
handler=logging.StreamHandler(sys.stderr)
handler.setFormatter(logging.Formatter(
        'tcutil_revital:%(levelname)s:%(asctime)s: %(message)s',
        '%Y%m%d.%H%M%S'))
logger=logging.getLogger()
logger.setLevel(logging.INFO)
logger.addHandler(handler)

##@var forecast
# Unused.
forecast=False

##@var inputs
# List of input tcvitals and message files to read in.
inputs=[]

##@var tcvlocs
# List of possible tcvitals locations.
tcvlocs=[]

##@var messagedir
# Directory with message files
messagedir=[]

##@var basins_needed
# In format=cycles_needed mode, the list of one-letter basins needed.

########################################################################

usage_message='''tcutil_generate_vitals.py version 5.7.1
SYNOPSIS:
  tcutil_generate_vitals.py 11L 2015

  Generates cycle lists and other information from TCVitals data.

SYNTAX:

  tcutil_generate_vitals.py [options] STORMID YEAR

The STORMID must be a capital, three-character storm identifier such
as 11L or 18E or 04S.  The only valid basin letters are the ones found
in the tcvitals database.

The year must be four digits.

OPTIONS:

  -v           => Be verbose.
  -W 14        => Set the "weak storm" threshold to 14 knots.
  -n           => Disable renumbering of invests to non-invests.
  -N           => Enable renaming of storms to last name seen.
  -u           => Unrenumber and unrename after renumbering and
                  renaming, discarding unrelated cycles
  -R           => Output data in Rocoto <cycledef> tags.
  -H           => Do not use.  Special output format for HHS.
  -c CYCLE     => Generate forecast cycle vitals for specific cycle (YYYYMMDDHH).
                  When specified, generates tmpvit, oldvit, tm03vit, and tp03vit files.
  -o OUTDIR    => Output directory for cycle vitals files.
  -i INTERVAL  => Cycling interval in hours (default: 6).
'''

def usage(why=None):
    """!Prints a usage message on stderr and exits with status 1."""
    sys.stderr.write(usage_message)
    if why:
        sys.stderr.write('\nFATAL ERROR: SCRIPT IS ABORTING DUE TO ERROR: %s\n'%(why,))
        sys.exit(1)
    else:
        sys.exit(0)

def generate_cycle_vitals(revital, stormid, cycle_str, cycling_interval_hours,
                          outdir=None, logger=None):
    """!Generate forecast cycle vitals files (tmpvit, oldvit, tm03vit, tp03vit).

    Similar to HAFSLauncher.gen_vitals() but simpler, focused on generating
    the time-specific vital files for a given cycle.

    @param revital tcutil.revital.Revital object with vitals data
    @param stormid three-character storm ID (e.g., "12L")
    @param cycle_str cycle string in format YYYYMMDDHH
    @param cycling_interval_hours cycling interval in hours (e.g., 6)
    @param outdir output directory (if None, prints to stdout)
    @param logger logging.Logger for log messages
    """

    if logger is None:
        logger = logging.getLogger()

    # Parse cycle
    try:
        cycle = datetime.strptime(cycle_str, '%Y%m%d%H')
    except ValueError as e:
        logger.error('Invalid cycle format: %s. Expected YYYYMMDDHH'%(cycle_str,))
        raise

    stnum = int(stormid[0:2], 10)
    STID = stormid.upper()
    strcycle = cycle.strftime('%Y%m%d%H')

    # Calculate prior cycle
    cycling_interval = abs(cycling_interval_hours) * 3600.0
    prior = tcutil.numerics.to_datetime_rel(-cycling_interval, cycle)
    strprior = prior.strftime('%Y%m%d%H')

    logger.info('gen_cycle_vitals: cycle=%s interval=%s hours prior=%s STID=%s'%(
            repr(strcycle), repr(cycling_interval_hours), repr(strprior),
            repr(STID)))

    # Define filter for requested storm
    def keep_condition(vit):
        return vit.stormid3.upper() == STID or \
               ('old_stormid3' in vit.__dict__ and
                vit.old_stormid3.upper() == STID)

    # Handle invest renumbering similar to gen_vitals()
    if stnum >= 50:
        logger.info('%s: Not renumbering invests because %d >= 50.'%(STID, stnum))
        unrenumbered = revital.copy()
        unrenumbered.discard_except(keep_condition)
        unrenumbered.clean_up_vitals()
        renumbered = unrenumbered
    else:
        logger.info('%s: Renumber and unrenumber invests.'%(STID,))
        unrenumbered = revital.copy()
        unrenumbered.renumber(unrenumber=True)
        unrenumbered.discard_except(keep_condition)
        unrenumbered.clean_up_vitals()
        renumbered = unrenumbered.copy()
        renumbered.swap_numbers()
        renumbered.clean_up_vitals()
        unrenumbered.mirror_renumbered_vitals()
        unrenumbered.clean_up_vitals()

    # Find current cycle's vitals
    syndat = None
    for vit in renumbered.each(STID):
        if vit.when == cycle:
            syndat = vit
            break

    if syndat is None:
        raise tcutil.storminfo.NoSuchVitals(
            'Error: cannot find %s at cycle %s'%(STID, strcycle))

    logger.info('syndat = %s'%(syndat.as_tcvitals(),))

    # Find prior cycle's vitals
    oldsyndat = None
    nodatasyndat = None

    for vit in unrenumbered.each(STID, old=True):
        if vit.when != prior:
            continue

        if oldsyndat is None:
            if nodatasyndat is not None and nodatasyndat.stnum < 50:
                logger.info('%s %s: not using as backup since found non-invest %s already'%(
                    str(vit.stormid3), str(vit.YMDH),
                    str(nodatasyndat.stormid3)))
            else:
                nodatasyndat = vit

    # Use prior vital or extrapolate
    if oldsyndat is None:
        if nodatasyndat is not None:
            oldsyndat = nodatasyndat
            logger.info('%s %s: using %s %s as prior cycle storm.'%(
                STID, strcycle, oldsyndat.stormid3, strprior))
        else:
            logger.warning('No prior syndat available. Extrapolating vitals.')
            oldsyndat = syndat - cycling_interval_hours
            logger.warning('extrapolated vitals: %s'%(oldsyndat.as_tcvitals(),))
    else:
        logger.info('%s %s: prior cycle on disk for %s %s'%(
            STID, strcycle, oldsyndat.stormid3, strprior))

    logger.info('oldsyndat = %s'%(oldsyndat.as_tcvitals(),))

    # Generate time-shifted vitals
    tm03syndat = syndat - 3.0  # vitals at T-03
    # Interpolate wmax and pmin at tm03
    tm03syndat.wmax = int(round(oldsyndat.wmax + 0.5 * (syndat.wmax - oldsyndat.wmax)))
    tm03syndat.wmax = max(min(tm03syndat.wmax, 99), 0)
    tm03syndat.pmin = int(round(oldsyndat.pmin + 0.5 * (syndat.pmin - oldsyndat.pmin)))
    tm03syndat.pmin = max(min(tm03syndat.pmin, 1100), 800)

    tp03syndat = syndat + 3.0  # vitals at T+03
    # Note: position is shifted, but wind/pressure are not extrapolated (as in gen_vitals)
    # Another approach is to also extrapolate to get wmax and pmin at tp03
    #tp03syndat.wmax=int(round(syndat.wmax+0.5*(syndat.wmax-oldsyndat.wmax)))
    #tp03syndat.wmax=max(min(tp03syndat.wmax, 99), 0)
    #tp03syndat.pmin=int(round(syndat.pmin+0.5*(syndat.pmin-oldsyndat.pmin)))
    #tp03syndat.pmin=max(min(tp03syndat.pmin, 1100), 800)

    logger.info('tm03syndat = %s'%(tm03syndat.as_tcvitals(),))
    logger.info('tp03syndat = %s'%(tp03syndat.as_tcvitals(),))

    # Write output files
    if outdir is not None:
        logger.info('Writing cycle vitals files to %s'%(outdir,))

        # Ensure output directory exists
        if not os.path.exists(outdir):
            os.makedirs(outdir, exist_ok=True)

        # tmpvit - current cycle vitals
        tmpvit_file = os.path.join(outdir, 'tmpvit')
        logger.info('%s: write current cycle vitals here'%(tmpvit_file,))
        with open(tmpvit_file, 'wt') as f:
            print(syndat.as_tcvitals(), file=f)

        # oldvit - prior cycle vitals
        oldvit_file = os.path.join(outdir, 'oldvit')
        logger.info('%s: write prior cycle vitals here'%(oldvit_file,))
        with open(oldvit_file, 'wt') as f:
            print(oldsyndat.as_tcvitals(), file=f)

        # tm03vit - T-03 vitals
        tm03vit_file = os.path.join(outdir, 'tm03vit')
        logger.info('%s: write T-03 vitals here'%(tm03vit_file,))
        with open(tm03vit_file, 'wt') as f:
            print(tm03syndat.as_tcvitals(), file=f)

        # tp03vit - T+03 vitals
        tp03vit_file = os.path.join(outdir, 'tp03vit')
        logger.info('%s: write T+03 vitals here'%(tp03vit_file,))
        with open(tp03vit_file, 'wt') as f:
            print(tp03syndat.as_tcvitals(), file=f)

        logger.info('Successfully wrote cycle vitals files.')
        return {
            'tmpvit': tmpvit_file,
            'oldvit': oldvit_file,
            'tm03vit': tm03vit_file,
            'tp03vit': tp03vit_file,
            'syndat': syndat,
            'oldsyndat': oldsyndat,
            'tm03syndat': tm03syndat,
            'tp03syndat': tp03syndat
        }
    else:
        # Print to stdout
        print('tmpvit:', file=sys.stdout)
        print(syndat.as_tcvitals(), file=sys.stdout)
        print('', file=sys.stdout)
        print('oldvit:', file=sys.stdout)
        print(oldsyndat.as_tcvitals(), file=sys.stdout)
        print('', file=sys.stdout)
        print('tm03vit:', file=sys.stdout)
        print(tm03syndat.as_tcvitals(), file=sys.stdout)
        print('', file=sys.stdout)
        print('tp03vit:', file=sys.stdout)
        print(tp03syndat.as_tcvitals(), file=sys.stdout)
        return {
            'syndat': syndat,
            'oldsyndat': oldsyndat,
            'tm03syndat': tm03syndat,
            'tp03syndat': tp03syndat
        }

def main():
    """!Main program.  Parses arguments, reads inputs, writes outputs."""
    # PARSE ARGUMENTS
    global logger, inputs, tcvlocs, messagedir, forecast
    global basins_needed
    renumber=True
    unrenumber=False
    rename=False
    format='tcvitals'
    threshold=14
    cycle_str=None
    outdir=None
    cycling_interval_hours=6
    try:
        (optlist,args) = getopt.getopt(sys.argv[1:],'HvnW:NuRC:c:o:i:')
        for opt,val in optlist:
            if   opt=='-v':
                logger.setLevel(logging.DEBUG)
                logger.debug('Verbosity enabled.')
            elif opt=='-C':
                logger.info('Switching to "cycles needed" format')
                format='cycles_needed'
                basins_needed=str(val)
            elif opt=='-c':
                cycle_str=str(val)
                logger.info('Generating cycle vitals for cycle: %s'%(cycle_str,))
            elif opt=='-o':
                outdir=str(val)
                logger.info('Output directory: %s'%(outdir,))
            elif opt=='-i':
                cycling_interval_hours=float(val)
                logger.info('Cycling interval: %s hours'%(cycling_interval_hours,))
            elif opt=='-W':
                threshold=int(val)
                logger.debug('Weak storm threshold is now %d'%(threshold,))
            elif opt=='-n':
                renumber=False
                logger.info('Disabling renumbering due to -n')
            elif opt=='-N':
                rename=True
                logger.info('Enabling renaming.')
            elif opt=='-u':
                unrenumber=True
                logger.info('Enabling un-renumbering and un-renaming.')
            elif opt=='-H': format='HHS'
            elif opt=='-R': format='rocoto'
            else:
                logger.error('FATAL ERROR: Invalid option %s'%(opt,))
                sys.exit(1)
    except (getopt.GetoptError,ValueError,TypeError) as e:
        usage('FATAL ERROR: '+str(e))
        sys.exit(1)

    if unrenumber and format=='tcvitals':
        logger.info('Switching to "renumbering" format output.')
        format='renumbering'

    ########################################################################
    # DECIDE VITALS LOCATIONS
    if 'SYNDAThafs' in os.environ:
        tcvlocs=[os.environ['SYNDAThafs'],]
    elif 'COMINarch' in os.environ and 'COMINmsg' in os.environ:
        tcvlocs = [ os.environ['COMINarch'], ]
        messagedir = [ os.environ['COMINmsg'], ]
    else:
        logger.critical('FATAL ERROR: cannot find tcvitals.')
        logger.critical('FATAL ERROR: Need either set SYNDAThafs or set COMINarch and COMINmsg.')
        sys.exit(2)
    ########################################################################

    if 'CASE_ROOT' in os.environ and os.environ['CASE_ROOT']=='FORECAST':
        for d in messagedir:
            if os.path.isdir(d):
                inputs.extend([os.path.join(d,'message%d'%(1+x,)) \
                                   for x in range(5)])
                break

    if len(args)<2:
        print('FATAL ERROR: Script requires at least two '\
            'arguments: stormid and year', file=sys.stderr)
        sys.exit(1)

    stormid=str(args[0]).upper()
    if stormid=='ALL':
        stormid='00X'
        stormnum=0
    else:
        stormnum=int(stormid[0:2])
    tcvyears_in=[ int(x) for x in str(args[1]).split()]
    tcvyears=list()
    xset=set()

    def check_test_vitals(vl):
        """This is a replacement for tcutil.storminfo.name_number_okay for
        use with TEST storms and internal stormids.  It allows through
        only the storm numbers matching stormnum, regardless of the
        storm name (usually TEST and UNKNOWN would be dropped)."""
        logger.info('Keeping only storm number %d in vitals'%(stormnum,))
        for vital in vl:
            if vital.stnum==stormnum:
                yield vital
            elif getattr(vital,'old_stnum','XX')==stormnum:
                yield vital

    for tcvyear in tcvyears_in:
        if tcvyear not in xset:
            xset.add(tcvyear)
            tcvyears.append(tcvyear)

    if len(args)>2 and args[2]!='':
        renumberlog=open(str(sys.argv[3]),'wt')
    else:
        renumberlog=None

    if len(args)>3:
        for tcvyear in tcvyears:
            tcvfile=os.path.join(str(args[3]),'syndat_tcvitals.%04d'%(tcvyear,))
            if not os.path.isdir(tcvfile):
                logger.error('FATAL ERROR: %s: syndat file does not exist'%(tcvfile,))
                sys.exit(1)
            inputs.append(tcvfile)
    else:
        for tcvyear in tcvyears:
            for thatdir in tcvlocs:
                thatfile=os.path.join(thatdir,'syndat_tcvitals.%04d'%(tcvyear,))
                if os.path.exists(thatfile) and os.path.getsize(thatfile)>0:
                    inputs.append(thatfile)
                    break
                else:
                    logger.debug('%s: empty or non-existent'%(thatfile,))

    try:
        revital=tcutil.revital.Revital(logger=logger)
        logger.info('List of input files: %s'%( repr(inputs), ))
        logger.info('Read input files...')
        revital.readfiles(inputs,raise_all=False)
        if not renumber:
            logger.info(
                'Not renumbering because renumbering is disabled via -n')
            logger.info('Cleaning up vitals instead.')
            revital.clean_up_vitals()
        elif stormnum<50:
            logger.info('Renumber invests with weak storm threshold %d...'
                        %(threshold,))
            revital.renumber(threshold=threshold,
                             discard_duplicates=unrenumber)
        elif stormnum>=90:
            logger.info('Not renumbering invests when storm of '
                        'interest is 90-99.')
            logger.info('Cleaning up vitals instead.')
            revital.clean_up_vitals()
        else:
            logger.info('Fake stormid requested.  Running limited clean-up.')
            revital.clean_up_vitals(name_number_checker=check_test_vitals)
        if rename and stormnum<50:
            logger.info('Renaming storms.')
            revital.rename()
        elif rename:
            logger.info('Not renaming storms because storm id is >=50')

        if unrenumber:
            logger.info('Unrenumbering and unrenaming storms.')
            revital.swap_numbers()
            revital.swap_names()

        # If cycle generation is requested, generate cycle vitals files
        if cycle_str is not None:
            logger.info('Generating cycle vitals for %s'%(cycle_str,))
            generate_cycle_vitals(revital, stormid, cycle_str,
                                cycling_interval_hours, outdir, logger)
        else:
            # Original behavior - print vitals
            logger.info('Reformat vitals...')
            if format=='rocoto' and stormid=='00X':
                cycleset=set([ vit.YMDH for vit in revital ])
                print(tcutil.rocoto.cycles_as_entity(cycleset))
            elif format=='cycles_needed':
                cycles=collections.defaultdict(set)
                for vit in revital:
                    if vit.basin1 in basins_needed:
                        if vit.stnum<50 and vit.stnum>0:
                            cycles[vit.when].add(vit.stormid3)
                for cycle in sorted(cycles.keys()):
                    print(cycle.strftime('%Y%m%d%H')+': '+' '.join(cycles[cycle]))
            elif format=='rocoto':
                # An iterator that iterates over YMDH values for vitals
                # with the proper stormid:
                def okcycles(revital):
                    for vit in revital:
                        if vit.stormid3==stormid:
                            yield vit.YMDH
                cycleset=set([ ymdh for ymdh in okcycles(revital) ])
                print(tcutil.rocoto.cycles_as_entity(cycleset))
            elif stormid=='00X':
                revital.print_vitals(sys.stdout,renumberlog=renumberlog,
                                     format=format,old=True)
            else:
                revital.print_vitals(sys.stdout,renumberlog=renumberlog,
                                     stormid=stormid,format=format,old=True)
    except Exception as e:
        logger.info(str(e),exc_info=True)
        logger.critical('FATAL ERROR: %s'%(str(e),))
        sys.exit(1)

if __name__=='__main__': main()
