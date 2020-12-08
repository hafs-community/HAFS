#! /usr/bin/env python3

##@namespace ush.tcutil_generate_vitals
# A utility script for tcvitals manipulation, a wrapper around tcutil.revital
#
# This script is a wrappar around the tcutil.storminfo and tcutil.revital
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
#      about how renumbering was done.  It must be preceeded by a non-empty
#      argument (such as the word "LOG").
# * /path/to/syndat/ --- a directory with tcvitals data.  If this is not
#      specified, the script will try to guess where the data resides.
#
# Options:
# * -v --- verbosity
# * -n --- disable renumbering of invest cycles
# * -H --- enable HHS output format.  Do not use.
#
# Environment Variables
# * $RUN_ENVIR --- set to NCO if you are NCEP Central Operations, or something else if you are not
# * $CASE_ROOT=HISTORY --- retrospective runs
# * $CASE_ROOT=FORECAST --- real-time runs
# * $COMINarch --- if you are NCO, this is the path to the tcvitals
# * $envir --- if you are NCO, this is the run environment (prod, para, test)
# * $COMINmsg --- if you are NCO, this is where the message files reside

import logging, sys, os, getopt, collections
#import produtil.sigsafety, produtil.fileop
import tcutil.revital
import tcutil.rocoto

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

##@var PARAFLAG
# True = we are not NCEP Central Operations ($RUN_ENVIR!=NCO in environment)
PARAFLAG = ( 'NCO' != os.environ.get('RUN_ENVIR','EMC').upper() )

##@var basins_needed
# In format=cycles_needed mode, the list of one-letter basins needed.

########################################################################
# THIS SECTION NEEDS HARD-CODED PATHS FOR EMC ##########################
# HARD-CODED PATHS ARE PROTECTED BY RUN_ENVIR!=NCO BLOCK ###############
def set_para_paths():
    """!Sets tcvitals and message file locations for non-NCO runs."""
    global tcvlocs, messagedir, inputs
    tcvlocs=[
        "/work/noaa/hwrf/noscrub/input/SYNDAT-PLUS",
        "/work/noaa/hwrf/noscrub/input/SYNDAT",
        "/scratch1/NCEPDEV/hwrf/noscrub/input/SYNDAT-PLUS",
        "/scratch1/NCEPDEV/hwrf/noscrub/input/SYNDAT",
        "/lfs3/HFIP/hwrf-data/hwrf-input/SYNDAT-PLUS",
        "/lfs3/HFIP/hwrf-data/hwrf-input/SYNDAT",
        "/lfs4/HFIP/hwrf-data/hwrf-input/SYNDAT-PLUS",
        "/lfs4/HFIP/hwrf-data/hwrf-input/SYNDAT",
        "/gpfs/hps3/emc/hwrf/noscrub/input/SYNDAT-PLUS",
        "/gpfs/hps3/emc/hwrf/noscrub/input/SYNDAT",
        "/gpfs/dell1/nco/ops/com/gfs/prod/syndat",
        ]
    messagedir=[
        "/work/noaa/hwrf/noscrub/input/MESSAGES",
        "/scratch1/NCEPDEV/hwrf/noscrub/input/MESSAGES",
        "/lfs1/HFIP/hwrf-vd/hwrf-input/MESSAGES",
        "/gpfs/hps/nco/ops/com/hur/prod/inpdata"
        ]
    if 'CASE_ROOT' in os.environ and os.environ['CASE_ROOT']=='FORECAST':
        tcvlocs=['/gpfs/dell1/nco/ops/com/gfs/prod/syndat']
    else:
        tcvlocs.append('/gpfs/dell1/nco/ops/com/gfs/prod/syndat')
# END OF SECTION WITH HARD-CODED PATHS #################################
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
'''

def usage(why=None):
    """!Prints a usage message on stderr and exits with status 1."""
    sys.stderr.write(usage_message)
    if why:
        sys.stderr.write('\nSCRIPT IS ABORTING DUE TO ERROR: %s\n'%(why,))
        sys.exit(1)
    else:
        sys.exit(0)

def main():
    """!Main program.  Parses arguments, reads inputs, writes outputs."""
    # PARSE ARGUMENTS
    global logger, inputs, tcvlocs, messagedir, forecast, PARAFLAG
    global basins_needed
    renumber=True
    unrenumber=False
    rename=False
    format='tcvitals'
    threshold=14
    try:
        (optlist,args) = getopt.getopt(sys.argv[1:],'HvnW:NuRC:')
        for opt,val in optlist:
            if   opt=='-v':
                logger.setLevel(logging.DEBUG)
                logger.debug('Verbosity enabled.')
            elif opt=='-C':
                logger.info('Switching to "cycles needed" format')
                format='cycles_needed'
                basins_needed=str(val)
            elif   opt=='-W':
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
                logger.error('Invalid option %s'%(opt,))
                sys.exit(1)
    except (getopt.GetoptError,ValueError,TypeError) as e:
        usage(str(e))
        sys.exit(1)

    if unrenumber and format=='tcvitals':
        logger.info('Switching to "renumbering" format output.')
        format='renumbering'

    ########################################################################
    # DECIDE VITALS LOCATIONS
    # THIS PARA BLOCK PROTECTS AGAINST NCO REACHING HARD-CODED PATHS:
    if PARAFLAG:
        set_para_paths()
    else:
        # Find tcvitals:
        if 'COMINarch' in os.environ:
            tcvlocs = [ os.environ['COMINarch'], ]
        else:
            fail('ERROR: Both $COMINarch and $envir are unset in '
                 '$RUN_ENVIR!=NCO mode.  Cannot find tcvitals.')
        # Find message files
        if 'COMINmsg' in os.environ:
            messagedir = [ os.environ['COMINmsg'], ]
        else:
            fail('ERROR: Both $COMINmsg and $envir are unset in '
                 '$RUN_ENVIR!=NCO mode.  Cannot find tcvitals.')
    ########################################################################

    if 'CASE_ROOT' in os.environ and os.environ['CASE_ROOT']=='FORECAST':
        for d in messagedir:
            if os.path.isdir(d):
                inputs.extend([os.path.join(d,'message%d'%(1+x,)) \
                                   for x in range(5)])
                break

    if len(args)<2:
        print('ERROR: Script requires at least two '\
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
                logger.error('%s: syndat file does not exist'%(tcvfile,))
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
        logger.critical('ERROR: %s'%(str(e),))

if __name__=='__main__': main()
