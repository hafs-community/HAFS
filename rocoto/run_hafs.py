#! /usr/bin/env python

##@namespace run_hafs
# @brief A wrapper around the Rocoto workflow system that knows how to run HAFS in Rocoto
#
# @anchor run_hafs_main
# This is a Python program, run_hafs.py, that users run to launch and maintain an
# HAFS workflow
#
# @code{.sh}
# run_hafs.py [options] [ensids and cycles] 95E case_root [conf]
# @endcode
#
# Arguments:
# * 95E --- stormid
# * case_root --- HISTORY for retrospective runs or FORECAST for real-time
# 
# Options:
# * -f --- Force a run even if the *.xml and *.db file already exist
# * -w workflow.xml --- specify the Rocoto XML file
# * -d workflow.db --- specify the Rocoto database file
# * -s site-file.ent --- Specify the site file in the sites/ subdirectory
#
# Cycles:
# * 2014081412 --- one cycle to run
# * 2014081400-2014081618 --- a range of cycles to run
# * 2014 --- run all cycles for this storm in this year
# * -t --- include cycles even if they are not in the tcvitals
# * -n --- disable renumbering of invests into non-invests
# * -W N --- discard invests weaker than N m/s before renumbering
#
# Conf opitons:
# * ../parm/hafs_more.conf --- read this configuration file
# * config.run_gsi=yes --- specify the value of one configuration option

##@cond RUN_HAFS_PY

import os, sys, re, logging, collections, StringIO, getopt, itertools
from os.path import realpath, normpath, dirname

def ask(question):
    sys.stdout.write(question)
    itry=0
    itrytoohard=100
    go=True
    while go:
        itry+=1
        x=sys.stdin.readline()
        if x.lower()=='y\n':
            return True
        elif x.lower()=='n\n':
            return False
        elif itry>=itrytoohard:
            sys.stderr.write('Giving up after %d failed responses.'%itry)
            sys.exit(2)
        else:
            sys.stdout.write('Please answer y or n.')

def usage(message=None,logger=None):
    """!Dumps a usage message and exits with status 2.
    @param message An extra message to send to stderr after the usage message
    @param logger Ignored."""
    print>>sys.stderr, '''
Usage: run_hafs.py [options] [cycles] 95E case_root [conf]

Mandatory arguments: 
  95E -- the storm to run
  case_root -- FORECAST = real-time mode, HISTORY = retrospective mod

Workflow options:
  -f -- Tells the run_hafs.py that you already ran it once for this
        storm, cycle list and experiment.  It will use any existing
        *.xml or *.db file without asking for permission.  Critical 
        in crontabs.
  -w workflow-file.xml -- use this as the output XML file to send 
        into rocotorun (rocotorun's -w option)
  -d workflow-db.db -- use this as the SQLite3 database file for
        Rocoto (rocotorun's -d option)

Specifying a site:
  -s site-file -- path to a custom-made site file, rather than using
        one automatically chosen from sites/*.ent.  Do not include any
        shell or XML metacharacters in the name.

PATHS:
  This script should be run from the rocoto/ subdirectory of the HAFS
  installation location so it can guess the ush/ and parm/ locations
  (../ush and ../parm).  You can override those guesses by providing
  the paths in the $USHhafs and $PARMhafs environment variables.

SPECIFYING CYCLES:

  -c N -- number of hours between cycles.  This ONLY affects cycle
   specifications after the -c option.

  [cycles] -- one or more cycle specifications:
    2014091312-2014091712     - run this range of cycles
    2014091312                - run this cycle
    2014                      - all cycles from 0Z January 1, 2014 to 
                                the end of that year.
    2014091312-2014091712 2014091800 - run cycles from 2014091312
                               through 2014091712 AND run 2014091800

  -t -- include cycles even if they are not in the tcvitals.  This
        option is turned on automatically when H214 cycle lists are
        requested.

  -n -- disable renumbering of invests to non-invests.  This is done
        automatically when an invest is requested.

  -W N -- discard invests weaker than N meters per second before
        renumbering.  Default: -W 14 if a non-invest storm is 
        requested, and -W 0 (don't discard) if an invest is requested.

Configuration ([conf]):
section.option=value -- override conf options on the command line
/path/to/file.conf -- additional conf files to parse'''
    if message is not None:
        print>>sys.stderr,str(message).rstrip()+'\n'
    sys.exit(2)

########################################################################
# Try to guess $USHhafs and $PARMhafs.  The $HOMEhafs or current
# working directory are used if $USHhafs and $PARMhafs are not set in
# the environment.  We also add the $USHhafs to the Python library
# path.

##@var USHhafs
# The ush/ subdirectory of the HAFS installation directory
USHhafs=None

##@var HOMEhafs
# The HAFS installation directory
HOMEhafs=None

##@var PARMhafs
# The parameter directory
PARMhafs=None

if os.environ.get('USHhafs',''):   USHhafs=os.environ['USHhafs']
if os.environ.get('PARMhafs',''):  PARMhafs=os.environ['PARMhafs']
if os.environ.get('HOMEhafs',''):  HOMEhafs=os.environ['HOMEhafs']

if HOMEhafs is None and (USHhafs is None or PARMhafs is None):
    HOMEhafs=dirname(os.getcwd())
    USHguess=os.path.join(HOMEhafs,'ush')
    PARMguess=os.path.join(HOMEhafs,'parm')
    if os.path.isdir(USHguess) and os.path.isdir(PARMguess):
        if USHhafs is None: USHhafs=USHguess
        if PARMhafs is None: PARMhafs=PARMguess

if HOMEhafs is not None:
    if USHhafs is None:            USHhafs=os.path.join(HOMEhafs,'ush')
    if PARMhafs is None:           PARMhafs=os.path.join(HOMEhafs,'parm')

if USHhafs is None: 
    print>>sys.stderr, "Cannot guess $USHhafs.  Please set $HOMEhafs or " \
        "$USHhafs in environment."
    sys.exit(2)

if PARMhafs is None: 
    print>>sys.stderr, "Cannot guess $PARMhafs.  Please set $HOMEhafs or " \
        "$PARMhafs in environment."
    sys.exit(2)

if HOMEhafs is None:
    print>>sys.stderr, "Cannot guess $HOMEhafs.  Please set $HOMEhafs " \
        "in the environment."
    sys.exit(2)

sys.path.append(USHhafs)

########################################################################
# Load and set up the produtil package.
import hafs.launcher, hafs.prelaunch
import tcutil.revital, tcutil.numerics, tcutil.rocoto
from tcutil.numerics import to_datetime, to_timedelta
from tcutil.rocoto import entity_quote

import produtil.setup, produtil.atparse, produtil.run, produtil.prog, \
    produtil.fileop, produtil.batchsystem, produtil.cluster

from produtil.fileop import remove_file, isnonempty
from produtil.run import run, exe, runstr
from produtil.prog import shbackslash

#######import hafs.launcher
#######import hafs_expt

produtil.batchsystem.set_jobname('run_hafs')
produtil.setup.setup(send_dbn=False)

########################################################################
# Global variables and constants

logger=logging.getLogger('run_hafs')

epsilon          = to_timedelta(5)  # five seconds
six_hours        = to_timedelta(6*3600)
cycling_interval = six_hours
cycleset         = set()
enset            = set()
mslist           = list()
mblist           = list()
benchmarkset     = None
parse_tcvitals   = True
renumber         = True
force            = False
site_file        = ''
outxml           = ''
outdb            = ''
dateargs         = list()
iarg             = 0
firstarg         = 0
weak_invest      = None
multistorm       = False
storms_opt	 = ''
basins_opt	 = ''
renumber_opt	 = ''


def okpath(path):
    return produtil.fileop.norm_expand_path(path,fullnorm=True)

########################################################################
# Parse the options and arguments.

short_opts = "c:d:fm:M:ns:tW:w:"
long_opts  = ["cycling=",
              "database=",
              "force",
              "multistorms=",
              "multibasins=",
              "renumber=",
              "site=",
              "tcvitals",
              "weak",
              "workflow="
             ]
try:
    opts, args = getopt.getopt(sys.argv[1:], short_opts, long_opts)
except getopt.GetoptError as err:
    print str(err)
    usage('SCRIPT IS ABORTING DUE TO UNRECOGNIZED ARGUMENT')

for k, v in opts:
    if    k in ('-c', '--cycling'):
        cycling_interval = to_timedelta(int(v)*3600)
    elif  k in ('-d', '--database'):
        outdb = v
    elif  k in ('-f', '--force'):
        force = True
    elif  k in ('-m', '--multistorms'):
        mslist.extend(v.split(","))
        multistorm = True
        storms_opt=''.join(['-m ', ','.join(mslist)])
    elif  k in ('-M', '--multibasins'):
        mblist.extend(v.split(","))
        multistorm = True
        basins_opt=''.join(['-M ', ','.join(mblist)])
    elif  k in ('-n', '--renumber'):
        renumber = False
        renumber_opt='-n'
    elif  k in ('-s', '--site'):
        site_file = str(v)
    elif  k in ('-t', '--tcvitals'):
        parse_tcvitals = False
    elif  k in ('-W', '--weak'):
        weak_invest = int(v)
    elif  k in ('-w', '--workflow'):
        outxml = v
    else:
        assert False, "UNHANDLED OPTION"


# Make sure the workflow isn't the database
if outxml[-3:]=='.db':
    usage('When using the -d option, the Rocoto XML filename must '
          'not end with ".db".')
# Make sure the database isn't the workflow
if outdb[-4:]=='.xml':
    usage('When using the -d option, the database filename must '
          'not end with ".xml".')

for arg in args:
    if re.match('\A\d\d\Z',arg):
        logger.info('ensemble id')
        # Single ensemble ID
        enset.add('%02d'%int(arg,10))
    elif re.match('\A\d\d-\d\d\Z',arg):
        logger.info('list of ensemble ids')
        # List of ensemble IDs
        en1=int(arg[0:2],10)
        en2=int(arg[3:],10)
        enset.update([ "%02d"%(x+en1) for x in range(en2-en1+1) ])
    elif re.match('\A\d{10}\Z',arg):
        logger.info('single date/time')
        # Single date/time
        cycleset.add(arg)
        dateargs.append(arg)
    elif re.match('\A\d{4}\Z',arg):
        logger.info('year')
        # Year
        start=to_datetime(arg+'01010000')
        end=to_datetime(arg+'12312359')
        now=start
        while now<end+epsilon:
            cycleset.add(now.strftime('%Y%m%d%H'))
            now+=cycling_interval
        dateargs.append(arg)
    elif re.match('\A\d{10}-\d{10}\Z',arg):
        logger.info('range of cycles')
        # Range of date/times
        start=to_datetime(arg[0:10])
        end=to_datetime(arg[11:])
        now=start
        while now<end+epsilon:
            cycleset.add(now.strftime('%Y%m%d%H'))
            now+=cycling_interval
        dateargs.append(start)
    elif re.match('\A\d\d[A-Z]\Z',arg.upper()):
        logger.info('storm id')
        # Storm ID.  This ends our argument parsing.  We pass the
        # remaining arguments on to parse_launch_args.
        firstarg=iarg
        if renumber:
            if arg[0]=='9':
                logger.info('Disabling renumbering for invest storm '+arg)
                renumber=False
            elif arg[0]=='8':
                logger.info('Disabling renumbering for test storm '+arg)
                renumber=False
        break
    elif re.match('\AH214:\d\d\d\d\Z',arg.upper()):
        # H214 cycle requested
        logger.info('H214 - use the H214 benchmark cycles')
        parse_tcvitals=False
        benchmarkset=arg.upper()
#    else:
#        usage('SCRIPT IS ABORTING DUE TO UNRECOGNIZED ARGUMENT "%s"'%(arg,))
    iarg+=1

if benchmarkset and cycleset:
    usage('SCRIPT IS ABORTING: YOU CANNOT SPECIFY CYCLES AND '
          'USE A BENCHMARK SET')

if enset==set(['99']):
    enset=set()

# Now parse the rest of the arguments the same way as exhafs_launch:
print 'firstarg',firstarg
print 'argsfirstarg..',args[firstarg:]

stid=args[firstarg]
case_root=args[firstarg+1]
logger.info('Running for storm '+stid.upper())

def fullify(s):
    m=re.match('''(?x)(?P<section>[a-zA-Z][a-zA-Z0-9_]*)\.(?P<option>[^=]+)=(?P<value>.*)$''',s)
    if not m:
        return os.path.abspath(s)
    else:
        return s

# Turn any conf files specified in arguments into fully-qualified
# paths.  This is needed because run_hafs will generally be started
# from a different directory than the exhafs_launch.py.
if firstarg+2<len(args):
    confargs=args[(firstarg+2):]
    more_launch_vars=' '.join(
        entity_quote(shbackslash(fullify(str(x))))
        for x in confargs)
else:
    confargs=list()
    more_launch_vars=''
logger.info('MORE_LAUNCH_VARS='+repr(more_launch_vars))

# Tell the hafs.launcher to parse the remaining arguments so we can
# make the conf information:

# Generate the conf file and run the hafs.launcher's sanity checks
# that do not require a cycle:
# mslist does not contain the fakestorm id
logger.info('MSLIST: ' +repr(mslist))
if multistorm:
    (case_root,parm,infiles,stids,stid,pstid,moreopts)=hafs.launcher.multistorm_parse_args(
            mslist,args[firstarg:],logger,usage,PARMhafs=PARMhafs)
    fakestid = stid
    logger=logging.getLogger('run_hafs_'+str(fakestid))
    # stids list includes all storm ids AND the fake storm id.
    conf = hafs.launcher.launch(infiles,None,fakestid,moreopts[stids.index(pstid)],
                                          case_root,init_dirs=False,
                                          prelaunch=hafs.launcher.prelaunch,
                                          fakestorm=True)
else:
    (case_root,parm,infiles,stid,moreopt)=hafs.launcher.parse_launch_args(
            args[firstarg:],logger,usage,PARMhafs=PARMhafs)

    logger=logging.getLogger('run_hafs_'+str(stid))

    if(weak_invest is None):
        if(str(stid)[0]=='9'):
            logger.info('Invest requested, and no -w given.  Not discarding '
                        'weak Invests.')
            weak_invest=0
        else:
            logger.info('Non-Invest requested, and no -w given.  Will start '
                        'cycling off of last Invest <14m/s.')
            weak_invest=14
            # Note: this weak_invest default must match the value in
            # relocate's Stage1.run function.

    conf=hafs.launcher.launch(infiles,None,stid,moreopt,case_root,
                              init_dirs=False,prelaunch=hafs.launcher.prelaunch)

logger.info('Run sanity checks.')
try:
    conf.timeless_sanity_check(enset,logger)
except Exception as e:
    tcutil.rocoto.sanity_check_failed(logger,e)
    sys.exit(1)
logger.info("I think I'm sane.")

# Try to connect to the jlogfile:
loghere=conf.getloc('jlogfile','')
if not loghere:
    try:
        loghere=os.path.join(
            conf.getloc('CDSCRUB'),conf.getstr('config','SUBEXPT'),
            'log','jlogfile')
    except KeyError as ke:
        loghere=None
if loghere:
    print 'Sending jlogfile messages to %s'%(loghere,)
    produtil.log.set_jlogfile(loghere)

########################################################################
# Parse the tcvitals

def check_test_vitals(vl):
    """!This is a replacement for tcutil.storminfo.name_number_okay for
    use with TEST storms and internal stormids.  It allows through
    only the storm numbers matching stormnum, regardless of the 
    storm name (usually TEST and UNKNOWN would be dropped)."""
    logger.info('Keeping only storm number %s in vitals'%(stid,))
    for vital in vl:
        if vital.stormid3.upper()==stid.upper():
            yield vital

if parse_tcvitals:
    logger.info('Getting list of tcvitals files.')
    syndatdir=conf.getdir('syndat')
    vitpattern=conf.getstr('config','vitpattern','syndat_tcvitals.%Y')
    fileset=set()
    for cycle in cycleset:
        when=to_datetime(cycle)
        vitfile=os.path.join(syndatdir,when.strftime(vitpattern))
        fileset.add(vitfile)
    revit=tcutil.revital.Revital(logger=logger)
    logger.info('List of files to scan: '+(','.join(fileset)))
    revit.readfiles(fileset,raise_all=False)
    if renumber:
        logger.info('Renumber invest cycles.')
        if weak_invest is not None:
            revit.renumber(threshold=int(weak_invest))
        else:
            revit.renumber()
    elif stid[0]=='8':
        logger.info('Fake stormid requested.  Running limited clean-up.')
        revit.clean_up_vitals(name_number_checker=check_test_vitals)
    else:
        logger.info('Not renumbering invest cycles.  Will just clean.')
        revit.clean_up_vitals()

    tcvset = set()
    if mslist:
        for ms_id in mslist:
            tcvset.update([ vit.when.strftime('%Y%m%d%H') for vit in revit.each(ms_id) ])
    else:
        tcvset.update([ vit.when.strftime('%Y%m%d%H') for vit in revit.each(stid) ])
    notok = cycleset - tcvset
    okset = cycleset - notok
    if not multistorm or mslist:
        cycleset=okset

    listed=list(notok)
    listed.sort()
    logger.debug('NOTOK = '+( ','.join(listed) ))

    listed=list(cycleset)
    listed.sort()

    if not listed:
        produtil.log.jlogger.info(
            '%s %s: no cycles to run.  Exiting.'
            %(str(stid),' ' + repr(dateargs)))
        sys.exit(0)
    else:
        logger.info('I will ONLY run these cycles, since they have vitals:'
                    +(','.join(listed)))

########################################################################
# Create the list of variables to send to the ATParser

VARS=dict(os.environ)
if cycleset:
    VARS['CYCLE_LIST']=tcutil.rocoto.cycles_as_entity(cycleset)
    for line in VARS['CYCLE_LIST'].splitlines():
        logger.info('Rocoto cycles: %s'%(line.rstrip(),))
    cyclelist=list(cycleset)
    cyclelist.sort()
    firstcycle=to_datetime(cyclelist[0])
    cycledesc=firstcycle.strftime('%Y%m%d%H')
else:
    assert(isinstance(benchmarkset,basestring))
    year=int(benchmarkset[5:])
    number=sid[0:2]
    basin1=sid[2].upper()
    (ibasin2,basin2,basin1,longinfo) = \
        tcutil.storminfo.expand_basin(basin1)
    cycledesc='&%s%s%s;'%(basin2,number,year)
    VARS['CYCLE_LIST']=cycledesc

if multistorm:
    VARS.update(MULTISTORM='YES',
                RENUM=renumber_opt,
                BASINS=basins_opt,
                MULTISTORM_SIDS=storms_opt,
                FAKE_SID=fakestid.upper())
else:
    VARS.update(MULTISTORM='NO')

try:
    stormlabel=conf.get('config','stormlabel','storm1')
except KeyError:
    stormlabel='storm1'

def yesno(b):
    return 'YES' if(b) else 'NO'

VARS.update(SID=stid.upper(),  stormlabel=str(stormlabel),
            WHERE_AM_I=conf.get('holdvars','WHERE_AM_I'),
            WHICH_JET=conf.get('holdvars','WHICH_JET','none'),
            MORE_LAUNCH_VARS=more_launch_vars,
            CASE_ROOT=case_root,
            SITE_FILE=site_file,
            FETCH_INPUT=yesno(conf.get('config','input_catalog')=='hafsdata'),
            ARCHIVE_FV3OUT=yesno(conf.getraw('archive','fv3out','')),
            )

for (key,val) in conf.items('rocotostr'):
    VARS[key]=str(val)
for (key,val) in conf.items('rocotobool'):
    VARS[key]=yesno(conf.getbool('rocotobool',key))

bad=False
for k,v in VARS.iteritems():
    if not isinstance(v,basestring):
        logger.error('%s: value is not a string.  '
                     'It is type %s with value %s'%(
                str(k),type(v).__name__,repr(v)))
        bad=True
if bad: sys.exit(1)

########################################################################
# Order the ATParser to create the XML file.

rocotoxml=StringIO.StringIO()
parser=produtil.atparse.ATParser(rocotoxml,varhash=VARS,logger=logger)
if multistorm:
    parser.parse_file('hafs_multistorm_workflow.xml.in')
else:
    parser.parse_file('hafs_workflow.xml.in')


outbase='hafs-%s-%s-%s'%(
    conf.get('config','SUBEXPT'),
    stid.upper(),
    cycledesc)

if not outxml: outxml=okpath(outbase+'.xml')
if not outdb: outdb=okpath(outbase+'.db')
havexml=isnonempty(outxml)

if havexml:
    if not force and \
          not ask('ALERT! %s: XML file exists.  Overwrite (y/n)?'%(outxml,)):
        logger.error('%s: file exists, user does not want to overwrite.'
                     %(outxml,))
        sys.exit(1)
    else:
        logger.warning('%s: overwriting pre-existing XML file.'%(outxml,))

havedb=isnonempty(outdb)
deletedb=False
if havedb:
    if force or ask(
          'ALERT! %s: database for old configuration exists.  Use it (y/n)?'
          %(outdb,)):
        logger.warning('%s: not deleting database for old configuration.'
                       %(outdb,))
    elif ask('%s: Delete database for old configuration (y/n)?'%(outdb,)):
        logger.warning('%s: deleting database for old configuration.'
                       %(outdb,))
        remove_file(outdb)
    else:
        logger.error('%s: database exists, user does not want to delete '
                     'or use it.  Aborting.')
        sys.exit(2)

with open(outxml,'wt') as outf:
    outf.write(rocotoxml.getvalue())

########################################################################
# Run rocotorun

clustername=produtil.cluster.name()

if clustername in ('tide','gyre'):
    WHERE_AM_I='wcoss'
elif clustername in ('luna','surge'):
    WHERE_AM_I='wcoss_cray'
elif clustername in ('mars','venus'):
    WHERE_AM_I='wcoss_dell_p3'
else:
    WHERE_AM_I=clustername

#   '--login', '-c', '. %s/machine-setup.sh ; which ruby ; which rocotorun ; rocotorun --verbose=5 -d %s -w %s'
#   %( shbackslash(USHhafs), shbackslash(outdb), 
cmd = exe('sh') [
    '--login', '-c', '. %s/hafs_pre_job.sh.inc; which ruby ; which rocotorun ; rocotorun --verbose=5 -d %s -w %s'
    %( shbackslash(USHhafs), shbackslash(outdb), 
       shbackslash(outxml) ) ] .env(QUIET_PRE_JOB='YES',
                                    HOMEhafs=HOMEhafs,
                                    WHERE_AM_I=WHERE_AM_I) \
                      < '/dev/null'
result=run(cmd,logger=logger)

if result:
    sys.exit(result)
    produtil.jlogger.critical('rocotorun failed')

produtil.log.postmsg('Successfully ran rocotorun for %s.'%(outbase,))
bakdb=outdb+'.bak'
logger.info('Making a backup copy of .db file here: %s'%(bakdb,))
produtil.fileop.deliver_file(outdb,bakdb)
logger.info('Success. Rejoice: hurrah!')
##@endcond
