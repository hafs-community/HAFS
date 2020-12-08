"""!Creates the initial HAFS directory structure, loads information into each job.

This module is used to create the initial HAFS conf file in the
first HAFS job via the hafs.launcher.launch().  The hafs.launcher.load()
then reloads that configuration.  The launch() function does more than
just create the conf file though.  It parses the tcvitals, creates
several initial files and directories and runs a sanity check on the
whole setup.

The HAFSLauncher class is used in place of an hafs.config.HAFSConfig
throughout the HAFS system.  It can be used as a drop-in replacement
for an hafs.config.HAFSConfig, but has additional features needed to
support sanity checks, initial creation of the HAFS system and
tcvitals generation."""

##@var __all__
# All symbols exported by "from hafs.launcher import *"
__all__=['load','launch','HAFSLauncher','parse_launch_args','multistorm_parse_args']

import os, re, sys, collections, random
import produtil.fileop, produtil.run, produtil.log
import tcutil.revital, tcutil.storminfo, tcutil.numerics
import hafs.config
import hafs.prelaunch

from random import Random
from produtil.fileop import isnonempty
from produtil.run import run, exe
from produtil.log import jlogger
from tcutil.numerics import to_datetime_rel, to_datetime
from hafs.config import HAFSConfig
from hafs.exceptions import HAFSDirInsane,HAFSStormInsane,HAFSCycleInsane, \
    HAFSVariableInsane,HAFSInputInsane,HAFSScriptInsane,HAFSExecutableInsane,\
    HAFSFixInsane,HAFSArchiveInsane,HAFSConfigInsane

def multistorm_parse_args(msids, args, logger, usage, PARMhafs=None, wrapper=False):
    """This is the multistorm argument parser. It is really just a wrapper around
    parse_launch_args().

    The last Element of the returned list is the launch args for the Fake storm.

    From the original arguments, returns a new list of launch args for all
    the storms in a multistorm run. The SID and optional config.startfile
    from the original sys.argv[1:] list are replaced with a storm id and a
    config.startfile (if present) from the MULTISTORM_SIDS.
    The following multistorm conf options are also added to each storm.
    config.fakestormid=, config.multistorm_sids=,config.multistorm_priority_sid=,
    config.multistorm_sids=,  General structure of the returned list.
    [[storm1, arg1, ..argN], ..[stormN, arg1, ..argN], [storm00, arg1, ..argN]]

    INPUT:
    args -- a copy of the initial command line args, excluding sys.argv[0]
    RETURNS:
    case_root,parm,infiles,stids,fake_stid,multistorm_priority_sid,moreopts[]
    """

    # See if the optional config.startfile argument is present and get its index.
    # startfile_idx is a list of indexes in the args_multistorm list that have
    # a 'config.startfile' substring. There should only be one or none.
    # if there are none, then startfile_idx = [], an empty list.
    startfile_idx = [args.index(arg) for arg in args if 'config.startfile' in arg]

    if len(startfile_idx) > 1:
        logger.error('Exiting, More than 1 config.startfile= parameter in the argument list.')
        sys.exit(2)

    # MULTISTORM Requirement-The fakestorm will be defined as "00L".
    fake_stid = '00L'

    assert(msids is not None)

    # Best guess at priority storm id
    if not msids:
        msids=list()
        msids=[fake_stid]

    if fake_stid != msids[0]:
        multistorm_priority_sid = msids[0]
    elif len(msids) > 1:
        multistorm_priority_sid = msids[1]
    else:
        #Else, running multistorm with no storm, only the fake storm.
        multistorm_priority_sid = msids[0]

    # THIS IS Required: multistorm_all_sids is list of ALL storm ids, which
    # means it includes the fakestorm. The fakestorm sid MUST be appended
    # at the end of multistorm_all_sids list. The call in
    # exhafs_launch.py:main().fakestorm_conf=hafs.launcher.launch(
    # ... moreopts[-1]...) relies on the fake storm being the last in this list.
    # Ultimately this allows for the creation of a start file of the fakestorm,
    # in addition to all the realstorms.


    # This just makes certain the fake storm is at the end of the list.
    # Also, OK if msids has only the fakestorm in its list.
    if fake_stid in msids:
        msids.remove(fake_stid)
        msids.append(fake_stid)
        multistorm_all_sids = list(msids)
    else:
        multistorm_all_sids = list(msids)
        multistorm_all_sids.append(fake_stid)

    args.append('config.fakestormid=' + fake_stid)
    args.append('config.multistorm_priority_sid=' + multistorm_priority_sid)
    args.append('config.multistorm_sids=' + ' '.join(msids))

    logger.info('Setting up hafs to run as a multi storm with sids: %s' %(msids))
    logger.info('HAFS multistorm: The priority sid is: %s'%(multistorm_priority_sid))
    logger.info('HAFS multistorm: The multistorm fake storm id is: %s' %(fake_stid))


    # Setup arguments for each storm, as if this script was called individually for each storm.
    # Update the storm id and startfile arguments for each storm.
    # [[storm1, arg1, ..argN], ..[stormN, arg1, ..argN], [storm00, arg1, ..argN]]
    multistorms = []
    stids = []
    moreopts = []
    # Used to build the start files for a multistorm when called from the wrappers.
    # ie. if "00L." passed in, it is replace in the startfile name in the loop below
    # for each storm.
    sid_passedin = args[0]
    for i, stormid in enumerate(multistorm_all_sids):
        multistorms.append(args[:])
        multistorms[i][0] = stormid
        if startfile_idx:
            if sid_passedin in multistorms[i][startfile_idx[0]]:
                multistorms[i][startfile_idx[0]]= \
                    args[startfile_idx[0]].replace(sid_passedin,stormid)
            else:
                multistorms[i][startfile_idx[0]]= args[startfile_idx[0]] + str(stormid)

    # The code block below inserts standard hafs_multistorm conf files to
    # the existing list of ordered hafs conf files and ensures the required 
    # multistorm order of conf files. 
    idx_system_conf=None
    for i, storm_args in enumerate(multistorms):
        (case_root,parm,infiles,stid,moreopt) = \
                parse_launch_args(storm_args,logger,usage,PARMhafs)
        for idx,str in enumerate(infiles):
            if 'system.conf' in  str:
                idx_system_conf=idx
        stids.append(stid)
        moreopts.append(moreopt)
        for confbn in [ 'hafs_multistorm.conf' ]:
            confy= os.path.join(parm, confbn)
            if not os.path.exists(confy):
                logger.error(confy+': conf file does not exist.')
                sys.exit(2)
            elif not os.path.isfile(confy):
                logger.error(confy+': conf file is not a regular file.')
                sys.exit(2)
            elif not produtil.fileop.isnonempty(confy):
                logger.warning(
                    confy+': conf file is empty.  Will continue anyway.')
            logger.info('Conf input: '+repr(confy))
            if idx_system_conf:
                infiles.insert(idx_system_conf+1,confy)
                idx_system_conf += 1
            else:
                infiles.append(confy)
        logger.info('MULTISTORM Conf input ORDER:')
        for conffile in infiles:
            logger.info('Conf input: '+repr(conffile))
    return (case_root,parm,infiles,stids,fake_stid,multistorm_priority_sid,moreopts)

def multistorm_priority(args, basins, logger, usage, PARMhafs=None, prelaunch=None,renumber=True):

    storms = list()
    strcycle=args[0]
    cyc=tcutil.numerics.to_datetime(strcycle)
    YMDH=cyc.strftime('%Y%m%d%H')
    (case_root,parm,infiles,stid,moreopt) = \
            parse_launch_args(args[1:],logger,usage,PARMhafs)
    print('INFILES: ', infiles)
    conf = launch(infiles,cyc,stid,moreopt,case_root,
                  init_dirs=False,prelaunch=prelaunch,
                  fakestorm=True)
    syndatdir=conf.getdir('syndat')
    vitpattern=conf.getstr('config','vitpattern','syndat_tcvitals.%Y')
    vitfile=os.path.join(syndatdir,cyc.strftime(vitpattern))
    multistorm=conf.getbool('config','run_multistorm',False)     #ADDED BY THIAGO TO DETERMINE IF "run_multistorm=true".
    rv=tcutil.revital.Revital(logger=logger)
    rv.readfiles(vitfile, raise_all=False)
    if renumber:
        rv.renumber()
    rv.delete_invest_duplicates()
    rv.clean_up_vitals()
    rv.discard_except(lambda v: v.YMDH==YMDH)
    rv.discard_except(lambda v: v.basin1 in basins)
    # ADDED BY THIAGO: HRD's new rule for East-pac storms only.
    # EDIT - GJA - 08/13/2017: Western threshold for EPAC storms is -135
    #                          and Eastern threshold for LANT storms is -25
    #                          Temp fix so relocation does not fail
    if multistorm:
        rv.discard_except(lambda v: v.basin1!='E' or (v.basin1=='E' and v.lon>=-140))
    rv.clean_up_vitals()
    rv.sort_by_function(rv.hrd_multistorm_sorter)
    for v in rv:
        sid = v.as_tcvitals().split()[1]
        storms.append(sid)
#    if len(storms) == 0:
#        logger.info('No storms for cycle: '+cyc.strftime('%Y%m%d%H'))
#        produtil.fileop.touch(os.path.join(conf.getdir('com'),
#            'no_storms.txt'))
    return(storms)

def parse_launch_args(args,logger,usage,PARMhafs=None):
    """!Parsed arguments to scripts that launch the HAFS system.

    This is the argument parser for the exhafs_launch.py and
    hafs_driver.py scripts.  It parses the storm ID and later
    arguments (in args).  Earlier arguments are parsed by the scripts
    themselves.  If something goes wrong, this function calls
    sys.exit(1) or sys.exit(2).

    The arguments depend on if PARMhafs=None or not.

    @code{.py}
    If PARMhafs is None:
      StormID CASE_ROOT /path/to/parm [options]
    Otherwise:
      StormID CASE_ROOT [options]
    @endcode

    * StormID --- three character storm identifier (ie.: 12L for Katrina)
    * CASE_ROOT -- HISTORY or FORECAST
    * /path/to/parm - path to the parm directory, which contains the
      default conf files.

    Options:
    * section.variable=value --- set this value in this section, no matter what
    * /path/to/file.conf --- read this conf file after the default conf files.

    Later conf files override earlier ones.  The conf files read in
    are:
    * parm/hafs_input.conf
    * parm/hafs.conf
    * parm/hafs_holdvars.conf
    * parm/hafs_basic.conf
    * parm/system.conf

    @param args the script arguments, after script-specific ones are removed
    @param logger a logging.Logger for log messages
    @param usage a function called to provide a usage message
    @param PARMhafs the directory with *.conf files"""
    if len(args)<2 or ( PARMhafs is None and len(args)<3):
        usage(logger=logger)
        sys.exit(2)

    # Get the storm ID:
    stid=args[0].upper()
    if not re.match('^[0-9][0-9][ABCELPQSW]$',stid):
        logger.error('%s: invalid storm id.  Must be a three character '
                     'storm ID such as 90L or 13W'%(stid,))
        sys.exit(2)

    logger.info('Running Storm ID is '+repr(stid))

    # Get the case root (real-time vs. retrospective):
    case_root=args[1].upper()
    if case_root=='HISTORY':
        real_time=False
    elif case_root=='FORECAST':
        real_time=True
    else:
        logger.error('%s: invalid case root.  Must be HISTORY for '
                     'retrospective runs or FORECAST for real-time runs.'
                     %(case_root,))
        sys.exit(2)
    logger.info('Case root is '+repr(case_root))

    # Find the parm directory
    if PARMhafs is None:
        parm=args[2]
        if not os.path.exists(parm):
            logger.error(parm+': parm directory does not exist')
            sys.exit(2)
        elif not os.path.isdir(parm):
            logger.error(parm+': parm directory is not a directory')
            sys.exit(2)
        logger.info('Scan %d optional arguments.'%(len(args)-3))
        args=args[3:]
    else:
        parm=PARMhafs
        logger.info('Scan %d optional arguments.'%(len(args)-1))
        args=args[2:]
    parm=os.path.realpath(parm)

    # Standard conf files:
    infiles=[ os.path.join(parm,'hafs_input.conf'),
              os.path.join(parm,'hafs.conf'),
              os.path.join(parm,'hafs_holdvars.conf'),
              os.path.join(parm,'hafs_basic.conf'),
              os.path.join(parm,'system.conf')
              ]

    # Now look for any option and conf file arguments:
    bad=False
    moreopt=collections.defaultdict(dict)
    for iarg in range(len(args)):
        logger.info(args[iarg])
        m=re.match('''(?x)
          (?P<section>[a-zA-Z][a-zA-Z0-9_]*)
           \.(?P<option>[^=]+)
           =(?P<value>.*)$''',args[iarg])
        if m:
            logger.info('Set [%s] %s = %s'%(
                    m.group('section'),m.group('option'),
                    repr(m.group('value'))))
            moreopt[m.group('section')][m.group('option')]=m.group('value')
        elif os.path.exists(args[iarg]):
            logger.info('%s: read this conf file'%(args[iarg],))
            infiles.append(args[iarg])
        else:
            bad=True
            logger.error('%s: invalid argument.  Not an config option '
                         '(a.b=c) nor a conf file.'%(args[iarg],))
    if bad:
        sys.exit(2)

    for file in infiles:
        if not os.path.exists(file):
            logger.error(file+': conf file does not exist.')
            sys.exit(2)
        elif not os.path.isfile(file):
            logger.error(file+': conf file is not a regular file.')
            sys.exit(2)
        elif not produtil.fileop.isnonempty(file):
            logger.warning(
                    file+': conf file is empty.  Will continue anyway.')
        logger.info('Conf input: '+repr(file))
    return (case_root,parm,infiles,stid,moreopt)

def load(filename):
    """!Loads the HAFSLauncher created by the launch() function.

    Creates an HAFSConfig object for an HAFS workflow that was
    previously initialized by hafs.launcher.launch.  The only argument
    is the name of the config file produced by the launch command.

    @param filename The storm*.conf file created by launch()"""
    conf=HAFSLauncher()
    conf.read(filename)
    logger=conf.log()

    # Multistorm - jtf
    # run_multistorm_00flag identifies the fakestorm of a multistorm.
    run_multistorm=conf.getbool('config','run_multistorm',False)
    run_multistorm_00flag = False
    if run_multistorm:
        fakestormid=conf.getstr('config','fakestormid','nofakeid')
        if fakestormid == 'nofakeid':
            msg = "Looks like you are trying to run a multistorm but "\
                  "no fake storm id is defined. This will happen if there are  "\
                  "no real storm ids specified for a multistorm run. "\
                  "Either provide a list of storms OR Set 'run_multistorm=no' "\
                  "in hafs_basic.conf and check if you are setting the 'MULTISTORM' "\
                  "env var in either, the rocoto/runhafs_wrapper or global.vars.ksh, "\
                  "and launcher_wrapper, if running the stand alone wrappers."
            raise HAFSConfigInsane(msg)
        this_stormid=conf.getstr('config','STID','nosid')
        if fakestormid == this_stormid:
            run_multistorm_00flag = True

    cycle=conf.cycle
    assert(cycle is not None)
    strcycle=cycle.strftime('%Y%m%d%H')
    logger.info('Running cycle: '+cycle.strftime('%Y%m%d%H'))

    WORKhafs=conf.getdir('WORKhafs')

    tmpvit=os.path.join(WORKhafs,'tmpvit')
    logger.info(tmpvit+': read vitals for current cycle')
    #syndat is a StormInfo object
    with open(tmpvit,'rt') as f:
        syndat=tcutil.storminfo.parse_tcvitals(f,logger,raise_all=True)
        syndat=syndat[0]
    logger.info('Current cycle vitals: '+syndat.as_tcvitals())

    oldvit=os.path.join(WORKhafs,'oldvit')
    logger.info(oldvit+': read vitals for prior cycle')
    with open(oldvit,'rt') as f:
        oldsyndat=tcutil.storminfo.parse_tcvitals(f,logger,raise_all=True)
        oldsyndat=oldsyndat[0]
    logger.info('Prior cycle vitals: '+oldsyndat.as_tcvitals())

    conf.set_storm(syndat,oldsyndat)

    if run_multistorm_00flag:
        _load_multistorm(fakestormid,conf,logger)

    return conf

# Multistorm - jtf
def _load_multistorm(fakestormid,conf,logger):
    """Do not call this.  It is an internal implementation routine.
    It is only used internally and is called during the fakestorm of
    a multistorm run.

    Adds the additional storms of a multistorm run to the HAFSConfig
    object.
    """
    assert(conf.getbool('config','run_multistorm',False))
    multistorm_sids = conf.getstr('config','multistorm_sids').split()
    logger.info('Multistorm - fakestorm run %s: Adding storm info '
                'for storms: %s'%(fakestormid,multistorm_sids))

    WORKhafs4fake=conf.getdir('WORKhafs')

    syndat_multistorm = []
    oldsyndat_multistorm = []

    for i,stormid in enumerate(multistorm_sids):
        WORKhafs4real = WORKhafs4fake.replace(fakestormid,stormid)

        #parse_tcvitals returns a 1 element list with element[0] being the StormInfo object.
        #That is we append [0] for each storm in a multistorm below.
        tmpvit=os.path.join(WORKhafs4real,'tmpvit')
        logger.info(tmpvit+': Multistorm %s: read vitals for current cycle'%(stormid))
        with open(tmpvit,'rt') as f:
            syndat_multistorm.append(tcutil.storminfo.parse_tcvitals(f,logger,raise_all=True)[0])
        logger.info('Multistorm %s: Current cycle vitals: %s'%(
            stormid,str(syndat_multistorm[i].as_tcvitals())))

        oldvit=os.path.join(WORKhafs4real,'oldvit')
        logger.info(oldvit+': Multistorm %s: read vitals for prior cycle'%(stormid))
        with open(oldvit,'rt') as f:
            oldsyndat_multistorm.append(tcutil.storminfo.parse_tcvitals(f,logger,raise_all=True)[0])
        logger.info('Multistorm %s: Prior cycle vitals: %s'%(
            stormid,str(oldsyndat_multistorm[i].as_tcvitals())))

        # TODO: CRITICAL, go back and consider sorting or better using a dictionary.
        # Though you can determine the stormid from the StormInfo object.
        # There is no guarantee that oldsyndat and syndat are in sync 1:1 in the lists.
        # consider that rational throughout.

    conf.set_storm_multistorm(multistorm_sids,syndat_multistorm,oldsyndat_multistorm)

def make_vit_for_prelaunch(stid):
    jtwc='JTWC %s FAKEFAKE  20381212 1200 010S 0100E 180 062 1000 1011 0278 15 074 -999 -999 -999 -999 S'
    nhc='NHC  %s FAKEFAKE  20381212 1200 010N 0100W 355 082 1009 1012 0167 13 056 -999 -999 -999 -999 S -999 -999 -999 -999 -9 -99N -999W -999 -999 -999 -999 '
    jtwc_basins='ABPSWOTU'
    nhc_basins='ELQC'
    basin1uc=stid[2:].upper()
    if basin1uc in nhc_basins:
        return tcutil.storminfo.parse_tcvitals([nhc%(stid,)])
    else:
        return tcutil.storminfo.parse_tcvitals([jtwc%(stid,)])

def prelaunch(conf,logger,cycle):
    """!This function makes per-cycle modifications to the
    configuration file storm1.conf.  

    This is called in scripts.exhhafs_launch and run_hafs.py by
    hafs.launcher.launch() on the configuration object
    (hafs.launcher.HAFSlauncher, a subclass of
    hafs.config.HAFSConfig), before the per-cycle storm1.conf
    configuration file is written.  Any changes made to the conf
    object will be stored in storm1.conf file and used in later jobs.
    This allows modifications to the configuration on a per-cycle
    basis.  Note that cycle=None and conf.cycle is unavailable when
    run_hafs.py calls prelaunch.
    @param conf the hafs.launcher.HAFSLauncher to modify
    @param logger a logging.Logger for log messages
    @param cycle the cycle to run, or None if this is being
       run from run_hafs.py or the ush.psychoanalyst"""

    hafs.prelaunch.prelaunch_rsmc(conf,logger,cycle)
    hafs.prelaunch.prelaunch_basin(conf,logger,cycle)

def launch(file_list,cycle,stid,moreopt,case_root,init_dirs=True,
           prelaunch=None, fakestorm=False, fakestorm_conf=None,
           storm_num=None):
    """!Initializes the directory structure for a new HAFS workflow.

    This function runs sanity checks on the HAFS installation and the
    arguments to this function.  If a cycle is supplied, it then calls
    a prelaunch function, and then generates the configuration file
    and initial directory structure.

    You can run this function in a special mode that just reads the
    conf file, without specifying a cycle, or making directories.  To
    do that, send cycle=None and init_dirs=False.  That mode is used
    by the script that prepares the rocoto XML file for a multi-cycle
    workflow.

    @returns the full path to the conf file that is created as a
    result.  That conf file should be passed in to the load() function
    at the beginning of every job.

    @param file_list a list of conf files to read
    @param cycle the cycle to run; anything accepted by to_datetime
    @param stid the three character storm identifier for the storm to run.
      For example, stid=11L is the eleventh storm of the season in the
      Atlantic basin.  Although this argument is optional, the single
      storm HAFS workflow will fail if stid is not provided.
    @param moreopt a dict of dicts with additional options to set.  This
      maps section name to option to value.
    @param case_root HISTORY for retrospective mode, FORECAST for real-time
    @param init_dirs True if the initial directories should be created,
    @param prelaunch a function to call on the configuration before
      writing it to disk.  Takes as arguments: conf,logger,cycle
      Note that the logger or cycle may be None.  The conf is the
      configuration object that will be written.   """

    # TODO: add fakestorm description and use <jtf>
    for filename in file_list:
        if not isinstance(filename,str):
            raise TypeError('First input to hafs.config.for_initial_job '
                            'must be a list of strings.')
    conf=HAFSLauncher()
    logger=conf.log()

    logger.debug('FAKESTORM: ' +repr(fakestorm))
    logger.debug('FAKESTORM CONF: ' +repr(fakestorm_conf))
    logger.info('STORM ID: ' +repr(stid))
    logger.info('GLOBAL STORM NUM: ' +repr(storm_num))

    if cycle is not None:
        conf.cycle=to_datetime(cycle)
        logger.info('Caller wants to launch a %s run of cycle %s storm %s.'
                    %(case_root,conf.cycle.strftime('%Y%m%d%H'),stid))
    else:
        logger.info('Caller wants to launch a %s run of storm %s.'
                    %(case_root,stid))
    conf.add_section('holdvars')
    conf.set('holdvars','CASE_ROOT',case_root)
    conf.set('config','case_root',case_root)
    conf.set('config','global_storm_num',storm_num)
    if case_root=='HISTORY':
        conf.set('config','fcsthist','hist')
        conf.set('config','realtime','false')
        hist=True
    else:
        conf.set('config','fcsthist','fcst')
        conf.set('config','realtime','true')
        hist=False

    for filename in file_list:
        logger.info("%s: parse this file"%(filename,))
        conf.read(filename)

    if not hist:
        input_catalog=conf.get('config','input_catalog','hafsdata')
        if input_catalog=='hafsdata':
            fcst_catalog=conf.get('config','fcst_catalog')
            conf.set('config','input_catalog',fcst_catalog)
            jlogger.info("FORECAST mode, so changing input_catalog to %s"
                         %(repr(fcst_catalog),))

    if moreopt is not None:
        for section,options in moreopt.items():
            if not conf.has_section(section):
                conf.add_section(section)
            for option,value in options.items():
                logger.info('Override: %s.%s=%s'
                            %(section,option,repr(value)))
                conf.set(section,option,value)
    conf.guess_default_values()
    cycling_interval=conf.getfloat('config','cycling_interval',6.0)
    cycling_interval=-abs(cycling_interval*3600.0)
    if cycle is not None:
        other_cycle=to_datetime_rel(cycling_interval,conf.cycle)

    if stid is not None and cycle is not None and stid[0:2]=='00':
        fakestorm=True
    if stid is not None and cycle is not None and not fakestorm:
        revit=conf.read_tcvitals_and_messages(other_cycle=other_cycle)
        conf.gen_vitals(stid,cycling_interval,revit)
    elif stid is not None and cycle is not None and fakestorm:
        revit=conf.read_fake_tcvitals()
        conf.gen_vitals(stid,cycling_interval,revit)

    # rocoto does not initialize the dirs, it returns here.
    if not init_dirs:
        if prelaunch is not None:
            vits=make_vit_for_prelaunch(stid)
            conf.vitals=vits[0]
            prelaunch(conf,logger,cycle)
            del conf.vitals
            del vits
        return conf

    produtil.fileop.makedirs(conf.getdir('com'),logger=logger)
    produtil.fileop.makedirs(conf.getdir('WORKhafs'),logger=logger)
    #produtil.fileop.makedirs(conf.getdir('lockdir'),logger=logger)
    #griblockdir=conf.getstr('regribber','griblockdir','')
    #if griblockdir:
    #    produtil.fileop.makedirs(griblockdir,logger=logger)

    logger.info('Expand certain [dir] values to ensure availability '
                'before vitals parsing.')
    for var in ( 'WORKhafs', 'HOMEhafs', 'com' ):
        expand=conf.getstr('dir',var)
        logger.info('Replace [dir] %s with %s'%(var,expand))
        conf.set('dir',var,expand)

    if stid is not None:
        conf.decide_domain_center()
        loc=conf.getdir('domlocfile')
        logger.info('%s: Writing domain center.'%(loc,))
        with open(loc,'wt') as f:
            f.write("%g\n%g\n"%(
                    conf.getfloat('config','domlat'),
                    conf.getfloat('config','domlon')))

    if prelaunch is not None:
        prelaunch(conf,logger,cycle)

    confloc=conf.getloc('CONFhafs')
    logger.info('%s: write hafs.conf here'%(confloc,))
    with open(confloc,'wt') as f:
        conf.write(f)

    with open(os.path.join(conf.getdir('WORKhafs'),'PDY'),'wt') as f:
        f.write(conf.strinterp(
                'config','export cyc={HH}\nexport PDY={YMD}\nYMDH={YMDH}\n'))

    if fakestorm_conf:
        sfile = os.path.join(fakestorm_conf.strinterp('dir','{com}'),
                             'storm%d.conf' %storm_num)
        logger.info('%s: write STORM conf here'%(sfile,))
        with open(sfile,'wt') as f:
            conf.write(f)

    return conf

class HAFSLauncher(HAFSConfig):
    """!A replacement for the hafs.config.HAFSConfig used throughout
    the HAFS system.  You should never need to instantiate one of
    these --- the launch() and load() functions do that for you.  This
    class is the underlying implementation of most of the
    functionality described in launch() and load()"""
    def __init__(self,conf=None):
        """!Creates a new HAFSLauncher
        @param conf The configuration file."""
        super(HAFSLauncher,self).__init__(conf)
        self._cycle=None
    ##@var _cycle
    # The cycle for this HAFS forecast.

    def storm_for_stormnum(self):
        """!Not implemented.

        This is intended to return the one letter basin, numeric storm
        ID and year for the specified storm number (1-10).

        @bug The hafs.launcher.HAFSLauncher.storm_for_stormnum() is
        not implemented and should probably be removed."""
        pass;
    def decide_domain_center(self,logger=None):
        """!Decide the outermost domain's center.

        If the domain center is not already set in the [config]
        section domlat and domlon variables, decides the domain center
        using the tcutil.storminfo.StormInfo.hafs_domain_center routine.
        @param logger the logging.Logger for log messages."""
        if logger is None: logger=self.log()
        if self.has_option('config','domlat') and \
                self.has_option('config','domlon'):
            cenla=self.getfloat('config','domlat')
            cenlo=self.getfloat('config','domlon')
            logger.info('Domain center is already set to lat=%g lon=%g'
                        %(cenla,cenlo))
            return
        (cenlo, cenla) = self.syndat.tcutil_domain_center(logger)
        self.set('config','domlat',cenla)
        self.set('config','domlon',cenlo)
        logger.info('Decided on domain center lat=%g lon=%g'%(cenla,cenlo))

    def choose_vitbase(self,storm_num=None):
        """!Decides the location of the vitals file.

        Decides the location of the vitfile that should be read in by
        read_precleaned_vitfile.  Optionally, you can specify the
        storm number (1-10) of the storm whose vitals should be read
        in.  Otherwise, a reasonable guess will be made.
        @param storm_num the index of the storm from 1-10
        @returns the vitals path"""
        if storm_num is not None:
            storm_num=int(storm_num)
            vitfile=os.path.join(self.getdir('WORKhafs'),
                'storm%d.vitals'%(storm_num,))
        else:
            stormlabel=self.getstr('config','stormlabel','storm1')
            vitfile=os.path.join(self.getdir('WORKhafs'),
                '%s.vitals'%(stormlabel,))
        return vitfile

    # This was created for the hafs multistorm basin scale implementation.
    # Needed so hafs could be run with no storms and also to
    # more easily setup the fake storm directories and other config
    # parameters dependent on having a vitals dictionary.
    def read_fake_tcvitals(self, fakestorm_vitals=None):
        """ Intended use is for the multistorm fake storm. Same as the
        read_tcvitals_and_messages method except the vitals are
        from fakestorm_vitals in hafs_multistorm.conf. basd on the arguments."""

        logger=self.log()
        inputs=list()

        default_fakestorm_vitals = 'NHC  00L FAKE      ' +\
                                    self._cycle.strftime('%Y%m%d %H%M') +\
                                    ' 250N 800W -99 -99 -999 -999 -099 -9 -99 -999 -999 -999 -999 M'

        if fakestorm_vitals is None:
            fakestorm_vitals=self.getstr('config','fakestorm_vitals',default_fakestorm_vitals)

        if fakestorm_vitals == default_fakestorm_vitals:
            logger.info('Using default fakestorm vitals: %s'%(default_fakestorm_vitals))
        inputs.append(fakestorm_vitals)
        revital=tcutil.revital.Revital(logger=logger)
        revital.readvitals(inputs,raise_all=False)
        return revital

    def read_tcvitals_and_messages(self,vitdir=None,vitpattern=None,
            include_messages=True,other_cycle=None):
        """!Reads in the tcvitals file and message files.

        Reads in the tcvitals files for the current cycle and
        optionally another cycle, which may be in the same file.  Also
        reads in message files if requested.  Cleans the result up and
        returns it as an tcutil.revital.Revital object.

        @param vitdir optional: the directory in which to find the tcvitals.
              Default: [dir] section syndat variable.

        @param vitpattern optional: passed into strftime to generate the
              name of the vitals file within vitdir. Default: [conf]
              section vitpattern variable, or syndat_tcvitals.%Y if
              missing.

        @param include_messages optional flag: if True, attempts to find
              the hurricane message files, and includes them in the
              list of files to read in.  Default: True.

        @param other_cycle optional: another cycle whose vitals file
              should also be parsed.  This can be anything accepted by
              to_datetime_rel(...,self.cycle).  This is intended to
              allow year-crossing cycling, such as a January 1, 00:00
              UTC cycle that is a warm start off of a prior December
              31, 18:00 UTC cycle.  If the other_cycle's vitals file
              is the same as the one from self.cycle, then the file is
              only read once.

        @return an tcutil.revital.Revital with the vitals data"""
        ENV=os.environ
        logger=self.log()
        inputs=list()
        if vitdir is None:
            vitdir=self.getdir('syndat')
        if vitpattern is None:
            vitpattern=self.getstr('config','vitpattern',
                                   'syndat_tcvitals.%Y')
        logger.info('VITDIR: %s' %(vitdir))
        file1=os.path.join(vitdir,self._cycle.strftime(vitpattern))
        inputs.append(file1)
        if other_cycle is not None:
            other_cycle=to_datetime_rel(other_cycle,self._cycle)
            file2=os.path.join(vitdir,other_cycle.strftime(vitpattern))
            if file2!=file1:
                inputs.append(file2)

        if include_messages:
            # Try to guess the location of the message files:
            mdir=self.getdir('COMINmsg',os.environ.get('COMINmsg',''))
            if mdir is None or mdir=='':
                mdir='/you/forgot/to/set/COMINmsg/'

            # Add the messages to the input files:
            nstorms_filename=os.path.join(mdir,'nstorms')
            nstorms=7
            try:
                with open(nstorms_filename,'rt') as nstorms_file:
                    dat=nstorms_file.readline()
                    nstorms=int(dat)
            except (EnvironmentError,ValueError,TypeError) as e:
                logger.error('%s: error reading: %s.  Will read all storms.'%(
                        nstorms_filename,str(e)),exc_info=True)
            for imessage in range(nstorms):
                file=os.path.join(mdir,'message%d'%(imessage+1,))
                if os.path.exists(file):
                    inputs.append(file)

        self.log().info('read vitals from: '+','.join(inputs))
        revital=tcutil.revital.Revital(logger=logger)
        revital.readfiles(inputs,raise_all=False)
        return revital

    def set_storm(self,syndat,oldsyndat):
        """!Sets the storm that is to be run.

        Sets the syndat and oldsyndat member variables, and several
        related options in the [config] section, to the storm in the
        provided tcvitals or message file data.

        * config.STID --- The three character storm id (ie.: 12L) of
            the storm to run.
        * config.stnum --- the numeric part of config.STID
        * config.basin1 --- the basin part of STID (ie.: the L in 12L)
        * config.basin1lc --- the lower-case version of config.basin1

        @param syndat the tcutil.storminfo.StormInfo for this cycle's vitals
        @param oldsyndat the tcutil.storminfo.StormInfo for the prior cycle"""
        assert(isinstance(syndat,tcutil.storminfo.StormInfo))
        if oldsyndat is not None:
            assert(isinstance(oldsyndat,tcutil.storminfo.StormInfo))
        self.set_options('config',STID=syndat.stormid3,stnum=syndat.stnum,
                         basin1=syndat.basin1,basin1lc=syndat.basin1lc)
        self.__dict__['syndat']=syndat.copy()
        self.__dict__['storminfo']=syndat.copy()
        if oldsyndat is not None:
            self.__dict__['oldsyndat']=oldsyndat.copy()
            self.__dict__['oldstorminfo']=oldsyndat.copy()

    # Multitorm - jtf
    def set_storm_multistorm(self,multistorm_real_sids,syndat4multistorm,oldsyndat4multistorm):
        """This is meant to be an internal implementation function and
        should not be called directly. This is meant to only be used internally
        by the fakestorm of a multistorm run.

        Adds the syndat_multstorm and oldsyndat_multistorm member
        variables for the fake storm.  They contain the StormInfo objects
        for all the storm in a multistorm run from the provided tcvitals
        or message file data.

        It is ultimately used for access to each storm's lat/lon
        information in a multistorm run. This is needed for the
        swcorner calculation for all the "stormNouter" storms.
        """
        # TODO: Rethink, Is this necessary, why are we doing these .copy() <jtf>
        # Just add them to the dictionary ?
        # Didn't have time to consider this, I just mirrored the behavior
        # of def set_storm, and treated the logic as a black box.
        # Not sure why we are creating a copy of the StormInfo object, just to
        # assign it to a dictionary key.
        syndat_fromcopy = []
        oldsyndat_fromcopy = []
        # TODO: Better, rather then a list make it a dictionary with sid as the key <jtf>
        # TODO: CRITICAL, consider is syndat and old syndat always in sync 1:1, make it a dictionary <jtf>
        # TODO: CRITICAL, think thru the case where oldsyndat is None <jtf>
        for index in range(len(multistorm_real_sids)):
            assert(isinstance(syndat4multistorm[index],tcutil.storminfo.StormInfo))
            if oldsyndat4multistorm[index] is not None:
                assert(isinstance(oldsyndat4multistorm[index],tcutil.storminfo.StormInfo))
            syndat_fromcopy.append(syndat4multistorm[index].copy())

            if oldsyndat4multistorm[index] is not None:
                oldsyndat_fromcopy.append(oldsyndat4multistorm[index].copy())
            else:
                oldsyndat_fromcopy.append(None)

        self.__dict__['syndat_multistorm']=syndat_fromcopy
        self.__dict__['oldsyndat_multistorm']=oldsyndat_fromcopy

    def tcautoseed(self,loud=True):
        """!Sets the random seed for ensemble perturbations.

        Automatically decides a random seed for the tcvitals
        perturbation, based on the storm number, basin and cycle.  The
        number and basin used are before the invest renumbering
        (self.syndat.old()).

        @param loud If loud=True (the default), then a message is sent
        to the jlogfile via postmsg with the seed, and information
        about the calculation that went into it."""
        si=self.syndat.old() # storminfo before renumbering
        icycle=int(self.cycle.strftime('%Y%m%d%H'))
        istnum=int(si.stnum)
        cbasin=str(si.basin1).upper()
        ibasin=ord(cbasin)
        seed=icycle ^ istnum ^ ibasin # ^ is bitwise exclusive or (XOR)
        if loud:
            produtil.log.postmsg(
                'Automatic perturbation seed calculation: '
                '%d %d%s => seed = %d^%d^ord("%s") = %d^%d^%d = %d'%(
                    icycle,istnum,cbasin,
                    icycle,istnum,cbasin,
                    icycle,istnum,ibasin,
                    seed))
        return seed

    def gen_vitals(self,STID,cycling_interval,revital,storm_num=None):
        """!Generate tcvitals files

        Given an tcutil.revital.Revital object, preferably from
        read_precleaned_vitfile or read_tcvitals_and_messages,
        searches for the specified storm's vitals.  Creates the files
        that are expected to exist in the WORKhafs directory.  The
        filenames are based off of the vitbase variable, but with
        various suffixes appended.  This function should only be
        called once per workflow, per storm.

        @param STID the three character stormid (12L)
        @param cycling_interval seconds between HAFS cycles (6*3600)
        @param revital The tcutil.revital.Revital with tcvitals data
        @param storm_num The storm index 1-10"""
        logger=self.log()
        stnum=int(STID[0:2],10)
        STID=STID.upper()
        strcycle=self._cycle.strftime('%Y%m%d%H')
        syndat=None
        oldsyndat=None

        if cycling_interval<0:
            cycling_interval=-cycling_interval
        cycling_interval=cycling_interval/3600.0
        prior=tcutil.numerics.to_datetime_rel(-cycling_interval*3600.,self._cycle)
        strprior=prior.strftime('%Y%m%d%H')
        logger.info('gen_vitals: cycle=%s interval=%s prior=%s STID=%s'%(
                repr(self.cycle),repr(cycling_interval),repr(prior),
                repr(STID)))

        def keep_condition(vit):
            return vit.stormid3.upper()==STID or \
                ( 'old_stormid3' in vit.__dict__ and
                  vit.old_stormid3.upper()==STID )

        if stnum>=50:
            logger.info('%s: Not renumbering invests because %d>=50.'
                        %(STID,stnum))
            unrenumbered=revital.copy()
            unrenumbered.discard_except(keep_condition)
            unrenumbered.clean_up_vitals()
            renumbered=unrenumbered
        else:
            logger.info('%s: Renumber and unrenumber invests.'%(STID,))
            unrenumbered=revital.copy()
            unrenumbered.renumber(unrenumber=True)
            unrenumbered.discard_except(keep_condition)
            unrenumbered.clean_up_vitals()
            renumbered=unrenumbered.copy()
            renumbered.swap_numbers()
            renumbered.clean_up_vitals()
            unrenumbered.mirror_renumbered_vitals()
            unrenumbered.clean_up_vitals()

        # Find the current cycle's vitals:
        for vit in renumbered.each(STID):
            if vit.when==self._cycle:
                syndat=vit

        if syndat is None:
            raise tcutil.storminfo.NoSuchVitals(
                'Error: cannot find %s cycle %s'%(STID,strcycle))
        logger.info('syndat='+syndat.as_tcvitals())
        self.set_storm(syndat,None)

        # Find the prior cycle's vitals.  First pass: look for a cycle
        # whose data that actually exists on disk.
        nodatasyndat=None
        for vit in unrenumbered.each(STID,old=True):
            if vit.when!=prior: continue # wrong cycle
            if oldsyndat is not None and oldsyndat.stnum<50:
                logger.info('%s %s: not checking these vitals for data on '
                            'disk since I found a non-invest number %s '
                            'already with data on disk'
                            %(str(vit.stormid3),str(vit.YMDH),
                              str(oldsyndat.stormid3)))
            else:
                checkfile=self.timestrinterp(
                    'config','{HISTCHECK}',atime=prior,ftime=prior,
                    oldvit=vit.__dict__,vit=syndat.__dict__)
                if os.path.exists(checkfile):
                    logger.info('%s: exists'%(checkfile,))
                    logger.info('%s %s: prior is %s %s and has data on disk'%
                              (STID,strcycle,vit.stormid3,strprior))
                    oldsyndat=vit
                else:
                    logger.info('%s: does not exist'%(checkfile,))
                    logger.info('%s %s: prior could be %s %s but there is '
                                'no data on disk'%
                                (STID,strcycle,vit.stormid3,strprior))
            if oldsyndat is None:
                if nodatasyndat is not None and nodatasyndat.stnum<50:
                    logger.info('%s %s: not using as backup since I found a '
                                'non-invest number %s already'
                                %(str(vit.stormid3),str(vit.YMDH),
                                  str(nodatasyndat.stormid3)))
                else:
                    nodatasyndat=vit

        self.set('config','expect_cold_start','no')
        if oldsyndat is None:
            logger.info('%s %s: no storm IDs for prior cycle have data '
                        'on disk.'%(STID,strcycle))
            if nodatasyndat is not None:
                oldsyndat=nodatasyndat
                logger.info('%s %s: will use %s %s as prior cycle storm.'
                          %(STID,strcycle,oldsyndat.stormid3,strprior))
                logger.info('prior vitals: '+oldsyndat.as_tcvitals())
            else:
                logger.warning('No prior syndat available.  This is a cold '
                               'start.  I will extrapolate vitals.')
                oldsyndat=syndat-cycling_interval # extrapolate vitals
                logger.warning('extrapolated vitals: %s'
                               %(oldsyndat.as_tcvitals()))
            self.set('config','expect_cold_start','yes')
        else:
            logger.info('%s %s prior cycle on disk for %s %s'
                        %(STID,strcycle,oldsyndat.stormid3,strprior))
            logger.info('prior cycle on disk: '+oldsyndat.as_tcvitals())

        self.set_storm(syndat,oldsyndat)
        vitbase=self.choose_vitbase(storm_num)

        vitbasedir=os.path.dirname(vitbase)
        print("vitbasedir",vitbasedir)
        produtil.fileop.makedirs(vitbasedir,logger=logger)

        logger.info('Reformat vitals...')
        filename=vitbase+'.allids'
        logger.info(
            filename+': write unrenumbered vitals with all storm IDs')
        with open(filename,'wt') as vitalsout:
            for vit in unrenumbered.each(stormid=STID,old=True):
                print(vit.as_tcvitals(), file=vitalsout)
        filename=vitbase+'.renumberlog'
        logger.info(filename+': write renumberlog with my storm ID')
        logger.info(vitbase+': write renumbered vitals')
        with open(filename,'wt') as renumberlog:
            with open(vitbase,'wt') as vitalsout:
                renumbered.print_vitals(vitalsout,renumberlog=renumberlog,
                                     stormid=STID,format='tcvitals')
        comdir=self.getdir('com')
        produtil.fileop.makedirs(comdir,logger=logger)
        logger.info('deliver renumberlog '+filename+' to '+comdir)
        produtil.fileop.deliver_file(filename,comdir,keep=True,logger=logger)
        filename=vitbase+'.oldid'
        logger.info(filename+': write vitals with original ID')
        with open(filename,'wt') as vitalsout:
            for vit in renumbered.each(stormid=STID):
                print(vit.old().as_tcvitals(), file=vitalsout)

        filename=os.path.join(self.getdir('WORKhafs'),'tmpvit')
        logger.info(filename+': write current cycle vitals here')
        with open(filename,'wt') as tmpvit:
            print(self.syndat.as_tcvitals(), file=tmpvit)

        filename=os.path.join(self.getdir('WORKhafs'),'oldvit')
        logger.info(filename+': write prior cycle vitals here')
        with open(filename,'wt') as tmpvit:
            print(self.oldsyndat.as_tcvitals(), file=tmpvit)

    def sanity_check_archive(self,logger=None):
        """!Runs a sanity check on the archiving settings.
        @param logger a logging.Logger for log messages"""
        if not self.getbool('sanity','check_archive',True): return
        archive=self.getloc('archive','NONE')
        if archive.lower()=='none':
            if logger is not None:
                logger.info('Archiving is disabled: archive=none')
            return

        adir=os.path.dirname(archive[5:])
        missing=False
        if archive[0:5]=='hpss:' or archive[0:5]=='hpsz:':
            logger.info('Cannot hsi -P ls / so skipping archive check.')
        elif archive[0:5]=='disk:':
            if os.path.exists(adir):
                if os.path.isdir(adir):
                    logger.info('%s: disk archive directory exists and is a '
                                'directory.'%(adir,))
                else:
                    msg='%s: disk archive directory is not a '\
                        'directory '%(adir,)
                    logger.warning(msg)
                    raise HAFSArchiveInsane(msg)
            else:
                logger.info('%s: disk archive directory does not exist'
                            %(adir,))
                missing=True
        else:
            msg='%s: Invalid archive method %s'%(archive,archive[0:4])
            logger.error(msg)
            raise HAFSArchiveInsane(msg)
        if missing:
            if not self.getbool('archive','mkdir',False):
                msg='%s: archive directory is missing and [archive] mkdir '\
                    'is disabled.  Archive job would fail.  Set [config] '\
                    'archive=none to disable archiving OR set [archive] '\
                    'mkdir=yes to make archive directory or disable the '\
                    'archive sanity check with [sanity] check_archive=no'\
                    %(archive,)
                logger.warning(msg)
                raise HAFSArchiveInsane(msg)

    def timeless_sanity_check(self,enset=None,logger=None):
        """!Runs all sanity checks that are not dependent on the cycle.

        Runs any sanity checks that are possible without knowing
        the cycle that is to be run.  This is intended to be used by
        the workflow automation system (rocoto, ecflow, etc.) to make
        sure everything is functional before starting any jobs.
        @param enset a set of ensemble ids
        @param logger the logging.Logger for log messages"""

        for dirvar in ( 'HOMEhafs', 'EXEChafs', 'EXhafs', 'USHhafs',
                        'FIXhafs', 'PARMhafs' ):
            logger.debug('%s: check this dir variable'%(dirvar,))
            thedir=self.getdir(dirvar)
            self.sanity_check_directory(thedir,dirvar,False,logger)

        # Make sure the hafs.launcher exists, and is the same as this
        # one.
        checkme=os.path.join(self.getdir('USHhafs'),'hafs','launcher.py')
        myfile=os.path.realpath(__file__)
        if myfile[-4:]=='.pyc': myfile=myfile[0:-1]
        if not produtil.fileop.isnonempty(checkme):
            raise HAFSScriptInsane(
                '%s: The ush/hafs/launcher.py does not exist, which is '
                'impossible because it is running now.  Check your paths '
                'and EXPT.'%(checkme,))
        if not os.path.samefile(checkme,myfile):
            raise HAFSScriptInsane(
                '%s: not the same as the launcher.py that is running now '
                '(%s) -- check your paths and EXPT.'%(checkme,myfile))
        self.sanity_check_forecast_length(logger)

    def sanity_check_forecast_length(self,logger=None):
        """!Ensures the forecast length is valid.
        @param logger the logging.Logger for log messages"""
        iflen=self.getint('config','forecast_length',126)
        if iflen<12:
            raise HAFSConfigInsane("The forecast length must be at least "
                                   "12hrs (you specified %dhrs)"%iflen)
        if iflen%6 != 0:
            raise HAFSConfigInsane("The forecast length must divisible by "
                                   "6hrs (you specified %dhrs)"%iflen)

    def sanity_check_directory(self,thedir,dirvar,writable=True,logger=None):
        """!Runs a sanity check on the provided directory paths.

        Checks to make sure the specified directory exists and can be
        read and executed.  If writable=True, also checks to see if it
        can be written.  The dirvar is an explanation of what the
        directory relates to, for example HOMEhafs.
        @param thedir a directory to check
        @param dirvar the variable that will be set to this directory (such as PARMhafs, USHhafs, etc.)
        @param writable Do we need to write to this directory?
        @param logger the logging.Logger for log messages"""
        if logger is None: logger=self.log('sanity.checker')
        logger.info('%s: check directory %s'%(dirvar,thedir))
        if not os.path.exists(thedir):
            raise HAFSDirInsane('%s: directory does not exist: %s'
                                 %(dirvar,thedir),thedir)
        if writable:
            if not os.access(thedir,os.W_OK):
                raise HAFSDirInsane('%s: cannot write directory: %s'
                                    %(dirvar,thedir),thedir)
        if not os.access(thedir,os.R_OK):
            raise HAFSDirInsane('%s: cannot read directory: %s'
                                 %(dirvar,thedir),thedir)
        if not os.access(thedir,os.X_OK):
            raise HAFSDirInsane('%s: cannot execute directory: %s'
                                 %(dirvar,thedir),thedir)

    def sanity_check(self):
        """!Runs nearly all sanity checks.

        Runs simple sanity checks on the HAFS installation directory
        and configuration to make sure everything looks okay.  May
        throw a wide variety of exceptions if sanity checks fail."""
        logger=self.log('sanity.checker')
        for dirvar in ( 'WORKhafs', 'com' ):
            logger.info('%s: check this dir variable'%(dirvar,))
            thedir=self.getdir(dirvar)
            self.sanity_check_directory(thedir,dirvar,True,logger)

        enset=set()
        enset.add(self.get('config','ENS','99'))

        self.timeless_sanity_check(enset,logger)

        CONFhafs=self.getdir('CONFhafs')
        logger.info('Try to load configuration file %s'%(CONFhafs,))
        redo=load(CONFhafs)

        logger.info('Compare new and old vitals')
        if 'syndat' in self.__dict__ and self.syndat.stormid3 != \
                redo.syndat.stormid3:
            raise HAFSStormInsane(
                "New directory has the wrong stormid: correct=%s conf=%s"
                %(self.syndat.stormid3,redo.syndat.stormid3))
        if self.cycle!=redo.cycle:
            raise HAFSCycleInsane(
                'New directory has the wrong cycle: correct=%s conf=%s'
                %(self.cycle.strftime('%Y%m%d%H'),
                  redo.cycle.strftime('%Y%m%d%H')))

        case_root=redo.getstr('config','case_root').upper()
        input_catalog=redo.getstr('config','input_catalog')
        logger.info('Case root is %s and input catalog is %s'
                    %(repr(case_root),repr(input_catalog)))

        if case_root=='HISTORY':
            logger.info('History mode.  Will skip data checks.')
        elif case_root=='FORECAST':
            logger.info('Real-time mode.  Will skip data checks.')
        else:
            raise HAFSVariableInsane(
                'config.case_root must be HISTORY or FORECAST not %s'
                %(repr(case_root),))

        self.sanity_check_archive(logger)

    def guess_default_values(self):
        """!Tries to guess default values for many configuration settings.

        Tries to set default values for some mandatory conf options.
        The default values come from either other options or from
        environment variables.  If no suitable default can be found,
        execution will continue, but later jobs may fail.

        Config options are as follows.  If $VAR appears, that refers
        to ENV["VAR"]:

        * config.cycle --- the cycle to run as a ten digit date (2014091418)
            Taken from cycle if present, otherwise $YMDH

        * config.storm_num --- the storm number as a priority 1 to 5.
            Taken from $storm_num or uses the default of 1.

        * config.stormlabel --- "storm" with the storm number appended
            (ie.: storm5 if storm_num=5).

        * dir.HOMEhafs --- set to HOMEhafs or $HOMEhafs

        * dir.WORKhafs --- set to WORKhafs or $WORKhafs

        * dir.syndat --- tcvitals directory.  Default: $COMINarch

        * config.input_catalog --- input catalog (conf section) name.
            Default: $INPUT_CATALOG or "hafsdata"

        * config.RUN_ENVIR --- NCO if you are NCEP Central Operations
            (NCO).  This is used to turn on or off DBNet alerts and
            other NCO-specific options.

        In addition, the following directories are added to the [dir]
        section:

        * USHhafs --- the location of ush scripts and the parent
            directory of the hafs, pom and produtil packages
        * FIXhafs --- the location of the HAFS fix directory
        * JOBhafs --- the location of the HAFS jobs directory.  This is
            not needed by normal users.
        * EXhafs --- the location of the HAFS scripts directory
        * PARMhafs --- the location of the HAFS parm/ directory
        * EXEChafs --- the location of the HAFS exec/ directory
        * utilexec --- the location of the HAFS nwport/exec or
            /nwprod/exec directory

        If set, these variables will be copied to the [config] section:
        * EXPT --- optional: the experiment identifier, which must be
            alphanumeric, and can contain underscores.  Default: HAFS
        * SUBEXPT --- optional: the subexperiment identifier, which
            must be alphanumeric, and can contain underscores.
            Default: set to value of EXPT       """
        ENV=os.environ
        logger=self.log()
        PARAFLAG=( ENV.get('RUN_ENVIR','EMC').upper()!='NCO' )

        def set_default(section,option,default,env1=None,env2=None):
            if not self.has_option(section,option):
                if env1 is not None and env1 in ENV:
                    self.set(section,option,ENV[env1])
                elif env2 is not None and env2 in ENV:
                    self.set(section,option,ENV[env2])
                elif default is not None:
                    self.set(section,option,str(default))
                else:
                    logger.error(
                        'Cannot find suitable default for [%s] option %s'%(
                            section,option))

        set_default('config','case_root','HISTORY','CASE_ROOT')
        set_default('config','EXPT','HAFS','EXPT')
        set_default('config','SUBEXPT','{EXPT}','SUBEXPT')
        set_default('dir','HOMEhafs',None,'HOMEhafs')
        set_default('dir','WORKhafs',None,'WORKhafs','DATA')
        set_default('config','datastore','{WORKhafs}/hafs_state.sqlite3')
        set_default('config','storm_num','1','storm_num')
        set_default('config','stormlabel','storm{storm_num}')
        set_default('config','input_catalog','hafsdata','INPUT_CATALOG')
        set_default('dir','syndat',None,'COMINarch')
        set_default('dir','com',None,'COMOUT')
        set_default('config','RUN_ENVIR','EMC','RUN_ENVIR')

        if not self.has_option('config','cycle'):
            if 'YMDH' in ENV:
                self.cycle=ENV['YMDH']
        #cycle=self.cycle

        #if 'NWPROD' in ENV:
        #    NWPROD='NWPROD',ENV['NWPROD']
        #elif 'envir' in ENV and os.path.exists('/nw'+ENV['envir']):
        #    NWPROD='/nw'+ENV['envir']
        #else:
        #    NWPROD='{HOMEhafs}/nwport'

        def dirset(evar,deff,parent='{HOMEhafs}'):
            if evar in ENV:
                self._conf.set('dir',evar,ENV[evar])
            elif not self._conf.has_option('dir',evar):
                self._conf.set('dir',evar,parent+'/'+deff.lower())

        dirset('FIXhafs','fix')
        dirset('USHhafs','ush')
        dirset('EXhafs','scripts')
        dirset('EXEChafs','exec')
        dirset('JOBhafs','jobs')
        dirset('PARMhafs','parm')
        #dirset('utilexec','util/exec',NWPROD)

    def make_holdvars(self,part1='{PARMhafs}/hafs_holdvars.txt',part2=None):
        """!Creates the com/storm*.holdvars.txt file

        Creates the storm*.holdvars.txt file needed by the old
        ksh-based scripts.  This is done for backward compatibility
        only.  The two arguments (part1 and part2) are two files to
        pass through self.strinterp and then into the holdvars file.
        Part 1 is mandatory, but part2 is optional.  It also fills in
        a few custom derived variables:

        *  cap_run_gsi --- capitalized version of [config] section run_gsi
        *  cap_run_vortexinit --- capitalized version of [config] entry run_vortexinit
        *  cap_run_hrdgraphics -- capitalized version of [config] entry run_hrdgraphics
        @param part1 The first input file to read
        @param part2 The second input file to read or None to disable"""
        assert(isinstance(part1,str))
        out=list()
        logger=self.log()

        # Generate the output grid for the write grid component of the forecast job
        output_grid=self.getstr('forecast','output_grid','rotated_latlon') 
        logger.info('output_grid is: %s'%(output_grid))
        output_grid_cen_lon=self.getfloat('forecast','output_grid_cen_lon',-62.0) 
        output_grid_cen_lat=self.getfloat('forecast','output_grid_cen_lat',22.0) 
        output_grid_lon_span=self.getfloat('forecast','output_grid_lon_span',70.0) 
        output_grid_lat_span=self.getfloat('forecast','output_grid_lat_span',60.0) 
        output_grid_dlon=self.getfloat('forecast','output_grid_dlon',0.025) 
        output_grid_dlat=self.getfloat('forecast','output_grid_dlat',0.025) 
        if output_grid=='rotated_latlon':
            output_grid_lon1=self.getfloat('forecast','output_grid_lon1',0.0-output_grid_lon_span/2.0) 
            output_grid_lat1=self.getfloat('forecast','output_grid_lat1',0.0-output_grid_lat_span/2.0) 
            output_grid_lon2=self.getfloat('forecast','output_grid_lon2',0.0+output_grid_lon_span/2.0) 
            output_grid_lat2=self.getfloat('forecast','output_grid_lat2',0.0+output_grid_lat_span/2.0) 
        elif output_grid=='regional_latlon':
            output_grid_lon1=self.getfloat('forecast','output_grid_lon1',output_grid_cen_lon-output_grid_lon_span/2.0) 
            output_grid_lat1=self.getfloat('forecast','output_grid_lat1',output_grid_cen_lat-output_grid_lat_span/2.0) 
            output_grid_lon2=self.getfloat('forecast','output_grid_lon2',output_grid_cen_lon+output_grid_lon_span/2.0) 
            output_grid_lat2=self.getfloat('forecast','output_grid_lat2',output_grid_cen_lat+output_grid_lat_span/2.0) 
        else:
            logger.error('Exiting, output_grid: %s not supported.'%(output_grid))
            sys.exit(2)
        self.set('holdvars','output_grid_lon1',output_grid_lon1)
        self.set('holdvars','output_grid_lat1',output_grid_lat1)
        self.set('holdvars','output_grid_lon2',output_grid_lon2)
        self.set('holdvars','output_grid_lat2',output_grid_lat2)

        # Generate synop_gridspecs if needed
        synop_gridspecs=self.getstr('post','synop_gridspecs','auto') 
        # if synop_gridspecs=auto, then synop_gridspecs will be automatically generated based on the output grid
        if synop_gridspecs=='auto':
            if output_grid=='rotated_latlon':
                latlon_lon0=output_grid_cen_lon+output_grid_lon1-9.
                latlon_lat0=output_grid_cen_lat+output_grid_lat1
                latlon_dlon=output_grid_dlon
                latlon_dlat=output_grid_dlat
                latlon_nlon=(output_grid_lon2-output_grid_lon1+18.)/output_grid_dlon
                latlon_nlat=(output_grid_lat2-output_grid_lat1)/output_grid_dlat
            elif output_grid=='regional_latlon':
                latlon_lon0=output_grid_lon1
                latlon_lat0=output_grid_lat1
                latlon_dlon=output_grid_dlon
                latlon_dlat=output_grid_dlat
                latlon_nlon=(output_grid_lon2-output_grid_lon1)/output_grid_dlon
                latlon_nlat=(output_grid_lat2-output_grid_lat1)/output_grid_dlat
            logger.info('since synop_gridspecs is %s' %(synop_gridspecs))
            synop_gridspecs='"latlon %f:%d:%f %f:%d:%f"'%(
                latlon_lon0,latlon_nlon,latlon_dlon,
                latlon_lat0,latlon_nlat,latlon_dlat)
            logger.info('automatically generated synop_gridspecs: %s' %(synop_gridspecs))
        self.set('holdvars','synop_gridspecs',synop_gridspecs)

        # Set trker_gridspecs if needed
        trker_gridspecs=self.getstr('post','trker_gridspecs','auto') 
        if trker_gridspecs=='auto':
            logger.info('since trker_gridspecs is %s' %(trker_gridspecs))
            trker_gridspecs=synop_gridspecs
            logger.info('automatically generated trker_gridspecs: %s' %(trker_gridspecs))
        self.set('holdvars','trker_gridspecs',trker_gridspecs)

        gsi_flag=self.getbool('config','run_gsi')
        self.set('holdvars','cap_run_gsi',('YES' if gsi_flag else 'NO'))

        reloc_flag=self.getbool('config','run_vortexinit')
        self.set('holdvars','cap_run_vortexinit',
                 ('YES' if reloc_flag else 'NO'))

        gplot_flag=self.getbool('config','run_hrdgraphics')
        self.set('holdvars','cap_run_hrdgraphics',
                 ('YES' if gplot_flag else 'NO'))

        with open(self.strinterp('dir',part1),'rt') as f:
            for line in f:
                out.append(self.strinterp('holdvars',line.rstrip()))
        if part2 is not None:
            with open(self.strinterp(part2),'rt') as f:
                for line in f:
                    out.append(self.strinterp(line.rstrip()))
        return '\n'.join(out) + '\n'
