

##@namespace hwrf_expt
# Generates the object structure of the HWRF system for use by the scripts.
#
# @anchor hwrf_expt_overview 
#
# This module is the part of the \ref experiment_layer that creates an
# object structure from classes in the hwrf and pom packages.  It
# defines what the HWRF experiment will run, and how the various parts
# of the HWRF system connect to one another.  As this module has
# gotten more complex, some of its implementation has been moved to
# the hwrf.hwrfsystem module, which contains helper functions for
# defining parts of the workflow.
#
# @sa hwrf.launcher The launcher module creates the initial directory
# structure, and runs sanity checks on the hwrf_expt
#
# @sa scripts The scripts load this hwrf_expt module and call the
# run() (or similar) function for one or more objects within.
# 
# @sa hwrf The hwrf package contains classes that know how to run each
# part of the HWRF system.
#
# @sa pom The pom package defines the initialization of the MPIPOMTC
# ocean model.
#
# @sa hwrf_alerts The hwrf_alerts module adds product alerts needed to
# ensure data is delivered to forecasters and the public in the
# NCEP production system.

##@var __all__
# Ensures that accidental "from hwrf_expt import *" does nothing.
__all__=[]

import os,os.path

import pdb
import produtil.datastore, produtil.run

import hwrf.config, hwrf.wrf, hwrf.post, hwrf.numerics, hwrf.launcher
import hwrf.regrib, hwrf.gribtask, hwrf.tracker, hwrf.storminfo
import hwrf.wps, hwrf.nhc_products, hwrf.copywrf, hwrf.fcsttask, hwrf.ensda
import hwrf.relocate, hwrf.init, hwrf.prep, hwrf.gsi
import hwrf.bufrprep, hwrf.gsipost, hwrf.prelaunch, hwrf.multistorm
import hwrf.coupling
import hwrf.hwrfsystem, hwrf.finalmergetask, hwrf.hycom, hwrf.ww3

from produtil.run import exe,alias
from produtil.fileop import isnonempty
from hwrf.wrf import WRFDomain,WRFSimulation,ExternalWRFTask
from hwrf.post import PostManyWRF
from hwrf.regrib import RegribMany,igrb1,clatlon,GRIB2,SATGRIB2

def prelaunch(conf,logger,cycle):
    """!This function makes per-cycle modifications to the
    configuration file storm1.conf.  

    This is called in scripts.exhwrf_launch and run_hwrf.py by
    hwrf.launcher.launch() on the configuration object
    (hwrf.launcher.HWRFLauncher, a subclass of
    hwrf.config.HWRFConfig), before the per-cycle storm1.conf
    configuration file is written.  Any changes made to the conf
    object will be stored in storm1.conf file and used in later jobs.
    This allows modifications to the configuration on a per-cycle
    basis.  Note that cycle=None and conf.cycle is unavailable when
    run_hwrf.py calls prelaunch.
    @param conf the hwrf.launcher.HWRFLauncher to modify
    @param logger a logging.Logger for log messages
    @param cycle the cycle to run, or None if this is being
       run from run_hwrf.py or the ush.psychoanalyst"""

    # Prelaunch that happens even when running run_hwrf.py:
    hwrf.prelaunch.prelaunch_ensid(conf,logger)

    if cycle is None: return

    # Prelaunch that only happens in exhwrf_launch, not run_hwrf.py:
    hwrf.prelaunch.prelaunch_ungrib(conf,logger,cycle)
    hwrf.prelaunch.prelaunch_rsmc(conf,logger,cycle)
    hwrf.prelaunch.prelaunch_basin(conf,logger,cycle)

def sanity_check(logger):
    """!Runs a sanity check on this module's contents.  This should be
    called after init_module.  

    This sanity check routine is called automatically by
    hwrf.launcher.HWRFLauncher.sanity_check_expt() as part of the
    standard sanity checks in the scripts.exhwrf_launch job.  It
    checks to see if all expected module-scope variables are present
    and initialized correctly.

    @param logger a logging.Logger for log messages"""
    logger.info('The runwrf object = %s'%(repr(runwrf),))
    logger.info('The gribber object = %s'%(repr(gribber),))
    logger.info('The datastore (ds) object = %s'%(repr(ds),))
    logger.info('The wrf object = %s'%(repr(wrf),))

    # Make sure the mandatory configuration variables are specified:
    gsi_flag=conf.getbool('config','run_gsi') 
    ocean_flag=conf.getbool('config','run_ocean')
    ocean=conf.getstr('config','ocean_model')
    wave_flag=conf.getbool('config','run_wave')
    wave=conf.getstr('config','wave_model')
    reloc_flag=conf.getbool('config','run_relocation')
    spectral_flag=conf.getbool('config','use_spectral')
    spectral_bdy=conf.getbool('config','spectral_bdy') and spectral_flag
    fallbacks_flag=conf.getbool('config','allow_fallbacks')
    extra_trackers=conf.getbool('config','extra_trackers',False)
    wrf_output_step=conf.getint('forecast_products','wrf_output_step',10800)

    # Check if the ocean model is a valid one and its init object exists:
    if ocean_flag:
        if ocean=='HYCOM':
            logger.info('The hycominit object = %s'%(repr(hycominit),))
            logger.info('The hycompost object = %s'%(repr(hycompost),))
        elif ocean=='POM':
            logger.info('The pominit object = %s'%(repr(pominit),))
        else:
            raise hwrf.exceptions.HWRFConfigUnsupported(
                '[config] ocean_model=%s but should be POM or HYCOM'%(
                    ocean))

    if wrf_output_step<1:
        raise hwrf.exceptions.PrecisionTooHigh(
            'The wrf_output_step must be at least 1 second')
    elif wrf_output_step!=10800 and 0 != (3600%wrf_output_step):
        raise hwrf.exceptions.TimestepModularityError(
            'One hour (3600 seconds) must be divisable by the WRF '
            'output timestep.  You specified %d.'%(wrf_output_step,))

    # Check if the wave model is a valid one and its init object exists:
    if wave_flag:
        if wave=='WW3':
            logger.info('The ww3init object = %s'%(repr(ww3init),))
            logger.info('The ww3post object = %s'%(repr(ww3post),))
        else:
            raise hwrf.exceptions.HWRFConfigUnsupported(
                '[config] wave_model=%s but should be WW3'%(
                    wave))

    # Make sure the configuration makes sense and is supported:
    if gsi_flag and not reloc_flag:
        logger.error("Cannot use GSI without relocation.")
        raise hwrf.exceptions.HWRFConfigUnsupported(
            "Cannot use GSI without relocation.")

def inputiter():
    """!Iterates over all inputs required by this configuration.  

    Calls the inputiter() function on all tasks in the module scope
    that are expected to have input data.  The result can be passed
    into the "data" argument of hwrf.input.InputSource.get.  Iterates
    over dicts that contain the following:
    * dataset --- string name of the dataset (gfs, gdas1, gefs,
        enkf, etc.)
    * item --- string name of the object (ie.: gfs_sf, gfs_sfcanl, bufr)
    * atime --- self.conf.cycle
    * ftime --- only present when relevant: the forecast time, in a 
        format accepted by to_datetime_rel
    * enkfmem --- only present when relevant: the ENKF member ID
    * obstype --- only present when relevant: the bufr data type.
    * optional --- True if the absence of this data is not considered a failure."""  
    gsi_flag=conf.getbool('config','run_gsi') 
    run_ensemble_da=conf.getbool('config','run_ensemble_da',False)
    spectral_flag=conf.getbool('config','use_spectral')
    spectral_bdy=conf.getbool('config','spectral_bdy') and spectral_flag
    ocean_flag=conf.getbool('config','run_ocean')
    ocean=conf.getstr('config','ocean_model','POM')
    wave_flag=conf.getbool('config','run_wave')
    wave=conf.getstr('config','wave_model','WW3')
    conditional_gsid03=conf.getbool('config','conditional_gsid03',False) 
    tdrflagfile=conf.strinterp('dir','{com}/{stormlabel}.tdr')
    realtime=conf.getbool('config','realtime')
    if not conditional_gsid03 or (conditional_gsid03 and isnonempty(tdrflagfile)):
        gsid03_flag=True
    else:
        gsid03_flag=False

    if ocean_flag:
        if ocean=='HYCOM':
            for d in hycominit.inputiter(): yield d
        elif ocean=='POM':
            for d in pominit.inputiter(): yield d
    if wave_flag:
        if wave=='WW3':
            for d in ww3init.inputiter(): yield d
    for d in gfs_init.inputiter(): yield d
    if gsi_flag:
        for d in gsi_d02.inputiter(): yield d
        if gsid03_flag:
            for d in gsi_d03.inputiter(): yield d
        for d in fgat_init.inputiter(): yield d
        if run_ensemble_da:
            for d in ensda.inputiter(): yield d
            if not realtime or not os.path.isdir('/dcom/us007003'): 
                for d in ensda_pre.inputiter(): yield d

##@var ww3init
# An hwrf.ww3.WW3Init that initializes wavewatch3, or None if wave
# coupling is disabled
ww3init=None

##@var hycominit
# An hwrf.hycom.HYCOMInit that initializes hycom, or None if hycom
# coupling is not in use.
hycominit=None

##@var ww3post
# An hwrf.ww3.WW3Post that post-processes wavewatch3 output, or None
# if wave coupling is disabled.
ww3post=None

##@var hycompost
# An hwrf.hycom.HYCOMPost that post-processes hycom output, or None if
# hycom coupling is not in use
hycompost=None
                
##@var conf
# An hwrf.launcher.HWRFLauncher for configuration information.
conf=None

##@var ds 
# A produtil.datstore.Datastore for product and task information storage.
ds=None

##@var moad 
# The hwrf.wrf.WRFDomain for the Mother Of All Domains (MOAD)
moad=None

##@var storm1outer
# The hwrf.wrf.WRFDomain for the intermediate resolution domain
storm1outer=None

##@var storm1inner
# The hwrf.wrf.WRFDomain for the innermost resolution domain
storm1inner=None

##@var wrf
# The hwrf.wrf.WRFSimulation describing the simulation to run
wrf=None

##@var runwrf
# The hwrf.fcsttask.WRFAtmos or hwrf.mpipomtc.WRFCoupledPOM that
# runs the full-length forecast.
runwrf=None

##@var nonsatpost
# The hwrf.post.PostManyWRF for generating non-satellite, native E grid GRIB1 files
# from the runwrf object.
nonsatpost=None

##@var satpost 
# The hwrf.post.PostManyWRF for generating synthetic satellite, native
# E grid GRIB1 files from the runwrf object.
satpost=None

##@var gfs_init
# The hwrf.init.HWRFInit that runs initialization tasks such as WPS
# and real_nmm on the parent global model analysis.
gfs_init=None

##@var gribber
# The hwrf.gribtask.GRIBTask that takes satpost and nonsatpost output,
# regrids it and converts to GRIB2 format.
gribber=None

##@var tracker
# The hwrf.tracker.TrackerTask that runs the tracker on output from the gribber.
tracker=None

##@var pominit
# The initialization for the MPIPOMTC ocean model that is coupled to
# WRF in the runwrf object.  This is an hwrf.mpipomtc.POMInit object.
pominit=None

##@var nhcp
# The hwrf.nhc_products.NHCProducts object that generates custom
# products for the National Hurricane Center.
nhcp=None

##@var wrfcopier
# An object that copies WRF inputs and outputs to the COM directory.
# This is an hwrf.copywrf.WRFCopyTask object.
wrfcopier=None

##@var WORKhwrf
# The scrub directory for this job
WORKhwrf=None

##@var HOMEhwrf
# The HWRF installation location
HOMEhwrf=None

##@var fgat_init
# The FGAT initialization, which interpolates parent global model
# forecasts to the HWRF grid and runs other initialization jobs to
# prepare inputs to the GSI.  This is an hwrf.init.FGATInit object.
fgat_init=None

##@var non_ocean_basins
# A list of one-letter basin IDs that are not supported by the
# selected ocean model.  This is used by the scripts.exhwrf_check_init
# to determine if the ocean initialization incorrectly decided not to
# run the ocean.
non_ocean_basins=None

##@var bufrprep
# An hwrf.bufrprep.Bufrprep object that turns data tanks into bufr
# files for input to GSI.
bufrprep=None

##@var gsi_d02
# The intermediate resolution GSI, an hwrf.gsi.FGATGSI, which knows
# how to run the GSI data assimilation system on output from the
# fgat_init, bufrprep and other objects.
gsi_d02=None

##@var gsi_d03
# The innermost domain resolution GSI, an hwrf.gsi.FGATGSI, which
# knows how to run the GSI data assimilation system on output from the
# fgat_init, bufrprep and other objects.
gsi_d03=None

##@var gsid03_flag
# A boolean value, True if gsi_d03 should be run and False otherwise
gsid03_flag=None

##@var gdas_merge
# An hwrf.relocate.Merge object that merges output from GSI and the
# fgat_init's hwrf.relocate.Stage3 to create the final input fields
# to the runwrf
gdas_merge=None

##@var cycle
# A synonym of conf.cycle: the cycle being run
cycle=None

##@var ensda
# An hwrf.ensda.DAEnsemble filled with hwrf.ensda.FromGFSENKF objects
# that run six hour forecasts based on the GFS ENKF
ensda=None

##@var prior_ensda
# An hwrf.ensda.DAEnsemble filled with hwrf.ensda.FromPriorCycle objects
# that provide access to the prior cycle's ENSDA six hour forecasts.
prior_ensda=None

##@var entest
# An hwrf.ensda.FromGFSENKF for testing
#@bug the hwrf_expt.entest is no longer needed and should be removed
entest=None

##@var da_ensemble_size
# Number of members in the ensda
da_ensemble_size=None

##@var trackerd01
# If enabled, an hwrf.tracker.TrackerTask that only uses the moad data
# to generate the track.
trackerd01=None

##@var trackerd02
# If enabled, an hwrf.tracker.TrackerTask that only uses the moad and
# intermediate resolution domains' data to generate the track.
trackerd02=None

##@var gsipost 
# An hwrf.gsipost.GSIPost that post-processes inputs and outputs to the
# gsi_d02 and gsi_d03 to create native E grid GRIB files input to 
# the gsigribber
gsipost=None

##@var gsigribber
# An hwrf.gribtask.GRIBTask that takes the gsipost output and turns it
# into lat-lon GRIB2 files suitable for analysis of the effect of GSI.
gsigribber=None

##@var ensda_pre
# Output from hwrf.ensda.ensda_pre_object_for(), the object that determines
# whether the data assimiltation ensemble should be run.
ensda_pre=None

def init_module(CONFhwrf=None,make_ensemble_da=True,make_post=True):
    """!Initializes the HWRF object structure.

    This function is called from all scripts and the
    hwrf.launcher.HWRFLauncher to initialize the HWRF object
    structure.  The created objects know how to run various parts of
    the system, and this function connects those objects to one
    another.
    @param CONFhwrf the configuration filename
    @param make_ensemble_da if True, the ensemble data assimilation objects
      are created
    @param make_post if True, the post-processing objects are created"""
    global conf,ds,moad,storm1outer,storm1inner,wrf, anl4trak
    global runwrf,nonsatpost,satpost,hwrfsub,stormloc,domloc, gfs_init
    global gribber, tracker, WRFOUThwrf, coupled, pominit
    global nhcp, wrfcopier, real12, real126, WORKhwrf, HOMEhwrf
    global rel_stage1, rel_stage2, rel_stage3, fgat_init, non_ocean_basins
    global bufrprep, gsi_d02, gsi_d03, gsid03_flag, gdas_merge, cycle, prior_ensda
    global ensda, entest, da_ensemble_size, trackerd01, trackerd02
    global gsigribber, gsipost, ensda_pre, ocstatus, finalmerge, multistormin
    global run_multistorm, run_multistorm_00flag
    global fcstlen, hycominit, hycompost, ww3init, ww3post, wvstatus

    # Figure out where to find the config file if it was not supplied
    # as an argument:
    if CONFhwrf is None:
        CONFhwrf=os.environ['CONFhwrf']
    
    # Generate the HWRFConfig object, which stores experiment
    # configuration, processor counts and so on.  It also knows how to
    # automatically generate other critical objects like the Datastore:
    conf=hwrf.launcher.load(CONFhwrf)
    logger=conf.log()
    logger.info('Initializing hwrf_expt module...')

    # MULTISTORM 
    # run_multistorm_00flag identifies the fakestorm of a multistorm.
    run_multistorm=conf.getbool('config','run_multistorm',False)
    run_multistorm_00flag = False
    if run_multistorm:
        fakestormid=conf.getstr('config','fakestormid','nofakeid')
        this_stormid=conf.getstr('config','STID','nosid')
        if fakestormid != 'nofakeid' and fakestormid == this_stormid:
            run_multistorm_00flag = True
        #list of all the real storms.
        multistorm_sids = conf.getstr(
                      'config','multistorm_sids','nosids').split()
        if multistorm_sids[0]=='nosids':
            raise hwrf.exceptions.HWRFConfigInsane(
                                  'No sids  provided for multistorm run.')
        num_realstorms = len(multistorm_sids)

    # Convenience variables:
    cycle=conf.cycle
    WORKhwrf=conf.getdir('WORKhwrf')
    HOMEhwrf=conf.getdir('HOMEhwrf')

    # Major configuration variables:
    gsi_flag=conf.getbool('config','run_gsi') 
    run_ensemble_da=conf.getbool('config','run_ensemble_da',False)
    make_ensemble_da=make_ensemble_da and run_ensemble_da
    satpost_flag=conf.getbool('config','run_satpost',True)
    ocean_flag=conf.getbool('config','run_ocean')
    ocean=conf.getstr('config','ocean_model','POM')
    wave_flag=conf.getbool('config','run_wave')
    wave=conf.getstr('config','wave_model','WW3')
    reloc_flag=conf.getbool('config','run_relocation')
    spectral_flag=conf.getbool('config','use_spectral')
    spectral_bdy=conf.getbool('config','spectral_bdy') and spectral_flag
    fallbacks_flag=conf.getbool('config','allow_fallbacks')
    gofile_flag=conf.getbool('config','make_gofile',False)
    extra_trackers=conf.getbool('config','extra_trackers',False)
    conditional_gsid03=conf.getbool('config','conditional_gsid03',False)
    conditional_gsid02=conf.getbool('config','conditional_gsid02',False)
    fcstlen=conf.getint('config','forecast_length',126)
    fgatstr=conf.getint('fgat','FGATSTR',-3)
    fgatend=conf.getint('fgat','FGATEND',3)
    fgatinv=conf.getint('fgat','FGATINV',3)
    tdrflagfile=conf.strinterp('dir','{com}/{stormlabel}.tdr')
    ocstatus = hwrf.coupling.CouplingStatus(conf,'ocstatus')
    wvstatus = hwrf.coupling.CouplingStatus(conf,'wvstatus')

    ww3_output_step=conf.getint('forecast_products','ww3_output_step',21600)
    ww3_pntout_step=conf.getint('forecast_products','ww3_pntout_step',21600)
    ww3_restart_step=conf.getint('forecast_products','ww3_restart_step',21600)

    ens_reloc_flag=conf.getbool('config','run_ens_relocation',False)
    if not conditional_gsid03 or (conditional_gsid03 and isnonempty(tdrflagfile)):
        gsid03_flag=True
    else:
        gsid03_flag=False

    # ----------------------------------------------------------------------
    # Define the HWRF workflow

    # Create and obtain the Datastore object, which stores the HWRF state
    # information:
    ds=conf.datastore

    # Create the objects in this module in a single transaction since that
    # speeds things up by a factor of 100:
    with ds.transaction():
        # Known types of domains:
        moad=WRFDomain(conf,'moad')
        storm1outer=WRFDomain(conf,'storm1outer')
        storm1inner=WRFDomain(conf,'storm1inner')
        storm1ghost_parent=WRFDomain(conf,'storm1ghost_parent')
        storm1ghost=WRFDomain(conf,'storm1ghost')
        storm1ghost_parent_big=WRFDomain(conf,'storm1ghost_parent_big')
        storm1ghost_big=WRFDomain(conf,'storm1ghost_big')
        postdoms=[moad,storm1outer,storm1inner]

        wrf=WRFSimulation(conf,'wrf',moad,conf.cycle,
                          conf.cycle+hwrf.numerics.to_timedelta(fcstlen*3600))
        wrf.add(storm1outer,moad)
        wrf.add(storm1inner,storm1outer)

        # --------------------------------------------------------------------
        # MULTISTORM
        # Only add additional domains to WRF for the fake storm (i.e. 00L) of 
        # a multistorm run. 
        # NOTE: This implementation is using  the fake storms' (i.e. 00L) conf 
        # HWRFConfig object for all storms in WRFDomain multistorm run.
        if run_multistorm_00flag:
            # Define the WRFDomains, and add the domains for storms 2 and beyond.
            stormNinner = {}; stormNouter = {}
            for i in range(2,num_realstorms + 1):
                stormNinner['storm%sinner'%i]=WRFDomain(conf,'storm%sinner'%i)
                stormNouter['storm%souter'%i]=WRFDomain(conf,'storm%souter'%i)
                wrf.add(stormNouter['storm%souter'%i], moad)
                wrf.add(stormNinner['storm%sinner'%i],stormNouter['storm%souter'%i])
            stormNinner['storm1inner']=storm1inner
            stormNouter['storm1outer']=storm1outer
        # ---------------------------------------------------------------------
          
        wrf.analysis_in(None)
        
        wrf_output_step=conf.getint('forecast_products','wrf_output_step',10800)
        pom_output_step=conf.getint('forecast_products','pom_output_step',86400)
        if wrf_output_step<10800:
            # Just output the history stream for smaller timesteps.
            wrf.add_output('history',step=wrf_output_step)
        else:
            # For the special case of three-hourly output, we still
            # need hourly for nine hours.
            wrf.add_output('history',step=3600*3,end=9*3600)
            wrf.add_output('auxhist2',step=3600,end=9*3600,
                           outname='wrfout_d<domain>_<date>')
            wrf.add_output('auxhist3',step=3600*3,
                           outname='wrfout_d<domain>_<date>')
        wrf.add_output('auxhist1',step=3600,outname='wrfdiag_d<domain>', \
                           frames_per_outfile=999,io_form=202)
        # add hifre2 file to the storm1inner nest nestlevel=2
        wrf.add_hifreq(2)
        #wrftask=ExternalWRFTask(ds,conf,'wrf',wrf,location=WRFOUThwrf)

        # Set up the wrfdoms.  This is the same as postdoms, but with
        # information filled in about grid IDs, output times, etc.:
        wrfdoms=[d for d in wrf]

        # --------------------------------------------------------------------
        # MULTISTORM Final Merge processing 
        if run_multistorm_00flag:
            finalmerge=hwrf.finalmergetask.FinalMergeTask(ds,conf,\
                         'finalmerge', taskname='finalmerge')

        # ------------------------------------------------------------------
        # ATMOSPHERE PRE-PROCESSING

        wrfghost=WRFSimulation(conf,'wrf',moad,conf.cycle,
                            conf.cycle+hwrf.numerics.to_timedelta(6*3600))
        wrfghost.add(storm1ghost_parent,moad)
        wrfghost.add(storm1ghost,storm1ghost_parent)

        wrfghost_big=WRFSimulation(conf,'wrf',moad,conf.cycle,
                            conf.cycle+hwrf.numerics.to_timedelta(6*3600))
        wrfghost_big.add(storm1ghost_parent_big,moad)
        wrfghost_big.add(storm1ghost_big,storm1ghost_parent_big)

        if run_multistorm_00flag:
            # NOTE: DO NOT SET the keyword argument outdir= , for fakestorm of multistorm. 
            gfs_init=hwrf.init.HWRFInit(
                ds,conf,'gfsinit',wrf,6*3600,fcstlen*3600,ibdystep=6*3600,
                realfcst=True, relocate=False, prep=spectral_flag, prepfcst=spectral_bdy, track=False)
        else:
            gfs_init=hwrf.init.HWRFInit(
                ds,conf,'gfsinit',wrf,6*3600,fcstlen*3600,ibdystep=6*3600,
                wrfghost=wrfghost_big,prep=spectral_flag,track=True,
                realfcst=True,relocate=reloc_flag,prepfcst=spectral_bdy)

        next_cycle=conf.cycle+hwrf.numerics.to_timedelta(6*3600)
        ensda_pre = hwrf.ensda.enada_pre_object_for(
            ds,conf,'tdrcheck',next_cycle)
        if gsi_flag:
            if make_ensemble_da:
                ##################################################
                # ENSEMBLE DA

                # NOTE: If you change this section, you must also
                # change the run_hwrf.py and hwrf_workflow.xml.in to
                # match.
                ensdadom=WRFDomain(conf,'ensdadom')
                ensdawrf=WRFSimulation(
                    conf,'enswrf',moad,conf.cycle,next_cycle)
                ensdawrf.add(ensdadom,moad)
                ensdawrf.add_output('history',step=3600*3,end=9*3600)
                ensdadoms=[ ensdawrf[moad], ensdawrf[ensdadom] ]
                for dom in ensdadoms:
                    assert(dom.get_grid_id() is not None)
            
                def makememb(clazz,ienkf,topname='ensda',priorcycle=None):
                    assert(isinstance(topname,basestring))
                    assert(isinstance(ienkf,int))
                    return clazz(
                        ds,conf,"hwrf_da_ens",gfs_init,ienkf,ensdawrf,
                        "%s.%03d"%(topname,ienkf),relocate=ens_reloc_flag,
                        priorcycle=priorcycle,track=ens_reloc_flag,
                        workdir=conf.getdir('WORKhwrf')+'/%s/%03d'%(
                            topname,ienkf),
                        outdir=conf.getdir('intercom')+'/%s/%03d'%(
                            topname,ienkf))

                entest=makememb(hwrf.ensda.FromGFSENKF,30,'entest')

                prior_ensda=hwrf.ensda.DAEnsemble(
                    ds,conf,'hwrf_da_ens',conf.cycle,'prior_ensda')
                da_ensemble_size=prior_ensda.confint('ensda_size',40)
                for i in xrange(da_ensemble_size):
                    prior_ensda.set_member(
                        conf.cycle,i+1,hwrf.ensda.FromPriorCycle(
                            ds,conf,"hwrf_da_ens",ensdadoms,
                            i+1,conf.cycle,taskname='%s.%03d'%(
                                'prior_ensda',i+1)))
                
                ensda=hwrf.ensda.DAEnsemble(
                    ds,conf,'hwrf_da_ens',conf.cycle,'ensda')
                for i in xrange(da_ensemble_size):
                    if ens_reloc_flag:
                        priorcycle=prior_ensda.member(conf.cycle,i+1)
                    else:
                        priorcycle=None
                    ensda.set_member(conf.cycle,i+1,makememb(
                            hwrf.ensda.FromGFSENKF,i+1,'ensda',priorcycle))

                ##################################################
             

            if gsid03_flag:
                gsid03=True
            else:
                gsid03=None
            fgat_init=hwrf.init.FGATInit(
                ds,conf,'fgat',wrf,3*3600,fcstlen*3600,wrfghost=wrfghost,
                prep=spectral_flag,track=True,realfcst=False,ibdystep=3*3600,
                in_dataset='gdas1',relocate=reloc_flag,gsi_d02=True,
                gsi_d03=gsid03,prepfcst=spectral_bdy)
            ceninit=fgat_init.init_at_time(conf.cycle)

            bufrprep=hwrf.bufrprep.Bufrprep(
                ds,conf,'bufrprep',taskname='bufrprep')

            ingsi_d02=fgat_init.get_relocates(storm1ghost_parent)
            gsi_d02=hwrf.gsi.FGATGSI(
                ds,conf,'gsi_d02',storm1ghost_parent,
                ceninit.rstage3.get_ghost(storm1ghost_parent),
                ingsi_d02,wrfghost)

            if gsid03_flag:
                ingsi_d03=fgat_init.get_relocates(storm1ghost)
                gsi_d03=hwrf.gsi.FGATGSI(
                    ds,conf,'gsi_d03',storm1ghost,
                    ceninit.rstage3.get_ghost(storm1ghost),
                    ingsi_d03,wrfghost)
            else:
                gsi_d03=None


            if make_ensemble_da:
                if ens_reloc_flag:
                    gsi_d02.set_ensda(ensda,ensdadoms)
                else:
                    gsi_d02.set_ensda(prior_ensda,ensdadoms)
                if gsid03_flag:
                    if ens_reloc_flag:
                        gsi_d03.set_ensda(ensda,ensdadoms)
                    else:
                        gsi_d03.set_ensda(prior_ensda,ensdadoms)

            gdas_merge=hwrf.relocate.Merge(
                ds,conf,'merge',ceninit.rstage3,
                wrfinput=gfs_init.realinit,
                wrfanl=gfs_init.runwrfanl,
                taskname='gdas_merge',
                gsi_d02=gsi_d02, gsi_d03=gsi_d03)
                

            ges_d02=ceninit.rstage3.get_ghost(storm1ghost_parent)
            if gsid03_flag:
                ges_d03=ceninit.rstage3.get_ghost(storm1ghost)
            else:
                ges_d03=ceninit.rstage3.get_wrfanl(storm1inner)
            gdas_merge.set_ges(ges_d02,ges_d03)

            # Create the GSI post.  It will post-process the ghost
            # files from the init.wrfghost, relocation and GSI for
            # each of d02 and d03:
            hgpp=hwrf.hwrfsystem.HWRFGSIPostProcessing(
                ds,conf,'gsi_products')
            (gsipost,gsigribber)=hgpp.make_gsi_post(
                gsi_d02,gsi_d03,storm1ghost,storm1ghost_parent,ceninit,
                gsid03_flag)

        # ------------------------------------------------------------------
        # OCEAN PRE-PROCESSING
        
        # Basins where we cannot run the chosen ocean model:
        non_ocean_basins=conf.getstr('config','non_ocean_basins','Q')

        # Ocean init job:    
        if ocean_flag:
            if ocean=='HYCOM':
                hycominit=hwrf.hycom.HYCOMInit(ds,conf,'hycominit',fcstlen=fcstlen)
            elif ocean=='POM':
                pominit=hwrf.mpipomtc.POMInit(ds,conf,'pom',fcstlen=fcstlen,
                                              outstep=pom_output_step)

        # ------------------------------------------------------------------
        # WAVE PRE-PROCESSING
        
        # Basins where we cannot run the chosen wave model:
        #non_wave_basins='ECABPSQ'
        non_wave_basins='Q'

        # Wave init job:    
        if wave_flag and wave=='WW3':
            ww3init=hwrf.ww3.WW3Init(ds,conf,'ww3init',fcstlen=fcstlen,
                        outstep=ww3_output_step,pntstep=ww3_pntout_step,rststep=ww3_restart_step)
            for p in ww3init.products():
                pass # print 'WW3INIT product = ',str(p)

        # ------------------------------------------------------------------
        # FORECAST 
        # NOTE: most of the logic is moved to hwrf.hwrfsystem

        fcst_sec=conf.getstr('config','forecast_section','runwrf')
        if not run_multistorm:
            hfcst=hwrf.hwrfsystem.ForecastBase(
                conf,ds,wrf,pominit,ww3init,hycominit,ocstatus)
            hfcst.make_forecast()
            runwrf=hfcst.runwrf
            hycompost=hfcst.hycompost
            ww3post=hfcst.ww3post

        if run_multistorm_00flag:
            hfcst=hwrf.hwrfsystem.FakeStormForecast(
                conf,ds,wrf,multistorm_sids,finalmerge,
                stormNouter,stormNinner)
            hfcst.make_forecast()
            runwrf=hfcst.runwrf
        elif run_multistorm:
            # In the multi-storm configuration, real storms' runwrf
            # objects make the forecast initial state, and watch the
            # fake storm run the forecast.  We don't put this in
            # hwrf.hwrfsystem because the code required would be
            # longer than making runwrf here:
            runwrf=hwrf.multistorm.ForecastWatcher(
                ds,conf,fakestormid,fcst_sec,wrf,'runwrf')
            multistormin=hwrf.multistorm.RealInit(
                ds,conf,fcst_sec,wrf,keeprun=False,taskname='multistorm_init')
            realwrf=multistormin
            realwrf.add_orig_wrfinput(gfs_init.realinit)
        else:
            # Single storm runwrf:
            realwrf=runwrf

        if not run_multistorm_00flag:
            realwrf.add_metgrid(gfs_init.metgrid) \
                .add_geogrid(gfs_init.geogrid) \
                .add_fort65(gfs_init.realinit) \
                .add_wrfbdy(gfs_init.realfcst)

            # Add the merge output for the initial conditions and realfcst
            # for the boundary:
            if gsi_flag and not run_multistorm_00flag:
                realwrf.add_wrfinput(gdas_merge) \
                    .add_wrfanl(gdas_merge,storm1outer) \
                    .add_wrfanl(gdas_merge,storm1inner)

            if (not gsi_flag or fallbacks_flag
                  or (conditional_gsid03 and 
                      conditional_gsid02 and gsi_flag) 
                  ) and not run_multistorm_00flag:
                if reloc_flag or (conditional_gsid03 and \
                                  conditional_gsid02):
                    realwrf.add_wrfinput(gfs_init.rstage3) \
                        .add_wrfanl(gfs_init.rstage3,storm1outer) \
                        .add_wrfanl(gfs_init.rstage3,storm1inner)
                else:
                    # Do not fall back to "no relocation" even if
                    # fallbacks are enabled.  This is because the
                    # forecasting skill is useless without vortex
                    # relocation.
                    realwrf.add_wrfinput(gfs_init.realinit) \
                        .add_wrfanl(gfs_init.runwrfanl,storm1outer) \
                        .add_wrfanl(gfs_init.runwrfanl,storm1inner)

        ################################################################

        #  RETURN HERE IF WE ARE NOT MAKING THE POST

        if not make_post: 
            logger.info('make_post=False, will not make post.  '
                        'Done in hwrf_expt module.')
            return

        ################################################################


        # ------------------------------------------------------------------
        # POST-PROCESSING

        hfpp=hwrf.hwrfsystem.HWRFForecastPostProcessing(
            ds,conf,'forecast_products',runwrf,wrf,postdoms,
            wrfdoms,moad,storm1inner,storm1outer)
        nonsatpost=hfpp.make_nonsatpost()
        satpost=hfpp.make_satpost()
        wrfcopier=hfpp.make_wrfcopier(multistorm=run_multistorm)
        assert(wrfcopier is not None)
        (gribber,tracker,track,nhcp)=\
            hfpp.make_gribber_tracker(extra_trackers,satpost_flag,gofile_flag)
        if extra_trackers:
            (trackerd01,trackerd02)=\
                hfpp.make_extra_trackers()

    logger.info('Done in hwrf_expt module.')

