"""!Functions called automatically before a cycle is launched.

This module contains utility functions for the hafs.launcher.launch()
prelaunch argument.  These functions edit the configuration of an
individual cycle before the cycle starts."""

import os
import tcutil.numerics
import produtil.fileop
from produtil.log import jlogger

##@var __all__
# Symbols exported by "from hafs.prelaunch import *"
__all__=['prelaunch_rsmc','prelaunch_basin']

def prelaunch_wind(conf,logger):
    """!Disables vortexinit and GSI if the wind is below some threshold."""
    threshold=conf.getint('config','min_wind_for_init',0)
    wind=conf.syndat.wmax
    if wind<threshold:
        jlogger.info('Wind %d < %d so disabling GSI and relocation.'%(
                wind,threshold))
        logger.info('Wind %d<%d - run_gsi, run_vortexinit and run_ens_vortexinit overridden to "no"'%(
                wind,threshold))
        conf.set('config','run_gsi','no')
        conf.set('config','run_vortexinit','no')
        conf.set('config','run_ens_vortexinit','no')

def prelaunch_rsmc(conf,logger,cycle):
    """!Modifies the configuration for the RSMC (JTWC, NHC, etc.)

    Modifies the configuration to work differently for JTWC and NHC
    storms.  Searches for the rsmc_conf option in the [prelaunch]
    section for the name of a configuration file to read, and reads it
    if it exists.
    @param conf the hafs.config.ProdConfig to modify
    @param logger the logging.Logger for log messages
    @param cycle the cycle being run."""
    rsmc_overrides=conf.getbool('prelaunch','rsmc_overrides')
    if not rsmc_overrides:
        logger.info('RSMC overrides are disabled.')
        return

    vit=conf.syndat
    rsmc=str(vit.center).upper()
    rfile=conf.strinterp('prelaunch','{rsmc_conf}',RSMC=rsmc)
    if not produtil.fileop.isnonempty(rfile):
        logger.warning('%s: RSMC override file is empty or non-existent'
                       %(rfile,))
    conf.read(rfile)

def prelaunch_basin(conf,logger,cycle):
    """!Modifies the configuration for the basin.

    @anchor prelaunch_basin_main
    Modifies the configuration to work differently for each basin.
    Searches for the basin_conf option in the [prelaunch] section for
    the name of a configuration file to read, and reads it if it
    exists.  If it does not exist, searches for the no_basin_conf
    section in [prelaunch] and runs that instead, if it exists.
    @param conf the hafs.config.ProdConfig to modify
    @param logger the logging.Logger for log messages
    @param cycle the cycle being run."""
    basin_overrides=conf.getbool('prelaunch','basin_overrides')
    if not basin_overrides:
        logger.info('Basin overrides are disabled.')
        return

    vit=conf.syndat
    if vit is None:
        logger.warning('Cannot use basin overrides - conf.syndat is None')
        return
    bfile=conf.strinterp('prelaunch','{basin_conf}',vit=vit)
    nfile=conf.strinterp('prelaunch','{no_basin_conf}',vit=vit)

    if os.path.exists(bfile):
        logger.warning('%s: reading basin override file'%(bfile,))
        conf.read(bfile)
    elif os.path.exists(nfile):
        logger.warning('%s: basin override enabled, but file is '
                       'missing or empty; will read %s instead.'
                       %(bfile,nfile))
        conf.read(nfile)
    else:
        logger.warning('%s: basin override enabled, and no "no_basin_file"'
                       'is available at %s; will not override defaults.'
                       %(bfile,nfile))

