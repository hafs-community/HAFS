#!/usr/bin/env python

##@namespace pom.init
#POM preprocessing and spinup scripts.
#
#This module contains classes and methods for ocean prepossessing,
#ocean spin up: Phase1 and Phase2 (also known as Phase3 and
#Phase4). The prognostic ocean spin up uses wind stress forcing that is
#based on NHC vitals. I use a simple "get_vitals" function to extract
#vitals from NHC vital file (syndat_tcvitals.${yyyy}).
#
#@note Please report bugs/questions/comments to bijuthomas(at)uri(dot)edu.
#@author Biju Thomas, GSO, University of Rhode Island.
#@date June 13, 2014

##@var __all__
# List of symbols exported by "from pom.init import *"
__all__=[ 'Hwrf', 'Oceanini', 'fbtr', 'g3', 'na', 'rt', 'phase', 'pget', 'prun', 'psend' ]

import os
import logging
import shutil
import os.path
import produtil.run, produtil.fileop, produtil.cd, produtil.rusage

from os.path import join as jn2r
from produtil.cd import NamedDir
from produtil.fileop import deliver_file, makedirs, make_symlink, rmall, isnonempty
from produtil.run import mpi, mpirun, checkrun, run, exe , openmp
from util import ysplitter, inpfile, logi2int
from exceptions import *
from check_lcfiles import LCUsefullness
from rtofs_infile import read_rtofs_infile, add_afile_infile, prep_rtofs_infile, get_rtofs_kdm
from track import get_vitals, track_shorten
from produtil.ecflow import set_ecflow_label

ocn_clim = {"GDEM":"1", "GDEM3":"2", "LEVIT":"3"}

class Hwrf(object):
    """!Abstract base class that stores data relating to the HWRF
    workflow in which the POM initialization resides."""
    def __init__(self,STORMNAME,STORMID,STARTDATE,EXEChwrf,PARMhwrf,
                      FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN):
        """!Hwrf constructor
        @param STORMNAME Upper-case storm name for filenames (KATRINA)
        @param STORMID Three character storm number and basin, upper-case (12L)
        @param STARTDATE Simulation analysis time as a string, YYYYMMDDHH (2005082918)
        @param EXEChwrf Directory with HWRF executables.
        @param PARMhwrf Directory with HWRF parameter files.
        @param FIXhwrf Directory with HWRF fixed files.
        @param LCDIR Directory with loop current files
        @param GFSDIR Directory with input files from GFS.
        @param CSTREAM Directory to place files for the POM forecast to read.
        @param COMIN HWRF final output directory."""
        self.STORMNAME = STORMNAME
        self.STORMID = STORMID
        self.STARTDATE = STARTDATE
        self.EXEChwrf = EXEChwrf
        self.PARMhwrf = "".join([PARMhwrf,"/pom"])
        self.LCDIR = LCDIR
        self.GFSDIR = GFSDIR
        self.FIXhwrf = FIXhwrf
        assert(self.GFSDIR.find('pom/output')<0)
        self.CSTREAM = CSTREAM
        self.COMIN = COMIN

    ##@var STORMNAME
    # Upper-case storm name for filenames (KATRINA)

    ##@var STORMID
    # Three character storm number and basin, upper-case (12L)

    ##@var STARTDATE
    # Simulation analysis time as a string, YYYYMMDDHH (2005082918)

    ##@var EXEChwrf
    # Directory with HWRF executables.

    ##@var PARMhwrf
    # Directory with HWRF parameter files.

    ##@var FIXhwrf
    # Directory with HWRF fixed files.

    ##@var LCDIR
    # Directory with loop current files

    ##@var GFSDIR
    # Directory with input files from GFS.

    ##@var CSTREAM
    # Directory to place files for the POM forecast to read.

    ##@var COMIN
    # HWRF final output directory.

class Oceanini(Hwrf):
    """!Parent class of POM initialization classes."""
    def __init__(self,ODIR,DOMAIN,ODATA,SSTASIM,INDATA,PHY1D,STORMNAME,STORMID,
                 STARTDATE, EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,
                 COMIN, **kwargs):
        """!Oceanini constructor
        @param ODIR Ocean output directory in CSTREAM
        @param SSTASIM SST ASIM flag, True or False
        @param DOMAIN Ocean domain.
        @param ODATA Type of ocean data.
        @param STORMNAME Upper-case storm name for filenames (KATRINA)
        @param STORMID Three character storm number and basin, upper-case (12L)
        @param STARTDATE Simulation analysis time as a string, YYYYMMDDHH (2005082918)
        @param EXEChwrf Directory with HWRF executables.
        @param PARMhwrf Directory with HWRF parameter files.
        @param FIXhwrf Directory with HWRF fixed files.
        @param LCDIR Directory with loop current files
        @param GFSDIR Directory with input files from GFS.
        @param CSTREAM Directory to place files for the POM forecast to read.
        @param COMIN HWRF final output directory.
        @param kwargs Can optionally contain conf, which should point
          to a ConfigParser-like object to use for configuration
          information.  An hwrf.config.HWRFConfig is acceptable for
          this."""
        assert(GFSDIR.find('pom/output')<0)
        super(Oceanini,self).__init__(STORMNAME,STORMID,STARTDATE,EXEChwrf,PARMhwrf,
                      FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN)
        self.ODIR = ODIR
        self.DOMAIN = DOMAIN
        self.ODATA = ODATA
        self.SSTASIM = SSTASIM
        self.INDATA = INDATA
        self.PHY1D = PHY1D
        self.conf = kwargs.pop('conf', None)
        self.gfssst_type = self.conf.getint('pom','gfssst_type',3)
    ##@var ODIR
    # Ocean output directory in CSTREAM

    ##@var SSTASIM
    # SST ASIM flag, True or False

    ##@var DOMAIN
    # Ocean domain.

    ##@var ODATA
    # Type of ocean data.

    ##@var conf
    # A ConfigParser-like object to use for configuration information.  This may be
    # a hwrf.config.HWRFConfig object.

    def setpath(self,SUBDIR=""):
        """!Creates the ocean run path, whose subdirectory is SUBDIR.
        @param SUBDIR the subdirectory to create
        @returns a tuple (rundir,finaldir) where rundir is CSTREAM+'/'+ODIR
          and finaldir is the SUBDIR subdirectory of that """
        RUNDIR = self.CSTREAM +"/"+self.ODIR
        try:
            if SUBDIR != "" and os.path.exists(RUNDIR) and   \
                                not os.path.exists(RUNDIR+"/"+SUBDIR):
                makedirs(RUNDIR+"/"+SUBDIR)
            if not os.path.exists(RUNDIR):
                makedirs(RUNDIR)
            return (RUNDIR,RUNDIR+"/"+SUBDIR)
        except:
          msg=("%s: CSTREAM does NOT exist" %(CSTREAM))
          raise IOError(msg)

lc_oper_info=\
"""The loop current files are unusable.  I will fall back to
climatology loop current files that are stored in the HWRF parm
directory.  Please check the NHC save areas and HWRF script logic.

The check_lcfiles says this:

------------------------------------------------------------------
%s
------------------------------------------------------------------

Note that off-season storms may be in this situation as a
regular course of operations since NHC and URI do not update
the loop current files off season.  That is especially true
if the storms are all far from the Loop Current.  In such
situations, climatology loop current is acceptable.
"""


class fbtr(Oceanini):
    """!Runs the FBTR (feature-based) initialization of the MPIPOMTC."""
    def __init__(self,*args,**kargs):
        """!fbtr constructor
        @param args,kargs passed to Oceanini.__init__()"""
        super(fbtr,self).__init__(*args,**kargs)
    def getinp(self, DEST,logger=None):
        """!Links or copies input files.

        @param DEST The directory in which to link or copy files.
        @param logger A logging.Logger for log messages."""
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        try:
            if self.gfssst_type == 1:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.pgrb2.0p25.f000"),jn2r(DEST,"fort.11"),True,logger=logger)
                inpfile(jn2r(DEST,"gribinfo"), ['000',mm,dd,hh],logger=logger)
            if self.gfssst_type == 2:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
            if self.gfssst_type == 3:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sanl"),jn2r(DEST,"fort.12"),True,logger=logger)
            if self.gfssst_type == 4:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.pgrb2.pgbm.f000"),jn2r(DEST,"fort.11"),True,logger=logger)
                inpfile(jn2r(DEST,"gribinfo"), ['000',mm,dd,hh],logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_topo_and_mask.united"),
                         jn2r(DEST,"fort.66"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_gdem."+mm+".ascii"),
                         jn2r(DEST,"fort.8"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_gdem."+mm1+".ascii"),
                         jn2r(DEST,"fort.90"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_readu.dat."+mm),
                         jn2r(DEST,"fort.24"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup_gdem3.dat."+mm),
                         jn2r(DEST,"fort.82"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup_gspath."+mm),
                         jn2r(DEST,"fort.50"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup.BAYuf"),
                         jn2r(DEST,"fort.55"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup.FSgsuf"),
                         jn2r(DEST,"fort.65"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup.SGYREuf"),
                         jn2r(DEST,"fort.75"),True,logger=logger)
            lfile = jn2r(self.LCDIR,"hwrf_gfdl_loop_current_rmy5.dat."+y4+mm+dd)
            rfile = jn2r(self.LCDIR,"hwrf_gfdl_loop_current_wc_ring_rmy5.dat."+y4+mm+dd)

            # Check for loop current file usability:
            useful=LCUsefullness(self.STARTDATE, lfile, rfile,logger=logger)
            useful.run()

            if useful:
                make_symlink(lfile,jn2r(DEST,"fort.31"),True,logger=logger)
                make_symlink(rfile,jn2r(DEST,"fort.32"),True,logger=logger)
            else:
                require_loop=self.conf.getstr('failure','loop_current','none')
                if require_loop=='unexpected_failure':
                    raise UnexpectedPOMFailureTest('Loop current unusable and loop current is required due to a failure test setting.')
                elif require_loop=='expected_failure':
                    raise ExpectedPOMFailureTest('Loop current unusable and loop current is required due to a failure test setting.')

                # Fallback to climatology
                make_symlink(jn2r(self.PARMhwrf,"hwrf_gfdl_loop_current_rmy5.dat"),
                         jn2r(DEST,"fort.31"),True,logger=logger)
                make_symlink(jn2r(self.PARMhwrf,"hwrf_gfdl_loop_current_wc_ring_rmy5.dat"),
                         jn2r(DEST,"fort.32"),True,logger=logger)
            inpfile(jn2r(DEST,"fort.91"), [mm, dd],logger=logger)
            inpfile(jn2r(DEST,"input_sharp"),[ocn_clim[self.INDATA]],logger=logger)
            inpfile(jn2r(DEST,"input"),
                    [self.ODATA, self.PHY1D, self.SSTASIM,self.STORMNAME],logger=logger)
        except (UnexpectedPOMFailureTest,ExpectedPOMFailureTest) as ee: raise
        except Exception as e:
            msg='Cannot find input: '+str(e)
            logger.warning(msg,exc_info=True)
            raise POMInputError(msg)
    def setrun(self, DEST,logger=None):
      """!Runs the FBTR initialization
      @param DEST the destination directory in which to run and generate output.
      @param logger a logging.Logger for log messages"""
      with NamedDir(DEST) as d:
        set_ecflow_label('method',"feature-based",logger)
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        rmall(jn2r(DEST,"fort.23"),jn2r(DEST,"fort.74"),jn2r(DEST,"fort.77"),logger=logger)
        rmall(jn2r(DEST,"lonlat.gfs"),jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"mask.gfs.dat"),logger=logger)
        if os.path.exists(jn2r(DEST,"fort.11")):
            with open('listing','wb') as f:
                listing=str(produtil.listing.Listing())
                f.write(listing)
            produtil.rusage.getrlimit(logger)
            # Raise stack limit to 6000M
            produtil.rusage.setrlimit(ignore=True,logger=logger,stack=6e9)
            if self.gfssst_type == 1 or self.gfssst_type == 4:
               xc = self.conf.getexe('grb2index')
               log = jn2r(DEST,"grb2index.out")
               retcode = run((exe(xc) ["fort.11", "fort.99"]) >= log,logger=logger) 
               logger.info("GRIB index file Created: {0}".format(DEST))
            xc = self.conf.getexe('hwrf_getsst')
            log = jn2r(DEST,"getsst.out")
            with produtil.rusage.rusage(logger=logger):
                retcode = run(openmp(exe(xc),threads=1).env(OMP_STACKSIZE='128M')[self.gfssst_type]
                              > log,logger=logger)
            logger.info("hwrf_getsst: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.74")):
                deliver_file(jn2r(DEST,"fort.74"),jn2r(DEST,"sst.gfs.dat"),False)
                deliver_file(jn2r(DEST,"fort.77"),jn2r(DEST,"mask.gfs.dat"),False)
                logger.info("GFS SST extracted: {0}".format(DEST))
            else:
                msg="hwrf_getsst Failed: NO GFS SST extracted: {0}".format(DEST)
                set_ecflow_label('method',"Feature-based hwrf_getsst FAILED - revert to uncoupled.",logger)
                logger.warning(msg)
                raise POMSSTError(msg)
        else:
            msg="GFS Spectral Files NOT Found in %s:" %(DEST)
            logger.error(msg)
            set_ecflow_label('method',"Feature-based missing GFS data; FAILED - revert to uncoupled.",logger)
            raise POMInputError(msg)

        if isnonempty('fort.31') and isnonempty('fort.32'):
            rmall(jn2r(DEST,"fort.13"),jn2r(DEST,"gfdl_initdata.united."+mm),logger=logger)
            xc = self.conf.getexe('hwrf_sharp_mcs_rf_l2m_rmy5')
            inp = jn2r(DEST,"input_sharp")
            log = jn2r(DEST,"sharp_mcs_r_l2b.out")
            retcode = run((exe(xc) < inp) >= log,logger=logger)
            logger.info("sharp_mcs_rf: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.13")):
                deliver_file(jn2r(DEST,"fort.13"),jn2r(DEST,"gfdl_initdata.united."+mm),False,logger=logger)
                logger.info("Specified LC penetration: {0}".format(retcode))
                set_ecflow_label('method',"feature-based with recent loop current",logger)
            else:
                deliver_file(jn2r(self.FIXhwrf,"gfdl_initdata.gdem.united."+mm),
                             jn2r(DEST,"gfdl_initdata.united."+mm),logger=logger)
                logger.warning("sharp_mcs_rf failed: Climate LC penetration %s" %(retcode))
                set_ecflow_label('method',"climatology loop current (sharp_mcs_rf failed)",logger)
        else:
            deliver_file(jn2r(self.FIXhwrf,"gfdl_initdata.gdem.united."+mm),
                         jn2r(DEST,"gfdl_initdata.united."+mm),logger=logger)

            require_loop=self.conf.getstr('failure','loop_current','none')
            if require_loop=='unexpected_failure':
                raise UnexpectedPOMFailureTest('Loop current unusable and loop current is required due to a failure test setting.')
            elif require_loop=='expected_failure':
                raise ExpectedPOMFailureTest('Loop current unusable and loop current is required due to a failure test setting.')

            # Log to highest log level so it is in the jlogfile:
            logger.critical("LC data NOT available: Climate LC penetration %s" %(retcode))
            set_ecflow_label('method',"climatology loop current (loop data is missing)",logger)
        if os.path.exists(jn2r(DEST,"gfdl_initdata.united."+mm)):
            make_symlink(DEST+"/gfdl_initdata.united."+mm, DEST+"/fort.13",True,logger=logger)
            xc = self.conf.getexe('hwrf_ocean_transatl06prep')
            log = jn2r(DEST,"transatl06prep.out")
            retcode = run(exe(xc)  >= log, logger=logger)
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.113")):
                deliver_file(jn2r(DEST,"fort.113"),jn2r(DEST,"gfdl_initdata."+self.DOMAIN+"."+mm),False,logger=logger)
                logger.info("transatl06prep: Success {0}".format(retcode))
            else:
                msg="transatl06prep: Failed {0}".format(retcode)
                set_ecflow_label('method',"Feature-based ocean_transatl06prep FAILED - revert to uncoupled.",logger)
                logger.warning(msg)
                raise POMPrepError(msg)
        else:
            logger.info("NOT Found in %s:" %("gfdl_initdata.united."+mm))
        if os.path.exists(jn2r(DEST,"gfdl_initdata."+self.DOMAIN+"."+mm)):
             deliver_file(jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"fort.21"),False,logger=logger)
             deliver_file(jn2r(DEST,"mask.gfs.dat"),jn2r(DEST,"fort.22"),False,logger=logger)
             rmall(jn2r(DEST,"fort.13"),logger=logger)
             make_symlink(jn2r(DEST,"gfdl_initdata."+self.DOMAIN+"."+mm),jn2r(DEST,"fort.13"),True,logger=logger)
             make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_topo_and_mask."+self.DOMAIN+
                          ".lores"), jn2r(DEST,"fort.66"),True,logger=logger)
             xc = self.conf.getexe("hwrf_ocean_pomprep_"+self.ODATA[0:2])
             inp = jn2r(DEST,"input")
             log = jn2r(DEST,"ocean_pomprep.out")
             retcode = run((exe(xc) < inp ) >= log,logger=logger)
             if retcode == 0 and os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")):
                 logger.info("ocean_pomprep: Success %s:" %(self.ODATA))
                 return True
             else:
                 msg="ocean_pomprep: Failed %s" %(self.ODATA)
                 set_ecflow_label('method',"Feature-based ocean_pomprep FAILED - revert to uncoupled.",logger)
                 logger.warning(msg)
                 raise POMInitFailed(msg)
        else:
            msg="NOT Found in %s:" %(''.join(["gfdl_initdata.",self.DOMAIN,".",mm]))
            set_ecflow_label('method',"Feature-based init missing fix file - revert to uncoupled.",logger)
            logger.warning(msg)
            raise POMInitFailed(msg)

class g3(Oceanini):
    """!Runs the G3 initialization for POM."""
    def __init__(self, *args,**kargs):
        """!g3 constructor
        @param args,kargs All arguments are passed to Oceanini.__init__()"""
        super(g3,self).__init__(*args,**kargs)
    def getinp(self, DEST,logger=None):
        """!Links all required input files.
        @param DEST directory in which to link inputs.
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        try:
            if self.gfssst_type == 1:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.pgrb2.0p25.f000"),jn2r(DEST,"fort.11"),True,logger=logger)
                inpfile(jn2r(DEST,"gribinfo"), ['000',mm,dd,hh],logger=logger)
            if self.gfssst_type == 2:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
            if self.gfssst_type == 3:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sanl"),jn2r(DEST,"fort.12"),True,logger=logger)
            if self.gfssst_type == 4:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.pgrb2.pgbm.f000"),jn2r(DEST,"fort.11"),True,logger=logger)
                inpfile(jn2r(DEST,"gribinfo"), ['000',mm,dd,hh],logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_topo_and_mask."+
                         self.DOMAIN+".lores"),jn2r(DEST,"fort.66"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"tgdemv3s"+mm+".nc"),
                         jn2r(DEST,"tin.nc"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"sgdemv3s"+mm+".nc"),
                         jn2r(DEST,"sin.nc"),True,logger=logger)
            inpfile(jn2r(DEST,"input"),
                 [self.ODATA, self.PHY1D, self.SSTASIM,self.STORMNAME],logger=logger)
        except Exception as e:
            msg='Input Data does NOT exist: %s'%(str(e),)
            logger.error(msg,exc_info=True)
            set_ecflow_label('method',"GDEM init missing data; FAILED - revert to uncoupled.",logger)
            raise POMInputError(msg)
    def setrun(self, DEST,logger=None):
      """!Runs the g3 ocean initialization
      @param DEST directory in which to run
      @param logger a logging.Logger for log messages"""
      with NamedDir(DEST):
        set_ecflow_label('method',"GDEM climatology",logger)
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        rmall(jn2r(DEST,"fort.23"),jn2r(DEST,"fort.74"),jn2r(DEST,"fort.77"),logger=logger)
        rmall(jn2r(DEST,"lonlat.gfs"),jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"mask.gfs.dat"),logger=logger)
        if os.path.exists(jn2r(DEST,"fort.11")):
            produtil.rusage.getrlimit(logger)
            # Raise stack limit to 6000M
            produtil.rusage.setrlimit(ignore=True,logger=logger,stack=6e9)
            if self.gfssst_type == 1 or self.gfssst_type == 4:
               xc = self.conf.getexe('grb2index')
               log = jn2r(DEST,"grb2index.out")
               retcode = run((exe(xc) ["fort.11", "fort.99"]) >= log,logger=logger) 
               logger.info("GRIB index file Created: {0}".format(DEST))
            xc = self.conf.getexe('hwrf_getsst')
            log = jn2r(DEST,"getsst.out")
            with produtil.rusage.rusage(logger=logger):
                retcode = run(openmp(exe(xc),threads=1).env(OMP_STACKSIZE='128M')[self.gfssst_type]
                              > log,logger=logger)
            logger.info("hwrf_getsst: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.74")):
                deliver_file(jn2r(DEST,"fort.74"),jn2r(DEST,"sst.gfs.dat"),False,logger=logger)
                deliver_file(jn2r(DEST,"fort.77"),jn2r(DEST,"mask.gfs.dat"),False,logger=logger)
                logger.info("GFS SST extracted: {0}".format(retcode))
            else:
                msg="hwrf_getsst Failed: NO GFS SST extracted %s" %(retcode)
                logger.warning(msg)
                set_ecflow_label('method',"Feature-based hwrf_getsst FAILED - revert to uncoupled.",logger)
                raise POMSSTError(msg)
        else:
            msg="GFS Spectral Files NOT Found in %s:" %(DEST)
            logger.warning(msg)
            set_ecflow_label('method',"Feature-based missing GFS data; FAILED - revert to uncoupled.",logger)
            raise POMInputError(msg)
        deliver_file(jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"fort.21"),False,logger=logger)
        deliver_file(jn2r(DEST,"mask.gfs.dat"),jn2r(DEST,"fort.22"),False,logger=logger)
        xc = self.conf.getexe("hwrf_ocean_pomprep_"+self.ODATA[0:2])
        inp = jn2r(DEST,"input")
        log = jn2r(DEST,"ocean_pomprep.out")
        retcode = run((exe(xc) < inp) > log,logger=logger)
        if retcode == 0 and os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")):
            logging.info("ocean_pomprep: Success %s:" %(self.ODATA))
            return True
        else:
            msg="ocean_pomprep: Failed %s:" %(self.ODATA)
            set_ecflow_label('method',"GDEM ocean_init FAILED - revert to uncoupled.",logger)
            logger.warning(msg)
            raise POMInitFailed(msg)

class na(Oceanini):
    """!Runs the na (NCODA) initialization of POM."""
    def __init__(self, *args,**kargs):
        """!na Constructor
        @param args,kargs All arguments passed to Oceanini.__init__()"""
        super(na,self).__init__(*args,**kargs)
    def getinp(self, DEST, logger=None):
        """!Obtains all input files, linking or copying them.
        @param DEST Directory in which to link files.
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        try:
            if self.gfssst_type == 1:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.pgrb2.0p25.f000"),jn2r(DEST,"fort.11"),True,logger=logger)
                inpfile(jn2r(DEST,"gribinfo"), ['000',mm,dd,hh],logger=logger)
            if self.gfssst_type == 2:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
            if self.gfssst_type == 3:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sanl"),jn2r(DEST,"fort.12"),True,logger=logger)
            if self.gfssst_type == 4:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.pgrb2.pgbm.f000"),jn2r(DEST,"fort.11"),True,logger=logger)
                inpfile(jn2r(DEST,"gribinfo"), ['000',mm,dd,hh],logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_topo_and_mask."+
                         self.DOMAIN+".lores"),jn2r(DEST,"fort.66"),True,logger=logger)
            make_symlink(jn2r(GFSDIR,"seatmp_pre_000000_005000_1o2161x1051_"+
                         y4+mm+dd+"00_00000000_analfld"),jn2r(DEST,"fort.48"),True,logger=logger)
            make_symlink(jn2r(GFSDIR,"salint_pre_000000_005000_1o2161x1051_"+
                         y4+mm+dd+"00_00000000_analfld"),jn2r(DEST,"fort.49"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"depths_sfc_000000_000000_1o2161x1051_datafld"),
                         jn2r(DEST,"fort.68"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"grdlon_sfc_000000_000000_1o2161x1051_datafld"),
                         jn2r(DEST,"fort.78"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"grdlat_sfc_000000_000000_1o2161x1051_datafld"),
                         jn2r(DEST,"fort.79"),True,logger=logger)
            ##################################################################################
            inpfile(jn2r(DEST,"input"),
                         [self.ODATA, self.PHY1D, self.SSTASIM,self.STORMNAME],logger=logger)
        except Exception as e:
            msg='Input data does NOT exist: %s'%(str(e),)
            logger.error(msg,exc_info=True)
            raise POMInputError(msg)
    def setrun(self, DEST,logger=None):
      """!Runs the na initialization.
      @param DEST directory in which to run.
      @param logger a logging.Logger for log messages."""
      with NamedDir(DEST):
        set_ecflow_label('method',"NCODA initialization",logger)
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        rmall(jn2r(DEST,"fort.23"),jn2r(DEST,"fort.74"),jn2r(DEST,"fort.77"),logger=logger)
        rmall(jn2r(DEST,"lonlat.gfs"),jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"mask.gfs.dat"),logger=logger)
        if os.path.exists(jn2r(DEST,"fort.11")):
            produtil.rusage.getrlimit(logger)
            # Raise stack limit to 6000M
            produtil.rusage.setrlimit(ignore=True,logger=logger,stack=6e9)
            if self.gfssst_type == 1 or self.gfssst_type == 4:
               xc = self.conf.getexe('grb2index')
               log = jn2r(DEST,"grb2index.out")
               retcode = run((exe(xc) ["fort.11", "fort.99"]) >= log,logger=logger) 
               logger.info("GRIB index file Created: {0}".format(DEST))
            xc = self.conf.getexe('hwrf_getsst')
            log = jn2r(DEST,"getsst.out")
            retcode = run(exe(xc).env(OMP_STACKSIZE='128M')[self.gfssst_type]
                          > log,logger=logger)
            logging.info("hwrf_getsst: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.74")):
                deliver_file(jn2r(DEST,"fort.74"),jn2r(DEST,"sst.gfs.dat"),False,logger=logger)
                deliver_file(jn2r(DEST,"fort.77"),jn2r(DEST,"mask.gfs.dat"),False,logger=logger)
                logger.info("GFS SST extracted: {0}".format(retcode))
            else:
                msg="hwrf_getsst Failed: NO GFS SST extracted %s"%(retcode)
                logger.warning(msg)
                raise POMSSTError(msg)
        else:
            msg="GFS Spectral Files NOT Found in %s:" %(DEST)
            logger.warning(msg)
            set_ecflow_label('method',"Feature-based missing GFS data; FAILED - revert to uncoupled.",logger)
            raise POMInputError(msg)
        deliver_file(jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"fort.21"),False,logger=logger)
        deliver_file(jn2r(DEST,"mask.gfs.dat"),jn2r(DEST,"fort.22"),False,logger=logger)
        xc = self.conf.getexe("hwrf_ocean_pomprep_"+self.ODATA[0:2])
        inp = jn2r(DEST,"input")
        log = jn2r(DEST,"ocean_pomprep.out")
        retcode = run((exe(xc) < inp) >log,logger=logger)
        if retcode == 0 and os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")):
            logging.info("ocean_pomprep: Success %s:" %(self.ODATA))
            return True
        else:
            set_ecflow_label('method',"NCODA ocean_init FAILED - revert to uncoupled.",logger)
            msg="ocean_pomprep: Failed %s:" %(self.ODATA)
            logger.warning(msg)
            raise POMInitFailed(msg)

class rt(Oceanini):
    """!Runs the rt (RTOF) initialization of POM."""
    def __init__(self, *args,**kargs):
        """!na Constructor
        @param args,kargs All arguments passed to Oceanini.__init__()"""
        super(rt,self).__init__(*args,**kargs)
    def getinp(self, DEST, logger=None):
        """!Obtains all input files, linking or copying them.
        @param DEST Directory in which to link files.
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        storminfo = self.conf.syndat
        lon = storminfo.lon if storminfo.lon > 0.0 else storminfo.lon + 360.0
        lat = storminfo.lat
        self.makeinp(DEST)
        try:
            if hh == "00":
                make_symlink(jn2r(self.GFSDIR,'rtofs_glo.t00z.n00.archv.a') ,
                         jn2r(DEST,'rtofs_glo.t00z.n00.archv.a'), True,logger=logger)
                make_symlink(jn2r(self.GFSDIR,'rtofs_glo.t00z.n00.archv.b') ,
                         jn2r(DEST,'rtofs_glo.t00z.n00.archv.b'), True,logger=logger)
                kdm = get_rtofs_kdm(jn2r(self.GFSDIR,'rtofs_glo.t00z.n00.archv.b'))
            else:
                make_symlink(jn2r(self.GFSDIR,''.join(['rtofs_glo.t00z.f',hh,'.archv.a'])) ,
                         jn2r(DEST,''.join(['rtofs_glo.t00z.f',hh,'.archv.a'])), True,logger=logger)
                make_symlink(jn2r(self.GFSDIR,''.join(['rtofs_glo.t00z.f',hh,'.archv.b'])) ,
                         jn2r(DEST,''.join(['rtofs_glo.t00z.f',hh,'.archv.b'])), True,logger=logger)
                kdm = get_rtofs_kdm(jn2r(self.GFSDIR,'rtofs_glo.t00z.f'+hh+'.archv.b'))
            if kdm == 32:
                FIXrtofs=jn2r(self.FIXhwrf,"../rtofs-navy-32/")
            elif kdm == 41:
                FIXrtofs=jn2r(self.FIXhwrf,"../rtofs-navy-41/")
            else:
                msg='Wrong kdm %d from %s'%(kdm,bfile)
                logger.error(msg,exc_info=True)
                set_ecflow_label('method',"RTOFS init FAILED (%s levels unrecognized) - revert to uncoupled"
                             %(repr(kdm),),logger)
                raise POMInputError(msg)
            make_symlink(jn2r(FIXrtofs,"./rtofs_glo.navy_0.08.regional.depth.a"),
                         jn2r(DEST,"regional.depth.a"),True,logger=logger)
            make_symlink(jn2r(FIXrtofs,"./rtofs_glo.navy_0.08.regional.depth.b"),
                         jn2r(DEST,"regional.depth.b"),True,logger=logger)
            make_symlink(jn2r(FIXrtofs,"./rtofs_glo.navy_0.08.regional.grid.a"),
                         jn2r(DEST,"regional.grid.a"),True,logger=logger)
            make_symlink(jn2r(FIXrtofs,"./rtofs_glo.navy_0.08.regional.grid.b"),
                         jn2r(DEST,"regional.grid.b"),True,logger=logger)
            if self.gfssst_type == 1:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.pgrb2.0p25.f000"),jn2r(DEST,"fort.11"),True,logger=logger)
                inpfile(jn2r(DEST,"gribinfo"), ['000',mm,dd,hh],logger=logger)
            if self.gfssst_type == 2:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
            if self.gfssst_type == 3:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sanl"),jn2r(DEST,"fort.12"),True,logger=logger)
            if self.gfssst_type == 4:
                make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.pgrb2.pgbm.f000"),jn2r(DEST,"fort.11"),True,logger=logger)
                inpfile(jn2r(DEST,"gribinfo"), ['000',mm,dd,hh],logger=logger)
            inpfile(jn2r(DEST,"input"),
                         [self.ODATA, self.PHY1D, self.SSTASIM, self.STORMNAME, "%.6f"%lon, "%.6f"%lat],logger=logger)
        except Exception as e:
            msg='Input data does NOT exist: %s'%(str(e),)
            logger.error(msg,exc_info=True)
            set_ecflow_label('method',"RTOFS init missing input data; FAILED - revert to uncoupled.",logger)
            raise POMInputError(msg)
    def makeinp(self, DEST, logger=None):
        """!Obtains all input files, linking or copying them.
        @param DEST Directory in which to link files.
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        if hh == "00":
            afile = 'rtofs_glo.t00z.n00.archv.a'
            bfile = 'rtofs_glo.t00z.n00.archv.b'
        else:
            afile=''.join(['rtofs_glo.t00z.f',hh,'.archv.a'])
            bfile=''.join(['rtofs_glo.t00z.f',hh,'.archv.b'])
        prep_rtofs_infile(jn2r(self.PARMhwrf,"rtofs_infile"), afile,
                         jn2r(self.GFSDIR,bfile), jn2r(DEST,"infile"))
    def setrun(self, DEST,logger=None):
      """!Runs the na initialization.
      @param DEST directory in which to run.
      @param logger a logging.Logger for log messages."""
      with NamedDir(DEST):
        set_ecflow_label('method',"RTOFS initialization starting...",logger)
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        rmall(jn2r(DEST,"fort.23"),jn2r(DEST,"fort.74"),jn2r(DEST,"fort.77"),logger=logger)
        rmall(jn2r(DEST,"lonlat.gfs"),jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"mask.gfs.dat"),logger=logger)
        if os.path.exists(jn2r(DEST,"fort.11")):
            produtil.rusage.getrlimit(logger)
            # Raise stack limit to 10000M
            produtil.rusage.setrlimit(ignore=True,logger=logger,stack=1e10)
            if self.gfssst_type == 1 or self.gfssst_type == 4:
               xc = self.conf.getexe('grb2index')
               log = jn2r(DEST,"grb2index.out")
               retcode = run((exe(xc) ["fort.11", "fort.99"]) >= log,logger=logger) 
               logger.info("GRIB index file Created: {0}".format(DEST))
            xc = self.conf.getexe('hwrf_getsst')
            log = jn2r(DEST,"getsst.out")
            retcode = run(exe(xc).env(OMP_STACKSIZE='128M')[self.gfssst_type]
                          > log,logger=logger)
            logging.info("hwrf_getsst: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.74")):
                deliver_file(jn2r(DEST,"fort.74"),jn2r(DEST,"sst.gfs.dat"),False,logger=logger)
                deliver_file(jn2r(DEST,"fort.77"),jn2r(DEST,"mask.gfs.dat"),False,logger=logger)
                logger.info("GFS SST extracted: {0}".format(retcode))
            else:
                msg="hwrf_getsst Failed: NO GFS SST extracted %s"%(retcode)
                logger.warning(msg)
                set_ecflow_label('method',"RTOFS init gfs_sst FAILED - revert to uncoupled.",logger)
                raise POMSSTError(msg)
        else:
            msg="GFS Spectral Files NOT Found in %s:" %(DEST)
            set_ecflow_label('method',"RTOFS init GFS data missing; FAILED - revert to uncoupled.",logger)
            logger.warning(msg)
            raise POMInputError(msg)
        deliver_file(jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"fort.21"),False,logger=logger)
        deliver_file(jn2r(DEST,"mask.gfs.dat"),jn2r(DEST,"fort.22"),False,logger=logger)
        xc = self.conf.getexe("hwrf_pom_archv2data3z")
        inp = jn2r(DEST,"infile")
        log = jn2r(DEST,"archv2data3z.out")
        retcode = run((exe(xc) < inp) >log,logger=logger)
        if retcode == 0 and os.path.exists(jn2r(DEST,"fort.40")):
                deliver_file(jn2r(DEST,"fort.40"),jn2r(DEST,"temprtf.dat"),False,logger=logger)
                deliver_file(jn2r(DEST,"fort.41"),jn2r(DEST,"salnrtf.dat"),False,logger=logger)
                deliver_file(jn2r(DEST,"fort.42"),jn2r(DEST,"uwndrtf.dat"),False,logger=logger)
                deliver_file(jn2r(DEST,"fort.43"),jn2r(DEST,"vwndrtf.dat"),False,logger=logger)
                logger.info("archv2data3z outputs delivered: {0}".format(retcode))
        else:
            msg="No fort.40 (archv2data3z) Files NOT Found in %s:" %(DEST)
            logger.warning(msg)
            set_ecflow_label('method',"RTOFS init archv2data3z FAILED - revert to uncoupled.",logger)
            raise POMInputError(msg)
        dims = read_rtofs_infile(jn2r(DEST,"infile"))
        if dims is None:
            msg="Return of read_rtofs_infile empty"
            logger.warning(msg)
            set_ecflow_label('method',"RTOFS init read_rtofs-infile FAILED - revert to uncoupled.",logger)
            raise POMInputError(msg)
        idm = dims['idm']
        jdm = dims['jdm']
        xc = self.conf.getexe("hwrf_pom_hycom2raw")
        if not os.path.exists(jn2r(DEST,"lonlatrtf.dat")):
            log = log = jn2r(DEST,"hycom2raw.out")
            retcode = run(( exe(xc) ["regional.grid.a", idm, jdm, "lonlatrtf.dat"] ) > log)
        if not os.path.exists(jn2r(DEST,"lonlatrtf.dat")) or retcode != 0:
            msg=" hycom2raw Failed and lonlatrtf.dat does not exist "
            logger.warning(msg)
            set_ecflow_label('method',"RTOFS init fix files missing; FAILED - revert to uncoupled.",logger)
            raise POMSSTError(msg)
        if not os.path.exists(jn2r(DEST,"depthrtf.dat")):
            retcode = run((exe(xc) ["regional.depth.a", idm, jdm, "depthrtf.dat"] )>> log )
        if not os.path.exists(jn2r(DEST,"depthrtf.dat")) or retcode != 0:
            msg=" hycom2raw Failed and lonlatrtf.dat does not exist "
            logger.warning(msg)
            set_ecflow_label('method',"RTOFS hycom2raw FAILED - revert to uncoupled.",logger)
            raise POMSSTError(msg)
        xc = self.conf.getexe("hwrf_ocean_pomprep_"+self.ODATA[0:2])
        inp = jn2r(DEST,"input")
        log = jn2r(DEST,"ocean_pomprep.out")
        retcode = run((exe(xc) < inp) >log,logger=logger)
        if retcode == 0 and os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")):
            logging.info("ocean_pomprep: Success %s:" %(self.ODATA))
            set_ecflow_label('method',"RTOFS initialization SUCCESS!",logger)
            return True
        else:
            msg="ocean_pomprep: Failed %s:" %(self.ODATA)
            set_ecflow_label('method',"RTOFS initialization FAILED - revert to uncoupled.",logger)
            logger.warning(msg)
            raise POMInitFailed(msg)

class phase(Oceanini):
    """!Runs a later phase of the ocean init to add additional
    features such as cold wakes."""
    def __init__(self,*args,**kargs):
        """!phase constructor
        @param args,kargs All arguments are passed to Oceanini.__init__()"""
        super(phase,self).__init__(*args,**kargs)
    def getinp(self, DEST, logger=None):
        """!Links all input data.
        @param DEST Directory in which to link files.
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=logging.getLogger('pom')
        try:
            (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
            SRC = jn2r(self.CSTREAM, self.ODIR)
            make_symlink(jn2r(SRC,self.STORMNAME+".grid.nc"),
                         jn2r(DEST,self.STORMNAME+".grid.nc"),True,logger=logger)
            make_symlink(jn2r(SRC,self.STORMNAME+".ts_initial.nc"),
                         jn2r(DEST,self.STORMNAME+".ts_initial.nc"),True,logger=logger)
            make_symlink(jn2r(SRC,self.STORMNAME+".ts_clim.nc"),
                         jn2r(DEST,self.STORMNAME+".ts_clim.nc"),True,logger=logger)
            make_symlink(jn2r(SRC,self.STORMNAME+".uv_initial.nc"),
                         jn2r(DEST,self.STORMNAME+".uv_initial.nc"),True,logger=logger)
            make_symlink(jn2r(SRC,self.STORMNAME+".el_initial.nc"),
                         jn2r(DEST,self.STORMNAME+".el_initial.nc"),True,logger=logger)
        except Exception as e:
            msg='Input data does not exist: %s'%(str(e),)
            logger.warning(msg,exc_info=True)
            raise POMInputError(msg)

    def setrun(self, DEST,logger=None):
      """!Runs the phase initialization
      @param DEST The directory in which to run.
      @param logger A logging.Logger for log messages."""
      with NamedDir(DEST):
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        if os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")) and \
           os.path.exists(jn2r(DEST,"pom.nml")):
            exe = self.conf.getexe("hwrf_ocean_init")
            log = jn2r(DEST,"ocean_init.out")
            retcode=run(mpirun(mpi(exe)*9) >= log ,logger=logger)
            logger.info("hwrf_ocean_init: err = %s" %( retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"restart.0001.nc")):
                logger.info("Phase completed HERE:  %s" %(DEST))
                return True
            else:
                msg="Phase FAILED HERE:  %s" %(DEST)
                logger.error(msg)
                return False
        else:
            msg="Input Files  NOT exists %s:" %(DEST)
            logger.warning(msg)
            raise POMInputError(msg)
    def sendout(self, SRC, infile, DEST, outfile,logger=None):
        """!Delivers an output file using produtil.fileop.deliver_file()
        @param SRC Directory with the input file.
        @param infile Basename of the input file inside of SRC
        @param DEST Destination directory of the file
        @param outfile Name of the file in the DEST directory.
        @param logger A logging.Logger for log messages."""
        if logger is None: logger=logging.getLogger('pom')
        if os.path.exists(jn2r(SRC, infile)):
            deliver_file(jn2r(SRC,infile),jn2r(DEST,outfile),False,logger=logger)
            return True
        else:
            msg="Restart file %s does NOT exist:" %(infile)
            logger.warning(msg)
            raise POMInitFailed(msg)

class pget:
    """!Utility class for obtaining input data."""
    def getinp(self, this, DEST,logger=None):
        """!Asks "this" to deliver its input by calling this.getinp(...)
        @param this an Oceanini object
        @param DEST directory in which to make links
        @param logger a logging.Logger for messages"""
        if logger is None: logger=logging.getLogger('pom')
        logger.info('get input this=%s DEST=%s'%(repr(this),repr(DEST)))
        this.getinp(DEST,logger=logger)
class prun:
    """!Utility class for executing ocean initialization setrun functions."""
    def setrun(self, this, DEST,logger=None):
        """!Runs this.setrun(...) to run the ocean initialization
        @param this an Oceanini object
        @param DEST the directory in which to run
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=logging.getLogger('pom')
        return this.setrun(DEST, logger=logger)
class psend:
    """!Utility class for delivering Oceanini output."""
    def sendout(self, this, SRC, infile, DEST, outfile, logger=None):
        """!Calls this.sendout(...)
        @param this An Oceanini object.
        @param SRC the directory from which to copy
        @param infile the input file basename of the file in SRC
        @param DEST the destination directory
        @param outfile the name of the output file in DEST
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=logging.getLogger('pom')
        return this.sendout(SRC, infile, DEST, outfile, logger=logger)

