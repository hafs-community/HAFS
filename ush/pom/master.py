#!/usr/bin/env python

##@namespace pom.master
#@Biju Thomas, GSO, University of Rhode Island on June 12, 2014.
#@ Please report bugs/questions/comments to bijuthomas(at)uri(dot)edu.

import logging
from init import Oceanini, fbtr, g3, na, pget, prun, phase, psend, rt
from util import dateplushours, ysplitter, logi2int, jn2r
from nml import nml, kppnml
import hwrf.launcher
from os.path import exists, getsize, split
from domain import choose_domain
from read_pom_conf import read_pom_conf
from track import get_vitals, track_shorten
from exceptions import *
import produtil.fileop
from produtil.fileop import rmall
from produtil.ecflow import set_ecflow_label

# To use default initialization in a given basin, enter "GDEM"  after the : below
# To use NCODA   initialization in a given basin, enter "NCODA" after the : below

##@var odata_d
# Mapping from basin letter to the list of available initializations for the basin.
odata_d = {"transatl":("fbtr","natr","rttr"), "eastpac":("g3ep","naep","rtep"),
           "westpac":("g3wp","nawp","rtwp"),"northind":("g3ni","nani","rtni"),
           "southind":("g3si","nasi","rtsi"),"swpac":("g3sw","nasw","rtsw"),
           "sepac":("g3se","nase","rtse") }

def run_init(STORMNAME,STORMID,STARTDATE,
             EXEChwrf,PARMhwrf,FIXhwrf,VITDIR,GFSDIR,
             LCDIR,CSTREAM,COMIN,init_data=None,logger=None,
             fcstlen=None,outstep=None,**kwargs):
    """!Run the ocean initialization.

    This is a wrapper around the pom.init module that selects the
    right ocean initialization for the chosen basin and delivers the
    output to the specified location.

    @param STORMNAME Upper-case storm name for filenames (KATRINA)
    @param STORMID Three character storm number and basin, upper-case (12L)
    @param STARTDATE Simulation analysis time as a string, YYYYMMDDHH (2005082918)
    @param EXEChwrf Directory with HWRF executables.
    @param PARMhwrf Directory with HWRF parameter files.
    @param FIXhwrf Directory with HWRF fixed files.
    @param GFSDIR Directory with input files from GFS.
    @param LCDIR Directory with loop current files
    @param CSTREAM Directory to place files for the POM forecast to read.
    @param COMIN HWRF final output directory.

    @param init_data The string name of the initialization method to
    use.  If this is unspecified, a suitable default is chosen.  Allowed values: GDEM, NCODA.

    @param logger a logging.Logger for log messages

    @param fcstlen forecast length in hours
    @param outstep output frequency in seconds (an integer)

    @param kwargs Additional keyword arguments are passed to Oceanini subclass constructors."""
    assert(GFSDIR.find('pom/output')<0)
    if logger is None: logger=logging.getLogger('pom')
    if fcstlen is None: fcstlen=126 # forecast length in hours
    if outstep is None: outstep=86400
    if outstep<540: outstep=540
    printfrq = outstep/86400.0

    (y4,mm,mm1,dd,hh)=ysplitter(STARTDATE)
    BASIN=STORMID[2].upper()
    conf = kwargs.get('conf', None)
    if conf is None:
       WORKDIR = '/'.join(list(filter(lambda x: x != '', CSTREAM.split("/"))))
       WORKDIR = split(split(WORKDIR)[0])[0]
       WORKDIR = split(split(WORKDIR)[0])[0]
       CONFhwrf = "".join(["/",WORKDIR, "/com/",STARTDATE,"/",STORMID,"/storm1.conf"])
       if exists(CONFhwrf) and getsize(CONFhwrf) > 0:
          conf = hwrf.launcher.load(CONFhwrf)
       else:
          logger.error("%s does not exists" %(CONFhwrf))
          msg=("%s Missing or zero size" %(CONFhwrf))
          raise POMConfigError(msg)
    DOMAIN = choose_domain(BASIN, conf, logger=None)
    if DOMAIN is None:
        logger.critical("Domain selection Failed")
        msg=("Domain selection Failed")
        raise POMConfigError(msg)

    #conf_file = "".join([PARMhwrf,"/pom/", "pom.conf_tmpl"])
    #pom_conf = read_pom_conf(conf_file, logger=None)
    # Passing options in hwrf.conf [pom] section in
    init_data =  conf.getstr('pom','ini_data','gdem').upper()
    INDATA = init_data
    GEOVEL = conf.getint('pom','geovflag',1)
    SSTASIM = conf.getint('pom','assi_sst',1)
    PHY1D = conf.getint('pom','oned_pom',0)
    kppflag = conf.getint('pom','kppflag',0)
    kpp_ric = conf.getfloat('pom','kpp_ric',0.36)
    kpp_lt_log = conf.getstr('pom','kpp_lt_log','.false.')
    if  DOMAIN == "transatl":
        CENTERID = "NHC"
        if init_data == 'GDEM' or init_data == 'GDEM3':
            NL = 33
            ODATA = odata_d[DOMAIN][0]
            prep = fbtr("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                     STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN, **kwargs)
        elif init_data == 'NCODA':
            NL = 34
            ODATA = odata_d[DOMAIN][1]
            prep = na("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                       STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        elif init_data == 'RTOF':
            NL = 40
            ODATA = odata_d[DOMAIN][2]
            prep = rt("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                       STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        else:
            msg=("%s:OCEAN INIT DATA IS NOT Supported" %(init_data))
            logger.critical(msg)
            raise POMConfigError(msg)
        (DATA,DATA1) = prep.setpath()
        ymdh = STARTDATE
        infiles = pget()
        infiles.getinp(prep,DATA)
        vitfile = jn2r(VITDIR,"syndat_tcvitals." + y4)
        trackfile = jn2r(DATA1,"track.full")
        trackshort = jn2r(DATA1,"track")
        rmall(trackfile,trackshort,logger=logger)
        nvit = get_vitals(vitfile, CENTERID, STORMID, y4,trackfile)
        nvit = track_shorten(trackfile,trackshort,STARTDATE)
        if nvit <= 0:
            msg=("Did not find track info for %s. Write an empty message." %(STARTDATE))
            logger.warning(msg)
            with open(trackshort,'wt') as fo:
                fo.write("%s\n"%("000  000 00000     000000 0000 000 0000 000 000 0000 0000"))
        runexes = prun()
        if not runexes.setrun(prep,DATA):
            msg='setrun failed for L domain prep'
            logger.warning(msg)
            raise POMInitFailed(msg)
    elif DOMAIN == "eastpac" :
        CENTERID = "NHC"
        if init_data == 'GDEM' or init_data == 'GDEM3':
            NL = 73
            ODATA = odata_d[DOMAIN][0]
            prep = g3("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                       STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        elif init_data == 'NCODA':
            NL = 34
            ODATA = odata_d[DOMAIN][1]
            prep = na("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                   STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                   **kwargs)
        elif init_data == 'RTOF':
            NL = 40
            ODATA = odata_d[DOMAIN][2]
            prep = rt("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                       STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        else:
            msg=("%s:OCEAN INIT DATA IS NOT Supported" %(init_data))
            logger.critical(msg)
            raise POMConfigError(msg)
        (DATA,DATA1) = prep.setpath()
        ymdh = STARTDATE
        infiles = pget()
        infiles.getinp(prep,DATA)
        vitfile = jn2r(VITDIR,"syndat_tcvitals." + y4)
        trackfile = jn2r(DATA1,"track.full")
        trackshort = jn2r(DATA1,"track")
        rmall(trackfile,trackshort,logger=logger)
        nvit = get_vitals(vitfile, CENTERID, STORMID, y4,trackfile)
        nvit = track_shorten(trackfile,trackshort,STARTDATE)
        if nvit <= 0:
            msg=("Did not find track info for %s. Write an empty message." %(STARTDATE))
            logger.warning(msg)
            with open(trackshort,'wt') as fo:
                fo.write("%s\n"%("000  000 00000     000000 0000 000 0000 000 000 0000 0000"))
        runexes = prun()
        if not runexes.setrun(prep,DATA):
            msg='setrun failed for E domain prep'
            logger.warning(msg)
            raise POMInitFailed(msg)
    elif DOMAIN in ["westpac", "northind", "southind", "swpac", "sepac"]:
        CENTERID = "JTWC"
        if init_data == 'GDEM'or init_data == 'GDEM3':
            NL = 73
            ODATA = odata_d[DOMAIN][0]
            prep = g3("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                       STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        elif init_data == 'NCODA':
            NL = 34
            ODATA = odata_d[DOMAIN][1]
            prep = na("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                   STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                   **kwargs)
        elif init_data == 'RTOF':
            NL = 40
            ODATA = odata_d[DOMAIN][2]
            prep = rt("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                       STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        else:
            msg=("%s:OCEAN INIT DATA IS NOT Supported" %(init_data))
            logger.critical(msg)
            raise POMConfigError(msg)
        (DATA,DATA1) = prep.setpath()
        ymdh = STARTDATE
        infiles = pget()
        infiles.getinp(prep,DATA)
        vitfile = jn2r(VITDIR,"syndat_tcvitals." + y4)
        trackfile = jn2r(DATA1,"track.full")
        trackshort = jn2r(DATA1,"track")
        rmall(trackfile,trackshort,logger=logger)
        nvit = get_vitals(vitfile, CENTERID, STORMID, y4,trackfile)
        nvit = track_shorten(trackfile,trackshort,STARTDATE)
        if nvit <= 0:
            msg=("Did not find track info for %s. Write an empty message." %(STARTDATE))
            logger.warning(msg)
            with open(trackshort,'wt') as fo:
                fo.write("%s\n"%("000  000 00000     000000 0000 000 0000 000 000 0000 0000"))
        runexes = prun()
        if not runexes.setrun(prep,DATA):
            msg='setrun failed for W/A/S/P/X/O/B/U/T/Q domain prep'
            logger.warning(msg)
            raise POMUnsupportedBasin(msg)
    else:
        msg=("%s : Domain is NOT Supported" %(DOMAIN))
        logger.critical(msg)
        raise POMUnsupportedBasin(msg)

    diag = phase("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                 STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                 **kwargs)
    (DATA,DATA1) = diag.setpath("PHASE1")
    namelst=nml()
    #Comment out and try to use the outstep to control the output time step.
    #printfrq = pom_conf['pom.prtd1val']

    namelst("'MPIPOM-TC:"+STORMNAME+STORMID+"'", STORMNAME,
            6, 45, "'"+y4+"-"+mm+"-"+dd+" "+hh+":00:00 +00:00'",0,
            "'restart.phase0.nc'",2.0,2.0,printfrq,3,
            0, 22.4, 0.,GEOVEL,NL, PHY1D, 0, kppflag)

    namelst.make(DATA1)
    infiles.getinp(diag,DATA1)
    if not  runexes.setrun(diag,DATA1):
        msg='setrun failed for diag'
        logger.warning(msg)
        raise POMInitFailed(msg)
    outfiles = psend()
    set_ecflow_label('wake',"Starting...",logger)
    prog = phase("OCEAN",DOMAIN,ODATA,str(SSTASIM),INDATA,str(PHY1D),STORMNAME,STORMID,
                 STARTDATE,EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                 **kwargs)
    ymdh = dateplushours(ymdh, -72)
    (DATA,DATA2) = prog.setpath("PHASE2")
    (y4,mm,mm1,dd,hh)=ysplitter(ymdh)
    namelst("'MPIPOM-TC:"+STORMNAME+STORMID+"'", STORMNAME,
            6, 45, "'"+y4+"-"+mm+"-"+dd+" "+hh+":00:00 +00:00'",1,
            "'restart.phase1.nc'",3.0,3.0,printfrq,1,
            0, 22.4, 9999.,GEOVEL,NL, PHY1D, 0, kppflag)

    namelst.make(DATA2)
    if outfiles.sendout(diag,DATA1,"restart.0001.nc",DATA2,"restart.phase1.nc"):
        vitfile = jn2r(VITDIR,"syndat_tcvitals." + y4)
        trackfile = jn2r(DATA2,"track.full")
        trackshort = jn2r(DATA2,"track")
        rmall(trackfile,trackshort,logger=logger)
        nvit = get_vitals(vitfile, CENTERID, STORMID, y4,trackfile)
        nvit = track_shorten(trackfile,trackshort,STARTDATE)
        if nvit >= 2 and init_data != 'RTOF':
            infiles.getinp(prog,DATA2)
            if runexes.setrun(prog,DATA2):
                set_ecflow_label('wake',"Cold wake added successfully.",logger)
                outfiles.sendout(prog,DATA2,"restart.0001.nc",DATA,"restart.phase2.nc")
            else:
                set_ecflow_label('wake',"Cold wake init failed, will run without.",logger)
                outfiles.sendout(prog,DATA2,"restart.phase1.nc",DATA,"restart.phase2.nc")
        else:
            if init_data=='RTOF':
                set_ecflow_label('wake',"Using RTOFS cold wake.",logger)
            else:
                set_ecflow_label('wake',"No cold wake (too few vitals times).",logger)
            outfiles.sendout(prog,DATA2,"restart.phase1.nc",DATA,"restart.phase2.nc")
    else:
        msg='sendout failed'
        logger.warning(msg)
        raise POMInitFailed(msg)

    # Make a namelist for the forecast model as pom.nml in the top-level directory:
    namelst=nml()
    (y4,mm,mm1,dd,hh)=ysplitter(STARTDATE)
    namelst("'MPIPOM-TC:"+STORMNAME+STORMID+"'", STORMNAME,
            6, 30, "'"+y4+"-"+mm+"-"+dd+" "+hh+":00:00 +00:00'",1,
            "'restart.phase2.nc'",9999.,fcstlen/24.,printfrq,1,
            0, 22.4, 9999.,GEOVEL,NL, PHY1D, 0, kppflag)
    namelst.make('.')
    # Make a namelist for the forecast model as kpp.nml in the top-level directory:
    kpplst=kppnml()
    kpplst(kpp_lt_log,kpp_ric)
    kpplst.make('.')

    return True
