"""!This module contains tasks to prepare input for the GFDL Vortex
Tracker, run the tracker and deliver its results."""

##@var __all__
# List of symbols to export by "from hwrf.tracker import *"
__all__ = [ 'hwrf_combine_subset', 'tracker_subset', 'expandlatlon',
            'vinttave','TrackerTask','GRIB1VintTave' , 'jtwc_rewrite']

import re, shutil, os, tempfile, math, datetime, sys
import hwrf.regrib, hwrf.hwrftask, hwrf.numerics, hwrf.exceptions
import produtil.run, produtil.cd, produtil.fileop, produtil.datastore

from produtil.ecflow import set_ecflow_meter

from hwrf.numerics import TimeArray, to_fraction, to_datetime, \
    to_datetime_rel, partial_ordering, to_timedelta

from hwrf.regrib import GRIBOp, GRIB1Op
from produtil.run import runstr,bigexe,alias,checkrun
from produtil.cd import TempDir, NamedDir
from produtil.fileop import isnonempty, deliver_file

from hwrf.exceptions import TrackerError,TrackerModeError,TrackerStormError, \
    MissingGRIBError,GRIBLocationError

########################################################################

def jtwc_rewrite(inatcf,outatcf,logger=None):
    """!Rewrites track files as used by the HWRF WPAC parallels from 2013-2015
    @param inatcf input ATCF file
    @param outatcf output ATCF file
    @param logger a logging.Logger for log messages"""
    with open(inatcf,'rt') as inf:
        with open(outatcf,'wt') as outf:
            for line in inf:
                line=line.rstrip() \
                    .replace('AA','IO')\
                    .replace('BB','IO')\
                    .replace('SP','SH')\
                    .replace('SI','SH')\
                    .replace('-9,',' 0,')\
                    .replace('-99,','  0,')\
                    .replace('-999,','   0,')\
                    .replace('-9999,','    0,')\
                    [0:112]
                hour=line[30:33]
                ihour=int(hour)
                if ihour%3==0:
                    if logger is not None:
                        logger.info('ATCF LINE: %s'%(line,))
                    print>>outf,line
                elif logger is not None:
                    logger.info('SKIP LINE: %s'%(line,))


########################################################################

def hwrf_diagnostic_subset(wgribout,hurricane_region=False):
    """!Selects lines for the hwrfprs_c files.

    This subroutine parses an array of lines from wgrib -s and selects
    lines to send into the hwrfprs_c and hwrfsat_c GRIB1/2 output
    files (hurricane_region=True) or hwrfprs_g and hwrfsat_g files
    (hurricane_region=False).  Long ago (in 2011) this was the input
    to the tracker.  In the current system, it contains fields the
    tracker does not need, lacks some fields it does need, and is in
    the wrong order for it to be accepted as tracker input.  It
    contains synthetic satellite and other diagnostic quantities that
    are useful for various data analysis in the high resolution
    combined grid.  The tracker scripts no longer use this output set
    for any purpose, but it is still valuable for diagnostics.  This
    is in the hwrf.tracker module for historical reasons: the
    hwrfprs_c files were originally intended for the tracker.
    @param wgribout output from wgrib
    @param hurricane_region if True, a more limited set of variables is kept"""
    
    if hurricane_region:
        # Skip specific humidity to save space.  T and Z are handled
        # in a later loop.
        var3d=['UGRD','VGRD','RH','ABSV']
    else:
        # Include SPFH for direct comparison to GFS, and include RH
        # for comparison to obs.  Include Z and T since the later loop
        # does not add them.
        var3d=['UGRD','VGRD','RH','SPFH','ABSV','HGT','TMP']

    # Interesting levels of some popular variables:
    for lev in [ 1000, 850, 700, 500, 250 ]:
        for var in var3d:
            for line in wgribout:
                if line.find('%s:%d mb:'%(var,lev))>=0:
                    yield line

    # Extra levels for Z and T for diagnosing warm core when
    # hurricane_region is True:
    if hurricane_region:
        for lev in [ 250, 300, 350, 400, 450, 500, 600, 700, 850, 1000 ]:
            for var in ['HGT','TMP']:
                for line in wgribout:
                    if line.find('%s:%d mb:'%(var,lev))>=0:
                        yield line

    # A bunch of 2D fields:
    for line in wgribout: 
        if line.find(' mb:')<0 and \
                re.search('PRMSL|SBT|BRT|AMSRE|trop|HPBL|PWAT'
                          '|CAPE|CIN|column|REFC|HLCY|GRD:10 m above'
                          '|TPCP:sfc|TMP:2 m above|RH:2 m above',line):
            yield line

def hwrf_combine_subset(w):
    """!A simple wrapper around hwrf_diagnostic_subset that provides a
    subset of the HWRF fields and vertical levels suitable for
    analyzing the large-scale hurricane structure.
    @param w output from wgrib"""
    for line in hwrf_diagnostic_subset(w,True):
        yield line

def hwrf_global_subset(w):
    """!A simple wrapper around hwrf_diagnostic_subset that provides a
    subset of the HWRF fields and vertical levels suitable for
    analyzing the synoptic-scale or global fields and comparison to
    the parent global model.
    @param w output from wgrib"""
    for line in hwrf_diagnostic_subset(w,False):
        yield line

########################################################################

##@var TRACKER_SUBSET
#The subset of fields needed from an HWRF GRIB1 file in order to run
#the tracker.  This is used by the tracker_subset function.
TRACKER_SUBSET=[ 'HGT:925', 'HGT:850', 'HGT:700', 'UGRD:850', 'UGRD:700',
                 'UGRD:500', 'VGRD:850', 'VGRD:700', 'VGRD:500', 
                 'UGRD:10 m ', 'VGRD:10 m ', 'ABSV:850', 'ABSV:700',
                 'PRMSL', 'HGT:900', 'HGT:800', 'HGT:750', 'HGT:650',
                 'HGT:600', 'HGT:550', 'HGT:500', 'HGT:450', 'HGT:400',
                 'HGT:350', 'HGT:300', 'HGT:250', 'TMP:500', 'TMP:450',
                 'TMP:400', 'TMP:350', 'TMP:300', 'TMP:250' ]
"""The subset of fields needed from an HWRF GRIB1 file in order to run
the tracker.  This is used by the tracker_subset function."""

def tracker_subset(wgribout):
    """!This is a GRIB subsetter intended to be sent to the subset
    operator of an hwrf.regrib.GRIB1Op object, to produce GRIB1 files
    that contain only fields needed by the tracker.

    This function iterates over wgrib -s output lines.  It subsets the
    lines, returning the ones the tracker expects, in the exact order
    the tracker expects them.  The input is an array of lines from
    wgrib -s.  This is intended to be used with the gribtask and
    regrib modules by doing gribop/tracker_subset to produce a GRIB1Op
    that will make a tracker subset of the given GRIB1 file.
    @param wgribout output from wgrib"""
    for findme in TRACKER_SUBSET:
        for line in wgribout:
            if findme in line:
                yield line
                break

########################################################################

# expandlatlon - the tracker scripts read the lat-lon grid information
# and expand the grid by 2.5 or 4.0 degrees.  This allows mimicing the
# behavior:

def action_expandlatlon(op,regrib,name,center,west,east,north,south,n0,n1,
                        scan,res0,res1,**kwargs):
    """!Internal function that expands a lat-lon grid to make a bigger one.

    This is part of the implementation of expandlatlon and should not
    be called directly.  Given the input grid from center.grib1grid,
    expand the grid in all four compass directions by a specified
    number of degrees, and produce a new grid with a different
    resolution, scanning mode and gridpoint count

    @param op the hwrf.regrib.GRIBOp being run
    @param regrib the hwrf.regrib.Regrib for data storage
    @param name the grid name
    @param center the GRIB1 product being centered upon
    @param west,east,north,south how many degrees to add in each direction (a float)
    @param n0,n1 gridpoint counts for the GRIB1 grid 255
    @param scan scanning mode flags for the GRIB1 grid 255
    @param res0,res1 resolution information for GRIB1 grid 255
    @param kwargs Ignored."""
    ingrid=center.grib1grid
    if regrib.logger is not None:
        regrib.logger.info('expandlatlon of input grid %s'%(ingrid,))
    #255 0 501 401 22042 103995 136 10042 118995 30 30 0'
    oldgrid=[int(x) for x in ingrid.split()]
    #[255, 0,       501,      401,    21405, 116717,  136,  9405,   131717, 30,    30,     0]
    ( ogid, ojunk1, onscount, oewcount, onlat, oelon, oscan, oslat, \
          owlon, onsres, oewres, ojunk2 ) = oldgrid

    # Define a convenience function to simplify this code.  We need to
    # do int(round()) to mimic awk's math:


    if 0 != (oscan&128):   (owlon,oelon) = (oelon,owlon)
    if 0 != (oscan&64):    (onlat,oslon) = (oslon,onlat)
    if 0 != (oscan&32):  (onsres,oewres) = (oewres,onsres)

    (iwlon,ielon)=(owlon-int(1000*west), oelon+int(1000*east))
    (islat,inlat)=(oslat-int(1000*south),onlat+int(1000*north))

    iscan=int(scan)
    if 0 != (iscan&128): (iwlon,ielon) = (ielon,iwlon)
    if 0 != (iscan&64):  (inlat,islat) = (islat,inlat)
    if 0 != (iscan&32):  (n0,n1) = (n1,n0)

    s = '255 0 %d %d %d %d %d %d %d %d %d 0' % \
        ( int(n0), int(n1), inlat, ielon, iscan, islat, iwlon,
          int(1000*res0), int(1000*res1) )
    if regrib.logger is not None:
        regrib.logger.info('expandlatlon output grid is %s'%(s,))
    return hwrf.regrib.GRIBGrid(name,s,None)

def expandlatlon(center,west,east,north,south,n,scan,res,name=None):
    """!Creates a GRID suitable for use with the hwrf.regrib module.
    Given the input grid from center.grib1grid, expand the grid in all
    four compass directions by a specified number of degrees, and
    produce a new grid with a different resolution, scanning mode and
    gridpoint count
    @param center the hwrf.regrib.GRIBBase on which to center
    @param west,east,north,south number of degrees to expand 
    @param n a pair of ints, the GRIB1 grid 255 gridpoint count information
    @param scan the GRIB1 grid 255 scanning mode information
    @param res a pair of ints, the GRIB1 grid 255 resolution information
    @param name the name of the grid
    @returns a new hwrf.regrib.GRIBGridCompute that will compute
    the expanded grid for any forecast time for which the center
    object can provide inputs"""
    assert(len(n)==2 and n[0]>0 and n[1]>0)
    assert(scan>=0 and scan<=255)
    assert(len(res)==2 and res[0]>0 and res[1]>0)
    if name is None:
        name='elatlon%dx%dp'%(int(n[0]),int(n[1]))
    return hwrf.regrib.GRIBGridCompute(name,action_expandlatlon,center,
        west,east,north,south,n[0],n[1],scan,res[0],res[1])

########################################################################

class GRIB1VintTave(GRIB1Op): 
    """!A GRIB1Op that runs vint and tave: two of the tracker prep
    programs.  They interpolate data vertically and temporally to
    produce supplemented input GRIB files needed by the tracker
    thermodynamic diagnostic output fields."""
    def __init__(self,op):
        """!GRIB1VintTave constructor"""
        super(GRIB1VintTave,self).__init__(None,op)
        assert('_args' in self.__dict__)
    def make(self,regrib,**kwargs):
        """!This runs the actual vint and tave programs on the GRIB1
        file that comes from the argument provided to the constructor.
        @param regrib the hwrf.regrib.Regrib for data storage information
        @param kwargs passed to self.arg.make()"""
        ingrib=None
        for arg in self.args():
            ingrib=arg.make(regrib,**kwargs)
            break
        assert(ingrib is not None)
        return self.action_vinttave(regrib,ingrib,**kwargs)
    def action_vinttave(self,regrib,ingrib,time=None,task=None,
                        blocksize=1048576,atime=None,**kwargs):
        """!Performs the actual work of GRIB1VintTave.make: copying
        input, running vint and tave, copying output.  Do not call
        directly.
        @protected
        @param regrib The hwrf.regrib.Regrib for data storage information
        @param ingrib the input GRIBBase that provides input files
        @param time the time at which the data is being processed
        @param task the hwrf.gribtask.GRIBTask that stores final output
        @param blocksize a block size for I/O
        @param atime analysis time
        @param kwargs Ignored."""
        infile=ingrib.grib1file
        grid=ingrib.grib1grid
    
        if atime is None and task is not None and 'atime' in task.__dict__:
            atime=task.atime

        assert(grid is not None)
        assert(grid!='')
    
        # Request some temporary filenames for the numerous input and
        # output files from these programs:
        inindex = regrib.tempfile('grbindex')
        znamelist = regrib.tempfile('vint_input')
        tnamelist = regrib.tempfile('tint_input')
        tave_namelist = regrib.tempfile('tave_input')
        (zfile,zindex) = regrib.gribtemp('vint.z.grb1')
        (tfile,tindex) = regrib.gribtemp('vint.t.grb1')
        (tavefile,taveindex) = regrib.gribtemp('tave.grb1')
        (outfile,outindex) = regrib.gribtemp('vinttave')
        outgrbindex=regrib.tempfile('vinttave.ix')
    
        if regrib.logger is not None:
            regrib.logger.info('vint-tave starting: infile=%s '
                               'inindex=%s grid=%s'
                               %(repr(infile),repr(inindex),repr(grid)))
    
        # Get the executables:
        copygb=regrib.copygb
        wgrib=regrib.wgrib
        vint=alias(bigexe(task.getexe('vint')))
        tave=alias(bigexe(task.getexe('tave')))
        grbindex=alias(bigexe(task.getexe('grbindex')))
    
        logger=regrib.logger

        # And make some aliases to simplify the rest of this code:
        fl=produtil.fileop.fortlink #fl = make a symlink
        if logger is not None: 
            logger.info('ftime for vinttave is %s'%(str(time),))
        # fahr = forecast-analysis in hrs.
        fahr=task.conftimestrinterp('{fahr}',ftime=time,atime=atime) 
        def runit(cmd,runner=checkrun):
            if logger is not None: logger.info(repr(cmd))
            return runner(cmd)
    
        # Run grbindex on the input file:
        if regrib.logger is not None:
            regrib.logger.info('generate grbindex file %s'%(inindex,))
        runit(grbindex[infile,inindex])
    
        with open(znamelist,'wt') as f:
            nmltxt='&timein ifcsthour=%s, iparm=7 , gribver=1, '\
                'g2_jpdtn=0/\n'%(fahr,)
            f.write(nmltxt)
            if logger is not None: logger.info('znamelist: '+nmltxt)
        fl({11:infile, 16:task.confstr('hgt_levs'), 31:inindex, 51:zfile},
           force=True)
        runit(vint < znamelist)
        runit(grbindex[zfile,zindex])
    
        with open(tnamelist,'wt') as f:
            nmltxt='&timein ifcsthour=%s, iparm=11 , gribver=1, '\
                'g2_jpdtn=0/\n'%(fahr,)
            f.write(nmltxt)
            if logger is not None: logger.info('tnamelist: '+nmltxt)
        fl({11:infile, 16:task.confstr('tmp_levs'), 
            31:inindex, 51:tfile},force=True)
        runit(vint < tnamelist)
        runit(grbindex[tfile,tindex])
    
        with open(tave_namelist,'wt') as f:
            nmltxt='&timein ifcsthour=%s, iparm=11 , gribver=1, '\
                'g2_jpdtn=0/\n'%(fahr,)
            f.write(nmltxt)
            if logger is not None: logger.info(nmltxt)
        fl({11:tfile, 31:tindex, 51:tavefile},force=True)
        runit(tave < tave_namelist)
        runit(grbindex[tavefile,taveindex])
    
        dashg='-g%s'%(grid,)
        runit(copygb[dashg,'-k4*-1 33 100 850',infile,inindex,outfile+'.u850'])
        runit(copygb[dashg,'-k4*-1 33 100 700',infile,inindex,outfile+'.u700'])
        runit(copygb[dashg,'-k4*-1 33 100 500',infile,inindex,outfile+'.u500'])
        runit(copygb[dashg,'-k4*-1 33 105  10',infile,inindex,outfile+'.u10m'])
        runit(copygb[dashg,'-k4*-1  7 100 850',infile,inindex,outfile+'.z850'])
        runit(copygb[dashg,'-k4*-1  7 100 700',infile,inindex,outfile+'.z700'])
        runit(copygb[dashg,'-k4*-1  2 102   0',infile,inindex,outfile+'.mslp'])
        runit(copygb[dashg,                    zfile, zindex, outfile+'.phase'])
        runit(copygb[dashg,                tavefile,taveindex,outfile+'.tave'])

        with open(outfile,'wb') as outf:
            for ext in ['u850','u700','u500','z850','z700',
                        'mslp','u10m','phase','tave']:
                partname='%s.%s'%(outfile,ext)
                if logger is not None and not \
                        produtil.fileop.isnonempty(partname):
                    logger.warning(
                        '%s: input file is empty!  Things may break later.'
                        %(partname,))
                with open(partname,'rb') as inf:
                    shutil.copyfileobj(inf,outf)
    
        runit(grbindex[outfile,outgrbindex])
        if logger is not None:
            logger.info('vint-tave completed: infile=%s inindex=%s grid=%s'
                               %(repr(outfile),repr(outgrbindex),repr(grid)))
    
        retval=hwrf.regrib.GRIB1File(outfile,None,grid,
                                     grib1grbindex=outgrbindex)
        assert(retval is not None)
        return retval

##@var vinttave
# An alias for GRIB1VintTave
vinttave=GRIB1VintTave   # an alias for the old vinttave function
"""An alias for GRIB1VintTave for backward compatibility"""
    
########################################################################

class RawATCFProduct(produtil.datastore.FileProduct):
    """!A FileProduct for tracker output.

    This is part of the internal implementation of TrackerTask and
    should not be used directly.  It is a FileProduct that contains
    the raw, unmodified tracker output"""
    def deliver(self,tracker,fhr,logger=None):
        """!Copies the tracker output to its destination without
        modification.  Sets availability and calls callbacks.
        @param tracker the Task that ran the tracker
        @param fhr the forecast hour,ignored
        @param logger a logging.Logger for log messages"""
        infile='output.atcfunix'
        if logger is None: logger=tracker.log()
        deliver_file(infile,self.location,keep=True,logger=logger)
        self.available=True
        if not self.has_callbacks():
            logger.debug('%s: has no callbacks, so no DBN and no NHC delivery'%(self.did,))
        else:
            logger.info('%s: has callbacks.  Will call them.'%(self.did,))
        self.call_callbacks(logger=logger)

class CleanATCFSubsetProduct(produtil.datastore.FileProduct):
    """!A FileProduct that delivers a subset of a track.

    This is part of the internal implementation of TrackerTask and
    should not be used directly.  It is a FileProduct that delivers a
    cleaned-up, subsetted version of the tracker output."""
    def __init__(self,**kwargs):
        """!Create a CleanATCFSubsetProduct that will deliver a track
        with the specified last allowable forecast hour.
        @param kwargs passed to produtil.datastore.FileProduct.__init__()"""
        super(CleanATCFSubsetProduct,self).__init__(**kwargs)
        #assert(subset_fhr is not None)
        #if subset_fhr is not None:
        #    self['subset_fhr']=str(subset_fhr)
    def deliver(self,tracker,fhr,logger=None):
        """!Reads the tracker output, discarding everything after the
        specified forecast hour.  If an invalid line is found after
        time 0, that line and everything after it is discarded.

        @param tracker the task that ran the tracker
        @param fhr the last forecast hour to include
        @param logger a logging.Logger for messages"""
        if logger is None: logger=tracker.log()
        infile='output.atcfunix'
        def get(s,v):
            if s in self:
                try:
                    return int(self[s])
                except (KeyError,TypeError,ValueError) as e: 
                    return v
        fhr=get('subset_fhr',fhr)
        freq=get('subset_freq',1)
        cut=get('line_cut',None)

        with open(infile,'rt') as fin:
            with tempfile.NamedTemporaryFile(delete=False,
                mode='w+t',bufsize=1,prefix=str(self.location),dir='.') \
                as fout:
                outfile=fout.name
                ofstat=os.fstat(fout.fileno())
                os.fchmod(fout.fileno(),ofstat.st_mode|0644)
                for inline in fin:
                    (outline,stop)=self.process_line(
                        tracker,fhr,freq,cut,inline)
                    # Now continue parsing the remaining lines
                    if stop: break # current hour is past last print hour
                    if outline is not None:
                        fout.write(outline)

        logger=tracker.log()
        deliver_file(outfile,self.location,keep=False,logger=logger)
        self.available=True
        if not self.has_callbacks():
            logger.info('%s: has no callbacks, so no DBN and no NHC delivery'%(self.did,))
        else:
            logger.info('%s: has callbacks.  Will call them.'%(self.did,))
        self.call_callbacks(logger=logger)
    def process_line(self,tracker,lastfhr,freq,cut,line):
        """!Processes one line of the track file.  Decides if it is
        time to cease processing of the track file (cut off the track)
        or continue parsing.  

        @param tracker the task that ran the tracker
        @param lastfhr the last forecast hour to include
        @param freq the output frequency in hours.  This allows
         one to turn an hourly track into a six hourly track
        @param cut index of the last character to include from each line
          For example, 112 would only include up to the RMW field.
        @param line the line to process.
        @return A tuple (line,stop) where "line" is the line to append
        to the output track, and "stop" is true iff it is time to stop
        parsing."""
        try:
            fhr=int(line[29:33])
            wind=int(line[47:51])
            if wind<=0:
                if fhr>0:
                    tracker.log().error(
                        'Invalid wind in track file: wind is 0 at forecast '
                        'hour %s.  Will cut off the track file at this '
                        'forecast hour.'%(repr(fhr),))
                    return (None,True)
            elif freq>1 and abs(fhr%freq)>=1:
                pass # not on an output time
            elif cut is not None:
                return (line.rstrip()[0:cut]+'\n',fhr>lastfhr)
            else:
                return (line,fhr>lastfhr)
        except (ValueError,KeyError) as e:
            # Invalid line: forecast hour or wind are missing or invalid.
            rline=line.rstrip()
            # Send a short error at ERROR level for the jlogfile, and 
            # one at WARNING for the master log. 
            tracker.log().error('Ignoring invalid ATCF line: %s'%(rline,))
            tracker.log().warning('''Invalid ATCF line found.  Wind or forecast hour are missing or invalid.  
  Line: %s
  Error: %s''' 
                                % (rline,str(e)), exc_info=True)
        # We get here if there is invalid data at hour 0.  We do not
        # append the invalid line (None), and we do not stop
        # processing (False).
        return (None,False)

########################################################################
class TrackerView(produtil.datastore.Task):
    """!This Task is used by the delivery script executed by the gettrk
    program itself.  It derives directly from Task and has no
    knowledge of the surrounding HWRF workflow: it only knows about
    the local track filename, destination filenames and final forecast
    hours for each destination file."""
    def __init__(self,dstore,taskname,fhr,fmin,logger=None,**kwargs):
        """!Creates a TrackerView for the specified datastore and
        taskname.  It will deliver a file subsetted to the specified
        forecast hour and minute.  

        @param dstore the produtil.datastore.Datastore for database info
        @param taskname the task name in the database
        @param fhr,fmin the last expected forecast time
        @param logger The given logger is used to log
        messages.  
        @param kwargs Other keywoard arguments are passed to the
        produtil.datastore.Task.__init__() constructor."""
        super(TrackerView,self).__init__(dstore=dstore,taskname=taskname,
                                         logger=logger,**kwargs)
        self.fhr=int(fhr)
        self.fmin=int(fmin)

    ##@var fhr
    # Last expected forecast hour

    ##@var fmin
    # Last expected forecast minute
    def run(self):
        """!Performs the actual delivery.  Queries the database for
        all products for this task that match certain requirements.
        All of this task's RawATCFProduct products are delivered, as
        well as any CleanATCFSubsetProduct products that have the
        right forecast time.  Type name is obtained from the
        database's prodtype column in the PRODUCTS table.  It is
        assumed that the products for this task have a database id
        beginning with "taskname::" where "taskname" is this task's
        name.  These assumptions match what is done by the
        TrackerTask."""
        ds=self.dstore
        logger=self.log()
        work=list()
        me=self.taskname
        now=self.fmin/60.0
        set_ecflow_meter('tracker',int(self.fhr),logger)
        with ds.transaction() as t:
            prodids=t.query('SELECT id,type FROM products WHERE id LIKE "'
                            +self.taskname+'%"')
            for (prodid,prodtype) in prodids:
                logger.debug('%s: prodtype=%s'%(prodid,prodtype))
                m=re.match('(.*)::(.*)',prodid)
                if not m: 
                    logger.warning('%s: product id does not match '
                                   'category::prodname pattern'%(prodid,))
                (category,prodname)=m.groups()
                if category!=me:
                    logger.info('%s: not my track (want category %s got %s)'
                                %(prodid,me,category))
                elif prodtype=='RawATCFProduct':
                    prodobj=RawATCFProduct(dstore=ds,prodname=prodname,
                                           category=category)
                    work.append(prodobj.deliver)
                elif prodtype=='CleanATCFSubsetProduct':
                    try:
                        prodobj=CleanATCFSubsetProduct(
                            dstore=ds,prodname=prodname,category=category)
                        if prodobj.available:
                            logger.info('%s: already delivered.  Will '
                                        'not recopy.'%(prodid,))
                            continue
                        if 'subset_fhr' in prodobj:
                            subset_fhr=float(prodobj['subset_fhr'])
                            if subset_fhr>now:
                                logger.debug(
                                    '%s: not delivering yet.  Too early: '
                                    '%fhrs > %fhrs'%(prodid,subset_fhr,now))
                                continue
                            else:
                                logger.info(
                                    '%s: It is time to deliver now.  Deliver'
                                    ' at %fhrs, time is %fhrs'
                                    %(prodid,subset_fhr,now))
                        else:
                            logger.warning('%s: CleanATCFSubsetProduct has '
                                           'no subset_fhr.'%(prodid,))
                        work.append(prodobj.deliver)
                    except Exception as e:
                        logger.error('%s: unexpected exception: %s'%(
                                prodid,str(e)),exc_info=True)
                        raise
                else:
                    logger.warning(
                        '%s: unrecognized prod type "%s" (only '
                        'RAWATCFProduct or CleanATCFSubsetProduct allowed).'
                        '  Will not deliver this while gettrk is running.'
                        %(prodid,prodtype))
        if self.fmin<1e-3:
            now=self.fhr
        else:
            now=self.fmin/60.
        for func in work:
            func(self, now,logger=logger) 
        if abs(now)<1e-3 or ( now>0 and abs((now-6.0)%24.0)==0 ):
            self.postmsg('%s reached fhr=%s fmin=%s'
                         %(self.taskname,repr(self.fhr),repr(self.fmin)))
        logger.info('%s reached fhr=%s fmin=%s'
                    %(self.taskname,repr(self.fhr),repr(self.fmin)))

########################################################################

##@var tracker_delivery_script
# A short Python script run by the gfdl_vortextracker Fortran program
# to deliver the track file.  Do not use this directly.  It is passed
# through .format(...) to insert certain data.  Logging is set up to
# send INFO level to stdout and stderr, contrary to the usual
# settings.  This is intentional, to ensure delivery messages go to
# the master file when the job is split across multiple streams in the
# jhwrf_products job.

tracker_delivery_script='''#! /usr/bin/env python
import sys, logging
import produtil.setup, produtil.datastore
import hwrf.tracker
produtil.setup.setup(eloglevel=logging.INFO)
t=hwrf.tracker.TrackerView(
    dstore=produtil.datastore.Datastore("""{dstorepath}"""),
    taskname="""{taskname}""",
    fhr=int(sys.argv[1]),
    fmin=int(sys.argv[2]) )
t.log().info("In ./deliver script")
t.run()'''
"""The delivery script for the tracker.  This should not be used
directly: the TrackerTask sends it through .format(...) to insert
certain data."""

########################################################################
class TrackerTask(hwrf.hwrftask.HWRFTask):
    """!This task runs the GFDL Vortex Tracker on HWRF output.  It is
    intended to be used with the GRIBTask.  At present, this only
    supports a moving grid tracker.  Placeholders are present for a
    stationary grid tracker invocation, but those are not tested."""

    ##@var NOVITALS
    # Constant used to represent the lack of vitals data.
    NOVITALS=''
    def __init__(self,ds,conf,section,start,end,step,write_vit=True,
                 phaseflag=True,structflag=False,ikeflag=False,
                 realtime=True,masterlogger=None,**kwargs):
        """!Creates a new TrackerTask

        @param ds the produtil.datastore.Datastore to use
        @param conf the hwrf.config.HWRFConfig for configuration options
        @param section the HWRFConfig section to use
        @param start,end,step  the times at which data may be available
            from the GRIBTasks. 
        @param realtime the tracker should wait for data to appear and
            call delivery scripts at every forecast time.
        @param write_vit,phaseflag,structflag,ikeflag Passded on to the tracker namelist.
        @param kwargs passed to hwrf.hwrftask.HWRFTask.__init__()

        See the tracker documentation for details."""
        super(TrackerTask,self).__init__(ds,conf,section,**kwargs)
        self._flags={'write_vit':bool(write_vit),
                     'phaseflag':bool(phaseflag),
                     'structflag':bool(structflag),
                     'ikeflag':bool(ikeflag),
                     'realtime':bool(realtime)}
        self._vitals=dict()
        self._grids=dict()
        self._start=to_datetime(start)
        self._end=to_datetime_rel(end,start)
        self._step=to_timedelta(step)
        self._moving=None
        self._deliveries=list()
        self._products=dict()
        self._nml=hwrf.namelist.Conf2Namelist(conf=self.conf,section=self.section)
        self._gmodname=self._nml.nl_get('fnameinfo','gmodname')
        self._rundescr=self._nml.nl_get('fnameinfo','rundescr')
        self._nml.nl_set_if_unset('atcfinfo','atcfymdh',
                                  int(self._start.strftime('%Y%m%d%H')))
        if masterlogger is None:
            masterlogger=produtil.log.masterlogger
        self.masterlogger=masterlogger
    def send_raw_atcfunix(self,prodname,location,category=None):
        """!Requests delivery of the raw, unmodified atcfunix file to
        the specified location.  This creates a new RawATCFProduct
        with the specified product name and category.  If the category
        is unspecified or None, then this class's taskname will be
        used.  Note that the taskname MUST be used if running in
        realtime=True mode.
        @param prodname the name of the new product
        @param location the output file location, which will
           be sent through confstrinterp() to do string replacement
           on forecast and analysis times, and other variables
        @param category if specified, the product category.  By default,
           this task's taskname is used."""
        category=category if(category is not None) else self.taskname
        sloc=self.confstrinterp(location)
        prod=RawATCFProduct(dstore=self.dstore,
                prodname=prodname,category=category,location=sloc)
        prod.location=sloc
        self._deliveries.append((None, prod))
        self._products[prodname]=prod
        return prod
    def product(self,name):
        """!Returns the tracker Product with the specified product name
        or raises KeyError if none is found.
        @param name the product name"""
        return self._products[name]
    def products(self,name=None):
        """!Iterates over all tracker Products.
        @param name only iterate over this named product"""
        if name is not None and name in self._products:
            yield self._products[name]
        else:
            for (n,p) in self._products.iteritems(): yield p
    def send_atcfunix_subset(self,prodname,location,fhr=None,category=None,freq=1,cut=None):
        """!Requests delivery of a cleaned subset of the tracker
        atcfunix file, only up to the specified forecast hour.  This
        creates a new CleanATCFSubsetProduct with the specified
        product name and category.  If the category is unspecified or
        None, then this class's taskname will be used.  Note that the
        taskname MUST be used if running in realtime=True mode.
        @param prodname the name of the product
        @param location the delivery location, which will be sent
          through confstrinterp() to perform string replacement
        @param fhr the last forecast hour
        @param category the product category to use instead of self.taskname
        @param freq the output ATCF frequency in hours
        @param cut the last character to include in each line.  For example,
          112 is the RMW"""
        if fhr is None:
            fhr=int(to_fraction(self._end-self._start)/3600.0)
            assert(fhr>0)
        elif isinstance(fhr,datetime.timedelta):
            fhr=math.ceil(to_fraction(fhr)/3600.0)
        else:
            fhr=int(math.ceil(fhr))
        assert(fhr>=0 and fhr<=126) # FIXME: REMOVE
        category=category if(category is not None) else self.taskname
        sloc=self.confstrinterp(location)
        assert(fhr is not None)
        prod=CleanATCFSubsetProduct(dstore=self.dstore,
                prodname=prodname,category=category,location=sloc)
        prod['subset_fhr']=str(fhr)
        prod['subset_freq']=str(freq)
        if cut is not None:
            prod['line_cut']=str(cut)
        else:
            prod['line_cut']='9999'
        prod.location=sloc
        assert('subset_fhr' in prod)
        assert(prod['subset_fhr'] is not None)
        assert(prod['subset_fhr'] != '')
        #assert(float(prod['subset_fhr'])>=12)
        self._deliveries.append((fhr, prod))
        self._products[prodname]=prod
        return prod
    def send_atcfunix(self,prodname,location,category=None):
        """!Requests delivery of a cleaned-up version of the full
        length atcfunix file.  This is the same as calling
        send_atcfunix_subset with the full simulation length as the
        end time.  The prodname will be the Product's name, and the
        location is the delivery location.  If specified and not None,
        the category will be the Product's category, otherwise it will
        be this Task's taskname.  Note that if realtime=True is
        enabled, the category must be the taskname.
        @param prodname the new product's name
        @param location the delivery location
        @param category the product category instead of self.taskname"""
        return self.send_atcfunix_subset(prodname,location,
            self._end-self._start,category=category)
    def getwrite_vit(self): 
        """!Flag: output genesis vitals?"""
        return self._flags['write_vit']

    ##@property write_vit
    # Output genesis vitals?  Read-only property.
    write_vit=property(getwrite_vit,None,None,'Flag: output genesis vitals?')
    def getphaseflag(self): 
        """!Flag: output phase info?"""
        return self._flags['phaseflag']
    ##@property phaseflag
    # Output phase info?  Read-only property.
    phaseflag=property(getphaseflag,None,None,'Flag: output phase info?')
    def getstructflag(self): 
        """!Flag: output structure info?"""
        return self._flags['structflag']
    ##@property structflag
    # Output storm structure information?  Read-only property.
    structflag=property(getstructflag,None,None,
                        'Flag: output storm structure information?')
    def getikeflag(self): 
        """!Flag: output IKE info?"""
        return self._flags['ikeflag']

    ##@property ikeflag
    # Output storm IKE information?  Read-only property.
    ikeflag=property(getikeflag,None,None,
                     'Flag: output storm IKE information?')
    def getrealtime(self): 
        """!Flag: run in real-time mode?"""
        return self._flags['realtime']

    ##@property realtime
    # Run in real-time mode?  Read-only property.
    realtime=property(getrealtime,None,None,'Flag: run in real-time mode?')

    def grab_gribs(self,gribtask,gribname):
        """!Loops through the given gribtask, getting all products with
        specified name (gribname) for the times requested in the
        TrackerBase constructor.  Will raise MissingGRIBError if any
        times have no product.  Returns a TimeArray of Product
        objects.
        @param gribtask the hwrf.gribtask.GRIBTask to query
        @param gribname the grib product name"""
        gribs=TimeArray(self._start,self._end,self._step)
        dt=to_fraction(self._step)/10
        for rtime in gribs.times():
            for ptime,product in gribtask.products(name=gribname,time=rtime,
                                                   yieldtime=True):
                timediff=abs(to_fraction(ptime-rtime,negok=True))
                self.log().debug('%s timediff %s at time %s'%
                                 (gribname,str(timediff),rtime.strftime(
                                 '%Y%m%d.%H%M%S')))
                if timediff>dt:
                    self.log().debug('%s: no %s at time %s'%
                                    ( gribtask.taskname,gribname,
                                      rtime.strftime('%Y%m%d.%H%M%S') ))
                    continue
                else:
                    self.log().debug('%s: found %s at time %s'%
                                    ( gribtask.taskname,gribname,
                                      rtime.strftime('%Y%m%d.%H%M%S') ))
                    gribs[rtime]=product
                    break
        return gribs

    def add_moving_grid(self,tcvitals,gribtask,gribname):
        """!Tells the tracker to run in moving grid mode, for the
        specified storm.  

        @param tcvitals The tcvitals must be an hwrf.storminfo.StormInfo.  
        @param gribtask The gribtask must be an hwrf.gribtask.GRIBTask.  
        @param gribname The gribname is the name in the
        hwrf.regrib.RegribMany of the operation that produces the
        tracker input for this moving storm."""
        if self._moving is False:
            raise TrackerModeError(
                'Cannot mix stationary regional and moving grids in one '
                    'tracker invocation.')
        if self._moving is True:
            raise TrackerModeError(
                'Cannot have multiple moving grids: the current tracker '
                'implementation does not support that.  Use multiple '
                'TrackerTask objects instead.')
        if self._storminfo is None: self._storminfo=tcvitals
        self._moving=True
        gribs=self.grab_gribs(gribtask,gribname)
        stid=tcvitals.longstormid
        if stid in self._grids or stid in self._vitals:
            raise TrackerStormError(
                '%s: More than one of this storm specified.'%(stid,))
        try:
            self._grids[stid]=gribs
            self._vitals[stid]=tcvitals
        except Exception:
            if stid in self._grids: del self._grids[stid]
            if stid in self._vitals: del self._vitals[stid]
            raise

    def add_regional_grid(self,gribtask,gribname):
        """!Adds a stationary grid on which to run the tracker.  
        @param gribtask The gribtask must be an hwrf.gribtask.GRIBTask
        to create the input.
        @param gribname The gribname is the operation name in the grib task's
        hwrf.regrib.RegribMany for the GRIB data source."""
        if self._moving is True:
            raise TrackerModeError(
                'Cannot mix stationary regional and moving grids in one '
                'tracker invocation.')
        self._moving=False
        gribs=self.grab_gribs(gribtask,gribname)
        self._grids[self.NOVITALS]=gribs

    def add_storm(self,tcvitals):
        """!Specifies the tcvitals for a storm to track.
        @param tcvitals the hwrf.storminfo.StormInfo to use"""
        if self._moving is None:
            raise TrackerModeError(
                'Specify tracker mode before adding storms.  Call '
                'add_moving_grid or add_regional_grid before add_storm')
        if self._moving is True:
            raise TrackerModeError(
                'In moving grid mode, storms must have associated grid '
                'data.  Use add_moving_grid instead of add_storm.')
        self._vitals[tcvitals.longstormid]=tcvitals

    def link_grib(self,inputlist):
        """!Symbolically links all input GRIB1/2 files to the current
        working directory using the file naming convention expected by
        the tracker.  If self.realtime=False, then will also check for
        the files' existence and will raise MissingGRIBError if one is
        missing.  If self.realtime=True, then it is assumed the
        tracker is being run in waitfor mode and will wait for the
        data on its own.  In either case, GRIBLocationError will be
        raised if any GRIB's Product.location is None or '' (the
        location must be known to make the symlink).
        @param inputlist a file to create with the list of tracker
          input files"""
        logger=self.log()
        for stid in self._grids.keys():
            strstid=str(stid)
            logger.info('process storm %s'%(strstid,))
            if stid in self._vitals:
                longstormid=str(self._vitals[stid].longstormid)
            else:
                longstormid='regional'
            grids=self._grids[stid]
            atcfymdh=int(self._nml.nl_get('atcfinfo','atcfymdh'))
            linkstart='%s.%s.%s.%10d.f' % \
                (self._gmodname,self._rundescr,longstormid,atcfymdh)
            for (ftime,grib) in grids.iteritems():
                logger.info('storm %s time %s'%(strstid,str(ftime)))
                if not self.realtime:
                    while not grib.check():
                        logger.info('%s: not yet available.  Sleeping.')
                        time.sleep(20)
                loc=grib.location
                locix=grib.grib1grbindex
                if loc is None or loc=='':
                    raise GRIBLocationError(
                        '%s: no location specified for this GRIB product.'
                        %(grib.did,))
                minutes=int(to_fraction(ftime-self._start)/60)
                produtil.fileop.make_symlink(loc,'%s%05.5d'%(
                        linkstart,minutes),logger=logger)
                if locix is None or locix=='':
                    logger.info(
                        '%s: no grbindex file location specified for this '
                        'GRIB product.  Assuming .grbindex extension of '
                        'the GRIB file.'%(grib.did,))
                    locix='%s.grbindex'%(loc,)
                produtil.fileop.make_symlink(
                    locix,'%s%05.5d.ix'%(linkstart,minutes),logger=logger)
        with open(inputlist,'wt') as f:
            i=0
            for ftime in self._grids[self._grids.keys()[0]].datatimes():
                i+=1
                minute=int(to_fraction(ftime-self._start)/60)
                f.write("%4d %5d\n"%(i,minute))

    def concat_grib(self,filename,ixfilename):
        """!Concats all input GRIB1/2 files into a single file, waiting
        for each one to become available first.  Produces the grbindex
        output for that file at the end.  Only works in regional grid
        mode, and will raise TrackerModeError if the mode is unknown
        or moving grid.

        @note This is routine is untested because it is a bad idea.
        It results in duplication of an enormous amount of data.
        However, historically, this is how the tracker used to be
        run.
        @param filename the input GRIB1 file name
        @param ixfilename the output index file name"""
        with open(filename,'wb') as outf:
            outf.truncate(0)
        for product in self._gribs:
            while not product.check():
                self.log().info('%s: not yet available.  Sleeping...'
                                %(product.did))
                time.sleep(20)
            with open(filename,'ab') as outf:
                with open(str(product.location()),'rb') as inf:
                    shutil.copyfileobj(inf,outf)
        grbindex=alias(bigexe(self.getexe('grbindex')))
        checkrun(grbindex['filename','ixfilename'])

    def make_namelist(self,filename):
        """!Construct the tracker namelist and write it to the
        specified file.  The "realtime" flag is True when running in
        real-time mode, and False if not.
        @param filename the output namelist file's name"""
        realtime=bool(self.realtime)

        # Make some aliases to shorten the below code:
        nml=self._nml             # the Conf2Namelist object with the namelist
        tm=self._start            # the datetime with the start time
        ns=nml.nl_set             # force namelist to have certain values
        nlsiu=nml.nl_set_if_unset # provide default values if missing

        ns('datein','inp%bcc',int(tm.year)/100)
        ns('datein','inp%byy',int(tm.year)%100)
        ns('datein','inp%bmm',int(tm.month))
        ns('datein','inp%bdd',int(tm.day))
        ns('datein','inp%bhh',int(tm.hour))
        nlsiu('datein','inp%modtyp','regional')
        nlsiu('datein','inp%lt_units','hours')
        ns('datein','inp%file_seq','multi')
        ns('datein','inp%nesttyp',
                   'moveable' if(self._moving) else 'stopped')
        nlsiu('atcfinfo','atcfnum',81)
        nlsiu('atcfinfo','atcfname','TEST')
        
        hours=int(to_fraction(self._step)/3600)
        minutes=int(to_fraction(self._step)/60-60*hours)
        ns('atcfinfo','atcffreq',hours*100+minutes)
        nlsiu('trackerinfo','trkrinfo%type','tracker')
        nlsiu('trackerinfo','trkrinfo%mslpthresh',0.0015)
        nlsiu('trackerinfo','trkrinfo%v850thresh',1.5000)
        nlsiu('trackerinfo','trkrinfo%gridtype','regional')
        nlsiu('trackerinfo','trkrinfo%contint',100.0)
        nlsiu('trackerinfo','trkrinfo%out_vit','y' if(self.write_vit) else 'n')
        nlsiu('trackerinfo','trkrinfo%gribver',1)
        nlsiu('trackerinfo','trkrinfo%g2_jpdtn',0)
        nlsiu('phaseinfo','phaseflag','y' if(self.phaseflag) else 'n')
        nlsiu('phaseinfo','phasescheme','both')
        nlsiu('phaseinfo','wcore_depth',1.0)
        nlsiu('structinfo','structflag','y' if (self.structflag) else 'n')
        nlsiu('structinfo','ikeflag','y' if(self.ikeflag) else 'n')

        for stid in self._grids.keys():
            if stid in self._vitals:
                longstormid=str(self._vitals[stid].longstormid)
            else:
                longstormid='regional'
            break
        ns('fnameinfo','atcfdescr',longstormid)
        nlsiu('verbose','verb',3)
        ns('waitinfo','use_waitfor', ('y' if(realtime) else 'n') )
        nlsiu('waitinfo','wait_min_age',10)
        nlsiu('waitinfo','wait_min_size',100)
        nlsiu('waitinfo','wait_max_wait',1800)
        nlsiu('waitinfo','wait_sleeptime',5)

        if self.realtime:
            ns('waitinfo','use_per_fcst_command','y')
            ns('waitinfo','per_fcst_command',"./deliver %[FHOUR] %[FMIN]")
        else:
            ns('waitinfo','use_per_fcst_command','n')
        
        # Intel Fortran is fussy about the order of the namelists and
        # the order of the values in the namelist, so we order them
        # here:
        order=['datein','atcfinfo','trackerinfo','phaseinfo','structinfo',
               'fnameinfo','verbose','waitinfo']
        Odatein=['inp%bcc','inp%byy','inp%bmm','inp%bdd','inp%bhh',
                 'inp%model','inp%modtyp','inp%lt_units','inp%file_seq',
                 'inp%nesttyp']
        Oatcfinfo=['atcfnum','atcfname','atcfymdh','atcffreq']
        Otrackerinfo=['trkrinfo%westbd','trkrinfo%eastbd',
                      'trkrinfo%northbd','trkrinfo%southbd',
                      'trkrinfo%type','trkrinfo%mslpthresh',
                      'trkrinfo%v850thresh','trkrinfo%gridtype',
                      'trkrinfo%contint','trkrinfo%out_vit']
        Ophaseinfo=['phaseflag','phasescheme','wcore_depth']
        Ostructinfo=['structflag','ikeflag']
        Ofnameinfo=['gmodname','rundescr','atcfdescr']
        Owaitinfo=['use_waitfor','wait_min_age','wait_min_size',
                   'wait_sleeptime','use_per_fcst_command',
                   'per_fcst_command']
        varorder={ 'datein':partial_ordering(Odatein),
                   'atcfinfo':partial_ordering(Oatcfinfo),
                   'trackerinfo':partial_ordering(Otrackerinfo),
                   'phaseinfo':partial_ordering(Ophaseinfo),
                   'structinfo':partial_ordering(Ostructinfo),
                   'fnameinfo':partial_ordering(Ofnameinfo),
                   'verbose':partial_ordering(['verb']),
                   'waitinfo':partial_ordering(Owaitinfo)        }
        with open(str(filename),'wt') as f:
            f.write(nml.make_namelist(section_sorter=partial_ordering(order),
                                      var_sorters=varorder))

    def run(self):
        """!Runs the tracker and delivers the resulting track files."""
        logger=self.log()
        gettrk=alias(bigexe(self.getexe('gettrk')))
        realtime=bool(self.realtime)
        # Update the database to say that no tracks are delivered
        for (f,p) in self._deliveries:
            p.available=False
        wd=self.workdir
        assert(wd)
        if os.path.exists(wd):
            shutil.rmtree(wd)
        with NamedDir(wd):
            # Link all input GRIB files and create the input list file:
            logger.info('link tracker input')
            self.link_grib('input.fcsttime')

            with open('deliver','wt') as f:
                f.write(tracker_delivery_script.format(
                        dstorepath=self.dstore.filename, 
                        taskname=self.taskname))
                ofstat=os.fstat(f.fileno())
                os.fchmod(f.fileno(),ofstat.st_mode|0755)

            # Create the tcvitals input file:
            with open('input.vitals','wt') as vitf:
                for stid,vital in self._vitals.iteritems():
                    vitf.write(vital.as_tcvitals())

            # Four files must exist and be empty, so we create them here:
            # 13, 14: used for genesis vitals, unused here
            # 11, 31: used for single combined input GRIB, unused here
            for empty in [13,14,11,31]:
                with open('fort.%d'%(empty,),'wb') as f:
                    f.truncate(0)

            # Make the tracker namelist:
            logger.info('make tracker namelist')
            self.make_namelist('input.nml')
            assert(isnonempty('input.nml'))

            # Prefix for all output files:
            pre='output'

            # Generate the list of fort.* files to link:
            linkme={
                15:'input.fcsttime',
                12:'input.vitals',
                61:'%s.all'%(pre,),
                62:'%s.atcf'%(pre,),
                63:'%s.radii'%(pre,),
                64:'%s.atcfunix'%(pre,),
                66:'%s.atcf_gen'%(pre,),
                68:'%s.atcf_sink'%(pre,),
                69:'%s.atcf_hfip'%(pre,),
                }

            if self.write_vit: linkme[67]='%s.genvitals'%(pre,)
            if self.phaseflag: linkme[71]='%s.cps_parms'%(pre,)
            if self.ikeflag:   linkme[74]='%s.ike'%(pre,)

            if self.structflag:
                linkme[72]='%s.structure'%(pre,)
                linkme[73]='%s.fractwind'%(pre,)
                linkme[76]='%s.pdfwind'%(pre,)

            for forti in sorted(linkme.keys()):
                logger.info('Link: fort.%d = %s'%(forti,linkme[forti]))

            # Link fort.* files:
            produtil.fileop.fortlink(linkme,force=True)

            # Run the tracker:
            self.postmsg('Starting gettrk.')
            cmd=(gettrk < './input.nml' )
            if self.redirect: 
                cmd = cmd >= 'tracker.log'
            self.masterlogger.info(repr(cmd)) 
            try: 
                checkrun(cmd) 
                self.postmsg('Successful return status from gettrk.') 
            except(Exception) as e: 
                logger.critical('GETTRK FAILED: '+str(e),exc_info=True) 
                raise 

            # Copy tracker log file to outdir:
            if self.redirect:
                tgt=os.path.join(self.outdir,'tracker.log')
                deliver_file('tracker.log',tgt,keep=True,logger=logger)

            self.call_completed_callbacks()

    def call_completed_callbacks(self,logger=None):
        """!Calls callbacks for all completed products.
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=self.log()
        # Run callbacks for all delivered products:
        for prod in self.products():
            prod.update() # invalidate cache, recheck DB
            if prod.available:
                if not prod.has_callbacks():
                    self.masterlogger.info('%s: rejoice: completed!! (no '
                                   'callbacks)'%(prod.did,))
                else:
                    self.masterlogger.info('%s: rejoice: completed!! (has '
                                   'callbacks)'%(prod.did,))
                prod.call_callbacks()
            else:
                logger.error('%s: tracker product is not complete.'
                             %(prod.did,))
