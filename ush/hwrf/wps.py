"""!This module contains Tasks to run the WRF Preprocessing System
(WPS): Geogrid, Ungrib and Metgrid.  

This module contains wrappers round the WRF Preprocessing System
executables ungrib.exe, metgrid.exe and geogrid.exe.  The Geogrid and
Metgrid classes handle their corresponding programs, while there are
two options for ungrib.

The Ungrib and SteppingUngrib are wrappers around ungrib which
function in different ways.  The Ungrib class runs ungrib on all GRIB
files at the same time, after all are available (the "traditional
way").  The SteppingUngrib runs ungrib on one file at a time, allowing
it to run in parallel with the parent model."""

##@var __all__
# Ensures nothing is exported by "from hwrf.wps import *"
__all__=[] # prevent accidental "from hwrf.wps import *"

import os, shutil, collections, glob, time, math, re, itertools
import string, urlparse, datetime, collections

import hwrf.namelist
import hwrf.exceptions

from produtil.ecflow import set_ecflow_meter

import produtil.fileop
import produtil.run
import produtil.locking

from hwrf.hwrftask      import HWRFTask
from hwrf.exceptions    import WPSError, UngribSubsetError, GeogridNoOutput, \
    TimestepTooShort, GeogridNoLog, UngribNoInput, UngribInputUnknown
from hwrf.numerics      import partial_ordering, TimeArray, to_timedelta, \
    to_datetime_rel, to_fraction, to_datetime
from produtil.datastore import FileProduct, COMPLETED, FAILED, RUNNING
from produtil.cd   import TempDir, NamedDir
from produtil.fileop    import *
from produtil.run       import runstr,checkrun,alias,bigexe
from collections        import defaultdict

########################################################################

##@var _wps_namelist_order 
# An hwrf.numerics.partial_ordering for sorting the WPS namelists
_wps_namelist_order = partial_ordering([
            'share',
            'geogrid',
            'ungrib',
            'metgrid',
            'mod_levs',
        ])

##@var _wps_nl_var_order
# A mapping from namelist name to hwrf.numerics.partial_ordering
# objects that sort namelist variables within each namelist.
_wps_nl_var_order = {
        'share' : partial_ordering([
                    'wrf_core',
                    'max_dom',
                    'start_date',
                    'end_date',
                    'max_dom',
                    'interval_seconds',
                    'io_form_geogrid',
                    'io_form_metgrid',
                    'nocolons',
                ]),
        'geogrid' : partial_ordering([
                    'parent_id',
                    'parent_grid_ratio',
                    'i_parent_start',
                    'j_parent_start',
                    'e_we',
                    'e_sn',
                    'geog_data_res',
                    'dx',
                    'dy',
                    'map_proj',
                    'ref_lat',
                    'ref_lon',
                    'geog_data_path',
                    'opt_geogrid_tbl_path',
                    'ref_x',
                    'ref_y',
                ]),
        'ungrib' : partial_ordering([
                    'out_format',
                    'prefix',
                ]),
        'metgrid' : partial_ordering([
                    'fg_name',
                    'io_form_metgrid',
                    'opt_metgrid_tbl_path',
                ]),
        'mod_levs' : partial_ordering([
                    'press_pa',
                ]),
        }

########################################################################

class WPSTask(HWRFTask):
    """!This subclass of HWRFTask represents a WPS Task.  Multiple WPS
    jobs run in the same work directory.  This class allows the jobs
    to be represented together as a set, with one work directory
    specified at the top level.  This class exists only to reduce code
    duplication by letting Metgrid, Geogrid and Ungrib share a
    superclass."""
    def __init__(self, dstore, conf, section, sim, domains, taskname=None, 
                 wpsdir=None, starttime=None, increment=None, endtime=None,
                 parent_atime=None, geogrid_from=None, **kwargs):
        """!Create a new WPSTask.

        @param dstore The produtil.datastore.Datastore for database information
        @param conf the hwrf.config.HWRFConfig for configuration info
        @param section the section to use in conf
        @param sim The "sim" is the
        WRFSimulation for which we are preparing input.  
        @param domains The domains must be a list of WRFDomain
        objects. 
        @param taskname  Optional: the name of this Task.  Default: config section name.
        @param wpsdir Optional: the directory in which to run WPS.
             Default: taskname subdirectory of conf.getdir("WORKhwrf")

        @param starttime,endtime Optional: simulation length.  Default: same as sim.
        @param increment Optional: boundary input interval.  Default: get from sim.
        @param parent_atime Optional: analysis time of parent model.  Default: starttime
        @param geogrid_from Optional: a Geogrid object whose output
             should be linked before running this WPS step.  This is
             used when the WPS Ungrib and Metgrid are run more than
             once on the same domain.  For example, one may run
             multiple analysis times or multiple forecast lengths off
             of the same geogrid output.
        @param kwargs Other options are sent to the hwrf.hwrftask.HWRFTask.__init__ constructor.
        """
        if taskname is None: taskname=section
        super(WPSTask,self).__init__(dstore,conf,section,taskname,**kwargs)
        self.location=self.workdir
        self._sim=sim
        # Get WRF's copy of the domains so we have the parent-nest info:
        self._domains=[sim[domain] for domain in domains]
        self._products=None
        self._geogrid_from=None
        if geogrid_from is not None:
            if not isinstance(geogrid_from,Geogrid):
                raise TypeError(
                    'The geogrid_from parameter to WPSTask.__init__ must '
                    'be a Geogrid.  You provided a %s %s.'
                    %(type(geogrid_from).__name__,repr(geogrid_from)))
            self._geogrid_from=geogrid_from
        self.nl = hwrf.namelist.Conf2Namelist(conf, self.confstr('namelist'))
        if starttime is None:  starttime=sim.simstart()
        self.starttime=to_datetime(starttime)
        if endtime is None: endtime=sim.simend()
        self.endtime=to_datetime_rel(endtime,starttime)
        self.increment=increment if increment is not None else 6*3600
        self.increment=to_timedelta(self.increment)
        if parent_atime is None:
            parent_atime=starttime
        self.parent_atime=parent_atime
        if self.increment<to_timedelta(300):
            raise TimestepTooShort(
                'Geogrid timestep %s is smaller than 300 seconds.'
                %(repr(self.increment),))
        self.log().debug('%s times: start=%s end=%s increment=%s'%(
                self.taskname, str(self.starttime), str(self.endtime),
                str(self.increment)))
        self._wps_namelist()
        self.make_products()

    ##@var nl 
    # an hwrf.namelist.Conf2Namelist with the namelist.wps information

    ##@var starttime
    # Simulation start time.

    ##@var endtime
    # Simulation end time.

    ##@var increment
    # Simulation boundary input interval

    ##@var parent_atime
    # Parent model analysis time
    
    def _guess_nocolons(self): 
        """!Guesses whether we should use colons in filenames.  This
        information is obtained from sim.get_nocolons().  Note that
        this requires re-scanning all input and output io_forms, so
        this function is moderately expensive."""
        nc=self._sim.get_nocolons()
        return nc

    ##@property nocolons
    # Read-only property that guesses whether colons should be
    # omitted from filenames (True) or not (False)
    nocolons=property(_guess_nocolons,None,None, \
        """A property that is True if WPS will omit colons from the
        output filenames, and False otherwise.""")

    def io_suffix(self,stream):
        """!Guesses the file suffix (.nc, etc.) for the specified stream.
        
        Tries to guess what io suffix WPS will use for a stream.  If
        the stream is not one of the known streams, this function logs
        a warning and tries to continue, but will likely fail.

        @param stream the stream of interest.
        """
        if(stream=='metgrid' or stream=='auxinput1'):
            return self._sim.get_io_suffix('auxinput1')
        elif(stream=='geogrid' or stream=='auxinput2'):
            return self._sim.get_io_suffix('auxinput2')
        else:
            self.log().warning(
                'caller requested unknown stream "%s" in WPSTask.io_suffix'
                %(stream,))
            return self._sim.get_io_suffix('input')

    def _maxdom(self):
        """!The number of WRF domains, and highest domain number."""
        return self._sim.maxdom()

    ##@property maxdom
    # The number of WRF domains.
    maxdom=property(_maxdom,None,None,"The number of WRF domains.")
    def _MOAD(self): 
        """!The mother of all domains as a WRFDomain."""
        return self._sim.get(1)

    ##@property MOAD
    # The Mother Of All Domains (MOAD) as an hwrf.wrf.WRFDomain
    MOAD=property(_MOAD,None,None,"Returns the Mother of All Domains (MOAD)")

    def getsim(self): 
        """!The WRFSimulation object for which we are preparing input."""
        return self._sim
    
    def check_geogrid(self):
        """!Checks to see if the geogrid MOAD output file is present
        and non-empty in the current working directory.  Raises
        GeogridNoOutput if the output is missing or empty."""
        suffix=self.io_suffix('geogrid')
        filename='geo_nmm.d01.%s'%(suffix,)
        if not produtil.fileop.isnonempty(filename):
            raise GeogridNoOutput('%s: does not exist or is empty'
                                  %(filename,))

    def link_geogrid(self):
        """!Links geogrid output from the specified Geogrid task to the
        current working directory.  Will raise an exception if Geogrid
        has not produced its outputs yet.  This is used when multiple
        Metgrid executions are used with the same Geogrid.  The
        geogrid only needs to be run once since its outputs do not
        change with time, just with domain configuration.

        Specifically, this finds all Product objects in task
        self._geogrid_from whose prodname contains "geo" and links
        from the product's location to its prodname in the current
        working directory.  This logic must match the logic in
        Geogrid.make_products and WPSTask.check_geogrid."""

        logger=self.log()
        fromtask=self._geogrid_from
        if fromtask is None:
            logger.info('Not linking geogrid inputs.  I hope you already '
                        'ran geogrid in this directory.')
            return
        else:
            logger.info('Task %s linking geo products from %s task %s'
                        %(self.taskname,type(fromtask).__name__,
                          fromtask.taskname))

        for product in fromtask.products():
            did=product.did
            name=product.prodname
            if name.find('geo')>=0:
                logger.info(did+': should copy to '+name)
            else:
                logger.info(did+': does not contain "geo" so skipping this.')
                continue

            # We need to link this file.  Make sure it looks good first:
            loc=product.location
            av=product.available
            msg=None
            if not av:
                msg=did+': product is not available'
            elif loc is None or loc=='':
                msg=did+': product has no location (but is available)'
            elif not os.path.exists(loc):
                msg=did+': file does not exist: '+repr(loc)
            elif not produtil.fileop.isnonempty(loc):
                msg=did+': file is empty: '+repr(loc)
            if msg is not None:
                logger.warning(msg)
                raise GeogridNoOutput(msg)

            produtil.fileop.make_symlink(
                loc,name,force=True,logger=logger)
    def times(self):
        """!Iterates over all output times."""
        now=self.starttime
        end=self.endtime
        dt=self.increment
        while not (now>end):
            yield now
            now+=dt

    ##@property sim
    # Read-only property: the hwrf.wrf.WRFSimulation for this WPSTask
    sim=property(getsim,None,None,
                 """Returns the WRF simulation for this WPSTask""")
    def domains(self):
        """!Iterates over the domains in this WPSTask"""
        for domain in self._domains: yield domain
    def link_fix(self,geog_data=False,table=None):
        """!Links all fix files for ungrib to the current working directory.
        @param geog_data if True, also link the geog-data directory
        @param table name of the table file symbolic link"""
        logger=self.log()
        try:
            if table:
                tbl=self.confstr('tbl')
                tblhere=str(table)
                logger.info('Link table file %s to %s'%(tblhere,tbl))
                make_symlink(tbl,tblhere,force=True,logger=logger)
            if geog_data:
                make_symlink(self.getdir('geog_data'),'geog-data',force=True,
                             logger=logger)
        except Exception as e:
            # No need for exc_info here since the run() jobs add that already.
            logger.warning('cannot link to WPS fix files: '+str(e))
            raise
    def make_product_structure(self,stream):
        """!Generates the self._products data structure used by the
        products method.  Should be called by make_products in
        subclasses."""
        if stream=='metgrid' and self.endtime == self.starttime:
            self._products=collections.defaultdict(
                lambda: hwrf.numerics.TimeArray(
                    self.starttime,self.starttime+self.increment,self.increment))
        else: 
            self._products=collections.defaultdict(
                lambda: hwrf.numerics.TimeArray(
                    self.starttime,self.endtime,self.increment))
    def make_products(self): 
        """!This subroutine should be implemented in subclasses.  It
        should call self.make_product_structure(), and then add
        products by doing:
        @code
          self._products[domain.moad_ratio()][time]=product
        @endcode
        where the domain is a WRFDomainBase subclass, the time is
        anything accepted by to_datetime_rel's first argument, and the
        product is any subclass of Product."""
    def products(self,time=None,domain=None):
        """!Iterates over all products
        @param domain the hwrf.wrf.WRFDomain
        @param time the time
        @note One, both or neither argument can be specified.  All matching
        products are yielded."""
        if self._products is not None:
            if domain is not None and domain.moad_ratio() in self._products:
                tprod=self._products[domain.moad_ratio()]
                if time is None:
                    for (time,product) in tprod.itervalues(): 
                        localprod=product
                        assert(localprod is not None)
                        yield localprod
                elif time in tprod:
                    localprod=tprod[time]
                    assert(localprod is not None)
                    yield localprod
            else:
                if time is not None:
                    epsilon=hwrf.numerics.to_timedelta(10) # 10 seconds
                for tprod in self._products.itervalues():
                    for thetime,product in tprod.iteritems():
                        if time is not None:
                            if not hwrf.numerics.within_dt_epsilon(
                                thetime,time,epsilon):
                                continue # skip this product because
                                         # it is not at the right
                                         # time.
                        assert(product is not None)
                        yield product
                
    def make_namelist(self):
        """!Returns the namelist.wps contents as a string."""
        return self.nl.remove_traits().set_sorters(_wps_namelist_order,
                    _wps_nl_var_order).make_namelist()
    def undeliver_products(self,time=None,domain=None,fromloc=None):
        """!Deletes all delivered products and marks them as
        unavailable.
        @param time,domain passed to products() to get the list of products
        @param fromloc Ignored."""
        logger=self.log()
        time=self.starttime
        prodlist=[p for p in self.products(time=time,domain=domain)]
        for p in prodlist:
            p.undeliver(logger=logger)
    def deliver_products(self,time=None,domain=None,fromloc=None,
                         keep=True,relink=False):
        """!This is called from self.run to deliver all products to the
        intercom directory.  

        @param time,domain The optional time and domain arguments
        are sent to self.products.  
        @param fromloc By default, this routine assumes that the file
        to be delivered is in the current working directory with the
        same name as the destination file.  To change that, give a
        lambda function in "fromloc", which converts the destination
        filename (the only argument) to a local filename.  
        @param keep The "keep" argument has the same meaning as in
        deliver_file: if False, the file may be moved to the
        destination.  
        @param relink If True, and the file is moved, then a symlink
        will be made from the original file location to the
        destination."""
        link_files=self.confbool('link_wrf_fix',True)
        keep=keep and link_files
        logger=self.log()
        if time is None:
            time=self.starttime
        bad=False
        if fromloc is None: fromloc=os.path.basename
        prodlist=[p for p in self.products(time=time,domain=domain)]
        for p in prodlist:
            try:
                f=p.location
                if f is None or len(f)<1:
                    bad=True
                    msg='%s: product has no location; cannot deliver it'%(
                        p.did,)
                    logger.warning(msg)
                    raise WPSError(msg)
                fl=fromloc(f)
                assert(isinstance(fl,basestring)) # from file should
                                                  # be a string
                p.deliver(frominfo=fl,keep=keep,logger=logger)
                if not keep and not os.path.exists(fl):
                    linkfile=fl
                    destfile=p.location
                    logger.info('%s: file was moved to destination %s'
                                %(linkfile,destfile))
                    if relink:
                        logger.info('%s: relink from delivered file %s'
                                    %(linkfile,destfile))
                        make_symlink(destfile,linkfile,force=True,
                                     logger=logger)
                    else:
                        logger.info('%s: not relinking.  File now at %s'
                                    %(linkfile,destfile))
            except EnvironmentError as e:
                logger.warning('%s: cannot deliver file'%(f,),exc_info=True)
                bad=True
        if bad:
            logger.critical('some output files were empty or missing; '
                            'aborting')

    def set_namelist_times(self,start,end):
        """!Adjusts self.nl to set the start and end times of the wps namelist.

        @param start the start time, a datetime.datetime or a time
        relative to self.start
        @param end the end time, a datetime.datetime or a time relative to start
        @note This routine does not write out the namelist.  Make sure
        you call make_namelist() to get the namelist as a string and
        then use an open/write block to write it to namelist.wps.
        """
        start=to_datetime_rel(start,self.starttime)
        end=to_datetime_rel(start,end)

    def _wps_namelist(self):

        """!Fills the self.nl namelist with correct information.  This
        must be called before make_namelist, and hence it is called
        from the constructor."""
        s = self.nl.nl_set
        siu = self.nl.nl_set_if_unset
        t = self.nl.trait_get
        maxdom = self.maxdom
        start = self.starttime.strftime("%Y-%m-%d_%H:%M:%S")
        end = self.endtime

        s('share', 'max_dom', maxdom)
        s('share', 'start_date', start)
        s('share', 'end_date', end)
        s('share', 'interval_seconds', int(self._conf.get('wrf','bdystep')))
        io_form_geogrid=int(self.sim.io_form_for('auxinput2'))%100
        io_form_metgrid=int(self.sim.io_form_for('auxinput1'))%100
        if io_form_metgrid==11: io_form_metgrid=2
        if io_form_geogrid==11: io_form_geogrid=2
        s('share', 'io_form_geogrid', io_form_geogrid)
        s('metgrid', 'io_form_metgrid', io_form_metgrid)
        s('share','nocolons',self.nocolons)

        pid = []
        gid = []
        pgr = []
        nx  = []
        ny  = []
        dx  = []
        dy  = []
        istart = []
        jstart = []
        res = []
        
        resolution = '10m' if (maxdom<3) else '2m'

        for d in self.domains():
            if d.parent is None:
                pid.append(1)
                pgr.append(1)
                dx.append(d.nl.trait_get('dx'))
                dy.append(d.nl.trait_get('dy'))
                istart.append(1)
                jstart.append(1)
            else:
                pid.append(d.parent.get_grid_id())
                pgr.append(d.nl.trait_get('parent_grid_ratio'))
                start=str(d.nl.trait_get('start','auto')).lower()
                if start == 'fixed':
                    istart.append(int(d.nl.trait_get('istart')))
                    jstart.append(int(d.nl.trait_get('jstart')))
                else:
                    istart.append(10)
                    jstart.append(10)

            gid.append(d.get_grid_id())
            nx.append(d.nl.trait_get('nx'))
            ny.append(d.nl.trait_get('ny'))
            res.append(resolution)

        # Set (s()) anything in the namelist that must have specific
        # values.  For values the user can override, set only if it is
        # already unset (siu())
        s  ('geogrid', 'parent_id', pid)
        s  ('geogrid', 'parent_grid_ratio', pgr)
        s  ('geogrid', 'e_we', nx)
        s  ('geogrid', 'e_sn', ny)
        s  ('geogrid', 'dx', dx)
        s  ('geogrid', 'dy', dy)
        s  ('geogrid', 'i_parent_start', istart)
        siu('geogrid', 'geog_data_res', res)
        siu('geogrid', 'map_proj','rotated_ll')
        s  ('geogrid', 'geog_data_path', './geog-data/')
        s  ('geogrid', 'opt_geogrid_tbl_path','./')
        s  ('geogrid', 'j_parent_start', jstart)
        s  ('geogrid', 'ref_lat', self.conffloat('domlat',section='config'))
        s  ('geogrid', 'ref_lon', self.conffloat('domlon',section='config'))

        s  ('metgrid', 'opt_metgrid_tbl_path','./')
        s  ('metgrid', 'fg_name','FILE')

        s  ('ungrib',  'prefix','FILE')
        siu('ungrib',  'out_format','WPS')

        siu('share',   'wrf_core','NMM')

########################################################################
class Geogrid(WPSTask):
    """!This is a HWRF task that pre processes the geogrid to define the
    model domains and interpolates static geographical data to the
    grids."""

    def __init__(self,*args,**kwargs):
        """!Creates a new Geogrid.  
        @param args,kwargs All arguments are passed to the
        WPSTask.__init__() constructor."""
        super(Geogrid, self).__init__(*args,**kwargs)

    def make_products(self):
        """!Creates the FileProduct objects for this Geogrid.

        @note Implementation note: If you change the list of products,
        make sure all geogrid outputs needed as input to ungrib or
        metgrid have a prodname that is the same as the destination
        file."""
        self.make_product_structure('geogrid')
        suffix = self.io_suffix('geogrid')
        time=self.starttime
        for d in self.domains():
            id = d.get_grid_id()
            if d.parent is None:
                f = "geo_nmm.d%02d.%s" %(id, suffix)
            else:
                f = "geo_nmm_nest.l%02d.%s" %(id -1, suffix)
            dest=os.path.join(self.outdir,f)
            prod=FileProduct(self._dstore, f, self.taskname,
                 meta={"domain": d.name}, location=dest)
            prod.location=dest
            self.log().debug('geogrid made product %s with location %s'
                             %(repr(prod.did),repr(prod.location)))
            self._products[d.moad_ratio()][time]=prod
    def geodat(self,dom):
        """!Returns the FileProduct for the geogrid data for the
        specified nesting ratio.  The specified domain does not have
        to be one of the known domains.  It just has to have the same
        nest:parent ration (WRFDomain.moad_ratio) as one.
        @param dom The hwrf.wrf.WRFDomain of interest."""
        ratio=dom.moad_ratio()
        assert(ratio in self._products)
        return self._products[ratio][self.starttime]
    def run(self):
        """!Copies inputs, links fix files, runs geogrid and delivers
        results."""
        logger=self.log()
        try:
            produtil.fileop.makedirs(self.outdir)
            with NamedDir(self.location) as dir:
                logger.info('Geogrid running in directory: '+os.getcwd())
                assert(not re.match('\A/tmp',os.getcwd()))

                for f in glob.glob('geo*'):
                    try:
                        produtil.fileop.remove_file(f,logger=logger)
                    except(EnvironmentError) as e:
                        logger.warning('%s: did not remove file, but '
                                       'continuing anyway'%(f,))
                self.link_fix(geog_data=True,table='GEOGRID.TBL')
    
                with open('namelist.wps', 'w') as f:
                    f.write(self.make_namelist())
    
                prog = self.getexe('hwrf_geogrid')
                log = self._section + '.log'
                cmd = produtil.run.mpirun(produtil.run.mpi(prog),
                                          allranks=True)
                if self.redirect: cmd=cmd > log
                logger.info('%s command: %s'%(self.taskname, repr(cmd),))
                produtil.run.checkrun(cmd,logger=logger)
                findme="Successful completion"
                geogrid_log=None
                for glog in ( 'geogrid.log', 'geogrid.log.0000', 
                              'geogrid.log.00000' ):
                    if os.path.exists(glog):
                        geogrid_log=glog
                    else:
                        logger.info('%s: does not exist.'%(glog,))
                if geogrid_log is None:
                    msg='WPS Geogrid failed: could not find geogrid log file.'
                    logger.error(msg)
                    self.state=FAILED
                    raise GeogridNoLog(msg)
                logger.info('%s: will check for %s'%(geogrid_log,findme))
                if not check_last_lines(geogrid_log,findme):
                    raise WPSError('%s: did not find "%s"'
                                   %(geogrid_log,findme))
                self.deliver_products(keep=False,relink=True)

        except WPSError as we:
            self.state=FAILED
            raise
        except Exception as e:
            logger.critical('WPS Geogrid failed: '+str(e),exc_info=True)
            self.state=FAILED
            raise
        self.state=COMPLETED
        self.postmsg('WPS Geogrid completed.')

########################################################################
class Ungrib(WPSTask):
    """!This is a HWRF task that extracts the meteorological fields
    from GRIB formatted files and write the fields to intermediate
    files.  It is a wrapper around the WPS ungrib.exe program."""

    def __init__(self,*args,**kwargs):
        """!Creates a new Ungrib.  All arguments are passed to the
        WRFTask constructor.  The only new arguments are:

        * in_ftime - Optional: the parent model forecast hour to use
            for analysis time data in this Ungrib.

        * in_item, in_dataset - Optional: the item and dataset, in
            hwrf.input terminology, to use to request the GRIB1/2
            input files.

        * in_item2, in_dataset2 - Optional: same as in_item and
            in_dataset, but for a second GRIB file that is appended to
            the first.

        * inputs - Optional: an hwrf.input.DataCatalog to provide
            input file locations.  Default: construct one using the
            catalog name from the "catalog" option in this objects
            config section.

        @param args,kwargs passed to WPSTask.__init__        """
        super(Ungrib, self).__init__(*args,**kwargs)
        if 'inputs' in kwargs and kwargs['inputs']:
            self.inputs=kwargs['inputs']
        else:
            hd=self.confstr('catalog','hwrfdata')
            self.inputs=hwrf.input.DataCatalog(
                self.conf,hd,self.sim.simstart())
        self.__one_time=bool(kwargs.get('one_time',False))
        
        # Get input model's analysis time:
        in_atime=kwargs['in_atime'] if('in_atime' in kwargs) else \
            self.confstr('atime','')
        if in_atime is None or in_atime=='':
            self.in_atime=self.sim.simstart()
        else:
            self.in_atime=to_datetime_rel(in_atime,self.sim.simstart())

        # Get first forecast hour to process from input model:
        in_ftime=kwargs['in_ftime'] if('in_ftime' in kwargs) else None
        if in_ftime is None: in_ftime=self.confint('ftime',0)
        if isinstance(in_ftime,datetime.datetime):
            in_ftime=in_ftime-in_atime
        if not isinstance(in_ftime,int) and not isinstance(in_ftime,float) \
                and not isinstance(in_ftime,datetime.timedelta):
            raise TypeError('in_ftime must be an int, a float or a '
                            'timedelta, not a %s (in_ftime=%s)'%(
                            in_ftime.__class__.__name__,repr(in_ftime)))

        self.in_ftime = to_timedelta(in_ftime)
        self.in_dataset = str(kwargs.get(
                'in_dataset',self.confstr('dataset','hwrfdata')))
        self.in_dataset2 = str(kwargs.get(
                'in_dataset2',self.confstr('dataset2',self.in_dataset)))
        self.in_item = str(kwargs.get(
                'in_item',self.confstr('item','gfs_pgrb')))
        self.in_item2 = str(kwargs.get(
                'in_item2',self.confstr('item2','')))
        self.log().debug('self.in_item=%s dataset=%s section=%s'%(
                repr(self.in_item),repr(self.in_dataset), 
                repr(self.section)))
        self._n_gribfiles = 0

        # if self.gribs_per_time>1 and (
        #       self.confstr('subset','')!='' 
        #       or self.confstr('subset_grib1','')!=''
        #       or self.confstr('subset_grib2','')!=''):
        #     raise hwrf.exceptions.UngribCannotSubset(
        #         'hwrf.wps.Ungrib does not know how to merge two GRIB '
        #         'files and subset them.')

    ##@var inputs 
    # the hwrf.input.DataCatalog to use for obtaining input data

    ##@var in_atime
    # Parent model analysis time

    ##@var in_ftime
    # Parent model forecast hour that maps to the analysis time of this model

    ##@var in_dataset
    # Input dataset for the first GRIB source

    ##@var in_dataset2
    # Input dataset for the second GRIB source

    ##@var in_item
    # Input item for the first GRIB source

    ##@var in_item2
    # Input item for the second GRIB source

    @property
    def one_time(self):
        """!If True, we are pretending that hour 0 is valid for all
        times.  This is equivalent to constant boundary conditions.
        If in_ftime is non-zero, then that hour is used instead."""
        return self.__one_time
        
    @property
    def gribs_per_time(self):
        """!How many grib files are processed for each time?  This is 2
        if an item2 or in_item2 were given in the config or
        constructor, respectively.  Otherwise it is 1."""
        if self.in_item2 is not None and self.in_item2!='':
            return 2
        return 1

    def inputiter(self):
        """!Iterates over all input files needed.  This is meant to be
        plugged in to an hwrf.input.InputSource to obtain input data
        in the scripts.exhwrf_input job."""
        start=self.starttime
        for t in self.times():
            dt=t-start
            if self.one_time: dt=0
            ftime=to_datetime_rel(dt,to_datetime_rel(self.in_ftime,
                                                     self.in_atime))
            yield dict(self.taskvars,dataset=self.in_dataset,
                       item=self.in_item,ftime=ftime,atime=self.in_atime)
            if not self.in_item2 or not self.in_dataset2: continue
            yield dict(self.taskvars,dataset=self.in_dataset2,
                       item=self.in_item2,ftime=ftime,atime=self.in_atime,
                       optional=self.confbool('item2_optional',False))

    def input_at(self,dt,igrib=1,require=False):
        """!Finds input data for a specific time and GRIB file
        @param dt the forecast time as a datetime.datetime, relative 
          to the in_ftime
        @param igrib 1 or 2, which input file is used (in_item or in_item2
          This is needed for models like GFS and GEFS that split their
          GRIB files into two parts.
        @param require if True, then hwrf.exceptions.UngribNoInput is
          raised when inputs cannot be found."""
        if self.__one_time: dt=0
        logger=self.log()
        item=self.in_item
        dataset=self.in_dataset
        ftime=to_datetime_rel(dt,to_datetime_rel(self.in_ftime,
                                                 self.in_atime))
        stratime=self.in_atime.strftime("%Y%m%d%H")
        logger.info("Check for dataset=%s item=%s ftime=%s atime=%s in %s"%(
                dataset, item, ftime.strftime("%Y%m%d%H"), stratime, 
                repr(self.inputs) ))
        logger.debug('inputs: '+repr(self.inputs))
        if igrib==1:
            ret=self.inputs.locate(self.in_dataset,self.in_item,
                                   ftime=ftime,atime=self.in_atime,
                                   **self.taskvars)
        else:
            ret=self.inputs.locate(self.in_dataset2,self.in_item2,
                                   ftime=ftime,atime=self.in_atime,
                                   **self.taskvars)
        self.log().info("Got back: "+repr(ret))
        if require and (ret is None or ret==''):
            raise UngribNoInput(
                "Cannot find input for: dataset=%s item=%s ftime=%s igrib=%d"
                "atime=%s in %s"%(dataset, item, ftime.strftime("%Y%m%d%H"),
                                  stratime, repr(self.inputs),igrib ))
        return ret
    def get_grib(self,require=False,at=None):
        """!Link or copies all the input GRIB files to the current
        working directory.  Note that if two grib files are requested,
        then this is done by copying the data.
        @param require if True, then hwrf.exceptions.UngribNoInput is
         raised when inputs cannot be found.
        @param at if specified and not None, then only process this time,
         otherwise process all times"""
        logger=self.log()
        j = 0
        files = []
        start=self.starttime
        igribmax=self.gribs_per_time
        if at is not None:
            times=[ to_datetime_rel(at,self.starttime) ]
        else:
            times=[t for t in self.times() ]
        for t in times:
            dt=t-start # forecast time as a datetime.timedelta
            fhr=int(math.ceil(to_fraction(dt)/3600)) # forecast hour as int
            logger.info('Need to get input for t-start=%s-%s=%s=hour %s '
                    'igribmax=%s'%(
                    repr(t),repr(start),repr(dt),repr(fhr),repr(igribmax)))
            opt2=self.confbool('item2_optional',False)
            for igrib in xrange(igribmax):
            #f = "gfs.t%02dz.pgrb%sf%02d" %(start.hour, g2, fhr)
                f=self.input_at(dt,igrib=igrib+1)
                if igrib==1 and opt2:
                    continue
                if f is None or f=='':
                    raise UngribNoInput(
                        "Cannot find input for hour %d"%(fhr,))
                
                files.append(f)
                logger.info('Input for hour %s is %s'%(repr(fhr),repr(f)))

        if self.realtime:
            if not wait_for_files(
                files,logger,
                maxwait=self.confint('max_grib_wait',1800),
                sleeptime=self.confint('grib_sleep_time',20),
                min_size=self.confint('min_grib_size',1),
                min_mtime_age=self.confint('min_grib_age',30),
                min_atime_age=None,
                min_ctime_age=None,
                min_fraction=1.0):
                
                logger.error('Some input GRIB files do not exist.  Giving '
                             'up.')
                raise UngribNoInput(
                    'Some GRIB files are missing.  See the log for details.')

        names=dict()
        if self.gribs_per_time>1:
            logger.info('Merging GRIB files.')
            for t in times:
                dt=t-start # forecast time as a datetime.timedelta
                fhr=int(math.ceil(to_fraction(dt)/3600)) # forecast
                                                         # hour as int
                in1=self.input_at(dt,igrib=1)
                in2=self.input_at(dt,igrib=2)
                assert(in1!=in2)
                out=self._rename_grib()
                names[t]=out
                logger.info('%s: merge GRIBs for time %s here'
                            %(repr(out),repr(fhr)))
                with open(out,'wb') as outf:
                    logger.info('%s: copy from %s'%(repr(out),repr(in1)))
                    with open(in1,'rb') as inf1:
                        shutil.copyfileobj(inf1,outf)
                    try:
                        logger.info('%s: copy from %s'%(repr(out),repr(in2)))
                        with open(in2,'rb') as inf2: 
                            shutil.copyfileobj(inf2,outf)
                    except EnvironmentError as e:
                        opt=self.confbool('item2_optional',False)
                        if opt: 
                            logger.warning('%s: ignoring exception'%(
                                    str(in2),),exc_info=True)
                        else:
                            raise
                logger.info('%s: done'%(repr(out),))
        else:
            logger.info('Not subsetting or merging GRIB files')
            # Don't subset.  Just link:
            for t in times:
                dt=t-start # forecast time as a datetime.timedelta
                in1=self.input_at(dt,igrib=1)
                out=self._rename_grib()
                names[t]=out
                make_symlink(in1,out,force=True,logger=self.log())

        subset0=self.confstr('subset','')
        subset1file=self.confstr('subset_grib1',subset0)
        subset2file=self.confstr('subset_grib2',subset0)
        if subset1file or subset2file:
            logger.info('Subsetting GRIB files')
            cmd2=alias(bigexe(self.getexe('wgrib2','wgrib2')))
            cmd1=alias(bigexe(self.getexe('wgrib')))
            subset1=list()
            subset2=list()

            if subset1file:
                with open(subset1file,'rt') as f:
                    for line in f:
                        if line: subset1.append(line.rstrip())
                subset1_re=re.compile('|'.join(subset1))
            else:
                subset1_re=None

            if subset2file:
                with open(subset1file,'rt') as f:
                    for line in f:
                        if line: subset2.append(line.rstrip())
                subset2_re=re.compile('|'.join(subset2))
            else:
                subset2_re=None

            for t in times:
                srcfile=names[t]
                tgtfile=os.path.basename(srcfile)+".subsetted"
                cmd=None
                subset_re=None
                gribver=produtil.fileop.gribver(srcfile)
                if gribver==2:
                    if subset2file:
                        cmd=cmd2
                        subset_re=subset2_re
                elif gribver==1:
                    if subset1file:
                        cmd=cmd1
                        subset_re=subset1_re
                else:
                    raise UngribInputUnknown(
                        "%s: is neither GRIB1 nor GRIB2."%(srcfile,))

                if cmd is not None and subset_re is not None:
                    logger.info("%s: subsetting from %s"%(tgtfile,srcfile))
                    self._subset_grib(srcfile,tgtfile,cmd,subset_re)
                    logger.info('%s: delete and replace with subset %s'%(srcfile,tgtfile))
                    os.unlink(srcfile)
                    os.rename(tgtfile,srcfile)
                else:
                    logger.info("%s: not subsetting"%(srcfile))

    def _subset_grib(self, srcfile,tgtfile,cmd,matcher):
        """!Runs wgrib on a GRIB1 input file to get its content
        listing.  Then runs wgrib again to subset it.
        * srcfile - the input file, to scan and subset
        * tgtfile - the location for the new, subset file
        * cmd - a produtil.prog.ImmutableRunner for wgrib
        * matcher - a regular expression, or anything else that has
            a .search method.  Each call to search takes one line and
            returns True if it should be included in the subset, or
            False otherwise.        """
        subset=''
        (k,d)=(0,0) # keep, discard
        for line in runstr(cmd[srcfile],logger=self.log()).splitlines(True):
            if matcher.search(line):
                subset+=line
                k+=1
            else:
                d+=1
        if not k:
            raise UngribSubsetError('%s: no matching records in file'%(
                    tgtfile,))
        self.log().info('%s => %s: keeping %d records, discarding %d'%(
                srcfile,tgtfile,k,d))
        remove_file(tgtfile)
        runme=cmd['-i','-grib','-o',tgtfile,srcfile] << subset
        self.log().info('run: %s'%(repr(runme),))
        checkrun(runme,logger=self.log())
        if not produtil.fileop.isnonempty(srcfile):
            raise UngribSubsetError('%s: file is non-existent or empty'
                                    %(tgtfile,))

    def _rename_grib(self, filename=None):
        """!Generates a GRIB filename using the input name expected by
        WPS: GRIBFILE.AAA for the first, GRIBFILE.AAB for the second,
        and so on.  An internal counter self._n_gribfiles keeps track
        of the number of files requested so far.  The optional
        filename argument is ignored.  
        @param filename Ignored.
        @returns the new filename chosen."""
        sufs = [a+b+c for a,b,c in itertools.product(
                string.ascii_uppercase, repeat = 3)]
        new_filename = "GRIBFILE.%s" %(sufs[self._n_gribfiles])
        self._n_gribfiles += 1
        return new_filename

    def run(self):
        """!Links inputs and runs ungrib.  Ungrib has no deliverables:
        it only places files in the local directory for a later
        Metgrid.run() to pick up and use."""
        logger=self.log() # avoid several function calls
        try: 
            produtil.fileop.makedirs(self.outdir)
            with NamedDir(self.location) as dir:
                logger.info('Ungrib starting in %s'%(os.getcwd(),))
                
                assert(not re.match('\A/tmp',os.getcwd()))

                with open('namelist.wps', 'w') as f:
                    f.write(self.make_namelist())
                    
                self.link_fix(table='Vtable')
                self.get_grib()

                prog = self.getexe('hwrf_ungrib')
                log = self._section + '.log'
                cmd = produtil.run.exe(prog)
                if self.redirect: cmd = cmd > log
                logger.info('%s command: %s'%(self.taskname, repr(cmd),))
                produtil.run.checkrun(cmd,logger=logger)
                self.deliver_products()
                self.check_outfiles()
        except Exception as e:
            logger.critical('WPS Ungrib failed: '+str(e),exc_info=True)
            self.state=FAILED
            raise
        self.state=COMPLETED
        self.postmsg('WPS Ungrib completed')

    def check_outfiles(self):
        """!Checks the expected ungrib output files to make sure they
        all exist and are non-empty."""
        bad=False
        logger=self.log()
        for i in self.times():
            f = "%s/FILE:%s" %(self.location, i.strftime("%Y-%m-%d_%H"))
            if produtil.fileop.isnonempty(f):
                logger.info('%s: exists, is non-empty.'%(f,))
            else:
                logger.warning('%s: does not exist or is empty'%(f,))
                bad=True
        if bad:
            logger.error('WPS Ungrib failed: some output files did not '
                         'exist or were empty.  See stdout/stderr log for '
                         'details.')
            raise WPSError(
                "WPS Ungrib output file %s does not exist or is empty" %f)
    def deliver_products(self,*args,**kwargs): 
        """!Does nothing.  Ungrib has no products to deliver.
        @param args,kwargs Ignored."""
    def products(self,**kwargs):
        """!Ungrib delivers no products.  Everything is kept in the WPS
        temporary directory and reused by metgrid.  The Metgrid
        implementation assumes it is running in the same directory as
        Ungrib, so no call to products() is needed.
        @param kwargs Ignored."""
        if False: yield # necessary for syntactic reasons

########################################################################

class SteppingUngrib(Ungrib):
    """!This subclass of ungrib processes one input file at a time,
    running in parallel with its parent model."""
    def run(self):
        """!Links inputs and runs ungrib.  Ungrib has no deliverables:
        it only places files in the local directory for a later
        Metgrid.run() to pick up and use."""
        logger=self.log() # avoid several function calls
        try: 
            produtil.fileop.makedirs(self.outdir)
            with NamedDir(self.location) as dir:
                logger.info('Ungrib starting in %s'%(os.getcwd(),))
                for t in self.times():
                    dt=t-self.conf.cycle # forecast time as datetime.timedelta
                    fhr=int(math.ceil(to_fraction(dt)/3600)) # forecast hour as int
                    set_ecflow_meter('input',fhr)
                    self.set_namelist_times(t,t)
                    with open('namelist.wps', 'w') as f:
                        f.write(self.make_namelist())
                    self.get_grib(at=t)

                    prog = self.getexe('hwrf_ungrib')
                    log = self._section + '.log'
                    cmd = produtil.run.exe(prog)
                    if self.redirect: cmd = cmd > log
                    logger.info('%s command: %s'%(self.taskname, repr(cmd),))
                    produtil.run.checkrun(cmd,logger=logger)
                self.check_outfiles()
        except Exception as e:
            logger.critical('WPS Ungrib failed: '+str(e),exc_info=True)
            self.state=FAILED
            raise
        self.state=COMPLETED
        self.postmsg('WPS Ungrib completed')
    def _rename_grib(self, filename=None):
        """!Returns GRIBFILE.AAA
        @param filename Ignored
        @returns the string "GRIBFILE.AAA" """
        return "GRIBFILE.AAA"

########################################################################
class Metgrid(WPSTask):
    """!This is a HWRF task that horizontally interpolates the
    meteorological fields extracted by ungrib to the model grids
    defined by geogrid.  It is a wrapper around the WPS metgrid.exe
    Fortran program."""

    def __init__(self, *args,**kwargs):
        """!Creates a new Metgrid.  
        @param args,kwargs All arguments are passed to the
          WPSTask.__init__() constructor."""
        super(Metgrid, self).__init__(*args,**kwargs)
        self._n_gribfiles=0
    def run(self):
        """!Copies inputs, runs metgrid, delivers outputs.  

        @note Ungrib must have been run in the same directory
        beforehand.  The Geogrid must have been run in the same
        directory, unless it was passed as input via geogrid_from= in
        the constructor."""
        logger=self.log()
        try:
            produtil.fileop.makedirs(self.outdir)
            with NamedDir(self.location,keep=not self.scrub) as dir:
                logger.info('WPS Metgrid running in dir: '+os.getcwd())
                assert(not re.match('\A/tmp',os.getcwd()))

                with open('namelist.wps', 'w') as f:
                    f.write(self.make_namelist())

                self.link_geogrid()
                self.check_geogrid()

                self.link_fix(table='METGRID.TBL')
                prog = self.getexe('hwrf_metgrid')
                log = self._section + '.log'
                cmd = produtil.run.mpirun(produtil.run.mpi(prog),
                                          allranks=True)
                if self.redirect: cmd = cmd > log
                logger.info('%s command: %s'%(self.taskname, repr(cmd),))
                ok = produtil.run.checkrun(cmd,logger=logger)
                for time in self.times():
                    logger.info('%s: deliver products for this time.'
                                %(time.strftime('%Y-%m-%d %H:%M:%S')))
                    self.deliver_products(keep=False,relink=True,time=time)
                    if self.endtime == self.starttime:
                        p=self._products[1][time]
                        timeb=self.starttime+self.increment
                        pb=self._products[1][timeb]
                        make_symlink(p.location,pb.location,force=True,
                                     logger=logger)
                        pb.available=True

        except Exception as e:
            logger.critical('WPS Metgrid failed: %s'%(str(e),), 
                            exc_info=True)
            self.state=FAILED
            raise
        self.state=COMPLETED
        self.postmsg('WPS Metgrid completed')
    def met_at_time(self,when):
        """!Returns a FileProduct for the Metgrid file at the specified
        time or None if no such product is found
        @param when the time of interest"""
        if when in self._products[1]:
            return self._products[1][when]
        else:
            return None
    def make_products(self):
        """!Generates FileProduct objects for this Metgrid.  This is
        called automatically from the constructor."""
        self.make_product_structure('metgrid')
        MOAD=self.MOAD
        suffix=self.io_suffix('metgrid')
        format = ("%Y-%m-%d_%H_%M_%S" if self.nocolons else \
                      "%Y-%m-%d_%H:%M:%S")
        for time in self.times():
            f="met_nmm.d01.%s.%s"%(time.strftime(format),suffix)
            loc=os.path.join(self.outdir,f)
            prod=FileProduct(self.dstore, f, self.taskname, location=loc)
            prod.location=loc
            self._products[1][time] = prod
        
        if self.endtime == self.starttime:
            timeb=self.starttime+self.increment
            fb="met_nmm.d01.%s.%s"%(timeb.strftime(format),suffix)
            locb=os.path.join(self.outdir,fb)
            prodb=FileProduct(self.dstore, fb, self.taskname, location=locb)
            self._products[1][timeb] = prodb


