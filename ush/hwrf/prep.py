"""!PrepHybrid runs the prep_hybrid program to transform GFS spectral files to the HWRF grid."""

##@var __all__
# The list of symbols exported by "from hwrf.prep import *"
__all__=['PrepHybrid']

import os
import produtil.datastore, produtil.cd, produtil.fileop, produtil.prog
import hwrf.numerics, hwrf.hwrftask, hwrf.wrf, hwrf.namelist, hwrf.exceptions
import hwrf.input
import produtil.rusage

from produtil.rusage import setrlimit, rusage, getrlimit
from hwrf.exceptions import NoGeogData
from hwrf.numerics import to_datetime, to_datetime_rel, to_timedelta, \
    to_fraction, timedelta_epsilon, TimeMapping, partial_ordering, \
    to_datetime_rel
from produtil.fileop import deliver_file, make_symlink, fortlink, realcwd, \
    wait_for_files
from produtil.run import alias, bigexe, checkrun, openmp
from produtil.datastore import FileProduct

class PrepHybrid(hwrf.hwrftask.HWRFTask):
    """!Runs the prep_hybrid program on GFS spectral files.

    Tracks a sequence of GFS spectral files for a list of times.  Runs
    prep_hybrid on them, one by one.  This can be done by calling
    run(), which runs prep_hybrid for all of them before returning, or
    runpart() which runs only one before returning.  If self.realtime
    is True, then this class will wait for data to appear."""
    def __init__(self,dstore,conf,section,wrf,geogdata,atime=None,
                 ftime=None,taskname=None,times=None,inputs=None,
                 in_item=None,in_dataset=None,one_time=False,
                 in_anl_item=None,**kwargs):
        """!PrepHybrid constructor.
        @param dstore the produtil.datastore.Datastore database
        @param conf the hwrf.config.HWRFConfig for config info
        @param section the section to use in conf
        @param wrf the hwrf.wrf.WRFSimultion being run
        @param geogdata the geogrid data product
        @param atime,ftime analysis and forecast times
        @param taskname taskname in the dstore
        @param times prep_hybrid input/output times
        @param inputs  the hwrf.input.DataCatalog with the input data
        @param in_item,in_dataset Input item and dataset
        @param in_anl_item alternate input item for analysis time
        @param one_time If True, only one time is prepped but we lie
          and say should be used for all times.  This is used to trick
          the HWRF into running with the GDAS 9hr forecast even though
          a 12hr boundary condition would be needed
        @param kwargs More keyword arguments are passed to hwrf.hwrftask.HWRFTask.__init__"""
        if taskname is None: taskname=section

        super(PrepHybrid,self).__init__(dstore,conf,section,taskname,**kwargs)

        if times is None:
            self.times=[ x for x in wrf.bdytimes() ]
        else:
            self.times=sorted(times) 

        if inputs is None:
            hd=self.confstr('catalog','hwrfdata')
            inputs=hwrf.input.DataCatalog(self.conf,hd,wrf.simstart())
        self._epsilon=timedelta_epsilon(
            self.times,default=to_fraction(wrf.bdystep())/10)
        if in_dataset is None:     in_dataset=self.confstr('dataset')
        self.in_dataset=in_dataset
        if in_item is None:        in_item=self.confstr('item')
        if in_anl_item is None:
            in_anl_item=self.confstr('anl_item','')
        if in_anl_item=='':        
            in_anl_item=in_item
        self.in_item=in_item
        self.in_anl_item=in_anl_item
        if atime is None: atime=wrf.simstart()
        if ftime is None: ftime=atime
        self.one_time=one_time
        self.in_atime=to_datetime(atime)
        self.in_ftime=to_datetime_rel(ftime,atime)
        self.geogdata=geogdata
        self.inputs=inputs
        self.bdyprod=TimeMapping(self.times)
        self.logprod=TimeMapping(self.times)
        self.initprod=None
        self.wrf=wrf

        self.make_products()
        self.nl=hwrf.namelist.Conf2Namelist(conf, self.confstr('namelist'))
        self.init_namelist()

        self._prep=None

    ##@var bdyprod
    # Mapping from time to boundary output product

    ##@var initprod
    # Prep initial state product.

    ##@var nl
    # hwrf.namelist.Conf2Namelist with the prep_hybrid namelist

    ##@var logprod
    # Mapping from time to prep.log log file product

    ##@var wrf
    #the hwrf.wrf.WRFSimultion being run

    ##@var geogdata
    #the geogrid data product

    ##@var in_atime
    # Analysis time.

    ##@var in_ftime
    #Forecast time.

    ##@var times
    #prep_hybrid input/output times

    ##@var inputs
    # the hwrf.input.DataCatalog with the input data

    ##@var in_item
    #hwrf.input.DataCatalog input item

    ##@var in_dataset
    #hwrf.input.DataCatalog Input dataset

    ##@var in_anl_item
    #alternate input item for analysis time

    ##@var one_time
    #If True, only one time is prepped but we lie
    #     and say should be used for all times.  This is used to trick
    #     the HWRF into running with the GDAS 9hr forecast even though
    #     a 12hr boundary condition would be needed

    def inputiter(self):
        """!Iterates over the list of data needed to run this class."""
        atime=self.in_atime
        for t in self.times:
            if hwrf.numerics.within_dt_epsilon(t,atime,5):
                in_item=self.in_anl_item
            else:
                in_item=self.in_item
            yield dict(self.taskvars,dataset=self.in_dataset,
                       item=in_item,ftime=t,atime=atime)
            if self.one_time: break
    def input_at(self,when):
        """!Finds the input needed for the specified time.
        
        @param when the time of interest
        @returns the spectral file at that time."""
        logger=self.log()
        ftime=to_datetime_rel(self.in_ftime,self.in_atime)
        if self.one_time:
            when=ftime
        else:
            when=to_datetime_rel(when,ftime)
        atime=self.in_atime
        if hwrf.numerics.within_dt_epsilon(when,atime,5):
            logger.info('Within dt epsilon: %s %s'%(when,atime))
            in_item=self.in_anl_item
        else:
            logger.info('NOT within dt epsilon: %s %s'%(when,atime))
            in_item=self.in_item

        self.log().info(
            "Check for dataset=%s item=%s ftime=%s atime=%s in %s"
            %(str(self.in_dataset), str(in_item), 
              when.strftime("%Y%m%d%H"),
              atime.strftime("%Y%m%d%H"), repr(self.inputs) ))
        return self.inputs.locate(self.in_dataset,in_item,ftime=when,
                                  atime=self.in_atime,**self.taskvars)
    def io_form_geogrid(self):
        """!Guesses the WRF I/O form that was used to run geogrid
        @returns the io_form for auxinput2"""
        return self.wrf.io_form_for('auxinput2')
    def init_namelist(self):
        """!Constructs the prep_hybrid namelist in the Conf2Namelist
        object in self.nl"""
        # Alias some functions for convenience:
        siu=self.nl.nl_set_if_unset  # set my namelist var if not yet set
        wg=self.wrf.nl.nl_get        # get WRF namelist variable
        wtg=self.wrf.nl.trait_get    # get WRF trait
        # Set or override several parameters:
        siu('prmfld','ntimes',1)
        siu('domain','levels',wg('domains','eta_levels'))
        siu('domain','ptsgm',float(wg('domains','ptsgm')))
        siu('domain','p_top_requested',float(wtg('ptop'))/100.0) # Pa->hPa
    def make_products(self):
        """!Creates products objects for later file delivery.

        Creates the produtil.datastore.FileProduct objects for
        delivery of prep_hybrid output."""
        first=True
        outdir=self.outdir
        ds=self.dstore
        cat=self.outdir
        self.initprod=FileProduct(ds,'hwrfinit_0',cat,
                                  location=os.path.join(outdir,'hwrfinit_0'))
        for itime in xrange(len(self.times)):
            time=self.times[itime]
            assert(time is not None)
            self.bdyprod[time]=FileProduct(ds,'hwrfbcs_%d'%(itime,),cat,
                    location=os.path.join(outdir,'hwrfbcs_%d'%(itime,)))
            self.logprod[time]=os.path.join(outdir,'prep_%d.log'%(itime,))
    def prep_init_file(self):
        """!Return the FileProduct for the prep_hybrid initial time output file."""
        return self.initprod
    def prep_bdy_files(self):
        """!Iterates over the FileProduct for the prep_hybrid boundary output files."""
        for prod in self.bdyprod.itervalues():
            yield prod
    def products(self,time=None,name=None):
        """!Iterates over all output products.

        Iterates over all products meeting the requirements.
        @param time the time of interest, or None for all times
        @param name "init" for initial state, "bdy" for boundary
          information, or None for all."""
        if name is None or name=='init':
            dt=to_timedelta(abs(to_fraction(time-self.wrf.simstart())))
            if time is None or dt<self._epsilon:
                yield self.initprod
        if name is None or name=='bdy':
            bdys=self.bdyprod
            if time is None:
                for (time,prod) in bdys.iteritems(): yield prod
            else:
                when=bdys.neartime(time,self._epsilon)
                if when in bdys: yield bdys[when]
    def unrun(self):
        """!Marks all products as unavailable, but does not delete
        delivered files."""
        for p in self.products():
            p.available=0
    def clean(self):
        """!Deletes all temporary files (the self.workdir)."""
        self.rmtree(os.path.join(self.workdir))
    def run(self,keep=True):
        """!Preps all times, even if they have already been prepped.
        @param keep if True, copy output files instead of moving them
          to the destination."""
        logger=self.log()
        self.postmsg('%s: prep is starting'%(self.taskname,))
        for now in self.times:
            logger.info('time %s: prep %s'%(now.strftime('%Y%m%d-%H%M%S'),
                                            self.input_at(now)))
        for ipiece in xrange(len(self.times)):
            self.run_ipiece(ipiece,keep=keep)
        self.postmsg('%s: prep is completed'%(self.taskname,))
    def runpart(self,keep=True):
        """!Preps one time that has not yet been prepped, and returns.
        @param keep if True, copy output files instead of moving them
          to the destination."""
        logger=self.log()
        for ipiece in xrange(len(self.times)):
            now=self.times[ipiece]
            if not self.bdyprod[now].available or ipiece==0 and \
                    not self.initprod.available:
                logger.info(
                    'time %s (piece %d): not done; process this one'
                    %(now.strftime('%Y%m%d-%H%M%S'),ipiece))
                try:
                    self.run_ipiece(ipiece,keep=keep)
                    return
                except Exception as e:
                    logger.warning('time %s (piece %d): %s'%(
                            now.strftime('%Y%m%d-%H%M%S'),ipiece,str(e)),
                                   exc_info=True)
                    raise
            else:
                self.log().info('time %s (piece %d): already complete'
                                %(now.strftime('%Y%m%d-%H%M%S'),ipiece))
    def run_ipiece(self,ipiece,keep=True):
        """!This is the internal helper function that runs the
        prep_hybrid for self.run and self.runpart.
        @param ipiece the index of the output time to run
        @param keep if True, copy output files instead of moving them
          to the destination."""
        logger=self.log() # avoid many function calls
        cenla=self.conf.getfloat('config','domlat')
        cenlo=self.conf.getfloat('config','domlon')
        now=self.times[ipiece]
        prepme=self.input_at(now)
        assert(isinstance(prepme,basestring))
        if self.realtime and not wait_for_files(
            prepme,logger,
            maxwait=self.confint('max_spectral_wait',1800),
            sleeptime=self.confint('spectral_sleep_time',20),
            min_size=self.confint('min_spectral_size',int(4e7)),
            min_mtime_age=self.confint('min_spectral_age',30),
            min_atime_age=None,
            min_ctime_age=None,
            min_fraction=1.0):
            msg='Some input spectral files do not exist.  Giving up.'
            logger.error(msg)
            raise hwrf.exceptions.NoSpectralData(msg)

        stime=now.strftime('%Y%m%d-%H%M%S')
        geoloc=self.geogdata.location
        if not self.geogdata.available:
            raise NoGeogData('%s product: WPS geogrid data is not yet '
                             'available. (loc=%s)'%(
                    str(self.geogdata.did), repr(self.geogdata.location)))
        moad=self.wrf.get_moad()
        # Decide where to run:
        there=self.conftimestrinterp('{workdir}/{fYMDH}',
                                     ftime=now,workdir=self.workdir)
        with produtil.cd.NamedDir(there,keep=keep,logger=logger):
            logger.info('%s: prep in directory %s',stime,realcwd())
            make_symlink(self.geogdata.location,'geogrid.out',
                         logger=logger,force=True)
            with open('dloc','wt')  as f:  f.write("%f\n%f\n"%(cenla,cenlo))
            with open('itime','wt') as f:  f.write("%d\n"%(ipiece,))
            bdyfile='../hwrfbcs00_%d'%(ipiece,)
            fortlink({52:bdyfile,
                      11:prepme,
                      44:'itime',
                      45:'dloc',           },logger=logger,force=True)
            if ipiece==0:
                initfile='../hwrfinit_%d'%(ipiece,)
                fortlink({53:initfile},logger=logger,force=True)
            with open('hwrf_na12_mkbnd.parm','wt') as f: 
                f.write(self.nl.make_namelist(section_sorter=partial_ordering(
                    ['rgrid','prmfld','domain'])))
            imax=self.confint('imax',1440)
            jmax=self.confint('jmax',721)
            iof=self.io_form_geogrid()%100
            if iof==11: iof=2
            ex=( bigexe(self.getexe('hwrf_prep')).env(
                    IO_FORM=iof,
                    OMP_STACKSIZE="128M",
                    MKL_NUM_THREADS='1'   )[
                    moad.nl.nl_get('domains','e_we'), 
                    moad.nl.nl_get('domains','e_sn'),
                    moad.nl.nl_get('domains','e_vert'),
                    moad.nl.nl_get('domains','dx'),
                    moad.nl.nl_get('domains','dy'),
                    imax, jmax]
                 < 'hwrf_na12_mkbnd.parm' ) >= 'prep.log'
            threads=self.confint('threads',0)
            # Try to change stack limit to 6GB:
            setrlimit(logger=logger,stack=6e9,ignore=True)
            # Print all resource limits:
            getrlimit(logger=logger)
            # AND collect resource usage during prep:
            with rusage(logger=logger):
                if threads<1:
                    logger.info('Use automatic thread count.')
                    ex2=openmp(ex) # use default threads
                else:
                    # Use specified threads
                    logger.info('Use %d threads'%(threads,))
                    ex2=openmp(ex,threads=threads)
                MALLOC_CHECK_=self.confint('MALLOC_CHECK_',-999)
                if MALLOC_CHECK_ != -999:
                    ex2.env(MALLOC_CHECK_=str(MALLOC_CHECK_))
                checkrun(ex2,logger=logger)
            if ipiece==0:
                produtil.fileop.makedirs(os.path.dirname(
                        self.initprod.location))
                self.initprod.deliver(
                    frominfo=initfile,logger=logger,keep=False)
            self.bdyprod[now].deliver(
                frominfo=bdyfile,logger=logger,keep=False)
            deliver_file('prep.log',self.logprod[now],logger=logger,
                         keep=False)
