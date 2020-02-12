"""!Contains Bufrprep, which converts data tanks to bufr files, and
otherwise preprocesses the prepbufr and bufr files."""

##@var __all__
# List of symbols to export by "from hwrf.bufrprep import *"
__all__ = ['Bufrprep']

import os, shutil
import hwrf.hwrftask, hwrf.exceptions
import datetime
import produtil.fileop, produtil.cd, produtil.cluster, produtil.log

from produtil.ecflow import set_ecflow_event
from produtil.cd import NamedDir
from produtil.fileop import make_symlink, deliver_file, isnonempty, \
              fortlink
from produtil.run import run, exe
from hwrf.hwrftask import HWRFTask
from hwrf.numerics import to_datetime, to_datetime_rel
from hwrf.exceptions import GSIInputError

class Bufrprep(HWRFTask):
    """!This is a HWRF task that preprocesses observations in data
    tanks to create bufr files suitable for input to hwrf.gsi classes.
    It also does other preprocessing of bufr and prepbufr files."""

    def __init__(self,dstore,conf,section,taskname=None,atime=None,
                 **kwargs):
        """!Bufrprep constructor
        @param dstore passed to Datum: the Datastore object for this Task
        @param conf the conf object for this task (passed to HWRFTask)
        @param section the conf section for this task (passed to HWRFTask)
        @param taskname Optional: the taskname for this product in the datastore
        @param atime the analsysis time
        @param kwargs Additional keyword arguments are passed to the
          hwrf.hwrftask.HWRFTask.__init__ constructor"""
        super(Bufrprep,self).__init__(dstore,conf,section,taskname=taskname,
                                     **kwargs)

        taskname=self.taskname
        if atime is None: atime=conf.cycle
        self._atime=to_datetime(atime)
        self._dirname=self.workdir
        self._stormid='999'
        # Get the DataCatalog for our data grabbers:
        self._in_catalog=None
        incat_name=None
        if 'in_catalog' in kwargs:
            ink=kwargs['in_catalog']
            if isinstance(ink,hwrf.input.DataCatalog):
                self._in_catalog=ink
            elif isinstance(ink,basestring):
                incat_name=ink
            elif ink is None: pass
            else:
                raise TypeError(
                    'In hwrf.bufrprep.Bufrprep.__init__, in_catalog must be None, '
                    'a basestring or a DataCatalog.  You provided an object '
                    'of type %s, with value %s.'
                    %(type(ink).__name__,repr(ink)))
        if self._in_catalog is None:
            if incat_name is None:
                incat_name=self.confstr('catalog')
            self._in_catalog=hwrf.input.DataCatalog(
                self.conf,incat_name,self._atime)


    def getstormid(self):
        """!The storm ID."""
        logger=self.log()
        atime=self._atime.strftime('%Y%m%d%H')
        year=int(atime[0:4])
        basin=self.storminfo.pubbasin2
        if year <= 2013:
            self._stormid=self.storminfo.stnum
        elif basin.upper()=='AL':
            self._stormid='%s%02d' % ('1',self.storminfo.stnum)
        elif basin.upper()=='EP':
            self._stormid='%s%02d' % ('2',self.storminfo.stnum)
        elif basin.upper()=='CP':
            self._stormid='%s%02d' % ('3',self.storminfo.stnum)
        else:
            self._stormid='999'

        logger.info('get input storm id %s'%(self._stormid))
        return self._stormid

    def grab_bufr(self,atime=None,morevars=None):
        """!Links or copies all needed bufr files to the local
        directory.
        @param atime the analysis time
        @param morevars passed to hwrf.config.HWRFConfig.get()
           and similar routines to define string replacements"""
        olist=self.confstr('obstypes')
        touched=set()
        for osection in olist.split(','):
            trim=osection.strip()
            if len(trim)>0 and not trim in touched:
                self.grab_obstype_section(trim,atime=atime,morevars=morevars)

    def grab_obstype_section(self,section,atime=None,morevars=None):
        """!Copies or links observations specified in the obstype
        sections to the current working directory.

        The section listed in self.section should contain an obstype
        option, whose value is a comma separated list of section
        names.  This method reads every section in that list.  For
        each section, the option keys are the local directory
        filenames expected by GSI, while the values are the data type
        part of the operational filename (ie.: the satwind in
        gfs.t12z.tm00.satwind.bufr_d).  There are a few special keys:

        * dataset - the name of the dataset for hwrf.input purposes
        * item - the name of the item for hwrf.input purposes
        * type - the type of observation: satellite, or anything else.
            At present, only "satellite" has any special meaning.

        If the type is "satellite" then the entire section will be
        skipped if sat_da=False in this task's config section.

        Once the section is parsed, the files are all linked to this
        directory.

        @param section Name of the section to read.
        @param atime Analysis time.
        @param morevars A dict for additional string replacements
          in the hwrf.config.HWRFConfig.get() family of functions."""
        logger=self.log()
        if not isinstance(section,basestring): section=str(section)

        if atime is None:
            atime=self._atime
        else:
            atime=to_datetime_rel(atime,self._atime)

        dataset=self.conf.get(section,'dataset')
        item=self.conf.get(section,'item')
        otype=self.conf.get(section,'type').lower()

        logger.warning('process obs section %s with dataset=%s item=%s '
                       'type=%s'%(section,dataset,item,otype))

        obstypes=list()
        items=self.conf.items(section)
        otdict=dict( [ (v,k) for k,v in items ] )
        namer=lambda f,t: otdict[t]

        for localname,obstype in items:
            if localname in ['dataset','item','type']: continue
            obstypes.append(obstype)

        for obstype in obstypes:
            logger.warning('Find obstype=%s in dataset=%s item=%s'
                           %(obstype,dataset,item))
            if not isinstance(obstype,basestring):
                raise TypeError(
                    'In bufrprep.Bufrprep.link_bufr, the obstypes parameter must '
                    'be an iterable container of basestrings.  One of the '
                    'elements was a %s (value %s) instead.'
                    %(type(obstype).__name__,repr(obstype)))
            there=self._in_catalog.locate(dataset,item,atime=atime,
                                          logger=logger,obstype=obstype)
            if there is None or there=='':
                msg='%s: Could not find a location for this obstype.'\
                    %(obstype,)
                logger.warning(msg)
            elif produtil.fileop.isnonempty(there):
                bn=os.path.basename(there)
                on=namer(bn,obstype)
                make_symlink(there,on,logger=logger,force=True)
            else:
                msg='%s: Observation file is empty or non-existant: %s'\
                    %(obstype,there)
                logger.warning(msg)

    def grab_prepbufr(self,atime=None,**kwargs):
        """!Links or copies the prepbufr file to the local directory.
        @param atime The analysis time.
        @param kwargs More string substitution variables for the
          hwrf.config.HWRFConfig family of functions."""
        if atime is None:
            atime=self._atime
        else:
            atime=to_datetime_rel(atime,self._atime)
        logger=self.log()
        bufr_dataset=self.confstr('bufr_dataset')
        prepbufr_item=self.confstr('prepbufr_item')
        there=self._in_catalog.locate(bufr_dataset,prepbufr_item,
            atime=atime,logger=logger,**kwargs)
        if there is None or there=='':
            msg='Could not find the prepbufr file (item=%s dataset=%s)' \
                %(repr(prepbufr_item),repr(bufr_dataset))
            logger.warning(msg)
            raise GSIInputError(msg)
        elif not produtil.fileop.isnonempty(there):
            msg=there+': is non-existant or empty'
            logger.error(msg)
            raise GSIInputError(msg)
        deliver_file(there,'prepbufr.ALL',keep=True,logger=logger)

    def tdrdump(self,atime=None,morevars=None):
        """!Gets the dumped TDR data from the pure shell jhwrf_bufrdump job.
        @param atime The analysis time.
        @param morevars More string substitution variables for the
          hwrf.config.HWRFConfig.get() family of functions."""
        if atime is None:
            atime=self._atime
        else:
            atime=to_datetime_rel(atime,self._atime)
        logger=self.log()

        input_catalog=self.conf.get('config','fcst_catalog')
        dcomenv=os.path.join(os.environ.get('DCOMROOT'),'us007003')
        dcom=self.conf.get(input_catalog,'dcom',dcomenv)

        tdrbufr=os.path.join(self.getdir('WORKhwrf'),'tldplr.ibm')
        if isnonempty(tdrbufr):
            produtil.log.postmsg('TDR dump found: '+tdrbufr)
            produtil.fileop.deliver_file(
                tdrbufr,'tldplrbufr',keep=True,logger=logger)
        elif self.conf.getbool('config','expect_tdr',False):
            msg='TDR data is missing.  This HWRF IT test requires that TDR data be present.'
            produtil.log.jlogger.critical(msg)
            raise hwrf.exceptions.ExpectedTDR(msg)
        else:
            produtil.log.postmsg('TDR dump missing: '+tdrbufr)



    def prep_prepbufr(self,option):
        """!pre-process prepbufr data

        Options:
        * option 0: make no change
        * option 1: remove some inner-core data
        * option 2: flag/unflag mass and dropsonde u, v data
        * option 3: unflag HS3 dropsonde data"""
        logger=self.log()
        fortlink({ 21:"./prepbufr.ALL",
                   51:"./prepbufr"})
        if option == 1:
            fprog = 'hwrf_rem_prepbufr_typ_in_circle'
            prog   = self.getexe(fprog)
            RRADC=self.conffloat('RRADC',0.)
            cmd = produtil.run.exe(prog).env(RLATC=self.storminfo.lat, \
                                   RLONC=self.storminfo.lon, \
                                   RRADC=RRADC)
        elif option == 2:
            self.write_vitals()
            fprog = 'hwrf_change_prepbufr_qm_in_circle'
            prog   = self.getexe(fprog)
            RRADC=self.conffloat('RRADC',0.)
            RBLDC=self.conffloat('RBLDC',0.)
            cmd = produtil.run.exe(prog).env(RRADC=RRADC, \
                                             RBLDC=RBLDC)
        elif option == 3:
            fprog = 'hwrf_change_prepbufr_qm_typ'
            prog   = self.getexe(fprog)
            cmd = produtil.run.exe(prog)

        if option > 0 and option <= 3:
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=logger)
        else:
            logger.info('no greater than 3 option, skip prep_prepbufr')

    def write_vitals(self,filename='tcvitals',logger=None):
        """!Writes the tcvitals (from self.storminfo) to the specified
        file.
        @param filename Name of the file to write
        @param logger  A logging.Logger for log messages"""
        if logger is None: logger=self.log()
        logger.info('Writing tcvitals to %s'%(repr(filename),))
        with open(filename,'wt') as f:
            f.write(self.storminfo.as_tcvitals()+"\n")
        assert(os.path.exists(filename))

    def run(self):
        """!Runs and delivers the results."""
        logger=self.log()
        try:
            logger.info('Run bufrprep in directory %s'%(self._dirname,))
            if os.path.exists(self._dirname):
                logger.info('Delete old data in %s'%(self._dirname,))
                shutil.rmtree(self._dirname)
            with NamedDir(self._dirname,keep=not self.scrub):
                """dump and precess TDR data"""
                realtime=bool(self.realtime)
                if realtime:
                    self.tdrdump()
                else:
                    self.grab_bufr()
                if os.path.isfile('tldplrbufr'):
                    self.getstormid()
                    logger.info('storm id %s'%(self._stormid))
                    self.readtdrstmid()
                    self.readtdrtime()
                    self.set_tdrstatus()
                else:
                    logger.info('Skip read tdr bufr.')
                """precess prepbufr data"""
                prepbufrprep=self.confint('prepbufrprep',0)
                if prepbufrprep > 0:
                    self.grab_prepbufr()
                    self.prep_prepbufr(prepbufrprep)
                self.deliver_products()

        except Exception as e:
            logger.critical('bufrprep failed: '+str(e),exc_info=True)
            raise

    def readtdrstmid(self):
        """!Runs the hwrf_readtdrstmid program."""
        self.log().info('readtdrstmid')
        logger=self.log()
        fprog = 'hwrf_readtdrstmid'
        prog  = self.getexe(fprog)

        log = '%s/logs/%s_%s.log' %(
            self._dirname, self.__class__.__name__, fprog)
        cmd = produtil.run.exe(prog) << self._stormid
        if self.redirect: cmd = cmd >= log
        produtil.run.checkrun(cmd,logger=logger)

    def readtdrtime(self):
        """!Runs the hwrf_readtdrtime program."""
        self.log().info('readtdrtime')
        logger=self.log()
        fprog = 'hwrf_readtdrtime'
        prog   = self.getexe(fprog)

        log = '%s/logs/%s_%s.log' %(
            self._dirname, self.__class__.__name__, fprog)
        cmd = produtil.run.exe(prog)
        if self.redirect: cmd = cmd >= log
        produtil.run.checkrun(cmd,logger=logger)

    def set_tdrstatus(self):
        """!Create a TDR status file in com directory"""
        self.log().info('set_tdrstatus')
        logger=self.log()
        stmidout=os.path.join(self._dirname,'stmid.dat')
        timeout=os.path.join(self._dirname,'tdrflag')
        rightstorm=isnonempty(stmidout)
        smalledgedump=isnonempty(timeout)
        tdrflagfile=os.path.join(self.conf.getdir('com'),self.icstr('{stormlabel}.tdr'))
        if rightstorm and not smalledgedump:
            with open(tdrflagfile,'wt') as f:
                f.write('ASSIMILATE_TDR=YES'+"\n")
            logger.info('tdr bufr is available for this storm, ASSIMILATE_TDR=YES')
        elif not rightstorm:
            logger.info('tdr bufr is not for this storm, ASSIMILATE_TDR=NO')
        else:
            logger.info('this tdr bufr file is a small edge dump, ASSIMILATE_TDR=NO')

    def deliver_products(self,atime=None,**kwargs):
        """!Delivers output products to the intercom directory.
        @param atime the analysis time
        @param kwargs Sent to hwrf.input.DataCatalog.parse()"""
        if atime is None:
            atime=self._atime
        else:
            atime=to_datetime_rel(atime,self._atime)
        logger=self.log()
        produtil.fileop.makedirs(self.outdir)
        if self.confint('prepbufrprep',0) > 0:
            prepbufr_item=self.confstr('prepbufr_item')
            there=self._in_catalog.parse(prepbufr_item,
                                   atime=atime,logger=logger,**kwargs)
            it=os.path.join(self.outdir,there)
            produtil.fileop.deliver_file(
                'prepbufr',it,keep=False,logger=logger)
        if bool(self.realtime) and isnonempty('tldplrbufr'):
            item=self.conf.get('tdr_new_obstype','item')
            there=self._in_catalog.parse(item,atime=atime,
                                   logger=logger,obstype='tldplr')
            it=os.path.join(self.outdir,there)
            produtil.fileop.deliver_file(
                'tldplrbufr',it,keep=True,logger=logger)
            tdrprod=self.icstr('{com}/{out_prefix}.tldplr.tm00.bufr_d')
            produtil.fileop.deliver_file(
                'tldplrbufr',tdrprod,keep=False,logger=logger)
            set_ecflow_event('FoundTDR')

