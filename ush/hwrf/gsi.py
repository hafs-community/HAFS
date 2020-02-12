"""!Runs the GSI data assimilation on the HWRF system."""

##@var __all__
# The list of symbols exported by "from hwrf.gsi import *"
__all__ = ['GSI_DATA_TYPES', 'GSIBase', 'FGATGSI' ]

import os, shutil, glob, gzip
import produtil.fileop, produtil.datastore, produtil.cd, produtil.run
import produtil.workpool
import hwrf.hwrftask, hwrf.wrf, hwrf.numerics, hwrf.namelist
import datetime
import produtil.rusage

from produtil.rusage import setrlimit, rusage, getrlimit
from produtil.datastore import COMPLETED
from produtil.cd import NamedDir
from produtil.fileop import make_symlink, deliver_file, isnonempty, \
    make_symlinks_in
from produtil.run import mpirun, mpi, openmp, checkrun, bigexe
from hwrf.numerics import to_datetime,to_datetime_rel, to_fraction, \
    to_timedelta
from hwrf.exceptions import GSIInputError

##@var GSI_DATA_TYPES
# List of all known GSI datastypes
GSI_DATA_TYPES = [
    "hirs2_n14", "msu_n14", "sndr_g08", "sndr_g11", "sndr_g12", "sndr_g13",
    "sndr_g08_prep", "sndr_g11_prep", "sndr_g12_prep", "sndr_g13_prep",
    "sndrd1_g11", "sndrd2_g11", "sndrd3_g11", "sndrd4_g11", "sndrd1_g12", 
    "sndrd2_g12", "sndrd3_g12", "sndrd4_g12", "sndrd1_g13", "sndrd2_g13",
    "sndrd3_g13", "sndrd4_g13", "sndrd1_g14", "sndrd2_g14", "sndrd3_g14",
    "sndrd4_g14", "sndrd1_g15", "sndrd2_g15", "sndrd3_g15", "sndrd4_g15",
    "hirs3_n15", "hirs3_n16", "hirs3_n17", "amsua_n15", "amsua_n16",
    "amsua_n17", "amsub_n15", "amsub_n16", "amsub_n17", "hsb_aqua",
    "airs_aqua", "amsua_aqua", "imgr_g08", "imgr_g11", "imgr_g12",
    "imgr_g14", "imgr_g15", "pcp_ssmi_dmsp", "pcp_tmi_trmm", "conv",
    "sbuv2_n16", "sbuv2_n17", "sbuv2_n18", "sbuv2_n19", "gome_metop-a",
    "omi_aura", "mls_aura", "ssmi_f13", "ssmi_f14", "ssmi_f15",
    "hirs4_n18", "hirs4_metop-a", "amsua_n18", "amsua_metop-a", "mhs_n18",
    "mhs_metop-a", "amsre_low_aqua", "amsre_mid_aqua", "amsre_hig_aqua",
    "ssmis_f16", "ssmis_f17", "ssmis_f18", "ssmis_f19", "ssmis_f20",
    "iasi_metop-a", "hirs4_n19", "amsua_n19", "mhs_n19", "seviri_m08",
    "seviri_m09", "seviri_m10", "cris_npp", "atms_npp", "hirs4_metop-b",
    "amsua_metop-b", "mhs_metop-b", "iasi_metop-b", "gome_metop-b" ]

class GSIBase(hwrf.hwrftask.HWRFTask):
    """!Base class of anything that runs the GSI.  Do not use directly."""

    ##@var used_regional_ensemble
    # Was the regional ensemble used for forecast error covariances?

    ##@var global_ensemble_size
    # The number of members in the global (parent model) ensemble

    ##@var hybrid_da
    # Was hybrid 3DVAR-ENKF data assimilation used?

    def __init__(self,dstore,conf,section,domain,wrf_in_prod,sim,
                 taskname=None,atime=None,parent_atime=None,
                 enkf_domains=None,ensda=None,**kwargs):
        """!The GSIBase constructor:
        @param dstore passed to Datum: the Datastore object for this Task
        @param conf the conf object for this task (passed to HWRFTask)
        @param section the conf section for this task (passed to HWRFTask)
        @param domain the WRFDomain for this GSI.  Must have been
             initialized by a WRFSimulation
        @param wrf_in_prod the Product for the wrfinput_d01 or
             ghost_d0* file for that domain
        @param sim the hwrf.wrf.WRFSimulation that will be run as the forecast
        @param taskname Optional: the taskname for this product in the datastore
        @param atime the analysis time as a datetime.datetime.
             Default: conf.cycle
        @param parent_atime the analysis time of the parent model.  This is
             relevant if the parent model's forecast is used as the 
             background.
        @param enkf_domains a list of WRF domains that should be copied
             from the hwrf.ensda.EnsembleDA.
        @param ensda a subclass of hwrf.ensda.DAEnsemble that provides
             regional ensemble forecasts to generate the forecast error
             covariance
        @param kwargs ignored; passed to HWRFTask"""
        super(GSIBase,self).__init__(dstore,conf,section,taskname=taskname,
                                     **kwargs)
        taskname=self.taskname
        self._ensda=ensda
        if atime is None: atime=conf.cycle
        if parent_atime is None:
            parent_atime=to_datetime_rel(-6*3600,atime)
        self._atime=to_datetime(atime)
        self._parent_atime=to_datetime(parent_atime)
        self._domain=domain
        self._wrf_in_prod=wrf_in_prod
        self._sim=sim
        self._enkf_domains=list()
        self._tdrflagfile=os.path.join(self.conf.getdir('com'), \
                                       self.icstr('{stormlabel}.tdr'))
        self.used_regional_ensemble=False
        self.global_ensemble_size=80
        self.hybrid_da=True
        if enkf_domains is not None:
            for domain in enkf_domains:
                self._enkf_domains.append(domain)
        assert(wrf_in_prod is not None)
        assert(wrf_in_prod.location is not None)
        assert(wrf_in_prod.location!='')

        # Generate the FileProduct object for our output file:
        with dstore.transaction() as t:
            prodname='gsi_out_'+self.domain.name
            prod=produtil.datastore.FileProduct(dstore,prodname,taskname,
                 location=os.path.join(self.outdir,prodname))
            self._wrf_out_prod=prod

        # Get the datasets and items for our data grabbers:
        def getc(what,default):
            if what in kwargs:
                return str(kwargs[what])
            s=self.confstr(what,'')
            if s is not None and s!='':
                return s
            return default
        #self._sigma_dataset=getc('sigma_dataset','gfs')
        #self._sigma_item=getc('sigma_item','gfs_sf')
        self._sigma_dataset=getc('sigma_dataset','gdas1')
        self._sigma_item=getc('sigma_item','gdas1_sf')
        self._bufr_dataset=getc('bufr_dataset','gfs')
        self._bufr_item=getc('bufr_item',self._bufr_dataset+'_bufr')
        self._prepbufr_item=getc('prepbufr_item','prepbufr_nr')
        self._enkf_dataset=getc('enkf_dataset','enkf')
        self._enkf_item=getc('enkf_item','enkf_sfg')
        self._biascr_dataset=getc('biascr_dataset','gdas1')
        self._biascr_item=getc('biascr_item','gdas1_biascr')
        self._biascr_pc_item=getc('biascr_pc_item','gdas1_biascr_pc')
        self._abias_item=getc('abias_item','gdas1_abias')
        self._satang_item=getc('satang_item','gdas1_satang')

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
                    'In hwrf.gsi.GSIBase.__init__, in_catalog must be None, '
                    'a basestring or a DataCatalog.  You provided an object '
                    'of type %s, with value %s.'
                    %(type(ink).__name__,repr(ink)))
        if self._in_catalog is None:
            if incat_name is None:
                incat_name=self.confstr('catalog')
            self._in_catalog=hwrf.input.DataCatalog(
                self.conf,incat_name,self.parent_atime)

    @property
    def wrf_top_Pa(self):
        """!The WRF model top in pascals"""
        p_top_requested=self._sim.nl.nl_get('domains','p_top_requested')
        return float(p_top_requested)

    @property
    def atime(self): 
        """!The analysis time of this GSI."""
        return self._atime

    @property
    def parent_atime(self): 
        """!Parent model analysis time.

        The analysis time of the parent data assimilation models
        (ie.: GDAS, GFS ENKF). This is usually six hours before
        self.atime."""
        return self._parent_atime

    def set_ensda(self,ensda,enkf_domains):
        """!Sets the hwrf.ensda.DAEnsemble to use, and the enkf domains.

        Specifies the ensemble to use for forecast error covariances,
        and the list of hwrf.wrf.WRFDomain domains that should be
        input to the GSI.
        @param ensda the hwrf.ensda.DAEnsemble that provides short
          simulation output, for the forecast error covariances
        @param enkf_domains an iterable of enkf domains, WRFDomain 
          objects, identifying which domains in ensda should be used."""
        self._enkf_domains=list()
        self._ensda=ensda
        for dom in enkf_domains:
            self._enkf_domains.append(dom)

    def inputiter(self):
        """!Iterate over needed inputs.
        
        Iterates over all files external to this workflow that are
        required to run the GSI.  This may include the GFS ENKF,
        prepbufr, bufr, and other files.  This is used by the
        hwrf.input module to obtain those inputs."""
        # Request GFS ENKF:
        maxmemb=self.confint('num_enkf',80)
        if maxmemb>0:
            fhour=self.conffloat('enkf_fhr',6.0)
            enkf_age=self.conffloat('enkf_age_hr',6.0)
            my_atime=self.atime
            atime=to_datetime_rel(-enkf_age*3600.0,my_atime)
            ftime=to_datetime_rel(3600*fhour,atime)
            for zmemb in xrange(maxmemb):
                imemb=1+zmemb
                yield dict(self.taskvars,dataset=self._enkf_dataset,
                           item=self._enkf_item,atime=atime,enkfmem=imemb,
                           ftime=ftime)

        # Request bias correction and/or satellite angle files:
        parent_atime=self.parent_atime
        if self.confbool('use_newradbc',False):
            yield dict(self.taskvars,dataset=self._biascr_dataset,
                       item=self._biascr_item,ftime=parent_atime,
                       atime=parent_atime)
            yield dict(self.taskvars,dataset=self._biascr_dataset,
                       item=self._biascr_pc_item,ftime=parent_atime,
                       atime=parent_atime)
        else:
            if self._abias_item:
                yield dict(self.taskvars,dataset=self._biascr_dataset,
                           item=self._abias_item,ftime=parent_atime,
                           atime=parent_atime)
            if self._satang_item:
                yield dict(self.taskvars,dataset=self._biascr_dataset,
                           item=self._satang_item,ftime=parent_atime,
                           atime=parent_atime)
        
        # Request prepbufr file:
        atime=self.atime
        yield dict(self.taskvars,dataset=self._bufr_dataset,
                   item=self._prepbufr_item,atime=atime)

        # Request bufr files:
        olist=self.confstr('obstypes')
        touched=set()
        for section in olist.split(','):
            trim=section.strip()
            if len(trim)<=0 or trim in touched: continue
            dataset=self.conf.get(section,'dataset')
            item=self.conf.get(section,'item')
            otype=self.conf.get(section,'type').lower()
            if otype=='satellite' and not self.confbool('sat_radiance_da',True):
                continue
            if otype=='satwnd' and not self.confbool('sat_wnd_da',True):
                continue
            for (localname,obstype) in self.conf.items(section):
                if localname in ['dataset','item','type']: 
                    # These are not obs types, so skip them.
                    continue
                yield dict(self.taskvars,dataset=dataset,item=item,
                           obstype=obstype,atime=atime,optional=True)
                # NOTE: optional=True, so the exhwrf_input will
                # continue if a bufr file is missing.

        # GDAS data:
        atime=self.parent_atime
        for hr in self.parent_fhrs():
            ftime=atime+datetime.timedelta(hours=hr)
            yield dict(self.taskvars,dataset=self._sigma_dataset,
                       item=self._sigma_item,atime=atime,ftime=ftime)

    def grab_enkf_input(self):
        """!Link or copy ensemble inputs.

        Links or copies the output of the last ENKF or hwrf.ensda
        cycle.  Calls grab_wrf_enkf() if any enkf_domains were given
        to the constructor.  Otherwise, calls grab_gfs_enkf() to get
        the global enkf.  Will also revert to grab_gfs_enkf() if the
        ensda should be used, but was unavailable."""
        self.used_regional_ensemble=False
        use_hwrf_ensemble=self.confbool('use_hwrf_ensemble',False)
        ensda_when=self.confstr('ensda_when','tdr_next_cycle').lower()
        stormlabel=self.conf.get('config','stormlabel')
        if ensda_when=='always' or ensda_when==stormlabel:
            self.used_regional_ensemble=self.grab_wrf_enkf(self._ensda)
        elif use_hwrf_ensemble and len(self._enkf_domains)>0 and \
           self._ensda is not None and (isnonempty(self._tdrflagfile)
           or ensda_when=='always'):
            self.used_regional_ensemble=self.grab_wrf_enkf(self._ensda)

        if not self.used_regional_ensemble:
            self.grab_gfs_enkf()

    def grab_bufr(self,atime=None,morevars=None):
        """!Link bufr files.

        Links or copies all needed bufr files to the local directory.
        If sat_da is False, satellite obs will be omitted.
        @param atime the analysis time to use when specifying the 
           required bufr files
        @param morevars additional variables to pass for string
           replacement when expanding bufr filenames in 
           configuration (hwrf.config.HWRFConfig) sections."""
        olist=self.confstr('obstypes')
        touched=set()
        for osection in olist.split(','):
            trim=osection.strip()
            if len(trim)>0 and not trim in touched:
                self.grab_obstype_section(trim,atime=atime,morevars=morevars)

    def grab_obstype_section(self,section,atime=None,morevars=None):
        """!Copies or links observations.

        @param section the obstype section to read
        @param atime the atime for string expansion when finding bufr files
        @param morevars more variables for string expansion when
          finding bufr files

        Copies or links observations specified in the obstype sections
        of the configuration file to the current working directory.

        The section listed in self.section should contain an "obstype"
        option, whose value is a comma separated list of section
        names.  This method reads every section in that list.  For example,

        @code{.unformatted}
           [gsi_d02]
           catalog = {input_catalog}
           ...
           obstypes = hdob_obstype,sat_obstypes,tdr_new_obstype
           ...
           [sat_obstypes]
           type=satellite
           dataset=gfs
           item=gfs_bufr
           gsnd1bufr=goesfv
           amsuabufr=1bamua
           satwndbufr=satwnd
           gpsrobufr=gpsro
           ...
           [hdob_obstype]
           ...
           [tdr_new_obstype]
           ...
        @endcode

        For each section, the option keys are the local directory
        filenames expected by GSI, while the values are the data type
        part of the operational filename (ie.: the satwind in
        gfs.t12z.tm00.satwind.bufr_d).  There are a few special keys:

          dataset - the name of the dataset for hwrf.input purposes
          item - the name of the item for hwrf.input purposes
          type - the type of observation: satellite, or anything else.
            At present, only "satellite" has any special meaning.

        If the type is "satellite" then the entire section will be
        skipped if sat_radiance_da=False in this task's config section.

        If the type is "satwnd" then the entire section will be
        skipped if sat_wnd_da=False in this task's config section.

        Once the section is parsed, the files are all linked to this
        directory.        """
        logger=self.log()
        if not isinstance(section,basestring): section=str(section)

        if atime is None: 
            atime=self.atime
        else:
            atime=to_datetime_rel(atime,self.atime)

        dataset=self.conf.get(section,'dataset')
        item=self.conf.get(section,'item')
        otype=self.conf.get(section,'type').lower()

        logger.warning('process obs section %s with dataset=%s item=%s '
                       'type=%s'%(section,dataset,item,otype))

        if otype=='satellite':
            if self.confbool('sat_radiance_da',True):
                logger.warning('%s: satellite radiance DA '
                               'is enabled.  Continuing...'%(section,))
            else:
                logger.warning('%s: satellite radiance DA is disabled in hwrf.conf.  '
                               'Not linking bufr files from this section.'
                               %(section,))
                return

        if otype=='satwnd':
            if self.confbool('sat_wnd_da',True):
                logger.warning('%s: assimilation of satellite wind '
                               'is enabled.  Continuing...'%(section,))
            else:
                logger.warning('%s: assimilation of satellite wind is disabled in hwrf.conf.  '
                               'Not linking bufr files from this section.'
                               %(section,))
                return

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
                    'In gsi.GSIBase.link_bufr, the obstypes parameter must '
                    'be an iterable container of basestrings.  One of the '
                    'elements was a %s (value %s) instead.'
                    %(type(obstype).__name__,repr(obstype)))
            if dataset == 'tdr' and bool(self.realtime):
                ds=os.path.join(self.getdir('intercom'),'bufrprep') 
                it=self._in_catalog.parse(item,atime=atime,
                                   logger=logger,obstype='tldplr')
                there=os.path.join(ds,it) 
            else:
                there=self._in_catalog.locate(dataset,item,atime=atime,
                                   logger=logger,obstype=obstype)
            if there is None or there=='':
                msg='%s: Could not find a location for this obstype.'\
                    %(obstype,)
                logger.warning(msg)
                if self.confbool('require_all_bufrs',False):
                    raise GSIInputError(msg)
            elif produtil.fileop.isnonempty(there):
                bn=os.path.basename(there)
                on=namer(bn,obstype)
                if dataset != 'tdr' or isnonempty(self._tdrflagfile):
                    make_symlink(there,on,logger=logger,force=True)
            else:
                msg='%s: Observation file is empty or non-existent: %s'\
                    %(obstype,there)
                logger.warning(msg)
                if self.confbool('require_all_bufrs',False):
                    raise GSIInputError(msg)
            

    def grab_prepbufr(self,atime=None,**kwargs):
        """!Links or copies the prepbufr file to the local directory.

        @param atime the analysis time, or time relative to
          self.atime.  Used for string expansion in the
          hwrf.config.HWRFConfig.
        @param kwargs also passed to the hwrf.config.HWRFConfig
          for string expansion"""
        if atime is None:
            atime=self.atime
        else:
            atime=to_datetime_rel(atime,self.atime)
        logger=self.log()
        there=self._in_catalog.locate(self._bufr_dataset,self._prepbufr_item,
            atime=atime,logger=logger,**kwargs)
        if self.conf.getint('bufrprep','prepbufrprep',0) == 0:
            there=self._in_catalog.locate(self._bufr_dataset,self._prepbufr_item,
                                          atime=atime,logger=logger,**kwargs)
        else:
            ds=os.path.join(self.getdir('intercom'),'bufrprep')
            it=self._in_catalog.parse(self._prepbufr_item,
                                   atime=atime,logger=logger,**kwargs)
            there=os.path.join(ds,it)
        if there is None or there=='':
            msg='Could not find the prepbufr file (item=%s dataset=%s)' \
                %(repr(self.prepbufr_item),repr(self._bufr_dataset))
            logger.warning(msg)
            raise GSIInputError(msg)
        elif not produtil.fileop.isnonempty(there):
            msg=there+': is non-existent or empty'
            logger.error(msg)
            raise GSIInputError(msg)
        make_symlink(there,'prepbufr',logger=logger,force=True)

    def write_vitals(self,filename='tcvitl'):
        """!Writes the tcvitals (from self.storminfo) to the specified
        file.
        @param filename the file to receive the tcvitals"""
        logger=self.log()
        logger.info('Writing tcvitals to %s'%(repr(filename),))
        with open(filename,'wt') as f:
            f.write(self.storminfo.as_tcvitals()+"\n")
        assert(os.path.exists(filename))

    def wrfout_copier(self,file):
        """!Generate the wrfout file converter.

        Returns the "copier" argument to deliver_file to use to copy
        the specified file.  Will be None, unless the file is HDF5, in
        which case it will be "ncks -6 source target" to decompress
        and convert to NetCDF3 style (uncompressed) with 64-bit
        indexing.  If ncks is missing, such a conversion is
        impossible, so None is returned.

        @param file the file that is to be copied"""
        logger=self.log()
        ncks=self.getexe('ncks','')
        if not ncks:
            logger.warning('ncks path not specified in conf [exe] '
                           'so I will search your $PATH')
            ncks=produtil.fileop.find_exe('ncks',raise_missing=False)
        if ncks:
            def copy(s,t,x):
                produtil.fileop.remove_file(t,logger=logger)
                checkrun(bigexe(ncks)['-6','-O',s,t]<'/dev/null',logger=logger)
            return copy
        else:
            return None

    def grab_wrf_enkf(self,ensda):
        """!Links the WRF ENKF files to this directory.
        @param ensda the hwrf.ensda.DAEnsemble that provides the files"""
        logger=self.log()
        logger.info('in grab_wrf_enkf')
        nameme=dict()
        plist=list()
        did=None
        for (ens,member) in ensda.members_at_time(self.atime):
            logger.info('ens,member = %s,%s'%(repr(ens),repr(member)))
            iens=int(ens)
            for domain in self._enkf_domains:
                logger.info('ens,member,domain = %s,%s,%s'
                            %(repr(ens),repr(member),repr(domain)))
                did=int(domain.get_grid_id())
                if self.conf.getbool('config','run_ens_relocation'):
                    if domain.is_moad():
                        continue
                    else:
                        prod=member.rstage3.get_wrfout(domain=domain)
                else:
                    if domain.is_moad():
                        continue
                    else:
                        prod=member.get_wrfanl(domain=domain,atime=self.atime)
                if prod is None:
                    logger.info('No product for domain %s.'%(str(domain),))
                    continue
                logger.info('domain %s prod %s'%(str(domain),prod.did))
                nameme[prod]='wrf_en%03d'%iens
                plist.append(prod)
        assert(did is not None)
        def renamer(p,l): return nameme[p]
        def copy(fromfile,tofile,copier):
            deliver_file(fromfile,tofile,logger=logger,keep=True,copier=copier)
        def link(fromfile,tofile):
            produtil.fileop.make_symlink(fromfile,tofile,logger=logger)

        workpool=None
        
        def actor(p,n,l): 
            fromfile=p.location
            l.info('Link %s to %s'%(fromfile,n))
            if produtil.fileop.netcdfver(fromfile)=='HDF5':
                l.info('%s: file is HDF5.  I will assume your GSI was built with support for compressed NetCDF4'%(fromfile,))
                copier=None
            else:
                l.info('%s: file is NetCDF3.'%(fromfile,))
                copier=None

            if copier is None:
                workpool.add_work(link,[fromfile,n])
            else:
                workpool.add_work(copy,[fromfile,n,copier])

        maxtime=self.confint('maxwait',240)
        if not self.realtime: maxtime=30

        with produtil.workpool.WorkPool(4) as wp:
            workpool=wp
            count=produtil.datastore.wait_for_products(
                plist,logger,renamer,actor,maxtime=maxtime)
            workpool.barrier()

        wanted=len(plist)
        if count<wanted:
            logger.critical('Found only %d of %d WRF ENKF files - '
                            'using GFS ENKF instead.'%(count,wanted))
            return False
        self.postmsg('Found all %d of %d WRF ENKF files.'%(count,wanted))
        return True

    def grab_gfs_enkf(self,atime=None,**kwargs):
        """!Links the GFS ENKF files to this directory
        @param atime the analysis time, or time relative to
          self.atime.  Used for string expansion in the
          hwrf.config.HWRFConfig.
        @param kwargs also passed to the hwrf.config.HWRFConfig
          for string expansion"""
        logger=self.log()
        maxmemb=self.confint('num_enkf',80) 
        if maxmemb==0: 
            logger.warning('Number of ENKF members requested is 0.  Will '
                           'not link anything.')
            return

        fhour=self.conffloat('enkf_fhr',6.0)
        enkf_age=self.conffloat('enkf_age_hr',6.0)
        my_atime=self.atime
        assert(my_atime is not None)
        atime=to_datetime_rel(-enkf_age*3600.0,my_atime)
        ftime=to_datetime_rel(3600*fhour,atime)
        with open('filelist06','wt') as fl:
            localmemb=0
            for zmemb in xrange(maxmemb):
                imemb=1+zmemb
                there=self._in_catalog.locate(self._enkf_dataset,
                    self._enkf_item,atime=atime,logger=logger,
                    enkfmem=imemb,ftime=ftime,**kwargs)
                if not produtil.fileop.isnonempty(there):
                    if not bool(self.realtime):
                        raise GSIInputError('required input file '
                                            'is empty or non-existent: %s'
                                              %(there,))
                    else:
                        logger.warning(there+': is empty or non-existent')
                else:
                    localmemb=localmemb+1
                    local=self.conftimestrinterp(
                    'sfg_{aYMDH}_fhr{fahr:02d}s_mem{imemb:03d}',
                    atime=atime,ftime=ftime,imemb=localmemb)
                    produtil.fileop.make_symlink(there,local,force=True,
                                             logger=logger)
                    fl.write(local+'\n')
            self.global_ensemble_size=localmemb
            if localmemb < 40:
                logger.info('Ensemble member is less than 40!!!'
                            'Will run 3DVAR instead of hybrid analysis')
                self.hybrid_da=False
            else:
                self.hybrid_da=True

    def copy_wrf_inout(self,filename='wrf_inout'):
        """!Copies the WRF analysis or input file to the specified
        filename
        @param filename the file to receive the data"""
        logger=self.log()
        def namer(p,logger,*args): return filename
        def actor(p,name,logger,*args): 
            deliver_file(p.location,name,logger=logger)
        produtil.datastore.wait_for_products([self._wrf_in_prod],
            logger,namer,actor)

    def get_ghost(self,domain):
        """!Obtain output ghost product for the specified domain.

        If this GSI is being run on the specified domain, returns the
        output product of GSI, otherwise returns None.
        @param domain the WRFDomain of interest"""
        if domain==self.domain:
            return self._wrf_out_prod
        return None

    def get_wrfanl(self,domain):
        """!Obtain output ghost product for the specified domain.

        If this GSI is being run on the specified domain, returns
        the output product of GSI, otherwise returns None.
        @param domain the WRFDomain of interest"""
        if domain==self.domain:
            return self._wrf_out_prod
        return None

    def get_wrfinput(self):
        """!Obtain output wrfinput data for the outermost WRF domain.

        If this GSI is being run on the WRF outermost domain (Mother
        Of All Domains, or MOAD), returns the output product of GSI.
        Otherwise, returns None."""
        if self.domain.is_moad():
            return self._wrf_out_prod
        else:
            return None
    @property
    def domain(self): 
        """!The WRF domain for which GSI is being run."""
        return self._domain

    def products(self,domains=None,prodtype='wrf_out_prod'):
        """!Iterates over all output products of this Task.
        @param domains the domains of interest
        @param prodtype ignored"""
        if domains is None:
            yield self._wrf_out_prod
        else:
            for domain in domains:
                if domain==self.domain:
                    yield self._wrf_in_prod
                    break

    def make_gsi_namelist(self,filename='gsiparm.anl'):
        """!Creates the GSI namelist in the specified file.
        @param filename the destination filename"""
        logger=self.log()
        invars=dict()

        use_gfs_stratosphere=self.confbool('use_gfs_stratosphere',True)
        if self.wrf_top_Pa < 201.0 and self.confbool('sat_radiance_da',True):
            invars.update( USE_GFS_STRATOSPHERE=use_gfs_stratosphere,
                           USE_GFS_OZONE=True,
                           REGIONAL_OZONE=False,
                           NDAT=80 )
        else:
            invars.update( USE_GFS_STRATOSPHERE=False,
                           USE_GFS_OZONE=False,
                           REGIONAL_OZONE=False,
                           NDAT=16 )

        if not self.confbool('use_newradbc',False):
            SETUP='''upd_pred(1)=0,upd_pred(2)=0,upd_pred(3)=0,
                     upd_pred(4)=0,upd_pred(5)=0,upd_pred(6)=0,
                     upd_pred(7)=0,upd_pred(8)=0,upd_pred(9)=0,
                     upd_pred(10)=0,upd_pred(11)=0,upd_pred(12)=0,'''
        else:
            SETUP='''newpc4pred=.true., adp_anglebc=.true., angord=4,
                     passive_bc=.false., use_edges=.false., emiss_bc=.true.,
                     diag_precon=.true., step_start=1.e-3, upd_pred(1)=0,
                     upd_pred(2)=0,upd_pred(3)=0,upd_pred(4)=0,
                     upd_pred(5)=0,upd_pred(6)=0,upd_pred(7)=0,
                     upd_pred(8)=0,upd_pred(9)=0,upd_pred(10)=0,
                     upd_pred(11)=0,upd_pred(12)=0,'''

        dom=self._sim[self._domain]
        nx=dom.nx
        assert(nx)
        ny=dom.ny
        assert(ny)
        nz=dom.nz
        assert(nz)
        invars.update(NLAT=ny-1, NLON=nx-1, NETCDF=True, LEVS=nz-1,
                      NLON_ENS_REGIONAL=nx-1,SETUP=SETUP,
                      NLAT_ENS_REGIONAL=ny-1)
        invars.update(tdrtype='tldplrbufr')#FIXME: tdrtype
        invars.update(gps_dtype='gps_bnd')

        singleobstest=self.confbool('singleobstest',False)
        invars.update(ONEOBSTEST=singleobstest)

        if self.used_regional_ensemble:
            logger.info('Using regional ensemble.')
            invars.update( MERGE_TWO_GRID_ENSPERTS=False,
                           REGIONAL_ENSEMBLE_OPTION=2,
                           ENSEMBLE_SIZE_REGIONAL=self._ensda.nmembers)
        else:
            logger.info('Using global ensemble.')
            invars.update( HYBENS_REGIONAL=self.hybrid_da,
                           ENSEMBLE_SIZE_REGIONAL=self.global_ensemble_size)

        nml_file=self.confstr('nml_file')
        nml_section=self.confstr('nml_section',self.section)
        atime=self.atime
        ni=hwrf.namelist.NamelistInserter(self.conf,nml_section)

        with open(nml_file,'rt') as nf:
            with open(filename,'wt') as of:
                of.write(ni.parse(nf,logger=logger,source=nml_file,
                                  raise_all=True,atime=atime,**invars))

    def after_gsi(self):
        """!Called by run() after the gsi executable completes.

        This is intended to be overridden by subclasses to perform
        some action after gsi is complete, but before products are
        delivered.  The default implementation does nothing."""

    def before_gsi(self):
        """!Called by run() just before running the gsi program.

        This is intended to be overridden by subclasses to perform
        some action after all inputs needed for gsi are available, but
        before gsi starts.  The default implementation does nothing."""

    def grab_more_inputs(self):
        """!Called by run() to obtain additional inputs before before_gsi()

        This is intended to be overridden by subclasses to copy or
        link more inputs for GSI.  The default implementation does
        nothing."""

    def grab_bias_satang(self):
        """!Copies or links bias correction and satellite angle files"""
        logger=self.log()
        #parent_atime=self._atime
        parent_atime=self.parent_atime
        def get(ds,it,fn):
            logger.info('%s: search for this at dataset=%s item=%s time=%s'
                        %(fn,ds,it,parent_atime.strftime('%Y%m%d%H')))
            there=self._in_catalog.locate(ds,it,atime=parent_atime,
                                          logger=logger,ftime=parent_atime)
            logger.info('%s: found at %s'%(fn,there))
            make_symlink(there,fn,force=True,logger=logger)
        if self.confbool('use_newradbc',False):
            get(self._biascr_dataset,self._biascr_item,'satbias_in')
            get(self._biascr_dataset,self._biascr_pc_item,'satbias_pc')
        else:
            get(self._biascr_dataset,self._abias_item,'satbias_in')
            get(self._biascr_dataset,self._satang_item,'satbias_angle')

    def run_gsi_exe(self):
        """!Runs the actual GSI executable."""
        logger=self.log()

        cmd = mpi(self.getexe('gsi'))

        threads=os.environ.get('GSI_THREADS','1')
        threads=int(threads)
        if threads>1:
            cmd=openmp(cmd,threads=threads)
        
        cmd = mpirun(cmd,allranks=True).env(OMP_STACKSIZE='128M')
        if threads==1:
            cmd=openmp(cmd,threads=threads)

        cmd = cmd < 'gsiparm.anl'
        
        # Redirection is mandatory for the GSI so we can process the
        # output later on.
        cmd = cmd > 'stdout'

        logger.warning(repr(cmd))
        # Send a message at the highest logging level before and after
        # GSI so the NCEP-wide jlogfile has a message about the status
        # of GSI.
        self.postmsg('Starting GSI for %s domain'%(str(self.domain),))

        sleeptime=self.conffloat('sleeptime',30.0)
        ntries=0
        maxtries=1
        while ntries<maxtries:
            try:
                ntries+=1
                getrlimit(logger=logger)
                with rusage(logger=logger):
                    checkrun(cmd,sleeptime=sleeptime)
                break
            except Exception as e:
                if ntries>=maxtries:
                    logger.critical('GSI failed for %s domain: %s'
                                    %(str(self.domain),str(e)),exc_info=True)
                    raise
                else:
                    logger.warning('GSI failed for %s domain: %s, will retry %d more time(s)'
                                 %(str(self.domain),str(e),maxtries-ntries),
                                 exc_info=True)
        self.postmsg('GSI succeeded for %s domain'%(str(self.domain),))

    def deliver_products(self):
        """!Delivers output products.

        This function is called by run() to deliver output files to
        the intercom or com directory and record in the database that
        they are delivered."""
        d=os.path.dirname(self._wrf_out_prod.location)
        logger=self.log()
        produtil.fileop.makedirs(d,logger=logger)
        self._wrf_out_prod.deliver(frominfo='wrf_inout',keep=False,
                                   logger=logger)
        produtil.fileop.deliver_file(
            'satbias_out',os.path.join(self.outdir,'satbias_out'),
            keep=False,logger=logger)

    def make_diag_files(self,tgtpre,nthreads):
        """!Creates GSI diagnostic files.

        Makes some diagnostic files and copies them to the specified
        delivery location.  Part of this routine is threaded: specify
        the number of worker threads in nthreads.  Minimum is 1.
        @param tgtpre the prefix to the output names, including the full path
        @param nthreads the maximum number of threads to use"""
        assert(nthreads>=1)
        assert(isinstance(tgtpre,basestring))
        assert(os.path.isabs(tgtpre))

        logger=self.log()

        # This section makes the many diagnostic *.gz files in worker
        # threads.  The actual work is done in _make_diag_for.
        logger.info('make diag *.gz files')
        with produtil.workpool.WorkPool(nthreads,logger) as workers:

            # Request that the workers make the stdout.anl:
            workers.add_work(self._make_stdout_anl,[tgtpre,logger])

            # Request that the workers deliver ensemble spread
            workers.add_work(self._make_ensemble_spread,[tgtpre,logger])

            # Request that the workers make the diag*gz files for all
            # data types:
            for dtype in GSI_DATA_TYPES:
                workers.add_work(self._make_diag_for,[tgtpre,dtype,logger])

            workers.barrier() # wait for work to complete

    def _make_stdout_anl(self,tgtpre,logger):
        """! Concatenates many "stdout" files into one.

        Do not call this function directly; it is part of the
        implementation of make_diag_files.  It concatenates "stdout"
        and fort.2* to a single log file.
        @param tgtpre the prefix to the output names, including the full path
        @param logger a logging.Logger to use for logging messages"""

        logger.info('stdout.anl: make this from stdout and fort.2*')
        with open('stdout.anl','wb') as outf:
            logger.info('stdout.anl: stdout')
            with open('stdout','rb') as inf:
                shutil.copyfileobj(inf,outf)
            for infile in glob.iglob('fort.2*'):
                infile=str(infile)
                logger.info('stdout.anl: %s'%(infile,))
                with open(infile,'rb') as inf:
                    shutil.copyfileobj(inf,outf)
        stdoutanl=tgtpre+'.stdout.anl'
        produtil.fileop.deliver_file(
            'stdout.anl',stdoutanl,keep=False,logger=logger)

    def _make_ensemble_spread(self,tgtpre,logger):
        """Do not call this function directly; it is part of the
        implementation of make_diag_files.  It delivers ensemble 
        spread binary and ctl files to com directory."""

        logger.info('deliver ensemble spread files')
        ctl=tgtpre+'.ens_spread.ctl'
        grd=tgtpre+'.ens_spread.grd'
        if isnonempty('ens_spread.ctl'):
            produtil.fileop.deliver_file(
                'ens_spread.ctl',ctl,keep=False,logger=logger)
            produtil.fileop.deliver_file(
                'ens_spread.grd',grd,keep=False,logger=logger)

    def _make_diag_for(self,tgtpre,dtype,logger):
        """!Generates one diagnostic output file.

        Do not call this function directly.  It is part of the
        internal implementation of make_diag_files.  Each worker
        thread calls this function for one GSI data type in the
        GSI_DATA_TYPES array, to create two zlib-compressed (gzipped)
        copies of various diagnostic files for that data type.  There
        are two such files per datatype: one for the first guess
        ("ges") and one for the analysis ("anl").
        @param tgtpre the prefix to the output names, including the full path
        @param logger a logging.Logger to use for logging messages
        @param dtype the string name of this datatype"""
        for loop in [ '01', '03' ]:
            string=loop
            if string=='01': string='ges'
            if string=='03': string='anl'
            theglob='pe*.%s_%s*'%(dtype,loop)
            globbed=sorted(glob.glob(theglob))
            if not globbed:
                logger.warning('No %s'%(theglob,))
                continue
            tmpfile='diag_%s_%s.gz'%(dtype,string)
            outfile=tgtpre+'.'+tmpfile
            logger.info('%s: gzip compress %d %s'
                        %(tmpfile,len(globbed),theglob))

            blocksize=1048576*64
            outf=None
            try:
                outf=gzip.open(tmpfile,'wb')
                for infile in globbed:
                    logger.info('%s: gzip compress %s'%(tmpfile,infile))
                    with open(infile,'rb') as inf:
                        while True:
                            indata=inf.read(blocksize)
                            if indata is None or indata=='':
                                break # end of input file
                            outf.write(indata)
            finally:
                if outf is not None:
                    outf.close()
                    del outf
            produtil.fileop.deliver_file(
                tmpfile,outfile,keep=False,logger=logger)

    def run(self):
        """!Runs the GSI and delivers the results.

        Executes the GSI in a temporary scrub directory, deleting it
        afterwards if self.scrub is False.  Follows this overall
        pattern:
        1. Make temporary area and cd there
        2. Copy inputs
        3. Run the grab_more_inputs(), which subclasses should override
        4. Calls make_gsi_namelist() to generate the namelist
        5. Calls before_gsi() which subclasses should override
        6. Calls run_gsi_exe() to run the actual GSI program
        7. Calls after_gsi() which subclasses should override
        8. Calls deliver_products() to copy files to COM 
        9. Generates diagnostic files.
        10. Deletes the temporary directory if self.scrub=False"""
        logger=self.log()
        dirname=self.workdir
        logger.info('Run gsi in directory %s'%(dirname,))
        if os.path.exists(dirname):
            logger.info('Delete old data in %s'%(dirname,))
            shutil.rmtree(dirname)
        with NamedDir(dirname,keep=not self.scrub):
            self.grab_fix_parm()
            self.grab_enkf_input()
            if not self.confbool('singleobstest',False):
                self.grab_bufr()
                self.grab_prepbufr()
                self.write_vitals()
            else:
                logger.info('This is a single obs test.')
            self.grab_bias_satang()
            self.grab_more_inputs()
            self.copy_wrf_inout()
            self.make_gsi_namelist()

            self.before_gsi()
            self.run_gsi_exe()
            self.after_gsi()

            self.deliver_products()

            diagpre=self.confstr('diagpre','')
            diagthreads=self.confint('diagthreads',10)
            if diagpre is None or diagpre=='':
                logger.info('GSI diagnostic outputs are disabled.')
            else:
                self.make_diag_files(diagpre,diagthreads)

        self.state=COMPLETED

    def grab_fix_parm(self):
        """!Links or copies to the local directory any fix or parm
        files needed by GSI."""
        logger=self.log()
        logger.info("Copying fix and parm files")
        s=self.icstr
        def lnsf(a,b):
            produtil.fileop.make_symlink(a,b,logger=logger,force=True)
       
        if self.confbool('sat_radiance_da',True):
            if self.confbool('use_gfs_stratosphere',True):
                anavinfo=s('{FIXgsi}/anavinfo_hwrf_L75')
            elif (self.confbool('hwrf_43lev_conf',False,section='prelaunch') or self.confbool('hwrf_other_conf',False,section='prelaunch')):
                anavinfo=s('{FIXgsi}/anavinfo_hwrf_L42')
            else:
                anavinfo=s('{PARMhwrf}/anavinfo_hwrf_L60')
        else:
            if (self.confbool('hwrf_43lev_conf',False,section='prelaunch') or self.confbool('hwrf_other_conf',False,section='prelaunch')):
                anavinfo=s('{FIXgsi}/anavinfo_hwrf_L42_nooz')
            else:
                anavinfo=s('{PARMhwrf}/anavinfo_hwrf_L60_nooz')
        berror=s('{FIXgsi}/nam_glb_berror.f77.gcv')
        emiscoef_IRwater=s('{FIXcrtm}/EmisCoeff/IR_Water/Big_Endian/Nalli.IRwater.EmisCoeff.bin')
        emiscoef_IRice=s('{FIXcrtm}/EmisCoeff/IR_Ice/SEcategory/Big_Endian/NPOESS.IRice.EmisCoeff.bin')
        emiscoef_IRland=s('{FIXcrtm}/EmisCoeff/IR_Land/SEcategory/Big_Endian/NPOESS.IRland.EmisCoeff.bin')
        emiscoef_IRsnow=s('{FIXcrtm}/EmisCoeff/IR_Snow/SEcategory/Big_Endian/NPOESS.IRsnow.EmisCoeff.bin')
        emiscoef_VISice=s('{FIXcrtm}/EmisCoeff/VIS_Ice/SEcategory/Big_Endian/NPOESS.VISice.EmisCoeff.bin')
        emiscoef_VISland=s('{FIXcrtm}/EmisCoeff/VIS_Land/SEcategory/Big_Endian/NPOESS.VISland.EmisCoeff.bin')
        emiscoef_VISsnow=s('{FIXcrtm}/EmisCoeff/VIS_Snow/SEcategory/Big_Endian/NPOESS.VISsnow.EmisCoeff.bin')
        emiscoef_VISwater=s('{FIXcrtm}/EmisCoeff/VIS_Water/SEcategory/Big_Endian/NPOESS.VISwater.EmisCoeff.bin')
        emiscoef_MWwater=s('{FIXcrtm}/EmisCoeff/MW_Water/Big_Endian/FASTEM6.MWwater.EmisCoeff.bin')
        aercoef=s('{FIXcrtm}/AerosolCoeff/Big_Endian/AerosolCoeff.bin')
        cldcoef=s('{FIXcrtm}/CloudCoeff/Big_Endian/CloudCoeff.bin')
        satinfo=s('{FIXgsi}/hwrf_satinfo.txt')
        atmsfilter=s('{FIXgsi}/atms_beamwidth.txt')
        scaninfo=s('{FIXgsi}/global_scaninfo.txt')
        satangl=s('{FIXgsi}/nam_global_satangbias.txt')
        pcpinfo=s('{FIXgsi}/nam_global_pcpinfo.txt')
        ozinfo=s('{FIXgsi}/global_ozinfo.txt')
        errtable=s('{FIXgsi}/hwrf_nam_errtable.r3dv')
        convinfo=s('{FIXgsi}/hwrf_convinfo.txt')
        
        # Only need this file for single obs test
        bufrtable=s('{FIXgsi}/prepobs_prep.bufrtable')
        
        # Only need this file for sst retrieval
        bftab_sst=s('{FIXgsi}/bufrtab.012')
        
        # NOTE: GSI now run for all storm depths.
        lnsf(anavinfo, './anavinfo')
        lnsf(berror,   './berror_stats')
        lnsf(emiscoef_IRwater, './Nalli.IRwater.EmisCoeff.bin')
        lnsf(emiscoef_IRice, './NPOESS.IRice.EmisCoeff.bin')
        lnsf(emiscoef_IRsnow, './NPOESS.IRsnow.EmisCoeff.bin')
        lnsf(emiscoef_IRland, './NPOESS.IRland.EmisCoeff.bin')
        lnsf(emiscoef_VISice, './NPOESS.VISice.EmisCoeff.bin')
        lnsf(emiscoef_VISland, './NPOESS.VISland.EmisCoeff.bin')
        lnsf(emiscoef_VISsnow, './NPOESS.VISsnow.EmisCoeff.bin')
        lnsf(emiscoef_VISwater, './NPOESS.VISwater.EmisCoeff.bin')
        lnsf(emiscoef_MWwater, './FASTEM6.MWwater.EmisCoeff.bin')
        lnsf(aercoef,  './AerosolCoeff.bin')
        lnsf(cldcoef,  './CloudCoeff.bin')
        #lnsf(satangl,  './satbias_angle')
        lnsf(satinfo,  './satinfo')
        lnsf(scaninfo, './scaninfo')
        lnsf(pcpinfo,  './pcpinfo')
        lnsf(ozinfo,   './ozinfo')
        lnsf(convinfo, './convinfo')
        lnsf(errtable, './errtable')
        lnsf(atmsfilter, './atms_beamwidth.txt')
        lnsf(bufrtable, './prepobs_prep.bufrtable')
        lnsf(bftab_sst, './bftab_sstphr')
        
        # Copy CRTM coefficient files based on entries in satinfo file
        with open('satinfo','rt') as f:
            for line in f:
                splat=line.split()
                if not splat or splat[0][0]=='!': continue
                satsen=splat[0]
                spccoeff=s('{satsen}.SpcCoeff.bin',satsen=satsen)
                if not isnonempty(spccoeff):
                    make_symlinks_in([
                            s('{FIXcrtm}/SpcCoeff/Big_Endian/{spccoeff}',
                              spccoeff=spccoeff),
                            s('{FIXcrtm}/TauCoeff/Big_Endian/{satsen}.TauCoeff.bin',
                              satsen=satsen) ],
                                     '.',force=True,logger=logger)

class FGATGSI(GSIBase):
    """!Runs the GSI based on the HWRF FGAT scheme."""
    def __init__(self,dstore,conf,section,domain,wrf_in_prod,fgat_in_prods,
                 sim,cycling_interval=None,taskname=None,atime=None,
                 enkf_domains=None,ensda=None,**kwargs):
        """!The FGATGSI constructor:
        @param dstore passed to Datum: the Datastore object for this Task
        @param conf the conf object for this task (passed to HWRFTask)
        @param section the conf section for this task (passed to HWRFTask)
        @param domain the WRFDomain for this GSI.  Must have been
             initialized by a WRFSimulation
        @param wrf_in_prod the Product for the wrfinput_d01 or
             ghost_d0* file for that domain
        @param fgat_in_prods a mapping from analysis time to a product
             for all of the FGAT times
        @param sim the hwrf.wrf.WRFSimulation that will be run as the forecast
        @param cycling_interval Optional: time between HWRF forecast
            cycles in seconds
        @param taskname Optional: the taskname for this product in the datastore
        @param atime the analysis time as a datetime.datetime.
             Default: conf.cycle
        @param enkf_domains a list of WRF domains that should be copied
             from the hwrf.ensda.EnsembleDA.
        @param ensda a subclass of hwrf.ensda.DAEnsemble that provides
             regional ensemble forecasts to generate the forecast error
             covariance
        @param kwargs ignored; passed to HWRFTask"""
        assert(wrf_in_prod is not None)
        if cycling_interval is None: cycling_interval=6*3600
        self.cycling_interval=to_timedelta(cycling_interval)
        if atime is None: atime=conf.cycle
        for prod in fgat_in_prods.itervalues():
            assert(isinstance(prod,produtil.datastore.Product))
        self._fgat_in_prod = [ (prod,to_datetime_rel(time,atime)) \
            for time,prod in fgat_in_prods.iteritems() ]
        assert(wrf_in_prod is not None)
        super(FGATGSI,self).__init__(dstore,conf,section,domain,wrf_in_prod,
           sim,taskname=taskname,atime=atime,enkf_domains=enkf_domains,
           ensda=ensda,**kwargs)
    ##@var cycling_interval
    # the time between forecast cycles, a datetime.timedelta

    def parent_fhrs(self):
        """!Iterates over FGAT forecast hours, relative to the parent
        analysis time.

        Iterates over all FGAT forecast hours (as ints) relative to
        the parent analysis time.  For example, 3, 6, and 9 for
        three-hourly FGAT off of GDAS."""
        parent_atime=self.parent_atime
        for p,t in self._fgat_in_prod:
            yield int(round(to_fraction(t-parent_atime)/3600))

    def fgat_fhrs(self):
        """!Iterates over FGAT forecast hours relative to this model.

        Iterates over all FGAT forecast hours (as ints) relative to
        this model's analysis time.  For example, -3, 0, and 3 for
        three-hourly FGAT off of GDAS."""
        atime=self.atime
        for p,t in self._fgat_in_prod:
            yield int(round(to_fraction(t-atime,negok=True)/3600))

    def grab_more_inputs(self):
        """!Links to the current working directory gdas native spectral
        output files for all FGAT hours."""
        atime=self.parent_atime
        logger=self.log()
        for hr in self.parent_fhrs():
            ftime = atime + datetime.timedelta(hours=hr)
            there=self._in_catalog.locate(
                self._sigma_dataset,self._sigma_item,
                atime=atime,ftime=ftime,logger=logger)
            if not isnonempty(there):
                raise GSIInputError(
                    '%s %s: required input file is empty or non-existent: %s'
                    %(self._sigma_dataset,self._sigma_item,there))
            here='gfs_sigf%02d'%(hr,)
            make_symlink(there,here,force=True,logger=logger)

    def copy_wrf_inout(self,filename='wrf_inout',others='wrf_inou%d'):
        """!Copy WRF analysis or input files to this directory.

        Copies the WRF analysis or input file to the specified
        filename.  Also copies the wrf input files for other FGAT
        hours to separate files specified by "others".  The "others"
        must be a format that includes at least one integer argument.
        @param filename ignored
        @param others a string format with a %d in it, used to
          generate output filenames"""
        logger=self.log()
        atime=self.atime
        names=dict()
        fci=to_fraction(self.cycling_interval)
        for p,t in self._fgat_in_prod:
            assert(isinstance(p,produtil.datastore.Product))
            dt=int(round(float((to_fraction(t-atime,negok=True) + fci)/
                               3600.0)))
            name=others%dt
            logger.debug('add prod=%s time=%s'%(repr(p.did),repr(dt)))
            names[p]=name

        names[self._wrf_in_prod]='wrf_inout'

        for p,n in names.iteritems():
            logger.debug('prod %s has name %s'%(repr(p.did),repr(n)))

        def namer(p,logger,*args): return names[p]
        def actor(p,name,logger,*args): 
            deliver_file(p.location,name,logger=logger)
        produtil.datastore.wait_for_products([a for a in names.iterkeys() ],
                                             logger,namer,actor)


###############################################################################

def unset_gsistatus(conf,logger=None):
    """!Delete the gsi status file.  

    Deletes all GSI status files, whose paths are determined from the
    given config object.  If the logger is not specified, the
    gsistatus subdomain of the conf default logging domain is used.
    @param conf the hwrf.config.HWRFConfig object
    @param logger Optional: a logging.Logger for logging."""
    if logger is None: logger=conf.log('gsistatus')
    for onetwo in ( 'gsistatus', 'gsistatus2' ):
        gsistat=conf.get('dir',onetwo)
        gsistatfile=os.path.join(conf.getdir('com'),gsistat)
        produtil.fileop.remove_file(gsistatfile,info=True,logger=logger)

def set_gsistatus(conf,logger=None):
    """!Sets the GSI status files.

    Set run_gsi_d02=YES (true) or =NO (false) depending on the
    configuration.  If the logger is not specified, the gsistatus
    subdomain of the conf default logging domain is used.
    @param conf the hwrf.config.HWRFConfig object
    @param logger Optional: the logging.Logger for log messages."""
    if logger is None: logger=conf.log('set gsi status')

    tdrflagfile=conf.strinterp('dir','{com}/{stormlabel}.tdr')
    run_gsi=conf.getbool('config','run_gsi')
    conditional_gsid03=conf.getbool('config','conditional_gsid03',False)
    conditional_gsid02=conf.getbool('config','conditional_gsid02',False)

    for onetwo in ( 'gsistatus', 'gsistatus2' ):
        gsistat=conf.get('dir',onetwo)
        gsistatfile=os.path.join(conf.getdir('com'),gsistat)

        gsid02flag='YES' if run_gsi else 'NO'
        gsid03flag='YES' if run_gsi else 'NO'
        if gsid03flag=='YES' and conditional_gsid03 and \
                not isnonempty(tdrflagfile):
            logger.info('GSI is disabled for d03 because flag file is '
                        'empty or does not exist: %s'%(tdrflagfile,))
            gsid03flag='NO'
        if gsid02flag=='YES' and conditional_gsid02 and \
                not isnonempty(tdrflagfile):
            logger.info('GSI is disabled for d02 because flag file is '
                        'empty or does not exist: %s'%(tdrflagfile,))
            gsid02flag='NO'

        logger.info('Setting run_gsi_d02=%s in gsi status file %s'%(
            gsid02flag,gsistatfile))
        logger.info('Setting run_gsi_d03=%s in gsi status file %s'%(
            gsid03flag,gsistatfile))
        with open(gsistatfile,'wt') as f:
            f.write('run_gsi_d02=%s\nrun_gsi_d03=%s\n'%(
                    gsid02flag,gsid03flag))

def get_gsistatus(conf,domain,logger=None):
    """!Checks the gsi status for a specific domain.  

    Checks the first GSI status file, scanning for information about
    the specified domain.  If the file does not exist or cannot be
    opened or read, then False is returned.  Otherwise, the file is
    scanned for run_gsi_d02=YES/NO or run_gsi_d03=YES/NO (case
    insensitive).  The last of those run_gsi_d02/d03 lines is used:
    NO=return False, YES=return True.
    @param conf the hwrf.config.HWRFConfig object with configuration info
    @param domain either "gsi_d02" or "gsi_d03",  the domain of interest
    @param logger Optional: the logging.Logger for log messages"""
    if domain!='gsi_d02' and domain!='gsi_d03':
        raise ValueError('In get_gsistatus, domain must be gsi_d02 '
                         'or gsi_d03.')
    if logger is None: logger=conf.log('get gsi status')

    gsistat=conf.get('config','gsistatus')
    gsistatfile=os.path.join(conf.getdir('com'),gsistat)
    gsi_success=None

    logger.info('%s: scan gsi status file for run_%s=YES or NO'%(
        gsistatfile,domain))

    try:
        with open(gsistatfile,'rt') as f:
            for line in f:
                if line.find('run_%s=YES'%(domain))>=0:
                    gsi_success=True
                    logger.info(
                        'gsi status file says: run_%s=YES'%(domain))
                elif line.find('run_%s=NO'%(domain))>=0:
                    gsi_success=False
                    logger.warning(
                        'gsi status file says: run_%s=NO'%(domain))
    except EnvironmentError as e:
        logger.error(
            'Error checking gsi status file: %s'
            %(str(e),),exc_info=True)
    except Exception as ee:
        logger.error(
            'Unhandled exception while checking gsi status file: %s'
            %(str(ee),),exc_info=True)
        raise

    if gsi_success is None:
        logger.warning('Could not scan gsi status file for run_%s=YES'
            'or NO. Assuming run_%s=NO.'%(domain,domain))
        gsi_success=False
    elif gsi_success:
            logger.info(
            'run_%s=YES: gsi status file says gsi init succeeded.'%(domain))
    else:
        logger.warning(
            'run_%s=NO: gsi status file says gsi init failed or was aborted.'
            %(domain))

    return gsi_success

