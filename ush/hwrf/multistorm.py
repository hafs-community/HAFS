import fractions,math,re,datetime,os
import produtil.fileop
import hwrf.wrf

from produtil.datastore import COMPLETED,RUNNING,UNSTARTED,FAILED,\
    Product, FileProduct, UpstreamFile
from produtil.fileop import isnonempty
from produtil.workpool import WorkPool

import hwrf.finalmergetask
from hwrf.hwrftask import HWRFTask
from hwrf.numerics import *
from hwrf.namelist import *
from hwrf.exceptions import *
from hwrf.wrfbase import *

__all__=['ForecastWatcher','FakeInit','RealInit']

class Input2FinalMerge(hwrf.finalmergetask.Merge2FinalMerge):
    """Links real_nmm wrfinput_d01 file or returns the FileProduct,
    regardless of the time and domain, in the current directory for
    a final merge."""
    def __init__(self,stormid,src,domain,atime): 
        super(Input2FinalMerge,self).__init__(stormid,src)
        self.domain=domain
        self.atime=atime
    def get_inputs(self,logger,just_check=False, stormid=None, **kwargs):
        if stormid is not None and stormid!=self.stormid:
            if logger is not None:
                logger.error(
                    'Wrong stormid requested: %s instead of %s, cannot link.'
                    %(stormid,self.stormid))
            return False
        if self.src is not None:
            tgt=self.tgtname
            p=self.src.wrfinput_at_time(self.atime,self.domain)
            if p:
                if not p.available:      p.check()
                return self.link_product(p,WRFInputMissing,logger,
                                         target=tgt,
                                         just_check=just_check)
            return False

# Used by the realstorms of a multistorm run to setup the paths to the fakestorm
# directories for the forecast products.
# So this classes as_product method overrides the ExternalWRFTask:as_product method
# and sets up the paths for the realstorm products.
#runwrf.mon::wrfdiag_d03|0|/pan2/projects/dtc-hurr/James.T.Frimel/pytmp/aa.hwrf_multistorm.int/2012071000/00L/runwrf/wrfdiag_d05|UpstreamFile

class ForecastWatcher(hwrf.wrf.ExternalWRFTask):
    def __init__(self,ds,conf,fakeid,section,wrf,taskname,**kwargs):
        workdir=conf.strinterp('dir','{realstormwork}/{taskname}',
                               realstorm=fakeid,taskname=taskname)

        self._d02map = None
        self._d03map = None
        with ds.transaction() as tx:
            super(ForecastWatcher,self).__init__(
                ds,conf,section,wrf,
                taskname=taskname,workdir=workdir,outdir=workdir)
            self.location=workdir
            self.workdir=workdir
            self.outdir=workdir

    @property
    def d02remap(self):
        """The outer domain value for a multistorm.."""
        return self._d02map

    @property
    def d03remap(self):
        """The inner domain value for a multitorm ."""
        return self._d03map

    def change_location(self):
        # Multistorm - jtf
        # Changes the one line **task**::runhwrf.mon |ExternalWRFTask in the dstore.
        # So when check runs in the post ... The correct location is used
        # for the WRF Simulation status rsl.out.0000, since it is running under
        # the fakestorm. 
        if self.ismultistorm and not self.isfakestorm:
            if 'location' in self:
                fakestormid=self.conf.getstr('config','fakestormid')
                this_stid=self.conf.getstr('config','STID')
                self['location']=self['location'].replace(this_stid,fakestormid)
                #kwargs['location']= kwargs['location'].replace(this_stid,fakestormid)

    def check_all_inputs(self):
        """!Placeholder to allow a ForecastWatcher to look like an
        hwrf.fcsttask.WRFAtmos to the scripts.exhwrf_check_init job.
        This allows the fake storms to run the init checking job."""
        return True


    def as_product(self,wrfout,relocate=False):
        """Converts a WRFOutput to a Product."""
        cached=self._get_cache(wrfout)
        if cached is not None: return cached
        rel=wrfout.path()
        #outdir=os.path.join(self['outdir'],rel)
        outdir=self['outdir']
        assert(outdir is not None)

        # TODO: Re-Assess, ASSUMING multistorm_sids order. <jtf>
        # Same concern sid processing order assumption also used in nhc_products.py
        #
        # Input to Forecast processing order determines the the mapping from
        # stormid to domain, 04E,05E,06E,00L ... d<01,02,03,04,05,06... names.
        # Go back and review this interconnect dependency to the storm order for forcast input.
        # Can I be assured that order is the multistorm_sids order ? FOr the logic here,
        # I'm assuming it is ....

        # Multistorm - jtf
        # Changes the runwrf.mon::wrfout_d... entries in database for each real storm.
        # Changes path location to 00L for all real storms in a multistorm run.
        # changes the FileProduct location to the correct domain for each real storm.
        # ie. for the 2nd storm in a multistorm d01 -> 00L/d01, d02 -> 00L/d04, d03 -> 00L/d05
        if self.ismultistorm and not self.isfakestorm:
            priority_stid= self.conf.getstr('config','multistorm_priority_sid')
            multistorm_sids=self.conf.getstr('config','multistorm_sids').split()
            this_stid=self.storminfo.stormid3
            this_stid_idx=multistorm_sids.index(this_stid)

            # All realstorms wrfout files are under 00L since that is where the forecast runs.
            fake_outdir=self.confstrinterp('{realstormwork}/runwrf',
              realstorm=self.conf.getstr('config','fakestormid'))
            wrfprod=os.path.basename(wrfout.path())

            # Now map the domain name - BIG ASSUMPTION. I don't like this.
            # Need to determine which storm we are on to determine dXXmap for the name.
            # Using multistorm ids order for this.
            # ie. [04E, 05E] for 05E this_stid_index=1, so d02map='d04'
            if this_stid != priority_stid and wrfout.domain().get_grid_id() != 1:
                if wrfout.domain().get_grid_id() == 2:
                    self._d02map='d%02d'%(this_stid_idx*2+2)
                    assert('d02' in wrfprod)
                    wrfprod=wrfprod.replace('d02',self._d02map)
                if wrfout.domain().get_grid_id() == 3:
                    self._d03map='d%02d'%(this_stid_idx*2+3)
                    assert('d03' in wrfprod)
                    wrfprod=wrfprod.replace('d03',self._d03map)
            loc=os.path.join(fake_outdir,wrfprod)
        else:
            loc=os.path.join(outdir,os.path.basename(wrfout.path()))

        with self.dstore.transaction() as t:
            uf=UpstreamFile(self.dstore,category=self.taskname,
                            prodname=rel,location=loc)
            uf['minage']=75
            uf['stream']=wrfout.stream()
            uf['location']=loc
        if relocate:    uf.location=loc
        return self._set_cache(wrfout,uf)

# fcsttask.py These methods override methods in WRFTaskBase.
class RealInit(hwrf.fcsttask.WRFTaskBase):
    def run_exe(self,*args,**kwargs): pass
    def link_fix(self,*args,**kwargs): pass
    def make_namelist(self,*args,**kwargs): pass

    def link_all_inputs(self,just_check=False):
        okay=True
        if 'merge' in self.inputs:
            okay=okay and self.link_input(
                'merge',stormid=self.storminfo.stormid3,
                just_check=just_check)
        else:
            msg='Input type "merge" was not specified.'
            self.log().critical(msg)
            raise WRFInputMissing(msg)
        okay=okay and super(RealInit,self).link_all_inputs(just_check)
        return okay
    
    def add_orig_wrfinput(self,r):
        super(RealInit,self).add_wrfinput(r)

    def add_wrfinput(self,r):
        """Adds an input source (via self.add_input) that will provide
        the wrfinput output file from gdas_merge, or whatever the last
        step in the real storm initialization may have been.  The
        given object must have a get_merge(stormid) function that
        returns a Product for a given storm"""
        return self.add_input('merge',Input2FinalMerge(
                self.storminfo.stormid3,r,self.sim.get_moad(),self.conf.cycle))

    # The multistorm_input task calls this to deliver these 
    # products from  a storms intercom directory to its com directory,
    # for use and input to the finalmerge task 
    def deliver_products(self):
        c='_' if self.sim.nocolons else ':'
        #jtfst 00.nc not {SS).nc and removed MM in atime
        atime='{year}-{month}-{day}_{HH}'+c+'00'+c+'00'
        met_nmm_time='{year}-{month}-{day}_{HH}'+c+'00'+c+'00.nc'
        prefix='{com}/{vit[stormid3]}.{YMDH}.multistorm.'
        stormid=self.storminfo.stormid3
        files=[ 'geo_nmm.d01.nc',
                'geo_nmm_nest.l01.nc',
                'geo_nmm_nest.l02.nc',
                'met_nmm.d01.'+met_nmm_time,
                'wrfinput_d01',
                'wrfbdy_d01',
                'fort.65',
                'wrfanl_d02_'+atime,
                'wrfanl_d03_'+atime,
                'wrfinput_d01_'+stormid+'gdas_merge.nc']
        with WorkPool(4) as workers:
            for f in files:
                comfile=self.icstr(prefix+f)
                localfile=self.icstr(f)
                if not produtil.fileop.isnonempty(localfile):
                    raise hwrf.exceptions.ForecastInputError(
                        '%s: does not exist or is empty, cannot copy to %s'%(
                            localfile,comfile))
                workers.add_work(self._copy_one,[localfile,comfile])
            workers.barrier()
    def _copy_one(self,fromf,tof):
        produtil.fileop.deliver_file(fromf,tof,moveok=False,
                                     logger=self.log())

class FakeInit(hwrf.hwrftask.HWRFTask):
    def __init__(self,ds,conf,section,wrf,realstorm,stormNinner,stormNouter,**kwargs):
        super(FakeInit,self).__init__(ds,conf,section,**kwargs)
        self.realstorm=realstorm
        self.colon='_' if wrf.get_nocolons() else ':'
        (self.stormNinner,self.stormNouter) = (stormNinner,stormNouter)
        self.__wrf=wrf
        with ds.transaction() as tx:
            self.define_locations_from_com()

    @property
    def sim(self):
        """!Returns the hwrf.wrf.WRFSimulation being run."""
        return self.__wrf

    # This defines the filenames in the storms com diredtory where
    # the final_merge task and the wrf forecast gets its inputs from.
    def define_locations_from_com(self):
        c='_' if self.sim.nocolons else ':'
        atime='{year}-{month}-{day}_{HH}'+c+'00'+c+'00'
        met_nmm_time='{year}-{month}-{day}_{HH}'+c+'00'+c+'00.nc'
        self.geofile={1:self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.geo_nmm.d01.nc',
                                        'geo_nmm.d01.nc'),
                      3:self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.geo_nmm_nest.l01.nc',
                                        'geo_nmm_nest.l01.nc'),
                      9:self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.geo_nmm_nest.l02.nc',
                                        'geo_nmm_nest.l02.nc') }
        #jtfst 00.nc not {SS}
        self.metfile=self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.met_nmm.d01.'+met_nmm_time,
                                     'met_nmm.d01.'+met_nmm_time)
        self.real_init=self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.wrfinput_d01')
        self.fort65=self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.fort.65')
        self.wrfbdy=self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.wrfbdy_d01')
        self.wrfinput_final=self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.wrfinput_d01')
        self.wrfanl_for=dict()
        self.merge_for=dict()
        multistorm_sids = self.conf.getstr(
                      'config','multistorm_sids','nosids').split()
        for i1 in xrange(len(multistorm_sids)):
            i=i1+1
            stormid=multistorm_sids[i1]
            self.wrfanl_for[self.stormNinner['storm%sinner'%i]]=\
                self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.wrfanl_d03_'+atime,
                                'storm%sinner_wrfanl'%i,stormid=stormid)
            self.wrfanl_for[self.stormNouter['storm%souter'%i]]=\
                self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.wrfanl_d02_'+atime,
                                'storm%souter_wrfanl'%i,stormid=stormid)
            self.merge_for[stormid]=\
                self.productify('{realstormcom}/{realstorm}.{YMDH}.multistorm.wrfinput_d01_'+stormid+'gdas_merge.nc',
                                'merge_%s'%(stormid,),stormid=stormid)

    def productify(self,filename,prodname=None,stormid=None):
        if stormid is None: stormid=self.realstorm
        loc=self.confstrinterp(filename,realstorm=stormid)
        basename=os.path.basename(loc)
        if prodname is None:
            prodname=basename
        else:
            prodname=self.confstrinterp(prodname)
            basename=prodname
        uf=UpstreamFile(self.dstore,prodname,self.taskname,location=loc)
        uf.location=loc
        uf['basename']=basename
        return uf

    # ie. returns com/../04E.2012071000.multistorm.wrfinput_d01_04Egdas_merge.nc
    def get_merge(self,stormid):
        if stormid in self.merge_for:
            return self.merge_for[stormid]
        return None

    # ie. returns  com/../04E.2012071000.multistorm.wrfinput_d01  
    # which is the gfsinit/realinit wrfinput_d01 file.
    def get_wrfinput_gfsinit_realinit(self):
        """Returns the wrfinput file regardless of the time or
        domain"""
        return self.wrfinput_final

    def wrfanl_at_time(self,atime,domain):
        if domain in self.wrfanl_for:
            return self.wrfanl_for[domain]
        return None

    def wrfinput_at_time(self,atime,domain):
        """Makes add_wrfinput work with this class."""
        return self.real_init

    def fort65_at_time(self,atime,domain):
        """Makes add_fort65 work with this class."""
        return self.fort65

    def wrfbdy_at_time(self,atime,domain):
        """Makes add_wrfbdy work with this class."""
        return self.wrfbdy

    def met_at_time(self,ftime):
        """Makes add_metgrid work with this class."""
        assert(self.metfile is not None)
        if ftime==self.conf.cycle:
            return self.metfile
        return None

    def geodat(self,domain,logger=None):
        """!Makes add_geogrid work with this class.
        @param domain the domain of interest
        @bug this only works for nesting ratios that are a power of 3"""
        ratio=domain.moad_ratio()

        if logger is None: logger=self.log()
        try:
            return self.geofile[ratio]
        except KeyError as ke:
            logger.error('BLAH BLAH Requesting domain %s at nest level %d which we do not have from geogrid: %s: %s'%
                          (str(domain),ratio,str(self.geofile),str(ke)),exc_info=True)
            return None


# This class was created in order to override make_namelist in the
# WRFTaskBase class since multistorm  calls swcorner_dynamic_multistorm
# And the multistorm fcsttask needs the realinit input.
class WRFAtmosMultiStorm(hwrf.fcsttask.WRFAtmos):

    def __init__(self,dstore,conf,section,wrf,keeprun=True,
                 wrfdiag_stream='auxhist1',**kwargs):

        super(WRFAtmosMultiStorm,self).__init__(dstore,conf,section,wrf,
                                                keeprun=keeprun,wrfdiag_stream=wrfdiag_stream,**kwargs)

    def make_namelist(self,filename='namelist.input',logger=None):
        """Runs set_ij_start (swcorner_dynamic) to generate the i & j
        start locations for domain 2, then generates the namelist.

        For the fakestorm of multistorm run, it generates the
        i & j start locations for each of the outer storms domain
        then generates the namelist."""
        if logger is None: logger=self.log()
        domlat=self.conf.getfloat('config','domlat')
        domlon=self.conf.getfloat('config','domlon')
        if self.isfakestorm:
            s=self.wrf().swcorner_dynamic_multistorm(
                self.getexe('swcorner_dynamic'),self.conf.syndat_multistorm,
                domlat,domlon,logger)
        else:
            s=self.wrf().swcorner_dynamic(self.getexe('swcorner_dynamic'),
                                      self.storminfo, domlat,domlon,logger)

        # Note: self.storminfo is self.conf.syndat, The multistorm info
        with open(filename,'wt') as nlin:
            nlin.write(s)

class WRFCopyTaskMS(hwrf.copywrf.WRFCopyTask):
    """This is a Task that copies WRF input and output files from the
    WRF FAKE Storm run directory to the COM directory,for a Multi Storm Basin Scale."""
    def __init__(self,dstore,conf,section,wrftask,out_prefix,**kwargs):

        super(WRFCopyTaskMS,self).__init__(
            dstore,conf,'copywrf',wrftask,conf.getstr('config','out_prefix'))

        self.__wrftask=wrftask

    # This is the ForecastWatcher object of a multistorm.
    @property
    def wrf_watcher(self):
        """!Returns the hwrf.wrf.WRFSimulation being run."""
        return self.__wrftask

    def comfile(self,orig,destname=None):
        """Generates a full path to the delivery location of the
        specified source file.  Returns the full path and the basename
        in a tuple."""

        # Multistorm - jtf
        # This function.
        # Changes **task**::copywrf task, only the *wrfout* FileProduct entries are changed.
        # Changes id and location.
        # For multistorm, make sure WRFCopyTaskMS Always uses the UpstreamFile
        # database prodname of (orig) instead of (orig.location) This ensures correct
        # name mapping since multistorm,real storm product locations in runwrf.mon are mapped
        # back to 00L/d<1,2,3,4,5,6,7 ...> and we want the copywrf productname
        # and location to always be referencing d01,d02,d03, for any of the storms
        # in a multistorm ... else they would be d01,d04,d05; d01,d06,d07 ...
        # Using the prodname is extremely convenient and necessary since we have
        # no reference to which dXX of the storm it is unless we assume
        # order of processing the storms.
        # ie. Here is an example database entry, This allows this d04 file under 00L to be
        #     copied and written as the desired d02 file under 05E.
        # id | available | location | type
        # runwrf.mon::wrfout_d02_2012-07-10_00:00:00|1|/pytmp/path/2012071000/00L/runwrf/wrfout_d04_2012-07-10_00:00:00|UpstreamFile
        # copywrf::05e.wrfout_d02_2012-07-10_00:00:00|1|/pytmp/path/com/2012071000/05E/05e.wrfout_d02_2012-07-10_00:00:00|FileProduct

        if(isinstance(orig,Product)):
            thisprodname=orig.getprodname()
            bn=os.path.basename(str(thisprodname))
        else:
            bn=os.path.basename(str(orig))

        # The bn_colon is the same as bn, but uses : to separate time
        # components, - to separate date components and an _ between
        # the date and time.  This matches the syntax expected by most
        # programs external to this workflow.
        bn_colon=re.sub(
            '([0-9][0-9][0-9][0-9])[_.-]([0-9][0-9])[_.-]([0-9][0-9])'
            '[_.-]([0-9][0-9])[_.-]([0-9][0-9])[_.-]([0-9][0-9])[^/]*$',
            r'\1-\2-\3_\4:\5:\6',bn)
        bn_colon_s00=re.sub(
            '([0-9][0-9][0-9][0-9])[_.-]([0-9][0-9])[_.-]([0-9][0-9])'
            '[_.-]([0-9][0-9])[_.-]([0-9][0-9])[_.-]([0-9][0-9])[^/]*$',
            r'\1-\2-\3_\4:\5:00',bn)

        if destname is None:
            fullbn='%s.%s'%(self.out_prefix,bn_colon)
        else:
            fullbn=self.confstrinterp(destname,inname=bn,
                                      inname_colon=bn_colon,
                                      inname_colon_s00=bn_colon_s00)
        return ( os.path.join(self.outdir,fullbn), fullbn )

    def _deliver_to_group(self,group,inprod,check=None,destname=None):
        """Do not call this function directly.  It is the internal
        implementation of d_initial and d_final.  Call those functions
        instead."""
        (comfile,combn)=self.comfile(inprod,destname=destname)
        if(isinstance(inprod,Product)):
            upstream=inprod
            if check is None: check=False
        else:
            # Multistorm - jtf
            # Setting the wrffile path for multistorm WRFCopyTask, copywrf-upstream::
            # Not sure why wrffile is using self._wrftask.location (non multistorm)
            # since that is empty ''. Look at the WRFCopyTask, copywrf-upstream lines in
            # the datastore AFTER the launcher task runs. Only the filenames are present.
            # AFTER WRFCopyTask is run ... the paths are updated. The only reason the task
            # has been working is because the UpstreamFile exists in the SID/runwrf workdir.
            # That is not the case for multistorm, since these UptreamFile's only exist in
            # the fakestorm 00L/runwrf dir.  Hence this change.
            # I think for non multistorm (ie. the outer else:) self._wrftask.workdir
            # should be used instead.  It has been working so I  will leave it alone.
            if self.ismultistorm:
                if self.isfakestorm:
                    wrffile=os.path.join(self._wrftask.workdir,inprod)
                else:
                    #fakestormid=self.conf.getstr('config','fakestormid')
                    #this_stid=self.storminfo.stormid3

                    fake_outdir=self.confstrinterp('{realstormwork}/runwrf',
                                                   realstorm=self.conf.getstr('config','fakestormid'))

                    #Note: self.wrf_watcher.d03remap does not exist, if this isfakestorm=True
                    if inprod == 'track_d03.patcf' and self.wrf_watcher.d03remap:
                        inprod_d03remap= inprod.replace('d03',self.wrf_watcher.d03remap)
                        wrffile=os.path.join(fake_outdir,inprod_d03remap)
                    else:
                        #wrffile=os.path.join(self._wrftask.workdir.replace(this_stid,fakestormid),inprod)
                        wrffile=os.path.join(fake_outdir,inprod)

            else:
                wrffile=os.path.join(self._wrftask.location,inprod)

            #This will return the domain map using wrfdiag_d03
            #with self.dstatore.transaction() at tx:
            #    self.dstore.transaction().query(
            # "select location from products where id='runwrf::wrfdiag_d03'")[0][0][-3]
            #Using ForecastWatcher above .. instead


            # Make an internal UpstreamFile to check for the file:
            upstream=UpstreamFile(dstore=self.dstore,prodname=combn,
                                  category="%s-upstream"%(self.taskname,),location=wrffile)
            upstream.location=wrffile
                # Nobody else can see this Product so we must check it:
            check=True

        product=FileProduct(
            dstore=self.dstore,prodname=combn,category=self.taskname,
            location=comfile)
        group.append( (upstream,product,bool(check)) )
        return self
