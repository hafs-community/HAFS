"""!Generates NHC-specific products and validates wrfdiag files.

This module implements NHCProducts, a subclass of HWRFTask that
runs the hwrf_nhc_products fortran program and delivers its outputs.
That program generates rainfall and wind swaths, various text files,
and other products."""

##@var __all__
# Symbols exported by "from hwrf.nhc_products import *" 
__all__=['NHCProducts']

import math, os, re, glob
import produtil.fileop, produtil.run, produtil.cd, produtil.datastore
import hwrf.hwrftask, hwrf.namelist, hwrf.constants, hwrf.storminfo, \
    hwrf.numerics

from math import pi,sqrt
from hwrf.numerics import to_fraction, split_fraction
from hwrf.constants import Rearth
from hwrf.namelist import to_fortnml
from produtil.fileop import make_symlink, deliver_file
from produtil.run import checkrun, exe, openmp, bigexe
from produtil.cd import TempDir
from produtil.datastore import COMPLETED, RUNNING, UpstreamFile, FileProduct

class NHCProducts(hwrf.hwrftask.HWRFTask):
    """!This is a wrapper around the hwrf_nhc_products Fortran program."""
    def __init__(self,dstore,conf,section,wrftask,track,domains,
                 vitals,stream='auxhist1',fcstlen=126,**kwargs):
        """!NHCProducts constructor.
        @param dstore the produtil.datastore.Datastore
        @param conf the HWRFConfig object
        @param section the config section to use
        @param wrftask the ExternalWRFTask, WRFAtmos or WRFCoupledPOM, or
            similar task, that can provide wrfdiag files and other WRF
            outputs.
        @param track the final tracker output Product
        @param domains the list of WRFDomains whose wrfdiag files should
            be processed
        @param vitals the tcvitals
        @param stream the WRF output stream for wrfdiag files
        @param fcstlen forecast length in hours
        @param kwargs Other keyword arguments are passed to the superclass constructor."""
        if 'outdir' not in kwargs: kwargs['outdir']=conf.getdir('com')
        super(NHCProducts,self).__init__(dstore,conf,section,**kwargs)
        self._wrftask=wrftask
        self._track=track # final track product
        assert(track is not None)
        self._domains=domains
        self._stream=stream
        self._vitals=vitals
        simend=wrftask.wrf().simend()
        self._wrfdiag=[ x for x in wrftask.products(stream=stream,
                                                    time=simend) ]
        self._products=self._make_products()
        self._name_for_ext=dict()
        self.__fcstlen=fcstlen
    def _make_products(self): 
        """!Generates FileProduct objects for all outputs."""
        pre=os.path.join(self.getdir('com'),
                         self.confstrinterp('{out_prefix}.').lower())
        PRE=os.path.join(self.getdir('com'),
                         self.confstrinterp('{out_prefix}.').upper())
        deliverme=dict( wind10m='wind10m.ascii', rainfall='rainfall.ascii',
                        wind10hrly='wind10hrly.ascii',swath='swath.grb2')
        DELIVERME=dict( stats='stats.tpc', htcf='hwrf_d03.htcf', afos='afos')
        with self.dstore.transaction() as t:
            prods=dict()
            for pname,psub in deliverme.iteritems():
                prod=FileProduct(
                    self.dstore,prodname=pname,category=self.taskname)
                prod.location=pre+psub
                prod['locbase']=os.path.basename(pre+psub)
                prods[pname]=prod
            for pname,psub in DELIVERME.iteritems():
                prod=FileProduct(
                    self.dstore,prodname=pname,category=self.taskname)
                prod.location=PRE+psub
                prod['locbase']=os.path.basename(PRE+psub)
                prods[pname]=prod
            # for name,prod in prods.iteritems():
            #     prod['minsize']=0
            #     prod['minage']=-120
            for domain in self._domains:
                diagname='wrfdiag_d%02d'%(domain.get_grid_id(),)
                prod = FileProduct(dstore=self.dstore, prodname=diagname, 
                                   category=self.taskname)
                prod.location = pre+diagname
                prod['locbase']=os.path.basename(pre+diagname)
                prods[diagname] = prod
        return prods
    def canrun(self,silent=True):
        """!Determines if the hwrf_nhc_products program can be run yet.
        @param silent If silent=True, does not log anything.
        @returns True if the program can run, False otherwise"""
        with self.dstore.transaction():
            # Check for track file:
            if not self._track.available:
                if not silent: 
                    self.log().info('Cannot run: track not yet available.  '
                                    'Location: %s available: %d'% (
                            str(self._track.location), 
                            int(self._track.available)))
                return False
            # Check for wrfdiag files.  Run the check() method on each
            # if it is not already available just in case it is an
            # upstream file.
            for w in self._wrfdiag:
                if not w.available:
                    w.check()
                    if not w.available:
                        if not silent: 
                            self.log().info('Cannot run: wrfdiag file %s '
                                            'not available'%(str(w),))
                        return False
        return True
    def get_res_cutoff(self,wrf,fudge_factor=0.8):
        """!Calculates resolution cutoff information.

        Calculates the outermost radius from the domain center at
        which the storm center can be, while still considered to be
        resolved by that domain.  This is used to detect failures of
        the nest motion, and report which nests actually contain the
        storm.  Iterates over radii for each nest, yielding a radius
        in km.
        @param wrf the hwrf.wrf.WRFSimulation being run
        @param fudge_factor fudge factor to reduce resolution to 
          politically correct values"""
        first=True
        for domain in wrf:
            if first:
                first=False
            else:
                sn=domain.nl.nl_get('domains','e_sn')
                assert(domain.dy is not None)
                assert(Rearth is not None)
                assert(sn is not None)
                assert(fudge_factor is not None)
                yield domain.dy*Rearth*pi/180.*sn*sqrt(2.)*fudge_factor
    def nesting_level(self,moad,nest):
        """!Determines the nesting level
        Determines the nesting level of the specified nest relative
        to the given moad.  If nest=moad, the result is 0, if nest is
        the direct child of moad, then 1, if it is the grandchild,
        then 2, and so on.
        @param moad the outermost domain
        @param nest the nest of interest
        @returns the nesting level, an integer"""
        level=0
        moadid=moad.get_grid_id()
        dom=nest
        while level<100 and dom.get_grid_id()!=moadid: # cap at 100
                                                       # just in case
            level+=1
            dom=dom.parent
        return level
    def write_namelist(self,f,wrf,moad,inner):
        """!Writes the products.nml namelist file.

        This is an internal implementation function; do not call it
        directly.  Writes the products.nml namelist to file object f.
        @param f the opened file object
        @param wrf the hwrf.wrf.WRFSimulation
        @param moad the outermost domain, an hwrf.wrf.WRFDomain
        @param inner the innermost domain, an hwrf.wrf.WRFDomain"""
        wrftask=self._wrftask
        g=moad.nl.nl_get
        basin1=self.confstr('basin1',section='config').upper()
        stnum=self.confint('stnum',section='config')
        assert(isinstance(wrftask.location,basestring))

        # Multistorm - jtf,st
        # This will work for both multistorm  and non-multistorm.
        # For basin scale we are now calling wrftask.products(...) instead
        # of hifreq=inner.hifreq_file().  This allows us to override the
        # ExternalWRFTask _as_products method in the ForecastWatcher class and
        # return the correct hifreq name and path.
        hifreqs=[x for x in wrftask.products(
            stream='hifreq',domains=[inner])]

        # hifreqs is a list of 22 of the same (not sure why) UpstreamFile objects.
        # [UpstreamFile(Datastore,'hifreq_d03.htcf','runwrf')]
        # The location has the correct path and name of the htcf file.
        # hifreqs[0].location '..../00L/runwrf/hifreq_d05.htcf'
        hifreq=os.path.basename(hifreqs[0].location)


        assert(isinstance(hifreq,basestring))

        wrftaskloc=wrftask.location

        # Construct a dict of replacement strings to substitute into
        # the namelist:
        dt=to_fraction(moad.dt)
        (dti,dtn,dtd)=split_fraction(dt) # integer part, numerator,
                                         #              denominator
        repl={ 'dx':g('domains','dx'),
               'dy':g('domains','dy'),
               'time_step':dti,
               'time_step_fract_num':dtn,
               'time_step_fract_den':dtd,
               'ide':g('domains','e_we'),
               'jde':g('domains','e_sn'),
               'YMDH':self.confint('YMDH',section='config'),
               'inhifreq':os.path.join(str(wrftaskloc),hifreq),
               'inatcf':self._track.location,
               'domlat':self.conffloat('domlat',section='config'),
               'domlon':self.conffloat('domlon',section='config'),
               'STORM':str(self.storminfo.stormname).upper(),
               'ATCFID':str(self.storminfo.stormid3).upper(),
               'TierI_model':to_fortnml(self.confstr('TierI_model','HWRF')),
               'TierI_submodel':to_fortnml(self.confstr('TierI_submodel',
                                                        'PARA')),
               'TierI_realtime':to_fortnml(self.confbool('TierI_realtime',
                                                         True)),
               'swathres':to_fortnml(self.conffloat('swathres',0.05)),
               'swathpad':to_fortnml(self.conffloat('swathpad',0.3)),
               'grads_byteswap':to_fortnml(self.confbool('grads_byteswap',
                                                         True)),
               'nestlev':to_fortnml(self.nesting_level(moad,inner)),
               'rescut':to_fortnml([float(x) for x in self.get_res_cutoff(
                        wrf)])
             }
        # Guess the forecast center from the basin:
        if(basin1=='L' or basin1=='E' or basin1=='C' or basin1=='Q'):
            repl['centername']='"NHC"'
        else:
            repl['centername']='"JTWC"'

        repl['fcst_len']=str(int(self.__fcstlen))

        # Now generate the actual namelist:
        f.write('''
&nhc_products
    intcvitals='tmpvit'
    inatcf='{inatcf}'
    inhifreq='{inhifreq}'
    inwrfdiag='wrfdiag_d<DOMAIN>'
    outpre='{STORM}{ATCFID}.{YMDH}.'
    mdstatus='MDstatus'
    resolution_cutoffs = {rescut}
    want_ymdh={YMDH}
    want_stid={ATCFID}
    want_centername={centername}
    coupler_dt=540.
    fcst_len={fcst_len}
    ide_moad={ide}
    jde_moad={jde}
    dlmd_moad={dx}
    dphd_moad={dy}
    clat={domlat}
    clon={domlon}
    nesting_level={nestlev}
    grads_byteswap={grads_byteswap}
    time_step={time_step}
    time_step_fract_num={time_step_fract_num}
    time_step_fract_den={time_step_fract_den}
    model={TierI_model}
    submodel={TierI_submodel}
    realtime={TierI_realtime}
    swath_latres=0.05
    swath_lonres=0.05
    swath_latpad=0.3
    swath_lonpad=0.3
    write_grib=T
/
'''.format(**repl))
    def product(self,name):
        """!Convenience function that returns the product with the
        given name, or raises KeyError if none is found."""
        return self._products[name]
    def wrfdiag_products(self,what=None):
        """!Iterates over wrfdiag file products
        @param what ignored"""
        for name,prod in self._products.iteritems():
            assert(isinstance(name,basestring))
            part=name[0:7]
            if part=='wrfdiag':
                yield prod
    def products(self,what=None):
        """!Returns Product objects describing files produced by this
        Task.
        @param what if specified, the name of the product of interest.
        Otherwise, all products are iterated over"""
        if what is not None:
            if what in self._products:
                yield self._products[what]
        else:
            for product in self._products.itervalues():
                yield product

    def run(self,deliver_wrfdiag=False):
        """!Copies inputs, runs the hwrf_nhc_input, and delivers results.
        @param deliver_wrfdiag if True, wrfdiag files are also delivered"""
        self.state=RUNNING
        wrf=self._wrftask.wrf()
        moad=wrf.get(self._domains[0])
        inner=wrf.get(self._domains[-1])
        logger=self.log()
        wd=self.workdir
        dir=os.path.dirname(wd)
        prefix=os.path.basename(wd)
        with TempDir(prefix=prefix,dir=dir,logger=logger):
            runme=self.getexe('hwrf_nhc_products')
            # Write the namelist:
            with open('products.nml','wt') as f:
                self.write_namelist(f,wrf,moad,inner)
            # Write the tcvitals:
            with open('tmpvit','wt') as f:
                if(isinstance(self._vitals,hwrf.storminfo.StormInfo)):
                    f.write("%s\n"%(self._vitals.as_tcvitals(),))
                else:
                    for vital in self._vitals:
                        f.write("%s\n"%(self._vitals.as_tcvitals(),))

            # Link all wrfdiag files:
            for domain in self._domains:
                # get the last wrfdiag time
                (start,end,interval)=domain.get_output_range(self._stream) 
                # get the wrfdiag file
                orig=[x for x in self._wrftask.products(
                        stream=self._stream,domains=[domain],time=end)] 
                # get the path of the last wrfdiag file in that list
                orig=orig[-1].location 
                # local filename needed by program
                here='wrfdiag_d%02d'%(domain.get_grid_id(),) 
                # make the symlink
                make_symlink(orig,here,force=True,logger=logger) 
                
            # Link to the coupling status file:
            make_symlink(os.path.join(self._wrftask.location,'MDstatus'),
                         'MDstatus')
            threads=self.confint(
                'threads',int(os.environ.get('NHC_PRODUCTS_NTHREADS','1')))
            cmd=openmp(exe(runme),threads=threads)
            cmd=cmd.env(OMP_STACKSIZE='128M')
            checkrun(cmd,logger=logger)
            self.deliver_outlist()
            if deliver_wrfdiag:
                self.deliver_wrfdiag()
            self.state=COMPLETED
    def rewrite_swath_ctl(self,ctlfile):
        """!Modifies the swath.ctl file to point to a lower-case
        swath.dat filename.
        @param ctlfile the file to modify"""
        newfile='%s.lowerdat'%(ctlfile,)
        with open(ctlfile,'rt') as fi:
            with open(newfile,'wt') as fo:
                for line in fi:
                    m=re.match('^(.*DSET +\^)(.*)$',line)
                    if m: line="%s%s\n"%(m.group(1),m.group(2).lower())
                    fo.write(line)
        return newfile
    def _deliver_it(self,fromloc,toloc,keep,logger):
        """!Helper function to deliver data.

        This is an internal implementation function used by
        deliver_outlist.  Do not call this directly.
        @param fromloc source file location
        @param toloc target file location
        @param keep if True, keep the origin file
        @param logger the logging.Logger to use"""
        assert(fromloc is not None)
        assert(toloc is not None)
        assert(isinstance(fromloc,basestring))
        assert(isinstance(toloc,basestring))
        assert(keep is not None)
        assert(logger is not None)
        keep=bool(keep)
        tobase=os.path.basename(toloc)
        for (k,p) in self._products.iteritems():
            locbase=p['locbase']
            if tobase==locbase:
                logger.info('%s is product %s (at %s), deliver that...'
                            %(tobase,p.did,locbase))
                p.deliver(frominfo=fromloc,keep=keep,logger=logger)
                return
            else:
                logger.info('%s is not product %s (at %s)'
                            %(tobase,p.did,locbase))
        logger.info('%s has no Product, deliver_file instead'%(toloc,))
        deliver_file(fromloc,toloc,keep=keep,logger=logger)
        
    def deliver_outlist(self):
        """!Reads the "outlist" output file from hwrf_nhc_products and
        delivers the listed files to the com directory."""
        logger=self.log()
        outfiles=list()
        od=self.outdir
        with open('outlist','rt') as outlist:
            for outfile in outlist:
                outfile=outfile.rstrip() # remove end-of-line character
                outfiles.append(outfile)
        for outfile in outfiles:
            bn=os.path.basename(outfile)
            if(re.search('\.swath.ctl',bn)):
                # Change swath.ctl file to lower-case, and change
                # the swath.dat filename inside to lower-case:
                newctl=self.rewrite_swath_ctl(outfile)
                with open(newctl,'rt') as f:
                    for line in f: 
                        line.rstrip()
                        logger.info('NEWCTL: '+repr(line))
                self._deliver_it(newctl,os.path.join(od,outfile.lower()),
                             keep=False,logger=logger)
            elif(re.search('\.(afos|stats.tpc|htcf|resolution|htcfstats)$',
                           bn)):
                # Deliver these twice: once in original (upper)
                # case, and once in lower-case:
                assert(outfile.find('swath')<0)
                logger.info('%s: deliver twice: as upper- and lower-case'
                            %(outfile,))
                self._deliver_it(outfile,os.path.join(od,outfile.lower()),
                             keep=True,logger=logger)
                self._deliver_it(outfile,os.path.join(od,outfile),
                             keep=False,logger=logger)
            elif(re.search('^a.*\.dat$',bn) and re.search('swath',bn)<0):
                # Deliver these files in original case
                assert(outfile.find('swath')<0)
                self._deliver_it(outfile,os.path.join(od,outfile),
                             keep=False,logger=logger)
                logger.info('%s: deliver as upper-case'%(outfile,))
            else:
                # Deliver remaining files in lower-case:
                logger.info('%s: deliver with original case'%(bn,))
                self._deliver_it(outfile,os.path.join(od,outfile.lower()),
                             keep=False,logger=logger)
            for (name,prod) in self._products.iteritems():
                loc=prod.location
                if loc and os.path.basename(loc)==os.path.basename(outfile):
                    prod.check(logger=logger) 
                    # Note: check instead of deliver because this
                    # is an UpstreamFile object.
        nprod=0
        for prod in self._products.iteritems():
            if isinstance(prod,produtil.datastore.UpstreamFile):
                prod.check(minage=-100)
                nprod+=1
                if not prod.available:
                    logger.warning('%s: not available at %s'%(
                            prod.did,prod.location))
                else:
                    logger.info('%s: available at %s'%(
                            prod.did,prod.location))
        logger.info('Checked %d UpstreamFile products'%nprod)
    def deliver_wrfdiag(self):
        """!Delivers wrfdiag files to their destination."""
        ncks=self.getexe('ncks','')
        logger=self.log()
        od=self.outdir
        if not ncks:
            ncks=produtil.fileop.find_exe('ncks',raise_missing=False)
        if ncks:
            def copy(src,tgt,junk):
                produtil.fileop.remove_file(tgt,logger=logger)
                checkrun(bigexe(ncks)['-4','-L','6',src,tgt]<'/dev/null',
                         logger=logger)
        else:
            copy=None
        # Copy wrfdiag files to com, converting to compressed NetCDF 4:
        for prod in self.wrfdiag_products():
            dest=os.path.join(od,self.confstrinterp(
                    '{out_prefix}.{prodname}',prodname=prod.prodname))
            src=os.path.join(self._wrftask.location,prod.prodname)
            logger.info("%s: deliver to %s"%(prod.did,prod.location))
            prod.deliver(frominfo=src,location=dest,copier=copy)
        # for filename in glob.glob('binary_d0?'):
        #     dest=self.confstrinterp('{com}/{out_prefix}.{prodname}',
        #                             prodname=filename)
        #     logger.info("%s: deliver to %s"%(filename,dest))
        #     produtil.fileop.deliver_file(filename,dest,keep=False,
        #                                  logger=logger)
