"""!Post-processes GSI input and output for data assimilation diagnostics."""

##@var __all__
# The list of symbols to export via "from hwrf.gsipost import *"
__all__ = ["GSIPost",'F_ORG','F_GES','F_ANL','F_ALL']

import os, glob, shutil
import produtil.fileop, produtil.run, produtil.cd
import hwrf.gsi, hwrf.hwrftask, hwrf.post, hwrf.exceptions

from produtil.run import run, checkrun, alias, mpi, mpirun, exe, bigexe
from produtil.cd import NamedDir
from produtil.fileop import deliver_file, make_symlink, remove_file
from hwrf.post import EGRIB1Product, check_post, link_post_fix

##@var F_ORG
#  Flag to indiciate the original, downscaled parent model data is desired
F_ORG=1

##@var F_GES
# Flag to indicate the vortex-relocated data (first guess) is desired
F_GES=2

##@var F_ANL
# Flag to indiciate the final WRF input (after GSI and merge) is desired
F_ANL=4

##@var F_ALL
# Flag to indicate all initialization steps are desired
F_ALL=F_ORG|F_GES|F_ANL

class GSIPost(hwrf.hwrftask.HWRFTask):
    """!Post-processes GSI inputs and outputs.

    Runs the hwrf_post on GSI inputs and outputs from one or more of three stages.
    * original data (F_ORG) - the original, downscaled, parent model data
    * first guess (F_GES) - the original data, with the vortex relocated
    * analysis (F_ANL) - the final data input to the forecast
    This class is compatible with the hwrf.regrib and hwrf.gribtask,
    and can be used in place of an hwrf.post.PostManyWRF.
    """
    def __init__(self,dstore,conf,section,**kwargs):
        """!Constructor for GSIPost
        @param dstore the produtil.datastore.Datastore database to use
        @param conf the hwrf.config.HWRFConfig configuration information
        @param section the section to use in conf
        @param kwargs keyword arguments passed to the superclass constructor"""
        super(GSIPost,self).__init__(dstore,conf,section,**kwargs)
        self._domains=list()
        self._wrfout=dict()

        self._wrforg=dict()
        self._wrfges=dict()
        self._wrfanl=dict()
        self._org_products=dict()
        self._ges_products=dict()
        self._anl_products=dict()

    def add_case(self,domain,wrfout,org=None,ges=None,anl=None):
        """!Adds another case to the list of those to post-process

        @param domain a WRFDomain to be processed
        @param wrfout a wrfout file for internal use.  It will be copied
          and its fields will e replaced with the ges and/or analysis
          file as needed
        @param org Optional: original down-scaled parent domain data
        @param ges Optional: a first guess wrfinput or restart file, generally after
          vortex relocation
        @param anl Optional: an analysis (GSI output) wrfinput or restart file
        @note At least one of org, ges or anl must be given."""
        assert(not (org is None and ges is None and anl is None))
        assert(wrfout is not None)
        assert(domain is not None)

        self._domains.append(domain)
        #self._domsize[domain]=[int(mdeglat),int(mdeglon),int(mdegres)]
        self._wrfout[domain]=wrfout

        if org is not None:
            self._wrforg[domain]=org
            self._org_products[domain]=EGRIB1Product(
                self.dstore,category=self.taskname,
                prodname=domain.name+'_org_'+org.prodname)

        if ges is not None:
            self._wrfges[domain]=ges
            self._ges_products[domain]=EGRIB1Product(
                self.dstore,category=self.taskname,
                prodname=domain.name+'_ges_'+ges.prodname)

        if anl is not None:
            self._wrfanl[domain]=anl
            self._anl_products[domain]=EGRIB1Product(
                self.dstore,category=self.taskname,
                prodname=domain.name+'_anl_'+anl.prodname)

    def run(self):
        """!Executes the GSI post for all cases on all domains."""
        logger=self.log()
        good=True
        for domain in self._domains:
            logger.info('%s: process this domain'%(str(domain),))
            wrfout=self._wrfout[domain]
            if domain in self._org_products:
                logger.info('%s: process org'%(str(domain),))
                self._process(self._wrforg[domain],
                    domain,'org',wrfout,self._org_products[domain])
            else:
                logger.info('%s: has no org data'%(str(domain),))

            if domain in self._ges_products:
                logger.info('%s: process ges'%(str(domain),))
                self._process(self._wrfges[domain],
                    domain,'ges',wrfout,self._ges_products[domain])
            else:
                logger.info('%s: has no ges data'%(str(domain),))
                
            if domain in self._anl_products:
                logger.info('%s: process anl'%(str(domain),))
                self._process(self._wrfanl[domain],
                    domain,'anl',wrfout,self._anl_products[domain])
            else:
                logger.info('%s: has no anl data'%(str(domain),))
        logger.info('Done processing domains.')
        
    def _process(self,inprod,domain,why,wrfout,outprod):
        """!Internal function that implements most of run()

        Do not call directly.  This is the implementation of
        self.run: it runs the post for one case, on one domain."""
        assert(inprod is not None)
        assert(outprod is not None)
        assert(wrfout is not None)
        assert(domain is not None)
        logger=self.log()
        #assert(outprod.location) # should already be set in constructor
        assert(wrfout.location)
        wrfthere=wrfout.location
        assert(wrfthere)
        if not produtil.fileop.isnonempty(wrfthere):
            raise hwrf.exceptions.PostHasNoInput(
                '%s: is empty or nonexistent'%(wrfthere,))
        if not inprod.location:
            logger.info('%s: not available (location unknown)'%(
                    inprod.did,))
        elif not inprod.available:
            logger.info('%s (%s): not available according to database'%(
                    inprod.did, inprod.location))
        shortname=domain.name+'_'+why+'_'+inprod.prodname
        workdir=os.path.join(self.workdir,shortname)
        if os.path.exists(workdir):
            logger.info('%s: exists; will delete'%(workdir,))
            shutil.rmtree(workdir)
        with NamedDir(workdir,keep=not self.scrub,logger=logger):
            diffwrf=alias(bigexe(self.getexe('hwrf_3dvar')))
            post=alias(mpi(self.getexe('post')))

            # Copy the fields into the wrfout file:
            deliver_file(wrfthere,'postinput',keep=True)
            make_symlink(inprod.location,'ghost',force=True)
            cmd=diffwrf['storm_relocate','ghost','flnm3','new_ght']
            checkrun(cmd >= 'storm_relocate.log',logger=logger)
            cmd=diffwrf['3dvar_update','postinput','new_ght']
            checkrun(cmd >= '3dvar_update.log',logger=logger)

            # Delete any stray fort.* files:
            for filename in glob.glob('fort.*'):
                produtil.fileop.remove_file(filename,logger=logger)

            # Run the post:
            datestamp=self.conf.cycle.strftime('%Y-%m-%d_%H:%M:%S')
            with open('itag','wt') as itag:
                itag.write("""postinput
netcdf
%s
NMM NEST
""" % (datestamp,))
            cmd=mpirun(post,allranks=True) >= 'vpost.log'
            needcrtm=self.confbool('needcrtm',False)
            logger.info('link post fix files')
            link_post_fix(self.getdir('FIXhwrf'),needcrtm,logger=logger)
            fort14=self.confstr('control')
            logger.info('%s: use this control file for gsi post'%(fort14,))
            produtil.fileop.make_symlink(fort14,'fort.14',force=True,
                                         logger=logger)
            logger.info('Run post, log to vpost.log.')
            ret=run(cmd,logger=logger)

            # Check to see if the post succeeded and get the center
            # lat & lon of the domain for post-processing:
            (ok,cenla,cenlo,filename)=check_post(ret,shortname,logger)
            if not ok:
                raise hwrf.exceptions.PostFailed('GSI post on '+shortname)
            #toloc=outprod.location
            toloc=os.path.join(self.outdir,shortname)
            outprod.deliver(toloc,{'CENLA':cenla,'CENLO':cenlo,
                                   'fromloc':filename},logger=logger)
        return True

    def products(self,domains=None,domain=None,which_step=F_ALL,**kwargs):
        """!Iterates over EGRIB1Product objects produced by this task.
       
        @param domains a list of WRFDomain objects.  Only these will be iterated.
        @param domain a single WRFDomain; only its products will be iterated
        @param which_step which steps are of interest:
           * F_ORG - original parent model data
           * F_GES - first guess (vortex-relocated parent model data)
           * F_ANL - analysis, the final input to the forecast
           * any integer or of the above
           * F_ALL - all products, the default
        @param kwargs ignored"""
        if domains is None and domain is not None:
            domains=[domain]
        elif domains is None:
            domains=self._domains
        for d in domains:
            if which_step&F_ORG and d in self._org_products:
                yield self._org_products[d]
            if which_step&F_ANL and d in self._anl_products:
                yield self._anl_products[d]
            if which_step&F_GES and d in self._ges_products:
                yield self._ges_products[d]


