"""This module handles WW3 related scripts for HAFS system."""
# Updates Biju Thomas on 07/30/2022
#     Added cfp option for WCOSS2

__all__ = ['WW3Init', 'WW3Post']

import os, re
import produtil.datastore, produtil.fileop, produtil.cd, produtil.run, produtil.log
import produtil.cluster
import tcutil.numerics
import hafs.hafstask, hafs.exceptions
import hafs.namelist, hafs.input

from produtil.datastore import FileProduct, RUNNING, COMPLETED, FAILED, UpstreamFile
from produtil.fileop import make_symlink, deliver_file, wait_for_files
from produtil.cd import NamedDir, TempDir
from produtil.run import mpi, mpirun, run, runstr, checkrun, exe, bigexe, alias
from tcutil.numerics import to_datetime, to_datetime_rel, to_fraction, to_timedelta
from hafs.exceptions import WW3InputError

prodnames={ 
    'mod_def':       ( './mod_def.ww3', '{com}/{out_prefix}.mod_def.ww3' ),
    'wind':          ( './wind.ww3', '{com}/{out_prefix}.wind.ww3' ),
    'current':       ( './current.ww3', '{com}/{out_prefix}.current.ww3' ),
    'restart':       ( './restart.ww3', '{com}/{out_prefix}.restart_init.ww3' ),
    'ww3_shel':      ( './ww3_shel.inp', '{com}/{out_prefix}.ww3_shel.inp' ) }

########################################################################
class WW3Init(hafs.hafstask.HAFSTask): 
    def __init__(self,dstore,conf,section,taskname=None,fcstlen=126,
                 outstep=21600, pntstep=21600, rststep=21600, **kwargs):
        """Creates a WW3Init
          dstore - the produtil.datastore.Datastore to use
          conf - the HAFSConfig to use
          section - the section name for this task
          taskname - the task name.  Default: section
          fcstlen - the forecast length in hours
          outstep - the output step in seconds
          pntstep - the pntout step in seconds
          rststep - the restart output step in seconds
        Other keyword arguments are passed to the superclass constructor."""
        super(WW3Init,self).__init__(dstore,conf,section,taskname=taskname,**kwargs)
        self._make_products()
        self.fcstlen=float(fcstlen)
        self.outstep=int(outstep)
        self.pntstep=int(pntstep)
        self.rststep=int(rststep)
    def _make_products(self):
        """Creates FileProduct objects for all output files.  The
        outdir is the directory to which the WW3 package output its
        final files."""
        self._products=dict()
        atime=tcutil.numerics.to_datetime(self.conf.cycle)
        ww3_bdy=self.confstr('ww3_bdy','no')
        if ww3_bdy == 'yes': 
            prodnames['nest']=( './nest.ww3', '{com}/{out_prefix}.nest.ww3' )
        with self.dstore.transaction():
            for prodname,filepaths in prodnames.items():
                (localpath,compath)=filepaths
                prod=produtil.datastore.FileProduct(
                    self.dstore,prodname,self.taskname)
                prod.location=self.timestr(compath,atime,atime)
                prod['localpath'] = localpath
                self._products[prodname]=( prod,localpath )

    def products(self,name=None,**kwargs):
        """Iterate over all products."""
        for prodname,stuff in self._products.items():
            (prod,localpath)=stuff
            if name is None or name==prod.prodname:
                yield prod

    def inputiter(self):
        atime=to_datetime(self.conf.cycle)
        etime=to_datetime_rel(3600*self.fcstlen,atime)
        interval=to_fraction(self.confint('input_step',6*3600))

        dataset=self.confstr('gfs_dataset','gfs')
        item=self.confstr('gfs_item','gfs')

        epsilon=to_timedelta(interval/10)
        ende=to_datetime_rel(epsilon,etime)
        when=atime
        while when<ende:
            yield dict(self.taskvars,dataset=dataset,item=item,ftime=when,atime=atime)
            when=to_datetime_rel(interval,when)
        ww3_bdy=self.confstr('ww3_bdy','no')
        if ww3_bdy == 'yes': 
            atime=to_datetime(self.conf.cycle)
            wtime=to_datetime_rel(-6*3600,atime)
            dataset=self.confstr('gfswave_dataset','gfswave')
            item=self.confstr('ww3bdy_item','ww3bdy_ibp')
            when=wtime
            yield dict(self.taskvars,dataset=dataset,item=item,ftime=when,atime=when,optional=True)
        ww3_rst=self.confstr('ww3_rst','no')
        print('ww3_rst=%s'%(ww3_rst))
        if ww3_rst == 'yes' or ww3_rst == 'always': 
            atime=to_datetime(self.conf.cycle)
            wtime=to_datetime_rel(-6*3600,atime)
            dataset=self.confstr('gdaswave_dataset','gdaswave')
            item=self.confstr('ww3rst_item','ww3rst_gnh_10m')
            when=wtime
            yield dict(self.taskvars,dataset=dataset,item=item,ftime=when,atime=when,optional=True)

    def gfsgrib2iter(self):
        logger=self.log()
        atime=to_datetime(self.conf.cycle) # sim start time
        etime=to_datetime_rel(self.fcstlen*3600,atime) # sim end time
        interval=to_fraction(self.confint('input_step',6*3600))
        dataset=self.confstr('gfs_dataset','gfs')
        item=self.confstr('gfs_item','gfs')
        hd=self.confstr('catalog','hafsdata')
        dc=hafs.input.DataCatalog(self.conf,hd,atime)
        epsilon=to_timedelta(interval/10)
        ende=to_datetime_rel(epsilon,etime)
        when=atime
        fhour=0
        maxwait=self.confint('max_grib_wait',3600)
        sleeptime=self.confint('grib_sleep_time',20)
        min_size=self.confint('min_grib_size',1)
        min_mtime_age=self.confint('min_grib_age',30)
        while when<ende:
            thefile=dc.locate(dataset=dataset,item=item,ftime=when,atime=atime,**self.taskvars)
            if self.realtime:
                waited=wait_for_files(
                    [thefile],logger,maxwait=maxwait,sleeptime=sleeptime,
                    min_size=min_size,min_mtime_age=min_mtime_age)
                if not waited:
                    msg='%s: did not exist or was too small after %d seconds'%(
                        thefile,min_size)
                    self.log().error(msg)
                    raise hafs.exceptions.WW3InputError(msg)
            yield thefile
            fhour=fhour+interval/3600
            when=to_datetime_rel(interval,when)

    def deliver_products(self):
        logger=self.log()
        for prodname,stuff in self._products.items():
            (prod,localpath)=stuff
            prod.deliver(frominfo=localpath,keep=False,logger=logger)

    def run(self):
        """Runs the WW3 initialization"""
        logger=self.log()
        dummycurr=True
        usegfswind=self.confstr('usegfswind','yes')
        if usegfswind == 'yes':
            dummywind=False
        elif usegfswind == 'no':
            dummywind=True
        else:
            # Wrong usegfswind value
            logger.warning('Wrong usegfswind value: %s. Assume usegfswind=yes.' 
                           'Set dummywind to False.'%(usegfswind,))
            usegfswind='yes'
            dummywind=False
        ww3_bdy=self.confstr('ww3_bdy','no')
        ww3_rst=self.confstr('ww3_rst','no')
        try:
            self.state=RUNNING
            redirect=self.confbool('redirect',True)
            with NamedDir(self.workdir,keep=not self.scrub,logger=logger,rm_first=True) as d:
                # Run ww3_grid
                def link(s,t):
                    make_symlink(s,t,force=True,logger=logger)
                deliver_file(self.icstr('{grid_inp}'),'ww3_grid.inp',keep=True,logger=logger)
                link(self.icstr('{grid_bot}'),'.')
                if ww3_bdy == 'yes': 
                    link(self.icstr('{grid_msk2}'),self.icstr('./ww3_grid_{vit[basin1lc]}.msk'))
                else:
                    link(self.icstr('{grid_msk}'),'.')
                link(self.icstr('{grid_obr}'),'.')
                link(self.getexe('ww3_grid'),'ww3_grid')
                #checkrun(exe(self.getexe('ww3_grid'))>='ww3_grid.log',logger=logger)
                cmd=exe('./ww3_grid')
                if redirect: cmd = cmd>='ww3_grid.log'
                checkrun(cmd,logger=logger)

                if usegfswind == 'yes': 
                    # Extract gfs wind from gfs grib2 data 
                    ncfile='gfs.uvgrd10m.nc' 
                    produtil.fileop.remove_file(ncfile,logger=logger)
                    cmd=alias(bigexe(self.getexe('wgrib2','wgrib2')))
                    for f in self.gfsgrib2iter(): 
                        logger.info('Extracting wind at 10 m from %s'%(f))
                        subset=''
                        for line in runstr(cmd[f],logger=logger).splitlines(True):
                            if re.search(':[UV]GRD:10 m above ground:',line):
                                subset+=line
                        runme=cmd[f,'-i', '-append', '-netcdf', ncfile] << subset
                        checkrun(runme, logger=logger)

                    if produtil.fileop.isnonempty(ncfile):
                        dummywind=False
                    else:
                        dummywind=True
                        produtil.log.jlogger.warning(
                            'ww3init: will use dummy wind because %s is missing '
                            'or empty.'%(ncfile,))
                        
                if dummywind:
                    # Run ww3_prep for dummy wind
                    deliver_file(self.icstr('{wind_inp}'),'ww3_prep.inp',keep=True,logger=logger)
                    link(self.getexe('ww3_prep'),'ww3_prep')
                    #checkrun(exe(self.getexe('ww3_prep'))>='ww3_prep_wind.log',logger=logger)
                    cmd=exe('./ww3_prep')
                    if redirect: cmd = cmd>='ww3_prep_wind.log'
                    checkrun(cmd,logger=logger)
                else:
                    # Run ww3_prnc for prep gfs wind
                    deliver_file(self.icstr('{prnc_inp_gfswind}'),'ww3_prnc.inp',keep=True,logger=logger)
                    link(self.getexe('ww3_prnc'),'ww3_prnc')
                    cmd=exe('./ww3_prnc')
                    if redirect: cmd = cmd>='ww3_prnc_wind.log'
                    checkrun(cmd,logger=logger)

                if dummycurr:
                    # Run ww3_prep for dummy current
                    deliver_file(self.icstr('{curr_inp}'),'ww3_prep.inp',keep=True,logger=logger)
                    # Prepare the curdummy.dat
                    with open('./curdummy.dat','w') as of:
                        for x in range(6):
                           of.write('0. 0. 0.\n')
                    link(self.getexe('ww3_prep'),'ww3_prep')
                    #checkrun(exe(self.getexe('ww3_prep'))>='ww3_prep_curr.log')
                    cmd=exe('./ww3_prep')
                    if redirect: cmd = cmd>='ww3_prep_curr.log'
                    checkrun(cmd,logger=logger)
                else:
                    # Extract current from global ocean model
                    logger.error('Not implemented yet')

                have_restart=False
                oldrst='(unknown)'
                try:
                    oldrst=self.icstr('{oldcom}/{oldvit[stormid3lc]}.restart.f006.ww3')
                    if produtil.fileop.isnonempty(oldrst):
                        produtil.fileop.deliver_file(oldrst,'restart.ww3',logger=logger)
                        have_restart=True
                        produtil.log.jlogger.info('%s: warm start for wave'%(oldrst,))
                    else:
                        produtil.log.jlogger.warning(
                            'restart.ww3: will generate restart.ww3 because %s is missing '
                            'or empty.'%(oldrst,))
                except Exception as ee:
                    produtil.log.jlogger.warning(
                        'restart.ww3: will generate restart.ww3 because %s is missing '
                        'or could not be copied; %s'%(oldrst,str(ee)),exc_info=True)

                if (not have_restart and ww3_rst == 'yes') or ww3_rst == 'always': 
                    try:
                        with NamedDir('ww3gint',keep=True,logger=logger) as nameddir:
                            logger.info('ww3_grid: generating mod_def.ww3 for gnh_10m gridi from gdaswave')
                            make_symlink('../mod_def.ww3','mod_def.hafs_ww3',force=True,logger=logger)
                            make_symlink(self.getexe('ww3_grid'),'ww3_grid',force=True,logger=logger)
                            deliver_file(self.icstr('{grid_gnh_10m_inp}'),'ww3_grid.inp',keep=True,logger=logger)
                            cmd=exe('./ww3_grid')
                            if redirect: cmd = cmd>='ww3_grid.log'
                            checkrun(cmd,logger=logger)
                            deliver_file('./mod_def.ww3','./mod_def.gnh_10m',keep=False,logger=logger)
                            logger.info('ww3_gint: generating restart.ww3 by using ww3_gint with restart files from gdaswave')
                            make_symlink(self.getexe('ww3_gint'),'ww3_gint',force=True,logger=logger)
                            #Get restart.gnh_10m
                            self.get_ww3rst_inputs()
                            #Prepare the namelist
                            self.make_gint_inp(logger)
                            #run ww3_gint
                            cmd=exe('./ww3_gint')
                            if redirect: cmd = cmd>='ww3_gint.log'
                            checkrun(cmd,logger=logger)
                            deliver_file('./restart.hafs_ww3','../restart.ww3',keep=False,logger=logger)
                        if produtil.fileop.isnonempty('restart.ww3'):
                            have_restart=True
                    except Exception as ee:
                        produtil.log.jlogger.warning(
                            'restart.ww3: will generate dummy because ww3_gint '
                            'did not run successfully.',exc_info=True)

                if not have_restart:
                    logger.info('restart.ww3: generating dummy with ww3_strt')
                    # Run ww3_strt
                    deliver_file(self.icstr('{strt_inp}'),'ww3_strt.inp',keep=True,logger=logger)
                    link(self.getexe('ww3_strt'),'ww3_strt')
                    cmd=exe('./ww3_strt')
                    if redirect: cmd = cmd>='ww3_strt.log'
                    checkrun(cmd,logger=logger)

                if ww3_bdy == 'yes': 
                    try:
                        logger.info('ww3_bound: generating ww3 boundary condition')
                        self.get_ww3bdy_inputs()
                        # Run ww3_bound
                        deliver_file(self.icstr('{bound_inp}'),'ww3_bound.inp',keep=True,logger=logger)
                        link(self.getexe('ww3_bound'),'ww3_bound')
                        cmd=exe('./ww3_bound')
                        if redirect: cmd = cmd>='ww3_bound.log'
                        checkrun(cmd,logger=logger)
                    except Exception as ee:
                        self._products.pop('nest',None)
                        prodnames.pop('nest',None)
                        produtil.log.jlogger.warning(
                            'ww3_bound: will run without input boundary condition because ww3_bound '
                            'did not run successfully.',exc_info=True)

                if redirect: self._copy_log()

                # Prepare ww3_shel.inp
                ni=hafs.namelist.NamelistInserter(self.conf,self.section)
                shel_inp=self.icstr('{shel_inp}')
                atime=to_datetime(self.conf.cycle) # sim start time
                etime=to_datetime_rel(self.fcstlen*3600,atime) # sim end time
                flddt=int(self.outstep)
                pntdt=int(self.pntstep)
                #flddt=self.conf.getint('forecast_products','ww3_output_step',10800)
                #pntdt=self.conf.getint('forecast_products','ww3_pntout_step',10800)
                if pntdt > 0:
                    # Point output requested, need to provide buoy information
                    buoy_inp=self.icstr('{buoy_inp}')
                    with open(buoy_inp,'r') as bf:
                        #Read the file content and take out the eof character in the end.
                        buoyfile=bf.read()[:-1]
                elif pntdt == 0:
                    # Point output no requested, no further info needed
                    buoyfile='$'
                else:
                    # Wrong pntdt value
                    logger.warning('Wrong ww3_pntout_step value: %d. Set ww3_pntout_step = 0'%(pntdt,))
                    pntdt=0
                    self.pntout=0
                    buoyfile='$'
                ci=self.conf.getfloat('config','cycling_interval',6)
                retime=to_datetime_rel(ci*3600*1,atime) # restart end time
                invars=dict()
                invars.update(RUN_BEG=atime.strftime('%Y%m%d %H%M%S'),
                              RUN_END=etime.strftime('%Y%m%d %H%M%S'),
                              FLD_BEG=atime.strftime('%Y%m%d %H%M%S'),
                              FLD_END=etime.strftime('%Y%m%d %H%M%S'),
                              FLD_DT=int(flddt), 
                              PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                              PNT_END=etime.strftime('%Y%m%d %H%M%S'),
                              PNT_DT=int(pntdt), 
                              BUOY_FILE=buoyfile,
                              RST_BEG=atime.strftime('%Y%m%d %H%M%S'),
                              RST_END=retime.strftime('%Y%m%d %H%M%S'),
                              RST_DT=int(self.rststep) )

                with open(shel_inp,'rt') as nf:
                    with open('ww3_shel.inp','wt') as of:
                        of.write(ni.parse(nf,logger=logger,source=shel_inp,
                                          raise_all=True,atime=self.conf.cycle,**invars))

                self.deliver_products()
            self.state=COMPLETED
        except Exception as e:
            logger.error('Unhandled exception in wave init: %s'
                         %(str(e),),exc_info=True)
            self.state=FAILED
            self._copy_log()
            raise

    def _copy_log(self):
        logger=self.log()
        for lf in [ 'ww3_grid.log', 'ww3_prep_wind.log', 'ww3_prep_curr.log', 
                    'ww3_strt.log', 'ww3_untarbdy.log', 'ww3_bound.log' ]:
            comloc=self.icstr('{com}/{out_prefix}.{lf}.ww3',lf=lf)
            if os.path.exists(lf):
                deliver_file(lf,comloc,keep=True,logger=logger)

    def get_ww3bdy_inputs(self):
        """!Obtains WW3 input boundary condition data, links or copies to ww3init dir. 

        WW3 input boundary data comes from previous cycle's gfswave."""
        logger=self.log()
        redirect=self.confbool('redirect',True)
        atime=to_datetime(self.conf.cycle)
        wtime=to_datetime_rel(-6*3600,atime)
        ww3catalog=self.confstr('catalog','hafsdata')
        ww3dc=hafs.input.DataCatalog(self.conf,ww3catalog,wtime)
        dataset=self.confstr('gfswave_dataset','gfswave')
        item=self.confstr('ww3bdy_item','ww3bdy_ibp')
        when=wtime
        for itry in range(3):
            when=to_datetime_rel(-6*3600*itry,wtime)
            ww3bdyfile=ww3dc.locate(dataset,item,atime=when,logger=logger)
            if not ww3bdyfile:
                logger.info('%s: cannot decide data location for this time.'%(
                        when.strftime('%Y%m%d%H'),))
            ok=True
            (L,S) = produtil.fileop.lstat_stat(ww3bdyfile)
            if S is None:
                logger.info('%s: does not exist'%(ww3bdyfile,))
                ok=False
            if S.st_size<10000:
                logger.info('%s: too small (should be >=%d bytes)'%(
                        ww3bdyfile,10000))
                ok=False
            if not ok: continue
            # We get here if the ww3bdyfile exists and is big enough.
            ww3bdyspectar='gfs.t'+when.strftime('%H')+'z.ibp_tar'
            make_symlink(ww3bdyfile,ww3bdyspectar,force=True,logger=logger)
            ww3bdyfbase=self.icstr('./gfswave.HWRF{vit[basin1lc]}*')
            #cmd=exe('tar')['-zxvf', ww3bdyspectar, '--wildcards', ww3bdyfbase]
            cmd=exe('tar')['-xvf', ww3bdyspectar, '--wildcards', ww3bdyfbase]
            if redirect: cmd = cmd>='ww3_untarbdy.log'
            checkrun(cmd,logger=logger)
            return

    def get_ww3rst_inputs(self):
        """!Obtains global gdaswave restart file, links or copies to ww3init dir. 

        WW3 input restart file comes from current cycle's gdaswave."""
        logger=self.log()
        atime=to_datetime(self.conf.cycle)
        wtime=to_datetime_rel(-6*3600,atime)
        ww3catalog=self.confstr('catalog','hafsdata')
        ww3dc=hafs.input.DataCatalog(self.conf,ww3catalog,atime)
        dataset=self.confstr('gdaswave_dataset','gdaswave')
        item=self.confstr('ww3rst_item','ww3rst_gnh_10m')
        when=wtime
        ww3rstfile=ww3dc.locate(dataset,item,atime=when,logger=logger)
        if not ww3rstfile:
            logger.info('%s: cannot decide data location for this time.'%(
                    when.strftime('%Y%m%d%H'),))
        ok=True
        (L,S) = produtil.fileop.lstat_stat(ww3rstfile)
        if S is None:
            logger.info('%s: does not exist'%(ww3bdyfile,))
            ok=False
        if S.st_size<10000:
            logger.info('%s: too small (should be >=%d bytes)'%(
                    ww3rstfile,10000))
            ok=False
        if not ok:
            logger.warning('%s: ww3rst file from gdaswave not ok for this time.'%(
                    when.strftime('%Y%m%d%H'),))
        # We get here if the ww3rstfile exists and is big enough.
        make_symlink(ww3rstfile,'restart.gnh_10m',force=True,logger=logger)
        return

    def make_gint_inp(self,logger):
        # Prepare ww3_gint.inp
        ni=hafs.namelist.NamelistInserter(self.conf,self.section)
        gint_inp=self.confstr('gint_inp','')
        if not gint_inp: gint_inp=self.icstr('{PARMww3}/ww3_gint.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(RUN_BEG=atime.strftime('%Y%m%d %H%M%S'))
        with open(gint_inp,'rt') as nf:
            with open('ww3_gint.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=gint_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

########################################################################

ww3postprodnames={ 
    'ww3outgrd':       ( './out_grd.ww3',    '{com}/{out_prefix}.out_grd.ww3' ),
    'ww3grb2':         ( './gribfile',       '{com}/{out_prefix}.ww3.grb2' ),
    'ww3grb2idx':      ( './gribfile.idx',   '{com}/{out_prefix}.ww3.grb2.idx' ),
    'ww3ounf':         ( './ww3.%Y.nc',      '{com}/{out_prefix}.ww3_ounf.nc' ),
    'ww3outpnt':       ( './out_pnt.ww3',    '{com}/{out_prefix}.out_pnt.ww3' ),
    'ww3ounpspec':     ( './ww3.%Y_spec.nc', '{com}/{out_prefix}.ww3_ounp_spec.nc' ),
    'ww3outpbull':     ( './ww3_bull.tar',   '{com}/{out_prefix}.ww3_bull.tar' ),
    'ww3outpcbull':    ( './ww3_cbull.tar',  '{com}/{out_prefix}.ww3_cbull.tar' ),
    'ww3outpcsbull':   ( './ww3_csbull.tar', '{com}/{out_prefix}.ww3_csbull.tar' ),
    'ww3outpspec':     ( './ww3_spec.tar',   '{com}/{out_prefix}.ww3_spec.tar' ) }

class WW3Post(hafs.hafstask.HAFSTask):
    """Run WW3 post-process."""
    def __init__(self,dstore,conf,section,fcstlen=126,outstep=10800,pntstep=10800,**kwargs):
        super(WW3Post,self).__init__(dstore,conf,section,**kwargs)
        self.fcstlen=float(fcstlen)
        self.outstep=int(outstep)
        self.pntstep=int(pntstep)
        self._make_products()
        self._ncks_path=False

    def _make_products(self):
        """Creates FileProduct objects for all WW3Post output files.  The
        outdir is the directory to which the WW3Post package output its
        final files."""
        self._products=dict()
        atime=tcutil.numerics.to_datetime(self.conf.cycle)
        with self.dstore.transaction():
            for prodname,filepaths in ww3postprodnames.items():
                (localpath,compath)=filepaths
                localpath=self.conf.cycle.strftime(localpath)
                prod=produtil.datastore.FileProduct(
                    self.dstore,prodname,self.taskname)
                prod.location=self.timestr(compath,atime,atime)
                prod['localpath'] = localpath
                self._products[prodname]=( prod,localpath )

    def products(self,name=None):
        """Iterate over all products."""
        for prodname,stuff in self._products.items():
            (prod,localpath)=stuff
            if name is None or name==prod.prodname:
                yield prod

    def __copy_ncks(self,source,target,ignore):
        ncks=self.ncks_path
        logger=self.log()
        produtil.fileop.remove_file(target,logger=logger)
        checkrun(bigexe(ncks)['-4','-L','6',source,target]<'/dev/null',
                 logger=logger)

    @property 
    def ncks_path(self):
        """Returns the path to ncks.  Returns None if ncks cannot be
        found.  This function will only search for ncks once, and will
        cache the result.  Set self._ncks_path=False to force a
        recheck."""
        if self._ncks_path is False:
            ncks=self.getexe('ncks','')
            if not self._ncks_path:
                ncks=produtil.fileop.find_exe('ncks',raise_missing=False)
            assert(ncks is None or 
                   (isinstance(ncks,str) and ncks!=''))
            self._ncks_path=ncks
        return self._ncks_path

    def run(self):
        """Run the WW3 post."""
        logger=self.log()
        redirect=self.confbool('redirect',True)
        self.state=RUNNING
        try:
            with NamedDir(self.workdir,keep=True,logger=logger,rm_first=True) as d:
                # Prepare mod_def.ww3
                ww3moddef=self.icstr('{com}/{out_prefix}.mod_def.ww3')
                if not os.path.exists(ww3moddef):
                    logger.error('%s: mod_def.ww3 not yet available from forecast'%(
                            ww3moddef,))
                deliver_file(ww3moddef,'mod_def.ww3',force=True,logger=logger)
                # Prepare and deliver out_grd.ww3
                if self.outstep>0:
                    ww3out=self.icstr('{WORKhafs}/forecast/out_grd.ww3')
                    if not os.path.exists(ww3out):
                        logger.error('%s: out_grd.ww3 not yet available from forecast'%(
                                ww3out,))
                    deliver_file(ww3out,'out_grd.ww3',force=True,logger=logger)
                    (prod,localpath)=self._products['ww3outgrd']
                    prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=None)
                # Prepare and deliver out_pnt.ww3
                if self.pntstep>0:
                    ww3pnt=self.icstr('{WORKhafs}/forecast/out_pnt.ww3')
                    if not os.path.exists(ww3pnt):
                        logger.error('%s: out_pnt.ww3 not yet available from forecast'%(
                                ww3pnt,))
                    deliver_file(ww3pnt,'out_pnt.ww3',force=True,logger=logger)
                    (prod,localpath)=self._products['ww3outpnt']
                    prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=None)
                # For field output in grib2 format
                ww3_grib_post=self.confstr('ww3_grib_post','yes',section='ww3post')
                if ww3_grib_post == 'yes' and self.outstep>0:
                    make_symlink(self.getexe('ww3_grib'),'ww3_grib',force=True,logger=logger)
                    # Prepare the namelist
                    self.make_grib_inp(logger)
                    cmd=exe('./ww3_grib')
                    if redirect: cmd = cmd>='ww3_grib.log'
                    checkrun(cmd,logger=logger)
                    indexfile='gribfile.idx'
                    wgrib2=self.getexe('wgrib2')
                    logger.info('ww3post: Generating grib idx file for gribfile')
                    checkrun(bigexe(wgrib2)['-s','gribfile'] > indexfile,logger=logger)
                    (prod,localpath)=self._products['ww3grb2']
                    prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=None)
                    (prod,localpath)=self._products['ww3grb2idx']
                    prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=None)
               # For point output ww3_outp
                ww3_outp_bull_post=self.confstr('ww3_outp_bull_post','yes',section='ww3post')
                ww3_outp_spec_post=self.confstr('ww3_outp_spec_post','yes',section='ww3post')
                if self.pntstep>0:
                    make_symlink(self.getexe('ww3_outp'),'ww3_outp',force=True,logger=logger)
                    # Need to get information about the total number of buoys and their IDs
                    self.make_outp_info_inp(logger)
                    cmd=exe('./ww3_outp')
                    cmd = cmd>='ww3_outp_info.log'
                    checkrun(cmd,logger=logger)
                    fname='ww3_outp_info.log'
                    with open(fname) as f:
                        ww3_outp_info = f.readlines()
                    indices = [i for i, elem in enumerate(ww3_outp_info) if '----------' in elem]
                    buoys=ww3_outp_info[indices[0]+1:indices[1]-2]
                    # For point bullitin output
                    if ww3_outp_bull_post == 'yes':
                        filebull=[] 
                        filecbull=[] 
                        filecsbull=[] 
                        filelog=[] 
                        commands=list()
                        for i, buoy in enumerate(buoys):
                            ipnt=i+1
                            buoyid=buoy.split()[0]
                            buoylon=buoy.split()[1]
                            buoylat=buoy.split()[2]
                            logger.info('ww3_outp_bull for buoy: %i, %s, %s, %s'%(ipnt,buoyid,buoylon,buoylat))
                            with NamedDir('ww3outpbull.%s'%(buoyid,),keep=True,logger=logger) as nameddir:
                                self.make_outp_bull_inp(ipnt,logger)
                                make_symlink('../mod_def.ww3','mod_def.ww3',force=True,logger=logger)
                                make_symlink('../out_pnt.ww3','out_pnt.ww3',force=True,logger=logger)
                                make_symlink(self.getexe('ww3_outp'),'ww3_outp',force=True,logger=logger)
                                buoybull=buoyid+'.bull'
                                buoycbull=buoyid+'.cbull'
                                buoycsv=buoyid+'.csv'
                                buoycsbull=buoyid+'.csbull'
                                buoylog='ww3_outp_bull_'+buoyid+'.log'
                                filebull.append(buoybull)
                                filecbull.append(buoycbull)
                                filecsbull.append(buoycsbull)
                                filelog.append(buoylog)
                                cmd=('cd '+nameddir.dirname+' && '+
                                    './ww3_outp > ../'+buoylog+' && '+
                                    'mv '+buoybull+' ../ && '+
                                    'mv '+buoycbull+' ../ && '+
                                    'mv '+buoycsv+' ../'+buoycsbull+' && '+
                                    'cd ../')
                                commands.append(cmd)
                        cmdfname='command.file.ww3outpbull'
                        with open(cmdfname,'wt') as cfpf:
                            cfpf.write('\n'.join(commands))
                        clustername=produtil.cluster.name()
                        threads=os.environ['TOTAL_TASKS']
                        logger.info('ww3_outp_bull total threads: %s ',threads)
                        if clustername in ('cactus','dogwood'):
                            cfp_path=produtil.fileop.find_exe('cfp')
                            cmd2=mpirun(mpi(cfp_path)[cmdfname],allranks=True)
                        else:
                            mpiserial_path=os.environ.get('MPISERIAL','*MISSING*')
                            if mpiserial_path=='*MISSING*':
                                mpiserial_path=self.getexe('mpiserial')
                            cmd2=mpirun(mpi(mpiserial_path)['-m',cmdfname],allranks=True)
                        checkrun(cmd2)
                        # Tar the outputs and diliver to com dir
                        cmd=exe('tar')['-cvf', 'ww3_bull.tar'][filebull]
                        checkrun(cmd,logger=logger)
                        cmd=exe('tar')['-cvf', 'ww3_cbull.tar'][filecbull]
                        checkrun(cmd,logger=logger)
                        cmd=exe('tar')['-cvf', 'ww3_csbull.tar'][filecsbull]
                        checkrun(cmd,logger=logger)
                        cmd=exe('cat')[filelog] >> 'ww3_outp_bull.log'
                        checkrun(cmd,logger=logger)
                        (prod,localpath)=self._products['ww3outpbull']
                        prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=None)
                        (prod,localpath)=self._products['ww3outpcbull']
                        prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=None)
                        (prod,localpath)=self._products['ww3outpcsbull']
                        prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=None)
                    # For point spec output
                    if ww3_outp_spec_post == 'yes':
                        fileout=[] 
                        filelog=[] 
                        commands=list()
                        ww3tstr=self.conf.cycle.strftime('%y%m%d%H')
                        for i, buoy in enumerate(buoys):
                            ipnt=i+1
                            buoyid=buoy.split()[0]
                            buoylon=buoy.split()[1]
                            buoylat=buoy.split()[2]
                            logger.info('ww3_outp_spec for buoy: %i, %s, %s, %s'%(ipnt,buoyid,buoylon,buoylat))
                            with NamedDir('ww3outpspec.%s'%(buoyid,),keep=True,logger=logger) as nameddir:
                                self.make_outp_spec_inp(ipnt,logger)
                                make_symlink('../mod_def.ww3','mod_def.ww3',force=True,logger=logger)
                                make_symlink('../out_pnt.ww3','out_pnt.ww3',force=True,logger=logger)
                                make_symlink(self.getexe('ww3_outp'),'ww3_outp',force=True,logger=logger)
                                buoyspc='ww3.'+ww3tstr+'.spc'
                                buoyout=buoyid+'.spc'
                                buoylog='ww3_outp_spec_'+buoyid+'.log'
                                fileout.append(buoyout)
                                filelog.append(buoylog)
                                cmd=('cd '+nameddir.dirname+' && '+
                                    './ww3_outp > ../'+buoylog+' && '+
                                    'mv '+buoyspc+' ../'+buoyout+' && '+
                                    'cd ../')
                                commands.append(cmd)
                        cmdfname='command.file.ww3outpspec'
                        with open(cmdfname,'wt') as cfpf:
                            cfpf.write('\n'.join(commands))
                        threads=os.environ['TOTAL_TASKS']
                        logger.info('ww3_outp_spec total threads: %s ',threads)
                        clustername=produtil.cluster.name()
                        if clustername in ('cactus','dogwood'):
                            cfp_path=produtil.fileop.find_exe('cfp')
                            cmd2=mpirun(mpi(cfp_path)[cmdfname],allranks=True)
                        else:
                            mpiserial_path=os.environ.get('MPISERIAL','*MISSING*')
                            if mpiserial_path=='*MISSING*':
                                mpiserial_path=self.getexe('mpiserial')
                            cmd2=mpirun(mpi(mpiserial_path)['-m',cmdfname],allranks=True)
                        checkrun(cmd2)
                        # Tar the outputs and deliver to com dir
                        cmd=exe('tar')['-cvf', 'ww3_spec.tar'][fileout]
                        checkrun(cmd,logger=logger)
                        cmd=exe('cat')[filelog] >> 'ww3_outp_spec.log'
                        checkrun(cmd,logger=logger)
                        (prod,localpath)=self._products['ww3outpspec']
                        prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=None)
                # Additional ww3post products
                # For field output in netcdf format
                ww3_ounf_post=self.confstr('ww3_ounf_post','yes',section='ww3post')
                if ww3_ounf_post == 'yes' and self.outstep>0:
                    make_symlink(self.getexe('ww3_ounf'),'ww3_ounf',force=True,logger=logger)
                    # Prepare the namelist
                    self.make_ounf_inp(logger)
                    # Run ww3_ounf
                    cmd=exe('./ww3_ounf')
                    if redirect: cmd = cmd>='ww3_ounf.log'
                    checkrun(cmd,logger=logger)
                    (prod,localpath)=self._products['ww3ounf']
                    logger.info('Delivering ww3ounf from %s to %s'%(localpath,prod.location))
                    prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=self.__copy_ncks)
                # For point spec output in netcdf format
                ww3_ounp_spec_post=self.confstr('ww3_ounp_spec_post','yes',section='ww3post')
                if ww3_ounp_spec_post == 'yes' and self.pntstep>0:
                    make_symlink(self.getexe('ww3_ounp'),'ww3_ounp',force=True,logger=logger)
                    # Prepare the namelist
                    self.make_ounp_spec_inp(logger)
                    # Run ww3_ounp
                    cmd=exe('./ww3_ounp')
                    if redirect: cmd = cmd>='ww3_ounp.log'
                    checkrun(cmd,logger=logger)
                    (prod,localpath)=self._products['ww3ounpspec']
                    prod.deliver(frominfo=localpath,location=prod.location,logger=logger,copier=self.__copy_ncks)
            self.state=COMPLETED
        except Exception as e:
            self.state=FAILED
            logger.error("WW3 post failed: %s"%(str(e),),exc_info=True)
            raise

    def make_grib_inp(self,logger):
        # Prepare ww3_grib.inp
        ni=hafs.namelist.NamelistInserter(self.conf,self.section)
        grib_inp=self.confstr('grib_inp','')
        if not grib_inp: grib_inp=self.icstr('{PARMww3}/ww3_grib.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(FLD_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      FLD_DT=int(self.outstep),
                      RUN_BEG=atime.strftime('%Y%m%d %H%M%S'))
        with open(grib_inp,'rt') as nf:
            with open('ww3_grib.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=grib_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_ounf_inp(self,logger):
        # Prepare ww3_ounf.inp
        ni=hafs.namelist.NamelistInserter(self.conf,self.section)
        ounf_inp=self.confstr('ounf_inp','')
        if not ounf_inp: ounf_inp=self.icstr('{PARMww3}/ww3_ounf.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(FLD_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      FLD_DT=int(self.outstep))
        with open(ounf_inp,'rt') as nf:
            with open('ww3_ounf.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=ounf_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_ounp_spec_inp(self,logger):
        # Prepare ww3_ounp.inp
        ni=hafs.namelist.NamelistInserter(self.conf,self.section)
        ounp_spec_inp=self.confstr('ounp_spec_inp','')
        if not ounp_spec_inp: ounp_spec_inp=self.icstr('{PARMww3}/ww3_ounp_spec.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      PNT_DT=int(self.pntstep))
        with open(ounp_spec_inp,'rt') as nf:
            with open('ww3_ounp.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=ounp_spec_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_outp_info_inp(self,logger):
        # Prepare ww3_outp.inp
        ni=hafs.namelist.NamelistInserter(self.conf,self.section)
        outp_info_inp=self.confstr('outp_info_inp','')
        if not outp_info_inp: outp_info_inp=self.icstr('{PARMww3}/ww3_outp_info.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      PNT_DT=int(self.pntstep))
        with open(outp_info_inp,'rt') as nf:
            with open('ww3_outp.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=outp_info_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_outp_bull_inp(self,ipnt,logger):
        # Prepare ww3_outp.inp
        ni=hafs.namelist.NamelistInserter(self.conf,self.section)
        outp_bull_inp=self.confstr('outp_bull_inp','')
        if not outp_bull_inp: outp_bull_inp=self.icstr('{PARMww3}/ww3_outp_bull.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      PNT_DT=int(self.pntstep),
                      PNT_NUM=int(ipnt),
                      RUN_BEG=atime.strftime('%Y%m%d %H%M%S'))
        with open(outp_bull_inp,'rt') as nf:
            with open('ww3_outp.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=outp_bull_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_outp_spec_inp(self,ipnt,logger):
        # Prepare ww3_outp.inp
        ni=hafs.namelist.NamelistInserter(self.conf,self.section)
        outp_spec_inp=self.confstr('outp_spec_inp','')
        if not outp_spec_inp: outp_spec_inp=self.icstr('{PARMww3}/ww3_outp_spec.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      PNT_DT=int(self.pntstep),
                      PNT_NUM=int(ipnt),
                      RUN_BEG=atime.strftime('%Y%m%d %H%M%S'))
        with open(outp_spec_inp,'rt') as nf:
            with open('ww3_outp.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=outp_spec_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

