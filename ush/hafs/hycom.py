
"""HYCOM related initialization and post-processing jobs."""
# Updates Biju Thomas on 05/26/2022
#     Added cfp option for WCOSS2
#     Updated command.preview file's inputs consistent for getarg() function in gfs2ofs2(BT)
import re, sys, os, glob, datetime, math, fractions, collections, subprocess
import tarfile
import produtil.fileop, produtil.log
import produtil.cluster
import tcutil.numerics, hafs.input, hafs.namelist
import hafs.hafstask, hafs.exceptions
import time, shutil
from produtil.cd import NamedDir
from produtil.fileop import make_symlink, isnonempty, remove_file, \
    deliver_file, gribver, wait_for_files
from produtil.datastore import FileProduct, COMPLETED, RUNNING, FAILED
from produtil.run import *
from produtil.log import jlogger
from tcutil.numerics import to_datetime,to_datetime_rel,TimeArray,to_fraction,\
    to_timedelta

##@var __all__
# Symbols exported by "from hafs.hycom import *"
__all__=['date_hycom2normal','date_normal2hycom','HYCOMInit1', 'HYCOMInit2', 'HYCOMPost']

NO_DEFAULT=object()

def yesno(x):
    return 'YES' if x else 'NO'

hycom_epoch=datetime.datetime(1900,12,31,0,0,0)

def date_hycom2normal(hycom):
    if isinstance(hycom,str):
        hycom=float(hycom)
    if isinstance(hycom,int):
        hycom=datetime.timedelta(hours=hycom*24)
    elif isinstance(hycom,float):
        hycom=datetime.timedelta(hours=hycom*24)
    elif isinstance(hycom,fractions.Fraction):
        hycom=datetime.timedelta(hours=float(hycom*24))
    return hycom_epoch+hycom

def date_normal2hycom(normal):
    if not isinstance(normal,datetime.datetime):
        normal=to_datetime(normal)
    return to_fraction(normal-hycom_epoch)/(3600*24)

def scriptexe(task,path):
    """Generates a produtil.prog.Runner for running a Hycom ksh
    script.  Adds a bunch of variables from config sections."""
    there=task.confstrinterp(path)
    e=exe(there)
    vars=dict()
    for k,v in task.conf.items(task.confstr('strings','hycomstrings')):
        vars[k]=str(v)
    for k,v in task.conf.items(task.confstr('bools','hycombools')):
        vars[k]=yesno(v)
    RTOFSDIR=task.meta('RTOFSDIR','')
    if RTOFSDIR:
        vars['RTOFSDIR']=RTOFSDIR
    return    e.env(**vars)

def read_RUNmodIDout(path):
    RUNmodIDout=''
    with open(path,'rt') as f:
        for line in f:
            m=re.match('^export RUNmodIDout=(.*)$',line)
            if m:
                RUNmodIDout=m.groups()[0]
    return RUNmodIDout

class HYCOMInit1(hafs.hafstask.HAFSTask):
    def remove_ocean(): self.uncouple()

    def __init__(self,dstore,conf,section,taskname=None,fcstlen=126,
                 **kwargs):
        super(HYCOMInit1,self).__init__(dstore,conf,section,
                                       taskname=taskname,**kwargs)
        self.forecast_exe=None
        self.run_coupled=True
        self.fcstlen=fcstlen
        self.make_products()
        self.spinlength=self.confint('spinlength',0)
        self.ic=None
        self.jc=None
        self.idm=None
        self.jdm=None
        self.ijgrid=None
        self.Application=None
        self.__rtofs_inputs=None
        self.__rtofs_inputs_ymd=None
        self.__blkdat=None

    def make_products(self):
        """Initializes all Product objects to make them available to
        future jobs."""
        # Add the HyCOM-specific products whose delivery location is
        # in COM with the standard output file prefix
        # (invest99l.2017110318).
        logger=self.log()
        self.hycom_settings=FileProduct(
            self.dstore,'hycom_settings',self.taskname,location=
            self.confstrinterp('{com}/{out_prefix}.hafs.hycom_settings'))

        # prodnameA and prodnameB are three-hourly:
        fhrs=list(range(int(self.fcstlen+25.001)))
        fhrs=fhrs[0::3]
        atime=to_datetime(self.conf.cycle)
        ftimes=[to_datetime_rel(t*3600,atime) for t in fhrs]
        self.init_file2a=TimeArray(atime,ftimes[-1],3*3600.0)
        self.init_file2b=TimeArray(atime,ftimes[-1],3*3600.0)
        for ftime in ftimes:
            prodnameA=self.timestr('hafs_basin.{fahr:03d}.a',ftime,atime)
            filepathA=self.timestr('{com}/{out_prefix}.{pn}',pn=prodnameA)
            prodnameB=self.timestr('hafs_basin.{fahr:03d}.b',ftime,atime)
            filepathB=self.timestr('{com}/{out_prefix}.{pn}',pn=prodnameB)
            self.init_file2a[ftime]=FileProduct(
                self.dstore,prodnameA,self.taskname,location=filepathA)
            self.init_file2b[ftime]=FileProduct(
                self.dstore,prodnameB,self.taskname,location=filepathB)

        # initial conditions:
        self.restart_out=dict()
        what='restart_out'
        for ab in 'ab':
                local=what+'.'+ab # like restart_out.a or restart_outR.b
                self.restart_out[local]=FileProduct(self.dstore,local,self.taskname)

        self.spin_archv_a=FileProduct(self.dstore,'spin_archv_a',self.taskname)
        self.spin_archv_b=FileProduct(self.dstore,'spin_archv_b',self.taskname)

        self.blkdat_input=FileProduct(
            self.dstore,'blkdat.input',self.taskname,location=
            self.confstrinterp('{com}/{out_prefix}.hafs.hycom.blkdat.input'))

    def last_lead_time_today(self,cychour):
        if cychour<6: return 0
        # 96 hours available minus 6z cycle
        #if cychour<12: return 96   DAN - 6z and 12z RTOFS runs too late so get from PDYm1
        if cychour<18: return 0
        return 192

    def find_rtofs_data(self):
        """!Fills the RTOFS staging area with RTOFS data."""

        logger=self.log()
        cyc=self.conf.cycle

        rtofs_atime=datetime.datetime(cyc.year,cyc.month,cyc.day,0)
        rtofs_ymd=rtofs_atime.strftime('%Y%m%d')

        # Input directories:
        inputs=self.rtofs_inputs
        logger.info('FRD: inputs=%s'%(repr(inputs)))
        oceands=self.confstr('ocean_dataset')
        ocean_now=self.confstr('ocean_now')
        ocean_fcst=self.confstr('ocean_fcst')

        logger.info('FRD: oceands=%s'%(repr(oceands)))

        oceanatime=datetime.datetime(cyc.year,cyc.month,cyc.day,0,0,0)
        cyctime=to_timedelta(cyc.hour*3600)
        logger.info('FRD: oceanatime=%s'%(repr(oceanatime)))
        logger.info('FRD: cyctime=%s'%(repr(cyctime)))

        # find the latest RTOFS data for IC:
        if cyc.hour==0:
           zeroloc=inputs.locate(oceands,ocean_now,atime=oceanatime,ab='a')
        else:
           zeroloc=inputs.locate(oceands,ocean_fcst,atime=oceanatime,ftime=oceanatime+cyctime,ab='a')

        zerodir=os.path.dirname(zeroloc)
        zerostat=-1
        if isnonempty(zeroloc):
            zerostat=0

        logger.info('FRD1: zeroloc=%s zerostat=%d '%(repr(zeroloc),zerostat))

        loc0=inputs.locate(oceands,ocean_now,atime=oceanatime,ab='a')
        dir0=os.path.dirname(loc0)

        # Decide the staging directory:
        outdir=self.confstr('RTOFS_STAGE','')
        if not outdir:
            outdir=os.path.join(self.workdir,rtofs_atime.strftime('rtofs.%Y%m%d'))

        # Get data:
        ni=hafs.namelist.NamelistInserter(self.conf,self.section)
        with NamedDir(outdir,keep=True,logger=logger,rm_first=False) as d:
            parmin=self.confstrinterp('{PARMhycom}/hafs_get_rtofs.nml.in')
            parmout='get_rtofs.nml'

            # from PDY (loc0)
            if zerostat==0:
                lastleadtimetoday=0
                #-org: starthr=cyc.hour-24
                # need to change it into
                starthr=cyc.hour
                endhr=cyc.hour
                #endhr=cyc.hour+18
                logger.info('FRDa: dir0=%s starthr=%d endhr=%d lastleadtimetoday=%d'%(repr(dir0),starthr,endhr,lastleadtimetoday))
                with open(parmin,'rt') as inf:
                    with open(parmout,'wt') as outf:
                        outf.write(ni.parse(inf,logger,parmin,atime=rtofs_atime,
                            INDIR1=dir0,INDIR2=dir0,INDIR3=dir0,
                            STARTHR=starthr,ENDHR=endhr,
                            LAST_LEAD_TIME_TODAY=18))
                checkrun(mpirun(mpi(self.getexe('hafs_get_rtofs')),allranks=True),logger=logger)
                os.rename('get_rtofs.nml','get_rtofs.nml.0')

    def run(self):
        """Runs the hycom initialization for hycominit1.  Raises an exception if
        something goes wrong.  Returns on success."""
        logger=self.log()
        # Reset coupling status information:
        self.forecast_exe=None
        self.run_coupled=False
        try:
            self.state=RUNNING
            # Inside the "with" block, we create and cd to the work
            # directory and then cd back out at the end.  The
            # "rm_first=True" means the directory is deleted first if
            # it already exists upon entry of the "with" block.
            with NamedDir(self.workdir,keep=not self.scrub,
                          logger=logger,rm_first=True) as d:
                self.select_domain(logger)
                produtil.fileop.makedirs(self.timestr('{com}'),logger=logger)
                self.hycom_settings.deliver(frominfo='./hycom_settings')

                # Deliver hycom_settings to intercom
                produtil.fileop.makedirs(self.timestr('{intercom}/hycominit'),logger=logger)
                prodname='hycom_settings'
                locintercom=self.timestr('{intercom}/hycominit/'+prodname)
                deliver_file(prodname,locintercom,keep=True,logger=logger)

                # Do not need this step anymore, use the RTOFS input files from COMRTOFS directly
                #self.find_rtofs_data()

                # Create BC and IC for this domain
                self.create_bc_ic(logger)

                # Find the runmodidout with respect to domain
                RUNmodIDout=self.RUNmodIDout
                RUNmodIDout=read_RUNmodIDout('./hycom_settings')

                # Deliver hafs_basin.{fahr:03d}.[ab] files to com
                atime=to_datetime(self.conf.cycle)
                ftime=to_datetime_rel(0*3600,atime)
                prodnameA=self.timestr('hafs_basin.{fahr:03d}.a',ftime,atime)
               #filepathA=self.timestr('{com}/{out_prefix}.{pn}',pn=prodnameA)
                filepathA=self.timestr('{intercom}/hycominit/{pn}',pn=prodnameA)
                prodnameB=self.timestr('hafs_basin.{fahr:03d}.b',ftime,atime)
               #filepathB=self.timestr('{com}/{out_prefix}.{pn}',pn=prodnameB)
                filepathB=self.timestr('{intercom}/hycominit/{pn}',pn=prodnameB)
                deliver_file(prodnameA,filepathA,keep=True,logger=logger)
                deliver_file(prodnameB,filepathB,keep=True,logger=logger)

                # Deliver restart files to com
                for(prodname,prod) in self.restart_out.items():
                    (local,ab)=prodname.split('.')
                    loc=self.timestr('{'+local+'}',ab=ab,RUNmodIDout='hafs.hycom')
                    prod.deliver(location=loc,frominfo=prodname,
                                 keep=True,logger=logger)

                # Deliver restart files to intercom
                for(prodname,prod) in self.restart_out.items():
                    (local,ab)=prodname.split('.')
                    locintercom=self.timestr('{intercom}/hycominit/'+prodname)
                    deliver_file(prodname,locintercom,keep=True,logger=logger)

                # Make the flag file to indicate we're done.
               #done=self.timestr('{com}/{out_prefix}.hafs.hycominit1.done')
               #with open(done,'wt') as f:
               #    f.write('hycominit1 done for this cycle\n')

                # Make sure we run coupled:
                self.run_coupled=True
            self.state=COMPLETED
        except Exception as e:
            logger.error('Unhandled exception in ocean init: %s'
                         %(str(e),),exc_info=True)
            self.state=FAILED
            raise
        except:  # fatal signal, other catastrophic errors
            self.state=FAILED
            raise

    def create_bc_ic(self,logger):
        thiscycle=self.conf.cycle
        prevcycle=to_datetime_rel(-6*3600,self.conf.cycle)
        spinlen=self.spinlength
        fsecs=self.fcstlen*3600
        fcstlen=(fsecs+1800)//3600
        end=to_datetime_rel(fcstlen*3600,self.conf.cycle)

        ocean_status=0
        boundary_conditions_from_rtofs=True
        same_domain=1
        force_coldstart=0

        if same_domain==1 and force_coldstart==0:
            spin=True
            create_subdomain=False

        logger.info('Create subdomain from RTOFS.')
        self.rtofs_subset_bdry_init(logger)

        logger.info('Spin up analysis.')
        self.rtofs_spin(logger)

    def select_domain(self,logger):
        hycom_domain=self.confstr('hycom_domain','small')
        basin=self.storminfo.pubbasin2
        Application=None
        if hycom_domain=='large' and basin in ['AL', 'EP', 'CP']:
            Application='nhc_basin'
        elif hycom_domain=='large' and basin in ['WP', 'IO']:
            Application='jtnh_basin'
        elif hycom_domain=='large' and basin in ['SH', 'SP', 'SI']:
            Application='jtsh_basin'
        elif basin=='AL':
            Application='hat10_basin'
        elif basin=='EP':
            Application='hep20_basin'
        elif basin=='CP':
            Application='hcp70_basin'
        elif basin=='WP':
            Application='hwp30_basin'
        elif basin=='IO':
            Application='hin40_basin'
        elif basin in [ 'SL', 'LS' ]:
            Application='hsn50_basin'
        elif basin in [ 'SH', 'SP', 'SI' ]:
            Application='hsp60_basin'
        else:
            msg='No ocean basin available for basin=%s lat=%s.  Run uncoupled.'%(
                basin,repr(atmos_lon))
            jlogger.warning(msg)
            raise hafs.exceptions.NoOceanBasin(msg)
        self.RUNmodIDin='rtofs_glo'
        RUNmodIDin=self.RUNmodIDin

        aptable=self.confstrinterp('{PARMhycom}/hafs_hycom.application_table')
        found=False
        with open(aptable,'rt') as apfile:
            for line in apfile:
                fields=line.split()
                if len(fields)<4:
                    logger.info('%s: ignore line %s'%(aptable,line.strip()))
                    continue
                (ap,inmod,outmod,gridid) = fields
                if ap==Application and inmod==self.RUNmodIDin:
                    found=True
                    self.RUNmodIDout=outmod
                    self.gridid=gridid
                    found=True
        del line,apfile
        if not found:
            msg='%s: could not find Application=%s RUNmodIDin=%s'%(
                aptable,repr(Application),repr(self.RUNmodIDin))
            logger.error(msg)
            raise hafs.exceptions.InvalidOceanInitMethod(msg)

        gridtable=self.confstrinterp('{PARMhycom}/hafs_hycom.grid_table')
        found=False
        with open(gridtable,'rt') as gridfile:
            for line in gridfile:
                fields=line.split()
                if len(fields)<11:
                    logger.info('%s: ignore line %s'%(gridtable,line.strip()))
                    continue
                (ingridid,gridlabelin,gridlabelout,grid_source,\
                     idm,jdm,kdm,ic,jc,ijgrid,gridno) = fields
                idm=int(idm)
                jdm=int(jdm)
                kdm=int(kdm)
                ic=int(ic)
                jc=int(jc)
                ijgrid=int(ijgrid)
                gridno=int(gridno)
                if ingridid==self.gridid and line.find(RUNmodIDin)>=0:
                    found=True
                    ( self.idm, self.jdm, self.kdm, self.kkdm ) = \
                     (     idm,      jdm,      kdm,      kdm  )
                    ( self.ic, self.jc, self.ijgrid, self.gridno ) = \
                     (     ic,      jc,      ijgrid,      gridno )
                    ( self.gridlabelin, self.gridlabelout ) = \
                     (     gridlabelin,      gridlabelout )
        if not found:
            msg='%s: could not find grid=%s RUNmodIDin=%s'%(
                gridtable,repr(self.gridid),repr(RUNmodIDin))
            logger.error(msg)
            raise hafs.exceptions.InvalidOceanInitMethod(msg)
        assert(self.section=='hycominit1')
        self.conf.set(self.section,'RUNmodIDin',self.RUNmodIDin)
        self.conf.set(self.section,'gridlabelin',self.gridlabelin)
        self.conf.set(self.section,'gridlabelout',self.gridlabelout)
        self.conf.set(self.section,'RUNmodIDout',self.RUNmodIDout)
        with open('hycom_settings','wt') as f:
            f.write('''export idm={idm}
export jdm={jdm}
export kdm={kdm}
export kkdm={kkdm}
export ic={ic}
export jc={jc}
export ijgrid={ijgrid}
export gridlabelout={gridlabelout}
export gridlabelin={gridlabelin}
export RUNmodIDout={RUNmodIDout}
export RUNmodIDin={RUNmodIDin}
export gridno={gridno}\n'''.format(**self.__dict__))

        logger.info('HYCOM grid: i,j,kdm=%d,%d,%d ic,jc=%d,%d ijgrid=%d gridno=%d'%(
                idm,jdm,kdm,ic,jc,ijgrid,gridno))
        jlogger.info('HYCOM domain: in=%s.%s out=%s.%s'%(
                self.RUNmodIDin,self.gridlabelin,
                self.RUNmodIDout,self.gridlabelout))

    @property
    def rtofs_inputs(self):
        if self.__rtofs_inputs is None:
            hd=self.confstr('catalog','hafsdata')
            inputs=hafs.input.DataCatalog(self.conf,hd,self.conf.cycle)
            self.__rtofs_inputs=inputs
        return self.__rtofs_inputs

    @property
    def rtofs_inputs_ymd(self):
        if self.__rtofs_inputs_ymd is None:
            hd=self.confstr('catalog','hafsdata')
            inputs=hafs.input.DataCatalog(self.conf,hd,self.conf.cycle)
            self.__rtofs_inputs_ymd=inputs
        return self.__rtofs_inputs_ymd

    def rtofs_subset_bdry_init(self,logger):
        inputs=self.rtofs_inputs_ymd
        logger.info('RI: inputsymd=%s'%(repr(inputs)))
        def linkf(ffrom,fto):
            produtil.fileop.make_symlink(ffrom,fto,force=True,logger=logger)

        RUNmodIDin=self.RUNmodIDin
        RUNmodIDout=self.RUNmodIDout
        gridlabelin=self.gridlabelin
        gridlabelout=self.gridlabelout

        if int(self.ijgrid)==1:
            cmd=alias(batchexe(self.getexe('hafs_rtofs_subregion')))
        elif int(self.ijgrid)==2:
            cmd=alias(batchexe(self.getexe('hafs_isubregion2avg')))
        else:
            msg='Invalid ijgrid value %s'%(repr(ijgrid),)
            logger.critical(msg)
            raise hafs.exceptions.InvalidOceanInitMethod(msg)

        self.linkab('{FIXhycom}/%s.%s.regional.grid'%(RUNmodIDin,gridlabelin),
             'regional.grid')
        self.linkab('{FIXhycom}/%s.%s.regional.depth'%(RUNmodIDin,gridlabelin),
             'regional.depth')
        self.linkab('{FIXhycom}/hafs_%s.%s.regional.grid'%(RUNmodIDout,gridlabelout),
             'regional.subgrid')
        self.linkab('{FIXhycom}/hafs_%s.%s.regional.depth'%(RUNmodIDout,gridlabelout),
             'regional.subdepth')

        cyc=self.conf.cycle

        # --- FIND INPUTS FROM PAST DAYS AND LINK TO archs_in.%d.ab (odd counts in hrrange)
        # --- hsk: and archv_in.%d.ab (even counts)
        outdir=self.confstr('RTOFS_STAGE','')
        icount=-1
        atype='v'

        ihr=0
        icount+=1
        now=to_datetime_rel(ihr*3600.0,cyc)
        filestringtime=now.strftime('%Y%m%d_%H%M%S')

        if cyc.hour == 0:
           rtofsatgz=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.n{aHH}.archv.a.tgz')
           rtofsa=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.n{aHH}.archv.a')
           rtofsb=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.n{aHH}.archv.b')
           archva=self.timestr('./rtofs_glo.t00z.n{aHH}.archv.a')
           archvb=self.timestr('./rtofs_glo.t00z.n{aHH}.archv.b')
        else:
           rtofsatgz=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.f{aHH}.archv.a.tgz')
           rtofsa=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.f{aHH}.archv.a')
           rtofsb=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.f{aHH}.archv.b')
           archva=self.timestr('./rtofs_glo.t00z.f{aHH}.archv.a')
           archvb=self.timestr('./rtofs_glo.t00z.f{aHH}.archv.b')

        if os.path.exists(rtofsb):
           produtil.fileop.make_symlink(rtofsb,archvb,force=True,logger=logger)
        else:
           logger.error('File %s does not exist'%(rtofsb))
           raise
        if os.path.exists(rtofsa):
           produtil.fileop.make_symlink(rtofsa,archva,force=True,logger=logger)
        elif os.path.exists(rtofsatgz):
           logger.info('File %s exists, untar it into %s'%(rtofsatgz,archva))
           with tarfile.open(rtofsatgz,'r:gz') as tgz:
              tgz.extractall()
        else:
           logger.error('Neither %s nor %s exists'%(rtofsa,rtofsatgz))
           raise

        linkf(archva,'archv_in.%d.a'%icount)
        linkf(archvb,'archv_in.%d.b'%icount)
        subregion_in='subregion.%d.in'%icount
        subregion_out='subregion.%d.out'%icount
        with open(subregion_in,'wt') as f:
            f.write("""archv_in.%d.b
hafs_basin.%03d.b
subregion %s
%d         'idm   ' = longitudinal array size
%d         'jdm   ' = latitudinal  array size
%d         'irefi ' = 1st index origin of subgrid or 0 if NOT aligned
%d         'jrefi ' = 2nd index origin of subgrid
1          'irefo ' = longitude output reference location
1          'jrefo ' = latitude output reference location
0          'iceflg' = ice in output archive flag (0=none,1=energy loan model)
""" %(
                icount, icount*3, self.Application,
                int(self.idm),int(self.jdm),
                int(self.ic),int(self.jc)))

        # --- RUN SUBREGIONING PROGRAM
        checkrun(cmd<subregion_in,logger=logger)

    def rtofs_spin(self,logger):
        cyc=self.conf.cycle
        spinstart=to_datetime_rel(-self.spinlength*3600,cyc)

        epsilon=30

        rtofs_restart_atgz=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.n00.restart.a.tgz')
        rtofs_restart_a=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.n00.restart.a')
        rtofs_restart_b=self.timestr('{COMrtofs}/rtofs.{aYMD}/rtofs_glo.t00z.n00.restart.b')
        restart_in_a=self.timestr('./rtofs_glo.t00z.n00.restart.a')
        restart_in_b=self.timestr('./rtofs_glo.t00z.n00.restart.b')

        if os.path.exists(rtofs_restart_b):
           produtil.fileop.make_symlink(rtofs_restart_b,restart_in_b,force=True,logger=logger)
        else:
           logger.error('File %s does not exist'%(rtofs_restart_b))
           raise
        if os.path.exists(rtofs_restart_a):
           produtil.fileop.make_symlink(rtofs_restart_a,restart_in_a,force=True,logger=logger)
        elif os.path.exists(rtofs_restart_atgz):
           logger.info('File %s exists, untar it into %s'%(rtofs_restart_atgz,restart_in_a))
           with tarfile.open(rtofs_restart_atgz,'r:gz') as tgz:
              tgz.extractall()
        else:
           logger.error('Neither %s nor %s exists'%(rtofs_restart_a,rtofs_restart_atgz))
           raise

        logger.info('restart_in_a=%s'%(repr(restart_in_a)))
        logger.info('restart_in_b=%s'%(repr(restart_in_b)))

        if restart_in_a is None:
            msg='No hycom restart file found.  Giving up.'
            jlogger.error(msg)
            if not allow_fallbacks and not expect:
                raise hafs.exceptions.OceanRestartMissing(msg)

        self.restart2restart(restart_in_a,restart_in_b,
                             'restart_pre.a','restart_pre.b',logger)
        with open('restart_pre.b','rt') as bf:
            bf.readline()
            splat=bf.readline().split()
            rydf=float(splat[4])
            ryd=date_hycom2normal(rydf)

        logger.info('SOME STRING FOR WHICH TO GREP - ryd=%s spinstart=%s ryd-spinstart=%s to_fraction(...)=%s abs=%s epsilon=%s'%(
                    repr(ryd),repr(spinstart),repr(ryd-spinstart),repr(to_fraction(ryd-spinstart,negok=True)),
                    repr(abs(to_fraction(ryd-spinstart,negok=True))),repr(epsilon)))
        if abs(to_fraction(ryd-spinstart,negok=True))<epsilon:
            logger.info('Restart is at right time.  Move restart_pre to restart_forspin.')
            self.moveab('restart_pre','restart_out')
        else:
            logger.info('Restart is at wrong time (%s instead of %s).  Will use archv2restart.'%(
                    ryd.strftime("%Y%m%d%H"),spinstart.strftime('%Y%m%d%H')))
            self.archv2restart(logger)

    def blkflag(self,flagname,default=NO_DEFAULT):
        blkdat=self.blkdat
        if  default is not NO_DEFAULT and \
                flagname not in blkdat:
            return default
        return abs(self.blkdat[flagname][0])>.01

    def copyab(self,sfrom,fto):
        ffrom=self.timestr(sfrom)
        produtil.fileop.deliver_file(
            ffrom+'.a',fto+'.a',keep=True,logger=self.log())
        produtil.fileop.deliver_file(
            ffrom+'.b',fto+'.b',keep=True,logger=self.log())

    def moveab(self,ffrom,fto):
        deliver_file(ffrom+'.a',fto+'.a',keep=False,logger=self.log())
        deliver_file(ffrom+'.b',fto+'.b',keep=False,logger=self.log())

    def linkab(self,sfrom,fto):
        ffrom=self.timestr(sfrom)
        produtil.fileop.make_symlink(
            ffrom+'.a',fto+'.a',force=True,logger=self.log())
        produtil.fileop.make_symlink(
            ffrom+'.b',fto+'.b',force=True,logger=self.log())

    @property
    def blkdat(self):
        if self.__blkdat is not None: return self.__blkdat
        d=collections.defaultdict(list)
        self.__blkdat=d
        blkdat_input=self.timestr('{PARMhycom}/hafs_{RUNmodIDout}.{gridlabelout}.fcst.blkdat.input')
        with open(blkdat_input) as f:
            for line in f:
                m=re.match("^\s*(\S+)\s*'\s*(\S+)\s*' = ",line)
                if m:
                    (val,kwd)=m.groups(0)
                    val=float(val)
                    d[kwd].append(val)
        return self.__blkdat

    @property
    def baclin(self):
        return self.blkdat['baclin'][0]

    def archv2restart(self,logger):
        self.linkab('{FIXhycom}/hafs_{RUNmodIDout}.{gridlabelout}.regional.grid',
             'regional.grid')
        self.linkab('{FIXhycom}/hafs_{RUNmodIDout}.{gridlabelout}.regional.depth',
             'regional.depth')
        self.linkab('{FIXhycom}/hafs_{RUNmodIDout}.{gridlabelout}.regional.grid',
             'regional.grid')
        self.linkab('{FIXhycom}/hafs_{RUNmodIDout}.{gridlabelout}.regional.depth',
             'regional.depth')

        baclin=self.baclin

        self.linkab('hafs_basin.000','archv2r_in')
        if isnonempty('restart_forspin.b'):
            self.linkab('restart_forspin','restart_in')
        elif isnonempty('restart_pre.b'):
            self.linkab('restart_pre','restart_in')
        remove_file('archv2restart.in',info=True,logger=logger)
        remove_file('archv2restart.out',info=True,logger=logger)
        remove_file('restart_out.b',info=True,logger=logger)
        remove_file('restart_out.a',info=True,logger=logger)
        with open('archv2restart.in','wt') as arin:
            arin.write('''archv2r_in.b
restart_in.a
restart_out.a
20                 'iexpt '   = experiment number x10  (000=from archive file)
3                  'yrflag'   = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
%d                 'idm   '   = longitudinal array size
%d                 'jdm   '   = latitudinal  array size
2         'kapref'   = thermobaric reference state (-1 to 3, optional, default 0)
%d                 'kdm   '   = number of layers
34.0               'thbase' = reference density (sigma units)
%f                 'baclin' = baroclinic time step (seconds)
'''%( self.idm, self.jdm, self.kdm, baclin))

        cmd=( exe(self.getexe('hafs_archv2restart')) < 'archv2restart.in' )
        checkrun(cmd,logger=logger)


    def restart2restart(self,in_a,in_b,out_a,out_b,logger):
        cyc=self.conf.cycle
        with open('regional.grid.b','rt') as rgbf:
            idm_more=rgbf.readline().split()
            idmglobal=int(idm_more[0])
            jdm_more=rgbf.readline().split()
            jdmglobal=int(jdm_more[0])
        # Usage: hycom_subset fin.a idm jdm i1 j1 idms jdms fout.a

        ex=exe(self.getexe('hafs_restart2restart'))
        restart_out='restart.%s.%s'%(cyc.strftime("%Y%m%d%H"),self.gridlabelout)
        cmd=ex[in_a,idmglobal,jdmglobal,self.ic,self.jc,self.idm,self.jdm,
               restart_out+'.a'] > restart_out+'.b.one'
        checkrun(cmd,logger=logger)
        deliver_file(restart_out+'.a',out_a,keep=True,logger=logger)

        (nc,wcb,twc)=(38,341,339)
        with open(out_b,'wt') as routf:
            with open(in_b,'rt') as globalf:
                line1=globalf.readline().strip('\r\n')
                line2=globalf.readline().strip('\r\n')
                routf.write('%s\n%s\n'%(line1,line2))
                del line1, line2
                with open(restart_out+'.b.one','rt') as regionf:
                    go=True
                    while go:
                        gline=globalf.readline()
                        if not gline or len(gline)<38:
                            break
                        gline=gline.strip('\r\n')
                        rline=regionf.readline().strip('\r\n')
                        splat=rline.replace('min, max =','').split()
                        if len(splat)<2:
                            break
                        oline='%s %16.7E%16.7E\n'\
                            %(gline[0:38],float(splat[0]),float(splat[1]))
                        routf.write(oline)

# Hycominit2 creates forcing for coupled run
class HYCOMInit2(hafs.hafstask.HAFSTask):
    def remove_ocean(): self.uncouple()

    def __init__(self,dstore,conf,section,taskname=None,fcstlen=126,
                 **kwargs):
        super(HYCOMInit2,self).__init__(dstore,conf,section,
                                       taskname=taskname,**kwargs)
        self.fcstlen=fcstlen
        self.make_products()
        self.spinlength=self.confint('spinlength',0)
        self.ic=None
        self.jc=None
        self.idm=None
        self.jdm=None
        self.ijgrid=None
        self.Application=None
        self.__rtofs_inputs=None
        self.__rtofs_inputs_ymd=None
        self.__blkdat=None

    @property
    def rtofs_inputs(self):
        if self.__rtofs_inputs is None:
            hd=self.confstr('catalog','hafsdata')
            inputs=hafs.input.DataCatalog(self.conf,hd,self.conf.cycle)
            self.__rtofs_inputs=inputs
        return self.__rtofs_inputs

    def make_products(self):
        """Initializes all Product objects to make them available to
        future jobs."""
        # Add the HyCOM-specific products whose delivery location is
        # in COM with the standard output file prefix
        # (invest99l.2017110318).
        logger=self.log()

        # forcing files for coupled run
        self.forcing_products=dict()
        ffiles=['airtmp','precip','presur','radflx','shwflx','surtmp',
                'tauewd','taunwd','vapmix','wndspd','rivers']

        for ffile in ffiles:
            for ab in 'ab':
                file='forcing.%s.%s'%(ffile,ab)
                comf=self.confstrinterp('{com}/{out_prefix}.'+file)
                prod=FileProduct(self.dstore,file,self.taskname,location=comf)
                prod.location=comf
                self.forcing_products[file]=prod
                logger.debug('%s => %s (%s)'%(file,comf,repr(prod)))

        self.limits=FileProduct(
            self.dstore,'limits',self.taskname,location=
            self.confstrinterp('{com}/{out_prefix}.hafs.hycom.limits'))

        self.blkdat_input=FileProduct(
            self.dstore,'blkdat.input',self.taskname,location=
            self.confstrinterp('{com}/{out_prefix}.hafs.hycom.blkdat.input'))

    def make_forecast_forcing(self,logger):
        cyc=self.conf.cycle
        adjust_wind=0
        adjust_river=self.confint('adjust_river',0)
        adjust_temp=self.confint('adjust_temp',0)
        interval=self.confint('forecast_forcing_interval',3)
        startdate=self.conf.cycle
        fcstlen=self.fcstlen
        enddate=to_datetime_rel(fcstlen*3600,startdate)

        self.linkab('{FIXhycom}/hafs_{RUNmodIDout}.{gridlabelout}.regional.grid',
             'regional.grid')
        self.linkab('{FIXhycom}/hafs_{RUNmodIDout}.{gridlabelout}.regional.depth',
             'regional.depth')

        self.rtofs_get_forcing(startdate,enddate,interval*3600,adjust_river,
                               adjust_temp,adjust_wind,'fcst',logger)
        with open('limits','wt') as limitf:
            limitf.write('  %f %f false false  \n'%(
                    float(date_normal2hycom(cyc)),
                    float(date_normal2hycom(enddate))))

    def run(self):
        """Runs the hycom initialization for hycominit2.  Raises an exception if
        something goes wrong.  Returns on success."""
        logger=self.log()
        # Reset coupling status information:
        try:
            self.state=RUNNING
            # Inside the "with" block, we create and cd to the work
            # directory and then cd back out at the end.  The
            # "rm_first=True" means the directory is deleted first if
            # it already exists upon entry of the "with" block.
            with NamedDir(self.workdir,keep=not self.scrub,
                          logger=logger,rm_first=True) as d:
                self.select_domain(logger)

                # Create forcing for coupled run and raise an exception if it fails:
                self.make_forecast_forcing(logger)

                # Deliver the forcing files to com
               #for (name,prod) in self.forcing_products.items():
               #    prod.deliver(frominfo='./'+name)

                # Deliver the forcing files to intercom
                produtil.fileop.makedirs(self.timestr('{intercom}/hycominit'),logger=logger)
                for (name,prod) in self.forcing_products.items():
                    locintercom=self.timestr('{intercom}/hycominit/'+name)
                    deliver_file(name,locintercom,keep=True,logger=logger)

                # Deliver limits to com
               #self.limits.deliver(frominfo='./limits')

                # Deliver limits to intercom
                deliver_file('./limits',self.timestr('{intercom}/hycominit/limits'),keep=True,logger=logger)

               ## Make the flag file to indicate we're done.
               #done=self.timestr('{com}/{out_prefix}.hafs.hycominit2.done')
               #with open(done,'wt') as f:
               #    f.write('hycominit2 done for this cycle\n')

                # Make sure we run coupled:
                self.run_coupled=True
            self.state=COMPLETED
        except Exception as e:
            logger.error('Unhandled exception in ocean init: %s'
                         %(str(e),),exc_info=True)
            self.state=FAILED
            raise
        except:  # fatal signal, other catastrophic errors
            self.state=FAILED
            raise

    def select_domain(self,logger):
        hycom_domain=self.confstr('hycom_domain','small')
        basin=self.storminfo.pubbasin2
        Application=None
        if hycom_domain=='large' and basin in ['AL', 'EP', 'CP']:
            Application='nhc_basin'
        elif hycom_domain=='large' and basin in ['WP', 'IO']:
            Application='jtnh_basin'
        elif hycom_domain=='large' and basin in ['SH', 'SP', 'SI']:
            Application='jtsh_basin'
        elif basin=='AL':
            Application='hat10_basin'
        elif basin=='EP':
            Application='hep20_basin'
        elif basin=='CP':
            Application='hcp70_basin'
        elif basin=='WP':
            Application='hwp30_basin'
        elif basin=='IO':
            Application='hin40_basin'
        elif basin in [ 'SL', 'LS' ]:
            Application='hsn50_basin'
        elif basin in [ 'SH', 'SP', 'SI' ]:
            Application='hsp60_basin'
        else:
            msg='No ocean basin available for basin=%s lat=%s.  Run uncoupled.'%(
                basin,repr(atmos_lon))
            jlogger.warning(msg)
            raise hafs.exceptions.NoOceanBasin(msg)
        self.RUNmodIDin='rtofs_glo'
        RUNmodIDin=self.RUNmodIDin

        aptable=self.confstrinterp('{PARMhycom}/hafs_hycom.application_table')
        found=False
        with open(aptable,'rt') as apfile:
            for line in apfile:
                fields=line.split()
                #if len(fields)<9:
                if len(fields)<4:
                    logger.info('%s: ignore line %s'%(aptable,line.strip()))
                    continue
                #(ap,inmod,outmod,gridid,proc,np1,np2,mp1,mp2) = fields
                (ap,inmod,outmod,gridid) = fields
                if ap==Application and inmod==self.RUNmodIDin:
                    found=True
                    self.RUNmodIDout=outmod
                    self.gridid=gridid
                    found=True
        del line,apfile
        if not found:
            msg='%s: could not find Application=%s RUNmodIDin=%s'%(
                aptable,repr(Application),repr(self.RUNmodIDin))
            logger.error(msg)
            raise hafs.exceptions.InvalidOceanInitMethod(msg)

        gridtable=self.confstrinterp('{PARMhycom}/hafs_hycom.grid_table')
        found=False
        with open(gridtable,'rt') as gridfile:
            for line in gridfile:
                fields=line.split()
                if len(fields)<11:
                    logger.info('%s: ignore line %s'%(gridtable,line.strip()))
                    continue
                (ingridid,gridlabelin,gridlabelout,grid_source,\
                     idm,jdm,kdm,ic,jc,ijgrid,gridno) = fields
                idm=int(idm)
                jdm=int(jdm)
                kdm=int(kdm)
                ic=int(ic)
                jc=int(jc)
                ijgrid=int(ijgrid)
                gridno=int(gridno)
                if ingridid==self.gridid and line.find(RUNmodIDin)>=0:
                    found=True
                    ( self.idm, self.jdm, self.kdm, self.kkdm ) = \
                     (     idm,      jdm,      kdm,      kdm  )
                    ( self.ic, self.jc, self.ijgrid, self.gridno ) = \
                     (     ic,      jc,      ijgrid,      gridno )
                    ( self.gridlabelin, self.gridlabelout ) = \
                     (     gridlabelin,      gridlabelout )
        if not found:
            msg='%s: could not find grid=%s RUNmodIDin=%s'%(
                gridtable,repr(self.gridid),repr(RUNmodIDin))
            logger.error(msg)
            raise hafs.exceptions.InvalidOceanInitMethod(msg)
        assert(self.section=='hycominit2')
        self.conf.set(self.section,'RUNmodIDin',self.RUNmodIDin)
        self.conf.set(self.section,'gridlabelin',self.gridlabelin)
        self.conf.set(self.section,'gridlabelout',self.gridlabelout)
        self.conf.set(self.section,'RUNmodIDout',self.RUNmodIDout)
        with open('hycom_settings','wt') as f:
            f.write('''export idm={idm}
export jdm={jdm}
export kdm={kdm}
export kkdm={kkdm}
export ic={ic}
export jc={jc}
export ijgrid={ijgrid}
export gridlabelout={gridlabelout}
export gridlabelin={gridlabelin}
export RUNmodIDout={RUNmodIDout}
export RUNmodIDin={RUNmodIDin}
export gridno={gridno}\n'''.format(**self.__dict__))

        logger.info('HYCOM grid: i,j,kdm=%d,%d,%d ic,jc=%d,%d ijgrid=%d gridno=%d'%(
                idm,jdm,kdm,ic,jc,ijgrid,gridno))
        jlogger.info('HYCOM domain: in=%s.%s out=%s.%s'%(
                self.RUNmodIDin,self.gridlabelin,
                self.RUNmodIDout,self.gridlabelout))

    # getges1 - if cannot find file then go back to previous cycle
    def getges1(self,atmosds,grid,time):
        logger=self.log()
        sixhrs=to_timedelta(6*3600)
        epsilon=to_timedelta(30)
        atime0=self.conf.cycle
        atime=atime0
        rinput=self.rtofs_inputs
        glocset=0
        for itry in range(0,-10,-1):
            if time>atime+epsilon:

                gloc=rinput.locate(atmosds,grid,atime=atime,ftime=time)
                logger.info('Looking for: %s - %s'%(repr(itry),repr(gloc)))
                if glocset==0:
                   gloc0=rinput.locate(atmosds,grid,atime=atime,ftime=time)
                   glocset=1

                if isnonempty(gloc):
                    logger.info('%s %s %s => %s'%(
                            repr(atmosds),repr(grid),repr(time),
                            repr(gloc)))
                    return (gloc)
                if itry<=-9:
                    if wait_for_files([gloc],logger=logger,maxwait=60,sleeptime=5):
                       logger.info('%s %s %s => %s'%(
                            repr(atmosds),repr(grid),repr(time),
                            repr(gloc)))
                       return (gloc)
                    else:
                       logger.warning('%s : do not exist or empty'%(gloc))
            else:
                logger.warning('%s<=%s+%s'%(repr(time),repr(atime),repr(epsilon)))
            atime=atime-sixhrs
        msg='Cannot find file for time %s; first file tried %s'%(time.strftime('%Y%m%d%H'),gloc0)
        self.log().error(msg)
        raise hafs.exceptions.NoOceanData(msg)


# seasforce4 (init2) -
#              calls gfs2ofs in mpmd mode. There are 10 instances of gfs2ofs
#              and it creates 10 forcing files.
    def ofs_seasforce4(self,date1,date2,mode,logger):
        if mode=='anal':
            ihours=1
            atmosds=self.confstr('atmos1_dataset')
            atmos_flux=self.confstr('atmos1_flux')
            atmos_grid=self.confstr('atmos1_grid')
        else:
            ihours=3
            atmosds=self.confstr('atmos2_dataset')
            atmos_flux=self.confstr('atmos2_flux')
            atmos_grid=self.confstr('atmos2_grid')
        cyc=self.conf.cycle
        hourstep=to_timedelta(ihours*3600)
        epsilon=to_timedelta(30)
        nm=to_fraction(date2-date1)/(3600*ihours) + 3
        listflx=['%d\n'%int(round(nm))]
        rdate1=date1-hourstep
        rdate2=date2+hourstep
        stopdate=rdate2+epsilon

        wgrib2=alias(exe(self.getexe('wgrib2')))
        grb2index=alias(exe(self.getexe('grb2index')))
        wgrib2loc=self.getexe('wgrib2')
        grb2indexloc=self.getexe('grb2index')
        if os.path.isdir(grb2indexloc): 
            grb2indexloc=grb2indexloc+"/grb2index"

        if mode=='anal':
            TYPEx='hour fcst'
            TYPEx='ave'
        else:
            TYPEx='ave'

        fields=[
            {"FLUX":'UFLX',   "LEVEL":'surface',           "TYPE":'ave'   },
            {"FLUX":"VFLX",   "LEVEL":"surface",           "TYPE":"ave"   },
            {"FLUX":"TMP",    "LEVEL":'2 m above ground',  "TYPE":"fcst"  },
            {"FLUX":"SPFH",   "LEVEL":'2 m above ground',  "TYPE":"fcst"  },
            {"FLUX":"PRATE",  "LEVEL":"surface",           "TYPE":"ave"   },
            {"FLUX":"UGRD",   "LEVEL":'10 m above ground', "TYPE":"fcst"  },
            {"FLUX":"VGRD",   "LEVEL":'10 m above ground', "TYPE":"fcst"  },
            {"FLUX":"SHTFL",  "LEVEL":"surface",           "TYPE":TYPEx   },
            {"FLUX":"LHTFL",  "LEVEL":"surface",           "TYPE":TYPEx   },
            {"FLUX":"DLWRF",  "LEVEL":"surface",           "TYPE":TYPEx   },
            {"FLUX":"ULWRF",  "LEVEL":"surface",           "TYPE":TYPEx   },
            {"FLUX":"DSWRF",  "LEVEL":"surface",           "TYPE":TYPEx   },
            {"FLUX":"USWRF",  "LEVEL":"surface",           "TYPE":TYPEx   },
            {"FLUX":"TMP",    "LEVEL":"surface",           "TYPE":"fcst"  },
            {"FLUX":"PRES",   "LEVEL":"surface",           "TYPE":"fcst"  },
            {"FLUX":"LAND",   "LEVEL":"surface",           "TYPE":"fcst"  },
            {"FLUX":"PRMSL",  "LEVEL":"sea level",         "TYPE":"fcst"  }
            ]

        datei=rdate1
        inputs=self.rtofs_inputs
        cmd=self.getexe('hafs_gfs2ofsinputs.py')
        commands=list()
        while datei<stopdate:
            sf=self.getges1(atmosds,atmos_grid,datei)
            flxfile=datei.strftime("%Y%m%d%H")+'.sfcflx'
            remove_file(flxfile,info=True,logger=logger)

            # Subset flux file:
            make_symlink(sf,flxfile+'.in2',logger=logger,force=True)

#START
#           sfindex=runstr(wgrib2[flxfile+'.in2'],logger=logger)
#           reindex=''
#           for flt in fields:
#               for line in sfindex.splitlines():
#                   if line.find(flt['FLUX']) >=0 and \
#                      line.find(flt['LEVEL']) >=0 and \
#                      line.find(flt['TYPE']) >=0:
#                       reindex+=line+'\n'
#                       logger.info('%s: keep(sf): %s'%(
#                               flxfile+'.in2',line.strip()))
#           logger.info('KEEP(SF):\n'+reindex)
#           checkrun(wgrib2[flxfile+'.in2',"-i",'-grib',flxfile+'.in3']
#                    << reindex,logger=logger)
#           checkrun(wgrib2[flxfile+'.in3',"-new_grid_winds","earth",
#              "-new_grid","gaussian","0:1440:0.25","89.75:720",flxfile+'.in4']
#              ,logger=logger)
#           deliver_file(flxfile+'.in4',flxfile,keep=True,logger=logger)
#           checkrun(grb2index[flxfile,flxfile+'.idx'])
#END

            listflx.append('%s %s %s %s %s\n'%(
                    datei.strftime('%Y%m%d%H'),flxfile,'none',sf,'none'))
            g2oinputs_out='gfs2outputs.%s.out'%datei.strftime('%Y%m%d%H')
            commands.append('%s %s %s %s %s < /dev/null > %s 2>&1\n'%(
                    cmd,mode,flxfile,wgrib2loc,grb2indexloc,g2oinputs_out))

            datei+=hourstep
        # end while datei<stopdate

        with open('command.file.preview','wt') as cfpf:
            cfpf.write(''.join(commands))

        clustername=produtil.cluster.name()
        tt=int(os.environ['TOTAL_TASKS'])
        logger.info ('CALLING gfs2ofsinputs %d ',tt)
        if clustername in ('cactus','dogwood'):
            cfp_path=produtil.fileop.find_exe('cfp')
            cmd2=mpirun(mpi(cfp_path)['./command.file.preview'],allranks=True)
        else:
            mpiserial_path=os.environ.get('MPISERIAL','*MISSING*') 
            if mpiserial_path=='*MISSING*':
                mpiserial_path=self.getexe('mpiserial','*MISSING*')
            if mpiserial_path=='*MISSING*':
                mpiserial_path=produtil.fileop.find_exe('mpiserial')
            cmd2=mpirun(mpi(mpiserial_path)['-m','command.file.preview'],allranks=True)
        checkrun(cmd2)

        with open('listflx.dat','wt') as listflxf:
            listflxf.write(''.join(listflx))
        with open('intp_pars.dat','wt') as ippdf:
            ippdf.write('''
&intp_pars
avstep = %d.,      ! averaging fluxes (in hours)
mrffreq = %d.,     ! frequency of MRF fluxes (in hours)  = mrffreq for no averaging
flxflg = 15,       ! Type of HYCOM input (=4 => nhycom=7 and =5 => nhycom=8)
dbgn = 0,         ! debugging =0 - no dbg; =1,2,3 - add output
avg3 = 2,         ! if avg3 = 1, then averaged fluxes are converted to instantaneous fields
wslocal = 0       ! if  wslocal = 1, then wind stress are computed from wind velcoities
/
''' %(ihours,ihours))
# echo "8 8 0 0 8 0 0 0 0 8 8 8 8 0 0 0" > jpdt_table.dat
        with open('jpdt_table.dat','wt') as j:
            j.write('''8 8 0 0 8 0 0 0 0 8 8 8 8 0 0 0''')
        cmd=self.getexe('hafs_gfs2ofs2')
        tt=int(os.environ['TOTAL_TASKS'])
        logger.info ('CALLING gfs2ofs2 %d ',tt)
        commands=list()         
        for i in range(1,11):   
            gfs2ofs_out='gfs2ofs.%d.out'%i  
            commands.append('%s %d > %s 2>&1\n'%(cmd,i,gfs2ofs_out))
        with open('command.file.preview_gfs2ofs','wt') as fid:
            fid.write(''.join(commands))
        if clustername in ('cactus','dogwood'):
            cfp_path=produtil.fileop.find_exe('cfp')
            cmd2=mpirun(mpi(cfp_path)['./command.file.preview_gfs2ofs'],allranks=True)
        else:
            mpiserial_path=os.environ.get('MPISERIAL','*MISSING*')  
            if mpiserial_path=='*MISSING*':
                 mpiserial_path=self.getexe('mpiserial','*MISSING*')
            if mpiserial_path=='*MISSING*': 
                 mpiserial_path=produtil.fileop.find_exe('mpiserial')
            cmd2=mpirun(mpi(mpiserial_path)['-m','command.file.preview_gfs2ofs'],allranks=True)
        checkrun(cmd2)
        self.ofs_timeinterp_forcing(logger)

    def ofs_forcing_info(self,filename):
        num_times=0
        blines=0
        aname=None
        with open(filename,'rt') as inf:
            start=inf.tell()
            for line in inf:
                if aname is None and line.find('span')>=0:
                    aname=line[0:10]
                    break
            if aname is None:
                msg='%s: could not find data records in file.'%(filename,)
                raise hafs.exceptions.OceanDataInvalid(msg)
            inf.seek(start)
            for line in inf:
                blines+=1
                if line.find(aname):
                    num_times+=1
        return (blines,num_times,blines-num_times,aname)

    def ofs_timeinterp_forcing(self,logger):
        with produtil.cd.NamedDir('temp',logger=logger,rm_first=True):
            for ofield in [ 'airtmp','precip','presur','radflx','shwflx',
                            'surtmp','tauewd','taunwd','vapmix','wndspd']:
                for ab in 'ab':
                    ffrom='../forcing.'+ofield+'.'+ab
                    if os.path.exists(ffrom):
                        fto='forcing.'+ofield+'.'+ab
                        logger.info('Move %s => %s'%(ffrom,fto))
                        os.rename(ffrom,fto)
        # End moving of old forcing to temp/

        ofs_timeinterp_forcing=alias(mpirun(mpi(self.getexe(
                    'hafs_timeinterp_forcing'))))

        xfield=[
            # First step, precip => [airtemp, ...]
            ['precip',['airtmp','presur','surtmp','vapmix','wndspd'] ],
            # Second step: airtmp => [precip, ...]
            ['airtmp',['precip','tauewd','taunwd','radflx','shwflx'] ]
            ]

        for ifield,ofields in xfield:
            ifile='temp/forcing.%s.b'%(ifield,)
            (_,num_times,ihead,_)=self.ofs_forcing_info(ifile)

            for ofield in ofields:
                tfile='timeinterp_forcing.%s.in'%(ofield,)
                ofile='temp/forcing.%s.b'%(ofield,)
                (_,num_frames,ohead,sname)=self.ofs_forcing_info(ofile)
                # wrong order: inputs=[ifield,ihead,num_times,ofield,ohead,num_frames,sname]
                inputs=[ifield,num_times,ihead,ofield,num_frames,ohead,sname]
                inputs=[str(i) for i in inputs]
                inputs='\n'.join(inputs) + '\n'
                logger.info('%s: write %s'%(tfile,repr(inputs)))
                with open(tfile,'wt') as tf:
                    tf.write(inputs)
                checkrun(ofs_timeinterp_forcing<tfile,logger=logger)
        # end for ifield,ofields in xfield

    def ofs_correct_forcing(self,logger):
        (_,num_frames,ihead,sname)=self.ofs_forcing_info('forcing.surtem.b')
        (_,_,_,aname)=self.ofs_forcing_info('forcing.airtmp.b')
        inputs=[ihead,num_frames,aname,sname]
        inputs='\n'.join([str(i) for i in inputs]) + '\n'
        with open('correct_forcing.in','wt') as cfif:
            cfif.write(inputs)
        self.moveab('forcing.airtmp','forcing.airtm1')
        hafs_correct_forcing=self.getexe('hafs_correct_forcing')
        checkrun(exe(hafs_correct_forcing)<'correct_forcing.in',logger=logger)

    def blkflag(self,flagname,default=NO_DEFAULT):
        blkdat=self.blkdat
        if  default is not NO_DEFAULT and \
                flagname not in blkdat:
            return default
        return abs(self.blkdat[flagname][0])>.01

    def rtofs_get_forcing(self,srtdate,enddate,intvl,adjust_river,
                          adjust_temp,adjust_wind,mode,logger):
        priver=self.blkflag('priver',False)
        flxflg=self.blkflag('flxflg',False)
        wndflg=self.blkflag('wndflg',False)
        atpflg=self.blkflag('atpflg',False)
        logger.info('ANOTHER STRING FOR WHICH TO GREP - mode=%s adjust_temp=%s adjust_wind=%s '%(
        repr(mode),repr(adjust_river),repr(adjust_wind)))
        forcingfilesmade=-1

        # Atmospheric forcing
        if not flxflg and not wndflg and not atpflg:
            logger.info('No atmospheric forcing created')
        else:
            forcingfilesmade=0
            logger.info('Create atmospheric forcing.')
            deliver_file(self.icstr('{FIXhycom}/ismus_msk1760x880.dat'),'ismus_msk1760x880.dat',keep=True,logger=logger)
            deliver_file(self.icstr('{FIXhycom}/ismus_msk3072x1536.dat'),'ismus_msk3072x1536.dat',keep=True,logger=logger)
            deliver_file(self.icstr('{FIXhycom}/ismus_msk1440x721.dat'),'ismus_msk1440x721.dat',keep=True,logger=logger)
            deliver_file(self.icstr('{FIXhycom}/ismus_msk1440x720.dat'),'ismus_msk1440x720.dat',keep=True,logger=logger)
            self.ofs_seasforce4(srtdate,enddate,mode,logger)
            if adjust_temp:
                self.ofs_correct_forcing(logger)
        # end if not flxflg and not wndflg and not atpflg

        # River forcing:
        if priver>0:
            self.copyab('{FIXhycom}/hafs_{RUNmodIDout}.{gridlabelout}.forcing.'
                   'rivers','forcing.rivers')
            logger.info('Station River Forcing Files copied')
            forcingfilesmade=0
        else:
            logger.info('River Forcing Files are not employed')

        # Wind forcing:
        if adjust_wind>0:
            with open('../tmpvit','rt') as t:
                splat=t.readline().split()
                depth=splat[18]
            if depth=='S':
                logger.info('Hurricane too shallow to run parameterized winds')
            else:
                self.ofs_windforcing(logger)
                if adjust_wind>1:
                    renameme=[
                        ['forcing.wndspd.a', 'forcing.wndspd.a.original'],
                        ['forcing.wndspd.b', 'forcing.wndspd.b.original'],
                        ['forcing.tauewd.a', 'forcing.tauewd.a.original'],
                        ['forcing.tauewd.b', 'forcing.tauewd.b.original'],
                        ['forcing.taunwd.a', 'forcing.taunwd.a.original'],
                        ['forcing.taunwd.b', 'forcing.taunwd.b.original'],
                        ['forcing.wdspd1.a', 'forcing.wndspd.a'],
                        ['forcing.wdspd1.b', 'forcing.wndspd.b'],
                        ['forcing.tauew1.a', 'forcing.tauewd.a'],
                        ['forcing.tauew1.b', 'forcing.tauewd.b'],
                        ['forcing.taunw1.a', 'forcing.taunwd.a'],
                        ['forcing.taunw1.b', 'forcing.taunwd.b'],
                        ]
                    for (ifrom,ito) in renameme:
                        logger.info('Rename %s to %s'%(ifrom,ito))
                        os.rename(ifrom,ito)

        return forcingfilesmade

    def copyab(self,sfrom,fto):
        ffrom=self.timestr(sfrom)
        produtil.fileop.deliver_file(
            ffrom+'.a',fto+'.a',keep=True,logger=self.log())
        produtil.fileop.deliver_file(
            ffrom+'.b',fto+'.b',keep=True,logger=self.log())

    def moveab(self,ffrom,fto):
        deliver_file(ffrom+'.a',fto+'.a',keep=False,logger=self.log())
        deliver_file(ffrom+'.b',fto+'.b',keep=False,logger=self.log())

    def linkab(self,sfrom,fto):
        ffrom=self.timestr(sfrom)
        produtil.fileop.make_symlink(
            ffrom+'.a',fto+'.a',force=True,logger=self.log())
        produtil.fileop.make_symlink(
            ffrom+'.b',fto+'.b',force=True,logger=self.log())

    @property
    def blkdat(self):
        if self.__blkdat is not None: return self.__blkdat
        d=collections.defaultdict(list)
        self.__blkdat=d
        blkdat_input=self.timestr('{PARMhycom}/hafs_{RUNmodIDout}.{gridlabelout}.fcst.blkdat.input')
        with open(blkdat_input) as f:
            for line in f:
                m=re.match("^\s*(\S+)\s*'\s*(\S+)\s*' = ",line)
                if m:
                    (val,kwd)=m.groups(0)
                    val=float(val)
                    d[kwd].append(val)
        return self.__blkdat

    @property
    def baclin(self):
        return self.blkdat['baclin'][0]

class HYCOMPost(hafs.hafstask.HAFSTask):

    """Runs the ocean post-processor on the HyCOM output, in parallel
    with the model."""
    def __init__(self,dstore,conf,section,fcstlen=126,**kwargs):
        super(HYCOMPost,self).__init__(dstore,conf,section,**kwargs)
        self.fcstlen=fcstlen

    def run(self):
      """Called from the ocean post job to run the HyCOM post."""
      logger=self.log()
      self.state=RUNNING

      #produtil.fileop.chdir('WORKhafs')
      with NamedDir(self.workdir,keep=not self.scrub, logger=logger,rm_first=True) as d:

        fcstlen=self.fcstlen
        FIXhycom=self.timestr('{FIXhycom}')
        PARMhycom=self.timestr('{PARMhycom}')
        opn=self.timestr('{out_prefix_nodate}')

        # get hycom subdomain specs from hycom_settings file
        hycomsettingsfile=self.timestr('{com}/{out_prefix}.hafs.hycom_settings')
        with open (hycomsettingsfile,'rt') as hsf:
          for line in hsf:
            m=re.match('^export idm=(.*)$',line)
            if m:
              idm=int(m.groups()[0])
            m=re.match('^export jdm=(.*)$',line)
            if m:
              jdm=int(m.groups()[0])
            m=re.match('^export kdm=(.*)$',line)
            if m:
              kdm=int(m.groups()[0])
            m=re.match('^export gridlabelout=(.*)$',line)
            if m:
               gridlabelout=m.groups()[0]
            m=re.match('^export RUNmodIDout=(.*)$',line)
            if m:
               RUNmodIDout=m.groups()[0]
        logger.info('POSTINFO- idm=%d jdm=%d kdm=%d gridlabelout=%s RUNmodIDout=%s '%(idm,jdm,kdm,gridlabelout,RUNmodIDout))

        ffrom='%s/hafs_%s.%s.regional.grid'%(FIXhycom,RUNmodIDout,gridlabelout)
        produtil.fileop.make_symlink(
            ffrom+'.a','regional.grid.a',force=True,logger=self.log())
        produtil.fileop.make_symlink(
            ffrom+'.b','regional.grid.b',force=True,logger=self.log())
        ffrom='%s/hafs_%s.%s.regional.depth'%(FIXhycom,RUNmodIDout,gridlabelout)
        produtil.fileop.make_symlink(
            ffrom+'.a','regional.depth.a',force=True,logger=self.log())
        produtil.fileop.make_symlink(
            ffrom+'.b','regional.depth.b',force=True,logger=self.log())

        ## Convert hafs_basin.000.[ab] to NetCDF
        logger.info('Convert hafs_basin.000.[ab] to NetCDF')
        stime=self.conf.cycle
        navtime=0
        archtime=to_datetime_rel(navtime*3600,stime)
        archtimestring=archtime.strftime('%Y_%j_%H')
        atime=to_datetime(self.conf.cycle)
        ftime=to_datetime_rel(0*3600,atime)
        prodnameA=self.timestr('hafs_basin.{fahr:03d}.a',ftime,atime)
       #filepathA=self.timestr('{com}/{out_prefix}.{pn}',pn=prodnameA)
        filepathA=self.timestr('{intercom}/hycominit/{pn}',pn=prodnameA)
        prodnameB=self.timestr('hafs_basin.{fahr:03d}.b',ftime,atime)
       #filepathB=self.timestr('{com}/{out_prefix}.{pn}',pn=prodnameB)
        filepathB=self.timestr('{intercom}/hycominit/{pn}',pn=prodnameB)
        if not os.path.exists(filepathA):
            logger.error('Cannot find file %s - exiting'%(filepathA))
            raise
        if not os.path.exists(filepathB):
            logger.error('Cannot find file %s - exiting'%(filepathB))
            raise
        logger.info('Will create ocean products for %s '%(filepathA))
        archxa='archv.a'
        archxb='archv.b'
        produtil.fileop.make_symlink(filepathA,archxa,force=True,logger=logger)
        produtil.fileop.make_symlink(filepathB,archxb,force=True,logger=logger)
        with open('topzfile','wt') as inf:
            inf.write("""%s
NetCDF
%d         'yyyy'   = year
%d         'month ' = month
%d         'day   ' = day
%d         'hour  ' = hour
%d         'verfhr' = verification hour
""" %(archxb,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),navtime))
        logger.info('Do Volume HERE')
        #concat topzinfile and parmfile
        parmfile='%s/hafs_hycom.archv2data_3z.in'%(PARMhycom)
        filenames = ['topzfile',parmfile]
        with open('tempinfile', 'w') as iff:
            for fname in filenames:
                with open(fname) as filen:
                    iff.write(filen.read())
        #replace idm,jdm,kdm
        replacements={'&idm':repr(idm),'&jdm':repr(jdm),'&kdm':repr(kdm)}
        with open ('tempinfile') as inf:
            with open ('infile','w') as outf:
                for line in inf:
                    for src,targ in replacements.items():
                        line=line.replace(src,targ)
                    outf.write(line)
       #outfile='%s.%04d%02d%02d%02d.hafs_%s_3z.f%03d'%(opn,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),RUNmodIDout,navtime)
        outfile='%s.%04d%02d%02d%02d.hafs.hycom.3z.f%03d'%(opn,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),navtime)
        outfileNC=outfile+'.nc'
        archv2data=alias(exe(self.getexe('hafs_archv3z2nc')).env(CDF051=outfileNC))
        checkrun(archv2data<'infile',logger=logger)
        deliver_file(outfileNC,self.icstr('{com}/'+outfileNC),keep=False,logger=logger)

        navtime=3
        epsilon=.1
        while navtime<fcstlen+epsilon:
            archtime=to_datetime_rel(navtime*3600,stime)
            archtimestring=archtime.strftime('%Y_%j_%H')
            if archtime.hour in [0, 6, 12, 18]:
                # convert archv.[ab] to .nc
                notabin='archv.%s'%(archtimestring)
                logfile=''.join([notabin,'.txt'])
                timesslept=0
                sleepmax=180
                while timesslept<sleepmax:
                    if os.path.exists("../../forecast/"+logfile):
                        break
                    else:
                        timesslept=timesslept+1
                        logger.warning('Cannot find file %s %d times'%( repr(logfile),timesslept))
                        time.sleep(10)
                if timesslept>=sleepmax:
                    logger.error('Cannot find file %s %d times - exiting'%( repr(logfile),timesslept))
                    raise
                logger.info('Will create ocean products for %s '%( repr(notabin)))
                afile=''.join(['../../forecast/'+notabin,'.a'])
                bfile=''.join(['../../forecast/'+notabin,'.b'])
                archxa='archv.a'
                archxb='archv.b'
                produtil.fileop.make_symlink(afile,archxa,force=True,logger=logger)
                produtil.fileop.make_symlink(bfile,archxb,force=True,logger=logger)
                with open('topzfile','wt') as inf:
                    inf.write("""%s
NetCDF
%d         'yyyy'   = year
%d         'month ' = month
%d         'day   ' = day
%d         'hour  ' = hour
%d         'verfhr' = verification hour
""" %(archxb,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),navtime))
                logger.info('Do Volume HERE')
                ## volume_3z to NetCDF
                #concat topzinfile and parmfile
                parmfile='%s/hafs_hycom.archv2data_3z.in'%(PARMhycom)
                filenames = ['topzfile',parmfile]
                with open('tempinfile', 'w') as iff:
                    for fname in filenames:
                        with open(fname) as filen:
                            iff.write(filen.read())
                #replace idm,jdm,kdm
                replacements={'&idm':repr(idm),'&jdm':repr(jdm),'&kdm':repr(kdm)}
                with open ('tempinfile') as inf:
                    with open ('infile','w') as outf:
                        for line in inf:
                            for src,targ in replacements.items():
                                line=line.replace(src,targ)
                            outf.write(line)
               #outfile='%s.%04d%02d%02d%02d.hafs_%s_3z.f%03d'%(opn,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),RUNmodIDout,navtime)
                outfile='%s.%04d%02d%02d%02d.hafs.hycom.3z.f%03d'%(opn,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),navtime)
                outfileNC=outfile+'.nc'
                archv2data=alias(exe(self.getexe('hafs_archv2data3z')).env(CDF051=outfileNC))
                checkrun(archv2data<'infile',logger=logger)
                deliver_file(outfileNC,self.icstr('{com}/'+outfileNC),keep=False,logger=logger)
                # deliver ab files to comout
               #notabout='hafs_%s.%s'%(RUNmodIDout,archtimestring)
               #deliver_file('../../forecast/'+notabin+'.a',self.icstr('{com}/{out_prefix}.'+notabout+'.a',keep=True,logger=logger))
               #deliver_file('../../forecast/'+notabin+'.b',self.icstr('{com}/{out_prefix}.'+notabout+'.b',keep=True,logger=logger))

            # convert 3-hrly archs.[ab] to .nc
            notabin='archs.%s'%(archtimestring)
            logfile=''.join([notabin,'.txt'])
            timesslept=0
            sleepmax=180
            while timesslept<sleepmax:
               if os.path.exists("../../forecast/"+logfile):
                  break
               else:
                  timesslept=timesslept+1
                  logger.warning('Cannot find file %s %d times'%( repr(logfile),timesslept))
                  time.sleep(10)
            if timesslept>=sleepmax:
               logger.error('Cannot find file %s %d times - exiting'%( repr(logfile),timesslept))
               raise
            logger.info('Will create ocean products for %s '%( repr(notabin)))
            afile=''.join(['../../forecast/'+notabin,'.a'])
            bfile=''.join(['../../forecast/'+notabin,'.b'])
            archxa='archs.a'
            archxb='archs.b'
            produtil.fileop.make_symlink(afile,archxa,force=True,logger=logger)
            produtil.fileop.make_symlink(bfile,archxb,force=True,logger=logger)
            with open('topzfile','wt') as inf:
                inf.write("""%s
NetCDF
%d         'yyyy'   = year
%d         'month ' = month
%d         'day   ' = day
%d         'hour  ' = hour
%d         'verfhr' = verification hour
""" %(archxb,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),navtime))
            logger.info('Do SURFACE HERE')
            ## surface_2d to NetCDF
            #concat topzinfile and parmfile
            parmfile='%s/hafs_hycom.archv2data_2d.in'%(PARMhycom)
            filenames = ['topzfile',parmfile]
            with open('tempinfile', 'w') as iff:
                for fname in filenames:
                    with open(fname) as filen:
                        iff.write(filen.read())
            #replace idm,jdm,kdm
            replacements={'&idm':repr(idm),'&jdm':repr(jdm),'&kdm':repr(kdm)}
            with open ('tempinfile') as inf:
                with open ('infile','w') as outf:
                    for line in inf:
                        for src,targ in replacements.items():
                            line=line.replace(src,targ)
                        outf.write(line)
           #outfile='%s.%04d%02d%02d%02d.hafs_%s_2d.f%03d'%(opn,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),RUNmodIDout,navtime)
            outfile='%s.%04d%02d%02d%02d.hafs.hycom.2d.f%03d'%(opn,int(stime.year),int(stime.month),int(stime.day),int(stime.hour),navtime)
            outfileNC=outfile+'.nc'
            archv2data=alias(exe(self.getexe('hafs_archv2data2d')).env(CDF001=outfileNC))
            checkrun(archv2data<'infile',logger=logger)
            deliver_file(outfileNC,self.icstr('{com}/'+outfileNC),keep=False,logger=logger)

            # this is the frequency of files
            navtime+=3
        logger.info('finishing up here')
        return
