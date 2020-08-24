
"""!Obtains input data needed by various subclasses of
hafs.hafstask.HAFSTask.

This module implements the functionality described in
hafs.hafstask.HAFSTask.inputiter().  It takes many HAFS tasks, asks
them what input is needed and collates that information.  It has a
list of many possible input sources, and knows which ones are
available from which cluster.  It goes through available input sources
in priority order, obtaining the input data."""

##@var __all__
# Symbols exported by "from hafs.input import *"
__all__=["DataCatalog","InputSource",'in_date_range']

import collections, os, ftplib, tempfile, configparser, urllib.parse, stat, \
    re, threading, time, datetime, io
import produtil.run, produtil.cluster, produtil.fileop, produtil.cd, \
    produtil.workpool, produtil.listing
import tcutil.numerics, hafs.exceptions

from produtil.run import alias, batchexe, checkrun, ExitStatusException, run
from produtil.fileop import deliver_file, isnonempty, make_symlink, makedirs
from tcutil.numerics import to_datetime, to_datetime_rel, to_timedelta
from hafs.exceptions import InputSourceBadType,PartialTransfer,\
    UnsupportedTransfer
from produtil.log import jlogger

########################################################################
def in_date_range(t,trange):
    """!Is this time in the given time range?

    @param t A time as a ten digit number.  For example, 1830123118 is
      December 31, 1830 at 18:00 UTC.
    @param trange A comma-separated list of time ranges such as
    this:
    * 2019081412 --- 12:00 UTC on August 14, 2019
    * 2019081412-2019082318 --- From 12:00 UTC on August 14, 2019
      through 18:00 UTC on August 23, 2019
    * 2019081412-2019082318,2011010100-2011123123 --- From 12:00 UTC
      on August 14, 2019 through 18:00 UTC on August 23, 2019 and all
      of year 2011.
    @returns True if t falls in the range trange, or False otherwise."""
    epsilon=to_timedelta('1800') # epsilon = one half hour
    t=to_datetime(t)
    for tr in trange.split(','):
        idash=tr.find('-')
        if idash<0:
            # single date
            start=to_datetime(tr)
            if t>=to_datetime_rel(-epsilon,start) \
                    and t<=to_datetime_rel(epsilon,start):
                return True
        else:
            # date range
            start=to_datetime(tr[0:10])
            end=to_datetime(tr[idash+1:idash+11])
            if t>=to_datetime_rel(-epsilon,start) \
                    and t<=to_datetime_rel(epsilon,end):
                return True
    return False


########################################################################
def tempopen(f,m):
    """!Convenience function that opens a temporary file using
    tempfile.NamedTemporaryFile."""
    produtil.fileop.makedirs(os.path.dirname(f))
    return tempfile.NamedTemporaryFile(prefix=os.path.basename(f),
                                  dir=os.path.dirname(f),
                                  mode=m,suffix='.tmp',delete=False)

########################################################################

def strsrc(d):
    """!Makes a string version of a dataset+item dict as produced by
    hafs.hafstask.HAFSTask.inputiter()"""
    s=io.StringIO()
    s.write("%s(%s"%(d.get("dataset","(**no*dataset**)"),
                     d.get("item","(**no*item**)")))
    for k in sorted(list(d.keys())):
        if k=='dataset' or k=='item': continue
        v=d[k]
        if isinstance(v,datetime.datetime):
            s.write(', %s=%s'%(str(k),v.strftime('%Y-%m-%d_%H:%M:%S')))
        else:
            s.write(', %s=%s'%(str(k),repr(v)))
    s.write(')')
    return s.getvalue()

########################################################################

def strsrc(d):
    """!Makes a string version of a dataset+item dict as produced by
    hafs.hafstask.HAFSTask.inputiter()"""
    s=io.StringIO()
    s.write("%s(%s"%(d.get("dataset","(**no*dataset**)"),
                     d.get("item","(**no*item**)")))
    for k in sorted(list(d.keys())):
        if k=='dataset' or k=='item': continue
        v=d[k]
        if isinstance(v,datetime.datetime):
            s.write(', %s=%s'%(str(k),v.strftime('%Y-%m-%d_%H:%M:%S')))
        else:
            s.write(', %s=%s'%(str(k),repr(v)))
    s.write(')')
    return s.getvalue()

########################################################################
class DataCatalog(object):
    """!Provides the location of a file in an archive, on disk or on a
    remote server via sftp or ftp.

    This class is a collection of functions that know how to provide
    the location of a file in either an archive or a filesystem.  It
    does not know how to actually obtain the file.  This serves as the
    underlying "where is that file" implementation of InputSource.
    All of this is driven by a section in an hafs.config.HAFSConfig
    object.

    For example, suppose one set up this configuration file:
    @code{.conf}
      [wcoss_fcst_nco]
      # WCOSS: Input locations for the production HAFS
      gfs               = /com/gfs/prod/gfs.{aYMD}/
      gdas1             = /com/gfs/prod/gdas.{aYMD}/
      gfs_sf            = gfs.t{aHH}z.sf{fahr:02d}
      gfs_sfcanl        = gfs.t{aHH}z.sfcanl
      gdas1_bufr        = gdas1.t{aHH}z.{obstype}.tm00.bufr_d
    @endcode

    In this example, "gfs" is a dataset, while "gfs_sfcanl" is an item
    in the dataset.  The DataCatalog.locate() function can find the
    location of a gfs_sf file given the inputs required for string
    expansion by hafs.config.HAFSConfig.timestrinterp().  In this
    case, only the analysis time is required for the "{aYMD}" in the
    dataset location and "{aHH}" in the gfs_sfcanl filename.
    @code{.py}
      dc=DataCatalog(conf,"wcoss_fcst_nco","2019091800")
      sfcanl=dc.locate("gfs","gfs_sfcanl")
      print sfcanl
    @endcode
    That code would print "/com/gfs/prod/gfs.20190818/gfs.t00z.sfcanl"
    which is the operational output path of the GFS surface analysis
    file for the analysis time in question.

    Suppose we wanted the spectral forecast file, "gfs_sf" instead,
    for forecast hour 54.  That also requires the forecast time
    ("ftime") in order to fill in the "{fahr:02d}" in the filename
    with the number 54.
    @code{.py}
      dc=DataCatalog(conf,"wcoss_fcst_nco","2019091800")
      sf48a=dc.locate("gfs","gfs_sf",ftime="2019092006")
      sf48b=dc.locate("gfs","gfs_sf",ftime=48*3600)
      print sf48a
      print sf48b
    @endcode
    That code would print "/com/gfs/prod/gfs.20190818/gfs.t00z.sf54"
    twice.  Note that you can specify the forecast time as an absolute
    time, or as a number of seconds relative to the analysis time and
    achieve the same effect either way.

    If we want the bufr file, we have to provide one more piece of
    information: the observation type, to fill in "{obstype}".
    @code{.py}
      dc=DataCatalog(conf,"wcoss_fcst_nco","2019091800")
      gpm=dc.locate("gdas1","gdas1_bufr",obstype="gpm")
      print gpm
    @endcode
    which prints "/com/gfs/prod/gdas.20190918/gdas1.t00z.gpm.tm00.bufr_d"
    """
    def __init__(self,conf,section,anltime):
        """!DataCatalog constructor
        @param conf the configuration object, an hafs.config.HAFSConfig
        @param section the section that provides location information
        @param anltime the default analysis time        """
        self.conf=conf
        if not isinstance(section,str):
            raise TypeError('In DataCatalog.__init__, section must be a '
                            'string.')
        self.section=section
        self.anltime=to_datetime(anltime)
    ##@var section
    # The section used for dataset and item locations in conf.

    ##@var conf
    # The configuration object, an hafs.config.HAFSConfig or subclass.

    ##@var anltime
    # The default analysis time for parse() and locate() if none is
    # specified.

    def __repr__(self):
        """!A string representation of this DataCatalog"""
        if isinstance(self.anltime,datetime.datetime):
            stime=self.anltime.strftime('%Y%m%d%H')
        else:
            stime=str(self.anltime)
        return "DataCatalog(conf,%s,%s)"%(repr(self.section), stime)
    def rt_updated(self):
        """!Is this dataset updated in real-time?

        @returns True if this dataset is updated in real-time, False
        otherwise.  By default, this will return True if
        conf[section,"rt_updated"] is set to "yes" or False otherwise."""
        try:
            return conf.getbool(section,'rt_updated',False)
        except ( configparser.Error,KeyError,TypeError,ValueError ) as e:
            return False
    def parse(self,string,atime=None,ftime=None,logger=None,dates=None,
              **kwargs):
        """!Internal function that performs string interpolation.

        This is an internal implementation function that you should
        not call directly.  It performs string interpolation using the
        underlying conf object.  This acts exactly like the expansions
        done in the hafs.conf file: {stuff} is expanded to the
        contents of the "stuff" variable.  Expansions are done in the
        section specified in the constructor.  In addition, various a*
        and f* variables are expanded based on the analysis time
        ("atime") and forecast time ("ftime").  See
        hafs.config.HAFSConfig.timestrinterp() for details.
        @param string the string being expanded
        @param atime Optional: the analysis time.  The default is self.anltime
        @param ftime Optional: the forecast time.
        @param logger Optional: a logging.Logger for log messages
        @param dates Optional: dates for which this datasource is valid.
          This is passed to in_date_range() for validation.  This is
          used to implement the InputSource date ranges.
        @param kwargs Additional keyword arguments are passed to the
          hafs.config.HAFSConfig.timestrinterp() for string replacement.
        @returns The return value from string interpolation or None if
          nothing was found."""
        if atime is None:
            if logger is not None:
                logger.info(
                    '{%s}: has no atime.  Will use atime=self.anltime=%s.'%(
                        str(string),repr(atime)))
            atime=self.anltime
        if ftime is None:
            if logger is not None:
                logger.info('{%s}: has no ftime.  Will use ftime=atime=%s.'%(
                        str(string),repr(atime)))
            ftime=atime
        atime=to_datetime(atime)
        ftime=to_datetime_rel(ftime,atime)
        if dates is not None and atime is not None:
            if not in_date_range(atime,dates):
                if logger is not None:
                    logger.info('{%s}: atime %s not in %s'%(
                            str(string),str(atime),str(dates)))
                return None
        if logger is not None:
            logger.info(
                'parsing {%s} with ftime=%s atime=%s in section %s'
                %(str(string),repr(ftime),repr(atime),repr(self.section)))
        return self.conf.timestrinterp(
            self.section,"{"+string+"}",ftime,atime,**kwargs)
    def locate(self,dataset,item,atime=None,ftime=None,logger=None,
               dates=None,**kwargs):
        """!Find the location of a requested piece of data.

        Locates the specified item for the specified dataset, at the
        given analysis time ("atime") and forecast time ("ftime").  If
        the requested data is known to not exist, returns None.  This
        should be overridden by subclasses.  The present
        implementation just does this: {dataset}/{item} expanding
        dataset and item with self.parse.  Any kwargs are passed
        along: this allows such things as ensemble ID, or switching
        between GRIB1 or GRIB2 via a keyword argument.
        @param dataset The name of the dataset.
        @param item The name of the item in the dataset.
        @param atime Optional: the analysis time.  The default is self.anltime.
        @param ftime Optional: the forecast time which can be anything
          accepted by tcutil.numerics.to_datetime_rel() relative to the
          analysis time.
        @param logger Optional: a logging.Logger for log messages.  If this
          is provided, several steps along the way of finding the data
          location are logged.
        @param dates Optional: dates for which this datasource is valid.
          This is passed to in_date_range() for validation.  This is
          used to implement the InputSource date ranges.
        @param kwargs Additional keyword arguments are passed by
          parse() to the hafs.config.HAFSConfig.timestrinterp() for
          string replacement.
        @return The path to the requested data or None if it is not found."""
        if logger is not None:
            logger.info(
                'locate item=%s atime=%s ftime=%s in dataset=%s'
                %(repr(item),repr(atime),repr(ftime),repr(dataset)))
        ds=self.parse(dataset,atime=atime,ftime=ftime,logger=logger,
                      dates=dates,**kwargs)
        if ds is None: return None
        it=self.parse(item,atime=atime,ftime=ftime,logger=logger,**kwargs)
        if '/#'.find(ds[-1:])>=0: # if ds ends with / or # then ...
            result=ds+it # caller has already specified means of url append
        else:
            result=ds+'/'+it  # assume we are appending to a parent directory
        if logger is not None:
            logger.info( 'result %s %s => %s'%(
                    repr(ds),repr(it),repr(result),))
        return result

########################################################################
class InputSource(object):
    """!Fetch data from multiple sources.

    This class knows how to fetch data from remote clusters, or the
    local machine.  The data locations are specified by a several
    DataCatalog sections, each of which is given a priority, a valid
    set of dates and a file transfer mechanism.  Data catalogs are
    tried in priority order.  Files are obtained in multiple threads
    at once, and several file transfer mechanisms are understood:

    * file://  ---  obtain files on disk
    * ftp://   ---  contact an FTP server
    * sftp://  ---  contact a server over SSH.  SSH-based rsync is used.
    * htar://  ---  use the proprietary htar program to get a tape archive

    However, only one DataCatalog is examined at a time.  All threads
    work on that one DataCatalog until all data that can be obtained
    from it is done.  Then the threads exit, and new ones are spawned
    to examine the next DataCatalog.

    For example, suppose you are on the Jet supercomputer running a
    HISTORY (retrospective) simulation.  You set up this configuration
    section in your hafs.conf config file:
    @code{.conf}
      [jet_sources_prod2019]
      jet_hist_PROD2019%location  = file:///
      jet_hist_PROD2019%histprio=90
      jet_hist_PROD2019%fcstprio=90

      prod15_data_sp%location=htar://
      prod15_data_sp%histprio=59
      prod15_data_sp%dates=2019011218-2019123118

      [jet_hist_PROD2019]
      @inc=gfs2019_naming
      inputroot2019=/lfs4/HFIP/hafs-data/hafs-input
      gfs={inputroot2019}/HISTORY/GFS.{aYYYY}/{aYMDH}/
      gfs_sfcanl = gfs.t{aHH}z.sfcanl

      [prod15_data_sp]
      inputroot=/NCEPPROD/2year/hpssprod/runhistory/rh{aYYYY}/{aYYYY}{aMM}/{aYMD}
      gfs={inputroot}/
      gfs_sfcanl = {gfs_tar}#./gfs.t{aHH}z.sfcanl

      [hafsdata]
      inputroot=/lfs4/HFIP/hafsv3/John.Doe/hafsdata
      gfs={inputroot}/hafs.{aYMDH}/
      gfs_sfcanl = gfs.t{aHH}z.sfcanl
    @endcode
    and this is the code:
    @code{.py}
      is=InputSource(conf,"jet_sources_prod2019","2019071806")
      hafsdata=DataCatalog(conf,"hafsdata")
      is.get([
         {"dataset":"gfs", "item":"gfs_sfcanl","atime"="2019071800"},
         {"dataset":"gfs", "item":"gfs_sfcanl","atime"="2019071806"},
         {"dataset":"gfs", "item":"gfs_sfcanl","atime"="2019071812"} ],
         hafsdata,realtime=False)
    @endcode

    In this example, the InputSource will look for three GFS surface
    analysis files.  It will search two possible locations for them:
    the on-disk Jet "PROD2019" history location and the NCO production
    tape files.  The disk location will be searched first because its
    history priority is 90, while the tape area has a priority of 59.

    Three files will show up eventually:

    * /lfs4/HFIP/hwrfv3/John.Doe/hafsdata/hafs.2019071800/gfs.t00z.sfcanl
    * /lfs4/HFIP/hwrfv3/John.Doe/hafsdata/hafs.2019071806/gfs.t06z.sfcanl
    * /lfs4/HFIP/hwrfv3/John.Doe/hafsdata/hafs.2019071812/gfs.t12z.sfcanl

    Each file will come from either here:

    * /lfs4/HFIP/hwrf-data/hafs-input/HISTORY/GFS.2019071800/gfs.t00z.sfcanl
    * /lfs4/HFIP/hwrf-data/hafs-input/HISTORY/GFS.2019071806/gfs.t06z.sfcanl
    * /lfs4/HFIP/hwrf-data/hafs-input/HISTORY/GFS.2019071812/gfs.t12z.sfcanl

    or here:

    * htar -xf /NCEPPROD/2year/hpssprod/runhistory/rh2019/201907/20190718/2019071800gfs.tar ./gfs.t00z.sfcanl
    * htar -xf /NCEPPROD/2year/hpssprod/runhistory/rh2019/201907/20190718/2019071806gfs.tar ./gfs.t06z.sfcanl
    * htar -xf /NCEPPROD/2year/hpssprod/runhistory/rh2019/201907/20190718/2019071812gfs.tar ./gfs.t12z.sfcanl    """
    def __init__(self,conf,section,anltime,htar=None,logger=None,hsi=None):
        """!InputSource constructor.
        @param conf    the hafs.config.HAFSConfig to use for
          configuration info
        @param section the section that specifies the list of data catalogs
        @param anltime the default analysis time
        @param htar    the produtil.prog.Runner that runs htar
        @param logger  a logging.Logger for log messages
        @param hsi     the produtil.prog.Runner that runs hsi"""
        self.conf=conf
        self.section=section
        self.anltime=anltime
        def none():
            return None
        def dictnone():
            return collections.defaultdict(none)
        self._sftp_dir_ok=collections.defaultdict(dictnone)
        self._logger=logger
        self.forecast=list() # FORECAST mode DataCatalogs
        self._f_sorted=True
        self.history=list() # HISTORY mode DataCatalogs
        self._h_sorted=True
        self.locks=collections.defaultdict(threading.Lock)
        assert(htar is not None)
        assert(hsi is not None)
        self.htar=alias(htar)
        self.hsi=alias(hsi)
        self.valid=collections.defaultdict(None)

        sections=[section]
        if conf.has_option(section,'@inc'):
            sections.extend(conf[section,'@inc'].split(','))

        sources=collections.defaultdict(dict)
        for sec in sections:
            for key in conf.keys(sec):
                c=key.find('%')
                if(c>0):
                    (src,attr)=(key[0:c],key[c+1:])
                    try:
                        sources[src][attr]=conf.get(sec,key)
                    except KeyError as ke:
                        if logger is not None:
                            logger.warning("[%s] %s: key error: %s"%(
                                    sec,key,str(ke)))
                        continue
        bad=list()
        for (src,attr) in sources.items():
            if 'location' in attr and ('histprio' in attr or \
                                           'fcstprio' in attr):
                dctype=attr.get('type','DataCatalog')
                if   dctype=='DataCatalog':
                    dc=DataCatalog(self.conf,src,self.anltime)
                else:
                    raise InputSourceBadType(
                        'Do not know how to make a DataCatalog of type "%s"'
                        %(dctype,))
                if 'dates' in attr:
                    dates=attr['dates']
                else:
                    dates='1970010100-2038011818'
                self.add(dc,location=attr['location'],
                         fcstprio=attr.get('fcstprio',None),
                         histprio=attr.get('histprio',None),
                         dates=dates)
            else:
                logger.warning('Bad source %s: must have location and either histprio or fcstprio.'%(src,))
                bad.append(str(src))
        if bad:
            raise hafs.exceptions.InvalidInputSpecification(
                'Input sources must ahve location and either histprio or '
                'fcstprio.  Check options in [%s]: %s and rerun launcher '
                'job.'%(self.section,', '.join(bad)))
        self._sort()
    ##@var conf
    # The hafs.config.HAFSConfig object used for configuration info

    ##@var section
    # The section in conf that contains the data catalog list and relevant info

    ##@var anltime
    # The default analysis time.

    ##@var forecast
    # List of forecast mode DataCatalog objects.

    ##@var history
    # List of history mode DataCatalog objects.

    ##@var locks
    # Lock objects to restrict access to FTP servers to one thread at a time.

    ##@var htar
    # A produtil.prog.ImmutableRunner that runs htar.

    ##@var hsi
    # A produtil.prog.ImmutableRunner that runs hsi.

    ##@var valid
    # Data source validity information.

    def _rsync_ssh_exe(self,netpart,path=None,checkdir='/',dest=None,logger=None):
        """!Creates a produtil.prog.Runner for running rsync over ssh.

        Returns a Runner object (as in produtil.run) for executing
        rsync -e ssh.  This subroutine is used to implement
        workarounds for known bugs.
        @param netpart The netpart portion of the sftp URL.
        @param path The path portion of the sftp URL.
        @param dest The destination on the local disk."""
        rsync=self.conf.getexe('rsync','rsync')
        if 'jet' in netpart or produtil.cluster.name()=='jet':
            # Workaround for Jet bug: use protocol 29
            cmd=alias(batchexe(rsync)['-e','ssh','--protocol','29'])
        else:
            cmd=alias(batchexe(rsync)['-e','ssh'])
        if path and dest:
            cmd=cmd['-LvptgoD',"%s:%s"%(netpart,path),dest]
            if logger is not None:
                logger.info('%s:%s=>%s = %s'%(netpart,path,dest,repr(cmd)))
        else:
            # Don't transfer a file.  Just check access.
            cmd=cmd['-d','%s:%s'%(netpart,checkdir)]
            if logger is not None:
                logger.info('check(%s:%s) = %s'%(netpart,checkdir,repr(cmd)))
        return cmd
    def _sort(self):
        """!Sorts the list of history and forecast DataCatalogs by
        decreasing priority."""
        self.forecast=sorted(self.forecast,key=lambda x: -x[0])
        self.history=sorted(self.history,key=lambda x: -x[0])
    def add(self,dc,location,fcstprio=None,histprio=None,dates=None):
        """!Adds a DataCatalog to this InputSource.

        Called automatically from the constructor to add a DataCatalog
        to this InputSource.  The list of add() calls is generated
        from the config section specified in the constructor.  You
        should never need to call this function unless you want to
        explicitly add more DataCatalog objects that are not listed in
        the config files.

        The location parameter is a URL from file, sftp, ftp or htar.
        Examples:

        * local files: file:///lfs4/HFIP/hwrf-data/hafs-input/
        * scp:         sftp://Some.Username@dtn-zeus.rdhpcs.noaa.gov/
        * ftp:         ftp://anonymous@ftpprd.ncep.noaa.gov/
        * htar:        htar:///NCEPPROD/1year/hpssprod/runhistory/rh2012/201204/20120418/

        @warning Bad things will happen if you add the same source
          twice.  Bad things.
        @note If fcstprio and histprio are both None, this call has no
          effect.

        @param dc the DataCatelog object
        @param location the URL of the data source, including the
            username if needed.
        @param fcstprio the priority for using this source in FORECAST
            (real-time) mode.  If missing or None, the source will not
            be used in FORECAST mode.
        @param histprio the priority for using this source in HISTORY
            (retrospective) mode.  If missing or None,the source will
            not be used in HISTORY mode.

        @param dates Dates for which this source is valid.  This is
          passed to the trange argument of in_date_range(t,trange) """
        if fcstprio is None and histprio is None: return
        if dates is None:
            dates='1970010100-2038011818'
        parsed=urllib.parse.urlparse(location)
        if fcstprio is not None:
            self.forecast.append( ( float(fcstprio), location, parsed, dc, dates ) )
            self._f_sorted=False
        if histprio is not None:
            self.history.append( ( float(histprio), location, parsed, dc, dates ) )
            self._h_sorted=False
    def open_ftp(self,netpart,logger=None,timeout=20):
        """!Opens an FTP connection

        Opens the specified ftp://user@host/... request subject to the
        specified timeout, logging to the specified logger (if present
        and non-Null).
        @param netpart The netpart portion of the URL
        @param logger the logging.Logger for log messages
        @param timeout the connection timeout in seconds"""
        if logger is None: logger=self._logger
        if logger is not None:
            logger.info('open_ftp %s'%(netpart,))
        r=re.search('([a-zA-Z0-9_.-]+)+@(.+)',netpart)
        if r:
            (user,host)=r.groups()
            if not user or not host:
                raise InvalidLogin(
                    'FTP logins must be of the form user@host but you '
                    'gave "%s"'%(netpart))
        else:
            (user,host)=('anonymous',netpart)
        f=None
        try:
            if logger is not None: logger.info('%s@%s: log in'%(user,host))
            f=ftplib.FTP(host,user,timeout=timeout)
            f.login()
            assert(f is not None)
            retval=f
            f=None
            valid['ftp://'+netpart]=True
            return retval
        except Exception as e:
            valid['ftp://'+netpart]=False
        finally:
            if f is not None:
                if logger is not None:
                    logger.warning('In finally block, closing FTP stream.')
                f.close()
    def rsync_check_access(self,netpart,logger=None,timeout=20,dirpath='/'):
        """!Checks to see if rsync can even access a remote server.
        @param netpart the netpart portion of the URL
        @param logger the logging.Logger for log messages
        @param timeout the connection timeout in seconds
        @returns True if the server is accessible and False otherwise"""
        try:
            cmd=self._rsync_ssh_exe(netpart,checkdir=dirpath)
            checkrun(cmd,logger=logger)
            return True
        except Exception as e:
            if logger is not None:
                logger.warning('%s: rsync cannot access: %s'
                               %(str(netpart),str(e)))
            return False

    def fetch_file(self,streams,dc,dsurl,urlmore,dest,logger=None,
                   timeout=20,realtime=True):
        """!Internal implementation function that fetches one file.

        You should not call this directly; it is meant to be called
        by "get" and re-implemented in subclasses.  This grabs one
        file, potentially from a remote location.  The URL for the
        base directory of some dataset is in dsurl, while the specific
        file is in urlmore.  The urlmore will be appended to the file
        part of dsurl via urljoin, and the resulting file will be
        transferred.
        @param streams a list used to store opened streams
        @param dc the DataCatalog being obtained
        @param dsurl the URL of the DataCatalog
        @param urlmore additional parts of the URL such as the
          reference or HTTP Get
        @param dest The local disk destination
        @param logger the logging.Logger for log messages
        @param timeout the connection timeout in seconds
        @param realtime True for FORECAST mode, False for HISTORY mode.
        @returns True if successful, False if not"""
        if logger is None: logger=self._logger
        parsed=urllib.parse.urlparse(dsurl)
        joined=urllib.parse.urljoin(dsurl,urlmore,allow_fragments=True)
        parsed=urllib.parse.urlparse(joined)
        if logger is not None:
            logger.info('%s + %s = %s',repr(dsurl),repr(urlmore),repr(joined))
        scheme=parsed.scheme
        path=parsed.path
        netpart=parsed.netloc
        n="%s://%s"%(scheme,netpart)
        if scheme== 'file':
            return self._impl_fetch_file(
                parsed,joined,scheme,path,netpart,streams,dc,dsurl,urlmore,dest,
                logger,timeout,realtime)
        elif scheme=='ftp':
            with self.locks[n]:
                return self._impl_fetch_ftp(
                    parsed,joined,scheme,path,netpart,streams,dc,dsurl,urlmore,dest,
                    logger,timeout,realtime)
        elif scheme=='sftp':
            return self._impl_fetch_sftp(
                parsed,joined,scheme,path,netpart,streams,dc,dsurl,urlmore,dest,
                logger,timeout,realtime)
        else:
            raise UnsupportedTransfer(
                'Cannot transfer this url: unsupported method (not htar, '
                'ftp, file or sftp): '+joined)
        return True
    def _impl_fetch_file(self,parsed,joined,scheme,path,netpart,streams,dc,dsurl,
                         urlmore,dest,logger,timeout,realtime):
        """!Fetches a file from local disk by making a symbolic link.
        @param parsed The parsed URL from urlparse.urlparse
        @param joined The joined URL from urlparse.urljoin
        @param scheme The data transfer scheme (ftp, sftp, etc.)
        @param path The URL path
        @param netpart the netpart portion of the URL.
        @param streams the array of transfer streams
        @param dc the DataCatalog for the remote data
        @param dsurl the dataset URL
        @param urlmore section and other parts of the URL
        @param dest the local disk destination
        @param logger the logging.Logger for messages, or None
        @param timeout connection timeout in seconds, ignored
        @param realtime True for FORECAST mode, False if not.  In
          FORECAST mode, the symbolic link is made even if the file
          does not exist, so long as the DataCatalog is marked as
          realtime (DataCatalog.rt_updated() returns True)
        @returns True on success, False if the file was not linked"""
        if logger is not None:
            logger.info('%s: from local file %s'%(dest,joined))
        if ( realtime and dc.rt_updated() ) or  \
                (os.path.exists(path) and os.access(path,os.R_OK)):
            makedirs(os.path.dirname(dest),logger=logger)
            make_symlink(path,dest,force=True,logger=logger)
        else:
            return False
            #produtil.fileop.deliver_file(path,dest,keep=True,logger=logger)
        return True
    def _impl_fetch_sftp(self,parsed,joined,scheme,path,netpart,streams,dc,dsurl,
                         urlmore,dest,logger,timeout,realtime):
        """!Fetches a file via rsync over ssh.
        @param parsed The parsed URL from urlparse.urlparse
        @param joined The joined URL from urlparse.urljoin
        @param scheme The data transfer scheme (ftp, sftp, etc.)
        @param path The URL path
        @param netpart the netpart portion of the URL.
        @param streams the array of transfer streams
        @param dc the DataCatalog for the remote data
        @param dsurl the dataset URL
        @param urlmore section and other parts of the URL
        @param dest the local disk destination
        @param logger the logging.Logger for messages, or None
        @param timeout connection timeout in seconds
        @param realtime True for FORECAST mode, False if not.  Ignored.
        @returns True on success, False if the file was not copied"""
        tempname=None
        try:
            dirpath=os.path.dirname(path)
            ok=self._sftp_dir_ok[netpart][dirpath]
            if ok is None:
                logger.info('%s:%s: check access.'%(netpart,dirpath))
                ok=self.rsync_check_access(
                    netpart,logger=logger,dirpath=dirpath)
                self._sftp_dir_ok[netpart][dirpath]=ok
            if ok is False:
                logger.info('%s:%s: skip: directory inaccessibble.'%(
                    netpart,path))
                return False
            else:
                logger.info('%s:%s: can access'%(netpart,path))
            makedirs(os.path.dirname(dest),logger=logger)
            with tempopen(dest,'wb') as f:
                tempname=f.name
            cmd=self._rsync_ssh_exe(netpart,path,dest=tempname)
            checkrun(cmd,logger=logger)
            os.rename(tempname,dest)
            tempname=None
        except produtil.run.ExitStatusException as e:
            if logger is not None:
                logger.warning("%s: non-zero exit status %s"%(
                        joined,repr(e.returncode)))
            return False
        finally:
            if tempname is not None:
                if logger is not None:
                    logger.warning('In finally block, deleting temp file %s.'%(tempname,))
                os.remove(tempname)
        return True
    def _impl_fetch_ftp(self,parsed,joined,scheme,path,netpart,streams,dc,dsurl,
                        urlmore,dest,logger,timeout,realtime):
        """!Fetches a file over FTP.
        @param parsed The parsed URL from urlparse.urlparse
        @param joined The joined URL from urlparse.urljoin
        @param scheme The data transfer scheme (ftp, sftp, etc.)
        @param path The URL path
        @param netpart the netpart portion of the URL.
        @param streams the array of transfer streams
        @param dc the DataCatalog for the remote data
        @param dsurl the dataset URL
        @param urlmore section and other parts of the URL
        @param dest the local disk destination
        @param logger the logging.Logger for messages, or None
        @param timeout connection timeout in seconds
        @param realtime True for FORECAST mode, False if not.  Ignored.
        @returns True on success, False if the file was not copied"""
        n="%s://%s"%(scheme,netpart)
        if n not in streams:
            streams[n]=self.open_ftp(n,logger=logger,timeout=timeout)
        stream=streams[n]
        tempname=None
        try:
            makedirs(os.path.dirname(dest),logger=logger)
            with tempopen(dest,'wb') as f:
                tempname=f.name
                if logger is not None:
                    logger.info('%s: pull %s => %s'
                                %(n,parsed.path,tempname))
                stream.retrbinary("RETR "+parsed.path,f.write)
            remote_size=stream.size(parsed.path)
            if remote_size is not None:
                local_size=os.path.getsize(tempname)
                if local_size!=remote_size:
                    if logger is not None:
                        logger.warning(
                            '%s: wrong size: %d local vs %d remote'
                            %(tempname,local_size,remote_size))
                    raise PartialTransfer(
                        'Could not transfer full file: only %d of %d '
                        'bytes transferred.'%(local_size,remote_size))
            if logger is not None:
                logger.info('%s: move from %s'%(dest,tempname))
            os.rename(tempname,dest)
            tempname=None
        finally:
            if tempname is not None:
                logger.warning('In finally block, removing temp file %s'%(
                        tempname))
                os.remove(tempname)
        return True
    def list_for(self,realtime=True):
        """!Returns the list of DataCatalog objects for FORECAST or
        HISTORY mode.
        @param realtime True for FORECAST mode, False for HISTORY
        @returns self.forecast or self.history
        @post _sort() has been called, sorting self.forecast and
          self.history in order of priority"""
        if realtime:
            if not self._f_sorted: self._sort()
            return self.forecast
        else:
            if not self._h_sorted: self._sort()
            return self.history

    def _impl_get_archive(self,archpath,parts,done,prio, loc, parsed, dc,
                          data,target_dc,realtime,logger,skip_existing):
        """!Fetches an archive from HPSS
        @param archpath path to the archive on HPSS
        @param parts list of required archive elements as integer index
          within the done argument
        @param[out] done list of bool, set to True if the part was obtained
        @param prio the priority of this input source
        @param loc,parsed,dc,data,target_dt,realtime,skip_existing Ignored.
        @param logger the logging.Logger for log messages"""
        with produtil.cd.TempDir(prefix="pull.",cd=False,
                                 keep_on_error=False) as td:
            assert(isinstance(td,produtil.cd.TempDir))
            assert(self.hsi is not None)
            if self.hsi is not None:
                i=self.hsi['get','-',':',archpath+'.idx']>"/dev/null"
                err=run(i,logger=logger)
                if err!=0:
                    logger.warning("%s.idx: exit status %d dumping index "
                                   "file. Htar will probably fail."
                                   %(archpath,int(err)))
            r=self.htar['-xpf',archpath]\
                [ [p for p in parts.keys()] ]\
                .cd(td.dirname)
            logger.info('%s: list contents'%(td.dirname,))
            for line in str(produtil.listing.Listing(path=td.dirname)):
                logger.info(line)
            stat=run(r,logger=logger)
            if stat!=0:
                logger.info('non-zero exit status %d from htar; will retry '
                            'in five seconds.'%stat)
                for x in range(50):
                    time.sleep(0.1)
                stat=run(r,logger=logger)
            if stat!=0:
                logger.info('non-zero exit status %d from htar; will keep '
                            'going anyway'%stat)
            if logger is not None:
                logger.info("%s: pull %d files"
                            %(archpath,len(parts)))
            nope=set() # Files missing from archive
            yup=set() # Files found in archive
            for (filepart,tgti) in parts.items():
                tgt=tgti[0]
                src=os.path.join(td.dirname,filepart)
                logger.debug('%s: check for this at %s'%(tgt,src))
                if os.path.exists(src):
                    makedirs(os.path.dirname(tgt),logger=logger)
                    deliver_file(src,tgt,keep=False,logger=logger)
                    for i in tgti[1:]:
                        logger.debug('%s: add %d'%(tgt,i))
                        done.add(i)
                    yup.add
                    relfile=os.path.relpath(src,td.dirname)
                    relfile=re.sub('^(../)+','',relfile)
                    yup.add(relfile)
                else:
                    relfile=os.path.relpath(src,td.dirname)
                    relfile=re.sub('^(../)+','',relfile)
                    nope.add(relfile)
                    logger.debug('%s: does not exist'%(src,))
            if nope:
                missing=sorted(list(nope))
                logger.warning('%s: does not have: %s'%(
                    archpath,', '.join(missing)))
            if yup:
                found=sorted(list(yup))
                logger.warning('%s: has files: %s'%(
                    archpath,', '.join(found)))
            if yup and not nope:
                logger.info('%s: gleefully reporting all desired '
                            'files found.'%(archpath,))


    def _impl_get_file(self,i,done,src,tgt,prio, loc, parsed, dc,streams,
                       archives,data,target_dc,realtime,logger,skip_existing):
        """!Obtain one or more files.
        @param i The index in done of the file being fetched
        @param done an array of logical flags telling which files are transferred
        @param src the source location
        @param tgt the target location
        @param prio the numerical priority
        @param loc the on-disk destination
        @param parsed the parsed URL as output by urlparse.urlparse
        @param dc the DataCatalog
        @param streams the array of transfer streams
            @param archives a double-nested dict of lists, mapping from
          archive name to file part to index within done of the file
          in question
        @param target_dc the DataCatalog of the target locations
        @param realtime True for FORECAST mode, False for HISTORY mode
        @param logger the logging.Logger for log messages
        @param skip_existing if True, do not re-download files that
        already exist on disk (in the target_dc)"""
        archsep=src.find('#')
        if archsep>=0:
            # This is in an archive, so we will have to stage
            # the archive first, and get the file in the
            # second pass.
            arch=src[0:archsep]
            filepart=src[archsep+1:]
            if arch in archives and filepart in archives[arch]:
                archives[arch][filepart].append(i)
            else:
                archives[arch][filepart]=[tgt,i]
        else:
            if src[0:5]=='htar:':
                logger.warning("%s: no # in path - skipping this"
                               %(src,))
                return
            try:
                if self.fetch_file(
                    streams,dc,loc,src,tgt,
                    logger=logger,realtime=realtime):
                    done.add(i)
            except (EnvironmentError,ExitStatusException) as e:
                if logger is not None:
                    logger.warning(
                        'fetching %s=>%s: %s'%(str(src),str(tgt),
                                               str(e)),exc_info=True)

    def priotable(self,dclist):
        """!Generates a string containing a human-readable, prioritized
        list of data sources.
        @param dclist The data source list from list_for()
        @returns A multi-line string containing the table.

        Example:
            Prioritized list of data sources:
            PRIO-   LOCATION = SOURCE @ DATES
            100 -   file:/// = DataCatalog(conf,'wcoss_fcst_PROD2019',2019080518) @ '1970010100-2038011818'
            098 -   file:/// = DataCatalog(conf,'wcoss_prepbufrnr_PROD2019',2019080518) @ '1970010100-2038011818'
            097 -    file:// = DataCatalog(conf,'zhan_gyre',2019080518) @ '2011060718-2011111200,2013051800-2013091018'"""
        s=io.StringIO()
        s.write('Prioritized list of data sources:\nPRIO-   LOCATION = SOURCE @ DATES\n')
        for ( prio, loc, parsed, dc, dates ) in dclist:
            s.write('%03d - %10s = %s @ %s\n'%(
                    int(prio),str(loc),repr(dc),repr(dates)))
        sv=s.getvalue()
        s.close()
        return sv

    def get(self,data,target_dc,realtime=False,logger=None,
            skip_existing=True):
        """!Transfers the specified set of data to the specified
        target.  The "target_dc" is a DataCatalog that specifies the
        destination filenames.  The "realtime" argument is True for
        FORECAST (real-time) mode runs, and False for HISTORY
        (retrospective) mode runs.  The "data" argument should be an
        iterable (list, tuple, etc.) where each element is a dict-like
        object that describes one file to obtain.  Each dict contains:

          dataset - string name of the dataset (gfs, gdas1, gefs,
            enkf, etc.)
          item - string name of the object (ie.: sf, sfcanl, bufr)
          atime - Optional: a datetime.datetime specifying the
            analysis time.  Default is the atime from the
            InputSource's constructor.
          ftime - Optional: a datetime.datetime specifying the
            forecast time.
          ...others... - any other keyword arguments will be sent to
            the .location functions in any of this InputSource's
            DataCatalog objects."""
        if logger is None: logger=self._logger
        dclist=self.list_for(realtime)
        done=set()
        logger.info(self.priotable(dclist))
        for ( prio, loc, parsed, dc, dates ) in dclist:
            assert(loc is not None)
            assert(prio is not None)
            assert(parsed is not None)
            assert(dc is not None)
            assert(dates is not None)
            scheme=parsed.scheme
            netpart=parsed.netloc
            logger.info('Consider: %03d - %10s = %s @ %s\n'%(
                    int(prio),str(loc),repr(dc),repr(dates)))
            if scheme=='sftp':
                if not self.rsync_check_access(netpart,logger):
                    logger.error('%s: cannot access; will skip'%(netpart,))
                    continue
            elif scheme not in ['ftp','htar','file']:
                logger.error('%s: invalid transfer mode %s; will skip'
                             %(loc,scheme,))
                continue
            streams=dict()
            archives=collections.defaultdict(dict)
            workpool=None
            try:
                with produtil.workpool.WorkPool(3,logger) as workpool:
                    i=0
                    seen=set()
                    jlogger.info('Pull from: %03d - %10s = %s @ %s\n'%(
                            int(prio),str(loc),repr(dc),repr(dates)))
                    for d in data:
                        i+=1
                        if i in done: continue # skip files we already
                                               # transferred
                        assert('dates' not in d)
                        tgt=target_dc.locate(**d)
                        if tgt is None:
                            continue
                        if tgt in seen:
                            if logger is not None:
                                logger.info('%s: already processing this'%(tgt,))
                            continue
                        if os.path.exists(tgt) and skip_existing:
                            if logger is not None:
                                logger.info('%s: already exists'%(tgt,))
                                done.add(i)
                                continue
                        if logger is not None:
                            logger.debug("%s => %s"%(repr(d),repr(tgt)))
                        src="(unknown)"
                        if logger is not None:
                            logger.debug('search for %s in %s'%(repr(d),repr(dc)))
                        try:
                            src=dc.locate(dates=dates,**d)
                        except KeyError as k:
                            logger.debug("%s: key error %s"%(src,str(k)))
                            continue
                        if src is None: continue
                        if logger is not None:
                            logger.info("SRC %s => %s"%(strsrc(d),repr(src)))
                        seen.add(tgt)
                        workpool.add_work(self._impl_get_file,args=[
                                i,done,src,tgt,prio, loc, parsed, dc,streams,
                                archives,data,target_dc,realtime,logger,
                                skip_existing])
                    workpool.barrier()
                    for (archpath,parts) in archives.items():
                        if len(parts)<=0:
                            if logger is not None:
                                logger.info("%s: nothing to pull; skip"
                                            %(archpath,))
                            continue
                        workpool.add_work(self._impl_get_archive,args=[
                                archpath,parts,done,prio, loc, parsed, dc,
                                data,target_dc,realtime,logger,skip_existing])
                    workpool.barrier()
            finally:
                if logger is not None:
                    logger.warning('In finally block, closing streams.')
                for (key,stream) in streams.items():
                    try:
                        stream.close()
                    except Exception as e:
                        if logger is not None:
                            logger.warning(
                                'Exception while closing stream %s: %s'
                                %(key,str(e)),exc_info=True)
            del workpool
        jlogger.info('Exited input loop after source: %03d - %10s = %s @ %s\n'%(
                int(prio),str(loc),repr(dc),repr(dates)))
        i=0
        bad=False
        for d in data:
            i+=1
            if i in done:
                continue
            tgt=target_dc.locate(**d)
            if os.path.exists(tgt):
                continue
            if d.get('optional',False):
                if logger is not None:
                    logger.info('missing optional data: %s'%(repr(d),))
            else:
                if logger is not None:
                    logger.warning('MISSING INPUT: %s'%(repr(d),))
                bad=True
        return not bad

    def get_one(self,dataset,item,dest,logger=None,timeout=20,realtime=True,
                **kwargs):
        """!This is a simple wrapper around fetch_file that gets only
        one file.  It will fail if the file requires pulling an
        archive.
        @param dataset the dataset to transfer
        @param item the desired item in the dataset
        @param dest the on-disk destination filename
        @param logger a logging.Logger for log messages
        @param timeout the connection timeout in seconds
        @param realtime True for FORECAST mode, False for HISTORY mode
        @param kwargs extra keyword arguments are passed to DataCatalog.locate()"""
        if logger is None: logger=self._logger
        streams=dict()
        try:
            dclist=list_for(realtime)
            for ( prio, loc, parsed, dc ) in dclist:
                src=dc.locate(dataset=dataset,item=item,**kwargs)
                if src is None: continue
                archsep=src.find('#')
                if archsep>=0:
                    raise NotImplementedError(
                        'Source is in an archive.  De-archiving is not '
                        'supported by "get_one."  Use "get" instead.')
                elif self.fetch_file(streams,dc,loc,src,dest,logger=logger):
                    break
        finally:
            if logger is not None:
                logger.warning('In finally block, closing streams.')
            for (key,stream) in streams.items():
                try:
                    stream.close()
                except Exception as e:
                    if logger is not None:
                        logger.warning(
                            'Exception while closing stream %s: %s'
                            %(key,str(e)),exc_info=True)
