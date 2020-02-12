"""!Contains the WRFCopyTask, which delivers WRF output and input.

This module contains the implementation of WRFCopyTask, which delivers
WRF input and output files.  It maintains three sets of deliveries: 

* initial - files created before the wrf execution begins
* wrfprod - files created by wrf during its execution
* final - files created at the end of the wrf execution, or just after wrf ends

The WRFCopyTask.run() and WRFCopyTask.runpart() monitor the WRF
execution and copy files as needed.  

Examples:

@code
wrf=WRFSimulation(...)
... add domains to the wrf simulation ...
sim=ExternalWRFTask(dstore,conf,section,wrf,...more stuff...)
wrfcopier=WRFCopyTask(dstore,conf,"wrf_copier",sim,
  "{vit[stormname]}{vit[stormid3]}.{YMDH}.")
wrfcopier.d_initial('namelist.input')
wrfcopier.d_initial('wrfinput_d01')
wrfcopier.d_initial('wrfbdy_d01')
wrfcopier.d_initial('fort.65')
fhr=0
while fhr-0.001<126:
  for product in runwrf.products(time=3600*fhr,stream="history"):
     wrfcopier.d_wrfprod(product,check=False)
  fhr+=3
wrfcopier.d_final('track_d03.patcf')
wrfcopier.run()
@endcode

This example will monitor an external WRF simulation.  It will deliver
four files at the beginning of the simulation: namelist.input,
wrfinput_d01, wrfbdy_d01 and fort.65.  One file will be delivered at
the end: track_d03.patcf.  All three-hourly history stream files
(wrfout_d...) will be delivered as they become available.  The files
will be delivered to com, prepending the storm name, id and cycle.

For example, this file
   wrfout_d01_2016-08-14_09:00:00
will go here:
   /path/to/com/invest97l.2016081400.wrfout_d01_2016-08-14_09:00:00
due to the wrfcopier.d_wrfprod() call.

All of the delivery happens in the last line of the code example when
WRFCopier.run() is called.  That call will not return until all files
are delivered or the WRF model fails.
"""

__all__ = ['WRFCopyTask']

import os, re, time
import produtil.datastore, produtil.locking, produtil.fileop
import hwrf.hwrftask, hwrf.wrf

from produtil.run import checkrun, run, exe, bigexe, mpi, mpirun
from produtil.datastore import COMPLETED,RUNNING,UNSTARTED,Product,\
    FileProduct,UpstreamFile
from hwrf.hwrftask import HWRFTask

##@var __all__
# list of symbols exported by "from hwrf.copywrf import *"
__all__=['WRFCopyTask']

class WRFCopyTask(HWRFTask):
    """!@brief wrf file delivery task
    @details This is a Task that copies WRF input and output files
    from the WRF run directory to the COM directory."""
    def __init__(self,dstore,conf,section,wrftask,out_prefix,**kwargs):
        """!@brief WRFCopyTask constructor
        @details Constructor for the WRFCopyTask
        @param dstore  the produtil.datastore.Datastore
        @param conf    the hwrf.config.HWRFConfig
        @param section the section to use in conf
        @param wrftask the task that runs WRF.  This should be an 
           hwrf.wrf.ExternalWRFTask, or a subclass of 
           hwrf.fcsttask.WRFTaskBase
        @param out_prefix  output file prefix, a string suitable for
           passing into hwrf.config.HWRFConfig.strinterp()
        @param kwargs  passed to the superclass constructor"""
        if 'outdir' not in kwargs:
            kwargs['outdir']=conf.getdir('com')
        super(WRFCopyTask,self).__init__(dstore,conf,section,**kwargs)
        self._wrftask=wrftask
        self.out_prefix=out_prefix
        self._initial=list()
        self._wrfprod=list()
        self._final=list()
        self._ncks_path=False
        self._could_not_find_ncks=False

    ##@var out_prefix
    # Prefix to prepend to output filenames after the com path.  This
    # string is sent through the self.confstrinterp, so it can contain
    # {...} escape sequences.

    ##@var _wrftask
    # The wrftask argument to __init__, the WRF simulation being monitored.

    ##@var _initial
    # The list of WRF initial state deliveries.

    ##@var _wrfprod
    # The list of deliveries to make during WRF execution.

    ##@var _initial
    # The list of deliveries to make after WRF finishes.

    ##@var _ncks_path
    # The path to ncks or the constant False if ncks is missing or unused.

    ##@var _could_not_find_ncks
    # True if we searched for ncks, could not find it, and gave up looking.

    @property 
    def ncks_path(self):
        """!@brief Returns the path to ncks.  
        @details Returns the path to the ncks program, used to convert
        between NetCDF 3 and compressed NetCDF 4 file formats.
        Returns None if ncks cannot be found.  This function will only
        search for ncks once, and will cache the result.  Set
        self._ncks_path=False to force a recheck."""
        if self._ncks_path is False and not self._could_not_find_ncks:
            ncks=self.getexe('ncks','')
            if not self._ncks_path:
                ncks=produtil.fileop.find_exe('ncks',raise_missing=False)
            if ncks:
                self._ncks_path=ncks
            else:
                self._could_not_find_ncks=True
        return self._ncks_path

    def compression_copier(self,src,vsubset=None):
        """!@brief creates and returns a compression_copier for deliver_file
        @brief Returns the object that should be sent as the "copier"
        argument to produtil.fileop.deliver_file() to copy the given
        source file. This is either None, or a function that calls
        ncks to compress NetCDF files.  If a vsubset argument is
        present, the file is subsetted, retaining only the variables
        vsubset (a comma separated list).
        @param src      the source file
        @param vsubset  unused, but may one day be used to subset the file
        @returns None if the source file is not NetCDF.  If it is
        NetCDF, then a copy(s,t,x) function is returned, suitable for
        passing to the copy argument of
        produtil.fileop.deliver_file()"""
        ncks=self.ncks_path
        if not ncks:
            return None # don't have ncks, so cannot deliver

        if produtil.fileop.netcdfver(src) is None: 
            return None # is not NetCDF, so just copy the file bit-by-bit

        # Source file IS NetCDF, or possibly non-NetCDF HDF5, but
        # we'll overlook that.  Use ncks to compress, raise an
        # exception if ncks returns non-zero.
        logger=self.log()
        def copy(s,t,x):
            produtil.fileop.remove_file(t,logger=logger)
            checkrun(bigexe(ncks)['-4','-L','6','-O',s,t]<'/dev/null',
                     logger=logger)
        return copy

    def decompression_copier(self,src):
        """!@brief returns a decompression copier for deliver_file
        @details Returns an object that has the reverse effect of
        self.compression_copier.  This will uncompress files that
        compression_copier copier would compress.  NetCDF files will
        all be converted to 64-bit indexing NetCDF 3 files.
        @param src      the source file
        @returns None if the source file is not NetCDF.  If it is
        NetCDF, then a copy(s,t,x) function is returned, suitable for
        passing to the copy argument of produtil.fileop.deliver_file()"""
        ncks=self.ncks_path
        if not ncks:
            return None # don't have ncks, so cannot deliver

        if produtil.fileop.netcdfver(src) is None: 
            return None # is not NetCDF, so just copy the file bit-by-bit

        # Source file IS NetCDF, or possibly non-NetCDF HDF5, but
        # we'll overlook that.  Use ncks to decompress, raise an
        # exception if ncks returns non-zero.
        logger=self.log()
        def copy(s,t,x):
            produtil.fileop.remove_file(t,logger=logger)
            checkrun(bigexe(ncks)['-6','-O',s,t]<'/dev/null',logger=logger)
        return copy

    def comfile(self,orig,destname=None):
        """!@brief get the full path to a file
        @details Generates a full path to the delivery location of the
        specified source file.  Returns the full path and the basename
        in a tuple.
        @returns a tuple (path,basename) where the path is the full
        path to the file, and the basename is os.path.basename(path)
        @param orig     the original filename
        @param destname optional: the desired destination name format
           which will be sent through hwrf.hwrftask.HWRFTask.confstrinterp()
           to generate the final destination filename"""
        if(isinstance(orig,Product)):
            bn=os.path.basename(str(orig.location))
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

    def d_initial(self,inprod,check=None,destname=None):
        """!@brief deliver a file generated before WRF starts
        @details Requests delivery of a file that is created before the
        wrf.exe invocation.  The "inprod" may be a Product or a
        filename relative to the WRF run directory.  The optional
        "check" argument enables calling the potentially expensive
        "check" subroutine on the upstream product every time it is
        considered for delivery.  If the input is a Product, and
        check=False, then only the "available" subroutine is called,
        which will not be updated unless another Task marks the
        product as available.  The default is check=False for Products
        and check=True for filenames.
        @param inprod the input produtil.datastore.Product
        @param check   do we need to call the product's check() 
          function?  This is needed if it is a produtil.datastore.UpstreamFile
        @param destname  optional: the destname argument to comfile()
          which will be used to generate the delivered filename in
          the COM directory."""
        return self._deliver_to_group(self._initial,inprod,check,destname)

    def d_final(self,inprod,check=None,destname=None):
        """!@brief deliver a file generated at the end of the WRF run
        @details Requests delivery of a file created by WRF that is not
        complete until the WRF exits.  Examples of this are the
        wrfdiag, hifreq and patcf files.  These files will be
        delivered when the underlying WRF Task has a state of
        COMPLETED.  The optional "check" argument enables calling the
        potentially expensive "check" subroutine on the upstream
        product every time it is considered for delivery.  If the
        input is a Product, and check=False, then only the "available"
        subroutine is called, which will not be updated unless another
        Task marks the product as available.  The default is
        check=False for Products and check=True for filenames.
        @param inprod the input produtil.datastore.Product
        @param check   do we need to call the product's check() 
          function?  This is needed if it is a produtil.datastore.UpstreamFile
        @param destname  optional: the destname argument to comfile()
          which will be used to generate the delivered filename in
          the COM directory."""
        return self._deliver_to_group(self._final,inprod,check,destname)

    def _deliver_to_group(self,group,inprod,check=None,destname=None):
        """!@brief internal function that arranges for future file delivery
        @details Do not call this function directly.  It is the internal
        implementation of d_initial and d_final.  Call those functions
        instead.
        @param group which group does this belong to?  Should be
          self._initial, self._wrfprod or self._final
        @param inprod the input produtil.datastore.Product
        @param check   do we need to call the product's check() 
          function?  This is needed if it is a produtil.datastore.UpstreamFile
        @param destname  optional: the destname argument to comfile()
          which will be used to generate the delivered filename in
          the COM directory."""
        (comfile,combn)=self.comfile(inprod,destname=destname)
        if(isinstance(inprod,Product)):
            upstream=inprod
            if check is None: check=False
        else:
            # Make an internal UpstreamFile to check for the file:
            wrffile=os.path.join(self._wrftask.location,inprod)
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
    def d_wrfprod(self,product,check=False,destname=None):
        """!@brief deliver a file generated during the WRF simulation
             such as a history or restart file
        @details Requests delivery of a WRF I/O subsystem output file.
        The "product" argument must be a Product object.  The optional
        argument "check" enables calling the potentially expensive
        "check" subroutine on the product every time it is considered
        for delivery.  If check=False, then only the "available"
        subroutine is called, which will not be updated unless another
        Task marks the product as available.
        @param product the produtil.datastore.Product
        @param check   do we need to call the product's check() 
          function?  This is needed if it is a produtil.datastore.UpstreamFile
        @param destname  optional: the destname argument to comfile()
          which will be used to generate the delivered filename in
          the COM directory."""
        if not isinstance(check,bool):
            raise TypeError('In d_wrfprod, check must be a bool, not a %s %s'%(
                    type(check).__name__,repr(check)))

        (comfile,combn)=self.comfile(product,destname=destname)

        if not isinstance(product,Product):
            wrffile=os.path.join(self._wrftask.location,product)
            upstream=UpstreamFile(dstore=self.dstore,prodname=combn,
                category="%s-upstream"%(self.taskname,),location=wrffile)
            upstream.location=wrffile
        else:
            upstream=product

        outproduct=FileProduct(dstore=self.dstore,prodname=combn,
                               category=self.taskname,location=comfile)
        outproduct.location=comfile
        self._wrfprod.append( (upstream,outproduct,bool(check)) )
        return self

    def run(self,check_all=False,raise_all=False):
        """!@brief watch for files to show up, delivering them when they do
        @details Keeps watching for WRF files to show up, copying them when
        they do.  This is just a simple wrapper around self.runpart,
        and does not return until runpart sets the state to something
        other than RUNNING.
        @param check_all if True, all non-delivered products have
          product.check() called on them"""
        self.state=RUNNING
        logger=self.log()
        while self.state==RUNNING:
            if self.run_helper(False,check_all,raise_all=raise_all):
                logger.info('Sleep 5...')
                time.sleep(5)
                logger.info('       ...done sleeping.')

    def deliver_group(self,group,check_all=False,raise_all=False):
        """!deliver files to COM

        @protected
        This is an internal implementation function. Do not call it
        directly. Takes a list of tuples containing an upstream
        product, a downstream product to deliver, and a boolean
        telling whether to check() the upstream product.  Delivers
        available products.  Returns a tuple containing two booleans:
        the first is True iff something was delivered, and the second
        is true iff something is left in the group that has not been
        delivered yet.  The check_all argument can be used to force a
        check on all products by setting check_all=True.

        @param group either self._initial, self._wrfprod or self._final
        @param check_all  if True, run product.check() on all products"""
        did_something=False
        more_to_do=False
        logger=self.log()
        lockdir=os.path.join(self.getdir('lockdir'),self.taskname)
        for inprod,outprod,check in group:
            logger.debug(
                'COPYWRF ITEM: inprod=%s outprod=%s check=%s check_all=%s'%(
                    repr(inprod),repr(outprod),repr(check),repr(check_all)))
            messagemore=''
            if not check and not check_all:
                messagemore=' or post has not posted it yet'
            try:
                if not outprod.available:
                    available=inprod.available
                    if not available and ( check or check_all ):
                        inprod.check()
                        available=inprod.available
                    if available:
                        logger.info('%s: delivering.'%(
                                str(outprod.location),))
                        lockfile=os.path.join(
                            lockdir,os.path.basename(inprod.location))
                        locker=produtil.locking.LockFile(
                            filename=lockfile,max_tries=1)
                        try:
                            with locker:
                                ifrom=inprod.location
                                copier=self.compression_copier(ifrom)
                                outprod.deliver(frominfo=ifrom,copier=copier)
                                did_something=True
                        except produtil.locking.LockHeld as lh:
                            logger.info(
                                ' Nope.  Another process is delivering this '
                                'file right now.  Moving on.')
                            more_to_do=True
                    else:
                        logger.info('%s: not yet available%s.'
                                    %(str(inprod.location),messagemore))
                        more_to_do=True
            except produtil.locking.LockHeld as lh:
                logger.info('%s: lock held, move on.'%(
                    repr(outprod.location),))
                more_to_do=True
            except Exception as e:
                more_to_do=True
                logger.warning('%s: trouble delivering: %s\n'%(
                        repr(outprod.location),str(e)),exc_info=True)
                if raise_all: raise
        return ( did_something, more_to_do )
    def unrun(self):
        """!@brief delete delivered files
        @details Calls the undeliver function on all products,
        deleting them from the destination.  Product objects' undeliver()
        functions are called to achieve this.  """
        for inprod,outprod,check in self._initial:
            outprod.undeliver()
        for inprod,outprod,check in self._wrfprod:
            outprod.undeliver()
    def runpart(self,check_all=False):
        """!@brief deliver one output file and return.
        @details Delivers one output file and returns.  Sets the state to
        COMPLETED if all files are delivered.  
        @param check_all Optional.  If True, forces a call to check()
        on all undelivered products, even if those products are not
        checked by default."""
        self.run_helper(True,check_all)
    def run_helper(self,runpart,check_all=False,raise_all=False):
        """!@brief internal implementation of run() and runpart()
        @details This is the internal implementation of run and
        runpart.  It delivers files, and returns False if all files
        are delivered.  
        @param runpart If runpart=True, run_helper() will return immediately
        after delivering one file.  
        @param check_all Optional.  If True, forces a call to check()
        on all undelivered products, even if those products are not
        checked by default."""
        self._wrftask.update_state()
        state=self._wrftask.state
        started = (state==RUNNING or state==COMPLETED)
        completed = ( state == COMPLETED )

        initial_complete=False # done copying wrf inputs
        parallel_complete=False # done copying wrf outputs
        logger=self.log()
        logger.info('wrf task state=%d so started=%s and completed=%s'
                    %(self._wrftask.state,repr(started),repr(completed)))
        more_init=False
        more_para=False
        more_final=False
        if started:
            (did_something,more_init)=self.deliver_group(
                self._initial,check_all,raise_all=raise_all)
            if did_something and runpart: 
                # This is runpart, and we just delivered some initial
                # products, so return.
                return True
        # Now deliver any parallel products:
        (did_something,more_para)=self.deliver_group(self._wrfprod,check_all)
        if did_something and runpart:
            return True
        # And the "final state" products:
        if completed:
            (did_something,more_final)=self.deliver_group(
                self._final,check_all)
            if did_something and runpart: 
                # This is runpart, and we just delivered some initial
                # products, so return.
                return True
        if not more_init and not more_para and not more_final:
            # Nothing remains to be delivered, so we're done.
            self.state=COMPLETED
            logger.info('nothing left to deliver')
            return False
        else:
            return True
