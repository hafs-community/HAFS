"""This module implements FcstTask classes for the hwrf 
multistorm basin scale multistorm final merge task.

    FinalMergeTask - Create a multistorm final merge domain 01 product,
        from the required domain 01 information from storm in
        a multistorm hwrf run.

"""

# Frimel, DTC This module was taken, copied, templated and modeled
#         from relocate.py and fcsttask.py  - Thank You !

# This FinalMergeTask class:
# Currently meant to run on a storm that has no ocean coupling.
# Currently No input checking is performed, assumes gfs realinit and gdas_merge task
# for each storm is complete and successful.

# 1. Get the list of storms .
# 2. set the priority storm pstorm, from the storm id list.
# 3. link to the gfsinit/realinit/wrfinput_d01 of the priority pstorm
# 4. link the /gdas_merge/wrfinput_d01 of the priority storm
# 5. For each additional storm link to the gdas_merge/wrfinput_d01 from each storm.
# 6. these are all links in the final_merge workdir back to each storm
# 7. Convert the files from netcdf to binary
# 8. Perform Final Merge
# 9. Convert the merged file from binary to netcdf
# 10 Deliver the final merge netcdf file to this tasks outdir.


__all__=[ 'FinalMergeTask' ]

import os, shutil
import hwrf.fcsttask
import produtil.fileop, produtil.cd, produtil.run
import produtil.datastore, produtil.listing
import itertools

from produtil.cd import NamedDir
from produtil.fileop import realcwd, make_symlink, deliver_file, make_symlinks_in
from produtil.datastore import FileProduct, COMPLETED, RUNNING, FAILED

from produtil.run import alias, bigexe, exe, checkrun
from hwrf.fcsttask import FcstTask
from hwrf.exceptions import WRFInputMissing

# Modeled after fcsttask.Input2Fcst(Object).
class Input2FinalMerge(object):
    """This is the abstract base class of anything that gets, or
    creates, input files for the Final Merge without running another
    Task.  For example, something that copies the wrfinput output would
    be a subclass of Input2FinalMerge"""
    def __init__(self,src): self.src=src
    def get_inputs(self,just_check=False,**kwargs):
        """In subclasses, this function copies or links the input
        files.  This default implementation does nothing."""
        return True
    def link_product(self,product,excclass,logger,target=None,
                     just_check=False):
        """Links the file from the given product to the target
        location (basename(product.location) if no target is
        provided).  If the product is not yet available or has no
        location, then the given exception class excclass is
        raised."""
        assert(logger is not None or not just_check)
        assert(isinstance(product,produtil.datastore.Product))
        (L,A)=(product.location,product.available)
        if L and A:
            if target is None: target=os.path.basename(L)
            if just_check:
                if isnonempty(L):
                    return True
                elif logger is not None:
                    msg='%s: file is empty or non-existent'%(L,)
                    logger.warning(msg)
                return False
            make_symlink(L, target, logger=logger,force=True)
            return True
        msg='%s: unavailable (available=%s location=%s)'%(
            str(product.did),repr(A),repr(L))
        if logger is not None: logger.warning(msg)
        if just_check: return False
        raise excclass(msg)

#######################################################################################

# Modeled after fcsttask.WFRInput2WRF(Input2Fcst).
class WRFInput2FinalMerge(Input2FinalMerge):
    """Links real_nmm gfsinit/realinit/wrfinput_d01 file or returns the FileProduct,
    regardless of the time and domain, in the current directory for
    a final merge."""
    def __init__(self,stormid,src):
        super(WRFInput2FinalMerge,self).__init__(src)
        self.stormid=stormid
    @property
    def tgtname(self):
        #return 'wrfinput_d01_'+self.stormid+'realinit.nc'
        return 'wrfinput_d01'
    def get_inputs(self,logger,just_check=False, **kwargs):
        if self.src is not None:
            p=self.src.get_wrfinput(self.stormid)
            tgt=self.tgtname
            if p:
                return self.link_product(p,WRFInputMissing,logger,
                                         target='wrfinput_d01',
                                         just_check=just_check)
            return False

class Merge2FinalMerge(Input2FinalMerge):
    """Links gdas_merge/wrfinput_d01 file or returns the FileProduct,
    regardless of the time and domain, in the current directory for
    a final merge."""
    def __init__(self,stormid,src): 
        super(Merge2FinalMerge,self).__init__(src)
        self.stormid=stormid
    @property
    def tgtname(self):
        return 'wrfinput_d01_'+self.stormid+'gdas_merge.nc'
    def get_inputs(self,logger,just_check=False, stormid=None, **kwargs):
        if stormid is not None and stormid!=self.stormid:
            if logger is not None:
                logger.error(
                    'Wrong stormid requested: %s instead of %s, cannot link.'
                    %(stormid,self.stormid))
            return False
        if self.src is not None:
            tgt=self.tgtname
            p=self.src.get_merge(self.stormid)
            if p:
                return self.link_product(p,WRFInputMissing,logger,
                                         target=tgt,just_check=just_check)
            return False

#######################################################################################

class FinalMergeTask(FcstTask):
    """This is a FcstTask since it prepares input to a forecast model. 
    The FcstTask class provides some methods required by the final merge task.
    This task merges all the domain 01 files from the relocation.Merge(RelocationTask)
    of each storm and produces a single d01 merged file that is comprise of all the
    storms in a multstorm."""

    def __init__(self,dstore,conf,section,taskname=None,**kwargs):

        super(FinalMergeTask,self).__init__(dstore,conf,section,taskname=taskname,
                                           **kwargs)
        #taskname=self.taskname
        # Just making sure.
        if self.isfakestorm:
            self.fake_stormid = conf.getstr('config','fakestormid')
            self.real_stormids = conf.getstr('config','multistorm_sids').split()
            self.priority_stormid = conf.getstr('config','multistorm_priority_sid')
        else:
            raise hwrf.exceptions.InvalidConfigOptName(
                        "Multistorm run: ERROR Can only run "
                        "finalmerge on the fakestorm of a multistorm run.")

        self.dest_dir=self.getdir('WORKhwrf')

        # Define the product (filename) this task is creating, 
        # in the Products Table of the database.
        # The final merge file will be specified in the intercom directory since it will
        # be shared by other storms.

        # TODO: CRITICAL, For now keeping the finalmerged file filename at wrfinput_d01.
        # which starts out as the copy of <prioritySID>/gfsinit/realinit/wrfinput_d01
        # in finalmerge/wrfinput_d01 but ends up being  the "final merged" file.
        self.fmprodname = 'wrfinput_d01'

        with dstore.transaction() as t:
            self._prod_wrfinput_d01_finalmerge=FileProduct(dstore,self.fmprodname,
                self.taskname, location=os.path.join(self.outdir,self.fmprodname))
    # Sam says:
    # Use self.merge_inputs and self.real_input to store locations of 
    # files in other storms' workflows.  I don't know where in this class
    # you grab the wrfinput files, but you would have to change it to grab
    # the files from the self.merge_inputs and self.real_input
    #     self.merge_inputs=list()
    #     self.real_input=None
    # def add_merge_input(filename):
    #     self.merge_inputs.append(UpstreamFile(self.dstore,self.conf,self.taskname,location=self.confstrinterp(filename)))
    #     return self
    # def set_real_input(filename)
    #     self.real_input=UpstreamFile(self.dstore,self.conf,self.taskname,location=self.confstrinterp(filename))
    #     return self
    # def wait_for_things(self):
    #     plist=list(self.merge_inputs)
    #     plist.append(self.real_input)
    #     wait_for_products(plist,self.log())
    def update_state(self):
        self.state=COMPLETED

    def wrfinput_at_time(self,atime,domain):
        return self._prod_wrfinput_d01_finalmerge

    def run(self):
        """Performs all work needed to run the Final Merge program and create
        the final merged product. Creates the work directory, CD's to it,
        Creates a domain mapping  dictionary for all the storms. ,link_all_inputs,
        run_exe, and deliver_products."""

        logger=self.log()
        # NOTE: some of these are sent as postmsg instead of
        # logger.info.  That ensures they are in the jlogfile, which
        # contains information across all cycles of all storms and all
        # models.  That way, we can find unexpected cold starts.

        self.postmsg('Final Merge Task running in directory: '+os.getcwd())

        # WORKhwrf/intercom/finalmerge
        produtil.fileop.makedirs(self.outdir)

        runhere=self.workdir
        self.keeprun = True
        if os.path.exists(runhere):
            logger.warning('%s: directory exists; will delete'%(runhere,))
            assert(not os.path.samefile(self.getdir('WORKhwrf'),runhere))
            shutil.rmtree(runhere)
        # jtfst  rundir ?
        with NamedDir(runhere,keep=self.keeprun,logger=logger,
                      keep_on_error=True) as rundir:
            try:
                self.state=RUNNING
                logger.info('%s running in directory %s'%(
                        self.taskname,realcwd()))

                # Copies all required input file in to the finalmerge working directory.
                self.copy_inputs()

                gfsrealinit, gdas_merge = self.convert_wrfinput2bin()
                self.final_merge(gfsrealinit, gdas_merge)
                self.finalmerge2netcdf()

                # self._prod_wrfinput_d01_finalmerge.deliver(
                #    frominfo=self.fmprodname,location=os.path.join(
                #    self.outdir,self.fmprodname))

                self.deliver_products()
                self.update_state()
                self._prod_wrfinput_d01_finalmerge.available=True

            except Exception as e:
                self.state=FAILED
                logger.critical('%s failed: %s'%(self.taskname,str(e)),
                                exc_info=True)
                raise
        self.update_state()
        self.postmsg('%s: completed'%(self.taskname,))


    # =========================================================================
    # The hwrf_final_merge program input requires the wrfinput_d01 input files
    # to be binary, not NetCDF. So they must be converted.
    #
    # The first storm in the list is considered the priority storm.
    # Converts the 'gfsinit/realinit' task's wrfinput_d01 file from netcdf to binary.
    # Converts the 'gdas_merge' task wrfinput_d01 files for all storms in the
    # multistorm from netcdf to binary.
    # ==========================================================================
    def convert_wrfinput2bin(self):
        """Runs the hwrf_diffwrf_3dvar program to convert all the required
        wrfinput_d01 files from netcdf to binary.

            returns a tuple (gfsrealinit_bin, gdas_merge_bins)
                with the the binary filenames that will be used
                as input for the final merge executable.
        """
        self.log().info('convert_wrfinput_d01_2bin')
        logger=self.log()
        fprog = 'hwrf_3dvar'
        prog = self.getexe(fprog)

        # TODO: CRITICAL, QuickImpl section Setting up links and paths - needs rework and hardening <jtf>
        #common to all storms, removes slash and sid from WORKhwrf, '/04E'
        commonWORKhwrf = self.getdir('WORKhwrf')[:-4]

        # Setup sym links from this FinalMergeTask workdir to each storms

        # Create the priority storm gfsinit/realinit sym links and .nc and .bin filenames.
        gfsrealinit_nclinkname='wrfinput_d01_'+self.priority_stormid+'gfsrealinit.nc'
        gfsrealinit_ncfile=os.path.join(self.workdir,'wrfinput_d01')
        gfsrealinit_bin='wrfinput_d01_'+self.priority_stormid+'gfsrealinit.bin'
        make_symlink(gfsrealinit_ncfile,gfsrealinit_nclinkname,force=True, logger=logger)

        fin_target = gfsrealinit_nclinkname
        fout_target = gfsrealinit_bin

        # Convert gfsrealinit netcdf to binary.
        log = '%s/logs/%s_%s_%s.log' %(
            self.dest_dir, self.__class__.__name__, fprog, 'gfsrealinit2bin'+self.priority_stormid)
        cmd = produtil.run.exe(prog)['storm_relocate', fin_target, 'flnm3', fout_target]
        if self.redirect: cmd = cmd >= log
        produtil.run.checkrun(cmd,logger=logger)

        print produtil.listing.Listing()
        # For all the gdas_merge files, in the finalmerge.workdir,
        # Create symlinks to the .nc files and generate the binary files.
        # /intercom/gdas_merge/wrfinput_d01
        gdas_merge_bins = []
        for stormid in self.real_stormids:
            gdasmerge_nclinkname = os.path.join(self.workdir, 'wrfinput_d01_'+stormid+'gdas_merge.nc')
            #gdasmerge_ncfile = os.path.join(commonWORKhwrf,stormid,'intercom/gdas_merge/wrfinput_d01')
            #make_symlink(gdasmerge_ncfile,gdasmerge_nclinkname,force=True, logger=logger)

            # Use the basename instead of the absolute path. hwrf_diffwrf_3dvar input
            # path/filename <= 124 characters.  Since this task is running in the
            # finalmerge directory using the basename should be ok.
            fin_target = os.path.basename(gdasmerge_nclinkname)
            fout_target = 'wrfinput_d01_'+stormid+'gdas_merge.bin'
            gdas_merge_bins.append(fout_target)

            log = '%s/logs/%s_%s_%s.log' %(
                self.dest_dir, self.__class__.__name__, fprog, 'gdas_merge2bin'+stormid)

            cmd = produtil.run.exe(prog)['storm_relocate', fin_target, 'flnm3', fout_target]
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=logger)

        return gfsrealinit_bin, gdas_merge_bins

    def finalmerge2netcdf(self):
        """Runs the hwrf_diffwrf_3dvar program to convert the final
        merge binary file to netcdf."""
        self.log().info('convert_wrfinput_d01_2bin')
        logger=self.log()
        fprog = 'hwrf_3dvar'
        prog = self.getexe(fprog)


        # Note: script that this command was modeled after had created another fort.56 link
        # fort.56 --> self.fmprodname+'.bin', seems like that is not required, but I'll also.
        make_symlink(self.fmprodname+'.finalmerge.bin','fort.56',force=True, logger=logger)
        # Do not join self.workdir to fin_target or fout_target files. There is a 124 character limit
        # to the hwrf_diffwrf_3dvar executables input, output path/filename
        fin_target = 'fort.56'
        fout_target = self.fmprodname
        #fin=os.path.join(self.workdir,'fort.56')
        #fout=os.path.join(self.workdir,self.fmprodname)
        log = '%s/logs/%s_%s_%s.log' %(
            self.dest_dir, self.__class__.__name__, fprog, 'finalmerge2nc'+self.fake_stormid)
        cmd = produtil.run.exe(prog)['3dvar_update',fout_target, fin_target]
        if self.redirect: cmd = cmd >= log
        produtil.run.checkrun(cmd,logger=logger)


    # The output of the final merge exe is a binary file and
    # that needs to be converted to netcdf.
    def final_merge(self, gfsrealinit_bin, gdasmerge_bins):
        """Runs the hwrf-utilities hwrf_final_merge program with
        the following inputs and generates the final merge output file,
        all in this tasks self.workdir .

            gfsrealinit_bin - the name of the d01 priority storm
                              gfsrealinit binary filename

            gdasmerge_bins - a list of gdas_merge d01 binary
                             filenames for each real storm"""

        self.log().info('final_merge')
        fprog = 'hwrf_final_merge'
        logger=self.log()
        prog = self.getexe('hwrf_final_merge')
        numstorms = len(self.real_stormids)
        gesfhr = 6


        # fort.nn links used are Based on the hwrf_final_merge.f90 source file.
        # The write is IUNIT=50+ITIM, where ITIM is the passed in gesfhr = 6. So fort.56
        # fort.40 is the gfsinit realinit d0 binary file
        # fort.41 ++ gdas_merge d0 binary file incremented for each storm in multistorm.

        # gesfhr is always 6
        evars = [ gesfhr,
                  numstorms ]

        ins = { 40:gfsrealinit_bin }
        for i, gdas_merge_bin in enumerate(gdasmerge_bins):
            ins[i+41] = gdas_merge_bin

        # This fort.56 link is broken, until after run_ext generates self.fmprodname
        ous = { 56:self.fmprodname+'.finalmerge.bin' }

        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def products(self,**kwargs):
        """Iterates over all products generated by this task."""
        yield self._prod_wrfinput_d01_finalmerge

    # Modeled from fcstask.WRFTaskBase.add_wrfinput.
    # Currently used for just adding gfsinit/realinit/wrfinput_d01
    # from the gfs_init.realinit object in the hwrf_expt.py
    def add_wrfinput(self,r,stormid):
        """Adds an input source (via self.add_input) that will provide
        the wrfinput output file from real_nmm.  The given object must
        have a wrfinput_at_time(atime,domain) function that returns a
        Product for a given analysis time and WRFDomain object.
        Returns self."""
        # TODO: think, Do I need to return , not in this case <jtf>
        # return self.add_input('wrfinput',WRFInput2FinalMerge(r))
        self.add_input('wrfinput',WRFInput2FinalMerge(stormid,r))

    def add_merge(self,r,stormid):
        """Adds an input source (via self.add_input) that will provide
        the wrfinput output file from a prior gdas_merge, real_nmm or
        relocation.  This is used for the storm's final output of the
        per-storm initialization."""
        return self.add_input('merge',Merge2FinalMerge(stormid,r))

    def deliver_products(self,missing=None,logger=None,keep=False,
                         frominfo=None,**kwargs):
        """Delivers products to intercom via Product.deliver.  Any
        keyword arguments are passed on to Product.deliver.  By
        default, keep=False, which means the local copy of the file
        may no longer exists.  If frominfo is specified, it will be
        ignored."""
        if logger is None: logger=self.log()
        logger.warning('Delivering products for %s'%(self.taskname,))
        #TODO: CRITICAL Look in to Had to change dir was using self.location
        produtil.fileop.makedirs(self.outdir,logger=logger)


        # TODO: CRITICAL Hmmmm Look in to why yield no p, products in self.products()
        # for p in self.products():
        #    loc=p.location
        p=self._prod_wrfinput_d01_finalmerge
        loc=p.location
        bloc=os.path.basename(loc)
        if os.path.exists(bloc):
            logger.warning('%s: deliver product from ./%s'%(p.did,bloc))
            p.deliver(frominfo=bloc,keep=keep,logger=logger,**kwargs)
        else:
            logger.warning('%s: ./%s does not exist.  Cannot deliver.'
                           %(p.did,bloc))
            if missing is not None:
                missing(p,bloc)

    # This copies all the inputs needed by the final merge task from the 
    # com directory of each storm to the final_merge working diretory.
    # copy_inputs and _make_plist_and_names is used to copy a file from
    # self.outdir = intercom/finalmerge to a products self.workdir. copy_inputs
    # reads the FileProduct availability flag in the datastore. Once it is
    # available |1| it copies the  from location specified in the datastore to
    # the workdir. The products in this list "plist", are products this finalmerge
    # task is waiting on. It is another Task outside of this task, that is generating and
    # updating the available flag for the product. This task is just waiting for
    # the products to become available so this task can do its work.
    # For example, FinalMerge is waiting on and needs to copy wrfinput_d01 from
    # <prioritySID>/intercom/gfsinit/realinit/wrfinput_d01 to <fakstormSID>/finalmerge.
    # For right now ... that is all final merge is using it for.
    # TODO: Change the following and implement correctly. <jtf>
    # At the moment The finalmerge initialization sets all the products
    # needed to run, both the finalmerge and WRF to |1|, in order for the links
    # to be generated. Products are assumed to be there and workflow product
    # dependencies are controlled with rocoto.

    # From relocate.RelocationTask.copy_inputs.
    def copy_inputs(self):
        """Copies, or makes, one or more input files."""
        logger=self.log()
        (plist,names,action)=self._make_plist_and_names()
        def namer(p,logger,*args):      return names[p]
        def actor(p,name,logger,*args): action[p](p,name,logger,*args)

        # Loop over all provided products, wait for them, and copy
        # them:
        # TODO: Assess I was using this, but commented out after Sam's
        # mods. This kept waiting for products realinit/wrfinput_d01
        produtil.datastore.wait_for_products(plist,logger,namer,actor)

    # Sets all the inputs needed for the final merge task.
    # plist is list of {FileProduct} objects that you set in the names dictionary.
    # ie. So this will copy wrfinput_d01.location to finalmerge.working dir.
    # From relocate.RelocationTask._make_plist_and_names.
    def _make_plist_and_names(self):
        """This is an internal implementation function that should not
        be called directly.  It returns a three-element tuple
        containing a list of products, and a dict mapping from product
        to the local filename, and a dict mapping from product to the
        copy method.  This is used to implement copy_inputs, to copy
        input files to the local directory from remote tasks'
        Products."""
        def copier(p,name,logger,*args):
            deliver_file(p.location,name,logger=logger,keep=True)
        def linker(p,name,logger,*args):
            make_symlink(p.location,name,force=True,logger=logger)

        # the names dictionary holds all the required input
        # as UpstreamFile objects.
        # The keys are the names of the files as they will be copied
        # to in the 00L/finalmerge work diretory.
        # ie. wrfinput_d01, wrfinput_d01_04Egdas_merge.nc, wrfinput_d01_05Egdas_merge.nc
        names=dict()

        # Sets the priority storms gfsinit/realint wrfinput_d01.
        # self._wrfinput is and UpstreamFile with a .location of
        # com/../04E.2012071000.multistorm.wrfinput_d01, for example.
        self._wrfinput_d01=self.inputs['wrfinput'][0].src.get_wrfinput_gfsinit_realinit()
        names[self._wrfinput_d01]='wrfinput_d01'

        # Sets all the gdas_merge files for each storm.
        # ie. com/../04E.2012071000.multistorm.wrfinput_d01_04Egdas_merge.nc
        for merge in self.inputs['merge']:
            mergeprod=merge.src.get_merge(merge.stormid)
            assert(isinstance(mergeprod,produtil.datastore.Product))
            tgtname=merge.tgtname
            self.log().info('Product %s location %s going to %s'%(
                    mergeprod.did,mergeprod.location,tgtname))
            names[mergeprod]=tgtname

        #names[self._prod_wrfinput_d01_finalmerge]='wrfinput_d01_00L_04Efm.nc'
        #names[self._ghost_d02]='wrfghost_d02'
        plist=[ k for k in names.iterkeys() ]
        actions=dict( (n,copier) for n in names.iterkeys() )
        return ( plist, names, actions )

    # This method is just for local development and testing in which
    # you can't run any of the binaries.... It should never be called
    # in production.
    # Setup a fake merged output file
    # Edit fmfile path as needed so it referes to where the fakefile exists.
    # self.fmprodname is just a dummy text file.
    def _fake_merge_file(self, sleep_secs):
        """Create a link to a phony final merge file for initial dev testing.
        without having to run the final merge executable"""

        logger = self.log()
        import time
        logger.info('fake_merge_file going to sleep for %s seconds'%sleep_secs)
        time.sleep(sleep_secs)

        fmlinkname = os.path.join(self.workdir,self.fmprodname)

        # absolute paths
        #fmfile = os.path.join(self.getdir('WORKhwrf'),'finalmerge.t',self.fmprodname)

        # relative paths, should test where you are.
        fmfile = os.path.join('../finalmerge.t',self.fmprodname)

        make_symlink(fmfile,fmlinkname,force=True, logger=logger)


    def run_ext(self, cmd, echo=None, inputs=None, incopies=None,
                outputs=None, opt_outputs=None):
        """Run an external command linking in fort.X files for input
        and output.  If redirect=True, redirect logs to a separate file.

        It will use "self.getexe()" on the command to find the
        external program to execute.

        If a list is passed in as the echo variable, then /bin/echo
        will be run with that list and piped to the external command.

        The inputs dictionary should consist of a fortran file
        number and the source file, such as:
           inputs = {11:tcvitals, 12:wrfout_d01}
        would produce symbolic links:
           fort.11 -> tcvitals
           fort.12 -> wrfout_d01
        input files can also be copied using incopies:
           incopies = {11:tcvitals, 12:wrfout_d01}
        would create files instead of links.

        The outputs and opt_outputs (optional outputs) should be of the
        dictionary as the inputs. As in:
           outputs = {56:new_data_4x, 85:storm_radius}
        this would mean the "fort.56" file would be renamed to "new_data_4x"
        and the "fort.85" renamed to "storm_radius".

        If opt_outputs is given then the fortran file is tested to see if it
        exists and only if it does is it renamed to the output filename.

        A log file will be created consisting of the stdout and stderr of the
        command run. It will be named consisting of the taskname and command.
        For example, if this is relocation stage 1 and the command is
        hwrf_pert_ct then the log file is "rel_stage_1_hwrf_pert_ct.log" """

        cmdname=str(cmd)
        logger = self.log()
        prog   = self.getexe(cmdname)
        logf   = '%s/logs/%s_%s.log' %(self.dest_dir,
                                       self.__class__.__name__, cmdname)

        # Build up the command
        if echo:
            echostr=""
            for s in echo:
                if isinstance(s,float): echostr+="%g "%(s,)
                elif isinstance(s,int): echostr+="%d "%(s,)
                else:                   echostr+="%s "%(str(s),)
            logger.info(
                'Converted %s to %s for stdin input to fortran command.'
                %(repr(echo),repr(echostr)))
            echostr+="\n"
            cmd = produtil.run.openmp(produtil.run.bigexe(prog)) << echostr
        else:
            cmd = produtil.run.openmp(produtil.run.bigexe(prog))

        # If redirection is requested, do so:
        if self.redirect: cmd = cmd >= logf

        # Clean up all the fortran inputs and outputs
        empty={}
        if inputs is None: inputs=empty
        if outputs is None: outputs=empty
        if incopies is None: incopies=empty
        iof = dict(itertools.chain(inputs.items(), outputs.items(),
                                   incopies.items()))
        for k in iof:
            produtil.fileop.remove_file('fort.'+str(k),logger=logger)

        # Link the inputs
        if inputs:
            produtil.fileop.fortlink(inputs, force=True,logger=logger)

        if incopies:
            produtil.fileop.fortcopy(incopies, force=True,
                                     only_log_errors=True, logger=logger)

        # Run the command
        logger.warning(repr(cmd)) # use logger.warning so it is in stderr
        produtil.run.checkrun(cmd, logger=logger)

        # Rename the outputs
        if outputs:
            for k, v in outputs.iteritems():
                ffile='fort.'+str(k)
                if os.path.exists(ffile):
                    deliver_file(ffile, v, keep=False,logger=logger)
                else:
                    logger.error('%s: did not make file %s (would mv to %s)'
                                 %(cmdname,ffile,str(v)))

        # Rename the optional outputs if they exist
        if opt_outputs:
            for k, v in opt_outputs.iteritems():
                ffile = 'fort.' + str(k)
                if os.path.exists(ffile):
                    deliver_file(ffile, v, keep=False,logger=logger)
                else:
                    logger.warning(
                        '%s: did not make file %s (would mv to %s).'
                        %(cmdname,ffile,str(v)))

        # Clean up the input links
        for k,v in inputs.iteritems():
            if os.path.islink('fort.'+str(k)):
                logger.info('%s: deleting input fort file (symlink to %s)'
                            %('fort.'+str(k),v))
                produtil.fileop.remove_file('fort.'+str(k),logger=logger)

        # Clean up the input copies
        for k,v in incopies.iteritems():
            if os.path.exists('fort.'+str(k)):
                logger.info('%s: deleting input fort file (copy of %s)'
                            %('fort.'+str(k),v))
                produtil.fileop.remove_file('fort.'+str(k),logger=logger)



    # TODO: Update and use this run_exe for the final merge task or get rid of it. <jtf>
    # This run_exe is from fcsttask.py WRFTaskBase(FcstTask)
    # Currently is not being used.
    def run_exe(self,exename='final_merge',runner=None,sleeptime=None):
        """Runs the executable this task is responsible for running.
        Determines if the program ran correctly.  The exename is the
        name of the argument in the [exe] section of the
        HWRFConfig.  Options:

          runner=Runner - pass a produtil.prog.Runner object if
          desired.  This overrides any decision of what to run: the
          exename will be ignored, and whatever is supplied in runner 
          is simply passed to produtil.run.run.
          sleeptime - passed to produtil.run.run to determine how
          often to check the child process.  By default, the sleeptime
          option in this task's config section is used, or if that is
          absent, 30 seconds."""
        if sleeptime is None:
            sleeptime=self.conffloat('sleeptime',30)
        logger=self.log()
        if runner is None:
            exe=self.getexe(exename)
            runner=produtil.run.exe(exe)
        stat=produtil.run.run(runner,logger=logger,sleeptime=sleeptime)
        logger.info('%s: exit status %d'%(exename,stat))
        if not check_last_lines('rsl.out.0000','SUCCESS COMPLETE',
                                logger=logger):
            msg='%s: did not see SUCCESS COMPLETE in rsl.out.0000'%(exename,)
            logger.error(msg)
            raise RealNMMError(msg)
        else:
            logger.info('%s: SUCCESS COMPLETE in rsl.out.0000'%(exename,))



