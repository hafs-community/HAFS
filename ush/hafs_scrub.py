#! /usr/bin/env python3

import logging, os, shutil, sys

if 'USHhafs' in os.environ:
    sys.path.append(os.environ['USHhafs'])
elif 'HOMEhafs' in os.environ:
    sys.path.append(os.path.join(os.environ['HOMEhafs'],'ush'))
else:
    guess_HOMEhafs=os.path.dirname(os.path.dirname(
            os.path.realpath(__file__)))
    guess_USHhafs=os.path.join(guess_HOMEhafs,'ush')
    sys.path.append(guess_USHhafs)

import hafs.launcher
import produtil.setup, produtil.fileop, produtil.log

from produtil.log import jlogger

##@namespace ush.hafs_scrub
# @brief A utility script that deletes directories, but adds safeguards and logging.
#
# This script deletes directories, and is called as follows:
# @code{.sh}
#  hafs_scrub.py /directory/one [/directory/two [...]]
#  hafs_scrub.py YES|NO  WORK|COM
#  hafs_scrub.py YES|NO /directory/one [/directory/two [...]]
# @endcode
#
# If WORK or COM is an argument then CONFhafs from the
# environment is used to read in stormN.conf and delete either 
# WORKhafs or com directory from the value in conf file.
#
# It will recursively delete up to thirty directories and will log
# messages as it goes.  It will refuse to delete the following
# directories:
# * /
# * any filesystem mount point
# * $USHhafs, $EXhafs, $PARMhafs, $HOMEhafs
# * $FIXgsi, $FIXhafs

class WillNotDelete(Exception):
    """!Raised by various safety checks if someone tries to delete
    something they should not, such as "/"."""

class Deleter(object):
    """!Recursive directory deleter with safeguards to prevent
    accidental deletion of certain critical directories."""
    def __init__(self,logger):
        """!Constructor for Deleter
        @param logger a logging.Logger for log messages"""
        self.__logger=logger
        self.__rmtrees=set()
        self.__rmdirs=set()
        self.badflag=False
    ##@var badflag
    # If True, then at least one directory had trouble being deleted

    @property
    def logger(self):
        """!Returns the logging.Logger used for log messages"""
        return self.__logger

    def validate_path(self,norm):
        """!Checks to see if the given path is one that should not be
        deleted.  

        Raises WillNotDelete if the given directory is one of these:
        * /
        * a mount point
        * Any of $HOMEhafs, $USHhafs, $EXhafs, $PARMhafs, $FIXhafs, or $FIXgsi

        @param norm the path to check"""
        if not os.path.exists(norm):
            return # cannot validate if it does not exist
        if os.path.ismount(norm):
            raise WillNotDelete('%s: is a mount point (fs root)'%(norm,))
        if os.path.samefile('/',norm):
            raise WillNotDelete('%s: is same as /'%(norm,))
        for var in ( 'HOMEhafs', 'USHhafs', 'EXhafs', 'PARMhafs', 
                     'FIXhafs', 'FIXgsi' ):
            if var in os.environ:
                vardir=os.environ[var]
                if vardir=='': continue
                if os.path.samefile(os.environ[var],norm):
                    raise WillNotDelete('%s: same as $%s'%(norm,var))

    def add(self,dirname):
        """!Adds a directory to the list to be deleted.  The directory
        is passed through various safeguards first.
        @param dirname the directory to delete"""
        norm=produtil.fileop.norm_expand_path(dirname,fullnorm=True)
        self.logger.info('%s: normalizes to %s'%(dirname,norm))
        self.validate_path(norm)
        self.logger.info('%s: will recursively delete this'%(norm,))

        parent=os.path.dirname(dirname)
        self.logger.info('%s: will rmdir this if it is empty'%(parent,))

        self.__rmdirs.add(parent)
        self.__rmtrees.add(dirname)

    def _rmtree_onerr(self,function,path,exc_info):
        """!Internal function used to log errors.

        This is an internal implementation function called by
        shutil.rmtree when an underlying function call failed.  See
        the Python documentation of shutil.rmtree for details.
        @param function the funciton that failed
        @param path the path to the function that caused problems
        @param exc_info the exception information
        @post self.badflag=True
        @protected"""
        self.logger.warning('%s: %s failed: %s'%(
                str(path),str(function),str(exc_info)))
        self.badflag=True

    def rmtree(self,tree):
        """!Deletes the tree, if possible.
        @protected
        @param tree the directory tree to delete"""
        try:
            # If it is a file, special file or symlink we can just
            # delete it via unlink:
            os.unlink(tree)
            return
        except EnvironmentError as e:
            pass
        # We get here for directories.
        self.logger.info('%s: rmtree'%(tree,))
        shutil.rmtree(tree,ignore_errors=False,onerror=self._rmtree_onerr)

    def have_dirs(self):
        """!Are there any directories to delete (ones passed to add())
        @returns the number of directories to delete"""
        return len(self.__rmdirs)>0

    def swap_dirs(self):
        """!Returns the list of directories to delete and clears the
        internal list."""
        dirs=self.__rmdirs
        self.__rmdirs=set()
        return dirs

    def go(self,max_rmdir_loop=30):
        """!Deletes all directories sent to add()
        
        @param max_rmdir_loop The maximum number of directories to
        delete before returning.  This is a safeguard against
        accidents."""
        logger=self.logger
        logger.info('Delete files: first pass.')
        self.badflag=False
        for tree in self.__rmtrees:
            self.rmtree(tree)

        if self.badflag:
            logger.warning('Some deletions failed.  Will try again.')
            logger.info('Delete files: second pass.')
            self.badflag=False
            for tree in self.__rmtrees:
                if os.path.exists(tree):
                    self.rmtree(tree)
                else:
                    logger.info('%s: already gone.'%(tree,))

        if self.badflag:
            logger.error('Could not delete files after two tries.')

        logger.info('Remove parent and ancestor directories.')
        iloop=0
        while iloop<max_rmdir_loop and self.have_dirs():
            iloop+=1
            dirs=self.swap_dirs()
            for dir in dirs:
                try:
                    logger.info('%s: rmdir'%(dir,))
                    os.rmdir(dir)
                except EnvironmentError as e:
                    logger.info('%s: cannot remove: %s'%(dir,repr(e)))
                    continue
                try:
                    parent=os.path.dirname(dir)
                    self.__rmdirs.add(parent)
                except EnvironmentError as e:
                    logger.warning('%s: cannot determine parent'%(dir,))
                    continue # can ignore this error
        if iloop>=max_rmdir_loop:
            logger.warning(
                'Hit maximum loop count of %d.  Some ancestor directories '
                'may still exist.'%(max_rmdir_loop,))

    def add_tmpdir_check(self,parent_dir,child_dir):
        """!Simple check to determine if the child_dir should be scrubbed
           based on the number of entries in the parent directory. 
        
        @return Returns True if child_dir should be scrubbed. 
        @param parent_dir The parent direcory of child_dir.
        @param child_dir The directory that may be deleted.
        """

        # WORKhafs dir listing < 2 assume WORKhafs was previously scrubbed
        # parent_dir dir listing < 2 assume parent_dir was previously scrubbed
        # so lets clean up the child_dir.
        if os.path.isdir(parent_dir):
            if len(os.listdir(parent_dir)) < 2:
                if os.path.isdir(child_dir):
                    return True
        else:
            return False
                    
def main():
    """!Main program: parses arguments, sends them to Deleter.add() and calls Deleter.go()"""
    logger=logging.getLogger('hafs_scrub')
    scrubber=Deleter(logger)

    # NOTE:
    # Multistorm &WORKhafs;, &COMhafs; and &CONFhafs;  passed in from the entity 
    # file, are  always set to the fakestorm dir values since everything from
    # rocoto's perspective is running under the fakestorm.
    # However, the CONFhafs environment variable is not. It refers to the
    # correct stormN.conf file .../com/.../00L/stormN.conf

    # The HAFS_FORCE_TMPDIR is created (currently, WORKhafs/tmpdir) before 
    # this script is even launched. If WORKhafs dirs were deleted the 
    # tmpdir is recreated when running scrub com jobs regardless if yes or no.
    # For multistorm, the tmpdir is only created under the fakestorm.

    # The logic block below is meant to support the following calls.
    # hafs_scrub.py
    # hafs_scrub.py  /directory/one [/directory/two [...]]
    # hafs_scrub.py  YES|NO /directory/one
    # hafs_scrub.py  YES|NO WORK|COM
    # hafs_scrub.py  YES|NO /directory/one [/directory/two [...]]

    logger.info('The SCRUB ARGS ARE: %s'%sys.argv)

    if len(sys.argv)==1:
        logger.info('No arguments were passed to hafs_scrub.')
    elif len(sys.argv)==2:
        do_scrub=sys.argv[1].upper()
        if do_scrub=='YES'or do_scrub=='NO':
            logger.info('Only the scrub flag %s was passed to hafs_scrub, provide some dirs to scrub.'%(do_scrub))
        else:
            scrubber.add(sys.argv[1])
            scrubber.go()
    # hafs_scrub.py  YES|NO WORK|COM
    else:
        do_scrub=sys.argv[1].upper()
        scrub_job=sys.argv[2].upper()
        if scrub_job=='WORK' or scrub_job=='COM':
            #Create a conf object to determine WORKhafs and com location
            #for the real storms - this is required for multistorm.
            environ_CONFhafs= os.environ['CONFhafs']
            conf=hafs.launcher.HAFSLauncher().read(environ_CONFhafs)

            #These are needed to deal with the tmpdir that may be created
            #when the scrub_com task runs.
            tmpdir=os.environ.get('HAFS_FORCE_TMPDIR','NO_TMPDIR')
            environ_WORKhafs=os.environ.get('WORKhafs','NO_WORKhafs')

            if do_scrub=='YES':
                if scrub_job == 'WORK':
                    scrubdir='WORKhafs'
                    scrubber.add(conf.getdir(scrubdir))
                elif scrub_job == 'COM':
                    scrubdir='com'
                    scrubber.add(conf.getdir(scrubdir))
                    if scrubber.add_tmpdir_check(environ_WORKhafs,tmpdir):
                        scrubber.add(tmpdir)
                        logger.info('Scrub job: %s , Removing %s since it was created by this task.'%(scrub_job,tmpdir))
                logger.info('Used conf file to determine scrub dir: %s : %s'%(scrubdir,environ_CONFhafs))
                scrubber.go()
            else: 
                if scrub_job == 'COM':
                    if scrubber.add_tmpdir_check(environ_WORKhafs,tmpdir):
                        scrubber.add(tmpdir)
                        logger.info('Scrub job: %s , Removing %s since it was created by this task.'%(scrub_job,tmpdir))
                        scrubber.go()
                logger.info('Scrub job: %s , Not scrubbing since arg for scrub action is: %s'%(scrub_job,do_scrub))

        # hafs_scrub.py  YES|NO /directory/one [/directory/two [...]]
        elif do_scrub=='YES':
            for arg in sys.argv[2:]:
                scrubber.add(arg)
            scrubber.go()
            
        elif do_scrub=='NO':
            logger.info('Scrub job: %s , Not scrubbing since arg for scrub action is not YES: %s'%(scrub_job,do_scrub))
        # hafs_scrub.py  /directory/one [/directory/two [...]] 
        else:
            for arg in sys.argv[1:]:
                scrubber.add(arg)
            scrubber.go()
  

if __name__=='__main__':
    try:
        produtil.setup.setup()
        main()
    except Exception as e:
        jlogger.critical('HAFS scrubber is aborting: '+str(e),exc_info=True)
        sys.exit(2)

