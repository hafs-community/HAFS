#! /usr/bin/env python3

##@namespace scripts.exhafs_archive
# Generates an archive file from HAFS COM directory outputs.  This
# archive file can be on disk, or on the HPSS archiving system (via
# the htar command).
#
# How this is done depends on the [config] section archive option, in
# the HAFS configuration files:
#
# * archive=none --- no archiving is done.  This script will exit
#   immediately without doing anything.
#
# * archive=disk:/path/to/archive.tar.gz --- make a gzipped tar file via
#   the tar -czf command, and place it on disk in a long-term storage area
#
# * archive=hpss:/path/to/archive.tar --- use the "htar" command to
#   place an archive on tape.
#
# * archive=hpsz:/path/to/archive.tar.gz --- a two-step process.  The
#   first step uses tar -czf to create an on-disk archive in a staging
#   area.  The second step copies that archive to tape using "hsi put"

import sys, os, glob

if 'USHhafs' in os.environ:
    sys.path.append(os.environ['USHhafs'])
elif 'HOMEhafs' in os.environ:
    sys.path.append(os.path.join(os.environ['HOMEhafs'],'ush'))
else:
    guess_HOMEhafs=os.path.dirname(os.path.dirname(
            os.path.realpath(__file__)))
    guess_USHhafs=os.path.join(guess_HOMEhafs,'ush')
    sys.path.append(guess_USHhafs)

import produtil.setup, produtil.log, produtil.run, produtil.cd
import hafs.launcher
from produtil.log import postmsg, jlogger
from produtil.run import batchexe, checkrun, run
from produtil.cd import NamedDir

def main_disk():
    """!Main program for disk archiving.  

    Creates an on-disk archiving for one of two cases:
    * disk:/path/to/archive.tar.gz --- generates an on-disk *.tar.gz
      archive in a long-term storage disk area
    * hpsz:/path/to/tape-archive.tar.gz --- generates an on-disk
      *.tar.gz archive in a temporary location so it can be copied to
      tape in a later step."""
    postmsg('hafs_archive disk step starting')
    environ_CONFhafs=os.environ.get('CONFhafs','NO_CONFhafs')
    #conf=hafs.launcher.HAFSLauncher().read(environ_CONFhafs)
    conf=hafs.launcher.load(environ_CONFhafs)
    logger=conf.log('archive.disk')
    if conf.has_section('archive'):
        makethedir=conf.getbool('archive','mkdir',False)
    else:
        makethedir=False
    archive=conf.getloc('archive','NONE')
    if archive=='NONE':
        logger.info('No archive location specified.  Exiting.')
        postmsg('hafs_archive disk step has nothing to do when archiving is '
                'disabled.')
        return
    with NamedDir(conf.getdir('com')):
        flist=[ filename+'\n' for filename in glob.glob('*') ]
        flist.sort()
        files=''.join(flist)
        assert(len(files)>0)
        if archive.lower()=='none':
            postmsg('Archiving is disabled: archive=none')
            return
        elif archive[0:5]=='disk:':
            path=archive[5:]
            if makethedir:
                adir=os.path.dirname(path)
                if not os.path.exists(adir):
                    produtil.fileop.makedirs(adir,logger=logger)
            flags='-cvp'
            if path[-3:]=='.gz' or path[-4:]=='.tgz':
                flags+='z'
            cmd=batchexe(conf.getexe('tar'))[flags+'f',path,'-T','-'] << files
        elif archive[0:5]=='hpss:':
            logger.info('HPSS archiving enabled.')
            logger.info('Nothing to do in the disk archiving step.')
            logger.info('Returning successfully after doing nothing.')
            postmsg('hafs_archive disk step does nothing when using htar '
                    'archives.')
            return
        elif archive[0:5]=='hpsz:':
            path=conf.strinterp('config','{WORKhafs}/stage-archive.tar.gz')
            flags='-cvp'
            if path[-3:]=='.gz' or path[-4:]=='.tgz':
                flags+='z'
            cmd=batchexe(conf.getexe('tar'))[flags+'f',path,'-T','-'] << files
        else:
            jlogger.error('Ignoring invalid archive method %s in %s'
                          %(archive[0:4],archive))
            return
        checkrun(cmd,logger=logger)
    postmsg('hafs_archive disk step completed')

def main_tape():
    """!Main program for tape archiving.  

    Does one of two things:

    * hpss:/path/to/archive.tar --- will use the htar command to
      archive COM directory outputs
    * hpsz:/path/to/archive.tar.gz --- will copy a tar.gz file from a
      temporary area, made by the disk archiving step, to a tape
      destination using the "hsi put" command."""
    postmsg('hafs_archive tape step starting')
    environ_CONFhafs=os.environ.get('CONFhafs','NO_CONFhafs')
    #conf=hafs.launcher.HAFSLauncher().read(environ_CONFhafs)
    conf=hafs.launcher.load(environ_CONFhafs)
    logger=conf.log('archive.tape')
    archive=conf.getloc('archive','NONE')
    if conf.has_section('archive'):
        makethedir=conf.getbool('archive','mkdir',False)
    else:
        makethedir=False
    if archive=='NONE':
        logger.info('No archive location specified.  Exiting.')
        postmsg('hafs_archive disk step has nothing to do when archiving is '
                'disabled.')
        return
    with NamedDir(conf.getdir('com')):
        flist=[ filename+'\n' for filename in glob.glob('*') ]
        flist.sort()
        files=''.join(flist)
        assert(len(files)>0)

        if archive.lower()=='none':
            postmsg('Archiving is disabled: archive=none')
            return
        elif archive[0:5]=='disk:':
            logger.info('Disk archiving enabled.')
            logger.info('Nothing to do in the HPSS archiving step.')
            logger.info('Returning successfully after doing nothing.')
            postmsg('hafs_archive tape step does nothing when using disk '
                    'archives.')
            return
        elif archive[0:5]!='hpss:' and archive[0:5]!='hpsz:':
            jlogger.error('Ignoring invalid archive method %s in %s'
                          %(archive[0:4],archive))
            return

        if makethedir:
            adir=os.path.dirname(archive[5:])
            logger.info('%s: make this HPSS directory, even if it exists'%(adir,))
            mcmd=batchexe(conf.getexe('hsi'))['-P','mkdir','-p',adir]
            run(mcmd,logger=logger)

        if archive[0:5]=='hpss:':
            path=archive[5:]
            flags='-cvp'
            cmd=batchexe(conf.getexe('htar'))[flags+'f',path,'-L','-'] << files
        elif archive[0:5]=='hpsz:':
            
            topath=archive[5:]
            frompath=conf.strinterp('config',
                                    '{WORKhafs}/stage-archive.tar.gz')
            cmd=batchexe(conf.getexe('hsi'))['put',frompath,':',topath]
        checkrun(cmd,logger=logger)
    postmsg('hafs_archive tape step completed')

if __name__=='__main__':
    try:
        acase=os.environ.get('ARCHIVE_STEP','BOTH').upper()
        produtil.setup.setup()
        if acase == 'DISK':
            main_disk()
        elif acase == 'TAPE':
            main_tape()
        elif acase == 'BOTH':
            main_disk()
            main_tape()
        else:
            postmsg('INVALID JHAFS_ARCHIVE STEP %s!! ABORTING!'
                    %(repr(acase),))
    except Exception as e:
        jlogger.critical('hafs_archive is aborting: '+str(e),exc_info=True)
        sys.exit(2)
