#! /usr/bin/env python3

##@namespace scripts.exhafs_fv3out_archive
# Archives native fv3out files to tape via the "htar" command.
#
# To enable this script, you must set the fv3out option in the [archive]
# configuration section:
# @code[.conf]
# [archive]
# fv3out=hpss:/NCEPDEV/emc-hwrf/2year/Jane.Doe/fv3out/{out_prefix}.tar
# @endcode
#
# The extension must be .tar, and the only archiving method supported is
# hpss:/ (which uses htar)

import os, sys, glob

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
import tcutil.numerics

from produtil.log import jlogger
from produtil.run import batchexe, checkrun, run

def main():
    environ_CONFhafs=os.environ.get('CONFhafs','NO_CONFhafs')
    conf=hafs.launcher.HAFSLauncher().read(environ_CONFhafs)
    if not conf.getstr('archive','fv3out',''):
        jlogger.info('No fv3out option in [archive] section.  Will not make fv3out archive.')
        sys.exit(0)

    logger=conf.log()

    files=list()
    WORKhafs=conf.getdir('WORKhafs','/can/not/find/WORKhafs/dir')
    forecastdir=os.path.join(WORKhafs,'forecast')
    with produtil.cd.NamedDir(forecastdir):
        files.append(glob.glob('*.nc'))
        thearchive=conf.timestrinterp('archive','{fv3out}',0)
        if thearchive[0:5]!='hpss:':
            logger.error('The fv3out archive path must begin with "hpss:": '+
                         thearchive)
            sys.exit(1)
        thearchive=thearchive[5:]
        adir=os.path.dirname(thearchive)
        mkdir=batchexe(conf.getexe('hsi'))['-P','mkdir','-p',adir]
        run(mkdir,logger=logger)
        cmd=batchexe(conf.getexe('htar'))['-vcpf',thearchive][files]
        checkrun(cmd,logger=logger)

if __name__=='__main__':
    try:
        produtil.setup.setup()
        jlogger.info("HAFS fv3out_archive job starting")
        main()
        jlogger.info("HAFS fv3out_archive job completed")
    except Exception as e:
        jlogger.critical('HAFS rundir archive is aborting: '+str(e),
                         exc_info=True)
        sys.exit(2)
