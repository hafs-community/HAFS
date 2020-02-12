#! /usr/bin/env python

import os, sys, glob, shutil, fileinput, datetime, re

import hwrf.hycom
import produtil.setup,  produtil.run, produtil.fileop, produtil.cd, produtil.batchsystem
from produtil.fileop import remove_file, make_symlink, chdir, fortlink, deliver_file, makedirs

from produtil.run import run, exe, openmp

import logging

logger=logging.getLogger("hmon")

def grep_file(rstr, infile, outfile, ignorecase):
    with open (outfile, 'w') as f:
        with open (infile, 'r') as myfile:
            for line in myfile.readlines():
                if re.search(rstr, line, ignorecase):
                    f.write(line)

def delete_file(file):
    for f in glob.glob(file):
        remove_file(f)

def removeall(file):
    for f in glob.glob(file):
        if os.path.isdir(file):
            if os.path.isdir(file)!=[]:
                shutil.rmtree(file, ignore_errors=True)
        else:
            remove_file(f)

def rm_glob(*args):
    for a_glob in args:
        logger.info("rm "+a_glob)
        for match in glob.glob(a_glob):
            if os.path.isdir(match):
                shutil.rmtree(match, ignore_errors=True)
            else:
                produtil.fileop.remove_file(match,logger=logger)

def make_symlinks(source, destination):
    sources=[]
    for file in glob.glob(source):
        if not(os.path.islink(file)):
            sources.append(file)
    produtil.fileop.make_symlinks_in(sources, destination, True, logger=logger)

def hmon_pgm(logger,pgm,*args):
    strargs = [str(arg) for arg in args]
    strargs+="\n" 
    produtil.run.checkrun(exe(pgm) << (" ".join(strargs)),logger=logger)

def nems_relocate(logger, itagId, hwrfin, gfs, domainId):
    logger.info("nems_relocate " + str(itagId) + " " + str(hwrfin) + " " +  str(gfs) + " " + str(domainId) )
    itagfile='itag%01d'%itagId
    with open(itagfile,'w') as f:
        f.write(hwrfin + '_d%02d\n'%domainId)
        f.write('relocate\n')
        f.write(gfs+'_d%02d\n'%domainId)
        f.close()
    print os.getcwd()
    ofile = 'stdout_wrfges%01d'%itagId
    cmd=openmp(exe("./nems_bin_io.exe"),threads=1).env(KMP_AFFINITY='disabled',KMP_STACKSIZE='2G',IOBUF_PARAMS='*:size=8M:count=32:vbuffer_count=4096:prefetch=1')
    produtil.run.checkrun((cmd<itagfile)>ofile, logger=logger)

def nems_update(logger, itagId, hwrfin, gfs, domainId):
    logger.info("nems_update " + str(itagId) + " " + str(hwrfin) + " " +  str(gfs) + " " + str(domainId) )
    itagfile='itag%01d'%itagId
    with open(itagfile,'w') as f:
        f.write(hwrfin + '_d%02d\n'%domainId)
        f.write('update\n')
        f.write(gfs+'_d%02d\n'%domainId)
        f.close()
    ofile = 'stdout_wrfges%01d'%itagId
    cmd = openmp(exe("./nems_bin_io.exe"),threads=1).env(KMP_AFFINITY='disabled',KMP_STACKSIZE='2G',IOBUF_PARAMS='*:size=8M:count=32:vbuffer_count=4096:prefetch=1') < itagfile
    cmd = cmd>=ofile ## the >+ redirects stdout and stderr
    produtil.run.checkrun(cmd, logger=logger)

def update_3dvar(logger, id):
    """!Runs the hwrf_diffwrf_3dvar program to update the output
    domains."""

    logger.info("update_3dvar" + " " + str(id))
    fprog = 'hwrf_3dvar'

    prog="./hwrf_diffwrf_3dvar.exe"

    cmd = produtil.run.exe(prog)["3dvar_update", 'wrfghost_d%02d'%id ,'data_merge_g%02d'%id]
    produtil.run.checkrun(cmd>"out.hwrf_relocate",logger=logger)

def relocate_storm(logger, id):
    """!Runs the hwrf_diffwrf_3dvar program on all inputs to
    create binary file for input to the Fortran programs."""
    logger.info("storm_relocate" + " " + str(id))

    fprog = 'hwrf_3dvar'

    prog="./hwrf_diffwrf_3dvar.exe"
    cmd = produtil.run.exe(prog)["storm_relocate", 'wrfout_d%02d'%id ,"flnm3", 'old_hwrf_d%02d'%id]
    produtil.run.checkrun(cmd>"out.hwrf_relocate",logger=logger)

def create_limits(logger, spinstart, cyc):
#convert start date (spinstart, e.g. 2016-07-11 18:00:00) and end date (cyc, e.g.2016-07-16 00:00:00 ) to hycom date and save them into a file named limits
# need to load "module load intel mvapich2" before running this function
# need to import hycom and datetime as well "import hwrf.hycom datetime"
    logger.info("create limits for time span: " + spinstart + " to " + cyc)
    hycom_epoch=datetime.datetime(1900,12,31,0,0,0)
#print hycom_epoch
    with open('limits','wt') as limitf:
        limitf.write('  %f %f false false  \n'%(
        float(hwrf.hycom.date_normal2hycom(spinstart)),
        float(hwrf.hycom.date_normal2hycom(cyc))))

def date_hycom2normal(logger, date):
#convert hycom date to normal date
    date = str(hwrf.hycom.date_hycom2normal(date))
	
    yyyy = date[0:4]
    mm =date[5:7]
    dd = date[8:10]
    hh= date[11:13]
    date = yyyy + mm + dd + hh
    logger.info(date)

    return date                 

def date2jday(logger, date):
    """!Convert YYYYMMDD dates to YYYYJJJ or back again.  The day of
    year (JJJ) is one-based: 1 is January 1, 2 is January 2, etc."""
    strlen = len(date)
    if strlen == 7:
        yyyyjjj = str(int(date,10)) #stay at 1-based for strptime
        dateobj=datetime.datetime.strptime(yyyyjjj,"%Y%j")
        return dateobj.strftime("%Y%m%d")

    elif strlen == 8:
        dateobj=datetime.datetime.strptime(date,"%Y%m%d")
        yyyyddd=int(dateobj.strftime("%Y%j"),10)
        yyyyjjj=yyyyddd
        return '%07d'%yyyyjjj

    else:
        raise ValueError('date2jday needs a 7-char or 8-char string like YYYYMMDD or YYYYJJJ')
		


