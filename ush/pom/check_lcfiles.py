#!/usr/bin/env python

##@namespace pom.check_lcfiles
#Checks loop current file timestamps.
#
#This module checks the datestamp in the Loop Current files and compare
#with the forecast start date. If the datestmp is 90 days older than
#forecast start date, it tells the ocean init scripts to not use
#them. Also note that if the datestamp in the Loop Current files later
#than forecast date, it tells the ocean init scripts to not not use
#them.
#
#@note Please report bugs/questions/comments to bijuthomas(at)mail(dot)uri(dot)edu.
#@author Biju Thomas, GSO, University of Rhode Island.
#@date August 5, 2014

from os.path import exists, getsize
from datetime import datetime
import logging

class LCUsefullness(object):
    """!Determines if loop current files are still usable for a cycle of interest."""
    def __init__(self,startdate, lfile, rfile, logger=None):
        """!Creates a new LCUsefullness object
        
        @param startdate the cycle of interest as a string, format YYYYMMDDHH
    @param lfile,rfile the loop current l and r files
    @param logger a logging.Logger for log messages."""
        super(LCUsefullness,self).__init__()
        self.startdate=startdate
        self.lfile=lfile
        self.rfile=rfile
        self.logger=logger
        if logger is None: logger=logging.getLogger('pom')
        self.result=None
        self.reason=None
        self.error=None
        self.result=None
        self.success=None

    def __str__(self):
        if self.result is None:
            return 'None'
        elif self.result:
            return self.success
        elif self.error:
            return "%s\n%s\n"%(self.reason,self.error)
        else: 
            return self.reason

    def __bool__(self):
        if self.result is None: run()
        return self.result

    def run(self):
        startdate=self.startdate
        lfile=self.lfile
        rfile=self.rfile
        logger=self.logger
        if exists(lfile) and getsize(lfile) > 0 and  \
            exists(rfile) and getsize(rfile) > 0:
            logger.info('LC: %s and %s exists' %(lfile,rfile))
            try:
                ldate = int(get_endline(lfile))
            except ValueError as e:
                self.reason="datestamp can not be accessed from %s" %(lfile)
                self.error="Error: %s" %(e)
                logger.error(self.reason)
                logger.error(self.error)
                self.result=False
            try:
                rdate = int(get_endline(rfile))
            except ValueError as e:
                self.reason="datestamp can not be accessed from %s" %(rfile)
                self.error="Error: %s" %(e)
                logger.error(self.reason)
                logger.error(self.error)
                self.result=False
            if ldate == rdate:
                if isinstance(startdate, str):
                    ymdh = startdate
                    try: 
                        t1 = datetime(int(ymdh[0:4]),int(ymdh[4:6]),int(ymdh[6:8]),int(ymdh[8:10]))
                    except ValueError as e:
                        self.reason="Invalid startdate format %s" %(startdate)
                        self.error="Error: %s" %(e)
                        logger.error(self.reason)
                        logger.error(self.error)
                        self.result=False
                    ymdh = str(ldate)
                    try:
                        t2 = datetime(int(ymdh[0:4]),int(ymdh[4:6]),int(ymdh[6:8]),int(ymdh[8:10]))
                    except ValueError as e:
                        self.reason="Invalid LC datestamp format in %s" %(str(ldate))
                        self.error="Error: %s " %(e)
                        logger.error(self.reason)
                        logger.error(self.error)
                        self.result=False
                    days = (t1 - t2).days
                    if days >= 0 and  days <= 90:
                        self.reason="LC files are created by %i days ago from %s" %(days, startdate)
                        self.success="%s and %s  files can be used for sharpening" %(lfile,rfile)
                        logger.info(self.reason)
                        logger.info(self.success)
                        self.result=True
                    else:
                        self.reason="LC files are created by %i days ago from %s" %(days,startdate)
                        logger.warn(self.reason)
                        logger.warn("%s and %s are OLD and Not using for sharpening" %(lfile,rfile))
                        self.result=False
                else:
                    self.reason="Invalid startdate(in Isthis_LCuseful) %s"%(startdate,)
                    logger.error(self.reason)
                    self.result=False  
            else:
                self.reason="Datestamps in %s and %s do not match" %(lfile,rfile)
                logger.error(self.reason)
                self.result=False
        else:
            logger.error("%s %s files do not exist" %(lfile,rfile))
            self.result=False

def get_endline(file):
    """!Read the last line of the given file and return it.
    @param file the file to read"""
    with open(file) as fid:
        data = fid.read() 
    return data.split()[len(data.split())-1]

##@var LCdir
# Loop current directory for running the test program

##@var lfile 
# rmy5 loop current file to test

##@var rfile 
# wc_ring_rmy5 loop current file to test

##@var startdate
# Cycle to check
