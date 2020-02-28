#!/usr/bin/env python

##@namespace pom.util
#Utility scripts for the pom package.
#
#Many of the functions here are for just testing the python based
#MPIPOM scripts during its development, can be replaced with much
#sophisticated functions in the produtil modules.
#
#@note Please report bugs/questions/comments to bijuthomas(at)mail(dot)uri(dot)edu.
#@author Biju Thomas, GSO, University of Rhode Island.
#@date June 8, 2014

##@var __all__
# List of symbols to provide by "from pom.util import *"
__all__ = 'ysplitter', 'trailzero',

import os
import re
import sys
import shutil
import os.path
import datetime
import subprocess

def ysplitter(ymdh):

    """!Splits a string with a ten digit YYYYMMDDHH into components

    @param ymdh A string "YYYYMMDDHH" containing a date and hour.
    @returns a tuple (y4,mm,mm1str,dd,hh) where y4 is the year, mm is
    the month, dd is the day of the month and hh is the hour.  The mmn1 is
    the nearest month start/end boundary considering the day of month"""
    y4=ymdh[0:4]
    mm=ymdh[4:6]
    dd=ymdh[6:8]
    hh=ymdh[8:10]
    if mm == "12":
        if int(dd) <= 15:
            mmn1 = int(mm) - 1
        else:
            mmn1 = 1
    elif mm == "01":
        if int(dd) <= 15:
            mmn1 = 12
        else:
            mmn1 = int(mm) + 1
    else:
        if int(dd) <= 15:
            mmn1 = int(mm) - 1
        else:
            mmn1 = int(mm) + 1
    if mmn1 <= 9:
        mm1str="0"+str(mmn1)
    else:
        mm1str=str(mmn1)
    return (y4,mm, mm1str,dd,hh)
def trailzero(astr, n):
    """!Returns a string of length n or greater, that consists of
    n-len(astr) zeros followed by astr
    @param n,astr the strings to consider"""
    ( '0'*max(0,int(n)-len(astr)) ) + astr
def inpfile(filename,lst,logger=None):
    """!Creates an input file from a list of strings
    @param filename the input file to create
    @param lst the list of strings
    @param logger a logging.Logger for log messages"""
    if logger is not None:
        logger.info('%s: creating input file'%(filename,))
    with open(filename,"w") as fid:
        for mem in lst:
            fid.write(mem)
            fid.write("\n")
def remove(file):
    """!Deletes the specified file.  Ignores errors.
    @param file The file to remove."""
    if arg is None or arg=='':
        return
    if os.path.exists(arg):
        try:
            os.unlink(arg)
        except EnvironmentError:
            pass
def rmall(*args):
    """!Deletes a list of files.
    @param args All positional arguments contain names of files to remove."""
    for arg in args:
        remove(arg)
def copy(src, dest, force=True):
    """!Copies the source to the destination without retaining permissions our group IDs.
    Errors are ignored.

    @param src,dest Source and destination filenames.
    @param force If True, the destination file is replaced if it already exists."""
    try:
        if os.path.exists(dest) and force:
            os.remove(dest)
    except os.error:
        pass
    try:
        if os.path.exists(dest) and os.stat(dest).st_size==0:
            os.remove(dest)
    except os.error:
        pass
    try:
        if not os.path.exists(dest):
            if os.path.exists(src):
                shutil.copy(src, dest)
            else:
                print("%s does NOT exists NOT copied" %(src))
        else:
            print("%s does exists NOT copied" %(dest))
    except os.error:
        pass

def move(src, dest, force=True):
    """!Renames the source file to the destination
    @param src,dest The source and destination filenames.
    @param force if True, the destination is replaced if it already exists."""
    try:
        if os.path.exists(dest) and force:
            os.remove(dest)
    except os.error:
        pass
    try:
        if os.path.exists(dest) and os.stat(dest).st_size==0:
            os.remove(dest)
    except os.error:
        pass
    try:
        if not os.path.exists(dest):
            if os.path.exists(src):
                shutil.move(src, dest)
            else:
                print("%s does NOT exists NOT moved" %(src))
        else:
            print("%s does exists NOT moved" %(dest))
    except os.error:
        pass
def link(src, dest):
    """!Links the source file to the destination.
    @param src,dest The source and destination filenames."""
    try:
        if os.path.exists(dest):
            os.remove(dest)
    except os.error:
        pass
    try:
        if os.path.exists(src):
            os.symlink(src, dest)
        else:
            print("%s does NOT exists NOT linked" %(src))
    except os.error:
        pass

def runexe(nproc, rundir, exefile, stdin, stdout):
    """!Runs an executable.  Will only run on the NOAA Jet cluster.

    @warning Do not use this function.  It only runs on NOAA Jet.  It
      lacks error checking other than returning the exit code and
      there is no logging.  Use the produtil.run module instead.
    @param nproc Number of processors to use
    @param rundir Directory in which to run.
    @param exefile Executable to run.
    @param stdin,stdout Input and output files.
    """
    if not os.path.exists(rundir):
        print("%s does NOT exists " %(rundir))
        return -999
    os.chdir(rundir)
    if not os.path.exists(exefile):
        print("%s does NOT exists " %(exefile))
        return -999
    if  nproc <= 1:
        if os.path.exists(stdin):
            fin = open(stdin, 'r')
        else:
            fin = None
        with open(rundir+"/std.out", 'w') as fout:
            try:
                retcode=subprocess.call(exefile, stdin=fin,stdout=fout)
            except OSError:
                print("Failed: (%s)" % exefile )
                pass
        return retcode
    else:
        with open(rundir+"/std.out", 'w') as fout:
            runstr="/apps/local/bin/mpiexec -np " + str(nproc) + "  " + exefile
            print(runstr)
            try:
                retcode=subprocess.call(runstr, stdout=fout, shell=True)
            except EnvironmentError as e:
                print("Failed: (%s): %s" %(exefile, str(e)))
                pass
        fout.close()
        return retcode
def read_input(filename):
    """!Reads the contents of the file for name,value pairs and returns a dict with the contents.

    Reads the file line-by-line.  Searches for pairs of strings
    "string1,string2" separated by spaces, colons, semicolons or
    commas.  Creates a dict mapping from the first string to the
    second.  Any strings past the second are ignored.

    @param filename the file to read."""
    try:
        input = {}
        with open(filename, 'rt') as f:
            for line in f:
                line=re.split(r'[;,:\s]\s*',line)
                input[line[1]]=line[0]
        return input
    except IOError:
        print("can NOT open (%s)" %filename)

def makenwrap(f):
    """!Wrapper for making the POM namelist.
    @param f the return value from pom.nml.make()"""
    def decoraten(self,DEST):
        filename = DEST+"/pom.nml"
        asgn = " = "
        with open(filename, "w+") as file:
            arg = "&pom_nml"
            file.write("%s\n" % (arg))
            for k in range(len(self.keys)):
                file.write("\t %s %s %s\n" % (self.keys[k], asgn, \
                    self.namelist[self.keys[k]]))
            file.write("%s\n" %('/'))
        return f
    return decoraten
def dateplushours(ymdh, hours):
    """!Adds a given number of hours to the date.
    @param ymdh A string "YYYYMMDDHH"
    @param hours number of hours to add"""
    if len(ymdh) == 10 and isinstance(ymdh, str)   \
                       and isinstance(hours, int):
           t=datetime.datetime(int(ymdh[0:4]),int(ymdh[4:6]),int(ymdh[6:8]),
                            int(ymdh[8:10]))
           t+=datetime.timedelta(hours=hours)
           return t.strftime('%Y%m%d%H')
    else:
        print("InputErr: ymdh hours in  dateplushours", ymdh, hours)

class counter:
    """!A simple integer counter class."""

    value = 0
    def set(self, x):
        """!Sets the counter value
        @param x the new counter value"""
        ##@var value
        # The current counter value.
        self.value = x
    def up(self):
        """!Increments the counter by 1."""
        self.value=self.value+1
    def down(self):
        """!Decrements the counter by 1."""
        self.value=self.value-1


def logi2int(l):
    """!Turns a bool to an int, returning 1 for True or 0 for False.
    @param l the bool value."""
    assert(type(l) == type(True))
    if l:
       return 1
    else:
       return 0

##@var jn2r
# A synonym for os.path.join
jn2r=os.path.join

def veto():
    """!Prints a message about not exiting, and returns False."""
    try:
        sys.exit(1)
    except SystemExit, value:
        print("caught exit(%s)" % value)
        return False
    try:
        os._exit(2)
    except SystemExit, value:
        print("caught exit(%s)" % value)
        return False
