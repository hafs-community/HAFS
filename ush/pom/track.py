#!/usr/bin/env python

##@namespace pom.track
#This module has  a simple "get_vitals" function to extract vitals
#from NHC vital file (syndat_tcvitals.year).
#
#@note Please report bugs/questions/comments to bijuthomas(at)mail(dot)uri(dot)edu.
#@author Biju Thomas, GSO, University of Rhode Island.
#@date June 11, 2014

##@var __all__
# List of symbols to export by "from pom.track import *"
__all__ = [ 'get_vitals' ]

import os.path
import re
from util import counter
from produtil.fileop import isnonempty
import logging

def get_vitals(vitalfile, centerid, stormid, yyyy, trackfile):
    """!Reads tcvitals
    @param vitalfile File to parse
    @param centerid Forecast center: NHC or JTWC
    @param stormid three character storm ID (ie.: 12L, 19W)
    @param yyyy Year.
    @param trackfile Output file."""
    try:
        vitals = []
        dates = []
        with open(vitalfile, "rt") as f: 
            for line in f:
                fields = re.split('[\s]\s*', line)
                if fields[0] == centerid.upper() and  \
                   fields[1] == stormid.upper():
                    if len(dates) == 0: 
                        dates.append(int(''.join([fields[3],fields[4]])))
                        vitals.append(line)
                    else:
                        if int(''.join([fields[3],fields[4]])) in dates:
                            n=dates.index(int( ''.join([fields[3],fields[4]])))
                            if len(line) >= len(vitals[n]):
                               dates[n] = int(''.join([fields[3],fields[4]]))
                               vitals[n] = line 
                        else:
                            dates.append(int(''.join([fields[3],fields[4]])))
                            vitals.append(line)     
            vitals = [y for (x,y) in sorted(zip(dates,vitals))]           
            #vitals.sort(key=dict(zip(vitals,dates)).get)
        c = counter()
        with open(trackfile, "wt") as f:
            for line in vitals:
                c.up()
                r1s = qck(line[69:73], line[73:78])
                r2s = qck(line[69:73], line[78:83])
                r3s = qck(line[69:73], line[83:88])
                r4s = qck(line[69:73], line[88:93])
                if len(line) <= 96:
                    f.write("%s%s%s%s%s%s%s%s \n"%(line[:19],line[21:36],line[37:42], \
                                                  line[43:73],r1s,r2s,r3s,r4s))
                else:
                    r5s = qck(line[69:73], line[95:100])
                    r6s = qck(line[69:73], line[100:105])
                    r7s = qck(line[69:73], line[105:110])
                    r8s = qck(line[69:73], line[110:115])
                    f.write("%s%s%s%s%s%s%s%s%s%s%s%s \n"%(line[:19],line[21:36],line[37:42], \
                                                  line[43:73],r1s,r2s,r3s,r4s,r5s,r6s,r7s,r8s))
            f.write("%s\n"%("000  000 00000     000000 0000 000 0000 000 000 0000 0000"))
        return c.value
    except IOError as err:
        print("%s" %err)
def qck(astr,bstr):
    """!Quality check function: if a>b or b is 999, returns -999, otherwise returns b
    @param astr,bstr string values containing integers a and b"""
    if int(astr) > int(bstr) or int(bstr) == 999:
        nstr =  " -999"
    else:
        nstr =  bstr
    return nstr

def track_shorten(fin, fout, ymdh, logger=None):
    """!Removes unwanted parts of the track file from get_vitals()
    @param fin,fout input and output files
    @param ymdh date and hour of interest as a ten digit number in a string
    @param logger a logging.Logger for log messages
    @returns 0 on error, or the number of lines kept in fin otherwise"""
    if logger is None: logger=logging.getLogger('pom')
    if not os.path.exists(fin) or not isnonempty(fin):
        logger.error("%s does not exists " %(fin))
        print("%s does not exists " %(fin))
        return 0
    with open(fin) as fid:
        with open(fout,'wt') as fo:
            c = counter()
            warn = True
            for l in fid:
                fo.write(l)
                c.up()
                if l[19:25] == ymdh[2:8] and l[26:28] == ymdh[8:10]:
                    warn = False
                    break
            fo.write("%s\n"%("000  000 00000     000000 0000 000 0000 000 000 0000 0000"))
    if (warn):
        logger.error("%s does not Found in %s"%(ymdh,fin))
        print("%s does not Found in %s"%(ymdh,fin))
        return 0
    else:
        return c.value

