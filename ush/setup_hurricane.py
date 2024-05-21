#! /usr/bin/env python
################################################################################
# Script Name: setup_hurricane.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script generates storm messages needed to run HAFS.
# History:
#   04/03/2023: Adapted from HWRF and improved for HAFS.
################################################################################
##@namespace ush.setup_hurricane
# @brief This script is run by the NOAA Senior Duty Meteorologist four times
# a day to generate the list of storms for the HFSA and HFSB hurricane
# models to run.
#
# This is an interactive program that uses the curses library to make
# a text-base, mouse-capable interface for deciding whether to run the
# HFSA and HFSB models for each storm.  The list of possible storms
# are sent by the National Hurricane Center (NHC) and Joint Typhoon
# Warning Center.  The script is able to use the data those centers
# send in real-time, and also the archived TCVitals database, which
# contains years of message files.
#
# The setup_hurricane script is configured using the following
# UNIX conf file:
# @code{.conf}
# [setup_hurricane]
# deliver=no ; should the final messages be delivered (yes) or just printed (no)
# envir={ENV[envir|-test]} ; run environment: prod, para, test
# hfsb_output={ENV[COMROOT]|-/com}/hur/{envir}/inphfsb ; where to place HFSB messages
# hfsa_output={ENV[COMROOT]|-/com}/hur/{envir}/inphfsa ; where to place HFSA messages
# maxhfsb=5 ; maximum number of HFSB storms
# maxhfsa=7 ; maximum number of HFSA storms
# nhc_max_storms=8 ; maximum number of NHC storms
# jtwc_max_storms=9 ; maximum number of JTWC storms
# nhc_input={ENV[nhcdir|-/nhc/save/guidance/storm-data/ncep]}/storm{istorm} ; nhc storm file name and path
# jtwc_input={ENV[DCOMROOT|-$DCOMROOT/prod]}/{YMD}/wtxtbul/storm_data/storm{istorm} ; jtwc storm file path
# tcvitals={ENV[COMINARCH|-$COMROOTp3/gfs/prod/syndat]}/syndat_tcvitals.{year} ; tcvitals location
# @endcode
#
# It finds the file from one of several locations, checked in this order:
# * $SETUP_HURRICANE_CONF environment variable
# * parm/setup_hurrucane_$USER.conf
# * parm/setup_hurricane.conf

import logging, sys, os, curses, collections, time, datetime, re, io
import produtil.setup, produtil.log
import tcutil.storminfo, tcutil.revital, tcutil.numerics, hafs.config
from tcutil.storminfo import InvalidVitals

def cmp(a, b):
    """ Python3 does not have cmp funtion """
    return (a > b) - (a < b)

def basin_center_checker(vl):
    """!Given a list of StormInfo objects, iterates over those that
    have the right basins for the right centers.  Rejects JTWC "L"
    basin storms, and rejects basins other than: ABWSPECQXL.  The "X"
    basin is a fake basin used to report parser and I/O errors in
    message files.  Rejects all forecast centers except NHC and JTWC
    @param vl a list of tcutil.storminfo.StormInfo objects"""
    for vital in vl:
        center=vital.center
        if center not in ('JTWC',"NHC"): continue
        if vital.basin1 not in 'ABWSPECQXL': continue
        yield vital

def name_number_checker(vl):
    """!Given an array of StormInfo objects, iterate over those that
    have valid names and numbers.  Discards "UNKNOWN" named storms,
    and numbers 50-79.  Discards "TEST" named storms if the number is
    <50 or >=90.
    @param vl a list of tcutil.storminfo.StormInfo objects"""
    for vital in vl:
        if vital.stormname=='UNKNOWN' or \
                (vital.stnum>50 and vital.stnum<90):
            continue
        if vital.stormname=='TEST' and (vital.stnum<50 or vital.stnum>90):
            continue
        yield vital

def sort_vitals(a,b):
    """!Comparison function for sorting storms by center, then
    priority, then by wind.  Lower priority numbers indicate more
    important storms, and hence are listed first.
    @param a,b tcutil.storminfo.StormInfo objects to order"""
    if a.center=='NHC' and b.center=='JTWC':
        # NHC goes before JTWC:
        return -1
    elif a.center=='JTWC' and b.center=='NHC':
        # JTWC goes after NHC:
        return 1
    elif hasattr(a,'priority') and hasattr(b,'priority') and \
            a.priority!=b.priority:
        # Lower priorities go after higher priorities:
        return cmp(a.priority,b.priority)
    if hasattr(a,'source') and hasattr(b,'source') and \
            a.source!=b.source:
        return -cmp(a.source,b.source)
    return -cmp(a.wmax,b.wmax) or cmp(a.stnum,b.stnum)


def sort_by_prio(a,b):
    """!Comparison function for sorting storms by priority.  NHC goes
    before JTWC.  Otherwise, sorts by RSMC's priority number.

    @param a,b tcutil.storminfo.StormInfo objects to order"""
    ap=a.priority
    bp=b.priority
    if a.center!='NHC': ap+=20   # big number = lower priority
    if b.center!='NHC': bp+=20
    return cmp(ap,bp)

def fake_prio(rv,firstprio=None):
    """!This function generates fake NHC and JTWC storm priority
    information for vitals that have none, simply based on the order
    the storm showes up in the rv.vitals list.  The optional second
    argument is the first number to use as the storm priority.  Any
    vitals that have a priority will not be modified.
    @param rv an iterable of tcutil.storminfo.StormInfo objects
    @param firstprio lowest priority number to assign"""
    if firstprio is None:
        firstprio=1
    firstprio=int(firstprio)-1
    d=collections.defaultdict(lambda: firstprio)
    for v in rv:
        if not hasattr(v,'priority'):
            prio=d[v.center]+1
            setattr(v,'priority',prio)
            d[v.center]=prio

class StormCurses(object):
    """!This class implements a user interface for selecting which
    storms HFSB and HFSA should run."""
    def __init__(self,vitals,YMDH,logger=None,maxhfsa=7,maxhfsb=5,
                 fake_sources=False):
        """!Creates a StormCurses object that will assist the SDM in
        choosing between the storms in the listed vitals.
        @param vitals the list of tcutil.storminfo.StormInfo objects to select from
        @param YMDH the cycle of interest
        @param logger a logging.Logger for log messages
        @param maxhfsa maximum number of HFSA storms
        @param maxhfsb maximum number of HFSB storms
        @param fake_sources if True, we're using TCVitals for a test run;
            if False, we're using the NHC and JTWC storm files"""
        when=tcutil.numerics.to_datetime(YMDH)
        self.YMDH=when.strftime('%Y%m%d%H')
        self.YMDHm6=tcutil.numerics.to_datetime_rel(-6*3600,when).\
            strftime('%Y%m%d%H')
        self.messagequeue=list()
        self.vitals=[x for x in vitals]
        self.stdscr=None
        self.logger=logger
        self.maxhfsa=maxhfsa
        self.maxhfsb=maxhfsb
        self.fake_sources=fake_sources
        self.resort()
        self.C_NORMAL=None
        self.C_SELECT=None
        self.C_WARN=None
        self.C_WARN_SELECT=None
        self.C_OCEAN=None
        self.C_LAND=None
        self.warnings=[ collections.defaultdict(list) for x in self.vitals ]
        # The warnings data structure provides a list of warnings
        # about each field in each vital line.  This is a
        # two-dimensional array of lists.  It is accessed as:
        #    self.warnings[i][field] = [ (reason1,details1),
        #                                (reason2,details2), ... ]
        # where i is the index in self.vitals and "field" is the name
        # of the vitals field (getattr(self.vitals[i],field)) The
        # (reason1,details1) tuple contains a short 1-2 word
        # explanation of the problem (reason1) and a detailed
        # explanation (details1)

        # Flags for "model X cannot run this storm:"
        self.hfsbcannot=[False]*len(self.vitals)
        self.hfsacannot=[False]*len(self.vitals)

        # Flags for "model X WILL run this storm:"
        self.hfsbwill=[False]*len(self.vitals)
        self.hfsawill=[False]*len(self.vitals)

        # Set the default values for the cannot and will flags, and
        # fill the self.warnings data structure:
        self.init_hfsa_hfsb()

    ##@var YMDH
    # the cycle of interst

    ##@var YMDHm6
    # the cycle before the cycle of interest

    ##@var messagequeue
    # a list of messages to display

    ##@var vitals
    # a list of tcutil.storminfo.StormInfo to select from

    ##@var stdscr
    # the curses screen used for display of text

    ##@var logger
    # a logging.Logger for log messages

    ##@var maxhfsa
    # Maximum number of HFSA storms allowed.

    ##@var maxhfsb
    # Maximum number of HFSB storms allowed.

    ##@var fake_sources
    # True=tcvitals in use, False=storm files

    ##@var C_NORMAL
    # Normal font

    ##@var C_SELECT
    # Font for selected text

    ##@var C_WARN
    # Font for text of storms that have warning messages

    ##@var C_WARN_SELECT
    # Font for text of storms that are selected AND have warning messages

    ##@var C_OCEAN
    # Unused: font for ocean locations on the map

    ##@var C_LAND
    # Unused: font for land locations on the map

    ##@var warnings
    # A mapping from storm to list of warning messages for that storm

    ##@var hfsbcannot
    # Array of logical telling whether each storm cannot be run by HFSB

    ##@var hfsacannot
    # Array of logical telling whether each storm cannot be run by HFSA

    ##@var hfsbwill
    # Array of logical telling whether HFSB will be run by each storm

    ##@var hfsawill
    # Array of logical telling whether HFSA will be run by each storm

    def __enter__(self):
        """!Sets up the curses library.  Use in a Python "with" block."""
        self.stdscr=curses.initscr()
        curses.start_color()
        curses.use_default_colors()
        curses.noecho()
        curses.cbreak()
        curses.mousemask(1)
        self.stdscr.keypad(1)

        curses.init_pair(1,-1,-1)
        self.C_NORMAL=curses.color_pair(1)

        curses.init_pair(2,curses.COLOR_RED,-1)
        self.C_WARN=curses.color_pair(2)

        curses.init_pair(3,curses.COLOR_WHITE,curses.COLOR_RED)
        self.C_WARN_SELECT=curses.color_pair(3)|curses.A_BOLD

        curses.init_pair(4,curses.COLOR_WHITE,curses.COLOR_BLUE)
        self.C_OCEAN=curses.color_pair(4)|curses.A_BOLD

        curses.init_pair(5,curses.COLOR_WHITE,curses.COLOR_GREEN)
        self.C_LAND=curses.color_pair(5)|curses.A_BOLD

        self.stdscr.attrset(self.C_NORMAL)

    def __exit__(self,type,value,tb):
        """!Ends the curses library and restores standard terminal
        functions.  Use in a Python "with" block.
        @param type,value,tb exception information"""
        self.stdscr.keypad(0)
        curses.nocbreak()
        curses.echo()
        curses.endwin()
        self.stdscr=None

    def test_screen(self):
        """!This routine is for testing only.  It displays text with
        all color combinations used by this class."""
        self.stdscr.clear()
        self.addstr(3,2,'TEST NORMAL',self.C_NORMAL)
        self.addstr(5,2,'TEST WARN',self.C_WARN)
        self.addstr(7,2,'TEST HIGHLIGHTED',
                           self.C_NORMAL|curses.A_STANDOUT)
        self.addstr(9,2,'TEST HIGHLIGHTED WARN',
                           curses.A_STANDOUT|self.C_WARN)
        self.addstr(11,2,'TEST OCEAN',self.C_OCEAN)
        self.addstr(13,2,'TEST LAND',self.C_LAND)
        self.stdscr.refresh()

    def make_storm_indices(self):
        """!Sets the "hfsamessage" and "hfsbmessage" attributes in all
        of self.vitals[*] to "messageN" (for an integer N), "-CANNOT-"
        or "---NO---" using setattr.  Uses self.hfsawill,
        self.hfsbwill, self.hfsacannot and self.hfsbcannot to make
        these judgements."""
        ihfsb=0
        ihfsa=0
        for i in range(len(self.vitals)):
            if self.hfsawill[i]:
                ihfsa+=1
                setattr(self.vitals[i],'hfsamessage','message%d'%ihfsa)
            elif self.hfsacannot[i]:
                setattr(self.vitals[i],'hfsamessage','-CANNOT-')
            else:
                setattr(self.vitals[i],'hfsamessage','---NO---')
            if self.hfsbwill[i]:
                ihfsb+=1
                setattr(self.vitals[i],'hfsbmessage','message%d'%ihfsb)
            elif self.hfsbcannot[i]:
                setattr(self.vitals[i],'hfsbmessage','-CANNOT-')
            else:
                setattr(self.vitals[i],'hfsbmessage','---NO---')

    def init_hfsa_hfsb(self):
        """!Decides if HFSA and HFSB can or should run each storm
        listed in the vitals.  Sets any warning or error flags for
        various fields."""
        i=-1
        hfsbcount=0
        hfsacount=0
        hfsadisable=[False] * len(self.hfsawill)
        for v in self.vitals:
            i+=1
            if v.YMDH!=self.YMDH:
                self.adderr(i,'source','wrong cycle',
                            'This data is for the wrong cycle: %s.'%(v.YMDH,))
            if v.center!='NHC':
                self.hfsbcannot[i]="Not NHC."
            if v.basin1 not in 'LEC':
                self.hfsbcannot[i]="Wrong basin."
            if getattr(v,'invalid',False) is True:
                self.hfsacannot[i]="Invalid vitals."
                self.hfsbcannot[i]="Invalid vitals."
                self.adderr(i,'stormname',v.stormname,
                              getattr(v,'explanation','Invalid vitals.'))
            source=getattr(v,'source','unknown')
            if source=='extrapolated tcvitals':
                self.addwarn(i,'source','extrapolated',
                            'Extrapolated from the previous cycle\'s tcvitals.')
                if v.wmax>=30:
                    self.addwarn(i,'wmax','missing',
                                 'Strong storm was not requested (missing bulletin?)')
                    self.addwarn(i,'center','??',
                                 'PROBABLE COMMUNICATION ERROR BETWEEN JTWC AND NCEP.')
                    self.addwarn(i,'center','??',
                                 'SUGGEST CALLING JTWC, RUN THIS STORM IF IT IS REAL.')
                else:
                    # Disable HFSA by default for weak extrapolated storms:
                    hfsadisable[i]=True
                # Always disable HFSB for extrapolated storms.  This
                # is redundant because these are JTWC, which HFSB does
                # not run:
                self.hfsbcannot[i]='this is extrapolated tcvitals data.'
                self.hfsbwill[i]=False
            if source=='tcvitals':
                self.addwarn(i,'center','??',
                             'PROBABLE COMMUNICATION ERROR BETWEEN JTWC AND NCEP.')
                self.addwarn(i,'center','??',
                             'SUGGEST CALLING JTWC, RUN THIS STORM IF IT IS REAL.')
                self.addwarn(i,'source','TCVitals',
                            'These vitals are from the tcvitals, not storm files.')
            if v.basin1 not in 'LECWPQSAB':
                self.adderr(i,'stormid3','unknown basin'
                            'The only supported basins are: '
                            'L E C W P Q S A B')
            if v.wmax<10:
                self.addwarn(i,'wmax','Vmax<10m/s',
                             'Wind is very weak.')
            if v.wmax>80:
                self.addwarn(i,'wmax','Vmax>80m/s',
                             'Wind is very strong.')
            if v.pmin<890:
                self.addwarn(i,'pmin','Pmin<890',
                             'Extremely low pressure (<890 mbar)')
            if v.pmin>1012:
                self.addwarn(i,'pmin','Pmin>1012',
                             'Extremely high pressure (>1012 mbar)')
            if v.pmin>v.poci:
                self.addwarn(i,'pmin','Pmin>Penvir',
                             'ERROR! Central pressure is higher than '
                             'outermost closed isobar! HFSA will fail.')
                self.addwarn(i,'poci','Pmin>Penvir',
                             'ERROR! Central pressure is higher than '
                             'outermost closed isobar! HFSA will fail.')
            if v.basin1 in 'LECWAB' and v.lat<=0:
                self.addwarn(i,'lat','south',
                             'latitude should be >0 for LECWAB basins')
                self.addwarn(i,'stormid3','south',
                             'latitude should be >0 for LECWAB basins')
            if v.basin1 in 'PQS' and v.lat>=0:
                self.addwarn(i,'lat','north',
                             'latitude should be <0 for PQS basins')
                self.addwarn(i,'stormid3','north',
                             'latitude should be <0 for PQS basins')
            if v.lat>60 or v.lat<-60:
                self.addwarn(i,'lat','subarctic',
                             'latitude is far away from the tropics')
            if v.lat<5 and v.lat>-5:
                self.addwarn(i,'lat','equatorial',
                             'latitude is very close to the equator')
            if hfsbcount<self.maxhfsb:
                self.hfsbwill[i] = not self.hfsbcannot[i]
            if self.hfsbwill[i]: hfsbcount+=1
            if hfsacount<self.maxhfsa:
                self.hfsawill[i] = not hfsadisable[i] and not self.hfsacannot[i]
            if self.hfsawill[i]: hfsacount+=1
        self.make_storm_indices()

    def toggle_run(self,istorm,hfsa=True,hfsb=True):
        """!Turns on or off the HFSB and/or HFSA model for the storm at
        index istorm of self.vitals.
        @param istorm index of the storm in self.vitals
        @param hfsa if True, toggle HFSA
        @param hfsb if True, toggle HFSB"""
        if istorm>=len(self.vitals): return
        nhfsa=0
        nhfsb=0
        for i in range(len(self.vitals)):
            if self.hfsawill[i]: nhfsa+=1
            if self.hfsbwill[i]: nhfsb+=1
        i=istorm
        if hfsa:
            if self.hfsacannot[i]:
                self.messagequeue.append('%s: HFSA cannot run: %s'%(self.vitals[i].stormid3,self.hfsacannot[i]))
            else:
                if self.hfsawill[i]:
                    self.hfsawill[i]=False
                elif nhfsa>=self.maxhfsa:
                    self.messagequeue.append('Too many HFSA storms.')
                else:
                    self.hfsawill[i]=True
        if hfsb:
            if self.hfsbcannot[i]:
                self.messagequeue.append('%s: HFSB cannot run: %s'%(self.vitals[i].stormid3,self.hfsbcannot[i]))
            else:
                if self.hfsbwill[i]:
                    self.hfsbwill[i]=False
                elif nhfsb>=self.maxhfsb:
                    self.messagequeue.append('Too many HFSB storms.')
                else:
                    self.hfsbwill[i]=True
        self.make_storm_indices()

    def hasmessage(self,i,field):
        """!Returns True if there are warnings or errors for storm i,
        and False otherwise.
        @param i index of the storm in self.vitals
        @param field the field of interest"""
        if field not in self.warnings[i]:
            return False
        warnings=self.warnings[i][field]
        return len(warnings)>0

    def adderr(self,i,field,reason,details,hfsb=True,hfsa=True):
        """!Records that vitals at index i cannot be used by either
        model due to an error in the specified field.  The "reason" is
        a short string explaining why and the "details" is a longer
        string with a full explanation.
        @param i index of the storm in self.vitals
        @param field the field that is having trouble
        @param reason why the storm cannot run
        @param details detailed reason why the storm cannot be run
        @param hfsb,hfsa if True, that model cannot run"""
        self.addwarn(i,field,reason,details)
        if hfsa: self.hfsacannot[i]=reason
        if hfsb: self.hfsbcannot[i]=reason

    def addwarn(self,i,field,reason,details):
        """Records that there is a problem with the specified field in
        the vitals entry at index i.  The "reason" variable gives a
        short 1-2 word reason, while the "details" variable gives a
        potentially long explanation of what is wrong.
        @param i index of the storm in self.vitals
        @param field the field with problems
        @param reason,details short and long explanation of the problem"""
        self.warnings[i][field].append([reason,details])

    def quit_confirmation(self):
        """!Clears the screen and informs the user that they asked to
        quit.  Waits for a keypress and then returns True."""
        self.stdscr.clear()
        self.addstr(0,0,
           'You have asked to quit without setting up the models.')
        self.addstr(1,0,'Press any key to quit...')
        self.stdscr.refresh()
        self.stdscr.getch()
        return True

    def setup_confirmation(self):
        """!Clears the screen and shows a setup confirmation screen,
        displaying what models will run what storms.  Asks the user if
        they're sure.  Returns True if the user is sure, or False
        otherwise."""
        self.stdscr.clear()
        self.addstr(0,0,
            'You have asked to setup the following simulations:')

        self.addstr(1,0,'HFSA:')
        nhfsa=0
        i=2
        for istorm in range(len(self.vitals)):
            if not self.hfsawill[istorm]: continue
            more=''
            v=self.vitals[istorm]
            if hasattr(v,'source') and not v.source.startswith('storm'):
                more='(source: %s)'%(v.source)
            line='  {v.hfsamessage:6s} = {v.center:4s} #{v.priority:1d} '\
                '{v.stormid3:3s} {v.stormname:10s} {more:s}'.format(
                v=v,more=more)
            self.addstr(i,0,line)
            i+=1
            nhfsa+=1
        if nhfsa==0:
            self.addstr(1,6,'NO STORMS!')
        else:
            self.addstr(1,6,'%d storms:'%nhfsa)

        i0=i
        self.addstr(i,0,'HFSB:')
        i+=1
        nhfsb=0
        for istorm in range(len(self.vitals)):
            if not self.hfsbwill[istorm]: continue
            line='  {v.hfsbmessage:6s} = {v.center:4s} #{v.priority:1d} '\
                '{v.stormid3:3s} {v.stormname:10s}'.format(
                v=self.vitals[istorm])
            self.addstr(i,0,line)
            i+=1
            nhfsb+=1
        if nhfsb==0:
            self.addstr(i0,6,'NO STORMS!')
        else:
            self.addstr(i0,6,'%d storms:'%nhfsb)

        self.addstr(i,0,'Type YES to confirm, or press any key to go back...')
        i+=1
        self.addstr(i,0,'     Y    E    S    ')
        #                01234567890123456789
        self.stdscr.refresh()
        ikey=0
        while True:
            k=self.stdscr.getch()
            if ikey==0 and k in (ord('y'),ord('Y')):
                ikey+=1
                self.addstr(i,3,'[-Y-]')
                self.stdscr.refresh()
            elif ikey==1 and k in (ord('e'),ord('E')):
                ikey+=1
                self.addstr(i,8,'[-E-]')
                self.stdscr.refresh()
            elif ikey==2 and k in (ord('s'),ord('S')):
                ikey+=2
                self.addstr(i,13,'[-S-]')
                for x in range(3):
                    self.stdscr.refresh()
                    time.sleep(0.5)
                    self.addstr(i,0,'--- CONFIRMED - SETUP - SEQUENCE ---')
                    self.stdscr.refresh()
                    time.sleep(0.5)
                    self.addstr(i,0,'--- CONFIRMED - SETUP - SEQUENCE ---',
                         curses.A_STANDOUT)
                return True
            else:
                self.addstr(i,0,'    CANCEL - GOING BACK                ')
                self.stdscr.refresh()
                time.sleep(0.5)
                return False

    def show_storm_screen(self,ihighlight=None):
        """!Prints the storm selection screen starting at line 0, and
        returns the number of lines printed.
        @param ihighlight index of the highlighted storm in self.vitals.
           To highlight nothing, provide None or an invalid index."""
        i=0
        self.stdscr.clear()
        i=self.show_storm_table(i,ihighlight)
        self.addstr(i,0,' ')
        i+=1
        self.addstr(i,0,'Controls: [N]ext [P]rev, toggle HFS[A] HFS[B] [H]all')
        i+=1
        self.addstr(i,0,'When done: [S]etup models or [Q]uit without doing anything')
        i+=1
        self.addstr(i,0,' ')
        i+=1
        if ihighlight is not None and len(self.vitals)>=ihighlight:
            i=self.show_storm_details(i,ihighlight)
        self.stdscr.refresh()
        return i

    def show_storm_heading(self,iline):
        """!Prints the storm selection table header starting at the
        specified line, and returns iline+2.
        @param iline the starting line"""
        title ='RSMC Source SID Storm-Name -Lat- -Lon--  Vmax Pmin Penv --HFSB-- --HFSA--'
        bar   ='-------------------------------------------------------------------------'
        iline=int(iline)
        assert(iline>=0)
        self.addstr(iline,0,title) ; iline+=1
        self.addstr(iline,0,bar) ; iline+=1
        return iline

    def show_storm_table_line(self,iline,istorm,highlight=False):
        """!Prints one line of the storm list table, for storm
        self.vitals[istorm], at line iline on the console.  If
        highlight=True, then the line is highlighted. Returns
        iline+1.
        @param iline the starting line
        @param istorm index of the storm in self.vitals
        @param highlight if True, highlight that line"""
        formats=[ ('center','4s'), ('source','6s'),
                  ('stormid3','3s'), ('stormname','10s'),
                  ('lat','4.1f'),('lon','5.1f'),
                  ('wmax','5.1f'), ('pmin','4.0f'),('poci','4.0f') ,
                  ('hfsbmessage','6s'), ('hfsamessage','6s'),]
        i=int(istorm)
        v=self.vitals[i]

        # Loop over format keys k, and format values f, constructing
        # two variables for each: d=the data to print, and m=the
        # display attributes.
        y=int(iline)
        x=0
        first=True
        for (k,f) in formats:
            fmt='{0:'+f+'}'
            if highlight:
                m=curses.A_REVERSE|self.C_NORMAL
            else:
                m=self.C_NORMAL

            # Handle keys that are not in self.vitals[i]:
            if k=='i':            d=fmt.format(i+1,)
            elif k=='source':
                if v.priority>9:
                    if v.source=='extrapolated tcvitals':
                        strprio='extrap'
                    elif v.source=='tcvitals':
                        strprio='syndat'
                    else:
                        strprio='??????'
                elif v.YMDH!=self.YMDH:
                    strprio='storm%d'%v.priority
                else:
                    strprio='storm%d'%v.priority
                d=fmt.format(strprio)
                if self.hasmessage(i,k):
                    if highlight:
                        m=self.C_WARN_SELECT
                    else:
                        m=self.C_WARN|curses.A_BOLD
            # Else, key is from self.vitals[i]:
            else:
                if self.hasmessage(i,k):
                    if highlight:
                        m=self.C_WARN_SELECT
                    else:
                        m=self.C_WARN|curses.A_BOLD

                val=getattr(v,k)
                if k=='lat':
                    d=fmt.format(abs(val),)+( 'N' if(val>=0) else 'S' )
                elif k=='lon':
                    d=fmt.format(abs(val),)+( 'E' if(val>=0) else 'W' )
                else:
                    d=fmt.format(val,)

            # Put a space before each column except the first:
            if not first:
                d=' '+d
            else:
                first=False

            # Put the colored text on the screen:
            try:
                self.addstr(y,x,d,m)
            except curses.error as e:
                pass
            x+=len(d)
        return x

    def show_storm_table(self,iline,ihighlight=None):
        """!Fills self.stdscr with a list of storms starting at line
        iline on the screen.  If ihighlight is not None, then it
        specifies the 1-based index of the storm that is presently
        highlighted.
        @param iline first line
        @param ihighlight index of the storm in self.vitals to highlight, or None to
            highlight no storms"""
        iline=self.show_storm_heading(iline)
        for i in range(len(self.vitals)):
            self.show_storm_table_line(iline,i,
                ihighlight is not None and i==ihighlight)
            iline+=1
        return iline

    def addstr(self,y,x,s,a=None):
        """!Puts a string s on the screen at the given y column and x
        row.  Optionally, sets the attributes a.  This is a simple
        wrapper around the curses addstr command.  It ignores any
        curses errors, which allows one to write a string that is not
        entirely on the terminal.
        @param y,x curses location
        @param s string to print
        @param a curses attributes
        @returns the number of characters printed (0 or len(s))"""
        try:
            if a is None:
                self.stdscr.addstr(y,x,s)
            else:
                self.stdscr.addstr(y,x,s,a)
        except curses.error as e:
            return 0
        return len(s)

    def show_storm_details(self,iline,istorm):
        """!Shows details about a storm self.vitals[istorm] starting on
        the given line number.
        @param iline the line number
        @param istorm the index of the storm in self.vitals"""
        if istorm>=len(self.vitals): return iline
#        istorm=int(istorm)  #UnboundLocalError: local variable 'int' referenced before assignment(??)
        v=self.vitals[istorm]

        # Storm name and forecast center info:
        priority=getattr(v,'priority',-999)
        if priority is None or priority<0:
            priority='none'
        else:
            priority='#%d'%(priority,)
        line='{v.center:s} {priority:s} = {v.YMDH:s} {v.stormid3:3s} {v.stormname:s}   (from {v.source:s})'\
            .format(i=istorm+1,v=v,priority=priority)
        self.addstr(iline,0,line)
        iline+=1

        # location and movement:
        latstr='%.1f%c'%(
            abs(v.lat),  'N' if(v.lat>=0) else 'S')
        lonstr='%.1f%c'%(
            abs(v.lon),  'E' if(v.lon>=0) else 'W')

        line='  at {latstr:s} {lonstr:s} moving {v.stormspeed:.1f}m/s '\
            'at {v.stormdir:.0f} degrees from north'.format(
            lonstr=lonstr,latstr=latstr,v=v)
        self.addstr(iline,0,line)
        iline+=1

        # Intensity and wind radii (two lines):
        line='  wind={v.wmax:.0f}m/s RMW={v.rmw:.0f}km R34: NE={v.NE34:.0f}, '\
            'SE={v.SE34:.0f}, SW={v.SW34:.0f}, NW={v.NW34:.0f} km'.format(v=v)
        self.addstr(iline,0,line)
        iline+=1
        line='  Pmin={v.pmin:.1f}mbar, outermost closed isobar '\
            'P={v.poci:.1f} at {v.roci:.1f}km radius'.format(v=v)
        self.addstr(iline,0,line)
        iline+=1

        # Warnings:
        have=False
        for (field,warnings) in self.warnings[istorm].items():
            iwarn=0
            for (short,int) in warnings:
                iwarn+=1
                have=True
                self.addstr(iline,0,'%s(%d) - %s: %s'%(
                        field,iwarn,short,int),
                        self.C_WARN|curses.A_BOLD)
                iline+=1
        if not have:
            self.addstr(iline,0,
                'I see no obvious errors in the vitals data for '
                '{v.stormname:s} {v.stormid3:3s}. '.format(v=v))
            iline+=1
        return iline

    def show_message_queue(self):
        """!If the message queue is not empty, clears the screen and
        shows the contents of the message queue.  Waits for any key
        press, then clears the message queue and returns."""
        if not self.messagequeue: return
        self.stdscr.clear()
        self.addstr(0,0,"ERROR:")
        for imessage in range(len(self.messagequeue)):
            self.addstr(imessage+1,3,self.messagequeue[imessage])
        self.addstr(imessage+2,5,"... PRESS ANY KEY ...")
        self.stdscr.refresh()
        try:
            self.stdscr.getch()
        except curses.error:
            pass
        self.messagequeue=list()

    def resort(self):
        """!Re-sorts the vitals into priority order."""
        YMDH=self.YMDH

        rv=tcutil.revital.Revital(self.logger)
        rv.extend(self.vitals)
        self.logger.info('Sorting vitals.')
        #self.logger.info('Cleaning up the vitals and removing duplicates.')
        #rv.discard_except(lambda v: v.YMDH==YMDH)
        rv.sort_by_function(sort_vitals)
        #rv.clean_up_vitals()
        #rv.sort_by_function(sort_vitals)
        #fake_prio(rv)
        #rv.sort_by_function(sort_by_prio)
        self.vitals=[x for x in rv.vitals]
        del rv
        if self.fake_sources:
            self.fill_source_and_priority()

    def fill_source_and_priority(self):
        """!Sets the "priority" attribute in all vitals to storm1...stormN"""
        ijtwc=0
        inhc=0
        for v in self.vitals:
            if v.center=='JTWC':
                ijtwc+=1
                v.source='storm%d'%ijtwc
                setattr(v,'priority',ijtwc)
            elif v.center=='NHC':
                inhc+=1
                v.source='storm%d'%inhc
                setattr(v,'priority',inhc)

    def get_curses_mouse(self):
        """!Gets a curses mouse event's details.

        @returns a tuple containing a string, and an index within
        self.vitals.  The string is "HFSA" if an hfsa message was
        clicked, "HFSB" for a HFSB message or "select" if the user
        clicked outside the message selection region.  If no vitals
        were clicked, this routine returns (None,None)."""
        (_,x,y,_,_) = curses.getmouse()
        #if not bstate&curses.BUTTON1_CLICKED:
        #    return (None,None) # don't care about this event
        ivital=y-2
        if ivital<0 or ivital>=len(self.vitals):
            return (None,None) # not in vital list
        if x>=56 and x<=63:
            return ('HFSB',ivital)
        elif x>=65 and x<=72:
            return ('HFSA',ivital)
        else:
            return ('select',ivital)

    def event_loop(self,iselect=0):
        """!The main event loop for the storm selection and setup
        confirmation screens.  Handles mouse and key events.

        @param iselect first storm selected (highlighted)
        @returns True if the user asked to setup the models, and
        confirmed the setup request.  Returns False or None otherwise."""
        iselect=int(iselect)
        istorms=len(self.vitals)
        self.show_storm_screen(iselect)
        while True:
            try:
                k=self.stdscr.getch()
            except curses.error:
                self.show_storm_screen(iselect)
                time.sleep(0.1)
            inew=iselect
            if k==curses.KEY_MOUSE:
                (action,ivital)=self.get_curses_mouse()
                if action=='select':
                    inew=ivital
                if action=='HFSB':
                    inew=ivital
                    iselect=inew
                    self.toggle_run(inew,hfsa=False,hfsb=True)
                    self.show_message_queue()
                    self.show_storm_screen(iselect)
                elif action=='HFSA':
                    inew=ivital
                    iselect=inew
                    self.toggle_run(inew,hfsa=True,hfsb=False)
                    self.show_message_queue()
                    self.show_storm_screen(iselect)
            elif k in ( ord('p'), ord('P'), ord('u'), ord('U'),
                        curses.KEY_UP, curses.KEY_LEFT ):
                if istorms>1:
                    inew=(iselect+istorms-1)%istorms
            elif k in ( ord('n'), ord('N'), ord('d'), ord('D'),
                        curses.KEY_DOWN, curses.KEY_RIGHT ):
                if istorms>1:
                    inew=(iselect+istorms+1)%istorms
            elif k in ( ord('a'), ord('A') ):
                self.toggle_run(iselect,hfsa=True,hfsb=False)
                self.show_message_queue()
                self.show_storm_screen(iselect)
            elif k in (ord('b'), ord('B'), ord('g'), ord('G')):
                self.toggle_run(iselect,hfsa=False,hfsb=True)
                self.show_message_queue()
                self.show_storm_screen(iselect)
            elif k in (ord('h'), ord('H')):
                self.toggle_run(iselect,hfsa=True,hfsb=True)
                self.show_message_queue()
                self.show_storm_screen(iselect)
            elif k in ( ord('q'), ord('Q') ):
                if self.quit_confirmation():
                    return False
                else:
                    self.show_storm_screen(iselect)
            elif k in ( ord('S'), ord('s') ):
                if self.setup_confirmation():
                    return True
                else:
                    # setup was canceled
                    self.show_storm_screen(iselect)
            if inew!=iselect:
                iselect=inew
                self.show_storm_screen(iselect)

    def setup(self,conf):
        """!Either setup the models, or print what would be done.

        @param conf Uses the specified configuration object (ideally,
        an HAFSConfig) to find output locations."""
        logger=self.logger
        self.make_storm_indices()
        sh='setup_hurricane'
        deliver=conf.getbool(sh,'deliver')
        if deliver:
            logger.warning('deliver=yes: will write message files')
        else:
            logger.warning('deliver=no: will NOT write message files')
        self._setup_one(conf,sh,'hfsb',deliver)
        self._setup_one(conf,sh,'hfsa',deliver)
        if not deliver:
            logger.warning("I DID NOT REALLY WRITE ANYTHING!!")
            logger.warning('You have deliver=no in your setup_hurricane '
                           'configuration file.')
            logger.warning('Change that to deliver=yes to enable delivery.')

    def _setup_one(self,conf,sh,gh,deliver):
        """!Internal function that sets up one storm.

        This is an internal implementation function.  Do not call it
        directly.  It sets up HFSA or HFSB or just prints what would
        be done (if deliver=False).
        @param deliver  False to just print what would be done, or
                        True to actually deliver
        @param gh   "hfsb" or "hfsa" (lower-case)
        @param sh   the name of the conf section to use ("setup_hurricane").
        @param conf an hafs.config.HAFSConfig for configuration info"""
        logger=self.logger
        outdir=conf.getstr(sh,gh+'_output')
        produtil.fileop.makedirs(outdir,logger=logger)
        maxstorm=conf.getstr(sh,'max'+gh)
        n=0
        nhc=list()
        jtwc=list()
        allstorms=list()
        if deliver:
            would=''
        else:
            would='would '
        for v in self.vitals:
            filename=getattr(v,gh+'message')
            if '-' in filename: continue
            n+=1
            history='history'+filename[7:]
            filename=os.path.join(outdir,filename)
            history=os.path.join(outdir,history)
            message=v.as_message()
            logger.info('%s: %swrite "%s"'%(filename,would,message))
            logger.info('%s: %sappend "%s"'%(history,would,message))
            if deliver:
                with open(filename,'wt') as f:
                    f.write(message+'\n')
                with open(history,'at') as f:
                    f.write(message+'\n')
            allstorms.append(message)
            if v.center=='NHC':
                nhc.append(message)
            else:
                jtwc.append(message)
        allfile=os.path.join(outdir,'storms.all')
        if os.path.exists(allfile):
            logger.info('%swrite prior cycle contents of storms.all '
                        'to storms.prev.'%(would,))
            rv=tcutil.revital.Revital()
            rv.readfiles([allfile],raise_all=False)
            outstring=''
            for v in rv:
                message=v.as_message()
                if v.YMDH==self.YMDHm6:
                    logger.info('%sinclude vit: %s'%(would,message))
                    outstring+=message+'\n'
                else:
                    logger.info('wrong cycle: %s'%(message,))
            if deliver:
                with open(os.path.join(outdir,'storms.prev'),'wt') as f:
                    f.write(outstring)
        else:
            logger.warning('%s: does not exist - cannot generate '
                           'storms.prev'%(allfile,))
        logger.info('%swrite %d lines to storms.nhc'%(would,len(nhc)))
        logger.info('%swrite %d lines to storms.jtwc'%(would,len(jtwc)))
        logger.info('%swrite %d lines to storms.all'%(would,len(allstorms)))
        if deliver:
            with open(os.path.join(outdir,'storms.nhc'),'wt') as f:
                f.write('\n'.join(nhc)+'\n')
            with open(os.path.join(outdir,'storms.jtwc'),'wt') as f:
                f.write('\n'.join(jtwc)+'\n')
            with open(os.path.join(outdir,'storms.all'),'wt') as f:
                f.write('\n'.join(allstorms)+'\n')
        nfilename=os.path.join(outdir,'nstorms')
        dfilename=os.path.join(outdir,'stormdate')
        logger.info('%s: %swrite "%d"'%(nfilename,would,n))
        logger.info('%s: %swrite "%s"'%(dfilename,would,self.YMDH[2:]))
        if deliver:
            with open(nfilename,'wt') as f:
                f.write('%d\n'%n)
            with open(dfilename,'wt') as f:
                f.write(self.YMDH[2:]+'\n')

########################################################################
def read_tcvitals(logger,files,cyc):
    """!Reads tcvitals.

    @param files If "files" is None or empty, will go to production
    locations to read vitals for the specified cycle.
    @param logger a logging.Logger for messages
    @param cyc the cycle to read"""
    rv=tcutil.revital.Revital(logger)
    YMDH=cyc.strftime('%Y%m%d%H')
    if files:
        for f in files:
            rv.readfiles(f,raise_all=False)
    elif cyc:
       #tcv=cyc.strftime(conf.getstr('setup_hurricane','tcvitals',morevars={'year':'%Y'}))
        tcvdir=os.environ.get('COMINarch','')
        tcv=cyc.strftime(os.path.join(tcvdir,'syndat_tcvitals.%Y'))
        logger.warning('Will read: %s'%(tcv,))
        rv.readfiles(tcv,raise_all=False)
    else:
        raise ValueError('In read_tcvitals, you must provide a cycle or files.')
    logger.info('Finished reading vitals.  Clean them up...')
    rv.clean_up_vitals()
    rv.discard_except(lambda v: v.YMDH==YMDH)
    return rv

def make_bad_message(center,priority,ymdh,stormname,explanation):
    """!Called when a storm's message cannot be

    read.  Creates a dummy StormInfo object with the correct time,
    priority and center, but invalid data.  The function expects a
    fake "stormname" that explains what went wrong concisely (eg.:
    "UNPARSABLE" or "MISSING").  The basin will be "E" (since both
    JTWC and NHC can send vitals of that basin) and the storm number
    will be the priority.  A full explanation of the problem should be
    in the "explanation" variable, which can be a multi-line string.
    @param center the RSMC: JTWC or NHC
    @param priority the storm priority
    @param ymdh the date
    @param stormname the storm name for the fake vitals
    @param explanation a full explanation of the problem"""

    format='{center:4s} {priority:02d}E {stormname:10s} {ymd:08d} {hh:02d}00 100N 0100W 010 010 1000 1010 0100 10 034 -999 -999 -999 -999 X'
    if len(center)>4: center=center[0:4]
    if len(stormname)>10: stormname=stormname[0:10]
    line=format.format(
        center=center,ymd=int(str(int(ymdh))[0:8],10),
        hh=int(str(int(ymdh))[8:10]),
        stormname=stormname,priority=int(priority))
    si=tcutil.storminfo.StormInfo('tcvitals',line)
    assert(isinstance(si,tcutil.storminfo.StormInfo))
    setattr(si,'priority',int(priority))
    setattr(si,'invalid',True)
    setattr(si,'explanation',str(explanation))
    return si

def read_message(logger,center,ymdh,filename,priority):
    """!Attempts to read a message from the specified file, logging any
    problems to the given logger.

    @param logger a logging.Logger for log messages
    @param center the RSMC: JTWC or NHC
    @param ymdh   the cycle
    @param filename  the file to read
    @param priority  the storm priority for the RSMC
    @returns None if the message file did not exist or was empty.
    Otherwise, a StormInfo is returned, but may contain invalid data
    from make_bad_message.  Invalid StormInfo objects can be detected
    by si.invalid==True."""
    if not os.path.exists(filename):
        logger.info('%s: does not exist'%(filename,))
        return None
    try:
        with open(filename,'rt') as f:
            line=f.readline()
        si=tcutil.storminfo.StormInfo('message',line)
        setattr(si,'invalid',False)
        try:
            logger.info('%s: read this: %s'%(filename,si.as_message()))
        except Exception as e:
            logger.info('%s: could not print contents: %s'
                        %(filename,str(e)),exc_info=True)
    except InvalidVitals as iv:
        logger.error('%s: skipping unparsable file: %s'
                     %(filename,str(iv)),exc_info=True)
        si=make_bad_message(center,priority,ymdh,'UNPARSABLE',str(iv))
    except (KeyError,ValueError,TypeError) as e:
        logger.error('%s: skipping: %s'%(filename,str(e)),exc_info=True)
        si=make_bad_message(center,priority,ymdh,'ERROR',str(e))
    setattr(si,'priority',priority)
    return si

def read_nstorms(logger,filename,rsmc,max_nstorms):
    """!Reads the number of storms file
    @param logger a logging.Logger for log messages
    @param filename the files path
    @param rsmc the RSMC: JTWC or NHC
    @param max_nstorms maximum allowed value for nstorms
    @returns an integer number of storms"""
    logger.info('%s: get %s nstorms'%(filename,rsmc))
    nstorms=int(max_nstorms)
    try:
        with open(filename,'rt') as f:
            line=f.readline().strip()
            iline=int(line,10)
            nstorms=max(0,min(max_nstorms,iline))
            logger.info('%s: %s nstorms = %d'%(filename,rsmc,nstorms))
    except (ValueError,TypeError,EnvironmentError) as e:
        logger.error("Trouble reading %s nstorms: %s"%(rsmc,str(e)))
    return nstorms

def read_nhc_and_jtwc_inputs(logger,conf):
    """!Reads NHC and JTWC storm files from locations specified in the
    given HAFSConfig object.
    @param logger a logging.Logger for messages
    @param conf an hafs.config.HAFSConfig with configuration info"""
    sh='setup_hurricane'
    cyc=conf.cycle
    YMDH=conf.getstr('config','YMDH')
    cycm6=tcutil.numerics.to_datetime_rel(-6*3600,cyc)
    YMDHm6=cycm6.strftime('%Y%m%d%H')
    maxjtwc=conf.getint(sh,'jtwc_max_storms')
    maxnhc=conf.getint(sh,'nhc_max_storms')
    threshold=conf.getint(sh,'renumber_threshold',14)
    assert(maxjtwc>=0)
    assert(maxnhc>=0)
    logger.info('Current cycle is %s and previous is %s'%(YMDH,YMDHm6))

    jtwc_nstorms=maxjtwc
    #jtwc_nstorms=read_nstorms(logger,conf.getstr(sh,'jtwc_nstorms'),
    #                          'jtwc',maxjtwc)
    nhc_nstorms=maxnhc
    #nhc_nstorms=read_nstorms(logger,conf.getstr(sh,'nhc_nstorms'),
    #                          'nhc',maxnhc)

    rv=tcutil.revital.Revital(logger=logger)
    for inhc in range(nhc_nstorms):
        filename=conf.strinterp(sh,'{nhc_input}',istorm=inhc+1)
        si=read_message(logger,'NHC',YMDH,filename,inhc+1)
        if si is None:
            logger.warning('%s: could not read message'%(filename,))
            continue
        if si.YMDH!=YMDH:
            logger.warning('Ignoring old storm: %s'%(si.as_message(),))
            continue
        setattr(si,'source','nhcstorm%d'%(1+inhc))
        setattr(si,'sourcefile',filename)
        rv.append(si)

    for ijtwc in range(jtwc_nstorms):
        filename=conf.strinterp(sh,'{jtwc_input}',istorm=ijtwc+1)
        si=read_message(logger,'JTWC',YMDH,filename,ijtwc+1)
        if si is not None:
            setattr(si,'source','jtwcstorm%d'%(1+ijtwc))
            setattr(si,'sourcefile',filename)
            rv.append(si)

    rv2=tcutil.revital.Revital(logger=logger)
    vitfile=conf.strinterp(sh,'{tcvitals}')
    rv2.readfiles([vitfile],raise_all=False)
    logger.info('Have %d tcvitals vitals.  Clean them up...'
                %(len(rv2.vitals),))
    #rv2.clean_up_vitals(basin_center_checker=basin_center_checker,
    #                    name_number_checker=name_number_checker)
    logger.info('Have %d tcvitals vitals after cleaning.'
                %(len(rv2.vitals),))
    tcprio=10
    for vit in reversed(rv2.vitals):
        if vit.center!='JTWC': continue
        if vit.YMDH!=YMDH: continue
        have=False
        for myvit in rv:
            if myvit.stormid3==vit.stormid3 and \
                    conf.cycle==myvit.when:
                logger.info('Not using tcvitals "%s" because of "%s"'%(
                        vit.as_message(),myvit.as_message()))
                have=True
                break
        if not have:
            vit2=vit.copy()
            setattr(vit2,'priority',tcprio)
            setattr(vit2,'source','tcvitals')
            rv.append(vit2)
            logger.warning('Adding vital from tcvitals: "%s"'%(
                    vit2.as_message()))

    #rv2.renumber(threshold=14)
    for vit in reversed(rv2.vitals):
        if vit.center!='JTWC': continue
        if vit.YMDH!=YMDHm6: continue
        if vit.stnum>=50: continue
        have=False
        for myvit in rv:
            if myvit.stormid3==vit.stormid3 and \
                   conf.cycle==myvit.when:
                logger.info('Not extrapolating "%s" because of "%s"'%(
                        vit.as_message(),myvit.as_message()))
                have=True
                break
        if not have:
            assert(vit.when==cycm6)
            vit2=vit+6  # extrapolate six hours
            assert(vit2.when==cyc)
            setattr(vit2,'priority',tcprio)
            setattr(vit2,'source','extrapolated tcvitals')
            rv.append(vit2)
            logger.warning('Extrapolating "%s" +6hrs => "%s"'%(
                    vit.as_message(),vit2.as_message()))

    return rv

def addlog(loghere,logger):
    try:
        thedir=os.path.dirname(loghere)
        produtil.fileop.makedirs(thedir,logger=logger)
        logstream=open(loghere,'at')
        loghandler=logging.StreamHandler(logstream)
        loghandler.setLevel(logging.DEBUG)
        logformat=logging.Formatter(
            "%(asctime)s.%(msecs)03d %(name)s (%(filename)s:%(lineno)d) "
            "%(levelname)s: %(message)s",  "%m/%d %H:%M:%S")
        loghandler.setFormatter(logformat)
        logging.getLogger().addHandler(loghandler)
    except Exception as e:
        logger.error('%s: cannot set up logging: %s'%(
                loghere,str(e)),exc_info=True)

def main():
    """!Main program for setup_hurricane"""

    # ------------------------------------------------------------------
    # Setup the produtil package and get a logger object.
    #
    # Only log ERRORs and higher to stdout/err to avoid WARNING
    # messages about tcvitals issues.
    produtil.setup.setup(jobname='setup_hurricane',
                         ologlevel=logging.ERROR,
                         eloglevel=logging.ERROR)
    logger=logging.getLogger('setup_hurricane')

    # ------------------------------------------------------------------
    # Decide between:
    #   stormfiles mode - run off of storm files, as in operations
    #   tcvitals mode - for testing purposes only, use tcvitals
    #      to simulate execution of past or future dates.
    source='tcvitals' if len(sys.argv)>1 else 'stormfiles'

    # ------------------------------------------------------------------
    # Add logging to another file if requested
    loghere=os.environ.get('SETUP_HURRICANE_LOG','')
    if loghere:
        addlog(loghere,logger)

    # ------------------------------------------------------------------
    # Figure out where we are and who we are.  Make sure SDM does not
    # run a test version.
    hafs_make_jobs_py=os.path.realpath(__file__)
    HOMEhafs=os.path.dirname(os.path.dirname(hafs_make_jobs_py))
    PARMhafs=os.path.join(HOMEhafs,'parm')
    user=os.environ['USER']

    # Make sure the SDM does not run a test version of this script:
    #if user=='SDM':
    #    # Make sure we're using the operational version.
    #    if re.match('/+nwprod',HOMEhafs):
    #        logger.info('You are SDM, running from an /nwprod copy of HAFS.')
    #        logger.info('I will deliver messages to operational locations.')
    #        logger.info('Rerun as another user for a test run instead.')
    #    else:
    #        logger.error('Safeguard activated: SDM must use setup_hurricane from /nwprod or /nwprod2 to generate tracks.')
    #        logger.error('Rerun as another user (not SDM) to run a test.')
    #        sys.exit(1)

    # ------------------------------------------------------------------
    # Read the configuration file
    if 'SETUP_HURRICANE_CONF' in os.environ and os.environ['SETUP_HURRICANE_CONF']:
        conffile=os.environ['SETUP_HURRICANE_CONF']
    else:
        confu=os.path.join(PARMhafs,'setup_hurricane_'+user+'.conf')
        confa=os.path.join(PARMhafs,'setup_hurricane.conf')
        if os.path.exists(confu):
            conffile=confu
        elif os.path.exists(confa):
            conffile=confa
        else:
            logger.error('FATAL ERROR: %s: does not exist'%(confa,))
            logger.error('FATAL ERROR: %s: does not exist'%(confu,))
            logger.error('FATAL ERROR: Please make one of them and rerun.')
            sys.exit(1)

    conf=hafs.config.HAFSConfig()
    conf.add_section('setup_hurricane')
    conf.set_options('setup_hurricane',deliver='no',envir='prod',maxhfsb='5',maxhfsa='7')
    with open(conffile) as f:
        conf.readfp(f)

    conf_error=False

    # Get default NHC storm directory if storm files are to be used:
#    if source!='tcvitals':
#        try:
#            conf.getstr('setup_hurricane','nhc_input')
#        except KeyError as e:
#            logger.error('Variable %s is undefined or otherwise invalid' % (str(e),))
#            conf_error=True

    # Get default COM directory paths using compath.py
    try:
        conf.getstr('setup_hurricane','tcvitals',morevars={'year':'1970'})
    except KeyError as e:
        if str(e).strip("'") == 'COMINarch':
            try:
                from compath import get_compath
                os.environ['COMINarch'] = get_compath('gfs/{0}/syndat'.format(os.environ.get('gfs_ver')))
            except ImportError:
                logger.error('FATAL ERROR: tcvitals is invalid and compath.py is unavailable. ' +
                             'Define tcvitals in the conf file or load the prod_util module!')
                conf_error=True
        else:
            logger.error('FATAL ERROR: Variable %s is undefined or otherwise invalid' % (str(e),))
            conf_error=True
    try:
        conf.getstr('setup_hurricane','hfsb_output')
        conf.getstr('setup_hurricane','hfsa_output')
    except KeyError as e:
        if str(e).strip("'").startswith('COMINmsg'):
            try:
                from compath import get_compath
                os.environ['COMINmsg_hfsb'] = get_compath('hafs/{0}/inphfsb'.format(os.environ.get('hafs_ver')))
                os.environ['COMINmsg_hfsa'] = get_compath('hafs/{0}/inphfsa'.format(os.environ.get('hafs_ver')))

            except ImportError:
                logger.error('FATAL ERROR: output directories are invalid and compath.py is unavailable.\n' +
                             'Define hfsb_output and hfsa_output in the conf file or load the prod_util module!')
                conf_error=True
        else:
            logger.error('FATAL ERROR: Variable %s is undefined or otherwise invalid' % (str(e),))
            conf_error=True

    if conf_error:
        logger.error('FATAL ERROR: Config error happenned, exiting.')
        sys.exit(1)

    logger.info('Message file directories set to %s and %s' % (
                conf.getstr('setup_hurricane','hfsb_output'),
                conf.getstr('setup_hurricane','hfsa_output')))

    # ------------------------------------------------------------------
    # Get the requested time and input files (if relevant):
    files=()
    if len(sys.argv)<2:
        n=datetime.datetime.now() # current time
    else:
        if len(sys.argv)>2:
            files=sys.argv[2:]
        n=tcutil.numerics.to_datetime(sys.argv[1])
    cyc=datetime.datetime( # round down to 6-hourly synoptic time
        year=n.year,month=n.month,day=n.day,hour=int(n.hour/6)*6)
    YMDH=cyc.strftime('%Y%m%d%H')
    conf.cycle=cyc

    if not loghere:
        envir=os.environ.get('envir','prod')
        addlog(os.path.join(conf.getstr('setup_hurricane','hfsa_output'),
               'setup_hurricane.log'),logger)

    # ------------------------------------------------------------------
    # Read the vitals and clean them up.
    if source=='tcvitals':
        rv=read_tcvitals(logger,files,cyc)
        assert(rv)
    else:
        rv=read_nhc_and_jtwc_inputs(logger,conf)
        assert(rv)

    # ------------------------------------------------------------------
    # Pass control to the StormCurses
    logger.info('Vitals prepared.  Start StormCurses quasi-GUI.')
    sc=StormCurses(rv.vitals,YMDH,logger,
                   fake_sources=(source=='tcvitals'))
    with sc:
        setup=sc.event_loop()
    if setup:
        logger.info('User pressed [S] and typed Y E S - setting up models.')
        sc.setup(conf)
    else:
        logger.info('User pressed [Q] - will NOT setup hurricane models.')

if __name__ == '__main__':
    main()
