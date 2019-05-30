"""!Defines the Revital class which manipulates tcvitals files.

This module deals with rewriting TCVitals files to remove errors,
change Invests to storms, and other such operations.  """

##@var __all__
# List of symbols to export by "from tcutil.revital import *"
__all__=['Revital','RevitalError','RevitalInitError']

import logging, datetime, getopt, sys, os.path, re, math, errno, collections
import tcutil.storminfo, tcutil.numerics

from tcutil.numerics import great_arc_dist, to_fraction, to_timedelta

class RevitalError(Exception): 
    """!Base class of errors related to rewriting vitals."""
class RevitalInitError(RevitalError): 
    """!This exception is raised when an argument to the Revital
    constructor is invalid."""

# Conveniences to simplify later code and avoid extra calls to to_timedelta:

##@var zero_time
# A datetime.timedelta that represents zero time difference
zero_time=to_timedelta(0)

##@var two_days
# A datetime.timedelta that represents positive 48 hours
two_days=to_timedelta(3600*24*2)

##@var six_hours
# A datetime.timedelta that represents positive 6 hours
six_hours=to_timedelta(3600*6)

class Revital:
    """!This class reads one or more tcvitals files and rewrites them
    as requested."""
    def __init__(self,logger=None,invest_number_name=False,stormid=None,
                  adeckdir=None,renumberlog=None,
                  search_dx=200e3, search_dt=None, debug=True,copy=None):
        """!Creates a Revital object:

        @param logger A logging.Logger object for logging or None to
            disable.  Default: None
        @param invest_number_name Rename storms to have the last
            non-INVEST name seen
        @param stormid Ignored.
        @param adeckdir Directory with A deck files.  This is used to
            read the storm type information from CARQ entries and
            append them to produce vitals with the storm type from
            vitals that lack it.
        @param renumberlog Ignored.
        @param search_dx Search radius in km for deciding whether two
            storms are the same.
        @param search_dt Search timespan for finding two storms that are
            the same.
        @param debug If True, enables DEBUG level logging.
        @param copy Used by copy() to make a shallow copy of a
            Revital.  If specified, the other arguments are ignored, and
            the copy's contents are copied.  Do not use this argument.
            If you need a copy, use copy() instead."""
        if copy is not None:
            (  self.search_dx, self.search_dt, self.logger, self.debug ) = \
             ( copy.search_dx, copy.search_dt, copy.logger, copy.debug )
            (  self.invest_number_name, self.adeckdir, self.is_cleaned ) = \
             ( copy.invest_number_name, copy.adeckdir, copy.is_cleaned )
            self.carqdat=dict()
            for key,cdat in copy.carqdat.iteritems():
                self.carqdat[key]=dict()
                for ymdh,card in cdat.iteritems():
                    self.carqdat[key][ymdh]=card.copy()
            self.carqfail=set(copy.carqfail)
            self.vitals=[ v.copy() for v in copy.vitals ]
            return
        self.search_dx=float(search_dx)
        self.search_dt=six_hours if(search_dt is None) else search_dt
        self.logger=logger
        self.debug=debug and logger is not None
        self.invest_number_name=bool(invest_number_name)
        self.adeckdir=adeckdir
        if adeckdir is not None:
            if not os.path.isdir(adeckdir):
                raise RevitalInitError(
                    'Specified directory %s is not a directory.'
                    %(adeckdir,))

        self.carqdat=dict()
        self.carqfail=set()
        self.vitals=list()
        self.is_cleaned=False
        return

    ##@var search_dx
    # Search radius in km for deciding whether two storms are the same.

    ##@var search_dt
    # Search timespan for finding two storms that are the same

    ##@var logger
    # The logging.Logger to use for log messages

    ##@var debug
    # If True, send numerous extra log messages at DEBUG level.

    ##@var invest_number_name
    # Rename stores to have the last non-INVEST name seen

    ##@var adeckdir
    # Directory with A deck files.  This is used to
    # read the storm type information from CARQ entries and
    # append them to produce vitals with the storm type from
    # vitals that lack it.

    ##@var carqdat
    # Contains CARQ entries read from the adeckdir A deck files.

    ##@var carqfail
    # Set of longstormid entries that had no CARQ data.

    ##@var vitals
    # The list of tcutil.storminfo.StormInfo objects being revitalized.

    ##@var is_cleaned
    # Has clean_up_vitals() been called since the last operation that
    # modified the vitals?

    def append(self,vital):
        """!Appends a vital entry to self.vitals.
        @param vital an tcutil.storminfo.StormInfo to append"""
        if not isinstance(vital,tcutil.storminfo.StormInfo):
            raise TypeError('The argument to Revital.append must be an tcutil.storminfo.StormInfo')
        self.vitals.append(vital)
    def extend(self,vitals):
        """!Given the specified iterable object, appends its contents
        to my own.
        @param vitals an iterable object filled with tcutil.storminfo.StormInfo"""
        self.vitals.extend(vitals)
        self.is_cleaned=False
    def copy(self):
        """!Returns a deep copy of this Revital.  Modifying the copy
        will not modify the original."""
        return Revital(copy=self)

    def readvitals(self, vitalslist, raise_all=True):
        """!Same as readfiles except the tcvitals have already been
           parsed and the vitals are being passed in, not the
           filelist.  This was created to handle the multistorm fake
           storm.  This is being used to validate any self generated
           vitals by the fake storm.
           @param vitalslist a list of strings with tcvitals data
           @param raise_all if True, raise exceptions for any parsing errors.
           @returns self"""

        if len(vitalslist) == 0:
            self.logger.critical('No parsed tcvitals provided to revital.readvitals')
        else:
            self.logger.info('Processing parsed tcvitals, line1: %s'%(vitalslist[:1]))

        self.vitals.extend(tcutil.storminfo.parse_tcvitals(
            vitalslist,raise_all=raise_all,logger=self.logger))
        self.is_cleaned=False
        if self.logger is not None and self.debug:
            self.logger.debug('line count: %d'%(len(self.vitals),))
        return self

    def readfiles(self,filelist,raise_all=True):
        """!Reads the list of files and parses them as tcvitals files.
        @param filelist a list of string filenames
        @param raise_all if True, all exceptions are raised.  If False,
           then exceptions are ignored, and the function will attempt to
           process all files, even if earlier ones failed."""
        if isinstance(filelist,basestring):
            filelist=[filelist]
        lines=list()
        opened=False 
        for tcvitals in filelist:
            if self.logger is not None:
                self.logger.info('read file: %s'%(tcvitals,))
            try: 
                with open(tcvitals,'rt') as f: 
                    lines.extend(f.readlines()) 
                opened=True 
            except EnvironmentError as e: 
                if e.errno==errno.ENOENT or e.errno==errno.EISDIR: 
                    self.logger.warning(tcvitals+': cannot open: '+str(e)) 
                    if raise_all: raise 
                else: 
                    self.logger.warning(tcvitals+': cannot open: '+str(e)) 
                    raise 
        if not opened: 
            self.logger.critical('No message files or tcvitals files '
                                 'provided to revital.readfiles.') 
        self.vitals.extend(tcutil.storminfo.parse_tcvitals(
            lines,raise_all=raise_all,logger=self.logger))
        self.is_cleaned=False
        if self.logger is not None and self.debug:
            self.logger.debug('line count: %d'%(len(self.vitals),))

    def move_latlon(self,vital,dt):
        """!Returns a tuple containing the latitude and longitude of
        the storm at a different position according to the storm
        motion vector.
        @param vital the tcutil.storminfo.StormInfo for the storm fix being extrapolated
        @param dt the time difference in hours"""
        stormspeed=getattr(vital,'stormspeed',None)
        stormdir=getattr(vital,'stormdir',None)
        if stormspeed is None or stormspeed<0 or stormdir is None or \
                stormdir<0 or stormspeed==0:
            # No storm motion vector
            return (None,None)
        pi180=math.pi/180.
        Rearth=6378137.
        k=stormspeed*dt/Rearth / pi180
        moveangle=pi180*(90.0-stormdir)
        dlat=k*math.sin(moveangle)
        dlon=k*math.cos(moveangle)/math.cos(vital.lat*pi180)
        return (vital.lat+dlat, vital.lon+dlon)

    def readcarq(self,longstormid):
        """!Tries to find the CARQ data for the specified storm.  Reads
        it into the self.carqdat array, or adds the stormid to
        self.carqfail if the data cannot be read in.
        @param longstormid the long stormid of the storm to read
        @post the self.carqfail will contain longstormid OR 
          self.carqdat[longstormid] will contain data for that storm"""
        if longstormid in self.carqfail: return
        filename=os.path.join(self.adeckdir,'a%s.dat'%(longstormid,))
        if not os.path.exists(filename) or os.path.getsize(filename) == 0:
            self.carqfail.add(longstormid)
            return
        with open(filename,'rt') as f:
            data=[ line for line in f.readlines() ]
        carq=tcutil.storminfo.parse_carq(data,logger=self.logger)
        cdat=dict()
        for card in carq:
            cdat[card.YMDH]=card
        self.carqdat[longstormid]=cdat

    def clean_up_vitals(self,name_number_checker=None,
                        basin_center_checker=None,vitals_cmp=None): 
        """!Calls the tcutil.storminfo.clean_up_vitals on this object's
        vitals.  The optional arguments are passed to
        tcutil.storminfo.clean_up_vitals.

        @param name_number_checker a function like
          tcutil.storminfo.name_number_okay() for validating the storm
          name and number
        @param vitals_cmp a cmp-like function for ordering tcutil.storminfo.StormInfo objects
        @param basin_center_checker a function like
          tcutil.storminfo.basin_center_okay() for checking the storm
          basin and forecast center (RSMC)

        @post is_cleaned=True"""
        self.vitals=tcutil.storminfo.clean_up_vitals( 
            self.vitals,name_number_checker=name_number_checker,
            basin_center_checker=basin_center_checker,
            vitals_cmp=vitals_cmp) 
        self.is_cleaned=True

    ##################################################################

    def renumber_one(self,vital,lastvit,vit_motion,other_motion,threshold):
        """!Internal function that handles renumbering of storms.

        @protected
        This is an internal implementation function that should
        never be called directly.  It handles part of the work of
        renumbering storms in the list of vitals.  You should call
        "renumber" instead.
        @param vital the vital being renumbered
        @param lastvit the last vital seen
        @param vit_motion time since vital
        @param other_motion time since other storms vital
        @param threshold cold start threshold, used to decide when to
           stop connecting an invest to a non-invest"""
        renumbered=False
        debug=self.debug and self.logger is not None
        logger=self.logger
    
        assert(vit_motion>=0)
        assert(other_motion<=0)
    
        # Get the storm's lat and lon for searching.  For the "subtract
        # the storm motion vector" mode (sub_motion=True), this is the
        # location minus the storm motion vector.  Otherwise, it is the
        # location.
        if vit_motion>0:
            (lat,lon)=self.move_latlon(vital,3600.0*vit_motion)
            if self.debug and lat is not None and lon is not None:
                self.logger.debug('    -- lat=%.3f lon=%.3f'%(lat,lon))
        else:
            (lat,lon)=(vital.lat,vital.lon)
        if debug and lat is None or lon is None:
            if debug: logger.debug('    -- no lat,lon for search')
            return False
    
        logger=self.logger
        debug=self.debug and logger is not None
        renumbered=False
        for stormid in lastvit.keys():
            othervit=lastvit[stormid]
            if threshold:
                old_id=getattr(othervit,'old_stnum',0)
                if old_id>=90 and othervit.wmax<threshold:
                    if debug:
                        logger.debug(
                            'Old %s vit was a low intensity invest, so '
                            'not considering it: %s'%(
                                othervit.old_stormid3,othervit.line))
                    continue
            if debug: logger.debug(' vs.  %s'%(othervit.line,))
            dt=othervit.when-vital.when
            if dt<zero_time:
                dt=-dt # allow reverse traversal
            if dt==zero_time: continue # same time, so nothing to do
            if dt>two_days:
                # Old storm, so age out of lastvit to save CPU time:
                if debug: logger.debug('    -- age out othervit')
                del lastvit[stormid]
                continue
            if dt>self.search_dt:
                if debug: logger.debug('    -- dt is too large')
                continue
            if debug: logger.debug('    -- within dt')
            if othervit.has_old_stnum:
                if othervit.old_stnum==vital.stnum and \
                        othervit.basin1==vital.basin1:
                    if debug:
                        logger.debug('    -- continue renumbering (%s) %s'
                                     %(vital.stormid3,vital.line))
                    if self.invest_number_name:
                        vital.rename_storm('INVEST%02d%1s'%
                                           (int(vital.stnum),vital.basin1))
                    vital.change_basin(othervit.basin1,othervit.pubbasin2)
                    vital.renumber_storm(int(othervit.stormid3[0:2]))
                    renumbered=True
                    if debug:
                        logger.debug(' NOW  %s'%(vital.line,))
                    lastvit[othervit.stormid3]=vital
                continue
    
            if other_motion<0:
                (otherlat,otherlon)=self.move_latlon(
                    othervit,3600.0*other_motion)
                if debug and otherlat is not None and otherlon is not None:
                    logger.debug('    -- vs lat=%.3f lon=%.3f'
                                 %(otherlat,otherlon))
            else:
                (otherlat,otherlon)=(othervit.lat,othervit.lon)
            if otherlat is None or otherlon is None:
                if debug: 
                    logger.debug(
                        '    -- cannot get other vitals location; moving on.')
                continue
    
            dist=great_arc_dist(lon,lat,otherlon,otherlat)
            if not (dist<self.search_dx and dt==six_hours):
                if debug: 
                    logger.debug(
                        '    -- not kinda near (distance %f km)'
                        %(dist/1e3,))
                continue
    
            if debug:
                logger.debug('    -- within dx: renumber to %s and store'%\
                                 (othervit.stormid3,))
            if self.invest_number_name:
                vital.rename_storm('INVEST%02d%1s'%
                                   (int(vital.stnum),vital.basin1))
            vital.change_basin(othervit.basin1,othervit.pubbasin2)
            vital.renumber_storm(int(othervit.stormid3[0:2])) 
            renumbered=True
            if debug: logger.debug(' NOW  %s'%(vital.line,))
            lastvit[othervit.stormid3]=vital
        if debug: logger.debug('    - renumbered = %s'%(repr(renumbered),))
        return renumbered

    def renumber(self,unrenumber=False,clean=True,threshold=0,
                 discard_duplicates=True):
        """!Renumbers storms with numbers 90-99, if possible, to have
        the same number as later 1-49 numbered storms.  

        Loops over all vitals from last to first, renumbering 90-99
        storms to have the same storm number as later 1-49 storms.  

        @param threshold If a threshold is given, then a cycle will
        only be considered for renumbering if it is either above that
        threshold, or is not an Invest. 
        @param unrenumber If unrenumber is True, the original storm
        numbers are restored after renumbering.  
        @param discard_duplicate If True, discard invests that are 
          duplicates of non-invests.  This feature is disabled if
          unrenumber is enabled or cleaning is disabled.
        @param clean If clean is True (the default), then
        self.clean_up_vitals is called, which will (among other
        things) delete vitals lines that have the same time and storm
        ID.  The cleaning is done after unrenumbering, so if both
        options are turned on, the result will contain only one entry
        per storm ID per time, but with all storm IDs that are
        available for a given storm at any one time."""

        if not self.is_cleaned: self.clean_up_vitals()
        self.is_cleaned=False

        if not threshold: threshold=0
        threshold=int(threshold)

        lastvit=dict()
        debug=self.debug and self.logger is not None
        logger=self.logger

        for vital in reversed(self.vitals):
            if debug: logger.debug('VITAL %s'%(vital.line,))
            key=vital.stormid3
            if vital.stnum>=50 and vital.stnum<90:
                continue # discard test and internal use numbers
            elif vital.stnum<90:
                lastvit[vital.stormid3]=vital
                continue
            elif self.renumber_one(vital,lastvit,0,0,threshold):
                if debug: logger.debug('    -- done renumbering this one')
                continue
            if debug:
                logger.debug(
                    ' - SEARCH AGAIN: subtract storm motion from later cycle')
            if self.renumber_one(vital,lastvit,0,-6,threshold):
                continue
            if debug:
                logger.debug(
                    ' - SEARCH AGAIN: add storm motion to earlier cycle')
            if self.renumber_one(vital,lastvit,6,0,threshold):
                continue
            if debug:
                logger.debug(
                    ' - SEARCH AGAIN: add half motion to later and '
                    'earlier cycle')
            if self.renumber_one(vital,lastvit,3,-3,threshold):
                continue
        if unrenumber:
            self.swap_numbers()
        if clean:
            if not unrenumber and discard_duplicates:
                if logger is not None:
                    logger.info('Delete Invests that are duplicates of non-Invests.')
                self.delete_invest_duplicates()
            if logger is not None:
                logger.info('Clean up the vitals again after renumbering...')
            self.vitals=tcutil.storminfo.clean_up_vitals(self.vitals)

    def delete_invest_duplicates(self):
        """!Deletes Invest entries that have the same location and time
        as non-invest entries."""
        o=collections.defaultdict(dict)
        # First pass: put in all entries with stnum<50.  If more than
        # one such entry has the same location, the last is kept.
        # Locations are rounded to the nearest 0.2 degrees to allow
        # for slight adjustments in lat/lon to be considered the same
        # location.
        for v in self.vitals:
            if v.stnum<50:
                k=(int(v.lat*5), int(v.lon*5))
                o[v.YMDH][k]=v
        # Second pass: put in storms with id >90 if no other storm has
        # the same location.
        for v in self.vitals:
            if v.stnum>=90:
                k=(int(v.lat*5), int(v.lon*5))
                if k not in o[v.YMDH]:
                    o[v.YMDH][k]=v
        # Final pass: create the new list:
        l=list()
        for yv in o.itervalues():
            for v in yv.itervalues():
                l.append(v)
        self.vitals=l

    def swap_numbers(self):
        """!Calls swap_numbers on all vitals to swap old and new storm IDs."""
        for vital in self.vitals:
            vital.swap_numbers()
        self.is_cleaned=False

    def mirror_renumbered_vitals(self):
        """!Duplicates all vitals that have been renumbered, creating
        one StormInfo with the old number and one with the new
        number."""
        newvit=list()
        for vital in self.vitals:
            newvit.append(vital)
            if 'old_stormid3' in vital.__dict__:
                newvit.append(vital.old())
        self.vitals=newvit
        self.is_cleaned=False

    def discard_except(self,keep_condition):
        """!Discards all vitals except those for which the
        keep_condition function returns True.  

        @param keep_condition A function that receives a StormInfo
        object as its only argument, returning True if the vital
        should be kept and False if not.  
        @note The list will be unmodified if an exception is thrown."""
        newvit=list()
        for vit in self.vitals:
            if keep_condition(vit):
                newvit.append(vit)
        self.vitals=newvit

    ##################################################################

    def swap_names(self):
        """!This subroutine undoes the effect of renaming by swapping
        old and new names"""
        for vital in self.vitals:
            if hasattr(vital,'old_stormname'):
                (vital.old_stormname,vital.stormname) = \
                    (vital.stormname,vital.old_stormname)

    def rename(self):
        """!This subroutine renames storms so that they have the last name seen
        for their storm number."""
        # Rename storms to have the last name seen
        logger=self.logger
        debug=self.debug and logger is not None
        lastname=dict()
        for vital in reversed(self.vitals):
            key=vital.stormid3
            if key in lastname:
                name=lastname[key]
                if vital.stormname!=name:
                    if debug:
                        logger.debug('Rename to %s: %s'%(name,vital.line))
                    vital.rename_storm(name)
                    if debug: logger.debug('Now: %s'%(vital.line,))
            else:
                lastname[key]=vital.stormname[0:9]

    def add_stormtype(self):
        """!Add the storm type parameter from the CARQ entries in the A
        deck."""
        logger=self.logger
        debug=self.debug and logger is not None
        for vital in self.vitals:
            lsid=vital.longstormid.lower()
            when=vital.YMDH
            if not lsid in self.carqdat:
                self.readcarq(lsid) # try to read this storm's data
            if lsid not in self.carqdat:
                if logger is not None:
                    logger.warning('storm %s: no CARQ data found.  '
                                   'Using stormtype XX.'%(lsid,))
                vital.set_stormtype('XX')
                continue
            carq=self.carqdat[lsid]
            if when not in carq:
                if logger is not None:
                    logger.warning('storm %s cycle %s: no CARQ data for '
                                   'this cycle.  Using stormtype XX.'
                                   %(lsid,when))
                vital.set_stormtype('XX')
                continue
            vital.set_stormtype(carq[when])

    def sort_by_function(self,cmpfun):
        """!Resorts the vitals using the specified cmp-like function.
        @param cmpfun a cmp-like function for comparing tcutil.storminfo.StormInfo objects"""
        self.vitals=sorted(self.vitals,cmp=cmpfun)

    def sort_by_storm(self):
        """!Resorts the vitals by storm instead of date.  See
        tcutil.storminfo.vit_cmp_by_storm for details."""
        self.vitals=sorted(self.vitals,cmp=tcutil.storminfo.vit_cmp_by_storm)
    def __iter__(self):
        """!Iterates over all vitals, yielding StormInfo objects for
        each one."""
        for x in self.vitals: yield x
    def each(self,stormid=None,old=False):
        """!Iterates over all vitals that match the specified stormid.
        If no stormid is given, iterates over all vitals.  

        @param stormid the storm ID to search for.  This can be
          a stormid3, stormid4 or longstormid.
        @param old If old=True, also searches the old_ copy of the
        stormid.  Any of stormid3, stormid4 or longstormid are
        accepted."""
        # Define a lexical scope function "selected" that will tell us
        # if a StormInfo object should be printed:
        if stormid is None:
            def selected(vital): return True
        else:
            stormid=str(stormid).upper()
            if   re.search('\A\d\d[a-zA-Z]\Z',stormid):  
                def selected(vital): return vital.stormid3==stormid
                if old:
                    def old_selected(vital):
                        return 'old_stormid3' in vital.__dict__ and \
                            vital.old_stormid3==stormid
            elif re.search('\A[a-zA-Z]{2}\d\d\Z',stormid):
                def selected(vital): return vital.stormid4==stormid
                if old:
                    def old_selected(vital):
                        return 'old_stormid4' in vital.__dict__ and \
                            vital.old_stormid4==stormid
            elif re.search('\A[a-zA-Z]{2}\d{6}\Z',stormid):
                def selected(vital): return vital.longstormid==stormid
                if old:
                    def old_selected(vital):
                        return 'old_longstormid' in vital.__dict__ and \
                            vital.old_longstormid==stormid
            else:
                raise RevitalError('Invalid storm id %s.  It must be '
                                   'one of these three formats: 04L '
                                   'AL04 AL042013'%(str(val),))

        # Loop over all vitals sending them to the given stream:
        for vit in self.vitals:
            if selected(vit):
                yield vit
            elif old:
                if old_selected(vit):
                    yield vit
    def print_vitals(self,stream,renumberlog=None,format='line',stormid=None,
                     old=False):
        """!Print the vitals to the given stream in a specified format.

        @param stream The stream (eg.: opened file) to receive the vitals.
        @param format Either "tcvitals" to reformat as tcvitals (cleaning up
         any errors); or "line" to simply print the original data for each
         line; or "HHS" to use the HHS output format.  (Do not use the "HHS"
         option unless you are HHS.)
        @param renumberlog If given, sends information about renaming and
        renumbering of the vitals to a second stream.  
        @param stormid The "stormid" argument is used to restrict
        printing to only a certain stormid.
        @param old If True, then vitals with an old_stormid that matches are
          also printed."""
        for vit in self.each(stormid=stormid,old=old):
            if renumberlog is not None:
                xstormid3=vit.stormid3
                oldid=getattr(vit,'old_stormid3',xstormid3)
                name=vit.stormname
                oldname=getattr(vit,'old_stormname',name)
                renumberlog.write('%10s %3s %3s %-9s %-9s\n'%
                   (vit.YMDH,oldid,xstormid3,oldname[0:9],name[0:9]))
            if format=='tcvitals':
                print>>stream, vit.as_tcvitals()
            elif format=='renumbering':
                s=vit.as_tcvitals()
                oldid=getattr(vit,'old_stormid3',vit.stormid3)
                oldname=getattr(vit,'old_stormname',vit.stormname)
                print>>stream,'%3s %9s => %s'%(oldid,oldname,s)
            elif format=='HHS':
                print>>stream, '%s %s "TCVT"'%(vit.longstormid.lower(),vit.YMDH)
            else:
                print>>stream, vit.line

    def hrd_multistorm_sorter(self,a,b):
        """!A drop-in replacement for "cmp" that can be used for sorting or
        comparison.  Returns -1 if a<b, 1 if a>b or 0 if a=b.  Decision is
        made in this order:

        1. User priority (a.userprio): lower (priority 1) is "more
            important" than higher numbers (priority 9999 is fill value).

        2.  Invest vs. non-invest: invest is less important

        3.  wind: stronger wind is more important than weaker wind

        4.  North Atlantic (L) storms: farther west is more important

        5.  North East Pacific (E) storms: farther East is more important

        If all of the above values are equal, 0 is returned.
        @returns -1, 0 or 1
        @param a,b the vitals to compare"""
        a_userprio=getattr(a,'userprio',9999)
        b_userprio=getattr(b,'userprio',9999)
        a_invest=1 if (a.stormname=='INVEST') else 0
        b_invest=1 if (b.stormname=='INVEST') else 0

        c = cmp(a_userprio,b_userprio) or cmp(a_invest,b_invest) or\
          -cmp(a.wmax,b.wmax) or\
          (a.basin1=='L' and b.basin1=='L' and cmp(a.lon,b.lon)) or \
          (a.basin1=='E' and b.basin1=='E' and -cmp(a.lon,b.lon))
        return c

    def multistorm_priority(self):
        """!Does nothing."""
        pass
