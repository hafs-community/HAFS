"""!low-level wrf implementation, underlying hwrf.wrf

This module contains low-level classes that underlie the
implementation of the hwrf.wrf module.  It also contains the WRFOutput
class, which is used to describe a WRF output file, and the
parse_wrf_outname, which turns WRF-style filename formats
(wrfout_d<domain>_<date>) into filenames."""

import fractions,math,re,datetime, pdb, logging

from hwrf.numerics import *
from hwrf.namelist import *
from hwrf.exceptions import *

## @var __all__
# the symbols exported by "from hwrf.wrfbase import *"
__all__=['parse_wrf_outname','WRFOutput','WRFDomainBase','WRFDomains']

########################################################################

def parse_wrf_outname(outname,grid_id,date,nocolons):
    """!generates wrf filenames from templates like construct_filename

    Takes a wrf outname, a grid_id and a date and produces the final
    string outname.  The mandatory boolean argument nocolons is set to
    the namelist &time_control nocolons value.  This mimics the
    construct_filename family of functions in the WRF source code.
    @param outname the filename format as passed to WRF
    @param grid_id the integer grid ID 
    @param date the date (anything accepted by hwrf.numerics.to_datetime)
    @param nocolons if True, colons in the date are converted to underscores
    @return the string filename"""
    assert(isinstance(outname,basestring))
    out=outname
    if re.search('(?i)<domain>',out):
        out=re.sub('(?i)<domain>','%02d'%(int(grid_id),),out)
    if re.search('(?i)<date>',out):
        out=re.sub('(?i)<date>',to_datetime(date).\
                       strftime('%Y-%m-%d_%H:%M:%S'),out)
    if nocolons:
        out=out[0:2]+re.sub(':','_',out[2:])
    return out

########################################################################

class WRFOutput:
    """!A class that provides information about WRF output and input files.

    This class is used throughout the HWRF scripts to identify WRF
    input and output filenames.  The underlying implementation knows
    how to deal with the odd rounding issues involved in predicting
    WRF filenames based on timesteps, including fractional timesteps.
    This class also tracks the analysis time, forecast time, stream
    and full path to the file."""
    def __init__(self,anltime,stream,domain,path,validtime=None,
                 fcstdelta=None):
        """!Creates a WRFOutput object that knows the path to its file
        (self.path()), the output time as a datetime
        (self.validtime()), the simulation start time as a datetime
        (self.anltime()), the output forecast second as a timedelta
        (self.fcstdelta()), the name of the WRF stream
        (self.stream()), and the WRF domain object (self.domain()).
        Do not modify the domain object or many things may break.

        You must specify exactly one of validtime or fcstdelta.  If
        you specify both, OverspecifiedOutputTime will be raised.  If
        you specify neither, NoOutputTime will be raised."""
        assert(isinstance(stream,basestring))
        assert(isinstance(path,basestring))
        self._anltime=to_datetime(anltime)
        if validtime is None and delta is None:
            raise NoOutputTime('In WRFOutput.__init__, both validtime and '
                               'fcstdelta were None')
        elif validtime is not None:
            self._validtime=to_datetime(validtime)
            self._fcstdelta=self._validtime-self._anltime
        elif fcstdelta is not None:
            self._fcstdelta=to_timedelta(fcstdelta)
            self._validtime=self._anltime + self._fcstdelta
        else:
            raise OverspecifiedOutputTime(
                'In WRFOutput.__init__, both validtime and fcstdelta were '
                'specified.  You must specify exactly one.')
        self._path=path
        self._stream=str(stream)
        assert(domain is None or isinstance(domain,WRFDomainBase))
        self._domain=domain
    def __hash__(self):
        """!provides an integer hash value so this object can be used
        as a key in a dict"""
        return hash(self._domain) ^ hash(self._stream) ^ hash(self._path) ^ \
            hash(self._validtime) ^ hash(self._anltime)
    def __eq__(self,other):
        """!does this WRFOutput equal that one?

        Returns True if the other WRFOutput object is identical to
        this one, and False if it is not.  For anything other than a
        WRFOutput, returns NotImplemented.
        @param other another object"""
        if not isinstance(other,WRFOutput):
            return NotImplemented
        if not self._domain==other._domain:         return False
        if not self._stream==other._stream:         return False
        if not self._path==other._path:             return False
        if not self._validtime==other._validtime:   return False
        if not self._anltime==other._anltime:       return False
        return True
    def __str__(self):
        """!a string representation of this output file"""
        return '%s output stream=%s path=%s' % \
            (repr(self.domain()),str(self.stream()),str(self.path()))
    def __repr__(self):
        """!a detailed string representation of this output file"""
        return 'WRFOutput(%s,%s,%s,%s,validtime=%s)' % \
            (repr(self.anltime()),repr(self.stream()),repr(self.domain()),
             repr(self.path()),repr(self.validtime()))
    def path(self): 
        """!Returns the full path to the output file."""
        return self._path
    def stream(self): 
        """!Returns the lower-case name of the WRF stream."""
        return self._stream
    def domain(self): 
        """!the hwrf.wrf.WRFDomain object

        Returns the domain object for this output file's WRF domain.
        Do not modify the returned object or many things will break."""
        return self._domain
    def fcstdelta(self):
        """!the difference between the analysis and forecast time.

        Returns the time difference between the valid (output) time
        and the analysis (simulation start) time."""
        return self._fcstdelta
    def validtime(self): 
        """!Returns the output time as a datetime.datetime."""
        return self._validtime
    def anltime(self): 
        """!Returns the analysis time as a datetime.datetime."""
        return self._anltime

########################################################################

class WRFDomainBase(object):
    """!superclass of WRFDomain

    This is the superclass of WRFDomain and it should not be
    instantiated directly.  It exists to eliminate a cyclic dependency
    in the module imports. """
    def __init__(self,conf,section,name=None,copy=None):
        """!Creates a new WRFDomainBase.
          conf - an HWRFConfig with configuration information
          section - the section to read for information about this domain
          name - the name of the domain.  Default: section name.
          copy - do not specify this.  It is used by self.copy()
            to copy a WRFDomainBase."""
        self.nestlevel=None
        self.parent=None
        self.nocolons=True
        self._start=None
        self._end=None
        self._dt=None
        self._output={}
        if copy is not None:
            self.nl=copy.nl.copy()
            self.name=str(copy.name)
            (  self._start,self._end,self._dt,self.nestlevel,self.parent,
               self.nocolons) = \
              (copy._start,copy._end,copy._dt,copy.nestlevel,copy.parent,
               copy.nocolons)
        else:
            self.nl=Conf2Namelist(conf,section)
            self.name=str(section)

    ##@var name
    # the name of this domain

    ##@var nl
    # the hwrf.namelist.Conf2Namelist with namelist information for
    # this domain

    ## @var nestlevel
    #  the wrf nesting level
 
    ## @var parent
    #  the parent domain

    ## @var nocolons
    #  should colons be eliminated from filenames?

    ## @var _start
    #  the start time for this domain

    ## @var _end
    #  the end time for this domain

    ## @var _dt
    #  this domain's timestep
    
    ## @var _output
    #  internal data structure used by subclasses to track output streams

    def __hash__(self): 
        """!an integer representation for hashing"""
        return hash(self.name)
    def __repr__(self): 
        """!a detailed string representation"""
        return '<WRFDomain name=%s>'%(str(self.name),)
    def __cmp__(self,other): 
        """!integer comparison for sorting, like the cmp function.
        Sorts by domain's string name.
        @param other the domain to compare against"""
        return cmp(self.name,other.name)
    def __str__(self): 
        """!synonym for __repr__()"""
        return repr(self) 
    def get_anl_time(self):
        """!returns the analysis time

        Returns the parent _start time if there is a parent, otherwise
        returns this domain's analysis time."""
        if self.parent is not None:
            return self.parent._start
        else:
            return self._start
    def get_grid_id(self):
        """!Raises NotImplementedError.  The WRFDomain overrides this
        to return the grid id."""
        raise NotImplementedError(
            'WRFDomainBase does not implement get_grid_id')
    def remove_forbidden(self):
        """!removes forbidden namelist entries

        Removes all namelist entries that the conf files are forbidden
        from specifying.  Generally this is anything related to domain
        start, end, size, I/O or timesteps."""
        self.nl=self.nl.copy(var_subset=self._nl_subsetter)
    def _nl_subsetter(self,s,v):
        """!returns True

        This returns True.  It is intended to be overridden in
        subclasses to subset the namelist values provided by the
        configuration files.  It should return True to keep the 
        variable, or False to reject it.
        @param s the namelist name
        @param v the variable name"""
        return True
    def copy(self):
        """!Returns a copy of this object.  The copy has its own data
        structures, so modifying the copy will not modify the
        original."""
        return WRFDomainBase(None,None,copy=self)
    def _validate_timespan(self,start,end,timestep):
        """!checks if a timespan is valid

        Analyzes a potential WRF simulation start, end and timestep
        to make sure it matches assumptions made within WRF, and
        within the hwrf package.
          1. There must be no timezone (UTC only)
          2. The start time must not contain microseconds.
          3. The end time must be at or after the start time
        @param start the start time of the simulation
        @param end the end time of the simulation
        @param timestep the simulation timestep or output frequency
        @returns a tuple (ts,te,dt) where:
          - ts = the start time as a datetime.datetime
          - te = the end time as a datetime.datetime
          - dt = the timestep as a fractions.Fraction"""
        ts=to_datetime(start)
        te=to_datetime(end)
        dt=to_fraction(timestep)
        if te.tzinfo is not None or ts.tzinfo is not None:
            raise TimezoneProvided(
                'WRF start and end times must not contain timezones.')
        if te.microsecond!=0 or ts.microsecond!=0:
            raise PrecisionTooHigh(
                'WRF start and end times must lie exactly on a second.')
        if(te<ts):
            raise EndBeforeStart(
                'Invalid domain start/end times: end is before start.',ts,te)
        return (ts,te,dt)
    def set_timing(self,start,end,timestep):
        """!sets the start and end times of this domain, and the domain
        timestep

        Validates the provided inputs through _validate_timespan().
        If accepted, sets the start and end times, and the timestep.
        @param start the start time of the simulation
        @param end the end time of the simulation
        @param timestep the simulation timestep or output frequency"""
        (ts,te,dt)=self._validate_timespan(start,end,timestep)
        self._start=ts
        self._end=te
        self._dt=dt
    def init_domain(self,grid_id):
        """!initializes grid ID and non-nesting-related variables

        Initializes this domain's variables that are unrelated to
        nesting.
        @param grid_id the grid id in WRF (1=moad, 2=...)"""
    def init_as_moad(self,simstart,simend,simdt,eta_levels):
        """!initializes this domain as the outermost domain

        Initializes this domain's variables so it knows to act as
        the Mother Of All Domains (MOAD) - the outermost domain.
        The grid ID is set to 1 and the parent to None.
        @param simstart simulation start time
        @param simend simulation end time
        @param simdt simulation outer domain timestep
        @param eta_levels NMM eta vertical levels as set in the
        eta_levels namelist value."""
        (self._simstart,self._simend,self._simdt)=(simstart,simend,simdt)
        self.set_timing(simstart,simend,simdt)
        self.init_domain(1)
        self.parent=None
    def init_as_nest(self,parent,grid_id,start,end):
        """!initializes this domain as a nest

        Initializes this domain's variables to make it a nest within
        the given parent WRFDomain
        @param parent the parent of this nest
        @param grid_id the grid ID
        @param start the nest's start time
        @param end the nest's end time"""
        self.init_domain(grid_id)
        self.parent=parent
        (self._simstart,self._simend,self._simdt) = \
            (parent._simstart,parent._simend,parent._simdt)
        if not is_at_timestep(parent._start,start,parent._dt):
            raise StartNotAtParentTimestep(
                'Start time %s for domain %d is not at parent %d timestep.'
                %(parent._start,grid_id,parent.grid_id))
        self.set_timing(parent._simstart,parent._simend,parent._simdt/3)
    def make_namelist(self):
        """!creates the wrf namelist as a string, and returns it."""
        return self.nl.make_namelist()

########################################################################

class WRFDomains(object):
    """!abstract base class of WRFSimulation

    This is the abstract base class of WRFSimulation.  You should not
    instantiate it directly.  Its purpose is to combine information
    about multiple WRFDomainBase objects (or WRFDomain objects) to
    make a single namelist.  It also has the ability to make aggregate
    changes to those objects, or obtain aggregate information about
    them.  This class exists only to avoid cyclic dependencies."""
    def copy(self):
        """! duplicates this object

        Returns a deep copy of this object, providing new data
        structures so modifying the copy will not modify the original.
        The underlying WRFDomain objects and their data structures are
        also duplicated."""
        return WRFDomains(None,None,None,None,None,None,dup=self)
    ## @var _nextid
    # the next grid_id to use

    ## @var _domains_done
    # if True, we can add more domains

    ## @var nl
    # the hwrf.namelist.Conf2Namelist with domain-independent namelist
    # information like eta_levels and debug=2.  The per-domain
    # information is in each WRFDomain contained within this WRFSimulation

    ## @var _grid
    # mapping from domain name to domain
    
    ## @var _to_id
    # mapping from domain name to grid_id 

    ## @var _to_name
    # mapping from grid_id to domain name
    
    ## @var _parent
    # mapping from domain name to its parent domain's name

    ## @var _simstart 
    # the simulation start time

    ## @var _simend
    # the simulation end time

    ## @var _timestep
    # the outermost domain timestep

    ## @var _io_form
    # the default io_form

    def __init__(self,conf,section,moad,simstart,simend,timestep=None,dup=None):
        """!WRFDomains constructor

        Creates a new WRFDomains object.
        @param conf the HWRFConfig to provide configuration information
        @param section the section to use in that config object
        @param moad the Mother of All Domains, as a WRFDomain 
        @param simstart,simend - simulation start and end times
        @param timestep the simulation timestep
        @param dup do not use.  This is used by the self.copy() do do a deep
        copy of a WRFDomains."""
        if dup is not None:
            # copy constructor
            self.nl=dup.nl.copy()
            ( self._simstart,self._simend,self._timestep,self._io_form ) = \
             ( dup._simstart, dup._simend, dup._timestep, dup._io_form )
            self._nextid=dup._nextid
            self._domains_done=dup._domains_done
            self._grid=dict()
            self._to_id=dict()
            self._to_name=dict()
            self._parent=dict()
            for (name,domain) in dup._grid.iteritems():
                gid=dup._to_id[name]
                self._grid[name]=domain.copy()
                self._to_id[name]=gid
                self._to_name[gid]=name
            for (a,b) in dup._parent.iteritems(): self._parent[a]=b
            return
        self.nl=Conf2Namelist(conf,str(section))
        eta_levels=self.nl.nl_get('domains','eta_levels')
        self._simstart=to_datetime(simstart)
        self._simend=to_datetime(simend)
        if timestep is None:
            timestep=self.nl.trait_get('dt')
        self._timestep=to_fraction(timestep)
        iof=self.nl.trait_get('io_form','missing')
        if iof=='missing':
            self._io_form=2 # default IO form
        else:
            self._io_form=int(iof)

        nc=self.get_nocolons()
        mymoad=moad.copy()
        mymoad.remove_forbidden()
        mymoad.init_as_moad(self._simstart,self._simend,self._timestep,
                            eta_levels)
        mymoad.nocolons=nc

        self._grid={mymoad.name:mymoad}
        self._to_id={mymoad.name:1}
        self._to_name={1:mymoad.name}
        self._parent={} # mapping of child name to parent name
        self._nextid=2 # next unused grid ID

        self._domains_done=False # True if we can no longer add domains
    def fill_auto_starts(self,ivalue,jvalue=None,fillthis='**AUTO**',
                         centerthis='**CENTERED**'):
        """!sets i_parent_start and j_parent_start if needed
        
        For domains whose i_parent_start or j_parent_start are set
        automatically (start=centered or start=auto), this fills those
        domains starts with the provided values.  Domains that should
        be centered in their parent (start=centerthis) are centered
        relative to their parent if they have one.

        @param ivalue the i value to set if an external fortran
          program has to be run to determine the i_parent_start and
          j_parent_start.
        @param jvalue the j value to set.  Defaults to the ivalue
        @param fillthis do not change - the string to locate for start=auto
        @param centerthis do not change - the string to locate for start=centered
        @returns self"""
        if jvalue is None: jvalue=ivalue

        # Make sure that the start J is odd if it is set.  Mimics the
        # ksh calculation.
        jvalue=(int(jvalue)+1)/2*2-1
        def intify(bob):
            if isinstance(bob,int): return bob
            return int(bob,10)

        for domain in self:
            got=domain.nl.nl_get('domains','i_parent_start')
            if isinstance(got,basestring):
                if got==fillthis:
                    domain.nl.nl_set('domains','i_parent_start',ivalue)
                elif got==centerthis:
                    if domain.parent is None:
                        raise InvalidDomainStart(
                            '%s: invalid value for start.  Only nests can '
                            'have start=centered'%(domain.name,))
                    dxp=intify(domain.parent.nl.nl_get('domains','e_we'))
                    dxd=intify(domain.nl.nl_get('domains','e_we'))
                    rat=intify(domain.nl.nl_get('domains','parent_grid_ratio',3))
                    icen=(dxp-dxd//rat)//2
                    domain.nl.nl_set('domains','i_parent_start',icen)
                else:
                    raise InvalidDomainStart(
                        '%s: invalid value for start.  Must be %s, %s or an '
                        'integer'%(got,fillthis,centerthis))
            got=domain.nl.nl_get('domains','j_parent_start')
            if isinstance(got,basestring):
                if got==fillthis:
                    domain.nl.nl_set('domains','j_parent_start',jvalue)
                elif got==centerthis:
                    if domain.parent is None:
                        raise InvalidDomainStart(
                            '%s: invalid value for start.  Only nests can '
                            'have start=centered'%(domain.name,))
                    dyp=intify(domain.parent.nl.nl_get('domains','e_sn'))
                    dyd=intify(domain.nl.nl_get('domains','e_sn'))
                    rat=intify(domain.nl.nl_get('domains','parent_grid_ratio',3))
                    jcen=(dyp-dyd//rat)//2
                    jcen=(int(jcen)+1)//2*2-1
                    domain.nl.nl_set('domains','j_parent_start',jcen)
                else:
                    raise InvalidDomainStart(
                        '%s: invalid value for start.  Must be %s, %s or an '
                        'integer'%(got,fillthis,centerthis))
        return self
    def fill_auto_starts_multistorm(self,ivalues,jvalues=None):
        """For domains whose i_parent_start or j_parent_start are set
        automatically, this fills those domains starts with the
        provided values, for the fakestorm of a multistorm run.
            ivalue = a list of i start values for N storms
            jvalue = a list of j start values for N storms"""

        numberof_outernests = int((self.maxdom()-1)/2)
        numberof_istartvalues = len(ivalues)
        assert(numberof_outernests==numberof_istartvalues)
        ij_index=0
        for domain in self:

            if jvalues[ij_index] is None: jvalues[ij_index]=ivalues[ij_index]

            # Make sure that the start J is odd if it is set.  Mimics the
            # ksh calculation.
            jvalues[ij_index]=(int(jvalues[ij_index])+1)/2*2-1

            # TODO: Analyze and Harden. <jtf>
            # Basic assumption is that all the I's and J's  for the N storms
            # are passed in and the sequences ths iterates through the domains,
            # is in the same order. Once a "set" for J is made by hitting an
            # "**AUTO**", than increment the ij index and assUme that the
            # ij_index is referencing the number to the domain intended.

            got=domain.nl.nl_get('domains','i_parent_start')
            if not isinstance(got,int):
                domain.nl.nl_set('domains','i_parent_start',ivalues[ij_index])
            got=domain.nl.nl_get('domains','j_parent_start')
            if not isinstance(got,int):
                domain.nl.nl_set('domains','j_parent_start',jvalues[ij_index])
                ij_index+=1
            if numberof_istartvalues==ij_index:
                break

        return self

    def simstart(self): 
        """!Returns the simulation start time as a datetime.datetime."""
        return self._simstart
    def simend(self):  
        """!Returns the simulation end time as a datetime.datetime."""
        return self._simend
    def timestep(self): 
        """!Returns the simulation timestep as a datetime.time_delta."""
        return self._timestep

    @property
    def nocolons(self):
        """!Should colons be omitted from filenames?  

        Should colons be omitted from filenames?  This is determined
        from the configuration files, if it is specified there, or
        from the I/O forms selected (some don't support colons).  The
        return value may be cached.  To ensure the value is recomputed
        instead, you can call get_nocolons."""
        if '_nocolons_cache' not in self.__dict__:
            return self.get_nocolons()
        return self._nocolons_cache

    def get_nocolons(self):
        """!Force a recheck of whether colons be omitted from filenames.

        Should colons be omitted from filenames?  This is determined
        from the configuration files, if it is specified there, or
        from the I/O forms selected (some don't support colons).  This
        method guesses the nocolons setting from the WRF/WPS/Real
        namelist.  The result of this calculation is never cached: all
        namelist options are re-scanned and the flag is recomputed
        each time.  To get the cached value, use the nocolons property
        instead."""
        nc=self.nl.trait_get('nocolons',True)
        if not nc:
            for var,value in self.nl.nl_each('time_control'):
                if var.find('io_form')>=0:
                    iof=int(value)%100
                    if iof==1 or iof==11:
                        nc=True
                        break
        if not nc:
            for var,value in self.nl.trait_each():
                if var.find('io_form')>=0:
                    iof=int(value)%100
                    if iof==1 or iof==11: 
                        nc=True
                        break
        self.nl.nl_set('time_control','nocolons',nc)
        self.__dict__['_nocolons_cache']=nc
        return nc
    def get_io_form(self): 
        """!Gets the default io_form."""
        return self._io_form
    def set_io_form(self,i): 
        """!Sets the default io_form."""
        self._io_form=i

    ## @var io_form
    #  the default io_form if none is specified

    io_form=property(get_io_form,set_io_form,None,
                     "The default I/O form for this WRF")

    def io_form_for(self,stream):
        """!Returns the io_form for the specified stream.

        @return the io_form for the given stream.  If none is
        specified, the default io_form is returned.
        @param stream the stream, a lower-case string"""
        if stream=='history': stream='output'
        iof=self.nl.trait_get('io_form_%s'%(stream),int(self._io_form))
        return iof
    def get_moad(self): 
        """!returns the MOAD as a WRFDomain."""
        return self._grid[self._to_name[1]]
    def get_last(self):
        """!returns the last domain added to this WRF."""
        return self._grid[self._to_name[self._nextid-1]]
    def add_output(self,stream='history',domain=None,io_form=None,
                   start=None,end=None,step=None,frames_per_outfile=None,
                   outname=None):
        """!request output from one or more streams

        Requests that one or all WRF domains output the specified
        stream.  The stream should be "history" or "auxhistN" for an
        integer N>0.  Optional "domain" specifies the domain that
        should output this stream, otherwise all domains will output
        it.

        @param stream    the stream: "history" or "auxhistN" for an integer N>0
        @param domain    the domain (optional)
        @param io_form   WRF IO form.  Simply calls self.set_io_form(stream,io_form)
        @param start     output start time (anything accepted by to_datetime)
            Default: simulation start time.
        @param end       output end time (anything accepted by to_datetime.
            Default: simulation end time.
        @param step      output interval, sent into to_out_interval().  
            Default: trait stream+"_interval" or 6hrs
        @param frames_per_outfile how many output times per output file
        @param outname output name or array of output names (one per
            domain).  Can only specify for all domains, not for only
            one.  Default: leave unspecified, and let WRF use its
            defaults.

        NOTE: You cannot add any more domains after calling this
        routine."""
        self._domains_done=True
        if io_form is None:
            io_form=self.io_form_for(stream)
        else:
            self.set_io_form(stream,io_form)
        if stream=='inputout':
            if outname is None: outname='wrfinputout_d<domain>'
            self.nl.nl_set('time_control','write_input',True)
        if outname is not None: self.set_outname(stream,outname)
        if start is not None: start=to_datetime_rel(start,self._simstart)
        nc=self.get_nocolons()
        for mydom in self:
            mydom.nocolons=nc
            if domain is not None and domain!=mydom and \
                    not mydom.has_output(stream):
                mydom.no_output(stream)
            else:
                mydom.add_output(
                    stream,start,end,step,outname,frames_per_outfile,
                    io_form,simstart=self._simstart)
        if stream=='restart':
            self.make_restart_time_scalar()

        return self
    def make_restart_time_scalar(self):
        """!Ensure that only one restart frequency is set
        
        Unlike most output frequencies, the restart frequency has to
        be the same for all domains, and is specified as a scalar
        value, not an array.  This function removes per-domain restart
        frequencies, moving the restart frequency up to the
        domain-independent namelist values.  The frequency used is the
        one for the last domain listed by self.__iter__."""
        for domain in self:
            for n in [ 'restart_begin', 'restart_begin_s', 'restart_begin_m',
                       'restart_begin_h', 'restart_begin_d',
                       'restart_interval', 'restart_interval_s',
                       'restart_interval_m', 'restart_interval_h',
                       'restart_interval_d' ]:
                if domain.nl.nl_have('time_control',n):
                    v=domain.nl.nl_get('time_control',n)
                    domain.nl.nl_del('time_control',n)
                    self.nl.nl_set('time_control',n,v)
    def set_outname(self,stream,outname):
        """!Set the wrf output filename for a stream

        Sets the WRF output filename format for the specified
        stream to the given name.  The name should contain @<domain@>
        and @<date@> if appropriate.
        @param stream the stream to change
        @param outname the new output filename format"""
        if stream=='inputout': stream='input'
        self.nl.nl_set('time_control','%s_outname'
                       %(str(stream),),str(outname))
    def set_io_form(self,stream,io_form):
        """!Sets the io_form for the specified stream.

        Set the io_form for the specified stream.  The "history" and
        "output" streams are synonyms.  
        @param stream the stream
        @param io_form the io_form"""
        if stream=='history': stream='output'
        self.nl.nl_set('time_control','io_form_%s'%(stream),int(io_form))
        self.nl.trait_set('io_form_%s'%(stream),int(io_form))
    def get_io_suffix(self,stream='history'):
        """!Return the suggested output suffix for filenames

        Gets the suggested output suffix for the specified stream.
        Returns "int" for 1 and "nc" for 2 or 11.
        @param stream the stream.  Default: "history" """
        iof=self.io_form_for(stream)%100
        if iof == 1:
            ios = "int"
        elif iof == 2 or iof==11:
            ios = "nc"
        else:
            raise NameError("Unsupported IO form %d" %self_.io_form)
        return ios
    def get(self,what):
        """!return the specified domain

        Returns a domain with the given name or grid_id.  This method
        may raise IndexError or KeyError if the domain does not exist.
        If passed a WRFDomainBase, then this simulation's copy of the
        domain with the same name is returned.

        @returns the specified domain.  
        @param what If this is a string or WRFDomain, then the domain
        with that name is returned.  If "what" is an integer, the
        domain with that ID is returned.  If it is a WRFDomainBase,
        then the name stored in that object is used instead."""
        if isinstance(what,WRFDomainBase): return self._grid[str(what.name)]
        if isinstance(what,basestring): return self._grid[str(what)]
        if isinstance(what,int): return self._grid[self._to_name[what]]
        raise KeyError('In WRF.get, the key must be a basestring, '
                       'WRFDomain or an int (or subclass thereof).  You '
                       'provided %s.'%(repr(what),))
    def maxdom(self):
        """!returns the highest domain number, which is also the number
        of domains."""
        return self._nextid-1
    def can_add(self):
        """!Can we still add domains to this simulation?

        Returns true if this WRF can accept new domains.  Certain
        operations, such as requesting output files, end the ability
        to add domains."""
        return not self._domans_done
    def add(self,child,parent=None):
        """!Add a WRFDomain to this simulation.

        Adds the child WRFDomain to this WRF, with the specified
        parent.  If the parent is not specified, the last added domain
        is used as the parent.  If specified, the parent may be
        anything accepted by self.get.  The return value is the new
        WRFDomain.

        @param child the domain to add

        @param parent the WRFDomain of the parent

        @return a new WRFDomain object with the same name as child   """

        if self._domains_done:
            raise DomainsDone(
                'You cannot add any domains after setting up I/O for a '
                'WRF object.')

        # Get the settings for the new domain, plus some related inputs:
        newid=self._nextid
        newname=str(child.name)
        newparent=self.get_last() if parent is None else self.get(parent)
        parentname=str(newparent.name)
        moad=self.get_moad()
        if newname in self._grid:
            raise DomainExists('%s: domain already exists'%(newname,),newname)

        # Initialize the new domain:
        mine=child.copy()
        mine.nl.trait_set('id',newid)
        mine.nocolons=self.get_nocolons()
        mine.remove_forbidden()
        mine.init_as_nest(newparent,newid,self._simstart,self._simend)

        # Add the new domain.  These assignments must not fail or the
        # object will be corrupted, but it should not be possible to
        # get a failure here:
        self._grid[newname]=mine
        self._to_id[newname]=newid
        self._to_name[newid]=newname
        self._parent[newname]=parentname
        self._nextid=newid+1

        self.nl.nl_set('domains','max_dom',int(self.maxdom()))

        return self
    # Special methods to allow wrf[domain], len(wrf), domain in wrf,
    # and for domain in wrf
    def __getitem__(self,what): 
        """!Same as self.get(what)

        Calls self.get(what)
        @param what the grid_id, domain name or WRFDomain to search for.
        @return the WRFDomain"""
        return self.get(what)
    def __len__(self): 
        """!Same as self.maxdom()."""
        return self.maxdom()
    def __contains__(self,what):
        """!does this simulation contain this domain?

        Returns True if self.get(what) succeeds, and False if it
        raises KeyError.  Any other exceptions are passed to the
        caller.
        @param what the grid_id or name of the WRFDomain to search
        for, or another WRFDomain whose name we should search for."""
        try:
            self.get(what)
            return True
        except KeyError: pass
        return False
    def __iter__(self):
        """!Iterates over all WRFDomain objects in this WRFDomains."""
        for grid_id in xrange(1,self._nextid):
            yield self._grid[self._to_name[grid_id]]
