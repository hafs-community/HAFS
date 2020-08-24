"""!This module provides two different ways to generate Fortran
namelist files from HAFSConfig sections:

    NamelistInserter - given a file, or a multi-line string, finds
      angle bracket (<vartype:stuff>) strings with values from an
      HAFSConfig section.  This is the suggested method for namelists
      with very few configurable items.

    Conf2Namelist - given one or more Config sections, find values of
      the form nlsec.nlkey=value.  The nlsec specifies the namelist
      &nlsec, the nlkey specifies the namelist variable name, and the
      value is converted to a Python value and then a Fortran namelist
      value.  The Conf2Namelist provides ways to append multiple
      Conf2Namelist objects' data to form Fortran one-dimensional
      arrays.  This is to support WRF namelists, where one specifies
      each domain's data in a 1D array.

In addition, this module provides two functions to_fortnml and
from_fortnml to convert between in-memory Python objects and strings
suitable for pasting in a Fortran namelist to achieve the same value
in Fortran."""

import collections,re,fractions,datetime,configparser,io,logging
import tcutil.numerics

from configparser import NoOptionError,NoSectionError
from hafs.exceptions import *
from tcutil.numerics import to_datetime, to_datetime_rel, to_fraction

##@var __all__
# Symbols exported by "from hafs.namelist import *"
__all__=['to_fortnml','from_fortnml','Conf2Namelist','NamelistInserter']

##@var emptydict
# used for efficiency in a few places that need an empty dict()
emptydict=dict()

class NamelistRecursion(Exception):
    """!used to indicate namelist recursion

    This exception is used only for internal purposes and will never
    be raised beyond this module."""

def to_fortnml(py,exc_hint=''):
    """!converts a Python object to fortran namelist format

    Converts the given python data structure to a string suitable for
    representing in a Fortran namelist.  It can handle bool, int,
    float, string, datetime, fraction and a list (or tuple) of those.
    A datetime is converted via strftime('"%Y-%m-%d_%H:%M:%S"').  The
    optional exc_hint is used when raising exceptions to give an idea
    of what file and line number, or config option name, generated the
    exception.

    Conversions:
    * True ==> "T"
    * False ==> "F"
    * int ==> str(py)
    * float ==> str(py)
    * fractions.Fraction ==> str(float(py))
    * basestring ==> double-quoted string
    * datetime.datetime ==> WPS/WRF format date stamp: %Y-%m-%D_%H:%M:%S
    * list or tuple ==> comma-separated list of converted values

    Anything else will raise an exception.

    @return         the converted value as a string
    @param py       the object to convert
    @param exc_hint prepended to exception messages when exceptions are
                    raised.  Should contain file and line information."""
    try:
        return __to_fortnml_impl(py,exc_hint=exc_hint)
    except NamelistRecursion:
        s=repr(py)
        s=s[0:37]+'...' if len(s)>40 else s
        raise NamelistValueError('%s: cannot convert nested python containers to Fortran namelist values.'%(exc_hint+s,))

def __to_fortnml_impl(py,recursed=False,exc_hint=''):
    """!internal function used to convert python objects to namelist syntax

    This function does the actual work of converting a Python object to
    Fortran namelist syntax.  Do not call it directly.
    @param py the object to convert
    @param recursed have we already recursed? Used to detect nested lists
    @param exc_hint the exc_hint argument to to_fortnml()"""
    if isinstance(py,bool):                  return 'T' if py else 'F'
    if isinstance(py,int):                   return str(py)
    if isinstance(py,float):                 return str(py)
    if isinstance(py,fractions.Fraction):    return str(float(py))
    if isinstance(py,str):                   return '"'+re.sub('"','""',str(py))+'"'
    if isinstance(py,datetime.datetime):     return py.strftime('"%Y-%m-%d_%H:%M:%S"') # WPS format
    if isinstance(py,list) or isinstance(py,tuple):
        if recursed:
            raise NamelistRecursion()
        else:
            return ', '.join([__to_fortnml_impl(x,recursed=True,
                exc_hint=exc_hint) for x in py])
    raise NamelistValueError('%s%s Unable to convert to a fortran namelist '
                             'value'%(exc_hint,repr(py)))

##@var fortnml_parse
# a regular expression for tokenizing a fortran namelist scalar value
fortnml_parse=re.compile("""(?ix)\s*(?:
    (?P<fraction>[0-9]+[+][0-9]+[/][0-9]+) |
    \"(?P<dqchar>(?:[^\"]+|\"\")+)\" |
    \'(?P<sqchar>(?:[^\']+|\'\')+)\' |
    (?P<real>
      (?:(?:[+-]?[0-9]+\.[0-9]*|[+-]?\.[0-9]+)(?:[eE][+-]?[0-9]+)?) |
      (?:[+-]?[0-9]+[eE][+-]?[0-9]+)
    ) |
    (?P<int>[+-]?[0-9]+) |
    (?P<identifier>[a-zA-Z_][a-zA-Z0-9_]+|[a-eg-su-zA-EG-SU-Z_]) |
    (?P<true>t|\.true\.) |
    (?P<false>f|\.false\.) |
    (?P<comment>[!#].*$) |
    (?P<comma>\s*,\s*) |
    (?P<bad>.)
)""")
"""A regular expression for tokenizing a fortran namelist scalar value."""

def from_fortnml(py):
    """!Converts a fortran namelist value to a Python object

    This is the inverse function for to_fortnml.  Given a string from
    a Fortran namelist value, returns an equivalent python object.
    Will throw NamelistValueError if an unrecognized object is
    present, or NamelistRecursion if you send a nested container (such
    as a list of lists).
    @param py the string to convert
    @return the Python object"""
    out=[]
    islist=False
    for match in fortnml_parse.finditer(py):
        tok=match.lastgroup
        if tok is None or tok=='bad':
            raise NamelistValueError('%s: cannot parse namelist value'%(py,))
        elif tok=='comment':    break
        elif tok=='int':        out.append(int(match.group(tok)))
        elif tok=='real':       out.append(float(match.group(tok)))
        elif tok=='true':       out.append(True)
        elif tok=='false':      out.append(False)
        elif tok=='fraction':   out.append(to_fraction(match.group(tok)))
        elif tok=='dqchar':     out.append(re.sub('""','"',match.group(tok)))
        elif tok=='sqchar':     out.append(re.sub("''","'",match.group(tok)))
        elif tok=='comma':      islist=True
        elif tok=='identifier': out.append(match.group(tok))
    if len(out)==1:
        return out[0]
    elif len(out)==0:
        raise NamelistValueError('%s: does not specify any data',(py,))
    else:
        return out

class NamelistInserter(object):
    """!Insert config file data into a Fortran namelist file.

    This class parses an input file that contains a partial namelist,
    inserting missing values of the format <s:cycle> with values from
    a ConfigParser-like object.  The <s:cycle> sorts of text follow a
    specific format:

    * @<s:varname@> -- insert a string surrounded by double quotes taken
      from the specified conf variable
    * @<f:varname@> -- insert a float, taken from conf variable varname
    * @<r:varname@> -- same as <f:varname>
    * @<i:varname@> -- insert an integer, taken from conf variable varname
    * @<l:varname@> -- insert a logical, taken from conf variable varname
    * @<b:varname@> -- same as <l:varname>

    @<d:varname@> -- the conf variable is converted to a
      datetime.datetime using tcutil.numerics.to_datetime, and then to a
      string of the format "YYYY-MM-DD_HH:MM:SS".  If atime is
      specified to the parse subroutine, then the value is allowed to
      be a difference relative to the atime (as accepted by
      tcutil.numerics.to_datetime_rel).

    @<u:varname@> -- convert the conf variable to a string, and dump its
      value unquoted.  This is used, for example, to have the name of a
      namelist variable be generated from a conf file.

    You can also specify angle braces without a type:

    @<varname@>

    to ask for the variable to be converted by guessing the type that
    was intended in the conf file.  For example:

    This conf file:
    @code
      [conf]
      var=T,F,T
    @endcode

    With this namelist:

    @code
      &nl
        myvar=@<var@> /
    @endcode

    will produce:

    @code
      &nl
        myvar=.true., .false., .true. /
    @endcode

    As for variables, one can request a subitem of a variable:

    * varname -- get variable varname
    * vit[stormname] -- get variable "vit" and then get
                        vit.__getitem__["stormname"]

    for subscriptable types.  This is mainly intended for vitals."""

    ## @var find_ltgt
    # regular expression that finds namelist insertion data @<...@>
    find_ltgt=re.compile('''(?P<pre>(?:[^<]|"(?:[^"]|""")*"|'(?:[^']|'{3})*')*)<(?:(?P<typ>[^:]*):)?(?P<var>[^>\[]*)(?:\[(?P<sub>[^\]]*)\])?>(?P<rest>.*)''')

    ## @var nlfalse
    #  regular expression that matches false values
    nlfalse=re.compile('\A(?i)(?:f.*|.false.|n|no|0*[1-9][0-9]*)\Z')

    ## @var nltrue
    #  regular expression that matches true values
    nltrue=re.compile('\A(?i)(?:t.*|.true.|y|yes|0)\Z')

    ## @var comment
    #  regular expression that matches comments
    comment=re.compile('(?P<code>(?:[^!]|\"(?:[^\"]|\"\"\")*\"|\'(?:[^\']|\'\'\')*\')*)(?P<comment>!.*$)?')

    def __init__(self,conf,section):
        """!NamelistInserter constructor

        Creates a new NamelistInserter that will get its data from the
        specified section of the HAFSConfig conf.
        @param conf the hafs.config.HAFSConfig object to use
        @param section the section to read"""
        self._conf=conf
        self._section=section

    ##@var _conf
    #  the hafs.config.HAFSConfig object sent to  __init__(conf,section)

    ##@var _section
    #  the section to read, sent to __init__(conf,section)

    def parse(self,line_iterable,logger=None,source='<internal>',
              raise_all=True,atime=None,ftime=None,**kwargs):
        """!Generates the namelist, returning it as a string.

        Reads the config section and file, generating the namelist and
        returning it as a string.

        @param line_iterable  an iterator that iterates over each line of
            the file.
        @param logger optional: a logging.Logger to log messages, or None
            to disable.  Default: None
        @param source  for warning or error messages: the filename.
                       Default: "@<internal@>"
        @param raise_all set to False to log messages instead of
            raising exceptions.  Raise only one exception at the end.
            Default: True; raise an exception as soon as the first
            error is seen.
        @param atime Optional: the analysis time for conf.timestrinterp
        @param ftime Optional: the forecast time for conf.timestrinterp
        @param kwargs   additional variables and their values for config
                        file accesses."""
        assert(not isinstance(line_iterable,str))
        if atime is not None:
            atime=to_datetime(atime)
        if ftime is not None:
            ftime=to_datetime_rel(ftime,atime)
        elif atime is not None:
            ftime=atime
        out=io.StringIO()
        conf=self._conf
        section=self._section
        iline=0
        def synerr(what):
            if logger is not None:
                logger.warning('%s:%d: syntax error: %s'%(source,iline,what))
        for line in line_iterable:
            line=line.rstrip()
            iline+=1
            linepart=line
            m=NamelistInserter.comment.match(linepart)
            comment=''
            if m:
                code=m.group('code')
                comment=m.group('comment')
                if not code:
                    out.write(line+'\n')
                    continue
                linepart=code
            while len(linepart)>0:
                m=NamelistInserter.find_ltgt.match(linepart)
                if m:
                    pre=m.group('pre')
                    typ=m.group('typ')
                    var=m.group('var')
                    sub=m.group('sub')
                    rest=m.group('rest')
                    if rest:
                        assert(linepart!=rest)
                        linepart=rest
                    else:
                        linepart=''
                    if pre: out.write(pre)
                    if typ is None: typ='*'
                    if not typ:    synerr('no output type specified')
                    elif not var:  synerr('no variable specified')
                    elif len(typ)!=1:
                        synerr('output type must be one of: bdfilsu, not %s'
                               %(typ,))
                    elif typ not in 'bdfilrsuBDFILRSU*':
                        synerr('invalid type %s specified: only bdfilsu are '
                               'allowed'%(typ,))
                    else:
                        try:
                            if var in kwargs:
                                val=kwargs[var]
                            elif atime is not None:
                                val=conf.timestrinterp(
                                    section,'{'+var+'}',ftime=ftime,
                                    atime=atime,**kwargs)
                            else:
                                val=conf.strinterp(
                                    section,'{'+var+'}',**kwargs)
                        except(KeyError,TypeError,ValueError,NoOptionError,
                               NoSectionError) as e:
                            if logger is not None:
                                logger.warning(
                                    '%s:%d: cannot find variable %s'
                                    %(source,iline,var))
                            if raise_all: raise
                            continue
                        if sub:
                            try:
                                newval=val[sub]
                                val=newval
                            except (TypeError,KeyError,ValueError,HAFSError) as e:
                                if logger is not None:
                                    logger.warning('%s:%d: %s[%s]: %s'%(
                                            source,iline,var,sub,str(e)),
                                                   exc_info=True)
                                if raise_all: raise
                                continue
                        try:
                            if typ in 'rRfF':   typval=float(val)
                            elif typ in 'iI': typval=int(val)
                            elif typ in 'bBlL':
                                if isinstance(val,bool):
                                    typval=val
                                elif isinstance(val,str):
                                    if NamelistInserter.nlfalse.match(val):
                                        typval=False
                                    elif NamelistInserter.nltrue.match(val):
                                        typval=True
                                    else:
                                        raise ValueError(
                                            '%s is not a valid logical'
                                            %(repr(val),))
                                else:
                                    typval=bool(val)
                            elif typ in 'dD':
                                dval=from_fortnml(val)
                                if atime is not None:
                                    typval=to_datetime_rel(dval,atime)
                                else:
                                    typval=to_datetime(dval)
                            elif typ=='*':
                                typval=from_fortnml(val)
                            else: # types u and s
                                typval=str(val)
                        except (TypeError,KeyError,ValueError) as e:
                            if logger is not None:
                                logger.warning(
                                    '%s:%d: cannot convert %s to type=%s: %s'
                                    %(source,iline,repr(val),typ,str(e)),
                                    exc_info=True)
                            if raise_all: raise
                            continue
                        if sub: fromthis='%s[%s]'%(var,sub)
                        else: fromthis=var
                        try:
                            if typ in 'bdfilrsBDFILRS*':
                                writeme=to_fortnml(
                                    typval,exc_hint=fromthis+':')
                            else: # type u
                                writeme=str(typval)
                        except (TypeError,KeyError,ValueError) as e:
                            if logger is not None:
                                logger.warning(
                                    '%s:%d: <%s:%s>=%s: error converting to '
                                    'string: %s'
                                    %(source,iline,typ,fromthis,repr(typval),
                                      str(e)),exc_info=True)
                            if raise_all: raise
                            continue
                        out.write(writeme)
                else:
                    # No more <typ:val> on this line.  Write the rest.
                    out.write(linepart)
                    linepart=''
            if comment:
                out.write(comment.rstrip()+'\n')
            else:
                out.write('\n')
        return out.getvalue()

class Conf2Namelist(object):
    """!Generates a Fortran namelist entirely from config files

    This class generates namelist information from any
    ConfigParser-like object, starting at a specified conf section.
    This differs from NamelistInserter in that no external file is
    read - only the ConfigParser is used.  Also, an
    hafs.config.HAFSConfig is not required; any ConfigParser-like
    object is sufficient.

    When parsing the section, variables of the form nlsection.nlkey
    will be put in namelist section nlsection, with variable name
    nlkey.  Variables that contain no "." but are valid Fortran
    variable names (except for the variable "namelist") are called
    "traits" and are accessible via trait_get, trait_set, and the
    other trait_* subroutines.  The special variable "namelist"
    specifies a list of conf sections to recurse into.  Variables in
    the local conf section will always override variables in included
    conf sections, and variables in later included conf sections will
    override variables in earlier included conf sections.

    For example:
    @code
        conf=RawConfigParser()
        conf.readfp(StringIO('''
          [sec1]
             happiness_quotient=0.7
             physics.mp_physics=85
             physics.cu_physics=84
             namelist=sec2,sec3
          [sec2]
             physics.cu_physics=4
             physics.bl_pbl_physics=93
          [sec3]
             physics.bl_pbl_physics=3
       ''')
       str(Conf2Namelist(conf,'sec1'))
    @endcode
    Will result in:
    @code{.unparsed}
         &physics
           bl_pbl_physics=3
           cu_physics=84
           mp_physics=85
         /
    @endcode
    and a trait accessible via self.trait_get('happiness_quotient')
    """

    ##@var testnl
    # a configuration string for testing
    testnl='''[sec1]
happiness_quotient=0.7
physics.mp_physics=85
physics.cu_physics=84
namelist=sec2,sec3
[sec2]
physics.cu_physics=4
physics.bl_pbl_physics=93
domains.something=32
[sec3]
physics.bl_pbl_physics=3'''
    """A test namelist for debugging"""

    ##@var nlentry
    # A regular expression from re.compile.  This is used to scan
    # config section option names to detect if they are namelist
    # variable names (format: section.variable).
    nlentry=re.compile('\A(?:(?P<section>[a-zA-Z_][a-zA-Z0-9_]*)\.)?(?P<var>[a-zA-Z_][a-zA-Z0-9_%]*)\Z')

    ##@var nlfalse
    # detects false Fortran logical constants
    nlfalse=re.compile('\A(?i)(?:f.*|.false.)\Z')
    """A regular expression from re.compile, to detect false Fortran logical values."""

    ##@var nltrue
    # detects true Fortran logical constants
    nltrue=re.compile('\A(?i)(?:t.*|.true.)\Z')
    """A regular expression from re.compile, to detect true Fortran logical values."""

    ##@var TRAIT
    # special section name for traits
    TRAIT='-trait-'
    """A special, fake, invalid namelist name for traits."""

    def copy(self,other):
        """!Returns a copy of self.

        Creates a shallow copy of self
        @param other unused; may be removed in the future"""
        return Conf2Namelist(section_sorter=self.section_sorter,
                             var_sorters=self.var_sorters,
                             logger=self.logger,
                             nl=self.nl)

    ##@var section_sorter
    # The cmp function used to sort sections

    ##@var var_sorters
    # A dict mapping from section name to a cmp function used
    # to sort namelist variables in that section

    ##@var nl
    # A dict of dicts used to store namelist information

    def __init__(self,conf=None,section=None,section_sorter=None,
                 var_sorters=None,logger=None,nl=None,morevars=None):
        """!Conf2Namelist constructor

        Creates a Conf2Namelist.

        @param conf the HAFSConfig object
        @param section the section to start searching from.
        @param section_sorter the cmp-like function to use to sort the
            sections when generating the output namelist
        @param var_sorters a dict-like mapping from section name to a
            cmp-like function to use to sort variable names within
            each section.
        @param logger a logging.Logger object to use to log messages
        @param nl a dict of dicts (or object that acts like that)
            to use to initialize the Conf2Namelist.  Warning: this
            functionality is untested
        @param morevars a dict with additional variables to use when
            expanding strings This is simply passed to conf.items.
            See the HAFSConfig documentation for details."""
        if morevars is None: morevars=emptydict
        self.section_sorter=None
        self.var_sorters=var_sorters
        if self.section_sorter is None:
            self.section_sorter=cmp
        self.nl=collections.defaultdict(lambda: collections.defaultdict())
        if nl is not None:
            for (sec,con) in nl.items():
                for key,val in con.items():
                    nl[sec][key]=val
        if conf is None or section is None:
            return
        initial=str(section)
        touched=set()
        parseme=[initial]
        nlentry=Conf2Namelist.nlentry
        while len(parseme)>0:
            sec=parseme.pop()
            if sec in touched:
                continue
            touched.add(sec)
            if logger is not None:
                logger.debug('Conf2Namelist now parsing section %s'%(sec,))
            for key,value in conf.items(sec,morevars=morevars):
                m=nlentry.match(key)
                if m and m.group('section') is not None:
                    self.nl_set_if_unset(m.group('section'),m.group('var'),
                                         from_fortnml(value))
                elif key=='namelist':
                    # namelist=list,of,conf,sections
                    # Add each one to the list of sections to parse
                    for sec2 in value.split(','):
                        trim=sec2.strip()
                        if len(trim)>0 and not trim in touched:
                            parseme.append(trim)
                elif re.match('\A[a-zA-Z_][a-zA-Z_0-9%]*\Z',key):
                    self.trait_set_if_unset(m.group('var'),
                                            from_fortnml(value))
                elif logger is not None:
                    logger.debug(
                        'Conf2Namelist ignoring %s = %s in section %s'
                        %(key,value,sec))
    def nl_section(self,*args):
        """!create namelists if they do not exist

        Ensures that the specified namelist exists.  Returns self.
        @param args list of namelist names"""
        for section in args:
            self.nl[str(section).lower()]
        return self
    def nl_set(self,section,var,data):
        """!Sets a variable in a namelist

        Sets the value of a namelist's variable.
        @param section the namelist name
        @param var the name of the variable in the namelist
        @param data the value.  This can be a string,
          datetime.datetime, int, float, fractions.Fraction or a list or
          tuple of such"""
        if not ( isinstance(data,str) or
                 isinstance(data,datetime.datetime) or
                 isinstance(data,int) or isinstance(data,float) or
                 isinstance(data,list) or
                 isinstance(data,fractions.Fraction) ):
            raise TypeError('%s: invalid type for namelist (value=%s)'
                            %(data.__class__.__name__,repr(data)))
        self.nl[str(section).lower()][str(var).lower()]=data
    def nl_del(self,section,var):
        """!Removes a variable from a namelist.

        Removes a variable from a namelist
        @param section the namelist
        @param var the variable to delete"""
        try:
            del self.nl[str(section).lower()][var]
        except KeyError: pass
    # Multistorm - jtf
    def nl_del_sect(self,section):
        """Removes a namelist section from the namelist"""
        try:
            del self.nl[str(section).lower()]
        except KeyError: pass
    def nl_have(self,section,var):
        """!does this namelist have this variable?

        Determines if the namelist exists and has the variable.

        @return True if the namelist exists and it has the variable, or
          False otherwise
        @param section the string name of the namelist
        @param var     the string name of the variable"""
        if section not in self.nl: return False
        return var in self.nl[section]
    # Multistorm - jtf
    def nl_have_sect(self,section):
        """Returns True if the namelist section exists in the namelist
        and False otherwise"""
        if section in self.nl: return True
        return False
    def nl_get(self,section,var,default=None):
        """!get the value of a variable from a namelist

        Gets the value of a variable in a namelist.  If the
        default is supplied and non-None, it will be returned if the
        variable does not exist.  Raises NamelistKeyError if the
        variable does not exist a the default is not provided.
        @param section the namelist name
        @param var the name of the variable in the namelist
        @param default the value to return if the namelist or variable
          do not exist """
        s=str(section).lower()
        v=str(var).lower()
        try:
            return self.nl[s][v]
        except KeyError:
            if default is not None:
                return default
            raise NamelistKeyError('no value given',s,v)
    def nl_set_if_unset(self,section,var,data):
        """!Sets the value of a namelist variable if it has no value.

        If the namelist variable has a value, this function does
        nothing.  Otherwise, it is the same as calling nl_set.
        @param section the namelist name
        @param var the variable name
        @param data the value to set, passed to nl_set()"""
        try:
            self.nl_get(section,var)
        except KeyError:
            self.nl_set(section,var,data)
    def nl_each(self,section):
        """!Iterates over variable,value tuples in the given
        namelist.
        @param section the namelist over which to iterate"""
        if not isinstance(section,str): section=str(section)
        if not section in self.nl: return
        for var,value in self.nl[section].items():
            yield var,value
    def trait_each(self):
        """!Iterates over variable,value tuples in the traits."""
        assert(Conf2Namelist.TRAIT in self.nl)
        assert(self.nl[Conf2Namelist.TRAIT])
        for var,value in self.nl[Conf2Namelist.TRAIT].items():
            yield var,value
    def trait_set(self,var,value):
        """!Sets a trait's value.
        This is the same as nl_set() but sets a trait.  It simply
        passes the special constant Conf2Namelist.TRAIT as the
        namelist
        @param var the name of the trait to set
        @param value the value to set"""
        return self.nl_set(Conf2Namelist.TRAIT,var,value)
    def trait_del(self,var):
        """!Deletes a trait.

        Deletes a trait.  This is the same as calling nl_del passing
        the Conf2Namelist.TRAIT constant as the section.
        @param var the variable to delete."""
        return self.nl_del(Conf2Namelist.TRAIT,var)
    def trait_get(self,var,default=None):
        """!Returns a trait's value.

        Returns the value of a trait.  If a default is given and
        non-None, returns the default if the trait does not exist.
        This is the same as calling nl_get() passing the
        Conf2Namelist.TRAIT as the section.
        @param var the trait to get
        @param default the default value if the trait is unset"""
        return self.nl_get(Conf2Namelist.TRAIT,var,default)
    def trait_have(self,var):
        """!Returns True if the trait exists, and False otherwise.

        Determines if a trait is set.  This is the same as passing
        Conf2Namelist.TRAIT as the section argument to nl_have()
        @param var the trait to query"""
        return self.nl_have(Conf2Namelist.TRAIT,var)
    def trait_set_if_unset(self,var,value):
        """!Sets the traits value if it does not have one.

        Sets the value of a trait if the trait does not already have a
        value.  This is the same as calling nl_set_if_unset() passing
        the special value Conf2Namelist.TRAIT as the section."""
        return self.nl_set_if_unset(Conf2Namelist.TRAIT,var,value)
    def join(self,others):
        """!create array values by joining multiple Conf2Namelist objects

        Generates a new Conf2Namelist by looping over all arguments in
        this namelist, and appending the other namelists' values in a
        list.  If the other namelist does not have a given value, then
        this namelist's value, or the last namelist that had a value
        for that argument, will be appended.  Variables or namelist
        sections that exist in the other namelists, but not this one,
        will be ignored.

        @param others an iterable object (list, tuple) of Conf2Namelist"""
        if len(others)==0:
            return self.copy()
        out=Conf2Namelist(None,None)
        for secname,mydict in self.nl.items():
            outsec=out.nl[secname]
            for var,value in mydict.items():
                thelist=[value]
                default=value
                for other in others:
                    nextval=other.nl_get(secname,var,default)
                    default=nextval
                    if isinstance(nextval,tuple) or isinstance(nextval,list):
                        thelist.extend(nextval)
                    else:
                        thelist.append(nextval)
                outsec[var]=thelist
        return out
    def copy(self,section_subset=None,var_subset=None,other=None):
        """!duplicates this object

        Returns a copy of this object, or if other is specified,
        copies this object's contents into the target Conf2Namelist.
        When copying into a target Conf2Namelist, only values that are
        not already in that namelist will be copied.  The copy has its
        own data structures, so modifying the copy will not modify the
        original.  Optionally, you can copy only a subset of this
        object:

        @param section_subset = a callable object that returns True
          for each section to be kept
        @param var_subset = a callable object that returns
          True for each variable to be kept.

        @param other if specified, this must be a Conf2Namelist.
        Instead of creating a new Conf2Namelist, data will be inserted
        into this one."""
        if other is None:
            other=Conf2Namelist(None,None)
        for s,sd in self.nl.items():
            if section_subset is None or section_subset(s):
                outdict=other.nl[s]
                for var,value in sd.items():
                    if var_subset is None or var_subset(s,var):
                        try:
                            junk=outdict[var]
                        except KeyError:
                            outdict[var]=value
        return other
    def remove_traits(self):
        """!Removes all traits.
        Deletes the special Conf2Namelist.TRAIT namelist
        @return self"""
        if Conf2Namelist.TRAIT in self.nl:
            del(self.nl[Conf2Namelist.TRAIT])
        return self
    def __str__(self):
        """!synonym for make_namelist()

        Generates a Fortran namelist as a string.  Equivalent to
        make_namelist()."""
        return self.make_namelist()
    def make_namelist(self,section_sorter=None,var_sorters=None,
                      morevars=None):
        """!generates the namelist as a string

        Returns the stringified namelist for this object, suitable for
        writing to a file for Fortran to read.  Optionally, you can
        provide a sorting for the namelist entries:

        @param section_sorter = a cmp-like function for sorting sections by
          name If absent, cmp is used.
        @param var_sorters = a dict-like mapping of section names to cmp-like
          objects for sorting variables within each section.  If
          absent, self.namelist_sorter(sectionname) is used
        @param morevars a dict of additional variables which override
          values set in self.nl"""
        out=''
        if section_sorter is None:
            section_sorter=self.section_sorter

        sd=None
        def getvar(var):
            if morevars is not None and var in morevars:
                return morevars[var]
            else:
                return sd[var]

        for sec in sorted(iter(self.nl.keys()),section_sorter):
            sd=self.nl[sec]
            if sec==Conf2Namelist.TRAIT:
                out+='! Traits:\n'
            else:
                out+='&%s\n'%(sec,)
            if sd:
                sorter=None
                if var_sorters is not None and sec in var_sorters:
                    sorter=var_sorters[sec]
                if sorter is None:
                    sorter=self.namelist_sorter(sec)
                if sec==Conf2Namelist.TRAIT:
                    out+="\n".join('!  %s = %s,'%(var,to_fortnml(getvar(var),
                        exc_hint='%s%%%s='%(str(sec),str(var)))) \
                            for var in sorted(list(sd.keys()),sorter))
                else:
                    out+="\n".join('  %s = %s,'%(var,to_fortnml(getvar(var),
                        exc_hint='%s%%%s='%(str(sec),str(var)))) \
                            for var in sorted(list(sd.keys()),sorter))
            if sec==Conf2Namelist.TRAIT:
                out+='\n\n'
            else:
                out+="\n/\n\n"
        return out
    def namelist_sorter(self,section):
        """!return a sorting function for the variables in a namelist

        Returns a cmp function that orders namelist entries for the
        specified namelist section.  See the argument "cmp" of the
        python built-in function sorted() for details.

        @param section the namelist or Conf2Namelist.TRAIT
        @return a cmp-like function sorter(a,b)  """
        if self.var_sorters is not None:
            if section in self.var_sorters:
                return self.var_sorters[section]
        return lambda x,y: cmp(x,y)
    def set_sorters(self,section_sorter,var_sorters):
        """!sets the sorting algorithms for namelists and  variables

        Sets the cmp-like functions for sorting sections and
        variables in each section.  The section_sorter sorts sections.
        The var_sorters is a dict-like mapping from section name to a
        cmp-like function for sorting that section.  If any sorter is
        unspecified, cmp will be used.  See the "cmp" argument of the
        python built-in function sorted() for details.

        @param section_sorter a cmp-like function for sorting
        namelist by namelist name.  If section_sorters is None,
        cmp is used.
        @param var_sorters a dict-like mapping from namelist name to a
        variable sorter function.  Each variable sorter function must
        be a cmp-like function that compares variable names.  If
        var_sorters is None, then cmp() will be used for all namelists.
        @return self"""
        self.section_sorter=cmp if section_sorter is None else section_sorter
        self.var_sorters=var_sorters
        return self
