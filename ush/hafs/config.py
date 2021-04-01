"""!parses UNIX conf files and makes the result readily available

The hafs.config module reads configuration information for the HAFS
system from one or more *.conf files, via the Python ConfigParser
module.  This module also automatically fills in certain information,
such as fields calculated from the tcvitals or date.  The result is
accessible via the HAFSConfig class, which provides many ways of
automatically accessing configuration options."""

##@var __all__
# decides what symbols are imported by "from hafs.config import *"
__all__=['from_file','from-string','confwalker','HAFSConfig','fordriver','ENVIRONMENT']

import collections,re,string,os,logging,threading
import os.path,sys
import datetime
import produtil.fileop, produtil.datastore
import tcutil.numerics, tcutil.storminfo, tcutil.revital
import hafs.exceptions

import configparser
from configparser import ConfigParser
from io import StringIO

from produtil.datastore import Datastore
from produtil.fileop import *

from tcutil.numerics import to_datetime
from string import Formatter
from configparser import NoOptionError,NoSectionError

from tcutil.numerics import to_datetime
from tcutil.storminfo import find_tcvitals_for
from hafs.exceptions import HAFSError

UNSPECIFIED=object()

########################################################################

class Environment(object):
    """!returns environment variables, allowing substitutions

    This class is used to read (but not write) environment variables
    and provide default values if an environment variable is unset or
    blank.  It is only meant to be used in string formats, by passing
    ENV=ENVIRONMENT.  There is a global constant in this module,
    ENVIRONMENT, which is an instance of this class.  You should never
    need to instantiate another one."""
    def __contains__(self,s):
        """!Determines if __getitem__ will return something (True) or
        raise KeyError (False).  Same as "s in os.environ" unless s
        contains "|-", in which case, the result is True."""
        return s.find('|-')>=0 or s in os.environ
    def __getitem__(self,s):
        """!Same as os.environ[s] unless s contains "|-".
           ENVIRONMENT["VARNAME|-substitute"]
        will return os.environ[VARNAME] if VARNAME is defined and
        non-empty in os.environ.  Otherwise, it will return
        "substitute"."""
        if not s: return ''
        i=s.find('|-')
        if i<0: return os.environ[s]
        var=s[0:i]
        sub=s[(i+2):]
        val=os.environ.get(var,'')
        if val!='': return val
        return sub

## @var ENVIRONMENT
#  an Environment object.  You should never need to instantiate another one.
ENVIRONMENT=Environment()

class ConfFormatter(Formatter):
    """!Internal class that implements HAFSConfig.strinterp()

    This class is part of the implementation of HAFSConfig: it is
    used to interpolate strings using a syntax similar to
    string.format(), but it allows recursion in the config sections,
    and it also is able to use the [config] and [dir] sections as
    defaults for variables not found in the current section."""
    def __init__(self,quoted_literals=False):
        """!Constructor for ConfFormatter"""
        super(ConfFormatter,self).__init__()
        if quoted_literals:
            self.format=self.slow_format
            self.vformat=self.slow_vformat
            self.parse=qparse

    @property
    def quoted_literals(self):
        return self.parse==qparse

    def slow_format(self,format_string,*args,**kwargs):
        return self.vformat(format_string,args,kwargs)
    def slow_vformat(self,format_string,args,kwargs):
        out=StringIO()
        for literal_text, field_name, format_spec, conversion in \
                self.parse(format_string):
            if literal_text:
                out.write(literal_text)
            if field_name:
                (obj, used_key) = self.get_field(field_name,args,kwargs)
                if obj is None and used_key:
                    obj=self.get_value(used_key,args,kwargs)
                value=obj
                if conversion=='s':
                    value=str(value)
                elif conversion=='r':
                    value=repr(value)
                elif conversion:
                    raise ValueError('Unknown conversion %s'%(repr(conversion),))
                if format_spec:
                    value=value.__format__(format_spec)
                out.write(value)
        ret=out.getvalue()
        out.close()
        assert(ret is not None)
        assert(isinstance(ret,str))
        return ret

    def get_value(self,key,args,kwargs):
        """!Return the value of variable, or a substitution.

        Never call this function.  It is called automatically by
        str.format.  It provides the value of an variable,
        or a string substitution.
        @param key the string key being analyzed by str.format()
        @param args the indexed arguments to str.format()
        @param kwargs the keyword arguments to str.format()"""
        kwargs['__depth']+=1
        if kwargs['__depth']>=configparser.MAX_INTERPOLATION_DEPTH:
            raise configparser.InterpolationDepthError(kwargs['__key'],
                kwargs['__section'],key)
        try:
            if isinstance(key,int):
                return args[key]
            conf=kwargs.get('__conf',None)
            if key in kwargs:
                v=kwargs[key]
            elif '__taskvars' in kwargs \
                    and kwargs['__taskvars'] \
                    and key in kwargs['__taskvars']:
                v=kwargs['__taskvars'][key]
            else:
                isec=key.find('/')
                if isec>=0:
                    section=key[0:isec]
                    nkey=key[(isec+1):]
                    if not section:
                        section=kwargs.get('__section',None)
                    if nkey:
                        key=nkey
                else:
                    section=kwargs.get('__section',None)
                conf=kwargs.get('__conf',None)
                v=NOTFOUND
                if section is not None and conf is not None:
                    if conf.has_option(section,key):
                        v=conf.get(section,key,raw=True)
                    elif conf.has_option(section,'@inc'):
                        for osec in conf.get(section,'@inc').split(','):
                            if conf.has_option(osec,key):
                                v=conf.get(osec,key,raw=True)
                    if v is NOTFOUND:
                        if conf.has_option('config',key):
                            v=conf.get('config',key,raw=True)
                        elif conf.has_option('dir',key):
                            v=conf.get('dir',key,raw=True)
                    if v is NOTFOUND:
                        raise KeyError(key)

            if isinstance(v,str):
                if v.find('{')>=0 or v.find('%')>=0:
                    vnew=self.vformat(v,args,kwargs)
                    assert(vnew is not None)
                    return vnew
            return v
        finally:
            kwargs['__depth']-=1

def qparse(format_string):
    """!Replacement for Formatter.parse which can be added to Formatter objects
    to turn {'...'} and {"..."} blocks into literal strings (the ... part).
    Apply this by doing f=Formatter() ; f.parse=qparse.  """
    if not format_string: return []
    if not isinstance(format_string, str):
        raise TypeError('iterparse expects a str, not a %s %s'%(
                type(format_string).__name__,repr(format_string)))
    result=list()
    literal_text=''
    field_name=None
    format_spec=None
    conversion=None
    for m in re.finditer(r'''(?xs) (
            \{ \' (?P<qescape>  (?: \' (?! \} ) | [^'] )* ) \' \}
          | \{ \" (?P<dqescape> (?: \" (?! \} ) | [^"] )* ) \" \}
          | (?P<replacement_field>
               \{
                  (?P<field_name>
                     [^\}:!\['"\{] [^\}:!\[]*
                     (?: \. [a-zA-Z_][a-zA-Z_0-9]+
                       | \[ [^\]]+ \] )*
                  )
                  (?: ! (?P<conversion>[rs]) )?
                  (?: :
                     (?P<format_spec>
                       (?: [^\{\}]+
                         | \{[^\}]*\} )*
                     )
                  )?
               \} )
          | (?P<left_set> \{\{ )
          | (?P<right_set> \}\} )
          | (?P<literal_text> [^\{\}]+ )
          | (?P<error> . ) ) ''',format_string):
        if m.group('qescape'):
            literal_text+=m.group('qescape')
        elif m.group('dqescape'):
            literal_text+=m.group('dqescape')
        elif m.group('left_set'):
            literal_text+='{'
        elif m.group('right_set'):
            literal_text+='}'
        elif m.group('literal_text'):
            literal_text+=m.group('literal_text')
        elif m.group('replacement_field'):
            result.append( ( literal_text,
                    m.group('field_name'),
                    m.group('format_spec'),
                    m.group('conversion') ) )
            literal_text=''
        elif m.group('error'):
            if m.group('error')=='{':
                raise ValueError("Single '{' encountered in format string")
            elif m.group('error')=='}':
                raise ValueError("Single '}' encountered in format string")
            else:
                raise ValueError("Unexpected %s in format string"%(
                        repr(m.group('error')),))
    if literal_text:
        result.append( ( literal_text, None, None, None ) )
    return result

########################################################################

##@var FCST_KEYS
#  the list of forecast time keys recognized by ConfTimeFormatter
FCST_KEYS={ 'fYMDHM':'%Y%m%d%H%M', 'fYMDH':'%Y%m%d%H', 'fYMD':'%Y%m%d',
            'fyear':'%Y', 'fYYYY':'%Y', 'fYY':'%y', 'fCC':'%C', 'fcen':'%C',
            'fmonth':'%m', 'fMM':'%m', 'fday':'%d', 'fDD':'%d', 'fhour':'%H',
            'fcyc':'%H', 'fHH':'%H', 'fminute':'%M', 'fmin':'%M' }
"""A list of keys recognized by ConfTimeFormatter if the key is
requested during string interpolation, and the key is not in the
relevant section.  This list of keys represents the forecast time.  It
is a dict mapping from the key name to the format sent to
datetime.datetime.strftime to generate the string value."""

##@var ANL_KEYS
#  the list of analysis time keys recognized by ConfTimeFormatter
ANL_KEYS={ 'aYMDHM':'%Y%m%d%H%M', 'aYMDH':'%Y%m%d%H', 'aYMD':'%Y%m%d',
           'ayear':'%Y', 'aYYYY':'%Y', 'aYY':'%y', 'aCC':'%C', 'acen':'%C',
           'amonth':'%m', 'aMM':'%m', 'aday':'%d', 'aDD':'%d', 'ahour':'%H',
           'acyc':'%H', 'aHH':'%H', 'aminute':'%M', 'amin':'%M' }
"""A list of keys recognized by ConfTimeFormatter if the key is
requested during string interpolation, and the key is not in the
relevant section.  This list of keys represents the analysis time.  It
is a dict mapping from the key name to the format sent to
datetime.datetime.strftime to generate the string value."""

##@var M6_KEYS
#  the list of analysis time ( -6h ) keys recognized by ConfTimeFormatter
ANL_M6_KEYS={ 'am6YMDHM':'%Y%m%d%H%M', 'am6YMDH':'%Y%m%d%H', 'am6YMD':'%Y%m%d',
           'am6year':'%Y', 'am6YYYY':'%Y', 'am6YY':'%y', 'am6CC':'%C', 'am6cen':'%C',
           'am6month':'%m', 'am6MM':'%m', 'am6day':'%d', 'am6DD':'%d', 'am6hour':'%H',
           'am6cyc':'%H', 'am6HH':'%H', 'am6minute':'%M', 'am6min':'%M' }
"""A list of keys recognized by ConfTimeFormatter if the key is
requested during string interpolation, and the key is not in the
relevant section.  This list of keys represents the analysis time.  It
is a dict mapping from the key name to the format sent to
datetime.datetime.strftime to generate the string value."""

##@var P6_KEYS
#  the list of analysis time ( +6h ) keys recognized by ConfTimeFormatter
ANL_P6_KEYS={ 'ap6YMDHM':'%Y%m%d%H%M', 'ap6YMDH':'%Y%m%d%H', 'ap6YMD':'%Y%m%d',
           'ap6year':'%Y', 'ap6YYYY':'%Y', 'ap6YY':'%y', 'ap6CC':'%C', 'ap6cen':'%C',
           'ap6month':'%m', 'ap6MM':'%m', 'ap6day':'%d', 'ap6DD':'%d', 'ap6hour':'%H',
           'ap6cyc':'%H', 'ap6HH':'%H', 'ap6minute':'%M', 'ap6min':'%M' }
"""A list of keys recognized by ConfTimeFormatter if the key is
requested during string interpolation, and the key is not in the
relevant section.  This list of keys represents the analysis time.  It
is a dict mapping from the key name to the format sent to
datetime.datetime.strftime to generate the string value."""

##@var TIME_DIFF_KEYS
# the list of "forecast time minus analysis time" keys recognized by
# ConfTimeFormatter
TIME_DIFF_KEYS=set(['fahr','famin','fahrmin'])
"""A list of keys recognized by ConfTimeFormatter if the key is
requested during string interpolation, and the key is not in the
relevant section.  This list of keys represents the time difference
between the forecast and analysis time.  Unlike FCST_KEYS and
ANL_KEYS, this is not a mapping: it is a set."""

##@var NOTFOUND
#  a special constant that represents a key not being found
NOTFOUND=object()

class ConfTimeFormatter(ConfFormatter):
    """!internal function that implements time formatting

    Like its superclass, ConfFormatter, this class is part of the
    implementation of HAFSConfig, and is used to interpolate strings
    in a way similar to string.format().  It works the same way as
    ConfFormatter, but accepts additional keys generated based on the
    forecast and analysis times:

        fYMDHM - 201409171200 = forecast time September 17, 2014 at 12:00 UTC
        fYMDH  - 2014091712
        fYMD   - 20140917
        fyear  - 2014
        fYYYY  - 2014
        fYY    - 14   (year % 100)
        fCC    - 20   (century)
        fcen   - 20
        fmonth - 09
        fMM    - 09
        fday   - 17
        fDD    - 17
        fhour  - 12
        fcyc   - 12
        fHH    - 12
        fminute - 00
        fmin   - 00

    Replace the initial "f" with "a" for analysis times.  In addition,
    the following are available for the time difference between
    forecast and analysis time.  Suppose the forecast is twenty-three
    hours and nineteen minutes (23:19) after the analysis time:

        fahr - 23
        famin - 1399   ( = 23*60+19)
        fahrmin - 19  """
    def __init__(self,quoted_literals=False):
        """!constructor for ConfTimeFormatter"""
        super(ConfTimeFormatter,self).__init__(
            quoted_literals=bool(quoted_literals))
    def get_value(self,key,args,kwargs):
        """!return the value of a variable, or a substitution

        Never call this function.  It is called automatically by
        str.format.  It provides the value of an variable,
        or a string substitution.
        @param key the string key being analyzed by str.format()
        @param args the indexed arguments to str.format()
        @param kwargs the keyword arguments to str.format()"""
        v=NOTFOUND
        kwargs['__depth']+=1
        if kwargs['__depth']>=configparser.MAX_INTERPOLATION_DEPTH:
            raise configparser.InterpolationDepthError(
                kwargs['__key'],kwargs['__section'],v)
        try:
            if isinstance(key,int):
                return args[key]
            if key in kwargs:
                v=kwargs[key]
            elif '__taskvars' in kwargs \
                    and kwargs['__taskvars'] \
                    and key in kwargs['__taskvars']:
                v=kwargs['__taskvars'][key]
            elif '__ftime' in kwargs and key in FCST_KEYS:
                v=kwargs['__ftime'].strftime(FCST_KEYS[key])
            elif '__atime' in kwargs and key in ANL_KEYS:
                v=kwargs['__atime'].strftime(ANL_KEYS[key])
            elif '__atime' in kwargs and key in ANL_M6_KEYS:
                am6=kwargs['__atime']-datetime.timedelta(0,3600*6)
                v=am6.strftime(ANL_M6_KEYS[key])
            elif '__atime' in kwargs and key in ANL_P6_KEYS:
                ap6=kwargs['__atime']+datetime.timedelta(0,3600*6)
                v=ap6.strftime(ANL_P6_KEYS[key])
            elif '__ftime' in kwargs and '__atime' in kwargs and \
                    key in TIME_DIFF_KEYS:
                (ihours,iminutes)=tcutil.numerics.fcst_hr_min(
                    kwargs['__ftime'],kwargs['__atime'])
                if key=='fahr':
                    v=int(ihours)
                elif key=='famin':
                    v=int(ihours*60+iminutes)
                elif key=='fahrmin':
                    v=int(iminutes)
                else:
                    v=int(ihours*60+iminutes)
            else:
                isec=key.find('/')
                if isec>=0:
                    section=key[0:isec]
                    nkey=key[(isec+1):]
                    if not section:
                        section=kwargs.get('__section',None)
                    if nkey:
                        key=nkey
                else:
                    section=kwargs.get('__section',None)
                conf=kwargs.get('__conf',None)
                if section and conf:
                    if conf.has_option(section,key):
                        v=conf.get(section,key)
                    elif conf.has_option(section,'@inc'):
                        for osec in conf.get(section,'@inc').split(','):
                            if conf.has_option(osec,key):
                                v=conf.get(osec,key)
                    if v is NOTFOUND:
                        if conf.has_option('config',key):
                            v=conf.get('config',key)
                        elif conf.has_option('dir',key):
                            v=conf.get('dir',key)
                    if v is NOTFOUND:
                        raise KeyError('Cannot find key %s in section %s'
                                       %(repr(key),repr(section)))

            if isinstance(v,str) and ( v.find('{')!=-1 or
                                              v.find('%')!=-1 ):
                try:
                    vnew=self.vformat(v,args,kwargs)
                    assert(vnew is not None)
                    return vnew
                except KeyError as e:
                    # Seriously, does the exception's class name
                    # really need to be this long?
                    raise ConfigParser.InterpolationMissingOptionError(
                        kwargs['__key'],kwargs['__section'],v,str(e))
            return v
        finally:
            kwargs['__depth']-=1

########################################################################

def confwalker(conf,start,selector,acceptor,recursevar):
    """!walks through a ConfigParser-like object performing some action

    Recurses through a ConfigParser-like object "conf" starting at
    section "start", performing a specified action.  The special
    variable whose name is in recursevar specifies a list of
    additional sections to recurse into.  No section will be processed
    more than once, and sections are processed in breadth-first order.
    For each variable seen in each section (including recursevar),
    this will call selector(sectionname, varname) to see if the
    variable should be processed.  If selector returns True, then
    acceptor(section, varname, value) will be called.

    @param conf the ConfigParser-like object
    @param start the starting section
    @param selector a function selector(section,option) that decides
       if an option needs processing (True) or not (False)
    @param acceptor a function acceptor(section,option,value)
       run on all options for which the selector returns True
    @param recursevar an option in each section that lists more
       sections the confwalker should touch.  If the selector returns
       True for the recursevar, then the recursevar will be sent to
       the acceptor.  However, it will be scanned for sections to
       recurse into even if the selector rejects it."""
    touched=set()
    requested=[str(start)]
    while len(requested)>0:
        sec=requested.pop(0)
        if sec in touched:
            continue
        touched.add(sec)
        for (key,val) in conf.items(sec):
            if selector(sec,key):
                acceptor(sec,key,val)
            if key==recursevar:
                for sec2 in reversed(val.split(',')):
                    trim=sec2.strip()
                    if len(trim)>0 and not trim in touched:
                        requested.append(trim)

########################################################################

def from_file(filename,quoted_literals=False):
    """!Reads the specified conf file into an HAFSConfig object.

    Creates a new HAFSConfig object and instructs it to read the specified file.
    @param filename the path to the file that is to be read
    @return  a new HAFSConfig object"""
    if not isinstance(filename,str):
        raise TypeError('First input to hafs.config.from_file must be a string.')
    conf=HAFSConfig(quoted_literals=bool(quoted_literals))
    conf.read(filename)
    return conf

def from_string(confstr,quoted_literals=False):
    """!Reads the given string as if it was a conf file into an HAFSConfig object

    Creates a new HAFSConfig object and reads the string data into it
    as if it was a config file
    @param confstr the config data
    @return a new HAFSConfig object"""
    if not isinstance(confstr,str):
        raise TypeError('First input to hafs.config.from_string must be a string.')
    conf=HAFSConfig(quoted_literals=bool(quoted_literals))
    conf.readstr(confstr)
    return conf

class HAFSConfig(object):
    """!a class that contains configuration information

    This class keeps track of configuration information for all tasks
    in a running HAFS model.  It can be used in a read-only manner as
    if it was a ConfigParser object.  All HAFSTask objects require an
    HAFSConfig object to keep track of registered task names via the
    register_task_name method, the current forecast cycle (cycle
    property) and the Datastore object (datastore property).

    This class should never be instantiated directly.  Instead, you
    should use the hafs.config.from_string or hafs.config.from_file to
    read configuration information from an in-memory string or a file.

    Also note that this class should not be used to create a new
    config file for the first HAFS job in a workflow.  The
    hafs.launcher module does that for you."""

    def __init__(self,conf=None,quoted_literals=False,strict=False, inline_comment_prefixes=(';',)):
        """!HAFSConfig constructor

        Creates a new HAFSConfig object.
        @param conf the underlying configparser.ConfigParser object
        that stores the actual config data. This was a SafeConfigParser
        in Python 2 but in Python 3 the SafeConfigParser is now ConfigParser.
        @param quoted_literals if True, then {'...'} and {"..."} will
          be interpreted as quoting the contained ... text.  Otherwise,
          those blocks will be considered errors.
        @param strict set default to False so it will not raise
          DuplicateOptionError or DuplicateSectionError, This param was
          added when ported to Python 3.6, to maintain the previous
          python 2 behavior.
        @param inline_comment_prefixes, defaults set to ;. This param was
          added when ported to Python 3.6, to maintain the previous
          python 2 behavior.

        Note: In Python 2, conf was ConfigParser.SafeConfigParser. In
        Python 3.2, the old ConfigParser class was removed in favor of
        SafeConfigParser which has in turn been renamed to ConfigParser.
        Support for inline comments is now turned off by default and
        section or option duplicates are not allowed in a single
        configuration source."""
        self._logger=logging.getLogger('prodconfig')
        logger=self._logger
        self._lock=threading.RLock()
        self._formatter=ConfFormatter(bool(quoted_literals))
        self._time_formatter=ConfTimeFormatter(bool(quoted_literals))
        self._datastore=None
        self._tasknames=set()
        # Added strict=False and inline_comment_prefixes for Python 3,
        # so everything works as it did before in Python 2.
        #self._conf=ConfigParser(strict=False, inline_comment_prefixes=(';',)) if (conf is None) else conf
        self._conf=ConfigParser(strict=strict, inline_comment_prefixes=inline_comment_prefixes) if (conf is None) else conf
        self._conf.optionxform=str

        self._conf.add_section('config')
        self._conf.add_section('dir')
        self._fallback_callbacks=list()

    @property
    def quoted_literals(self):
        return self._time_formatter.quoted_literals and \
               self._formatter.quoted_literals

    def fallback(self,name,details):
        """!Asks whether the specified fallback is allowed.  May perform
        other tasks, such as alerting the operator.

        Calls the list of functions sent to add_fallback_callback.
        Each one receives the result of the last, and the final result
        at the end is returned.  Note that ALL of the callbacks are
        called, even if one returns False; this is not a short-circuit
        operation.  This is done to allow all reporting methods report
        to their operator and decide whether the fallback is allowed.

        Each function called is f(allow,name,details) where:

        - allow = True or False, whether the callbacks called thus far
          have allowed the fallback.

        - name = The short name of the fallback.

        - details = A long, human-readable description.  May be
          several lines long.

        @param name the name of the emergency situation

        @warning This function may take seconds or minutes to return.
        It could perform cpu- or time-intensive operations such as
        emailing an operator.

        """
        allow=self.getbool('config','allow_fallbacks',False)
        for fc in self._fallback_callbacks:
            allow=bool(fc(allow,name,details))
        return allow

    def add_fallback_callback(self,function):
        """!Appends a function to the list of fallback callback functions
        called by fallback()

        Appends the given function to the list that fallback()
        searches while determining if a workflow emergency fallback
        option is allowed.

        @param function a function f(allow,name,details)
        @see fallbacks()"""
        self._fallback_callbacks.append(function)

    def readstr(self,source):

        """!read config data and add it to this object

        Given a string with conf data in it, parses the data.
        @param source the data to parse
        @return self"""
        fp=StringIO(str(source))
        self._conf.readfp(fp)
        fp.close()
        return self

    def read(self,source):
        """!reads and parses a config file

        Opens the specified config file and reads it, adding its
        contents to the configuration.  This is used to implement the
        from_file module-scope function.  You can use it again on an
        HAFSConfig object to read additional files.
        @param source the file to read
        @return self"""
        self._conf.read(source)
        return self

    def readfp(self,source):
        """!read config data from an open file

        Reads a config file from the specified file-like object.
        This is used to implement the readstr.
        @param source the opened file to read
        @return self"""
        self._conf.readfp(source)
        return self

    def readstr(self,string):
        """!reads config data from an in-memory string

        Reads the given string as a config file.  This is used to
        implement the from_string module-scope function.  You can use
        it again to read more config data into an existing HAFSConfig.
        @param string the string to parse
        @return self"""
        sio=StringIO(string)
        self._conf.readfp(sio)
        return self

    def set_options(self,section,**kwargs):
        """!set values of several options in a section

        Sets the value of several options in one section.  The
        keywords arguments are the names of the options to set and the
        keyword values are the option values.
        @param section the section being modified
        @param kwargs additional keyword arguments are the option names
            and values"""
        for k,v in kwargs.items():
            value=str(v)
            self._conf.set(section,k,value)

    def read_precleaned_vitfile(self,vitfile):
        """!reads tcvitals

        WARNING: This is presently unused and may be removed.  It does
        not belong in HAFSConfig.

        Reads tcvitals from the specified file if specified, or from
        the current cycle's vitals storage area if not.  Does not
        parse, clean or otherwise modify the vitals: they are assumed
        to contain output for only one storm, with no duplicate
        cycles.  Ideally, the file should have been created by the
        hafs.launcher module.  Returns an tcutil.revital.Revital object."""
        logger=self.log()
        logger.info('read vitals from: '+vitfile)
        revital=tcutil.revital.Revital(logger=logger)
        with open(vitfile,'rt') as f:
            revital.readfiles([vitfile])
        return revital

    @property
    def realtime(self):
        """!is this a real-time simulation?

        Is this configuration for a real-time simulation?  Defaults to
        True if unknown.  This is the same as doing
        getbool('config','realtime',True)."""
        return self.getbool('config','realtime',True)
    def set(self,section,key,value):
        """!set a config option

        Sets the specified config option (key) in the specified
        section, to the specified value.  All three are converted to
        strings via str() before setting the value."""
        self._conf.set(str(section),str(key),str(value))
    def __enter__(self):
        """!grab the thread lock

        Grabs this HAFSConfig's thread lock.  This is only for future
        compatibility and is never used."""
        self._lock.acquire()
    def __exit__(self,a,b,c):
        """!release the thread lock

        Releases this HAFSConfig's thread lock.  This is only for
        future compatibility and is never used.
        @param a,b,c unused"""
        self._lock.release()
    def register_hafs_task(self,name):
        """!add an hafs.hafstask.HAFSTask to the database

        Checks to ensure that there is no other task by this name, and
        records the fact that there is now a task.  This is used by
        the hafs.hafstask.HAFSTask to ensure only one task is made by
        any name."""
        with self:
            if name in self._tasknames:
                raise hafs.exceptions.DuplicateTaskName(
                    '%s: attempted to use this task name twice'%(name,))
            self._tasknames.add(name)
    def log(self,sublog=None):
        """!returns a logging.Logger object

        Returns a logging.Logger object.  If the sublog argument is
        provided, then the logger will be under that subdomain of the
        "hafs" logging domain.  Otherwise, this HAFSConfig's logger
        (usually the "hafs" domain) is returned.
        @param sublog the logging subdomain, or None
        @return a logging.Logger object"""
        if sublog is not None:
            with self:
                return logging.getLogger('hafs.'+sublog)
        return self._logger
    def getdatastore(self):
        """!returns the Datastore

        Returns the produtil.datastore.Datastore object for this
        HAFSConfig."""
        d=self._datastore
        if d is not None:
            return d
        with self:
            if self._datastore is None:
                dsfile=self.getstr('config','datastore')
                self._datastore=produtil.datastore.Datastore(dsfile,
                    logger=self.log('datastore'))
            return self._datastore

    ##@var datastore
    #  read-only property: the Datastore object for this HAFS simulation
    datastore=property(getdatastore,None,None, \
        """Returns the Datastore object for this HAFS simulation,
        creating it if necessary.  If the underlying datastore file
        did not already exist, it will be opened in create=True mode.""")

    def getcycle(self):
        """!get the analysis time

        Returns the analysis time of this HAFS workflow as a
        datetime.datetime."""
        if self._cycle is None:
            self._cycle=to_datetime(self._conf.get('config','cycle'))
        return self._cycle
    def setcycle(self,cycle):
        """!set the analysis time

        Sets the analysis time of this HAFS workflow.  Also sets the
        [config] section's "cycle" option.  Accepts anything that
        tcutil.numerics.to_datetime recognizes."""
        cycle=to_datetime(cycle)
        strcycle=cycle.strftime('%Y%m%d%H')
        self._conf.set('config','cycle',strcycle)
        self._cycle=cycle
        self.set_time_vars()

    ##@var cycle
    # the analysis cycle, a datetime.datetime object
    cycle=property(getcycle,setcycle,None,
        """The cycle this HAFS simulation should run, as a datetime.datetime.""")

    def set_time_vars(self):
        """!internal function that sets time-related variables

        Sets many config options in the [config] section based on this
        HAFS workflow's analysis time.  This is called automatically
        when the cycle property is assigned.  You never need to call
        this function directly.

         YMDHM - 201409171200 = forecast time September 17, 2014 at 12:00 UTC
         YMDH  - 2014091712
         YMD   - 20140917
         year  - 2014
         YYYY  - 2014
         YY    - 14   (year % 100)
         CC    - 20   (century)
         cen   - 20
         month - 09
         MM    - 09
         day   - 17
         DD    - 17
         hour  - 12
         cyc   - 12
         HH    - 12
         minute - 00
         min   - 00"""
        with self:
            for var,fmt in [ ('YMDHM','%Y%m%d%H%M'), ('YMDH','%Y%m%d%H'),
                             ('YMD','%Y%m%d'), ('year','%Y'), ('YYYY','%Y'),
                             ('YY','%y'), ('CC','%C'), ('cen','%C'),
                             ('month','%m'), ('MM','%m'), ('day','%d'),
                             ('DD','%d'), ('hour','%H'), ('cyc','%H'),
                             ('HH','%H'), ('minute','%M'), ('min','%M') ]:
                self._conf.set('config',var,self._cycle.strftime(fmt))
    def add_section(self,sec):
        """!add a new config section

        Adds a section to this HAFSConfig.  If the section did not
        already exist, it will be initialized empty.  Otherwise, this
        function has no effect.
        @param sec the new section's name"""
        with self:
            self._conf.add_section(sec)
            return self
    def has_section(self,sec):
        """!does this section exist?

        Determines if a config section exists (even if it is empty)
        @return  True if this HAFSConfig has the given section and
        False otherwise.
        @param   sec the section to check for"""
        with self:
            return self._conf.has_section(sec)
    def has_option(self,sec,opt):
        """! is this option set?

        Determines if an option is set in the specified section
        @return True if this HAFSConfig has the given option in the
        specified section, and False otherwise.
        @param sec the section
        @param opt the name of the option in that section"""
        with self:
            return self._conf.has_option(sec,opt)
    def getdir(self,name,default=None,morevars=None,taskvars=None):
        """! query the "dir" section

        Search the "dir" section.
        @return the specified key (name) from the "dir" section.
        Other options are passed to self.getstr.

        @param default the default value if the option is unset
        @param morevars more variables for string substitution
        @param taskvars even more variables for string substitution
        @param name the option name to search for"""
        with self:
            return self.getstr('dir',name,default=default,
                               morevars=morevars,taskvars=taskvars)
    def getloc(self,name,default=None,morevars=None,taskvars=None):
        """!search the config, exe and dir sections in that order

        Find the location of a file in the named option.  Searches
        the [config], [exe] and [dir] sections in order for an option
        by that name, returning the first one found.
        @param default the default value if the option is unset
        @param morevars more variables for string substitution
        @param taskvars even more variables for string substitution
        @param name the option name to search for
        @returns the resulting value"""
        with self:
            if self.has_option('config',name):
                return self.getstr('config',name,default=default,
                                   morevars=morevars,taskvars=taskvars)
            elif self.has_option('exe',name):
                return self.getstr('exe',name,default=default,
                                   morevars=morevars,taskvars=taskvars)
            else:
                return self.getstr('dir',name,default=default,
                                   morevars=morevars,taskvars=taskvars)
    def getexe(self,name,default=None,morevars=None,taskvars=None):
        """! query the "exe" section

        Search the "exe" section.
        @return the specified key (name) from the "exe" section.
        Other options are passed to self.getstr.

        @param default the default value if the option is unset
        @param morevars more variables for string substitution
        @param taskvars even more variables for string substitution
        @param name the option name to search for"""
        with self:
            return self.getstr('exe',name,default=default,morevars=morevars,
                               taskvars=taskvars)
    def __getitem__(self,arg):
        """!convenience function; replaces self.items and self.get

        This is a convenience function that provides access to the
        self.items or self.get functions.

        * conf["section"] -- returns a dict containing the results of
                              self.items(arg)
        * conf[a,b,c] -- returns self.get(a,b,c)
                          (b and c are optional)
        @param arg the arguments: a list or string"""
        with self:
            if isinstance(arg,str):
                return dict(self.items(arg))
            elif ( isinstance(arg,list) or isinstance(arg,tuple) ):
                if len(arg)==1:   return dict(self.items(arg))
                if len(arg)==2:   return self.get(str(arg[0]),str(arg[1]))
                if len(arg)==3:   return self.get(str(arg[0]),str(arg[1]),
                                                  default=arg[2])
        return NotImplemented
    def makedirs(self,*args):
        """!calls produtil.fileop.makedirs() on directories in the [dir] section

        This is a simple utility function that calls
        produtil.fileop.makedirs() on some of the directories in the
        [dir] section.
        @param args the keys in the [dir] section for the directories
        to make."""
        with self:
            dirs=[self.getstr('dir',arg) for arg in args]
        for makeme in dirs:
            produtil.fileop.makedirs(makeme)
    def keys(self,sec):
        """!get options in a section

        Returns a list containing the config options in the given
        section.
        @param sec the string name of the section"""
        with self:
            return [ opt for opt in self._conf.options(sec) ]

    def sections(self):
        """!gets the list of all sections from a configuration object"""
        return self._conf.sections()

    def items(self,sec,morevars=None,taskvars=None):
        """!get the list of (option,value) tuples for a section

        Returns a section's options as a list of two-element
        tuples.  Each tuple contains a config option, and the value of
        the config option after string interpolation.  Note that the
        special config section inclusion option "@inc" is also
        returned.
        @param sec the section
        @param morevars variables for string substitution
        @param taskvars yet more variables
        @return a list of (option,value) tuples, where the value is
          after string expansion"""
        out=[]
        with self:
            for opt in self._conf.options(sec):
                out.append((opt,self._interp(sec,opt,morevars,
                                             taskvars=taskvars)))
        return out
    def write(self,fileobject):
        """!write the contents of this HAFSConfig to a file

        Writes the contents of an HAFSConfig to the specified file,
        without interpolating (expanding) any strings.  The file will
        be suitable for reading in to a new HAFSConfig object in a
        later job.  This is used by the hafs.launcher module to create
        the initial config file.

        @param fileobject an opened file to write to"""
        with self:
            self._conf.write(fileobject)
    def getraw(self,sec,opt,default=None):
        """!return the raw value of an option

        Returns the raw value for the specified section and option,
        without string interpolation.  That is, any {...} will be
        returned unmodified.  Raises an exception if no value is set.
        Will not search other sections, unlike other accessors.
        @param sec the section
        @param opt the option name
        @param default the value to return if the option is unset.
           If unspecified or None, NoOptionError is raised"""
        try:
            return self._conf.get(sec,opt,raw=True)
        except NoOptionError:
            if default is not None: return default
            raise

    def getvitals(self):
        if 'syndat' in self.__dict__:
            return self.syndat
        return None

    def setvitals(self,vit):
        assert(isinstance(vit,tcutil.storminfo.StormInfo))
        self.syndat=vit

    def delvitals(self):
        if 'syndat' in self.__dict__:
            del self.syndat

    vitals=property(getvitals,setvitals,delvitals,
                    """The tcutil.storminfo.StormInfo describing the storm to be run, or a fake storm representing the basin in multi-storm mode.""")

    def strinterp(self,sec,string,**kwargs):
        """!perform string expansion

        Performs this HAFSConfig's string interpolation on the
        specified string, as if it was a value from the specified
        section.
        @param sec the section name
        @param string the string to expand
        @param kwargs more variables for string substitution"""
        assert(isinstance(sec,str))
        assert(isinstance(string,str))
        with self:
            if 'vit' not in kwargs and 'syndat' in self.__dict__:
                kwargs['vit']=self.syndat.__dict__
            if 'oldvit' not in kwargs and 'oldsyndat' in self.__dict__:
                kwargs['oldvit']=self.oldsyndat.__dict__
            return self._formatter.format(string,__section=sec,
                __key='__string__',__depth=0,__conf=self._conf,
                ENV=ENVIRONMENT,**kwargs)
    def timestrinterp(self,sec,string,ftime=None,atime=None,**kwargs):
        """!performs string expansion, including time variables

        Performs this HAFSConfig's string interpolation on the
        specified string, as self.strinterp would, but adds in
        additional keys based on the given analysis and forecast
        times.  The keys are the same as the keys added to [config]
        for the cycle, except with "a" prepended for the analysis
        time, or "f" for the forecast time.  There are three more keys
        for the difference between the forecast an analysis time.  The
        famin is the forecast time in minutes, rounded down.  The fahr
        and fahrmin are the forecast hour, rounded down, and the
        remainder in minutes, rounded down to the next nearest minute.

        If the analysis time is None or unspecified, then self.cycle
        is used.  The atime can be anything understood by
        tcutil.numerics.to_datetime and the ftime can be anything
        understood by tcutil.numerics.to_datetime_rel, given the atime
        (or, absent atime, self.cycle) as the second argument.

        This is implemented as a wrapper around the
        self._time_formatter object, which knows how to expand the a*
        and f* variables without having to generate all of them.

        @param sec the section name
        @param string the string to expand
        @param ftime the forecast time or None
        @param atime the analysis time or None
        @param kwargs more variables for string expansion"""
        if atime is not None:
            atime=tcutil.numerics.to_datetime(atime)
        else:
            atime=self.cycle
        if ftime is None:
            ftime=atime
        else:
            ftime=tcutil.numerics.to_datetime_rel(ftime,atime)
        if 'vit' not in kwargs and 'syndat' in self.__dict__:
            kwargs['vit']=self.syndat.__dict__
        if 'oldvit' not in kwargs and 'oldsyndat' in self.__dict__:
            kwargs['oldvit']=self.oldsyndat.__dict__
        with self:
            return self._time_formatter.format(string,__section=sec,
                __key='__string__',__depth=0,__conf=self._conf,ENV=ENVIRONMENT,
                __atime=atime, __ftime=ftime,**kwargs)

    def _interp(self,sec,opt,morevars=None,taskvars=None):
        """!implementation of data-getting routines

        This is the underlying implementation of the various self.get*
        routines, and lies below the self._get.  It reads a config
        option opt from the config section sec.

        If the string contains a {...} expansion, the _interp will
        perform string interpolation, expanding {...} strings
        according to ConfigParser rules.  If the section contains an
        @inc, and any variables requested are not found, then the
        sections listed in @inc are searched.  Failing that, the
        config and dir sections are searched.

        @param sec the section name
        @param opt the option name
        @param morevars a dict containing variables whose values will
        override anything in this HAFSConfig when performing string
        interpolation.
        @param taskvars  serves the same purpose as morevars, but
        provides a second scope.
        @return the result of the string expansion"""
        vitdict={}
        olddict={}
        if 'syndat' in self.__dict__: vitdict=self.syndat.__dict__
        if 'oldsyndat' in self.__dict__: olddict=self.oldsyndat.__dict__
        sections=( sec, 'config','dir', '@inc' )
        gotted=False
        for section in sections:
            if section=='@inc':
                try:
                    inc=self._conf.get(sec,'@inc')
                except NoOptionError:
                    inc=''
                if inc:
                    touched=set(( sec,'config','dir' ))
                    for incsection in inc.split(","):
                        trim=incsection.strip()
                        if len(trim)>0 and trim not in touched:
                            touched.add(trim)
                            try:
                                got=self._conf.get(trim,opt,raw=True)
                                gotted=True
                                break
                            except (KeyError,NoSectionError,NoOptionError) as e:
                                pass # var not in section; search elsewhere
                if gotted: break
            else:
                try:
                    got=self._conf.get(section,opt,raw=True)
                    gotted=True
                    break
                except (KeyError,NoSectionError,NoOptionError) as e:
                    pass # var not in section; search elsewhere

        if not gotted:
            raise NoOptionError(opt,sec)

        if morevars is None:
            return self._formatter.format(got,
                __section=sec,__key=opt,__depth=0,__conf=self._conf, vit=vitdict,
                ENV=ENVIRONMENT, oldvit=olddict,__taskvars=taskvars)
        else:
            return self._formatter.format(got,
                __section=sec,__key=opt,__depth=0,__conf=self._conf, vit=vitdict,
                ENV=ENVIRONMENT, oldvit=olddict,__taskvars=taskvars,
                                          **morevars)

    def _get(self,sec,opt,typeobj,default,badtypeok,morevars=None,taskvars=None):
        """! high-level implemention of get routines

        This is the implementation of all of the self.get* routines.
        It obtains option opt from section sec via the self._interp,
        providing the optional list of additional variables for string
        interpolation in the morevars.  It then converts to the given
        type via typeobj (for example, typeobj=int for int
        conversion).  If default is not None, and the variable cannot
        be found, then the default is returned.

        @param sec the section name
        @param opt the option name in that section
        @param default the default value to return if the variable
          cannot be found, or None if no default is provided.
        @param badtypeok if True and default is not None, and the type
          conversion failed, then default is returned.  Otherwise, the
          TypeError resulting from the failed type conversion is passed
          to the caller.
        @param morevars a dict containing variables whose values will
          override anything in this HAFSConfig when performing string
          interpolation.
        @param taskvars  serves the same purpose as morevars, but
          provides a second scope.        """
        try:
            s=self._interp(sec,opt,morevars=morevars,taskvars=taskvars)
            assert(s is not None)
            return typeobj(s)
        except NoOptionError:
            if default is not None:
                return default
            raise
        except TypeError:
            if badtypeok:
                if default is not None: return default
                return None
            raise
    def getint(self,sec,opt,default=None,badtypeok=False,morevars=None,taskvars=None):
        """!get an integer value

        Gets option opt from section sec and expands it; see "get" for
        details.  Attempts to convert it to an int.

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        with self:
            return self._get(sec,opt,int,default,badtypeok,morevars,taskvars=taskvars)

    def getfloat(self,sec,opt,default=None,badtypeok=False,morevars=None,taskvars=None):
        """!get a float value

        Gets option opt from section sec and expands it; see "get" for
        details.  Attempts to convert it to a float

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        with self:
            return self._get(sec,opt,float,default,badtypeok,morevars,taskvars=taskvars)

    def getstr(self,sec,opt,default=None,badtypeok=False,morevars=None,taskvars=None):
        """!get a string value

        Gets option opt from section sec and expands it; see "get" for
        details.  Attempts to convert it to a str

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        with self:
            return self._get(sec,opt,str,default,badtypeok,morevars,taskvars=taskvars)

    def get(self,sec,opt,default=None,badtypeok=False,morevars=None,taskvars=None):
        """!get the value of an option from a section

        Gets option opt from section sec, expands it and converts
        to a string.  If the option is not found and default is
        specified, returns default.  If badtypeok, returns default if
        the option is found, but cannot be converted.  The morevars is
        used during string expansion: if {abc} is in the value of the
        given option, and morevars contains a key abc, then {abc} will
        be expanded using that value.  The morevars is a dict that
        allows the caller to override the list of variables for string
        extrapolation.
        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        with self:
            try:
                return self._interp(sec,opt,morevars,taskvars=taskvars)
            except NoOptionError:
                if default is not None:
                    return default
                raise

    def options(self,sec):
        """!what options are in this section?

        Returns a list of options in the given section
        @param sec the section"""
        with self:
            return self._conf.options(sec)

    def getboolean(self,sec,opt,default=None,badtypeok=False,morevars=None,taskvars=None):
        """!alias for getbool: get a bool value

        This is an alias for getbool for code expecting a
        ConfigParser.  Gets option opt from section sec and expands
        it; see "get" for details.  Attempts to convert it to a bool

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        return self.getbool(sec,opt,default=default,badtypeok=badtypeok,
                            morevars=morevars,taskvars=taskvars)

    def getbool(self,sec,opt,default=None,badtypeok=False,morevars=None,taskvars=None):
        """!get a bool value

        Gets option opt from section sec and expands it; see "get" for
        details.  Attempts to convert it to a bool

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        try:
            with self:
                s=self._interp(sec,opt,morevars=morevars,taskvars=taskvars)
        except NoOptionError:
            if default is not None:
                return bool(default)
            raise
        if re.match('(?i)\A(?:T|\.true\.|true|yes|on|1)\Z',s):   return True
        if re.match('(?i)\A(?:F|\.false\.|false|no|off|0)\Z',s): return False
        try:
            return int(s)==0
        except ValueError as e: pass
        if badtypeok and default is not None:
            return bool(default)
        raise ValueError('%s.%s: invalid value for HAFS conf file boolean: %s'
                         %(sec,opt,repr(s)))
