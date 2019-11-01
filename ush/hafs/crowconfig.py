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

import collections,re,string,os,logging,threading,copy, io
import os.path,sys
import datetime
import produtil.fileop, produtil.datastore
import tcutil.numerics, tcutil.storminfo, tcutil.revital
import hafs.exceptions

from collections.abc import Mapping, Sequence

from io import StringIO

from produtil.datastore import Datastore
from produtil.fileop import *

import crow
from crow.config.eval_tools import expand as crow_expander

from tcutil.numerics import to_datetime
from string import Formatter
from configparser import NoOptionError,NoSectionError

from tcutil.numerics import to_datetime
from tcutil.storminfo import find_tcvitals_for
from hafs.exceptions import HAFSError

INITIAL_DOCUMENT='''
config: {}
dir: {}
exe: {}
'''

##@var EMPTY_SECTION
# An internal CROW representation of an empty section.
EMPTY_SECTION=crow.config.from_string('---\n{}',multi_document=True)[0]

SIX_HOURS=datetime.timedelta(seconds=3600*6)

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

##@var CYC_KEYS
#  the list of forecast time keys recognized by ConfTimeFormatter 
CYC_KEYS=dict([ ('YMDHM','%Y%m%d%H%M'), ('YMDH','%Y%m%d%H'), 
                             ('YMD','%Y%m%d'), ('year','%Y'), ('YYYY','%Y'),
                             ('YY','%y'), ('CC','%C'), ('cen','%C'),
                             ('month','%m'), ('MM','%m'), ('day','%d'),
                             ('DD','%d'), ('hour','%H'), ('cyc','%H'),
                             ('HH','%H'), ('minute','%M'), ('min','%M') ])
"""A list of keys for the cycle time, without the "a" prefix.

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
         min   - 00

"""

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

##@var SPECIAL_CROW_KEYS
# Keys in a CROW YAML map that should never be converted to
# standard Python types.
SPECIAL_CROW_KEYS=set([ 'Inherit', 'Template', 'Evaluate' ])

##@var NOTFOUND
#  a special constant that represents a key not being found
NOTFOUND=object()

##@var UNSPECIFIED
#  a special constant representing an argument not passed
UNSPECIFIED=object()

class VitWrapper(object):
    def __init__(self,child):
        self.__child=child
    def setchild(self,child):
        self.__child=child
    def getchild(self,child):
        return self.__child
    def __getattr__(self,key,default=UNSPECIFIED):
        if default is UNSPECIFIED:
            return getattr(self.__child,key)
        else:
            return getattr(self.__child,key,default)
    def __setattr__(self,key,val):
        return setattr(self.__child,key,val)
    def __hasattr__(self,key):
        return hasattr(self.__child,key)

    # Implementing getattr, setattr, and hasattr will break copy and
    # deepcopy unless they're manually implemented:

    def __copy__(self):
        cls=type(self)
        return cls(self.__child)
    def __deepcopy__(self,memo):
        cls=type(self)
        new=cls(copy.deepcopy(self.__child))
        memo[id(self)]=new
        return new

TIMEINFO_KEYS=set(['fcsttime','anltime','dt'])
TIMEINFO_KEYS.update(FCST_KEYS.keys())
TIMEINFO_KEYS.update(ANL_KEYS.keys())
TIMEINFO_KEYS.update(ANL_M6_KEYS.keys())
TIMEINFO_KEYS.update(ANL_P6_KEYS.keys())
TIMEINFO_KEYS.update(TIME_DIFF_KEYS)

class TimeInfo(Mapping):
    def __init__(self,cyctime=None,anltime=None,fcsttime=None):
        self.__anltime=None
        self.__cyctime=None
        self.__anlp6=None
        self.__anlm6=None
        self.__dt=None
        self.cyctime=cyctime
        self.fcsttime=fcsttime
        self.anltime=anltime

    def getcyctime(self): return self.__cyctime
    def setcyctime(self,c): self.__cyctime=c

    def __len__(self): return len(TIMEINFO_KEYS)

    @property
    def dt(self): return self.__dt
    @property
    def anlp6(self): return self.__anlp6
    @property
    def anlm6(self): return self.__anlm6

    def __update_dt_info(self):
        if self.__fcsttime is not None and self.__anltime is not None:
            self.__dt=self.__fcsttime-self.__anltime
            (self.__ihours,self.__iminutes) = tcutil.numerics.fcst_hr_min(
                self.__fcsttime,self.__anltime)
        else:
            self.__dt=None
            self.__ihours=None
            self.__iminutes=None

    def getfcsttime(self): return self.__fcsttime
    def setfcsttime(self,t):
        if t is None:
            self.delfcsttime()
        else:
            self.__fcsttime=datetime.datetime(t)
            self.__update_dt_info()
        return self.__fcsttime
    def delfcsttime(self):
        self.__fcsttime=None
        self.__dt=None
        self.__ihours=None
        self.__iminutes=None
        return None
    fcsttime=property(getfcsttime,setfcsttime,delfcsttime,
                      """The forecast time for the current query""")

    def getanltime(self): return self.__anltime
    def setanltime(self,t):
        if t is None:
            self.delanltime()
        else:
            self.__anltime=datetime.datetime(t)
            self.__anlm6=anltime-SIX_HOURS
            self.__anlp6=anltime+SIX_HOURS
            self.__update_dt_info()
        return self.__anltime
    def delanltime(self):
        self.__anltime=None
        self.__anlm6=None
        self.__anlp6=None
        self.__update_dt_info()
        return None
    anltime=property(getanltime,setanltime,delanltime,
                     """The analysis time of this cycle""")

    def __getattr__(self,key):
        try:
            return self[key]
        except (ValueError,TypeError,IndexError) as e:
            raise AttributeError(str(e))

    def __iter__(self):
        for key in TIMEINFO_KEYS:
            return key

    def __contains__(self,key):
        return key in TIMEINFO_KEYS

    def __getitem__(self,key):
        if key not in TIMEINFO_KEYS:
            raise KeyError(key)

        if key=='fcsttime':
            return self.__fcsttime # Even if it is None
        elif key=='anltime':
            return self.__anltime # Even if it is None
        elif key=='dt':
            return self.__dt # Even if it is None

        have_fcst=self.__fcsttime is not None
        have_anl=self.__anltime is not None

        if key in CYC_KEYS:
            if self.__cycle is not None:
                raise KeyError(f'{key}: cycle time is unknown')
            return self.__cyctime.strftime(CYC_KEYS[key])
        if key in FCST_KEYS:
            if not have_fcst: raise KeyError(f'{key}: fcst time is unknown')
            return self.__fcsttime.strftime(FCST_KEYS[key])
        if key in ANL_KEYS:
            if not have_anl: raise KeyError(f'{key}: anl time is unknown')
            return self.__anltime.strftime(ANL_KEYS[key])
        if key in ANL_P6_KEYS:
            if not have_anl: raise KeyError(f'{key}: anl time is unknown')
            return self.__anlp6.strftime(ANL_M6_KEYS[key])
        if key in ANL_M6_KEYS:
            if not have_anl: raise KeyError(f'{key}: anl time is unknown')
            return self.__anlm6.strftime(ANL_M6_KEYS[key])
        if key in TIME_DIFF_KEYS:
            if not have_anl: raise KeyError(f'{key}: anl time is unknown')
            if not have_fcst: raise KeyError(f'{key}: fcst time is unknown')
            if key[2:]=='hr': return int(self.__ihours)
            if key[2:]=='min': return int(self.__ihours*60+self.__iminutes)
            if key[2:]=='hrmin': return int(self.__iminutes)
        # Should never get here.
        raise KeyError(f'{key}: internal error; key not processed')

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
    conf=HAFSConfig()
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
    conf=HAFSConfig()
    conf.readstr(confstr)
    return conf

class DictWrapper(Mapping):
    def __init__(self,child=None):
        self.__child=child
    def _get_child(self):
        return self.__child
    def _set_child(self,child):
        self.__child=child
    def _clear_child(self):
        self.__child=None
    _child=property(_get_child,_set_child,_clear_child,
                    """Internal variable to store the dict that is being """
                    """wrapped.  If the dict is None, it is treated as """
                    """an empty dict.""")
    def __len__(self):
        if self.__child is None: return 0
        return len(self.__child)
    def __iter__(self):
        if self.__child is None:
            return
        for k in self.__child():
            yield k
    def __getitem__(self,k):
        if self.__child is None:
            raise KeyError(k)
        return self.__child[k]
    def get(self,k,d):
        if self.__child is None:
            return d
        return self.__child.get(k,d)
    def __copy__(self):
        cls=type(self)
        return cls(self.__child)
    def __deepcopy__(self,memo):
        cls=type(self)
        new=cls(copy.deepcopy(self.__child))
        memo[id(self)]=new
        return new

class AttrDictWrapper(DictWrapper):
    def __getattr__(self,k):
        if k in self:
            return self[k]
    def __hasattr__(self,k):
        return k in self

class StormInfoWrapper(object):
    def __init__(self,child=None):
        self.__child=child
    def _get_child(self):
        return self.__child
    def _set_child(self,child):
        self.__child=child
    def _clear_child(self):
        raise BaseException('clear child')
        self.__child=None
    _child=property(_get_child,_set_child,_clear_child,
      """StormInfo object being viewed""")
    def __getattr__(self,attr):
        if self.__child is None:
            raise AttributeError(attr)
        return getattr(self.__child,attr)
    def __hasattr__(self,attr):
        if self.__child is None:
            raise AttributeError(attr)
        return hasattr(self.__child,attr)
    def __copy__(self):
        cls=type(self)
        return cls(self.__child)
    def __deepcopy__(self,memo):
        cls=type(self)
        new=cls(copy.deepcopy(self.__child))
        memo[id(self)]=new
        return new

class MultiDictWrapper(Mapping):
    """!This is a dict-like object that makes multiple dicts act as one.
    Its methods look over the dicts in order, returning the result
    from the first dict that has a matching key.  This class is
    intended to be used in favor of a new dict, when the underlying
    dicts have special behaviors that are lost upon copy to a standard dict.

    @note Modified from crow.config.eval_tools.multidict"""
    def __init__(self,*args):
        self.__dicts=list(args)
    def __len__(self):
        return sum([ len(d) for d in self.__dicts ])
    def __copy__(self):
        return type(self)(*self.__dicts)
    def __deepcopy__(self):
        newdicts=[ copy.deepcopy(d,memo) for d in self.__dicts ]
        new=cls(self)(*newdicts)
        memo[id(self)]=new
        return new
    def __hasattr__(self,key):
        return key in self
    def __contains__(self,key):
        for d in self.__dicts:
            if key in d:
                return True
        return False
    def __iter__(self):
        seen=set()
        for d in self.__dicts:
            for k in d:
                if k in seen: continue
                seen.add(k)
                return k
    def __getitem__(self,key):
        for d in self.__dicts:
            if key in d:
                return d[key]
        raise KeyError(key)
    def __getattr__(self,key):
        for d in self.__dicts:
            if key in d:
                return d[key]
        raise AttributeError(key)
    def __repr__(self):
        return '%s(%s)'%(
            type(self).__name__,
            ','.join([repr(d) for d in self.__dicts]))
    def __str__(self):
        return '{'+', '.join([f'{k}:{v}' for k,v in self])+'}'

def convert_to_py(crowobj,memo,retain_crow):
    i=id(crowobj)
    if i in memo: return memo[i]
    if isinstance(crowobj,str):        return crowobj
    if isinstance(crowobj,bool):       return crowobj
    if isinstance(crowobj,float):      return crowobj
    if isinstance(crowobj,int):        return crowobj
    if isinstance(crowobj,datetime.datetime):
        return crowobj
    if isinstance(crowobj,datetime.timedelta):
        return crowobj
    if isinstance(crowobj,Sequence):
        py=list()
        memo[i]=py
        for s in crowobj:
            py.append(convert_to_py(s,memo,retain_crow))
        return py
    if isinstance(crowobj,Mapping):
        py=dict()
        memo[i]=py
        for k,v in crowobj.items():
            if k in SPECIAL_CROW_KEYS and not retain_crow:
                continue
            py[k]=convert_to_py(v,memo,retain_crow)
        return py
    if retain_crow:
        return crowobj
    return NotImplemented

def convert_to_conf_scalar(crowobj):
    if isinstance(crowobj,str):
        if '\n' in crowobj or ';' in crowobj:
            return NotImplemented
        return crowobj
    if isinstance(crowobj,bool):
        return 'true' if crowobj else 'false'
    if isinstance(crowobj,float):
        return '%g'%crowobj
    if isinstance(crowobj,int):
        return '%d'%crowobj
    if isinstance(crowobj,datetime.datetime):
        return crowobj.strftime('%Y%m%d%H%M')
    if isinstance(crowobj,datetime.timedelta):
        return crowobj.total_seconds()
    return NotImplemented

def convert_to_conf(section,fd):
    count_okay=0
    for opt,val in section.items():
        if opt in SPECIAL_CROW_KEYS:
            fd.write(f'; {opt!r} omitted: key has special meaning in CROW\n')
            continue
        if isinstance(val,str):
            result=convert_to_conf_scalar(val)
        elif isinstance(val,Sequence):
            okay=True
            index=-1
            result=list()
            for item in val:
                index=index+1
                one=convert_to_conf_scalar(item)
                if one is NotImplemented:
                    fd.write(f'; {opt!r} omitted: element at index {index} could not be expressed in a string\n')
                    okay=False
                    break
                if ',' in one:
                    fd.write(f'; {opt!r} omitted: element at index {index} contains a comma (",")\n')
                    okay=False
                    break
                result.append(one)
            if not okay:
                continue # Error has already been printed
            result=','.join(result)
        else:
            result=convert_to_conf_scalar(val)
        if result is NotImplemented:
            fd.write(f'; {opt!r} omitted: cannot represent its value as a string\n')
        else:
            count_okay+=1
            fd.write(f'{opt}={result}\n')
    return count_okay

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
        @param quoted_literals ignored; present for backward compatibility
        @param strict ignored; present for backward compatibility
        @param inline_comment_prefixes ignored; present for backward compatibility"""
        self._logger=logging.getLogger('prodconfig')
        logger=self._logger
        self._lock=threading.RLock()
        self._datastore=None
        self._tasknames=set()

        self._doc=crow.config.from_string(INITIAL_DOCUMENT)

        self._globals={
            'cyc':TimeInfo(),
            'vit':StormInfoWrapper(),
            'oldvit':StormInfoWrapper(),
            'more':AttrDictWrapper(),
            'task':AttrDictWrapper(),
        }

        self._globals['all']=MultiDictWrapper(
            self._doc.config,self._doc.dir,self._doc.exe,
            self._globals['cyc'],self._globals['task'],
            self._globals['more'])


        crow.config.update_globals(self._doc,self._globals)

        # Added strict=False and inline_comment_prefixes for Python 3, 
        # so everything works as it did before in Python 2.
        self._fallback_callbacks=list()

        self._prior_vit=None
        self._prior_oldvit=None

    @property
    def quoted_literals(self):   return True

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
        allow=self._doc.config.get('allow_fallbacks',False)
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

    def readstr(self,s):
        """!read config data and add it to this object

        Given a string with conf data in it, parses the data.
        @param source the data to parse
        @return self"""
        context=f'"{repr(s[:13])[1:-1]}..."'
        for doc in crow.config.from_string(s.read(),multi_document=True,evaluate_immediates=False):
            self.__merge_from_crow(doc,context)

    def readfp(self,fd):
        """!read config data from an open file

        Reads a config file from the specified file-like object.
        This is used to implement the readstr.
        @param source the opened file to read
        @return self"""
        s=fd.read()
        context=f'"{repr(s[:13])[1:-1]}..."'
        for doc in crow.config.from_string(s,multi_document=True,evaluate_immediates=False):
            self.__merge_from_crow(doc,context)

    def read(self,filename):
        """!reads and parses a config file

        Opens the specified config file and reads it, adding its
        contents to the configuration.  This is used to implement the
        from_file module-scope function.  You can use it again on an
        HAFSConfig object to read additional files.
        @param source the file to read
        @return self"""
        for doc in crow.config.from_file(filename,multi_document=True):
            self.__merge_from_crow(doc,filename)

    def __merge_from_crow(self,doc,context):
        """!Internal function that merges all document-level values
        from this document to self's document

        @param doc The document to merge
        @param context Contextual information for error messages"""
        child=doc._raw_child()
        my_child=self._doc._raw_child()
        for key,expr in child.items():
            if not isinstance(expr,Mapping):
                raise TypeError(f'{context}: {key}: all document-level types must be maps.')
            if not hasattr(expr,'_raw_child'):
                raise TypeError(f'{context}: {key}: all document-level types must have been read in by CROW.')
            if key not in my_child:
                self.add_section(key)
            my_child[key]._raw_child().update(expr._raw_child())
        crow.config.update_globals(self._doc,self._globals)

    def set_options(self,section,**kwargs):
        """!set values of several options in a section

        Sets the value of several options in one section.  The
        keywords arguments are the names of the options to set and the
        keyword values are the option values.
        @param section the section being modified
        @param kwargs additional keyword arguments are the option names
            and values"""
        strsection=str(section)
        if strsection not in self._doc:
            self._doc[strsection]=copy.deepcopy(EMPTY_SECTION)
        for k,v in kwargs.items():
            value=str(v)
            self._doc[strsection][str(k)]=value

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
    def doc(self):
        """!The top-level CROW document."""
        return self._doc

    @property
    def realtime(self):
        """!is this a real-time simulation?

        Is this configuration for a real-time simulation?  Defaults to
        True if unknown.  This is the same as doing
        getbool('config','realtime',True)."""
        return self.getbool('config','realtime',True)
    def set(self,section,key,value,expand=UNSPECIFIED):
        """!set a config option

        Sets the specified config option (key) in the specified
        section, to the specified value.  All three are converted to
        strings via str() before setting the value."""
        strsection=str(section)
        if strsection not in self._doc:
            self._doc[strsection]=copy.deepcopy(EMPTY_SECTION)
        if isinstance(value,str) and ( expand is UNSPECIFIED or expand ):
            self._doc[strsection][str(key)]=crow_expander(value)
        else:
            self._doc[strsection][str(key)]=value
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
        self.clear_locals()
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
            self._cycle=to_datetime(self._doc.config.cycle)
        return self._cycle
    def setcycle(self,cycle):
        """!set the analysis time

        Sets the analysis time of this HAFS workflow.  Also sets the
        [config] section's "cycle" option.  Accepts anything that
        tcutil.numerics.to_datetime recognizes."""
        cycle=to_datetime(cycle)
        strcycle=cycle.strftime('%Y%m%d%H')
        self._doc.config.cycle=strcycle
        self.set_time_vars()

    ##@var cycle
    # the analysis cycle, a datetime.datetime object
    cycle=property(getcycle,setcycle,None,
        """The cycle this HAFS simulation should run, as a datetime.datetime.""")

    def set_time_vars(self):
        """!internal function that sets time-related variables

        Updates internal variables so that cycle information will be
        correct        """
        cycle=datetime.datetime.strptime(
            '%Y%m%d%H',self._doc.config.cycle)
        self._globals['cyc'].cycle=cycle
    def add_section(self,sec):
        """!add a new config section

        Adds a section to this HAFSConfig.  If the section did not
        already exist, it will be initialized empty.  Otherwise, this
        function has no effect.
        @param sec the new section's name"""
        strsection=str(sec)
        with self:
            if strsection not in self.doc:
                self._doc[strsection]=copy.deepcopy(EMPTY_SECTION)
        return self
    def has_section(self,sec): 
        """!does this section exist?

        Determines if a config section exists (even if it is empty)
        @return  True if this HAFSConfig has the given section and
        False otherwise.
        @param   sec the section to check for"""
        with self:
            return sec in self._doc
    def has_option(self,sec,opt):
        """! is this option set?
        
        Determines if an option is set in the specified section
        @return True if this HAFSConfig has the given option in the
        specified section, and False otherwise.
        @param sec the section
        @param opt the name of the option in that section"""
        with self:
            return sec in self._doc and opt in self._doc[sec]
    def getdir(self,name,default=UNSPECIFIED,morevars=None,taskvars=None):
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
    def getloc(self,name,default=UNSPECIFIED,morevars=None,taskvars=None):
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
            self.set_locals(morevars=morevars,taskvars=taskvars)
            for secname in [ 'config', 'exe', 'dir' ]:
                section=self._doc[secname]
                if name in section:
                    return section[name]
        if default is UNSPECIFIED:
            raise KeyError
        return default
    def getexe(self,name,default=UNSPECIFIED,morevars=None,taskvars=None):
        """! query the "exe" section

        Search the "exe" section.
        @return the specified key (name) from the "exe" section.
        Other options are passed to self.getstr.

        @param default the default value if the option is unset
        @param morevars more variables for string substitution
        @param taskvars even more variables for string substitution
        @param name the option name to search for"""
        with self:
            return self.getstr('exe',name,default=default,
                               morevars=morevars,taskvars=taskvars)
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
                if len(arg)==1:
                    return self._doc[str(arg[0])]
                if len(arg)==2:
                    return self._doc[str(arg[0])][str(arg[1])]
                if len(arg)==3:
                    strsection=str(arg[0])
                    strkey=str(arg[1])
                    if strsection not in self._doc:
                        return arg[2]
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
            if sec not in self._doc: return []
            return list(self._doc[sec].keys())
        
    def sections(self):
        """!gets the list of all sections from a configuration object"""
        return list(self._doc.keys())
    
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
            if sec not in self._doc: return []
            return [ (k,v) for k,v in self._doc[sec].items() ]
    def write(self,fileobject):
        """!write the contents of this HAFSConfig to a file

        Writes the contents of an HAFSConfig to the specified file,
        without performing any calculations except immediate
        calculations.  In other words, it will be CROW YAML.  The file
        will be suitable for reading in to a new HAFSConfig object in
        a later job.  To write YAML without CROW types, use
        py_evaluate_to_yaml()

        @param fileobject an opened file to write to

        """
        with self:
            fileobject.write(crow.config.to_yaml(self._doc))
    def getraw(self,sec,opt,default=UNSPECIFIED):
        """!return the raw value of an option

        Returns the value specified for an option in the YAML file.
        If this is a calculated value, then the internal CROW
        representation of the calculation is returned.

        @param sec the section
        @param opt the option name
        @param default the value to return if the option is unset.
        """
        if default is not UNSPECIFIED:
            if sec not in self._doc:
                return default
            if opt not in self._doc[sec]:
                return default
        return self._doc[sec]._raw(opt)

    def clear_locals(self):
        del self._globals['task']._child
        del self._globals['more']._child
        self._globals['vit']._child=self._prior_vit
        self._globals['oldvit']._child=self._prior_oldvit
    def set_locals(self,vit=None,oldvit=None,taskvars=None,morevars=None,fcsttime=None,anltime=None):
        if fcsttime is not None:
            self._globals['cyc'].fcsttime=fcsttime
        if anltime is not None:
            self._globals['cyc'].anltime=anltime
        if taskvars:
            self._globals['task']._child=taskvars
        if morevars:
            self._globals['more']._child=morevars
        self._prior_vit=self._globals['vit']._child
        if vit is not None:
            self._globals['vit']._child=vit
        self._prior_oldvit=self._globals['oldvit']._child
        if oldvit is not None:
            self._globals['oldvit']._child=oldvit
        if self._globals['vit']._child is None:
            line='JTWC 91S INVEST    20190422 0000 094S 0522E 260 052 1004 1007 0315 13 074 -999 -999 -999 -999 S'
            canned_vit=tcutil.storminfo.StormInfo('tcvitals',line.rstrip('\n'))
            self.vitals=canned_vit
            self._globals['oldvit']._child=canned_vit-6
            self._prior_vit=self.vitals
            self._prior_oldvit=self._globals['oldvit']._child

    def getvitals(self):
        if 'syndat' in self.__dict__:
            return self.syndat
        return None

    def setvitals(self,vit):
        assert(isinstance(vit,tcutil.storminfo.StormInfo))
        self.syndat=vit
        self._globals['vit']._child=vit

    def delvitals(self):
        if 'syndat' in self.__dict__:
            raise BaseException('delvitals')
            del self.syndat
            del self._globals['vit']._child

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
            if kwargs:
                self.set_locals(morevars=kwargs)
            return crow.config.expand_text(string,self._doc[sec])
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
        if 'vit' in morevars:
            vit=morevars['vit']
        else:
            vit=self.syndat
        if 'oldvit' in morevars:
            oldvit=morevars['oldvit']
        else:
            oldvit=slef.oldsyndat
        with self:
            self.set_locals(morevars=kwargs,fcsttime=ftime,
                            anltime=anltime,vit=vit,oldvit=oldvit)
            return crow.config.expand_text(string,self._doc[sec])

    def _get(self,sec,opt,typeobj=UNSPECIFIED,default=UNSPECIFIED,badtypeok=False,morevars=None,taskvars=None):
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
        with self:
            section=self._doc.get(sec,NOTFOUND)
            if section is NOTFOUND:
                if default is not UNSPECIFIED:
                    return default
                raise KeyError(sec)
            self.set_locals(morevars=morevars,taskvars=taskvars)
            if default is not UNSPECIFIED:
                result=section.get(opt,default)
            else:
                result=section[opt]
            if typeobj is UNSPECIFIED:
                return result
            try:
                return typeobj(result)
            except (ValueError,TypeError) as e:
                if badtypeok:
                    if default is not UNSPECIFIED:
                        return default
                    return None
                raise
    def getint(self,sec,opt,default=UNSPECIFIED,badtypeok=False,morevars=None,taskvars=None):
        """!get an integer value

        Gets option opt from section sec and expands it; see "get" for
        details.  Attempts to convert it to an int.  

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        return self._get(sec,opt,int,default,badtypeok,morevars,taskvars=taskvars)

    def getfloat(self,sec,opt,default=UNSPECIFIED,badtypeok=False,morevars=None,taskvars=None):
        """!get a float value

        Gets option opt from section sec and expands it; see "get" for
        details.  Attempts to convert it to a float

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        return self._get(sec,opt,float,default,badtypeok,morevars,taskvars=taskvars)

    def getstr(self,sec,opt,default=UNSPECIFIED,badtypeok=False,morevars=None,taskvars=None):
        """!get a string value

        Gets option opt from section sec and expands it; see "get" for
        details.  Attempts to convert it to a str

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        return self._get(sec,opt,str,default,badtypeok,morevars,taskvars=taskvars)

    def py_evaluate(self,retain_crow=False,morevars=None,taskvars=None):
        """!Evaluates expressions and returns a dict of dicts

        Walks through all sections, evaluating any CROW calculations.
        All contents are converted to standard python types if
        possible.  The configuration information is returned as a dict
        of dicts.

        """
        memo=dict()
        with self:
            self.set_locals(morevars=morevars,taskvars=taskvars)
            return convert_to_py(self._doc,memo,bool(retain_crow))

    def py_evaluate_to_yaml(self,retain_crow=False,morevars=None,taskvars=None):
        """!Evaluates expressions and generates resulting YAML

        Walks through all sections, evaluating any CROW calculations.
        All contents are converted to standard python types if
        possible.  (That is done by to_py().)  The result is convert
        to YAML and returned as a string."""
        py=self.py_evaluate(retain_crow=retain_crow,morevars=morevars,taskvars=taskvars)
        return crow.config.to_yaml(py)

    def py_evaluate_to_conf(self,morevars=None,taskvars=None):
        """!Evaluates expressions and generates resulting INI-style files

        Walks through all sections, evaluating any CROW calculations.
        All contents are converted to standard python types if
        possible.  (That is done by to_py().)  The result is convert
        to YAML and returned as a string.        """
        fd=io.StringIO()
        with self:
            self.set_locals(morevars=morevars,taskvars=taskvars)
            for secname,sec in self._doc.items():
                fd.write(f'[{secname}]\n')
                count_okay=convert_to_conf(sec,fd)
                if not count_okay:
                    fd.write('; (empty section)\n')
                fd.write('\n')
        result=fd.getvalue()
        fd.close()
        return result

    def get(self,sec,opt,default=UNSPECIFIED,badtypeok=False,morevars=None,taskvars=None):
        """!get the value of an option from a section

        Gets option opt from section sec and returns the result in the
        type that was used in YAML.  If the option is not found and
        default is specified, returns default.  If badtypeok, returns
        default if the option is found, but cannot be converted.

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion
        """
        with self:
            try:
                return self._get(sec,opt,morevars=morevars,taskvars=taskvars)
            except NoOptionError:
                if default is not None:
                    return default
                raise

    def options(self,sec):
        """!what options are in this section?

        Returns a list of options in the given section
        @param sec the section"""
        with self:
            return list(self._doc[sec].keys())
    
    def getboolean(self,sec,opt,default=UNSPECIFIED,badtypeok=False,morevars=None,taskvars=None):
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

    def getbool(self,sec,opt,default=UNSPECIFIED,badtypeok=False,morevars=None,taskvars=None):
        """!get a bool value

        Gets option opt from section sec and expands it; see "get" for
        details.  Attempts to convert it to a bool

        @param sec,opt the section and option
        @param default if specified and not None, then the default is
          returned if an option has no value or the section does not exist
        @param badtypeok is True, and the conversion fails, and a
          default is specified, the default will be returned.
        @param morevars,taskvars dicts of more variables for string expansion"""
        return bool(self.get(sec,opt,default=default,morevars=morevars,taskvars=taskvars))
