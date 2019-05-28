"""!Exceptions raised by the tcutil package

This module contains exceptions raised by the tcutil package.  The only
exceptions are the tcutil.revital and tcutil.storminfo modules which
contain their own exceptions."""

########################################################################
# TIME-RELATED EXCEPTIONS

class TimeError(Exception): 
    """!Base class used for time-related  exceptions."""

# Time and timestep exceptions:
class InvalidTimestep(TimeError): 
    """!Raised when a timestep is invalid, such as a negative timestep
    for a situation that requires a positive one."""
class TimestepModularityError(TimeError):
    """!Called when one hour is not divisable by the WRF output
    timestep."""
class OverspecifiedOutputTime(TimeError): 
    """!Raised when an output time is specified in two redundant ways.

    For example, one could specify a forecast time directly, and also
    specify the analysis time and forecast time delta."""
class NoOutputTime(TimeError): 
    """!Raised when a time was required, but none was provided."""
class TimezoneProvided(TimeError): 
    """!Raised when a timezone is provided.  The tcutil package does not
    support timezones: all times are in UTC."""
class PrecisionTooHigh(TimeError): 
    """!Raised when a time was requested with higher precision than available.

    Raised when a time was provided that contained fractions of a
    second, for a function that cannot handle that.  For example, the
    WRF output files must be exactly on a second boundary."""
class NotInTimespan(TimeError): 
    """!Raised when a time is outside the range of times being
    processed by a function."""
class NoNearbyValues(TimeError): 
    """!Raised when an operation has a set of known times, but another
    provided time is not near one of those known times."""

class InvalidTimespan(TimeError):
    """!Superclass of exceptions relating to groups of one or more
    distinct times and relationships between them."""
    ##@var start
    #  the start of the problematic timespan

    ##@var end
    #  the end of the problematic timespan

    def __init__(self,message,start,end):
        """! Constructor for InvalidTimespan
        
        @param message the string explanation of the problem
        @param start the start of the timespan
        @param end   the end of the timespan"""
        super(InvalidTimespan,self).__init__(message)
        self.start=start
        self.end=end
class EndBeforeStart(InvalidTimespan): 
    """!Raised when the end of a timespan is before the beginning."""
class EndNotTimestep(InvalidTimespan): 
    """!Raised when the end of a timespan is not a timestep.
    Presently unused.

    Presently unused, this was to indicate that the end of a timespan
    is not on a timestep, for temporally discrete processes.  Such end
    times are allowed in WRF, so this exception is unused."""
class StartNotAtParentTimestep(InvalidTimespan): 
    """!Raised when a timespan's beginning is not at a timestep."""
class TimestepTooLong(InvalidTimespan): 
    """!Raised when a timestep is too long for the process under
    consideration."""
class TimestepTooShort(InvalidTimespan): 
    """!Raised when a timestep is too short for the process under
    consideration."""
class NoTimespan(InvalidTimespan): 
    """!Raised when a timespan was expected, but none was available."""
