"""!Exceptions raised by the hafs package

This module contains exceptions raised by the hafs package."""

class HAFSError(Exception):
    """!Base class of all exceptions in this module.

    This is the base class of exceptions raised by the HAFS module
    due to HAFS-specific failures.  It is possible to get other
    exceptions from the StandardException hierarchy in certain failure
    cases.  For example, trying to obtain the fifth of three domains
    may raise KeyError.  Also, see pom.exceptions for exceptions that
    may be raised from the pom package.  See the produtil package for
    exceptions that may come from lower levels."""

########################################################################
# CONFIGURATION EXCEPTIONS

class DuplicateTaskName(HAFSError):
    """!Raised when more than one task is registered with the same
    name in an HAFSConfig object."""

class InvalidConfigOptName(HAFSError):
    """!Raised when one tries to use an invalid string for an option
    name."""

########################################################################
# FAILURE TESTING

class FailureTestException(HAFSError):
    """!Raised to simulate a failure condition in the HAFS system."""

class ExpectedFailureTest(FailureTestException):
    """!Raised to simulate a failure condition in the HAFS system that
    should be caught, and trigger automated fallbacks."""

class UnexpectedFailureTest(FailureTestException):
    """!Raised to simulate a failure condition in the HAFS system that
    should never happen, and should result in an immediate abort of
    the workflow, even if automated fallbacks are enabled.    """

########################################################################
# ARCHIVING EXCEPTIONS


########################################################################
# NAMELIST-RELATED EXCEPTIONS

class NamelistValueError(ValueError):
    """!Raised when hafs.namelist cannot convert a value to or from
    Fortran namelist format."""
class NamelistKeyError(KeyError):
    """!Raised when an hafs.namelist is asked for a key that does not
    exist."""
    ##@var section
    # the section that was searched

    ##@var var
    #  the option that was requested

    ##@var message
    #  the exception message

    def __init__(self,message,section,var):
        """!Constructor.
        @param message the string message
        @param section the section that was searched
        @param var the option that was requested"""
        super(NamelistKeyError,self).__init__(message)
        self.section=section
        self.var=var
        self.message=message
    def __str__(self):
        """!Generates a string description of this exception."""
        if self.section=='-trait-':
            return 'trait %s: %s' % (self.var,self.message)
        else:
            return '&%s %s: %s' % (self.section,self.var,self.message)
    def __repr__(self):
        """!Generates a string representation of this object."""
        return 'NamelistError(%s,%s,%s)' % \
            ( repr(self.message), repr(self.section), repr(self.var) )

########################################################################
# SANITY CHECKER EXCEPTIONS

class HAFSSanityError(HAFSError):
    """!Base class of all sanity checker exceptions."""
class HAFSDirInsane(HAFSSanityError):
    """!Raised when a directory is unspecified, missing or invalid."""
    ##@var dir
    #  The directory in question.

    def __init__(self,message,dir):
        """!HAFSDirInsane constructor.
        @param message  a string explanation of the problem
        @param dir      the directory in question"""
        super(HAFSDirInsane,self).__init__(message)
        self.dir=dir
class HAFSConfigInsane(HAFSSanityError):
    """!Raised when the requested configuration conf or hafs_expt files
    fail a sanity check."""
class HAFSConfigUnsupported(HAFSConfigInsane):
    """!Raised when the user requests a configuration that makes sense,
    but is not supported."""
class HAFSConfigFileOrder(HAFSConfigInsane):
    """!Raised when configuration files were specified in the wrong order."""
class HAFSStormInsane(HAFSSanityError):
    """!Raised when the configuration had a different storm than expected."""
class HAFSCycleInsane(HAFSSanityError):
    """!Raised when the configuration had a different cycle than expected."""
class HAFSVariableInsane(HAFSSanityError):
    """!Raised when a sanity check on a variable's value failed."""
class HAFSInputInsane(HAFSSanityError):
    """!Raised when input files to HAFS fail a sanity check."""
class HAFSScriptInsane(HAFSSanityError):
    """!Raised when HAFS scripts fail a sanity check."""
class HAFSExecutableInsane(HAFSSanityError):
    """!Raised when the HAFS executables fail a sanity check."""
class HAFSFixInsane(HAFSSanityError):
    """!Raised when the HAFS fix files fail a sanity check."""
class HAFSArchiveInsane(HAFSSanityError):
    """!Raised when the sanity check of the HAFS archiving settings
    fails."""
class HAFSDataModelInsane(HAFSSanityError):
    """!Raised when the sanity check of the HAFS data model settings
    fails."""

########################################################################
# OCEAN AND WAVE EXCEPTIONS

#NOTE: See pom.exceptions for more
class OceanInitFailed(HAFSError):
    """!Raised when the ocean init did not produce some expected outputs."""
class NoOceanData(HAFSError):
    """!Raised when the parent global ocean model data was unavailable."""
class OceanExeUnspecified(OceanInitFailed):
    """!Raised when the HyCOM init foregets to choose an executable for
    the forecast job."""
class InvalidOceanInitMethod(OceanInitFailed):
    """!Raised when an invalid ocean initialization method is requested."""
class OceanRestartMissing(OceanInitFailed):
    """!Raised when the ocean restart file is missing."""
class NoOceanBasin(OceanInitFailed):
    """!Raised when there is no ocean basin for the selected domain center."""
class OceanDataInvalid(OceanInitFailed):
    """!Raised when an ocean input file contains invalid data."""

class WaveInitFailed(HAFSError):
    """!Raised when the wave initialization failes."""
class WW3InputError(WaveInitFailed):
    """!Raised when the wavewatch 3 cannot find necessary input."""

########################################################################
# COUPLING EXCEPTIONS

class HAFSCouplingError(HAFSError):
    """!Superclass of atmosphere-ocean-wave-otherthings coupling
    exceptions."""
class NoCoupledComponents(HAFSCouplingError):
    """!Raised when one requests a coupled forecast without specifying
    what is being coupled."""
class EmptyCouplerNamelist(HAFSCouplingError):
    """!Raised when the NCEP Coupler is to be used for coupling but its
    namelist file is empty or missing."""

########################################################################
# GSI EXCEPTIONS

class GSIInputError(HAFSError):
    """!Raised when GSI cannot find a required input file."""
class ExpectedTDR(HAFSError):
    """!Used in failure testing to abort the system if TDR was not present."""

########################################################################
# TRACKER EXCEPTIONS

class TrackerError(HAFSError):
    """!Base class of hafs.tracker exceptions."""
class TrackerModeError(TrackerError):
    """!Raised when an impossible tracker configuration is requested,
    such as running with a grid that is both regional and global."""
class TrackerStormError(TrackerError):
    """!Raised when multiple storms are requested, but only one was
    expected."""
class TrackerInputError(TrackerError):
    """!Base class of exceptions raised when the tracker's input files
    are missing or invalid."""
class MissingGRIBError(TrackerInputError):
    """!Not currently used, this would be raised when GRIB inputs to
    the tracker are missing."""
class GRIBLocationError(TrackerInputError):
    """!Raised when no location is specified for a tracker input GRIB
    file."""

########################################################################
# TIME-RELATED EXCEPTIONS (used by many modules)

class HAFSTimeError(HAFSError):
    """!Base class used for time-related HAFS exceptions."""

# Time and timestep exceptions:
class InvalidTimestep(HAFSTimeError):
    """!Raised when a timestep is invalid, such as a negative timestep
    for a situation that requires a positive one."""
class TimestepModularityError(HAFSTimeError):
    """!Called when one hour is not divisable by the WRF output
    timestep."""
class OverspecifiedOutputTime(HAFSTimeError):
    """!Raised when an output time is specified in two redundant ways.

    For example, one could specify a forecast time directly, and also
    specify the analysis time and forecast time delta."""
class NoOutputTime(HAFSTimeError):
    """!Raised when a time was required, but none was provided."""
class TimezoneProvided(HAFSTimeError):
    """!Raised when a timezone is provided.  The hafs package does not
    support timezones: all times are in UTC."""
class PrecisionTooHigh(HAFSTimeError):
    """!Raised when a time was requested with higher precision than available.

    Raised when a time was provided that contained fractions of a
    second, for a function that cannot handle that.  For example, the
    WRF output files must be exactly on a second boundary."""
class NotInTimespan(HAFSTimeError):
    """!Raised when a time is outside the range of times being
    processed by a function."""
class NoNearbyValues(HAFSTimeError):
    """!Raised when an operation has a set of known times, but another
    provided time is not near one of those known times."""

class InvalidTimespan(HAFSTimeError):
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

########################################################################
# REGRIB-RELATED EXCEPTIONS (mostly hafs.regrib and hafs.gribtask)

class RegribError(HAFSError):
    """!Superclass of errors used by Regrib."""
class GRIBInputError(RegribError):
    """!Raised when a GRIB file is invalid.

    Raised when a GRIB file is provided, but that file is invalid.
    This can be due to either an input to an operation, or the output
    from the operation."""
class Subsetless(RegribError):
    """!Raised when a Regrib was expecting a GRIB subsetter function,
    but no such function was provided."""

class InvalidRegribResult(RegribError):
    """!Debug assetion in hafs.regrib used to detect type mismatches.

    Part of debug assertions in hafs.regrib, this is raised when the
    wrong type is generated by the "make" function."""

class RegribProductError(RegribError):
    """!Superclass of errors relating to regrib products."""
class NoProductError(RegribProductError):
    """!Raised when an operation that produces input to Regrib should
    have produced a Product, but produced nothing at all."""
class ProductAmbiguityError(RegribProductError):
    """!Raised when an operation that provides input to Regrib produces
    more than one product."""
class NoIndexError(RegribError):
    """!Raised when a GRIB file should have an index file already, but
    doesn't."""

class RegribManyError(RegribError):
    """!Base class of errors from the hafs.regrib.RegribMany"""
class RegribKeyError(RegribManyError):
    """!Raised when a RegribMany is given an invalid name: one that
    does not match a known grid or operation."""

class RegribGridError(RegribError):
    """!Base class of grid-related regrib errors."""
class GridlessError(RegribGridError):
    """!Raised when a grid was expected but none was provided."""
class GridMismatchError(RegribGridError):
    """!Raised when two GRIB files have non-matching grids, but a
    matching grid is required."""

class GribberError(RegribError):
    """!Exceptions for hafs.regrib.GRIBTask for certain internal errors.

    Raised by GRIBTask for unexpected errors that did not come from
    the underlying RegribAll object.  This is used in GRIBTask.run's
    "run it now" mode, when setting raiseall=True."""

########################################################################
# INPUT EXCEPTIONS

class HAFSInputError(HAFSError):
    """!Base class of exceptions related to the hafs.input module."""
class InputSourceBadType(HAFSInputError):
    """!Raised when a configuration file requests a DataCatalog class
    that does not exist."""
class InvalidInputSpecification(HAFSInputError):
    """!Raised when an input source is missing the location, or both
    histprio and fcstprio."""
class PartialTransfer(HAFSInputError):
    """!Raised when a file transfer, done by an InputSource, was
    incomplete."""
class UnsupportedTransfer(HAFSInputError):
    """!Raised when a configuration file requests an unsupported data
    transfer method (such as carrier pigeon)."""

########################################################################
# Post exceptions

class PostFailed(HAFSError):
    """!Raised upon errors that would cause a retry, in the
    PostOneWRF.run when passed the raiseall=True argument."""

class PostHasNoInput(HAFSError):
    """!Raised when the post's input file is not available and
    raiseall=True in PostOneWRF.run"""

########################################################################
# Relocation exceptions

class RelocationError(HAFSError):
    """!Raised when something in the vortex relocation fails."""
class RelocationInputError(RelocationError):
    """!Raised when required inputs to the relocation are missing."""
class RelocationConfigurationError(RelocationError):
    """!Raised when an impossible configuration is requested."""
class RelocateOutputMissing(RelocationError):
    """!Raised when a relocation program did not produce an expected
    output file."""
class UnexpectedColdStart(RelocationError):
    """!Raised when the relocation could not find the prior cycle's 6hr
    forecast, but it expected to be able to."""
class StormRadiusError(RelocationError):
    """!Raised when the merge cannot find the storm_radius file in the
    relocate or fix directory."""
class NoSuchDomain(RelocationError):
    """!Raised by hafs.input when trying to get the wrong domain from
    its hafs.relocate.Relocate child objects."""
class EnsdaTrackerMissing(RelocationError):
    """Raised when the relocation could not find the prior cycle's
    ensemble forecast track, but it expected to be able to."""
