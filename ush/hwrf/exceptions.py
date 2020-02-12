"""!Exceptions raised by the hwrf package

This module contains exceptions raised by the hwrf package.  The only
exceptions are the hwrf.revital and hwrf.storminfo modules which
contain their own exceptions for backward compatibility to old
scripts."""

class HWRFError(Exception): 
    """!Base class of all exceptions in this module.

    This is the base class of exceptions raised by the HWRF module
    due to HWRF-specific failures.  It is possible to get other
    exceptions from the StandardException hierarchy in certain failure
    cases.  For example, trying to obtain the fifth of three domains
    may raise KeyError.  Also, see pom.exceptions for exceptions that
    may be raised from the pom package.  See the produtil package for
    exceptions that may come from lower levels."""

########################################################################
# CONFIGURATION EXCEPTIONS

class DuplicateTaskName(HWRFError):
    """!Raised when more than one task is registered with the same
    name in an HWRFConfig object."""

class InvalidConfigOptName(HWRFError):
    """!Raised when one tries to use an invalid string for an option
    name."""

########################################################################
# FAILURE TESTING

class FailureTestException(HWRFError):
    """!Raised to simulate a failure condition in the HWRF system."""

class ExpectedFailureTest(FailureTestException):
    """!Raised to simulate a failure condition in the HWRF system that
    should be caught, and trigger automated fallbacks."""

class UnexpectedFailureTest(FailureTestException):
    """!Raised to simulate a failure condition in the HWRF system that
    should never happen, and should result in an immediate abort of
    the workflow, even if automated fallbacks are enabled.    """

########################################################################
# ARCHIVING EXCEPTIONS


########################################################################
# NAMELIST-RELATED EXCEPTIONS

class NamelistValueError(ValueError):
    """!Raised when hwrf.namelist cannot convert a value to or from
    Fortran namelist format."""
class NamelistKeyError(KeyError):
    """!Raised when an hwrf.namelist is asked for a key that does not
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

class HWRFSanityError(HWRFError):
    """!Base class of all sanity checker exceptions."""
class HWRFDirInsane(HWRFSanityError):
    """!Raised when a directory is unspecified, missing or invalid."""
    ##@var dir
    #  The directory in question.

    def __init__(self,message,dir):
        """!HWRFDirInsane constructor.
        @param message  a string explanation of the problem
        @param dir      the directory in question"""
        super(HWRFDirInsane,self).__init__(message)
        self.dir=dir
class HWRFConfigInsane(HWRFSanityError):
    """!Raised when the requested configuration conf or hwrf_expt files
    fail a sanity check."""
class HWRFConfigUnsupported(HWRFConfigInsane):
    """!Raised when the user requests a configuration that makes sense,
    but is not supported."""
class HWRFConfigFileOrder(HWRFConfigInsane):
    """!Raised when configuration files were specified in the wrong order."""
class HWRFStormInsane(HWRFSanityError):
    """!Raised when the configuration had a different storm than expected."""
class HWRFCycleInsane(HWRFSanityError):
    """!Raised when the configuration had a different cycle than expected."""
class HWRFVariableInsane(HWRFSanityError):
    """!Raised when a sanity check on a variable's value failed."""
class HWRFInputInsane(HWRFSanityError):
    """!Raised when input files to HWRF fail a sanity check."""
class HWRFScriptInsane(HWRFSanityError):
    """!Raised when HWRF scripts fail a sanity check."""
class HWRFExecutableInsane(HWRFSanityError):
    """!Raised when the HWRF executables fail a sanity check."""
class HWRFFixInsane(HWRFSanityError):
    """!Raised when the HWRF fix files fail a sanity check."""
class HWRFArchiveInsane(HWRFSanityError):
    """!Raised when the sanity check of the HWRF archiving settings
    fails."""

########################################################################
# OCEAN AND WAVE EXCEPTIONS

#NOTE: See pom.exceptions for more
class OceanInitFailed(HWRFError):
    """!Raised when the ocean init did not produce some expected outputs."""
class NoOceanData(HWRFError):
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

class WaveInitFailed(HWRFError):
    """!Raised when the wave initialization failes."""
class WW3InputError(WaveInitFailed):
    """!Raised when the wavewatch 3 cannot find necessary input."""

########################################################################
# COUPLING EXCEPTIONS

class HWRFCouplingError(HWRFError):
    """!Superclass of atmosphere-ocean-wave-otherthings coupling
    exceptions."""
class NoCoupledComponents(HWRFCouplingError):
    """!Raised when one requests a coupled forecast without specifying
    what is being coupled."""
class EmptyCouplerNamelist(HWRFCouplingError):
    """!Raised when the NCEP Coupler is to be used for coupling but its
    namelist file is empty or missing."""

########################################################################
# GSI EXCEPTIONS

class GSIInputError(HWRFError):
    """!Raised when GSI cannot find a required input file."""
class ExpectedTDR(HWRFError):
    """!Used in failure testing to abort the system if TDR was not present."""
    
########################################################################
# TRACKER EXCEPTIONS

class TrackerError(HWRFError): 
    """!Base class of hwrf.tracker exceptions."""
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

class HWRFTimeError(HWRFError): 
    """!Base class used for time-related HWRF exceptions."""

# Time and timestep exceptions:
class InvalidTimestep(HWRFTimeError): 
    """!Raised when a timestep is invalid, such as a negative timestep
    for a situation that requires a positive one."""
class TimestepModularityError(HWRFTimeError):
    """!Called when one hour is not divisable by the WRF output
    timestep."""
class OverspecifiedOutputTime(HWRFTimeError): 
    """!Raised when an output time is specified in two redundant ways.

    For example, one could specify a forecast time directly, and also
    specify the analysis time and forecast time delta."""
class NoOutputTime(HWRFTimeError): 
    """!Raised when a time was required, but none was provided."""
class TimezoneProvided(HWRFTimeError): 
    """!Raised when a timezone is provided.  The hwrf package does not
    support timezones: all times are in UTC."""
class PrecisionTooHigh(HWRFTimeError): 
    """!Raised when a time was requested with higher precision than available.

    Raised when a time was provided that contained fractions of a
    second, for a function that cannot handle that.  For example, the
    WRF output files must be exactly on a second boundary."""
class NotInTimespan(HWRFTimeError): 
    """!Raised when a time is outside the range of times being
    processed by a function."""
class NoNearbyValues(HWRFTimeError): 
    """!Raised when an operation has a set of known times, but another
    provided time is not near one of those known times."""

class InvalidTimespan(HWRFTimeError):
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
# REGRIB-RELATED EXCEPTIONS (mostly hwrf.regrib and hwrf.gribtask)

class RegribError(HWRFError): 
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
    """!Debug assetion in hwrf.regrib used to detect type mismatches.

    Part of debug assertions in hwrf.regrib, this is raised when the
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
    """!Base class of errors from the hwrf.regrib.RegribMany"""
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
    """!Exceptions for hwrf.regrib.GRIBTask for certain internal errors.

    Raised by GRIBTask for unexpected errors that did not come from
    the underlying RegribAll object.  This is used in GRIBTask.run's
    "run it now" mode, when setting raiseall=True."""

########################################################################
# WRF EXCEPTIONS

class WRFError(HWRFError): 
    """!Base class of WRF-related errors."""
class DomainsDone(WRFError): 
    """!Raised when it is no longer possible to add domains, but the
    caller tried to add one."""
class WRFDomainError(WRFError):
    """!Base class of errors related to a WRF domain."""
    def __init__(self,message,domainname=None):
        """! WRFDomainError constructor.
        @param message the string description of the message
        @param domainname the name of the hwrf.wrf.WRFDomain in question"""
        super(WRFDomainError,self).__init__(message)
        self.domainname=domainname
    ##@var domainname
    # The domain in question, or None if unknown.

class InvalidDomainStart(WRFDomainError): 
    """!Raised when the hwrf.wrf.WRFDomain start type is unrecognized.

    Raised when the conf value specifying the type of domain parent
    start information contains invalid data (not "fixed" or "auto.")"""
class DomainExists(WRFDomainError): 
    """!Raised when adding a domain, if a domain by the same name
    already exists."""
class OutputStreamDisabled(WRFError): 
    """!Raised when attempting to obtain information about when a WRF
    stream outputs, if the stream is disabled."""
class RealNMMError(WRFError): 
    """!Raised when failing to run the real_nmm program."""
class SetNestFailed(WRFError): 
    """!Raised when failing to set the parent start location via the
    set_nest (set_ij_start) program."""

########################################################################
# FORECAST INPUT DATA EXCEPTIONS

class ForecastInputError(HWRFError): 
    """!Parent class of exceptions specific to the hwrf.fcsttask module"""
class WRFMissing(ForecastInputError):
    """!Raised when wrf or real want to run but needed input data is missing."""
class WRFGeogridMissing(WRFMissing):
    """!Raised when wrf or real want to run but geogrid data is missing"""
class WRFMetgridMissing(WRFMissing):
    """!Raised when wrf or real want to run, but metgrid data is missing."""
class WRFInputMissing(WRFMissing):
    """!Raised when wrf or real want to run, but wrfinput files are missing."""
class WRFBdyMissing(WRFMissing):
    """!Raised when wrf or real want to run, but wrfbdy files are missing."""
class WRFAnlMissing(WRFMissing):
    """!Raised when wrf or real want to run, but wrfanl files are missing."""
class WRFPrepMissing(WRFMissing):
    """!Raised when real wants prep_hybrid data, but that data is missing"""

########################################################################
# WPS EXCEPTIONS

class WPSError(HWRFError): 
    """!Base class of WPS-related exceptions."""
class WPSAssertion(HWRFError):
    """!Raised when WPS inputs are implausable."""
class GeogridNoLog(WPSError):
    """!Raised when geogrid did not create a log file."""
class GeogridNoOutput(WPSError):
    """!Raised when a geogrid output file is missing or empty."""
class UngribNoInput(WPSError):
    """!Raised when ungrib cannot find an input file it needs."""
class UngribInputUnknown(WPSError):
    """!Raised for hwrf.wps.Ungrib input problems.

    Raised when ungrib cannot figure out if an input file is GRIB1 or
    GRIB2.  This error usually means it is neither file type (or it is
    empty)."""
class UngribSubsetError(WPSError):
    """!Raised when wgrib or wgrib2 generates an empty or invalid file."""
class UngribCannotSubset(WPSError):
    """!Raised when hwrf.wps.Ungrib cannot subset files as requested.

    Raised when the caller wants to merge and subset GRIB files in
    a single operation.  The Ungrib class does not presently support
    that."""

########################################################################
# PrepHybrid exceptions:

class PrepHybridError(HWRFError): 
    """!Base class of exceptions related to the prep_hybrid program."""
class NoGeogData(PrepHybridError): 
    """!Raised when the prep_hybrid program cannot find WPS geogrid
    output data."""
class NoSpectralData(PrepHybridError):
    """!Raised when the spectral input files to prep_hybrid do not
    exist after some specified amount of time."""

########################################################################
# INPUT EXCEPTIONS

class HWRFInputError(HWRFError): 
    """!Base class of exceptions related to the hwrf.input module."""
class InputSourceBadType(HWRFInputError):
    """!Raised when a configuration file requests a DataCatalog class
    that does not exist."""
class InvalidInputSpecification(HWRFInputError):
    """!Raised when an input source is missing the location, or both
    histprio and fcstprio."""
class PartialTransfer(HWRFInputError):
    """!Raised when a file transfer, done by an InputSource, was
    incomplete."""
class UnsupportedTransfer(HWRFInputError):
    """!Raised when a configuration file requests an unsupported data
    transfer method (such as carrier pigeon)."""

########################################################################
# Post exceptions

class PostFailed(HWRFError):
    """!Raised upon errors that would cause a retry, in the
    PostOneWRF.run when passed the raiseall=True argument."""

class PostHasNoInput(HWRFError):
    """!Raised when the post's input file is not available and
    raiseall=True in PostOneWRF.run"""

########################################################################
# Relocation exceptions

class RelocationError(HWRFError):
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
    """!Raised by hwrf.input when trying to get the wrong domain from
    its hwrf.relocate.Relocate child objects."""
class EnsdaTrackerMissing(RelocationError):
    """Raised when the relocation could not find the prior cycle's 
    ensemble forecast track, but it expected to be able to."""
