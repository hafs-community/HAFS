#!/usr/bin/env python

##@namespace pom.exceptions
#This module contains exception classes for reporting errors in the
#POM initialization.
#
#@author Samuel.Trahan, NOAA NCEP EMC
#@author Biju Thomas, GSO, University of Rhode Island.
#@note Please report bugs/questions/comments to bijuthomas(at)mail(dot)uri(dot)edu.
#@date June 13, 2014.


class POMException(Exception):
    """!The base class of all exceptions relating to the POM"""

class UnexpectedPOMFailureTest(POMException):
    """!An exception raised during failure testing to represent a
    situation that should abort the entire workflow."""

class ExpectedPOMFailureTest(POMException):
    """!An exception raised during failure testing to represent a
    situation that should cause the workflow to disable ocean
    coupling    """

class POMInputError(POMException):

    """!Raised when a required input or input directory did not exist."""

class POMInitFailed(POMException):
    """!Raised when a POM initialization program unexpectedly fails."""

class POMSSTError(POMException):
    """!Raised when the init has trouble extracting SSTs."""

class POMPrepError(POMException):
    """!Raised when the POM prep fails."""

class POMConfigError(POMException):
    """!Raised when an impossible configuration is requested, such as
    an unsupported tropical basin."""

class POMUnsupportedBasin(POMConfigError):
    """!Raised when an unsupported basin is requested."""
