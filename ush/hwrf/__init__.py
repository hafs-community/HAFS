##@namespace hwrf
# Defines classes that know how to run all parts of the HWRF system.
# 
# @anchor hwrf_overview
# The hwrf package implements the atmospheric and coupling aspects of
# the HWRF system.  It provides many classes that perform
# initialization, forecast, post-processing and data delivery.
# Broadly speaking, these are divided into three groups: high-level
# packages that actually run parts of the system, packages that
# describe the work to be done by the HWRF system, and low-level
# packages that provide simple utilities and abstract base classes.
# 
# @section highlevel High-Level Packages
#
# The packages in this section implement the high-level logic of the
# HWRF system, such as running GSI or WRF, or copying files to COM.
# By and large, these packages define hwrf.hwrftask.HWRFTask
# subclasses.  The batch jobs or scripts then call the run() function
# on the task of interest to tell it to do its job.  Later tasks in
# the workflow obtain data from earlier tasks by calling the
# products() iterator on those tasks.
#
# Relevant modules, in the approximate order in which they're used in
# the workflow, are:
# * hwrf.launcher --- creates the initial HWRF working directories and
#   important files such as the database, configuration, holdvars, 
#   and storm information.
# * hwrf.input --- obtains input data from disk, FTP, SSH or tape to meet
#   the input data requirements given by each tasks' inputiter() iterator.
# * hwrf.wps --- runs the WRF Pre-Processing System (WPS) on parent model
#   data to produce inputs to the real_nmm and wrf programs.
# * hwrf.prep --- runs the prep_hybrid program on parent model spectral
#   data to produce inputs to the real_nmm
# * hwrf.relocate --- relocates, resizes and modifies the intensity of
#   the tropical cyclone vortex.  
# * hwrf.bufrprep --- converts data dumps to bufr files for input to GSI
# * hwrf.gsi --- runs the GSI data assimilation system
# * hwrf.fcsttask --- runs the atmosphere-only WRF and real_nmm, including
#   short simulations for generating wrfanl files, six hour analysis cycle
#   simulations and the full 126hr forecast job.  
# * hwrf.mpipomtc --- interfaces with the pom package to run the POM
#   ocean model initialization and run the POM-coupled WRF forecast
# * hwrf.post -- runs the HWRF post to convert model output to GRIB files
# * hwrf.gsipost -- a wrapper around hwrf.post that handles inputs and
#   outputs to GSI, assisting in GSI diagnostics.
# * hwrf.gribtask --- runs copygb, wgrib, grbindex and similar programs
#   to manipulate GRIB files and copy them to their final destination
# * hwrf.copywrf --- copies WRF input and output data to some destination
# * hwrf.nhc_products --- NHC-specific product creation and delivery
# * hwrf.tracker --- runs the GFDL vortex tracker
# * hwrf.ensda --- data assimilation ensemble
# * hwrf.rocoto -- utilities to interface between HWRF and the Rocoto
#   workflow automation system
#
# @section describers Packages that Describe the HWRF System
#
# Some packages in the HWRF system only describe the workflow and how
# it is to be executed.  These modules allow for complex querying and
# modification of the work before the work is actually started.  This
# way, tasks know in detail the inputs and outputs to all other tasks
# in the workflow.
#
# These modules include:
# * hwrf.wrf, hwrf.wrfbase --- describe a WRF simulation, generates
#   the WRF namelist from the HWRF configuration files, predicts input
#   and output filenames based on namelist settings
# * hwrf.regrib --- describes regribbing operations and has most of
#   the implementation of those operations.  This is used by
#   hwrf.gribtask to do the actual regribbing.
# * hwrf.ensda --- contains classes to describe a two-dimensional
#   ensemble-vs-time array of tasks that represent the steps of an
#   ENKF or hybrid ENKF data assimilation system.  Also has wrappers
#   around many of the high-level modules to create the GFS ENKF-based
#   HWRF DA ensemble, hwrf.ensda.FromGFSENKF.
# * hwrf.hwrfsystem --- a wrapper around many of the high-level modules that 
#   simplifies the definition of the HWRF post-processing and data
#   delivery.
# * hwrf.init --- a wrapper around many of the high-level modules that
#   combines objects in complex ways to create the HWRF initialization
#   system.  
#
# @section lowlevel Low-Level Logic Modules
#
# Several of the modules in the hwrf package provide utilities or
# abstract base classes that simplify the higher-level modules
# described above.  
#
# * hwrf.config, hwrf.hwrftask --- allows for configuration of the
#   HWRF system using UNIX Conf files.  Implements some functionality
#   common to all, or nearly all, HWRF tasks.  This includes product
#   listing, directory and executable specification, scrubbing
#   settings, input data requirements and others.
# * hwrf.exceptions --- exception classes thrown by the HWRF module.
#   All exceptions defined in the hwrf package that can leave an hwrf
#   module are defined here to avoid cyclic dependencies in the import
#   statements.  This allows one to just do "from hwrf.exceptions
#   import *" to get all HWRF-specific exceptions.
# * hwrf.constants --- constant values used in the HWRF system
# * hwrf.numerics --- time and date manipulation and other numerical
#   routines used throughout the HWRF system.
# * hwrf.prelaunch --- utilities for changing the HWRF configuration
#   before the hwrf.launcher completes.  This allows per-cycle
#   configuration changes, such as only running a 12hr forecast for 6Z
#   and 18Z cycles.
# * hwrf.storminfo --- parsing of ATCF, message and tcvitals files, which
#   specify storm information.
# * hwrf.revital --- complex manipulations of tcvitals data
