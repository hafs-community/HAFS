#! /usr/bin/env python3
################################################################################
# Script Name: __init__.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   The script defines classes that know HAFS Python scripts.
# History:
#   04/10/2019: Initial version for HAFS application (adapted from HWRF)
################################################################################

##@namespace hafs
# Defines classes that know how to run all parts of the HAFS system.
#
# @anchor hafs_overview
#
# @section highlevel High-Level Packages
#
# Relevant modules, in the approximate order in which they're used in
# the workflow, are:
# * hafs.launcher --- creates the initial HAFS working directories and
#   important files such as the database, configuration, holdvars,
#   and storm information.
# * hafs.input --- obtains input data from disk, FTP, SSH or tape to meet
#   the input data requirements given by each tasks' inputiter() iterator.
#
# @section lowlevel Low-Level Logic Modules
#
# * hafs.exceptions --- exception classes thrown by the HAFS module.
#   All exceptions defined in the hafs package that can leave an hafs
#   module are defined here to avoid cyclic dependencies in the import
#   statements.  This allows one to just do "from hafs.exceptions
#   import *" to get all HAFS-specific exceptions.
# * hafs.prelaunch --- utilities for changing the HAFS configuration
#   before the hafs.launcher completes.  This allows per-cycle
#   configuration changes, such as only running a 12hr forecast for 6Z
#   and 18Z cycles.
