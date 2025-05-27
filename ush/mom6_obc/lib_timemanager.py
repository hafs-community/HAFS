#! /usr/bin/env python3
################################################################################
# Script Name: lib_timemanager.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This module contains a class of timeobject for MOM6 Open Boundary Condtion.
# History:
#   05/13/2023: Initial version for HAFSv1 operational implementation
#   02/02/2024: Clean up for MOM6 OBC input files
################################################################################


class timeobject(object):

    def __init__(self, value=0):
        self.data = value
        self.units = ''
        self.calendar = ''
