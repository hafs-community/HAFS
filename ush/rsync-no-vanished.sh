#!/bin/sh
################################################################################
# Script Name: rsync-no-vanished.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script provides a workaround for rsync while making special treatments
#   for vanished files.
################################################################################
(rsync "$@"; if [ $? == 24 ]; then exit 0; else exit $?; fi) 2>&1 \
	    | grep -v 'vanished'
