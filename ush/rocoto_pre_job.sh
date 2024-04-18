#!/bin/sh
################################################################################
# Script Name: rocoto_pre_job.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script sources hafs_pre_job.sh.inc and then execute the script
#   specified through the input augument(s).
################################################################################
set -x
date
. $USHhafs/hafs_pre_job.sh.inc
exec "$@"
