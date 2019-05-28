#! /bin/sh --login
set -x -u -e
date
. $USHhafs/hafs_pre_job.sh.inc
exec "$@"
