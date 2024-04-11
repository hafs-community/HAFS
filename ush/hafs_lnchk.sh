#!/bin/sh
################################################################################
# Script Name: hafs_lnchk.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script is a wrapper for the "ln -sf" command. It creates a symbolic
#   link together with some additional broken link checking capabilities.
################################################################################

if [ $# -ne 3 ]; then
   echo "This script is a wrapper for the 'ln -sf' command, it creates a symbolic link"
   echo "together with some additional broken link checking capabilities."
   echo "It requires three arguments: a check option, the target, and the symlink."
   echo "Usage: $@ -[r|e|w|d] target symlink"
   echo "  -r: regular symbolic link, same as ln -sf, no broken symbolic link check"
   echo "  -e: error exit with FATAL ERROR message if the symbolic link is broken"
   echo "  -w: print a WARNING message if the symbolic link is broken"
   echo "  -d: print a WARNING message and delelete the symbolic link if it is broken"
   exit 1
fi

if [ "$3" = '.' -o "$3" = './' ]; then
   symlink=${PWD:?}/$(basename $2)
elif [ -d $3 ]; then
   symlink=${3%/}/$(basename $2)
else
   symlink=$3
fi

ln -sf $2 ${symlink}

if [ "$1" = "-r" ]; then
  # No checks, same as ln -sf
  exit
elif [ "$1" = "-w" ]; then
  if [ ! -e ${symlink} ]; then
    echo "WARNING: Broken symbolic link: ${symlink} => $2."
  fi
elif [ "$1" = "-d" ]; then
  if [ ! -e ${symlink} ]; then
    echo "WARNING: Broken symbolic link: ${symlink} => $2. Delete the symbolic link."
    if [ -L ${symlink} ]; then
      rm ${symlink}
    fi
  fi
elif [ "$1" = "-e" ]; then
  if [ ! -e ${symlink} ]; then
    echo "FATAL ERROR: Broken symbolic link: ${symlink} => $2. Exiting."
    exit 1
  fi
else
  echo "FATAL ERROR: Unknown command line option: $1."
  exit 1
fi
