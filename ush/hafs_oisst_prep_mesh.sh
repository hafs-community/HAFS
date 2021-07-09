#!/bin/bash

set -xe

nozlev="${1:-oisst-avhrr-v02r01.merged_nozlev.nc}"

if ( ! which cdo ) ; then
    echo "The \"cdo\" command isn't in your path! Go find it and rerun this job." 1>&2
    exit 1
fi  

if ( ! which ncwa ) ; then
    echo "The \"ncwa\" command from the NetCDF Data Operators (nco) is not in your path! Go find the nco and rerun this job." 1>&2
    exit 1
fi  

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}

merged=oisst-avhrr-v02r01.merged.nc
mesh_ocn="$mesh_ocn"
mesh_dir=$( dirname "$mesh_ocn" )

# Start & end times are at day precision, not hour
m1date=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 -24 hours" +%Y%m%d )
p1date=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 +$(( NHRS+24 )) hours" +%Y%m%d )
now=$m1date
end=$p1date

set +x
echo "Generating ESMF mesh from OISST files."
echo "Running in dir \"$PWD\""
echo "OISST Date range is $now to $end" 
set -x

# Generate the filenames.
mergefiles=''
missing=''
itry=0
itrytoohard=99 # infinite loop guard
while (( now <= end && itry <= itrytoohard )) ; do
    infile="$DOCNdir/oisst-avhrr-v02r01.${now:0:8}.nc"
    if [[ ! -s "$infile" || ! -r "$infile" ]] ; then
        echo "OISST input file is missing: $infile" 2>&1
        missing="$missing $infile"
    else
        mergefiles="$mergefiles $infile"
    fi
    now=$( date -d "${now:0:4}-${now:4:2}-${now:6:2}t00:00:00+00 +24 hours" +%Y%m%d )
    itry=$(( itry+1 ))
done
if (( itry > itrytoohard )) ; then
    echo "Infinite loop detected! The \"date\" command did not behave as expected. Aborting!" 1>&2
    exit 1
fi

if [[ "${missing:-}Q" != Q ]] ; then
    set +x
    echo "You are missing some GHRSST input files!"
    for infile in $missing ; do
        echo "  missing: $infile"
    done
    echo " -> SCRIPT IS ABORTING BECAUSE INPUT FILES ARE MISSING <- "
    exit 1
fi

set +x
echo "OISST input files are:"
for f in $mergefiles ; do
    echo "  - $f"
done
echo "Will merge oisst files into $merged"
echo "Will remove z levels into $nozlev"
set -x

# Merge all oisst files into one, as expected by hafs_esmf_mesh.py
cdo mergetime $mergefiles "$merged"
test -s "$merged"
test -r "$merged"

# Remove z dimension
ncwa -O -a zlev "$merged" "$nozlev"
test -s "$nozlev"
test -r "$nozlev"

# Rejoice.
set +x
echo "Successfully merged OISST files and removed z dimension."
