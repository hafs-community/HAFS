#!/bin/bash

set -xe

subset="${1:-ghrsst_subset.nc}"
merged=ghrsst_merged.nc

if ( ! which cdo ) ; then
    echo "The \"cdo\" command isn't in your path! Go find it and rerun this job." 1>&2
    exit 1
fi

if ( ! which ncks ) ; then
    echo "The \"ncks\" command from the NetCDF Data Operators (nco) is not in your path! Go find the nco and rerun this job." 1>&2
    exit 1
fi

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS} 
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}

# Start & end times are at day precision, not hour
m1date=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 -24 hours" +%Y%m%d )
p1date=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 +$(( NHRS+24 )) hours" +%Y%m%d )
now=$m1date
end=$p1date

set +x
echo "Generating ESMF mesh from GHRSST files."
echo "Running in dir \"$PWD\""
echo "Using files from \"$DOCNdir\""
echo "GHRSST Date range is $now to $end"
set -x

# Generate the filenames.
missing=''
mergefiles=''
itry=0
itrytoohard=99 # infinite loop guard
while (( now <= end && itry <= itrytoohard )) ; do
    infile="$DOCNdir/JPL-L4_GHRSST-SSTfnd-MUR-GLOB-${now:0:8}.nc"
    if [[ ! -s "$infile" || ! -r "$infile" ]] ; then
        echo "GHRSST input file is missing: $infile" 2>&1
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
echo "GHRSST input files are:"
for f in $mergefiles ; do
    echo "  - $f"
done
echo "Will merge ghrsst files into $merged"
       echo "Will subset data over HAFS region into $subset"
set -x

# Merge all ghrsst files into one, as expected by hafs_esmf_mesh.py 
cdo mergetime $mergefiles "$merged"
test -s "$merged"
test -r "$merged"

# Subset data over HAFS region (lat, 250 to 355 and lon, 0 to 50)   
ncks -O -d lon,6999,17499 -d lat,8999,13999 "$merged" "$subset"
test -s "$subset"
test -r "$subset"

# Rejoice.
set +x
echo "Successfully merged GHRSST files and subsetted area."
