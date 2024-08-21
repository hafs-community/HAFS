#!/bin/sh
################################################################################
# Script Name: hafs_oisst_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script prepares OISST mesh and input files for the DOCN component.
################################################################################
set -xe

set -u
output_path="$1"
set +u

if ( ! which cdo ); then
    set +x
    echo "The \"cdo\" command isn't in your path! Go find it and rerun this job." 1>&2
    set -x
    exit 1
fi

if ( ! which ncwa ); then
    set +x
    echo "The \"ncwa\" command from the NetCDF Data Operators (nco) is not in your path! Go find the nco and rerun this job." 1>&2
    set -x
    exit 1
fi

HOMEhafs=${HOMEhafs:?}
WORKhafs=${WORKhafs:?}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}

set -u

# Start & end times are at day precision, not hour
m1date=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 -24 hours" +%Y%m%d )
p1date=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 +$(( NHRS+24 )) hours" +%Y%m%d )
now=$m1date
end=$p1date
merged=oisst-avhrr-v02r01.merged.nc
nozlev=oisst-avhrr-v02r01.nozlev.nc
outfile=$( printf "%s/DOCN_input_%05d.nc" "$output_path" 0 )

set +x
echo "Generating ESMF mesh from OISST files."
echo "Running in dir \"$PWD\""
echo "OISST Date range is $now to $end"
set -x

rm -f DOCN_input* merged.nc

# Generate the filenames.
usefiles=''
missing=''
itime=0
infinity=9999 # infinite loop guard
while (( now <= end && itime < infinity )); do
    infile="$DOCNdir/oisst-avhrr-v02r01.${now:0:8}.nc"
    if [[ ! -s "$infile" || ! -r "$infile" ]]; then
        echo "OISST input file is missing: $infile" 2>&1
        missing="$missing $infile"
    else
        usefiles="$usefiles $infile"
    fi
    now=$( date -d "${now:0:4}-${now:4:2}-${now:6:2}t00:00:00+00 +24 hours" +%Y%m%d )
    itime=$(( itime+1 ))
done
if (( itime >= infinity )); then
    echo "Infinite loop detected! The \"date\" command did not behave as expected. Aborting!" 1>&2
    exit 1
fi

if [[ "${missing:-}Q" != Q ]]; then
    set +x
    echo "You are missing some OISST input files!"
    for infile in $missing; do
        echo "  missing: $infile"
    done
    echo " -> SCRIPT IS ABORTING BECAUSE INPUT FILES ARE MISSING <- "
    exit 1
fi

set +x
echo "OISST input files are:"
for f in $usefiles; do
    echo "  - $f"
done
echo "Will merge oisst files into $merged"
echo "Will remove z levels into $nozlev"
set -x

# Merge all oisst files into one, as expected by hafs_esmf_mesh.py
cdo mergetime $usefiles "$merged"
test -s "$merged"
test -r "$merged"

# Remove z dimension
ncwa -O -a zlev "$merged" "$nozlev"
test -s "$nozlev"
test -r "$nozlev"

# Deliver to intercom:
$USHhafs/produtil_deliver.py -m "$nozlev" "$outfile"

# Rejoice.
set +x
echo "Successfully merged OISST files and removed z dimension."
echo "Merged file is at: $outfile"
echo "Please enjoy your files and have a nice day."
