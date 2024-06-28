#!/bin/sh
################################################################################
# Script Name: hafs_era5_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script prepares and links ERA5 files for CDEPS DATM component.
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

set +x
echo "Linking ERA5 files."
echo "Running in dir \"$PWD\""
echo "Will link ERA5 files into $output_path"
echo "ERA5 Date range is $now to $end"
set -x

rm -f DATM_input* merged.nc

# Generate the filenames.
usefiles=''
missing=''
itime=0
infinity=9999 # infinite loop guard
while (( now <= end && itime < infinity )); do
    infile="$DATMdir/ERA5_${now:0:8}.nc"
    if [[ ! -s "$infile" || ! -r "$infile" ]]; then
        echo "ERA5 input file is missing: $infile" 2>&1
        missing="$missing $infile"
    else
        usefiles="$usefiles $infile"
        outfile=$( printf "%s/DATM_input_%05d.nc" "$output_path" $itime )
        ${NLN} "$infile" "$outfile"
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
    echo "You are missing some ERA5 input files!"
    for infile in $missing; do
        echo "  missing: $infile"
    done
    echo " -> SCRIPT IS ABORTING BECAUSE INPUT FILES ARE MISSING <- "
    exit 1
fi

set +x
echo "ERA5 input files are:"
for f in $usefiles; do
    echo "  - $f"
done
set -x

# Rejoice.
set +x
echo "Successfully linked ERA5 files at $output_path"
echo "Please enjoy your files and have a nice day."
