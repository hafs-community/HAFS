#!/bin/sh
################################################################################
# Script Name: hafs_ghrsst_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script prepares GHRSST mesh and input files for the DOCN component.
################################################################################
set -xe

set -u
output_path="$1"
set +u

if ( ! which cdo ); then
    echo "The \"cdo\" command isn't in your path! Go find it and rerun this job." 1>&2
    exit 1
fi

if ( ! which ncks ); then
    echo "The \"ncks\" command from the NetCDF Data Operators (nco) is not in your path! Go find the nco and rerun this job." 1>&2
    exit 1
fi

HOMEhafs=${HOMEhafs:?}
WORKhafs=${WORKhafs:?}
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
usefiles=''
itime=0
infinity=9999 # infinite loop guard
while (( now <= end && itime < infinity )); do
    infile="$DOCNdir/JPL-L4_GHRSST-SSTfnd-MUR-GLOB-${now:0:8}.nc"
    if [[ ! -s "$infile" || ! -r "$infile" ]]; then
        echo "GHRSST input file is missing: $infile" 2>&1
        missing="$missing $infile"
    else
        usefiles="$mergefiles $infile"

        # Discard all vars except what we need; convert to NetCDF3:
        rm -f vars.nc
        ncks -v time,lat,lon,analysed_sst -6 "$infile" vars.nc

        # Subset data over HAFS region (lat, 250 to 355 and lon, 0 to 50)
        rm -f subset.nc
        cdo -sellonlatbox,-118,-5,-15.0,60.0 vars.nc subset.nc

        # Convert temperature units:
		aos_old=$(ncdump -c subset.nc | grep add_offset | grep analysed_sst | awk '{print $3}')
        aos_new=$(echo "scale=3; $aos_old-273.15" | bc)
        ncatted -O -a add_offset,analysed_sst,o,f,"$aos_new" subset.nc

        outfile=$( printf "%s/DOCN_input_%05d.nc" "$output_path" $itime )
        $USHhafs/produtil_deliver.py -m subset.nc "$outfile"
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
    echo "You are missing some GHRSST input files!"
    for infile in $missing; do
        echo "  missing: $infile"
    done
    echo " -> SCRIPT IS ABORTING BECAUSE INPUT FILES ARE MISSING <- "
    exit 1
fi

#ncks -O -d lon,6999,17499 -d lat,8999,13999 "$merged" ghrsst_v1.nc

# Rejoice.
set +x
echo "Successfully subsetted and corrected units of GHRSST files."
echo "Please enjoy your files and have a nice day."
