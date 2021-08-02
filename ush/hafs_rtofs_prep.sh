#!/bin/bash

set -xe

merged="${1:-merged.nc}"

for exe in cdo ncks ncrename ncap2 ; do
    if ( ! which "$exe" ) ; then
        echo "The \"$exe\" command isn't in your path! Go find it and rerun this job." 1>&2
        exit 1
    fi  
done

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}

mesh_ocn="$mesh_ocn"
mesh_dir=$( dirname "$mesh_ocn" )

# Start & end times are at day precision, not hour
m1date=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 -24 hours" +%Y%m%d )
p1date=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 +$(( NHRS+24 )) hours" +%Y%m%d )
now=$m1date
end=$p1date

set +x
echo "Generating ESMF mesh from RTOFS files."
echo "Running in dir \"$PWD\""
echo "Using files from \"$DOCNdir\""
echo "Running in dir \"$PWD\""
set -x

files_to_merge=''
missing=''

for fhour in $( seq 0 3 $NHRS ) ; do
    infile="$DOCNdir/rtofs_glo_2ds_${CDATE:0:8}_f"$( printf %03d $fhour ).nc
    if [[ ! -s "$infile" || ! -r "$infile" ]] ; then
        echo "RTOFS input file is missing: $infile" 2>&1
        missing="$missing $infile"
    else
        files_to_merge="$files_to_merge $infile"
    fi
done

if [[ "${missing:-}Q" != Q ]] ; then
    set +x
    echo "You are missing some RTOFS input files!"
    for infile in $missing ; do
        echo "  missing: $infile"
    done
    echo " -> SCRIPT IS ABORTING BECAUSE INPUT FILES ARE MISSING <- "
    exit 1
fi

set +x
echo "RTOFS input files are:"
for infile in $files_to_merge ; do
        echo "  - $infile"
done
echo "Will merge RTOFS files into $merged"
set -x

# Merge files
cdo mergetime $files_to_merge rtofs_glo_2ds_prog.nc

# Subset data over HAFS region (lat, 250 to 355.0732 and lon, 0 to 50.0108)
ncks -O -d X,2198,3511 -d Y,1504,2228 rtofs_glo_2ds_prog.nc rtofs_glo_2ds_prog_v1.nc

# Keep only required variables
ncks -O -v sst,Latitude,Longitude rtofs_glo_2ds_prog_v1.nc rtofs_glo_2ds_prog_v2.nc
rm -f rtofs_glo_2ds_prog_v1.nc
ncks -O -C -x -v Date rtofs_glo_2ds_prog_v2.nc rtofs_glo_2ds_prog_v3.nc
rm -f rtofs_glo_2ds_prog_v2.nc

# Rename variables to make them CDEPS compatible.
# We have a workaround in here because I once tried to change a
# variable name "MT" to "time" in the data, and it was set to the
# missing value.  This could be a bug in the NetCDF Operators, but the
# following code produces a correct file:
ncrename -d MT,time rtofs_glo_2ds_prog_v3.nc
ncap2 -O -v -s 'time=MT' rtofs_glo_2ds_prog_v3.nc rtofs_glo_2ds_prog_v4.nc
rm -f rtofs_glo_2ds_prog_v3.nc
ncks -A -C -v time rtofs_glo_2ds_prog_v4.nc rtofs_glo_2ds_prog_v5.nc
rm -f rtofs_glo_2ds_prog_v4.nc
ncks -x -v MT rtofs_glo_2ds_prog_v5.nc "$merged"
rm -f rtofs_glo_2ds_prog_v5.nc

# Rejoice.
set +x
echo "RTOFS files successfully merged." 
