#!/bin/bash

if [[ "$make_mesh_atm" != yes || "$run_datm" != yes ]] ; then
    echo "This job should only be run if \$run_datm and \$make_mesh_atm are both yes."
    echo "  \$make_mesh_atm=\"$make_mesh_atm\""
    echo "  \$run_datm=\"$run_datm\""
    echo "Billions of electrons have whizzed by, the universe's entropy has increased, and yet nothing has been accomplished."
    echo " -> SCRIPT IS EXITING BECAUSE THIS JOB SHOULD NOT BE RUN <- "
    exit 0
fi

set -xe

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}
APRUNS=${APRUNS:-"aprun -b -j1 -n1 -N1 -d1 -cc depth"}

ofile=ofile.nc
mesh_atm="$mesh_atm"
mesh_dir=$( dirname "$mesh_atm" )

# Pick an input file. This will get the newest one, if the names are
# identical except for YYYYMMDD date string:
test -d "$DATMdir"
#ifile=$( ls -1 "$DATMdir"/*nc | sort -ur | head -1 )
ifile="$DATMdir/ERA5_${CDATE:0:8}.nc"

test -s "$ifile"
test -r "$ifile"

[ -d "$mesh_dir" ] || mkdir "$mesh_dir"
rm -f "$mesh_atm"
[ -e "$ofile" -o -L "$ofile" ]] && rm -f "$ofile"

set +x
echo "Generating ESMF mesh from $datm_source files."
echo "Running in dir \"$PWD\""
echo "ERA5 grid generation input file is \"$ifile\""
echo "Temporary output mesh is $ofile"
echo "Will deliver to \"$mesh_atm\""
set -x

# Generate the mesh from the merged file:
datm_source=${DATM_SOURCE:-ERA5}
if [[ "$datm_source" == ERA5 ]] ; then
    $APRUNS $USHhafs/hafs_esmf_mesh.py --ifile "$ifile" --ofile "$ofile" \
        --overwrite --latvar latitude --lonvar longitude --double
else
    echo "ERROR: Unknown data atmosphere source $datm_source. Giving up." 2>&1
    echo " -> SCRIPT IS FAILING BECAUSE OF INVALID \$DATM_SOURCE VALUE <- "
    exit 1
fi
test -s "$ofile"

# Copy mesh to final destination.
$USHhafs/produtil_deliver.py -m "$ofile" "$mesh_atm"
test -s "$mesh_atm"

ls -l "$mesh_atm"

# Rejoice.
set +x
echo "DATM $datm_source mesh was successfully generated."
echo "Enjoy your mesh and have a nice day."
