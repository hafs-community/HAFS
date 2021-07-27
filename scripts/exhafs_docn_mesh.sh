#!/bin/bash

if [[ "$run_docn" != yes ]] ; then
    echo "This job should only be run if \$run_docn=yes"
    echo "  \$run_docn=\"$run_docn\""
    echo "Beware! You may anger Poseidon by misusing this script. Avoid coastlines."
    echo " -> SCRIPT IS EXITING BECAUSE THIS JOB SHOULD NOT BE RUN <- "
    exit 0
fi  

set -xe

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}
APRUNS=${APRUNS:-"aprun -b -j1 -n1 -N1 -d1 -cc depth"}

merged=merged.nc
ofile=ofile.nc
mesh_ocn="$mesh_ocn"
mesh_dir=$( dirname "$mesh_ocn" )
docn_source=${DOCN_SOURCE:-OISST}

[ -d "$mesh_dir" ] || mkdir "$mesh_dir"   
test -e "$ofile" -o -L "$ofile"  && rm -f "$ofile"

if [[ "$make_mesh_ocn" == yes ]] ; then
    rm -f "$mesh_ocn"
fi

if [[ "$docn_source" == OISST ]] ; then   
    "$USHhafs/hafs_oisst_prep_mesh.sh" "$merged"
elif [[ "${docn_source}" == RTOFS ]] ; then
    "$USHhafs/hafs_rtofs_prep_mesh.sh" "$merged"
elif [[ "${docn_source}" == GHRSST ]] ; then
    "$USHhafs/hafs_ghrsst_prep_mesh.sh" "$merged"
else
    echo "ERROR: Unknown data ocean source $docn_source. Giving up." 2>&1
    echo " -> SCRIPT IS FAILING BECAUSE OF INVALID \$DOCN_SOURCE VALUE <- "
    exit 1
fi

test -s "$merged"
test -r "$merged"
$USHhafs/produtil_deliver.py -m "$merged" "$merged_docn_input"
test -s "$merged_docn_input"
test -r "$merged_docn_input"

if [[ "$make_mesh_ocn" != yes ]] ; then
    set +x
    echo "Delivered merged file to $merged"
    echo "Will use a premade mesh."
    echo "Please enjoy your merged files and have a nice day."
    set -x
    exit 0
fi

set +x
echo "Subprocess successfully merged ocean input files."
echo "Will now generate mesh in \"$ofile\""
echo "Will deliver to \"$mesh_ocn\""
set -x

# Generate the mesh from the merged file. 
if [[ "$docn_source" == OISST ]] ; then   
    $APRUNS $USHhafs/hafs_esmf_mesh.py --ifile "$merged" --ofile "$ofile" \
        --maskvar sst --maskcal --double --overwrite
elif [[ "${docn_source}" == RTOFS ]] ; then
    $APRUNS $USHhafs/hafs_esmf_mesh.py --ifile "$merged" --ofile "$ofile" \
        --overwrite --latvar Latitude --lonvar Longitude \
        --maskvar sst --maskcal —double   
elif [[ "${docn_source}" == GHRSST ]] ; then
    $APRUNS $USHhafs/hafs_esmf_mesh.py --ifile "$merged" --ofile "$ofile" \
        --maskvar analysed_sst --maskcal --overwrite --double
fi
test -s "$ofile"

# Copy mesh and merged file to final destinations.
$USHhafs/produtil_deliver.py -m "$ofile" "$mesh_ocn"
test -s "$mesh_ocn"

ls -l "$mesh_ocn"

# Rejoice.
set +x
echo "DOCN mesh was successfully generated."
echo "Enjoy your mesh and have a nice day."
