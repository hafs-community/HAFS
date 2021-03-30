#!/bin/bash

if [[ "$make_mesh_ocn" != yes || "$run_docn" != yes ]] ; then
    echo "This job should only be run if \$run_docn and \$make_mesh_ocn are both yes."
    echo "  \$make_mesh_ocn=\"$make_mesh_ocn\""
    echo "  \$run_docn=\"$run_docn\""
    echo "Beware! You may anger Poseidon by misusing this script. Avoid coastlines."
    echo " -> SCRIPT IS EXITING BECAUSE THIS JOB SHOULD NOT BE RUN <- "
    exit 0
fi

if ( ! which cdo ) ; then
    echo "The \"cdo\" command isn't in your path! Go find it and rerun this job." 1>&2
    exit 1
fi

if ( ! which ncwa ) ; then
    echo "The \"ncwa\" command from the NetCDF Data Operators (nco) is not in your path! Go find the nco and rerun this job." 1>&2
    exit 1
fi

set -xe

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}
APRUNS=${APRUNS:-"aprun -b -j1 -n1 -N1 -d1 -cc depth"}

merged=oisst-avhrr-v02r01.merged.nc
nozlev=oisst-avhrr-v02r01.merged_nozlev.nc
ofile=ofile.nc
mesh_ocn="$mesh_ocn"
mesh_dir=$( dirname "$mesh_ocn" )

# Start & end times are at day precision, not hour
now=${CDATE:0:8}
end=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 +$NHRS hours" +%Y%m%d )

set +x
echo "Generating ESMF mesh from OISST files."
echo "Running in dir \"$PWD\""
echo "OISST Date range is $now to $end"
set -x

# Generate the filenames.
mergefiles=''
itry=0
itrytoohard=99 # infinite loop guard
while (( now <= end && itry <= itrytoohard )) ; do
    mergefiles="$mergefiles $DOCNdir/oisst-avhrr-v02r01.${now:0:8}.nc"
    now=$( date -d "${now:0:4}-${now:4:2}-${now:6:2}t00:00:00+00 +24 hours" +%Y%m%d )
    itry=$(( itry+1 ))
done
if (( itry > itrytoohard )) ; then
    echo "Infinite loop detected! The \"date\" command did not behave as expected. Aborting!" 1>&2
    exit 1
fi

set +x
echo "OISST input files are:"
for f in $mergefiles ; do
    echo "  - $f"
done
echo "Will merge oisst files into $merged"
echo "Will remove z levels into $nozlev"
echo "Temporary output mesh is $ofile"
echo "Will deliver to \"$mesh_ocn\""
set -x

# Merge all oisst files into one, as expected by hafs_esmf_mesh.py
cdo mergetime $mergefiles "$merged"
test -s "$merged"
test -r "$merged"

# Remove z dimension
ncwa -O -a zlev "$merged" "$nozlev"
test -s "$nozlev"
test -r "$nozlev"

[ -d "$mesh_dir" ] || mkdir "$mesh_dir"
rm -f "$mesh_ocn"
test -e "$ofile" -o -L "$ofile"  && rm -f "$ofile"

# Generate the mesh from the merged oisst file.
$APRUNS $USHhafs/hafs_esmf_mesh.py --ifile "$nozlev" --ofile "$ofile" \
    --maskvar sst --maskcal --double --overwrite
test -s "$ofile"

# Copy mesh to final destination.
$USHhafs/produtil_deliver.py -m "$ofile" "$mesh_ocn"
test -s "$mesh_ocn"

ls -l "$mesh_ocn"

# Rejoice.
set +x
echo "OISST files successfully merged."
echo "DOCN mesh was successfully generated."
echo "Enjoy your mesh and have a nice day."
