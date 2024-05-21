#!/bin/sh
################################################################################
# Script Name: exhafs_datm_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the data atmosphere component needed preprocessing steps.
################################################################################
set -xe

if [[ "$run_datm" != yes ]]; then
  echo "This job should only be run if \$run_datm is yes."
  echo "  \$run_datm=\"$run_datm\""
  echo "Billions of electrons have whizzed by, the universe's entropy has increased, and yet nothing has been accomplished."
  echo " -> SCRIPT IS EXITING BECAUSE THIS JOB SHOULD NOT BE RUN <- "
  exit 0
fi

HOMEhafs=${HOMEhafs:?}
WORKhafs=${WORKhafs:?}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}

ifile=$datm_input_path/DATM_input_00000.nc
ofile=ofile.nc
mesh_atm="$mesh_atm"
mesh_dir=$( dirname "$mesh_atm" )
datm_source=${DATM_SOURCE:-ERA5}

mkdir -p "$docn_input_path"
mkdir -p "$mesh_dir"
rm -f "$ofile"

if [[ "$make_mesh_ocn" == yes ]]; then
  rm -f "$mesh_ocn"
fi

if [[ "$datm_source" == ERA5 ]]; then
  $APRUNS "$USHhafs/cdeps_utils/hafs_era5_prep.sh" "$datm_input_path"
else
  echo "ERROR: Unknown data atmosphere source $datm_source. Giving up." 2>&1
  echo " -> SCRIPT IS FAILING BECAUSE OF INVALID \$DATM_SOURCE VALUE <- "
  exit 1
fi

if [[ "$make_mesh_atm" != yes ]]; then
  set +x
  echo "Processed atmosphere files are in $datm_input_path"
  echo "Will use a premade mesh."
  echo "Please enjoy your files and have a nice day."
  set -x
  exit 0
fi

set +x
echo "Generating ESMF mesh from $datm_source files."
echo "Running in dir \"$PWD\""
set -x

test -s "$ifile"
test -r "$ifile"

set +x
echo "Grid generation $datm_source input file is \"$ifile\""
echo "Temporary output mesh is $ofile"
echo "Will deliver to \"$mesh_atm\""
set -x

# Generate the mesh.
if [[ "$datm_source" == ERA5 ]]; then
  $APRUNS $USHhafs/cdeps_utils/hafs_esmf_mesh.py --ifile "$ifile" --ofile "$ofile" \
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
