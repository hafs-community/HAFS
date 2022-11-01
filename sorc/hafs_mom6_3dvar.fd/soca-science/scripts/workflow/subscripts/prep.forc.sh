#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# prep.forc.sh
#   Prepare forcing files, either by linking to an already prepared database
#   at FORC_DIR, or by downloading and processing new files from the interwebs
#================================================================================

EOF

# environment variable defaults
FORC_GEN_ENABLED=${FORC_GEN_ENABLED:-F}
FORC_GEN_DIR=${FORC_GEN_DIR:-$EXP_DIR/forc/%Y/%Y%m%d}
[[ $FORC_GEN_ENABLED =~ [fFnN0] ]] &&
  FORC_GEN_SOURCE=${FORC_GEN_SOURCE:-}
FORC_SRC=${FORC_SRC:-$EXP_DIR/forc}
FORC_SRC_ARCHIVE=${FORC_SRC_ARCHIVE:-%Y/%Y%m%d}

# Required environment variables:
envars=()
envars+=("DATE")
envars+=("FCST_LEN")
envars+=("FORC_DIR")
envars+=("FORC_GEN_ENABLED")
envars+=("FORC_GEN_DIR")
envars+=("FORC_GEN_SOURCE")
envars+=("FORC_SCRIPTS_DIR")
envars+=("FORC_SRC")
envars+=("FORC_SRC_ARCHIVE")
envars+=("WORK_DIR")
envars+=("EXP_DIR")
envars+=("FREE_FCST")

# make sure required env vars exist
set +u
for v in ${envars[@]}; do
    if [[ -z "${!v+x}" ]]; then
        echo "ERROR: env var $v is not set."; exit 1
    fi
    printf "%-25s %s\n" " $v " "${!v}"
done
set -u
echo ""

# subsitute date in certain variables
FORC_SRC=$(date -ud "$DATE" +"$FORC_SRC" )
FORC_SRC_ARCHIVE=$(date -ud "$DATE" +"$FORC_SRC_ARCHIVE" )
FORC_GEN_DIR=$(date -ud "$DATE" +"$FORC_GEN_DIR" )

#file name we are looking for
ymd=$(date -ud "$DATE" +"%Y%m%d")
atm_file=atm_$ymd.nc

# Free forecast uses only one atm.nc
if [[ "$FREE_FCST" =~ [yYtT1] ]]; then
  ln -sf $EXP_DIR/FORC $FORC_DIR
  exit 0
fi

# 1) Do the forcing files already exist? If so exit early
if [[ -f "$FORC_DIR/$atm_file" ]]; then
    echo "File already exists."
    echo " $FORC_DIR/$atm_file"
    rm -rf $FORC_DIR
    #exit 0
fi
#rm -rf $FORC_DIR


# 2) if FORC_SRC points to a tar/zip file, extract from there
if [[ "$FORC_SRC" =~ .*\.zip$ && -f "$FORC_SRC" ]]; then
    echo "Found an archive file to search: $FORC_SRC"
    mkdir -p archive
    (cd archive && unzip -o $FORC_SRC $FORC_SRC_ARCHIVE/* ) || exit 1
    ln -s $(readlink -f archive/$FORC_SRC_ARCHIVE) $FORC_DIR
    [[ -f $FORC_DIR/$atm_file ]] && exit 0
fi

# 3) otherwise, search the $FORC_SRC list and find first available file to
# link to
forc_src_dir=""
for ext in "" "/$ymd" "/${ymd:0:4}/$ymd"; do
    d=${FORC_SRC}${ext}
    echo "searching $d"

    if [[ -d $d && -f $d/$atm_file ]]; then
        echo "Forcing file found at $d"
        forc_src_dir=$d
        break
    fi
done
if [[ "$forc_src_dir" != "" ]]; then
    mkdir -p $FORC_DIR
    ln -s $forc_src_dir/* $FORC_DIR/.
    exit 0
fi

if [[ ! "$FORC_GEN_ENABLED" =~ [yYtT1] ]]; then
    printf "ERROR: Cannot find forcing files, set \$FORC_GEN_ENABLED=T or "
    printf "check \$FORC_SRC_DIR \n\n"
    exit 1
fi


# 4) otherwise, generate from files on the internet
#mkdir -p $FORC_GEN_DIR
echo ""
echo "Generating forcing file from internet."
$FORC_SCRIPTS_DIR/forc_$FORC_GEN_SOURCE.py "$DATE" -o $atm_file -t $(pwd)
mkdir -p $FORC_GEN_DIR
mv $atm_file $FORC_GEN_DIR
ln -s $(readlink -f $FORC_GEN_DIR) $FORC_DIR
