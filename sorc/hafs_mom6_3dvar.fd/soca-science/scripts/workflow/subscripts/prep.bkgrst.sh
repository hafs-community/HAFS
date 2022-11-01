#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# prep.bkg_rst.sh
#   Prepare a background restart file. Either by linking from a previous cycle
#   in the current experiment, from an external BKRST_DIR, or by performing a
#   coldstart from outside ICs
#================================================================================

EOF

# default environment variable values
BKGRST_COLDSTART_ENABLED=${BKGRST_COLDSTART_ENABLED:-F}
BKGRST_SRC=${BKGRST_SRC:-$EXP_DIR/rst}
BKGRST_SRC_ARCHIVE=${BKGRST_SRC_ARCHIVE:-%Y%m%d%H}

# Required environment variables:
envars=()
envars+=("DATE")
envars+=("BKGRST_SRC")
envars+=("BKGRST_SRC_ARCHIVE")
envars+=("BKGRST_COLDSTART_ENABLED")
envars+=("BKGRST_DIR")
envars+=("SUBSCRIPTS_DIR")
envars+=("WORK_DIR")

# make sure required env vars exist
set +u
for v in ${envars[@]}; do
    if [[ -z "${!v}" ]]; then
        echo "ERROR: env var $v is not set."; exit 1
    fi
    printf "%-25s %s\n" " $v " "${!v}"
done
set -u
echo ""

# subsitute date in certain variables
BKGRST_SRC=$(date -ud "$DATE" +"$BKGRST_SRC")
BKGRST_SRC_ARCHIVE=$(date -ud "$DATE" +"$BKGRST_SRC_ARCHIVE")

ymdh=$(date -ud "$DATE" +%Y%m%d%H )

# 1) Does a background already exist? If so exit early
if [[ -d "$BKGRST_DIR"  &&  $(ls $BKGRST_DIR/*.nc -1q 2>/dev/null | wc -l) -gt 0 ]]; then
    echo "Background already exists."
    echo " $BKGRST_DIR"
    exit 0
fi
rm -rf $BKGRST_DIR

# 2) if BKGRST_SRC points to a tar/zip file
if [[ "$BKGRST_SRC" =~ .*\.zip$ && -f "$BKGRST_SRC" ]]; then
    echo "Found an archive file: $BKGRST_SRC"
    mkdir -p archive
    (cd archive && unzip -o $BKGRST_SRC $BKGRST_SRC_ARCHIVE/*) || exit 1
    mkdir -p $BKGRST_DIR
    ln -s $(readlink -f archive/$BKGRST_SRC_ARCHIVE/*) $BKGRST_DIR/
    exit 0
fi

# 3) otherwise, link to another already existing location
#  can either be the exact path to the folder containing the restart files,
#  or the general path to the entire set of restart file dates
rst_src_dir=""
for ext in "" "/$ymdh" "/${ymdh:0:4}/$ymdh"; do
    for d in "$BKGRST_SRC$ext" "$BKGRST_SRC$ext/ctrl" "$BKGRST_SRC/ctrl$ext"; do
        echo "searching $d"
        if [[ -d $d && $(ls $d/*.nc -1q 2>/dev/null | wc -l) -gt 0 ]]; then
            rst_src_dir=$d
            break
        fi
    done
    if [[ "$rst_src_dir" != "" ]]; then
        echo "linking files from $rst_src_dir"
        mkdir -p $BKGRST_DIR
        ( cd $BKGRST_DIR; ln -s $rst_src_dir/* .)
        exit 0
    fi
done

# 4) otherwise, can we do a coldstart?
if [[ "$BKGRST_COLDSTART_ENABLED" =~ [yYtT1] ]]; then
    echo "Generating background from coldstart..."
    (
        export WORK_DIR=$WORK_DIR/coldstart
        $SUBSCRIPTS_DIR/prep.bkgrst.coldstart.sh
    )
    exit 0
fi

# 5) all of the above has failed, die a horrible death
echo "ERROR: unable to find a suitable set of background files"
exit 1
