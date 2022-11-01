#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# post.cleanup.sh
#   Perform any cleanup actions at the end of a DA cycle (Currently consisting
#   of just deleting unneeded restart dates)
#================================================================================

EOF

# default values
SAVE_RST_CYCLES=${SAVE_RST_CYCLES:-1}
SAVE_RST_REGEX=${SAVE_RST_REGEX:- }

# Required environment variables:
envars=()
envars+=("DATE")
envars+=("FCST_LEN")
envars+=("SAVE_RST_CYCLES")
envars+=("SAVE_RST_REGEX")
envars+=("SCRATCH_DIR")

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

# check for restart files of old cycles, and delete them
[[ "$SAVE_RST_CYCLES" -lt 0 ]] && SAVE_RST_CYCLES=0
diff=$((FCST_LEN * (SAVE_RST_CYCLES) ))
max_date=$(date -ud "$DATE - $diff hours" "+%Y%m%d%H")
echo "Deleting cycles on/before $max_date"

shopt -s nullglob
RST_DIR=$EXP_DIR/rst
for f in $RST_DIR/??????????/; do
    f=${f%/}
    d=${f##*/}

    # don't delete if new enough
    [[ "$d" -gt "$max_date" ]] && continue

    # don't delete if matches a regular expression
    if [[ "$SAVE_RST_REGEX" != "" ]]; then
        [[ "$d" =~ $SAVE_RST_REGEX ]] && continue
    fi

    # otherwise, if we've made it this far, delete the cycle
    echo "Deleting $d"
    rm -r $f
done
