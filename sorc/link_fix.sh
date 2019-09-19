#!/bin/sh
set -x
source ./machine-setup.sh > /dev/null 2>&1

HOMEhafs=$(pwd)/..
FIXhafs=${HOMEhafs}/fix
mkdir -p ${FIXhafs}
cd ${FIXhafs}
mkdir -p fix_fv3
if [ ${target} == "wcoss_cray" ]; then
    ln -sf /gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix/fix_am ./
    ln -sf /gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix/fix_orog ./
elif [[ ${target} == "wcoss_dell_p3" || ${target} == "wcoss" ]]; then
    ln -sf /gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix/fix_am ./
    ln -sf /gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix/fix_orog ./
elif [ ${target} == "hera" ]; then
    ln -sf /scratch1/NCEPDEV/global/glopara/fix/fix_am ./
    ln -sf /scratch1/NCEPDEV/global/glopara/fix/fix_orog ./
elif [ ${target} == "theia" ]; then
    ln -sf /scratch4/NCEPDEV/global/save/glopara/git/fv3gfs/fix/fix_am ./
    ln -sf /scratch4/NCEPDEV/global/save/glopara/git/fv3gfs/fix/fix_orog ./
elif [ ${target} == "jet" ]; then
    ln -sf /mnt/lfs1/projects/hwrf-data/git/fv3gfs/fix/fix_am ./
    ln -sf /mnt/lfs1/projects/hwrf-data/git/fv3gfs/fix/fix_orog ./
else
    echo "Unknown site " ${target}
    exit 1
fi
