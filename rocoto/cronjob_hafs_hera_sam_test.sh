#!/bin/sh
set -x
date

ROCOTOhafs=/scratch2/BMC/wrfruc/Samuel.Trahan/crow-hafs/retest-crowhafs/rocoto
cd ${ROCOTOhafs}
EXPT=$(basename $(dirname ${ROCOTOhafs}))
#dev="-f"
dev="sites/hera.yaml"
scrubopt="config.scrub_work=no config.scrub_com=no"

./run_hafs.py ${dev} -t 2019091600 09L HISTORY config.EXPT=${EXPT} config.SUBEXPT=${EXPT} $dev ${scrubopt}

#./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
#   config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional \
#   ${scrubopt}

date
echo 'cronjob done'
