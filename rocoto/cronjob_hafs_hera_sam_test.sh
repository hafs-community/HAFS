#!/bin/sh
set -x
date

ROCOTOhafs=/scratch2/BMC/wrfruc/Samuel.Trahan/crow-hafs/remerge-crowhafs/rocoto
cd ${ROCOTOhafs}
EXPT=$(basename $(dirname ${ROCOTOhafs}))
#dev="-f"
dev="-s sites/hera.yaml -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#/apps/intel/intelpython3/bin/python3 ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static \
#    config.NHRS=12 \
#    ${scrubopt} \
#    ../parm/hafs_regional_static.yaml

#/apps/intel/intelpython3/bin/python3 ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_static \
#    config.NHRS=12 \
#    ${scrubopt} \
#    ../parm/hafs_globnest_static.yaml

 # (Upstream update)
 # ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional \
 #     config.NHRS=12 \
 #     ${scrubopt}


 /apps/intel/intelpython3/bin/python3 ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional \
     config.NHRS=12 \
     ${scrubopt}

 # On Hera, set glob_layouty to 10 in hafs_globnest.yaml and hafs_globnest_static.yaml
 # before running this:
 /apps/intel/intelpython3/bin/python3 ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest \
     config.NHRS=12 \
     ${scrubopt} \
     ../parm/hafs_globnest.yaml

#/apps/intel/intelpython3/bin/python3 ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_grib2ab_lbc \
#    config.ictype=gfsnemsio config.bctype=gfsgrib2ab_0p25 \
#    config.NHRS=12 \
#    ${scrubopt} \
#    ../parm/hafs_regional_static.yaml

 # /apps/intel/intelpython3/bin/python3 ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_grib2ab_lbc \
 #     config.ictype=gfsnemsio config.bctype=gfsgrib2ab_0p25 \
 #     config.NHRS=12 \
 #     ${scrubopt}

 # (Upstream update)
 # ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_grib2_lbc \
 #     config.ictype=gfsnemsio config.bctype=gfsgrib2_0p25 \
 #     config.NHRS=12 \
 #     ${scrubopt}

 # /apps/intel/intelpython3/bin/python3 ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
 #     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_grib2_lbc \
 #     config.ictype=gfsnemsio config.bctype=gfsgrib2_0p25 \
 #     config.NHRS=12 \
 #     ${scrubopt}

/apps/intel/intelpython3/bin/python3 ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
   config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_grib2_lbc \
   config.ictype=gfsnemsio config.bctype=gfsgrib2_0p25 \
   config.NHRS=12 \
   ${scrubopt} \
   ../parm/hafs_regional_static.yaml

date
echo 'cronjob done'
