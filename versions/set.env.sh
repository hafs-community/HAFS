#! /bin/sh

set -x
name=`basename $ECF_NAME`
EMCCANNED=/lfs/h2/emc/hur/noscrub/CANNED_input_for_HAFS
# COMIN_BASE=$(compath.py hafs/v2.0.0)
COMIN_BASE=/lfs/h2/emc/ptmp/emc.hur/ecflow_hafs/test/com/hafs/v2.0
if [ $name = "jhafs_launch" ]; then
  if [ -s ${EMCCANNED}/com/hafs/v2.0/inp${RUN}/message${storm_num}_$PDY$cyc ] ;then
    rm -rf ${COMIN_BASE}/inp${RUN}/message${storm_num}
    mkdir -p ${COMIN_BASE}/inp${RUN}
    ln -sf ${EMCCANNED}/com/hafs/v2.0/inp${RUN}/message${storm_num}_$PDY$cyc ${COMIN_BASE}/inp${RUN}/message${storm_num}
  else
    echo "${EMCCANNED}/com/hafs/v2.0/inp${RUN}/message${storm_num}_$PDY$cyc not existed, no link to ${COMIN_BASE}/inp${RUN}/message${storm_num}"
   if [[ ${storm_num} = "1" && `ls ${EMCCANNED}/com/hafs/v2.0/inp${RUN}/message*$PDY$cyc*|wc -l` = "1" ]]; then
     echo " found ${EMCCANNED}/com/hafs/v2.0/inp${RUN}/message*$PDY$cyc, linking to ${COMIN_BASE}/inp${RUN}/message${storm_num} .."
     rm -rf ${COMIN_BASE}/inp${RUN}/message${storm_num}
     mkdir -p ${COMIN_BASE}/inp${RUN}
     ln -sf ${EMCCANNED}/com/hafs/v2.0/inp${RUN}/message*$PDY$cyc* ${COMIN_BASE}/inp${RUN}/message${storm_num}
   else
     echo " ${storm_num} != 1 or ${EMCCANNED}/com/hafs/v2.0/inp${RUN}/message*$PDY$cyc is more than 1, skipped "
   fi
  fi
fi

#if [[ $PDY != '' && $cyc != '' && -s ${COMIN_BASE}/inp${RUN}/message${storm_num} ]] ; then
#  STORM_NAME_ID=`cat ${COMIN_BASE}/inp${RUN}/message${storm_num} |awk '{print $3"_"$2}'`
#  echo "STORM_NAME_ID=${STORM_NAME_ID}"
#  export WORKhafs=${WORKhafs:-${DATAROOT:?}/${RUN:?}${storm_num:?}_${cyc:?}_${envir:?}_${hafs_ver:?}_${STORM_NAME_ID}_$PDY$cyc}
#  echo "WORKhafs=$WORKhafs"
#else
#  echo "Not set WORKhafs since PDY=$PDY or cyc=$cyc or ${COMIN_BASE}/inp${RUN}/message${storm_num} ??"
#fi
echo "End of set.env.sh"
