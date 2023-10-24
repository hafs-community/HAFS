#!/bin/sh
# This script may be used to check if the Regression Test for the HAFS system
# passed or not. The User need to specify the HAFS code directory (HAFS_dir)
# (usually HOMEhafs) and output (HAFS_out) directories (usually CDSCRUB) below.
# The script lists the *.xml files under $HAFS_dir/rocoto directory and gets
# the different configuration names.
# The script looks for:
# 1. storm1.done 2. post and prod logs 
# 3. SUCCEEDED for completion task

# Author: Mrinal Biswas DTC/NCAR
# Contact: biswas@ucar.edu

#set -x

HAFS_dir=${1:-$(dirname $(pwd))}

if [ Q$(which rocotostat) = 'Q' ]; then
  echo "Error: Make sure rocotostat and rocotorewind in the path"
  exit 1
fi

cd ${HAFS_dir}/rocoto

for file in *.xml; do

#  file_noext=`echo $file |cut -f1 -d '.'`
  file_noext=`echo $file |rev|cut -c 5- |rev`
  echo $file_noext
  storm_init=`echo $file|rev|cut -f1 -d'-'|cut -f2 -d '.'|rev`
  sid=`echo $file|rev|cut -f2 -d '-'|rev`
  subexpt=`echo ${file_noext}|rev|cut -f3 -d'-'|rev`
  HAFS_out=$(grep "ENTITY WORKhafs" $file | cut -d'"' -f 2 | cut -d'&' -f1)

  echo `pwd`
  echo "Running ${subexpt}-${sid}-${storm_init} configuration"
  rocotostat -d hafs-${subexpt}-${sid}-${storm_init}.db -w hafs-${subexpt}-${sid}-${storm_init}.xml

  if_complete=`rocotostat -d hafs-${subexpt}-${sid}-${storm_init}.db -w hafs-${subexpt}-${sid}-${storm_init}.xml|grep -e completion |grep -e SUCCEEDED|wc -l`
  storm1_done=${HAFS_out}/${subexpt}/com/${storm_init}/${sid}/storm1.done

  # Check if rocoto completion task ran successfully or not

  if [ $if_complete == "1" ] || [ $if_complete == "2" ]; then
    echo "ROCOTO SAYS COMPLETION TASK SUCCEEDED"
  else
    echo "ROCOTO SAYS COMPLETION TASK DID NOT SUCCEED"
  fi

  # Check the post and product log files

  if [ $if_complete == "1" ] || [ $if_complete == "2" ]; then
    post_log=`cat ${HAFS_out}/${subexpt}/${storm_init}/${sid}/hafs_atm_post1.log|grep "status=0"|tail -1|wc -l`
    prod_log=`cat ${HAFS_out}/${subexpt}/${storm_init}/${sid}/hafs_product.log|grep "successfully ran run_product.parent"|tail -1|wc -l`
    if [[ $post_log == "1" ]]; then
      echo "POST RAN TILL COMPLETION"
    else
      echo "POST DID NOT RAN TILL COMPLETION"
    fi
    if [[ $prod_log == "1" ]]; then
      echo "PRODUCT RAN TILL COMPLETION"
    else
      echo "PRODUCT DID NOT RAN TILL COMPLETION"
    fi
  fi

  # Check storm1.done and atcfunix files

  if [[ -e ${storm1_done} ]]; then
    echo "FOUND STORM1.DONE"
  else
    echo "STORM1.DONE DO NOT EXIST"
  fi

  # Check if everything passed

  if [ $if_complete == "1" ] || [ $if_complete == "2" ]; then
    if [[ -e ${storm1_done} ]]; then
    if [[ $post_log == "1" ]]; then
    if [[ $prod_log == "1" ]]; then
      echo "REGRESSION TEST PASSED!! YAYYY!!"
    else
      echo "REGRESSION TEST FAILED!! IT'S NOT YOUR FAULT!!"
    fi
    fi
    fi
  fi

done

exit

