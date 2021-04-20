#!/bin/sh
# This script may be used to check if the Regression Test for the HAFS system
# passed or not. The User need to specify the HAFS code directory (HAFS_dir)  
# (usually HOMEhafs) and output (HAFS_out) directories (usually CDSCRUB) below.
# The script lists the *.xml files under $HAFS_dir/rocoto directory and gets 
# the different configuration names.  
# The script looks for:
# 1. storm1.done 2. *atcfunix.all 3. hafsprs.synoptic.f012.grb2
# 4. dynf012.nc 5. phyf012.nc 6. the number of dyn and phy files
# 5. hycominit2.done for coupled runs 6. exit 0 in post and prod log files 
# 7. SUCCEEDED for completion task 

# Author: Mrinal Biswas DTC/NCAR
# Do not contact: biswas@ucar.edu



#set -x

HAFS_dir=/your/HAFS/code/directory
HAFS_out=/your/HAFS/scrub/directory

expt_name="`echo $(basename ${HAFS_dir})`"
echo $expt_name

cd ${HAFS_dir}/rocoto
for i in *.xml; do

  file_noext=`echo $i |cut -f1 -d '.'`
  storm_init=`echo $i|rev|cut -f1 -d'-'|cut -f2 -d '.'|rev`
  sid=`echo $i|rev|cut -f2 -d '-'|rev`
  expts=`echo ${file_noext}|rev|cut -f3 -d'-'|rev|awk -F"$expt_name" '{print $2}'`

echo "Running ${HAFS_dir}/${expt_name}${expts}-${sid}-${storm_init} configuration"

echo `pwd`
  rocotostat -d hafs-${expt_name}${expts}-${sid}-${storm_init}.db -w hafs-${expt_name}${expts}-${sid}-${storm_init}.xml

  if_complete=`rocotostat -d hafs-${expt_name}${expts}-${sid}-${storm_init}.db -w hafs-${expt_name}${expts}-${sid}-${storm_init}.xml|grep -e completion |grep -e SUCCEEDED|wc -l`
  storm1_done=${HAFS_out}/${expt_name}${expts}/com/${storm_init}/${sid}/storm1.done
  atcfunix=$(/usr/bin/find ${HAFS_out}/${expt_name}${expts}/com/${storm_init}/${sid} -type f -name "*atcfunix.all")
  hafsprs_synoptic=$(/usr/bin/find ${HAFS_out}/${expt_name}${expts}/com/${storm_init}/${sid} -type f -name "*.hafsprs.synoptic.0p03.f012.grb2")
  dynf_files=$(/usr/bin/find ${HAFS_out}/${expt_name}${expts}/${storm_init}/${sid}/forecast -type f -name "dynf012.nc")
  phyf_files=$(/usr/bin/find ${HAFS_out}/${expt_name}${expts}/${storm_init}/${sid}/forecast -type f -name "phyf012.nc")
  hafs_hycom=$(/usr/bin/find ${HAFS_out}/${expt_name}${expts}/com/${storm_init}/${sid} -type f -name "*hafs_hycom*")

  dynf_files_cnt="`ls ${HAFS_out}/${expt_name}${expts}/${storm_init}/${sid}/forecast/dynf*.nc|wc -l`"
  phyf_files_cnt="`ls ${HAFS_out}/${expt_name}${expts}/${storm_init}/${sid}/forecast/phyf*.nc|wc -l`"

# Check if HYCOM init ran successfully or not

    if [[ `echo $i|grep "regional_static_cplocean3"|wc -l` == "1" ]]; then
        hafs_hycom_cnt=`cat ${HAFS_out}/${expt_name}${expts}/com/${storm_init}/${sid}/*hycominit2.done`
      if [[ $hafs_hycom_cnt == "hycominit2 done for this cycle" ]]; then
           echo "HYCOM INIT SUCCESSFUL"
        else
           echo "HYCOM INIT NOT SUCCESSFUL"
      fi
    fi
   
    if [[ `echo $i|grep "regional_cplocean2"|wc -l` == "1" ]]; then
        hafs_hycom_cnt=`cat ${HAFS_out}/${expt_name}${expts}/com/${storm_init}/${sid}/*hycominit2.done`
      if [[ $hafs_hycom_cnt == "hycominit2 done for this cycle" ]]; then
           echo "HYCOM INIT SUCCESSFUL"
        else
           echo "HYCOM INIT NOT SUCCESSFUL"
      fi
    fi

# Check if rocoto completion task ran successfully or not

    if [[ $if_complete == "1" ]]; then
       echo "ROCOTO SAYS COMPLETION TASK SUCCEEDED"
      else
       echo "ROCOTO SAYS COMPLETION TASK DID NOT SUCCEED"
      fi

# Check the post and product log files

    if [[ $if_complete == "1" ]]; then
       post_log=`cat ${HAFS_out}/${expt_name}${expts}/${storm_init}/${sid}/hafs_post.log|grep "exit 0"`
       prod_log=`cat ${HAFS_out}/${expt_name}${expts}/${storm_init}/${sid}/hafs_product.log|grep "exit 0"`
        if [[ $post_log == "+ exit 0" ]]; then
           echo "POST RAN TILL COMPLETION"
          else
           echo "POST DID NOT RAN TILL COMPLETION"
        fi
        if [[ $prod_log == "+ exit 0" ]]; then
           echo "PRODUCT RAN TILL COMPLETION"
          else
           echo "PRODUCT DID NOT RAN TILL COMPLETION"
        fi
    fi

# Count the number of dyn and phy files

    if [[ $dynf_files_cnt == "5" && $phyf_files_cnt == "5" ]]; then
     echo "ALL DYN AND PHY FILES PRESENT"
      else
     echo "ALL DYN AND PHY FILES NOT PRESENT"
    fi

# Check storm1.done atcfunix and hafsprs.synoptic files

    if [[ -e ${storm1_done} && -e ${atcfunix} && -e ${hafsprs_synoptic} ]]; then
         echo "FOUND STORM1.DONE, TRACKER OUTPUT, HAFSPRS.SYNOPTIC FILES "
      else
         echo "STORM1.DONE, TRACKER OUTPUT, HAFSPRS.SYNOPTIC FILES DO NOT EXIST" 
    fi

# Check to see if dyn and phy files are present

    if [[ -f "${dynf_files}" && -f "${phyf_files}" ]]; then
       echo "FOUND ALL DYN AND PHY FILES IN THE FORECAST DIRECTORY"
      else
       echo "DID NOT FIND ALL THE DYN AND PHY FILES IN THE FORECAST DIRECTORY"
    fi

# Check if everything passed 

    if [[ $if_complete == "1" ]]; then
       if [[ $dynf_files_cnt == "5" && $phyf_files_cnt == "5" ]]; then
       if [[ -e ${storm1_done} && -e ${atcfunix} && -e ${hafsprs_synoptic} ]]; then
       if [[ $post_log == "+ exit 0" ]]; then
       if [[ $prod_log == "+ exit 0" ]]; then
         echo "REGRESSION TEST PASSED!! YAYYY!!"
      else
         echo "REGRESSION TEST FAILED!! IT'S NOT YOUR FAULT!!"
       fi
       fi
       fi
       fi
    fi

done
exit
