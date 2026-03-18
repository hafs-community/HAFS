#!/bin/sh
################################################################################
# Script Name: hafs_ships_diag.sh
# Authors:
#   NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
#   Kate Musgrave
# Abstract:
#   This script generates TC SHIPS diagnostics. It takes grib2 input and makes
#   diagnostic files for each of the model initial times specified in
#   input.list.
# Usage:
#   Required input files:
#     input.params
#     input.list
#     input.plvls
#     parent and nested grid files specified in input.list
#       (for all times specified in input.params)
#     adeck (track) file(s) for storm(s) in input.list
# History:
#   01/29/2026: Adopted from HAFS/HWRF graphics, originally from Kate Musgrave.
################################################################################
set -x -o pipefail

#:starting from home directory
homedir=`echo $PWD`
maindir="${homedir}"
outputdir="${homedir}/"
adeckdir="${homedir}/"
cd ${maindir}

#:retrieve variables/settings from the input.params file
#:-note: tmax, tint (in hrs); not currently set up for minutes
set `cat ${maindir}/'input.params'`
nlvls=${1} tmax=${2} tint=${3} mnested=${4}
smodel2=${5} sruntype=${6} sversion=${7}

imiss=-9999
imissn=$(( ${imiss} * -1 ))

#:add text strings for finding and printing fields
fieldbegT='TMP:'
fieldbegR='RH:'
fieldbegZ='HGT:'
fieldbegU='UGRD:'
fieldbegV='VGRD:'
fieldend=' mb'
fieldtextbeg='FIELD:'
fieldT='T_'
fieldR='R_'
fieldZ='Z_'
fieldU='U_'
fieldV='V_'
fieldP='P_'
fieldSST='SST'
fieldOHC='OHC'
fieldTPW='TPW'
fieldsurf='SURF'

#:loop through all available input files - initial times
#:-requires presence of parent grid; nested grid only will not work
#:-requires presence of nested grid (no adeck to provide center fix)
#:-will only accept pressure lvl data on regular lat/lon grid
for initfile in `cat ${maindir}/input.list`
do
#:process filename
   test -e ${initfile}
   if [ $? -ne 0 ]
   then
      echo 'initial file:'${initfile}' not found'
      exit 1
   fi
   initbase=`basename ${initfile}`
   initdir=`dirname ${initfile}`
   echo ${initbase} > ${maindir}/'tname.txt'
   ./hafs_ships_nameparse.x
   export err=$?; err_chk
   set `cat ${maindir}/'tname2.txt'`
   sname=${1} snum=${2} sbasin=${3} sdtg=${4}
   syr=${5} smo=${6} sda=${7} sti=${8}
   smodel=${9} svert=${10} sgrid=${11} sfitype=${12}
   sforeti=${13} sfint=${14} sfipregr=${15} sfipostgrnt=${16}
   sletter=${17}

#: specify diagnostic filename and adeck filename
   smodeluc=`echo ${smodel} | tr 'a-z' 'A-Z'`
   sfiout='s'${sbasin}${snum}${syr}'_'${smodel2}'_'${sruntype}${sversion}'_'${sdtg}'_diag.dat'
   sfiadeck=${adeckdir}'a'${sbasin}${snum}${syr}'.dat'

#:check for adeck/track file
   test -e ${sfiadeck}
   if [ $? -ne 0 ]
   then
      sfiadeck="${adeckdir}${snum}${sletter}.${sdtg}.${smodel2}.trak.atcfunix"
      echo "basin sletter="${sletter}
      echo "adeck file="${sfiadeck}
      echo 'looking for track file:'${sfiadeck}
      test -e ${sfiadeck}
      if [ $? -ne 0 ]
      then
         echo 'track file:'${sfiadeck}' not found'
         exit 1
      fi
   fi
   cp ${sfiadeck} tempadeck.dat

#:set up control file for creating total diagnostic files
   dtxt='diaginfo.txt'
   echo ${nlvls} > ${dtxt}
   echo ${mnested} >> ${dtxt}
   echo ${tmax} >> ${dtxt}
   echo ${tint} >> ${dtxt}
   echo ${sdtg} >> ${dtxt}
   echo ${smodel} >> ${dtxt}
   echo ${sbasin} >> ${dtxt}
   echo ${snum} >> ${dtxt}
   echo ${sname} >> ${dtxt}

#:set base filenames for the chosen run
   pbase=${initdir}'/'${sfipregr}'p'${sfipostgrnt}
   nbase=${initdir}'/'${sfipregr}'n'${sfipostgrnt}

#:set output filenames for current forecast time (text files)
   outptxt=${maindir}/temp_fieldp.txt
   outntxt=${maindir}/temp_fieldn.txt

#:loop through times indicated by tmax and tint (starting at 0)
   currtime=0
   while [ $currtime -le $tmax ]
   do
      if [ $currtime -lt 10 ]
      then
         pcurr=${pbase}'0'${currtime}
         ncurr=${nbase}'0'${currtime}
         fcurr='f00'${currtime}
         diagcurr='mdiagf00'${currtime}'.dat'
      elif [ $currtime -lt 100 ]
      then
         pcurr=${pbase}${currtime}
         ncurr=${nbase}${currtime}
         fcurr='f0'${currtime}
         diagcurr='mdiagf0'${currtime}'.dat'
      else
         pcurr=${pbase}${currtime}
         ncurr=${nbase}${currtime}
         fcurr='f'${currtime}
         diagcurr='mdiagf'${currtime}'.dat'
      fi
#:tests for existence of parent grid
      test -e ${pcurr}
      if [ $? -ne 0 ]
      then
         echo 'parent grid:'${pcurr}' not found'
         pgexist=0
      else
         pgexist=1
      fi
#:tests for existence of nested grid
      if [ ${mnested} -eq 1 ]
      then
         test -e ${ncurr}
         if [ $? -ne 0 ]
         then
            echo 'nested grid:'${ncurr}' not found'
            exit 1
         fi
      fi

#:get current center location from tmpadeck.dat
      echo ${sdtg} ${smodel} ${imiss} ${currtime} > tempadinfo.txt
#      cp tempadinfo.txt tempadinfo${fcurr}.txt
      ./hafs_ships_getcenter.x
      export err=$?; err_chk
#      cp center.txt center${fcurr}.txt
#      echo center${fcurr}.txt >> ${dtxt}
# when lat is <0 (souther Hem), this does not work
#      set `cat ${maindir}/'center.txt'`
#      centerlat=${1} centerlon=${2}
      centerlat=$( head -n 1 ${maindir}/center.txt | awk '{print $1}')
      centerlon=$( head -n 1 ${maindir}/center.txt | awk '{print $2}')

#:if center location exists at current time, get parent and nested grids
#: otherwise, skip grids and call null case for parameter file fill-in
      if [ ${centerlat} -eq ${imissn} ] || [ ${centerlon} -eq ${imissn} ]
      then
#:    fill in the missing value array here for current time
         ./hafs_ships_inddiagnull.x
         export err=$?; err_chk
         mv params.txt params${fcurr}.txt
         echo params${fcurr}.txt >> ${dtxt}
#:get parent grid information (nx, ny, lat, lon, lat/lon intervals)
      else
         ${WGRIB2} -nxny -d 1 ${pcurr} > tempnxny.txt
         ${WGRIB2} -grid -d 1 ${pcurr} > templatlon.txt
         ./hafs_ships_gridparse.x
         export err=$?; err_chk
         mv tempgrid.txt temp_gridp.txt

#:get parent grid fields
#        T 2m
         outpbin=${fieldT}${fieldsurf}'_p.bin'
         ${WGRIB2} -match "TMP:2 m " -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        RH 2m
         outpbin=${fieldR}${fieldsurf}'_p.bin'
         ${WGRIB2} -match "RH:2 m " -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        U 10m
         outpbin=${fieldU}${fieldsurf}'_p.bin'
         ${WGRIB2} -match "UGRD:10 m " -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        V 10m
         outpbin=${fieldV}${fieldsurf}'_p.bin'
         ${WGRIB2} -match "VGRD:10 m " -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        SLP
         outpbin=${fieldP}${fieldsurf}'_p.bin'
         ${WGRIB2} -match "PRMSL" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi

#:cycle through specified pressure levels to retrieve sounding data
         for currplvl in `cat ${maindir}/input.plvls`
         do
            if [ $currplvl -lt 10 ]
            then
               currplvlt='000'${currplvl}
            elif [ $currplvl -lt 100 ]
            then
               currplvlt='00'${currplvl}
            elif [ $currplvl -lt 1000 ]
            then
               currplvlt='0'${currplvl}
            else
               currplvlt=${currplvl}
            fi
#           T
            fieldname=${fieldbegT}${currplvl}${fieldend}
            outpbin=${fieldT}${currplvlt}'_p.bin'
            ${WGRIB2} -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
#           RH
            fieldname=${fieldbegR}${currplvl}${fieldend}
            outpbin=${fieldR}${currplvlt}'_p.bin'
            ${WGRIB2} -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
#           Z
            fieldname=${fieldbegZ}${currplvl}${fieldend}
            outpbin=${fieldZ}${currplvlt}'_p.bin'
            ${WGRIB2} -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
#           U
            fieldname=${fieldbegU}${currplvl}${fieldend}
            outpbin=${fieldU}${currplvlt}'_p.bin'
            ${WGRIB2} -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
#           V
            fieldname=${fieldbegV}${currplvl}${fieldend}
            outpbin=${fieldV}${currplvlt}'_p.bin'
            ${WGRIB2} -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
         done
#        TPW
         outpbin=${fieldTPW}'_p.bin'
         ${WGRIB2} -match "PWAT" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        SST
         outpbin=${fieldSST}'_p.bin'
         ${WGRIB2} -match "WTMP:surf" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#:list the _p.bin files into outptxt
         ls -1 *_p.bin > ${outptxt}
#:if no parent fields are found make an empty list file
         if [ ! -f "$outptxt" ]
         then
            touch ${outptxt}
         fi

#:get nested grid if nested grid specified:
         if [ ${mnested} -eq 1 ]
         then
#:get nested grid information (nx, ny, lat, lon, lat/lon intervals)
            ${WGRIB2} -nxny -d 1 ${ncurr} > tempnxny.txt
            ${WGRIB2} -grid -d 1 ${ncurr} > templatlon.txt
            ./hafs_ships_gridparse.x
            export err=$?; err_chk
            mv tempgrid.txt temp_gridn.txt
#:get nested grid fields
#           U 10m
            outnbin=${fieldU}${fieldsurf}'_n.bin'
            ${WGRIB2} -match "UGRD:10 m " -bin ${outnbin} ${ncurr}
            if [ ! -s "$outnbin" ]
            then
               echo "Warning: No field for ${outnbin}, skipping."
               rm ${outnbin}
            fi
#           V 10m
            outnbin=${fieldV}${fieldsurf}'_n.bin'
            ${WGRIB2} -match "VGRD:10 m " -bin ${outnbin} ${ncurr}
            if [ ! -s "$outnbin" ]
            then
               echo "Warning: No field for ${outnbin}, skipping."
               rm ${outnbin}
            fi
#           SLP
            outnbin=${fieldP}${fieldsurf}'_n.bin'
            ${WGRIB2} -match "PRMSL" -bin ${outnbin} ${ncurr}
            if [ ! -s "$outnbin" ]
            then
               echo "Warning: No field for ${outnbin}, skipping."
               rm ${outnbin}
            fi
#           SST
            outnbin=${fieldSST}'_n.bin'
            ${WGRIB2} -match "WTMP:surf" -bin ${outnbin} ${ncurr}
            if [ ! -s "$outnbin" ]
            then
               echo "Warning: No field for ${outnbin}, skipping."
               rm ${outnbin}
            fi
#:list the _n.bin files into outntxt
            ls -1 *_n.bin > ${outntxt}
#:if no nested fields are found make an empty list file
            if [ ! -f "$outntxt" ]
            then
               touch ${outntxt}
            fi
         fi

#:run diagnostic parameter calculation for current time
         ./hafs_ships_inddiag.x
         export err=$?; err_chk
         mv params.txt params${fcurr}.txt
         echo params${fcurr}.txt >> ${dtxt}

#:remove binary files before starting next time
         rm *.bin
         rm ${outptxt}
         rm ${outntxt}

      fi   #finish if statement for reading and calculating parameters

#:calculate next forecast time
      currtime=$(( $currtime + $tint ))
   done   #finish loop for each individual time

#:run diagnostic file output from accumulation of individual times
   ./hafs_ships_totaldiag.x
   export err=$?; err_chk
   mv diag.txt ${outputdir}${sfiout}

done   #finish loop for each specified initial file

