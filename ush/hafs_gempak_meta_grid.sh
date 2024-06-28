#!/bin/ksh
################################################################################
# Script Name: hafs_gempak_meta_grid.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script makes HAFS GEMPAK meta files for the parent grid.
# History:
#   03/18/2023: This script was adopted/inerited from HWRF and updated for HAFS.
# Note: ksh can do floating point calculations directly.
################################################################################
set -x -o pipefail

mkdir -p $DATA/grid
cd $DATA/grid

device="NC | grid.nmeta"
intercom=${intercom:-"${WORKhafs}/intercom/gempak"}

# Make sure gempak files are ready
for fhr in $(seq -f'%03g' $fstart $finc $fend); do
  full_domain=${DATA}/${NET}p/${NET}p_${PDY}${cyc}f${fhr}_${storm_id}
  # Make sure gempak files are ready
  MAX_WAIT_TIME=${MAX_WAIT_TIME:-1200}
  n=0
  while [ $n -le ${MAX_WAIT_TIME} ]; do
    if [ -f ${intercom}/${NET}p_${PDY}${cyc}f${fhr}_${storm_id}.done ]; then
      echo "$full_domain, ${intercom}/${NET}p_${PDY}${cyc}f${fhr}_${storm_id}.done ready, continue"
      break
    else
      sleep 10s
    fi
    if [ $n -gt ${MAX_WAIT_TIME} ] && [ ! -f ${intercom}/${NET}p_${PDY}${cyc}f${fhr}_${storm_id}.done ]; then
      echo "FATAL ERROR: Waited $full_domain too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
      exit 1
    fi
    n=$((n+10))
  done
done

$GEMEXE/gdinfo << EOF
GDFILE  = ${DATA}/${NET}p/${NET}p_${PDY}${cyc}f000_${storm_id}
LSTALL  = no
OUTPUT  = f/gdinfo_$$.dat
run
exit
EOF

LL=($(grep -m1 "LL CORNER:" gdinfo_$$.dat | awk -F: '{print $2}'))
UR=($(grep -m1 "UR CORNER:" gdinfo_$$.dat | awk -F: '{print $2}'))

lat1=$(( ${LL[0]} - 1 ))
lon1=$(( ${LL[1]} - 1 ))
lat2=$(( ${UR[0]} + 1 ))
lon2=$(( ${UR[1]} + 1 ))

if [ $lon1 -gt 180 ]; then
  lon1=$((lon1-360))
fi
if [ $lon1 -lt -180 ]; then
  lon1=$((360+lon1))
fi
if [ $lon2 -gt 180 ]; then
  lon2=$((lon2-360))
fi
if [ $lon2 -lt -180 ]; then
  lon2=$((360+lon2))
fi

garea="${lat1};${lon1};${lat2};${lon2}"

latlon="1/2//1;1/5;5"
YYMMDD=$(echo $PDY | cut -c3-)

${NLN} $PARMhafs/gempak/datatype.tbl datatype.tbl

$GEMEXE/gdplot2_nc <<EOF
GDFILE  = ${NET}P:${storm_id} | ${YYMMDD}/${cyc}00
GDATTIM = FALL
DEVICE	= $device
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 1
MAP     = 1
CLEAR	= yes
CLRBAR  = 1

GAREA	= $garea
PROJ	= ced/0;0;0
LATLON	= $latlon

GLEVEL  = 500:1000!500:1000!0
GVCORD  = pres    !pres    !none
PANEL   = 0
SKIP    = 0
SCALE   = -1        !-1             !0
GDPFUN  = sm5s(ldf(hght)) !sm5s(ldf(hght))      !sm9s(pmsl)
TYPE    = c
CONTUR  = 2         !2              !7
CINT    = 6/0/540   !6/546/999      !4
LINE    = 6/3/2     !2/3/2          !20//3
FINT    =
FLINE   =
HILO    = !! 26;2/H#;L#/1018-1070;900-1012//30;30/y
HLSYM   = 2;1.5//21//hw
CLRBAR  = 1
WIND    = 18//1
REFVEC  =
TEXT    = 1/21//hw
CLEAR   = yes
TITLE	= 5/-2/~HAFS ^  MSLP, 1000-500 MB THKN for ${storm_id}|^MSLP,1000-500 THKN!0
IJSKIP  = 1
FILTER  = yes
list
run

GLEVEL  = 250:850        !0
GVCORD  = pres           !none
SKIP    = 0
SCALE   = 0
GDPFUN  = vecr(lyr_swtm(urel|850-250),lyr_swtm(vrel|850-250))     !sm9s(pmsl)
TYPE    = b              !c
CONTUR  = 2
CINT    = 0              !4
LINE    = 0              !20//3
FINT    =
FLINE   =
HILO    = 0    !26;2/H#;L#/1018-1070;900-1012//30;30/y
HLSYM   = 0    !2;1.5//21//hw
CLRBAR  = 0
WIND    = bk10/0.9/1.4/112!bk0
TITLE   = 1/-2/~ ? 850-250 MB MLW and MSLP for ${storm_id}|~850-250mb MLW & MSLP!0
IJSKIP  = 1
FILTER  = .7
run

GLEVEL  = 500:850         !850    !500         !0
GVCORD  = pres            !pres   !pres        !none
SKIP    = 0/5;5
GDPFUN  = mag(vldf(obs))  !(wnd)  !(wnd)       !sm9s(pmsl)
TYPE    = c/f ! a ! a                          !c
CONTUR  = 2
CINT    = 5/20                                 !4
LINE    = 26//2                                !20//2
FINT    = 5/15
FLINE   = 0;24;30;29;23;22;14;15;16;17;20;5
HILO    =                                      !26;2/H#;L#/1018-1070;900-1012//30;30/y
HLSYM   =                                      !1;1//21//hw
CLRBAR  = 1
WIND    = ! AM7/.5/1/221/.4 ! AM6/.5/1/221/.4
TITLE   = 1/-2/~ ? 850-500 MB WIND SHEAR and MSLP for ${storm_id} (850mb magenta, 500mb cyan)|~850-500mb SHEAR & PMSL  !0
FILTER  = yes
IJSKIP  = 2
CLEAR   = yes  !no
list
run

GLEVEL  = 200:850         !850    !200
TITLE   = 1/-2/~ ? 850-200 MB WIND SHEAR for ${storm_id} (850mb magenta, 200mb cyan)|~850-200mb SHEAR  !0
WIND    = ! AM7/.5/1/221/.4 ! AM6/.5/1/221/.4
FINT    = 5/25
IJSKIP  = 5
run

GLEVEL  = 850
GVCORD  = PRES
PANEL   = 0
SKIP    = 0/2;2            !           !               !            !
SCALE   = 5                !5          !5              !5           !0
GDPFUN  = sm9s((avor(wnd)))//v   !v          !mul(v,-1)      !mul(v,-1)   !kntv(wnd)
TYPE    = c/f              !c          !c/f            !c           !b
CONTUR  = 1
CINT    = 2/10/99          !2/4/8      !2/10/99        !2/4/8
LINE    = 7/5/1/2          ! 29/5/1/2  ! 7/5/1/2       ! 29/5/1/2
FINT    = 16;20;24;28;32;36;40;44
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!        !2;6/X;N/10-99;10-99!
HLSYM   = 
CLRBAR  = 1
WIND    = 9/0.7/2/112
REFVEC  =
TITLE   = 1//HAFS ~ @ WINDS and VORTICITY for ${storm_id}|~@ WINDS and VORT!0
TEXT    = 1/21//hw
CLEAR   = yes
FILTER  = yes
IJSKIP  = 1
!list
run

GLEVEL  = 500
SCALE   = 5                !5          !5              !5           !0            !-1
GDPFUN  = sm9s((avor(wnd)))//v   !v    !mul(v,-1)      !mul(v,-1)   !kntv(wnd)    !sm5s(hght)
TYPE    = c/f              !c          !c/f            !c           !b            !c
CINT    = 2/10/99          !2/4/8      !2/10/99        !2/4/8       !             !6
LINE    = 7/5/1/2          ! 29/5/1/2  ! 7/5/1/2       ! 29/5/1/2   !             !5/1/2/1
TITLE   = 1//HAFS ~ @ HGT, WINDS and VORTICITY for ${storm_id}|~@ HGT,WINDS,VORT!0
run

GLEVEL  = 200
run

SCALE   = 0               !0                !5              !5                !-1
GDPFUN  = mag(kntv(wnd))  !mag(kntv(wnd))   !sm5s(div(wnd)) !sm5s(div(wnd))   !sm5s(hght)
TYPE    = c               !c/f              !c/f            !c                !c
CONTUR  = 2
CINT    = 30;50           !70;90;110;130;150;170;190   !-11;-9;-7;-5;-3    !2/3/18   !6
LINE    = 26              !32//2            !19/-2//2       !20               !1//2
FINT    =                 !70;90;110;130;150;170;190 !3;5;7;9;11;13  !
FLINE   =                 !0;24;25;29;7;15;14;2      !0;23;22;21;17;16;2   !
HILO    = 0               !0                !0              !0             !1/H#;L#//7
HLSYM   = 0               !0                !0              !0             !1;1//21;21/2;2/hw
CLRBAR  = 0               !0                !1/V/LL         !0
WIND    = 0
TITLE   = 1//HAFS ~ @ HGT, WINDS and DIVERGENCE for ${storm_id}|~@ HGT,WINDS,DIV!0
run

exit
EOF
export err=$?; err_chk

$GEMEXE/gpend
export err=$?; err_chk

############################################################
# Gempak does not always have a non-zero return code when it
# cannot produce the desired grid. Check for this case here.
############################################################
ls -l grid.nmeta
export err=$?; err_chk

GMETAF=${COMOUT}/gempak/${storm_id}/meta/${RUN}_${PDY}_${cyc}_${storm_id}
if [ ${SENDCOM:-YES} = "YES" ]; then
  ${FCP} grid.nmeta ${GMETAF}
  if [ ${SENDDBN:-NO} = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL $(echo ${RUN} | tr [a-z] [A-Z])_METAFILE $job ${GMETAF}
  fi
fi

