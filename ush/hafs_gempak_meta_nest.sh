#!/bin/ksh
################################################################################
# Script Name: hafs_gempak_meta_nest.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script makes HAFS GEMPAK meta files for the nest grid.
# History:
#   03/18/2023: This script was adopted/inerited from HWRF and updated for HAFS.
# Note: ksh can do floating point calculations directly.
################################################################################
set -x -o pipefail

mkdir -p $DATA/nest
cd $DATA/nest

device="NC | nest.nmeta"
minlat=90
maxlat=-90
minlon=180
maxlon=-180

intercom=${intercom:-"${WORKhafs}/intercom/gempak"}

for fhr in $(seq -f'%03g' $fstart $finc $fend); do
  nested_grid=${DATA}/${NET}/${NET}n_${PDY}${cyc}f${fhr}_${storm_id}
  # Make sure gempak files are ready
  MAX_WAIT_TIME=${MAX_WAIT_TIME:-1200}
  n=0
  while [ $n -le ${MAX_WAIT_TIME} ]; do
    if [ -f ${intercom}/${NET}n_${PDY}${cyc}f${fhr}_${storm_id}.done ]; then
      break
    else
      sleep 10s
    fi
    if [ $n -gt ${MAX_WAIT_TIME} ] && [ ! -f ${intercom}/${NET}n_${PDY}${cyc}f${fhr}_${storm_id}.done ]; then
      echo "FATAL ERROR: Waited $nested_grid too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
      exit 1
    fi
    n=$((n+10))
  done

  $GEMEXE/gdinfo << EOF
GDFILE  = $nested_grid
LSTALL  = no
OUTPUT  = f/gdinfo_$$.dat
run
EOF

  LL=($(grep -m1 "LL CORNER:" gdinfo_$$.dat | awk -F: '{print $2}'))
  UR=($(grep -m1 "UR CORNER:" gdinfo_$$.dat | awk -F: '{print $2}'))

  if [ ${LL[0]} -lt $minlat ]; then minlat=${LL[0]}; fi
  if [ ${LL[1]} -lt $minlon ]; then minlon=${LL[1]}; fi
  if [ ${UR[0]} -gt $maxlat ]; then maxlat=${UR[0]}; fi
  if [ ${UR[1]} -gt $maxlon ]; then maxlon=${UR[1]}; fi
done

minlon=$((minlon-1))
maxlat=$((maxlat+1))

dlat=$((maxlat-minlat))
dlon=$((maxlon-minlon))
if [ $dlat -gt $dlon ]; then
  diff=$((dlat-dlon-2))
  minlon=$((minlon-diff))
  clrbar="1"
elif [ $dlon -gt $dlat ]; then
  diff=$((dlon-dlat-2))
  maxlat=$((maxlat+diff))
  clrbar="1/h/lc/0.5;0.95/0.5;0.01/1"
elif [ $dlon -eq $dlat ]; then
  minlon=$((minlon-2))
  clrbar="1/h/lc/0.5;0.95/0.5;0.01/1"
fi

garea="${minlat};${minlon};${maxlat};${maxlon}"

if [ $dlat -gt 30 -o $dlon -gt 30 ]; then
  latlon="1/2//1;1/10;10"
elif [ $dlat -gt 20 -o $dlon -gt 20 ]; then
  latlon="1/2//2;2/1;1"
else
  latlon="1/2//1;1/1;1"
fi

for fhr in $(seq -f'%03g' $fstart $finc $fend); do
  echo "PROCESSING HOUR $fhr ----------------------------------------"
  nested_grid=${DATA}/${NET}/${NET}n_${PDY}${cyc}f${fhr}_${storm_id}
  full_domain=${DATA}/${NET}p/${NET}p_${PDY}${cyc}f${fhr}_${storm_id}
  # Make sure gempak files are ready
  MAX_WAIT_TIME=${MAX_WAIT_TIME:-1200}
  n=0
  while [ $n -le ${MAX_WAIT_TIME} ]; do
    if [ -f ${intercom}/${NET}p_${PDY}${cyc}f${fhr}_${storm_id}.done ]; then
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

  $GEMEXE/gdplot2_nc <<EOF
\$MAPFIL = hipowo.cia
GDFILE   = ${nested_grid}
GDATTIM  = F${fhr}
DEVICE   = ${device}
PANEL    = 0
TEXT     = 1/21//hw
CONTUR   = 1
MAP      = 1
CLEAR    = yes
CLRBAR   = ${clrbar}

GAREA    = ${garea}
PROJ     = ced/0;0;0/0;1;0;0
LATLON   = ${latlon}

GLEVEL  = 1000!1000!0
GVCORD  = pres!pres!none
PANEL   = 0
SKIP    =                             !      !0/1
FILTER  = yes
SCALE   = 0
GDPFUN  = mag(kntv(wnd%pres))//w      !w     !sm5s(sm5s(pmsl))   !kntv(wnd@1000%pres)
TYPE    = c/f                         !c     !c                  !b
CONTUR  = 1
CINT    = 5/10/20                     !5/25    !2/980
LINE    = 6//1/0                      !32//1/0 !20//1
FINT    = 20;35;65
FLINE   = 0;25;17;2
HILO    = !!;31/;L#1//3//yes
HLSYM   = 2;1.5//21//hw
WIND    = 18//1
REFVEC  = 
TEXT    = 1/21//hw
CLEAR   = yes
MAP     = 1
TITLE   = 5/0/HAFS ~ PMSL, 1000MB WINDS (KTS)|~PMSL,1000MB WINDS!0
IJSKIP  = 1
list
run

GLEVEL  = 10!10!0
GVCORD  = hght!hght!none
PANEL   = 0
SKIP    = 0/1;-2
FILTER  = yes
SCALE   = 0
GDPFUN  = mag(kntv(wnd%hght)) !mag(kntv(wnd%hght)) !sm5s(sm5s(pmsl))  !kntv(wnd@10%hght)
TYPE    = c/f                 !c                   !f                 !b
CONTUR  = 1
CINT    = 5/10/20   !5/25   !0
LINE    = 6//1/0    !32//1/0!0
FINT    = 20;35;65!!
FLINE   = 0;25;17;2!!0
HILO    = !!;31/;L#1//3//yes
HLSYM   = 2;1.5//21//hw
WIND    = 18//1
REFVEC  = 
TEXT    = 1/21//hw
CLEAR   = yes
MAP     = 1
TITLE	= 5/0/HAFS ~ 10M WINDS (KTS) w/ MIN PRESSURE|~10M WINDS,PMIN!0
GDFILE  = ${full_domain}
IJSKIP  = 2
list
run

GLEVEL  = 10!10
GVCORD  = hght!hght
GDPFUN  = mag(kntv(wnd%hght)) !kntv(wnd@10%hght)
TYPE    = c/f                 !b
CINT    = 5/20/40
LINE    = 6//1/0
FINT    = 20;35;65
FLINE   = 0;25;17;2
HILO    = 
HLSYM   = 
CLEAR   = no
GDFILE  = ${nested_grid}
TITLE	= 0
list
run

GLEVEL  = 0
GVCORD  = none
GDPFUN  = sm5s(sm5s(pmsl))
TYPE    = f
FINT    = 0
FLINE   = 0
HILO    = ;31/;L#1//3//yes
HLSYM   = 2;1.5//21//hw
GDFILE  = ${full_domain}
run

GLEVEL  = 850
GVCORD  = PRES
PANEL   = 0
SKIP    = 0/2;2
FILTER  = yes
SCALE   = 5                !5               !0
GDPFUN  = sm9s(avor(wnd))  !sm9s(avor(wnd)) !kntv(wnd)
TYPE    = c/f              !c               !b
CONTUR  = 1
CINT    = 2/10/99          !2/4/8
LINE    = 7/5/1/2          ! 29/5/1/2
FINT    = 16;20;24;28;32;36;40;44
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!
HLSYM   = 
WIND    = 9/0.7/2/112
REFVEC  = 
TITLE   = 5/0/HAFS ~ @ WINDS and VORTICITY for ${storm_id}|~@ WINDS and VORT!0
TEXT    = 1/21//hw
CLEAR   = yes
GDFILE  = ${full_domain}
IJSKIP  = 1
list
run

CLEAR   = no
GDFILE  = ${nested_grid}
TITLE   = 0
list
run

GLEVEL  = 700
SCALE   = 5                !5               !0         !0
GDPFUN  = sm9s(avor(wnd))  !sm9s(avor(wnd)) !kntv(wnd) !sm5s(hght)
TYPE    = c/f              !c               !b         !c
CINT    = 2/10/99          !2/4/8           !          !60
LINE    = 7/5/1/2          !29/5/1/2        !          !5/1/2/1
CLEAR   = yes
TITLE   = 5/0/HAFS ~ @ HEIGHT, WINDS and VORTICITY for ${storm_id}|~@ HGT,WINDS and VORT!0
GDFILE  = ${full_domain}
list
run

CLEAR   = no
SCALE   = 5                !5               !0
TYPE    = c/f              !c               !b
CINT    = 2/10/99          !2/4/8           !
LINE    = 7/5/1/2          !29/5/1/2        !
GDPFUN  = sm9s(avor(wnd))  !sm9s(avor(wnd)) !kntv(wnd)
GDFILE  = ${nested_grid}
TITLE	= 0
list
run

GLEVEL  = 500
GDPFUN  = sm9s(avor(wnd))  !sm9s(avor(wnd)) !kntv(wnd) !sm5s(hght)
TYPE    = c/f              !c               !b         !c
CINT    = 2/10/99          !2/4/8           !          !60
LINE    = 7/5/1/2          !29/5/1/2        !          !5/1/2/1
CLEAR   = yes
TITLE   = 5/0/HAFS ~ @ WINDS and VORTICITY for ${storm_id}|~@ HGT,WINDS and VORT!0
GDFILE  = ${full_domain}
list
run

CLEAR   = no
GDPFUN  = sm9s(avor(wnd))  !sm9s(avor(wnd)) !kntv(wnd)
TYPE    = c/f              !c               !b
CINT    = 2/10/99          !2/4/8
LINE    = 7/5/1/2          !29/5/1/2
GDFILE  = ${nested_grid}
TITLE	= 0
list
run

GLEVEL  = 200
GDPFUN  = sm9s(avor(wnd))  !sm9s(avor(wnd)) !kntv(wnd) !sm5s(hght)
TYPE    = c/f              !c               !b         !c
CINT    = 2/10/99          !2/4/8           !          !60
LINE    = 7/5/1/2          !29/5/1/2        !          !5/1/2/1
CLEAR   = yes
TITLE   = 5/0/HAFS ~ @ WINDS and VORTICITY for ${storm_id}|~@ HGT,WINDS and VORT!0
GDFILE  = ${full_domain}
list
run

CLEAR   = no
GDPFUN  = sm9s(avor(wnd))  !sm9s(avor(wnd)) !kntv(wnd)
TYPE    = c/f              !c               !b
CINT    = 2/10/99          !2/4/8
LINE    = 7/5/1/2          !29/5/1/2
GDFILE  = ${nested_grid}
TITLE	= 0
list
run

SCALE   = 0               !0                !5              !5                !-1            !0
CLEAR   = yes
GDPFUN  = mag(kntv(wnd))  !mag(kntv(wnd))   !sm5s(div(wnd)) !sm5s(div(wnd))   !sm5s(hght)    !kntv(wnd)
TYPE    = c               !c/f              !c/f            !c                !c             !b
CONTUR  = 2
CINT    = 30;50           !70;90;110;130;150;170;190   !-11;-9;-7;-5;-3    !2/3/18   !6
LINE    = 26              !32//2            !19/-2//2       !20               !1//2
FINT    =                 !70;90;110;130;150;170;190 !3;5;7;9;11;13  !
FLINE   =                 !0;24;25;29;7;15;14;2      !0;23;22;21;17;16;2   !
HILO    = 0               !0                !0              !0             !1/H#;L#//7
HLSYM   = 0               !0                !0              !0             !1;1//21;21/2;2/hw
CLRBAR  = 0               !0                !1/V/LL         !0
WIND    = BM18/1/1/1
TITLE   = 1//HAFS ~ @ HGT, WINDS and DIVERGENCE for ${storm_id}|~@ HGT,WINDS,DIV!0
GDFILE  = ${full_domain}
run

GDFILE  = ${full_domain}
CLEAR   = yes
GLEVEL  = 500:850         !850    !500        !0
GVCORD  = pres            !pres   !pres       !none
SCALE   = 0
GDPFUN  = mag(vldf(obs))  !(wnd)  !(wnd)      !sm9s(pmsl)
TYPE    = c/f             !a      !a          !c
CONTUR  = 2
CINT    = 5/20                                !4
LINE    = 26//2                               !20//2
FINT    = 5/25
FLINE   = 0;24;30;29;23;22;14;15;16;17;20;5
HILO    =                                     !26;2/H#;L#/1018-1070;900-1012//30;30/y
HLSYM   =                                     !1.5;1.5//21//hw
CLRBAR  = 1
WIND    = ! AM7/.4/1/221/.4 ! AM6/.4/1/221/.4
TITLE   = 5/-2/~ ? 850-500 MB WIND SHEAR and PMSL for ${storm_id} (850mb magenta, 500mb cyan)|~850-500mb SHEAR & PMSL  !0
run

CLEAR   = no
GLEVEL  = 500:850         !850    !500
GVCORD  = pres            !pres   !pres
GDPFUN  = mag(vldf(obs))  !(wnd)  !(wnd)
TYPE    = c/f             !a      !a
CINT    = 5/20
LINE    = 26//2
HILO    = 
HLSYM   = 
GDFILE  = ${nested_grid}
TITLE   = 0
run

GLEVEL  = 200:850         !850    !200
GDFILE  = ${full_domain}
CLEAR   = yes
TITLE   = 5/-2/~ ? 850-200 MB WIND SHEAR for ${storm_id} (850mb magenta, 200mb cyan)|~850-200mb SHEAR  !0
run

CLEAR   = no
GDFILE  = ${nested_grid}
TITLE   = 0
run

GLEVEL  = 700
GDFILE  = ${nested_grid}
CLEAR   = yes
SCALE   = 0
GDPFUN  = sm5s(lyr_swtm(relh|850-700))
TYPE    = f
CINT    = 10;20;80;90  ! 30;40;50;60;70
LINE    = 32//2        ! 23//2
FINT    = 10;30;70;90
FLINE   = 18;8;0;22;23
TITLE   = 5/-2/~ ? 850-700 MEAN RH and 700 HGT for ${storm_id}|~850-700mb MEAN RH,700 HGT!0
run

CLEAR   = no
GDFILE  = ${full_domain}
SCALE   = 0    !0   !-1
TYPE    = c/f  !c   !c
GDPFUN  = sm5s(lyr_swtm(relh|850-700))  !sm5s(lyr_swtm(relh|850-700))   !sm5s(hght)
CINT    = 10;20;80;90  ! 30;40;50;60;70   !3
LINE    = 32//2        ! 23//2            !5/1/2/1
TITLE   = 5/-2/~ ? 850-700 MEAN RH and 700 HGT for ${storm_id}!0
run

GLEVEL  = 500
GDFILE  = ${full_domain}
CLEAR   = yes
GDPFUN  = sm5s(lyr_swtm(relh|500-300))  !sm5s(lyr_swtm(relh|500-300))  !sm5s(hght)
CINT    = 10;20;80;90  ! 30;40;50;60;70   !6
SCALE   = 0    !0   !-1
TYPE    = c/f   !c  !c
LINE    = 32//2        ! 23//2            !5/1/2/1
FINT    = 10;30;70;90
FLINE   = 18;8;0;22;23
TITLE   = 5/-2/~ ? 500-300 MEAN RH and 500 for ${storm_id}|~500-300mb MEAN RH,500 HGT!0
run

exit
EOF

done

YYMMDD=$(echo $PDY | cut -c3-)

${NLN} $PARMhafs/gempak/datatype.tbl datatype.tbl

$GEMEXE/gdplot2_nc <<EOF
GDFILE  = HAFS:${storm_id} | ${YYMMDD}/${cyc}00
GDATTIM = F006-F126-06
GLEVEL  = 0
GVCORD  = none
SKIP    = 0
SCALE   = 0
GDPFUN  = p06i
TYPE    = f
CONTUR  = 2
CINT    = 
LINE    = 
FINT    = .01;.1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9
FLINE   = 0;21-30;14-20;5
HILO    = 31;0/x#2/0.1-99/10/100;0/y
HLSYM   = 1.5
CLRBAR  = 1
WIND    = 
TITLE   = 5/-2/~ ? 6-HR TOTAL PRECIPITATION (IN) for ${storm_id}|~6-HR TOTAL PCPN
IJSKIP  = 0
CLEAR   = yes
run

GDFILE  = HAFS:${storm_id} | ${YYMMDD}/${cyc}00
GDATTIM = F024-F126-06
GDPFUN  = p24i
TITLE   = 5/-2/~ ? 24-HR TOTAL PRECIPITATION (IN) for ${storm_id}|~24-HR TOTAL PCPN
run

exit
EOF
export err=$?; err_chk

gpend
export err=$?; err_chk

# Get the ASCII file that contains track information.  This will be used to create the TRACK
# in the metafile.  It is important that this file below exist each time.
statfile="${out_prefix}.${RUN}.grib.stats.short"
# Make sure gempak files are ready
MAX_WAIT_TIME=${MAX_WAIT_TIME:-1200}
n=0
while [ $n -le ${MAX_WAIT_TIME} ]; do
  if [ -f ${COMIN}/${statfile} ]; then
    break
  else
    sleep 10s
  fi
  if [ $n -gt ${MAX_WAIT_TIME} ] && [ ! -f ${COMIN}/${statfile} ]; then
    echo "FATAL ERROR: Waited ${COMIN}/${statfile} too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
    exit 1
  fi
  n=$((n+10))
done
${FCP} ${COMIN}/${statfile} ./

numlines=$(cat $statfile | wc -l)
cnt=1
while [ $cnt -le $numlines ]; do
  line=$( head -n $cnt $statfile | tail -1)
  ishour=`echo $line | cut -c1-5`
  if [[ $ishour == "HOUR:" ]]; then
    echo $line >> stats.short
  fi
  cnt=$((cnt+1))
done

for nested_grid in ${DATA}/${NET}/${NET}n_${PDY}${cyc}f???_${storm_id}; do
  fhr=$( printf "%d" $(echo $nested_grid | rev | cut -c5-7 | rev) )
  np=$((fhr / 6))
  np=$((np+1))

  $GEMEXE/gdcntr <<EOF
clear   = yes
LATLON  = ${latlon}
DEVICE  = ${device}
GFUNC   = hght
GLEVEL  = 500
GVCORD  = pres
CINT    = 9990
CTYPE   = c
GDFILE  = $nested_grid
GDATTIM = F${fhr}
TITLE   = 5/0/HAFS ~ FORECAST TRACK for ${storm_id}|~TRACK
SCALE   = 0
FINT    = 
FLINE   = 
HILO    = 
HLSYM   = 
GVECT   = 
WIND    = 
run
EOF
  export err=$?; err_chk

  c="commands.tmp"

  cat >$c <<EOF
ginitp
0
gsline
1 0 2 0
gscolr
5
EOF

  if [ $np -gt 1 ]; then
    echo gline  >>$c
    echo m  >>$c
    sed "s/HOUR:/HOUR: /g" stats.short | awk '$1 == "HOUR:" {print $0}' | awk '$2 <= n {print $6,$4}' n=$fhr | wc -l  >>$c
    sed "s/HOUR:/HOUR: /g" stats.short | awk '$1 == "HOUR:" {print $0}' | awk '$2 <= n {print $6,$4}' n=$fhr >>$c
  fi

  cat >>$c <<EOF
gsmrkr
16 0 1.5 2
gmark
m
EOF

  sed "s/HOUR:/HOUR: /g" stats.short | awk '$1 == "HOUR:" {print $0}' | awk '$2 <= n {print $6,$4}' n=$fhr | wc -l  >>$c
  sed "s/HOUR:/HOUR: /g" stats.short | awk '$1 == "HOUR:" {print $0}' | awk '$2 <= n {print $6,$4}' n=$fhr >>$c

  cat >>$c <<EOF
geplot
gsline
1 0 1 0
exit
EOF

  $GEMEXE/atest <$c
  export err=$?; err_chk

done

$GEMEXE/gpend
export err=$?; err_chk

############################################################
# Gempak does not always have a non-zero return code when it
# cannot produce the desired grid. Check for this case here.
############################################################
ls -l nest.nmeta
export err=$?; err_chk

GMETAF=${COMOUT}/gempak/${storm_id}/meta/${RUN}_${PDY}_${cyc}_${storm_id}_nest
if [ ${SENDCOM:-YES} = "YES" ]; then
  ${FCP} nest.nmeta ${GMETAF}
  if [ ${SENDDBN:-NO} = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL $(echo ${RUN} | tr [a-z] [A-Z])_METAFILE $job ${GMETAF}
  fi
fi

