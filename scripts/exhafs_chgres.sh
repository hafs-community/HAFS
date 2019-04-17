#!/bin/ksh
############################################################################
# Script name:		run_chgres.sh
# Script description:	Makes ICs on fv3 globally uniform cubed-sphere grid 
#                       using operational GFS initial conditions.
# Script history log:
#   1) 2016-09-30       Fanglin Yang
#   2) 2017-02-08	Fanglin Yang and George Gayno
#			Use the new CHGRES George Gayno developed.
#   3) 2017-03-08	Fanglin Yang
#			Generalized/streamlined script and enabled to run
#			on multiple platforms.
#   4) 2017-03-20	Fanglin Yang
#			Added option to process NEMS GFS initial conditions.
#			Switch to use ush/global_chgres.sh.
#   5) 2018-04-02	Ben Blake
#			Script for 3-km FV3 nest created from
#			ush/global_chgres_driver.sh
#   6) 2018-04-05	Ben Blake
#			Script updated to handle global_chgres_driver.sh
#			changes
#   7) 2018-04-13	Ben Blake
#			Various settings moved to JFV3_CHGRES J-job
#   8) 2018-06-14       Jim Abeles
#                       Add capability to use files from FV3GFS parallel
############################################################################
set -x

export CDUMP=gfs		# gfs or gdas
export CDAS=gfs
export LEVS=${LEVS:-65}
export LSOIL=4

export ictype=${ictype:-pfv3gfs}
export nst_anl=.false.          # false or true to include NST analysis
export gtype=${gtype:-regional}           # grid type = uniform, stretch, nest, or stand alone regional

if [ $gtype = uniform ];  then
  echo "creating uniform ICs"
  export ntiles=6
elif [ $gtype = stretch ]; then
  echo "creating stretched ICs"
  export ntiles=6
elif [ $gtype = nest ]; then
  echo "creating nested ICs"
  export ntiles=7
elif [ $gtype = regional ]; then
  echo "creating standalone regional ICs"
  export ntiles=1
  export TILE_NUM=7
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest, or regional"
  exit 9
fi


# input data is FV3GFS (ictype is 'pfv3gfs')

if [ $ictype = pfv3gfs ]; then		# input is fv3gfs parallel
  export ATMANL=$INIDIR/${CDUMP}.t${cyc}z.atmanl.nemsio
  export SFCANL=$INIDIR/${CDUMP}.t${cyc}z.sfcanl.nemsio
else
  if [ ${ATMANL:-"NULL"} = "NULL" ]; then
   if [ -s ${INIDIR}/gfnanl.${CDUMP}.$CDATE ]; then
    export ATMANL=$INIDIR/gfnanl.${CDUMP}.$CDATE
    export SFCANL=$INIDIR/sfnanl.${CDUMP}.$CDATE
   else
    export ATMANL=$INIDIR/${CDUMP}.t${cyc}z.atmanl.nemsio
    export SFCANL=$INIDIR/${CDUMP}.t${cyc}z.sfcanl.nemsio
   fi
  fi
fi

export NSTANL="NULL"
export SOILTYPE_INP=statsgo
export VEGTYPE_INP=igbp
export nopdpvv=.true.

LONB_ATM=0	# not used for ops files
LATB_ATM=0
JCAP_CASE=$((CRES*2-2))
LONB_SFC=$((CRES*4))
LATB_SFC=$((CRES*2))
if [ $CRES -gt 768 -o $gtype = stretch -o $gtype = nest ]; then
  JCAP_CASE=1534
  LONB_SFC=3072
  LATB_SFC=1536
fi

# to use new albedo, soil/veg type
export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=${LANDICE_OPT:-2}
export IALB=1
export SOILTYPE_OUT=statsgo
export VEGTYPE_OUT=igbp
export FNZORC=igbp

export SIGLEVEL=${FIXam}/global_hyblev.l${LEVS}.txt
if [ $LEVS = 128 ]; then export SIGLEVEL=${FIXam}/global_hyblev.l${LEVS}B.txt; fi
export FNGLAC=${FIXam}/global_glacier.2x2.grb
export FNMXIC=${FIXam}/global_maxice.2x2.grb
export FNTSFC=${FIXam}/cfs_oi2sst1x1monclim19822001.grb
export FNSNOC=${FIXam}/global_snoclim.1.875.grb
export FNALBC2=${FIXam}/global_albedo4.1x1.grb
export FNAISC=${FIXam}/cfs_ice1x1monclim19822001.grb
export FNTG3C=${FIXam}/global_tg3clim.2.6x1.5.grb
export FNVEGC=${FIXam}/global_vegfrac.0.144.decpercent.grb
export FNVMNC=${FIXam}/global_shdmin.0.144x0.144.grb
export FNVMXC=${FIXam}/global_shdmax.0.144x0.144.grb
export FNSLPC=${FIXam}/global_slope.1x1.grb
export FNMSKH=${FIXam}/seaice_newland.grb
export FNSMCC=${FIXam}/global_soilmgldas.statsgo.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.grb
export FNSOTC=${FIXam}/global_soiltype.statsgo.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNVETC=${FIXam}/global_vegtype.igbp.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNABSC=${FIXam}/global_mxsnoalb.uariz.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNALBC=${FIXam}/global_snowfree_albedo.bosu.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb

#export FNALBC2=${FIXnew}/C768.facsf.tile7.nc
#export FNALBC=${FIXnew}/C768.snowfree_albedo.tile7.nc
#export FNTG3C=${FIXnew}/C768.substrate_temperature.tile7.nc
#export FNVEGC=${FIXnew}/C768.vegetation_greenness.tile7.nc
#export FNVETC=${FIXnew}/C768.vegetation_type.tile7.nc
#export FNSTOC=${FIXnew}/C768.soil_type.tile7.nc
#export FNVMNC=${FIXnew}/C768.vegetation_greenness.tile7.nc
#export FNVMXC=${FIXnew}/C768.vegetation_greenness.tile7.nc
#export FNSLPC=${FIXnew}/C768.slope_type.tile7.nc
#export FNABSC=${FIXnew}/C768.maximum_snow_albedo.tile7.nc

#
# For a regional run, if REGIONAL=2 (generate boundary data only) this script is called multiple times
# so that each boundary time other than hour 0 will be done individually. This allows multiple instances
# of chgres to execute simultaneously.
#

if [ $REGIONAL -ne 2 ]; then		# REGIONAL -ne 2 is for uniform and regional hour 0

#--------------------------------------------------
# Convert atmospheric file.
#--------------------------------------------------
  export CHGRESVARS="use_ufo=.false.,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
  export SIGINP=$ATMANL
  export SFCINP=NULL
  export NSTINP=NULL
  export JCAP=$JCAP_CASE
  export LATB=$LATB_ATM
  export LONB=$LONB_ATM

  $CHGRESSH
  rc=$?
  if [[ $rc -ne 0 ]] ; then
   echo "***ERROR*** rc= $rc"
   exit $rc
  fi

  mv ${DATA}/gfs_data.tile*.nc $OUTDIR/.
  mv ${DATA}/gfs_ctrl.nc       $OUTDIR/.
  if [ $gtype = regional ]; then
    mv ${DATA}/gfs_bndy.tile7.nc $OUTDIR/gfs_bndy.tile7.000.nc
  fi

#---------------------------------------------------
# Convert surface and nst files one tile at a time.
#---------------------------------------------------

  export CHGRESVARS="use_ufo=.true.,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
  export SIGINP=NULL
  export SFCINP=$SFCANL
  export NSTINP=$NSTANL
  export JCAP=$JCAP_CASE
  export LATB=$LATB_SFC
  export LONB=$LONB_SFC

  if [ $gtype = regional ]; then
    $CHGRESSH
    mv ${DATA}/out.sfc.tile${TILE_NUM}.nc $OUTDIR/sfc_data.tile${TILE_NUM}.nc
    rc=$?
    if [[ $rc -ne 0 ]] ; then
      echo "***ERROR*** rc= $rc"
      exit $rc
    fi
  else
    tile=1
    while [ $tile -le $ntiles ]; do
      export TILE_NUM=$tile
      $CHGRESSH
      rc=$?
      if [[ $rc -ne 0 ]] ; then
        echo "***ERROR*** rc= $rc"
        exit $rc
      fi
      mv ${DATA}/out.sfc.tile${tile}.nc $OUTDIR/sfc_data.tile${tile}.nc
      tile=`expr $tile + 1 `
    done
  fi

else # REGIONAL = 2, just generate boundary data

  export CHGRESVARS="use_ufo=.false.,nst_anl=$nst_anl,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
  export ATMANL=$INIDIR/${CDUMP}.t${cyc}z.atmf${bchour}.nemsio
  export SIGINP=$ATMANL
  export SFCIMP=NULL
  export NSTINP=NULL
  export LATB=$LATB_ATM
  export LONB=$LONB_ATM

  $CHGRESSH
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "***ERROR*** rc= $rc"
    exit $rc
  fi
  
  if [ -r ${DATA}/gfs_bndy.tile7.nc ]; then
    mv ${DATA}/gfs_bndy.tile7.nc $OUTDIR/gfs_bndy.tile7.${bchour}.nc
  else
    echo "FATAL ERROR: Boundary file was not created successfully."
    err_exit
  fi

fi

exit 0
