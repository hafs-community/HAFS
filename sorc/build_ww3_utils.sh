#!/bin/sh
set -xeu

source ./machine-setup.sh > /dev/null 2>&1

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

module use ../modulefiles
module load hafs.${target}
module list

if [ $target = hera ]; then target=hera.intel ; fi
if [ $target = orion ]; then target=orion.intel ; fi
if [ $target = jet ]; then target=jet.intel ; fi

#module use hafs_forecast.fd/modulefiles
#module load ufs_${target}

cd hafs_forecast.fd/WW3

export WW3_DIR=$( pwd -P )/model
export WW3_BINDIR="${WW3_DIR}/bin"
export WW3_TMPDIR=${WW3_DIR}/tmp
export WW3_EXEDIR=${WW3_DIR}/exe
export WW3_COMP=$target 
export WW3_CC=gcc
export WW3_F90=gfortran
export SWITCHFILE="${WW3_DIR}/esmf/switch"

export WWATCH3_ENV=${WW3_BINDIR}/wwatch3.env
export PNG_LIB=${PNG_LIB:-$PNG_ROOT/lib64/libpng.a}
export Z_LIB=${Z_LIB:-$ZLIB_ROOT/lib/libz.a}
export JASPER_LIB=${JASPER_LIB:-$JASPER_ROOT/lib64/libjasper.a}
export WWATCH3_NETCDF=NC4
export NETCDF_CONFIG=${NETCDF_ROOT:-${NETCDF}}/bin/nc-config

rm -f $WWATCH3_ENV
echo '#'                                              > $WWATCH3_ENV
echo '# ---------------------------------------'      >> $WWATCH3_ENV
echo '# Environment variables for wavewatch III'      >> $WWATCH3_ENV
echo '# ---------------------------------------'      >> $WWATCH3_ENV
echo '#'                                              >> $WWATCH3_ENV
echo "WWATCH3_LPR      ${PRINTER:-printer}"           >> $WWATCH3_ENV
echo "WWATCH3_F90      $WW3_F90"                      >> $WWATCH3_ENV
echo "WWATCH3_CC       $WW3_CC"                       >> $WWATCH3_ENV
echo "WWATCH3_DIR      $WW3_DIR"                      >> $WWATCH3_ENV
echo "WWATCH3_TMP      $WW3_TMPDIR"                   >> $WWATCH3_ENV
echo 'WWATCH3_SOURCE   yes'                           >> $WWATCH3_ENV
echo 'WWATCH3_LIST     yes'                           >> $WWATCH3_ENV
echo ''                                               >> $WWATCH3_ENV

${WW3_BINDIR}/w3_clean -m 
${WW3_BINDIR}/w3_setup -q -c $WW3_COMP $WW3_DIR

# Check NetCDF setup
if [ -z "$WWATCH3_NETCDF" ]; then
  ww3_NetCDF=""
else
  ww3_NetCDF="ww3_prnc ww3_ounf ww3_ounp ww3_bounc"
fi

cd ${WW3_BINDIR}
mkdir -p ../exec/
rm -f ../exec/*

# Process switch files
echo $(cat ${SWITCHFILE}) > ${WW3_BINDIR}/tempswitch

cp tempswitch switch.hold
# Update switch for NOMPI
sed -e 's/DIST/SHRD/g' \
    -e 's/OMPG //g'\
    -e 's/OMPH //g'\
    -e 's/OMPX //g'\
    -e 's/MPI //g'\
    -e "s/PDLIB/ /g"\
    switch.hold > switch.shrd
# Update switch for MPI
sed 's/SHRD/DIST MPI/g' switch.shrd > switch.MPI
# Update switch for grib
sed -e 's/NOGRB/NCEP2 NCO/g' switch.shrd > switch.GRIB

# Build NOMPI exes for prep and post (except grib)
cp switch.shrd switch
#ww3_utils="ww3_grid ww3_strt ww3_prep ww3_outf ww3_outp ww3_trck ww3_gspl gx_outf gx_outp ww3_systrk ww3_bound ww3_gint ${ww3_NetCDF}"
#ww3_utils="ww3_grid ww3_strt ww3_prep ww3_outf ww3_outp ww3_trck ww3_gspl gx_outf gx_outp ww3_bound ww3_gint ${ww3_NetCDF}"
ww3_utils="ww3_grid ww3_strt ww3_prep ww3_outf ww3_outp ww3_trck ww3_gspl ww3_bound ww3_gint ${ww3_NetCDF}"
${WW3_BINDIR}/w3_make ${ww3_utils}
cd ../exe
cp switch ../exec/switch.NOMPI
cp exec_type ../exec/exec_type.NOMPI
cp ${ww3_utils} ../exec/
cd ../bin

#${WW3_BINDIR}/w3_make ww3_outp
#cd ../exe
#cp ww3_outp ../exec/
#cd ../bin

# Build ww3_grib
cp switch.GRIB switch
${WW3_BINDIR}/w3_make ww3_grib
cd ../exe
cp switch ../exec/switch.GRIB
cp exec_type ../exec/exec_type.GRIB
cp ww3_grib ../exec/
cd ../bin

# Build MPI exes
#cp switch.MPI switch
#${WW3_BINDIR}/w3_make ww3_shel ww3_multi ww3_sbs1
#cd ../exe
#cp switch ../exec/switch.MPI
#cp exec_type ../exec/exec_type.MPI
#cp ww3_shel ww3_multi ww3_sbs1 ../exec/
#cd ../bin

# Reset switch file
cp switch.hold switch

#${WW3_BINDIR}/w3_clean -c
#rm -f ${WW3_BINDIR}/tempswitch
#rm -f switch.hold switch.shrd switch.GRIB switch.MPI

