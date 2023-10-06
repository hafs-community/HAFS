#!/bin/sh
set -xeu

HOMEhafs="../../../../"
source ${HOMEhafs}/sorc/machine-setup.sh > /dev/null 2>&1

module use ${HOMEhafs}/modulefiles
module load modulefile.hafs.${target}
module list

export NETCDF_INCLUDE="-I${NETCDF}/include"
export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf"
export HDF5_INCLUDE=${HDF5_INCLUDE:-"-I${HDF5_INCLUDES:--I${HDF5}/include}"}
export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5_LIBRARIES:--L${HDF5}/lib} -lhdf5_hl -lhdf5"}
export ZLIB_INCLUDE=${ZLIB_INCLUDE:-"-I${ZLIB_INCLUDES:--I${ZLIB_ROOT}/include}"}
export ZLIB_LDFLAGS=${ZLIB_LDFLAGS:-"-L${ZLIB_LIBRARIES:--L${ZLIB_ROOT}/lib} -lz -ldl -lm"}

rm -f *.o *.mod *.exe

#FFLAGS="-O0 -g -traceback -check all -fp-model precise -assume byterecl -convert big_endian"
#LDFLAGS="-O0 -g -traceback -check all -fp-model precise -assume byterecl -convert big_endian"
 FFLAGS="-O3 -g -traceback -fp-model precise -assume byterecl -convert big_endian"
 LDFLAGS="-O3 -g -traceback -fp-model precise -assume byterecl -convert big_endian"

mpif90 -f90=ifort -c ${FFLAGS} module_mpi.f90
mpif90 -f90=ifort -c ${FFLAGS} module_structure.f90
mpif90 -f90=ifort -c ${FFLAGS} sub_tcinfo.f90
mpif90 -f90=ifort -c ${FFLAGS} -I${NETCDF}/include sub_netcdf.f90
mpif90 -f90=ifort -c ${FFLAGS} sub_tools.f90
mpif90 -f90=ifort -c ${FFLAGS} sub_grids.f90
mpif90 -f90=ifort -c ${FFLAGS} sub_wind_process.f90
mpif90 -f90=ifort -c ${FFLAGS} -I${NETCDF}/include sub_hafs_remap.f90
mpif90 -f90=ifort -c ${FFLAGS} -I${NETCDF}/include sub_hafsvi_proc.f90
mpif90 -f90=ifort -c ${FFLAGS} hafs_datool.f90

#mpif90 -f90=ifort -o hafs_datool.exe ${LDFLAGS} module_mpi.o module_structure.o sub_tcinfo.o sub_netcdf.o sub_tools.o sub_grids.o sub_hafs_remap.o sub_hafsvi_preproc.o hafs_datool.o -L${NETCDF}/lib -lnetcdff -lnetcdf -L${HDF5_LIBRARIES} -lhdf5_hl -lhdf5 -lz
mpif90 -f90=ifort -o hafs_datool.exe ${LDFLAGS} module_mpi.o module_structure.o sub_tcinfo.o sub_netcdf.o sub_tools.o sub_grids.o sub_wind_process.o sub_hafs_remap.o sub_hafsvi_proc.o hafs_datool.o -L${NETCDF}/lib -lnetcdff -lnetcdf -L${HDF5_LIBRARIES} -lhdf5_hl -lhdf5 -lz

cp -p hafs_datool.exe ../../exec/hafs_datool.x
