help([[
loads HAFS prerequisites on GAEA C6
]])

prepend_path("MODULEPATH", "/autofs/ncrc-svm1_proj/epic/spack-stack/spack-stack-1.6.0/envs/unified-env-c6/install/modulefiles/Core")

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.5.0"
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))

stack_intel_ver=os.getenv("stack_intel_ver") or "2023.2.0"
load(pathJoin("stack-intel", stack_intel_ver))

stack_mpich_ver=os.getenv("stack_mpich_ver") or "8.1.29"
load(pathJoin("stack-cray-mpich", stack_mpich_ver))

craype_ver=os.getenv("craype_ver") or "2.7.30"
load(pathJoin("craype", craype_ver))

cmake_ver=os.getenv("cmake_ver") or "3.23.1"
load(pathJoin("cmake", cmake_ver))

python_ver=os.getenv("python_ver") or "3.10.13"
load(pathJoin("stack-python", python_ver))

jasper_ver=os.getenv("jasper_ver") or "2.0.32"
load(pathJoin("jasper", jasper_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.13"
load(pathJoin("zlib", zlib_ver))

libpng_ver=os.getenv("libpng_ver") or "1.6.37"
load(pathJoin("libpng", libpng_ver))

libjpeg_ver=os.getenv("libjpeg_ver") or "2.1.0"
load(pathJoin("libjpeg", libjpeg_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.14.0"
load(pathJoin("hdf5", hdf5_ver))

netcdf_c_ver=os.getenv("netcdf_c_ver") or "4.9.2"
load(pathJoin("netcdf-c", netcdf_c_ver))

netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "4.6.1"
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))

parallelio_ver=os.getenv("parallelio_ver") or "2.5.10"
load(pathJoin("parallelio", parallelio_ver))

fms_ver=os.getenv("fms_ver") or "2023.04"
load(pathJoin("fms", fms_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

crtm_ver=os.getenv("crtm_ver") or "2.4.0.1"
load(pathJoin("crtm", crtm_ver))

g2_ver=os.getenv("g2_ver") or "3.4.5"
load(pathJoin("g2", g2_ver))

g2tmpl_ver=os.getenv("g2tmpl_ver") or "1.10.2"
load(pathJoin("g2tmpl", g2tmpl_ver))

bufr_ver=os.getenv("bufr_ver") or "12.0.1"
load(pathJoin("bufr", bufr_ver))

ip_ver=os.getenv("ip_ver") or "4.3.0"
load(pathJoin("ip", ip_ver))

sp_ver=os.getenv("sp_ver") or "2.5.0"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.10.0"
load(pathJoin("w3emc", w3emc_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nco_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
load(pathJoin("sfcio", sfcio_ver))

wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
load(pathJoin("wrf-io", wrf_io_ver))

prod_util_ver=os.getenv("prod_util_ver") or "2.1.1"
load(pathJoin("prod_util", prod_util_ver))

grib_util_ver=os.getenv("grib_util_ver") or "1.3.0"
load(pathJoin("grib-util", grib_util_ver))

wgrib2_ver=os.getenv("wgrib2_ver") or "2.0.8"
load(pathJoin("wgrib2", wgrib2_ver))

gsl_ver=os.getenv("gsl_ver") or "2.7.1"
load(pathJoin("gsl", gsl_ver))

nco_ver=os.getenv("nco_ver") or "5.0.6"
load(pathJoin("nco", nco_ver))

cdo_ver=os.getenv("cdo_ver") or "2.2.0"
load(pathJoin("cdo", cdo_ver))

ncio_ver=os.getenv("ncio_ver") or "1.1.2"
load(pathJoin("ncio", ncio_ver))

xarray_ver=os.getenv("xarray_ver") or "2023.7.0"
load(pathJoin("py-xarray",xarray_ver))

pynetcdf4_ver=os.getenv("pynetcdf4_ver") or "1.5.8"
load(pathJoin("py-netcdf4", pynetcdf4_ver))

pyyaml_ver=os.getenv("pyyaml_ver") or "6.0"
load(pathJoin("py-pyyaml", pyyaml_ver))

esmf_ver=os.getenv("esmf_ver") or "8.6.0"
load(pathJoin("esmf", esmf_ver))

prepend_path("MODULEPATH", "/autofs/ncrc-svm1_proj/hurr1/hafs/shared/modulefiles")
rocoto_ver=os.getenv("rocoto_ver") or "1.3.7_fix"
load(pathJoin("rocoto", rocoto_ver))

prepend_path("MODULEPATH", "/usw/hpss/modulefiles")
load("hsi")

setenv("CC", "cc")
setenv("CXX", "CC")
setenv("FC", "ftn")
setenv("CMAKE_C_COMPILER", "cc")
setenv("CMAKE_CXX_COMPILER", "CC")
setenv("CMAKE_Fortran_COMPILER", "ftn")
setenv("CMAKE_Platform", "gaeac6")

whatis("Description: HAFS build/run environment")
