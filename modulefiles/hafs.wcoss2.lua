help([[
loads HAFS application level modulefile on WCOSS2 (Cactus and Dogwood)
]])

envvar_ver=os.getenv("envvar_ver") or "1.0"
load(pathJoin("envvar", envvar_ver))

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.1.0"
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))

intel_ver=os.getenv("intel_ver") or "19.1.3.304"
load(pathJoin("intel", intel_ver))

craype_ver=os.getenv("craype_ver") or "2.7.13"
load(pathJoin("craype", craype_ver))

cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.7"
load(pathJoin("cray-mpich", cray_mpich_ver))

cray_pals_ver=os.getenv("cray_pals_ver") or "1.0.12"
load(pathJoin("cray-pals", cray_pals_ver))

python_ver=os.getenv("python_ver") or "3.8.6"
load(pathJoin("python", python_ver))

cmake_ver=os.getenv("cmake_ver") or "3.20.2"
load(pathJoin("cmake", cmake_ver))

setenv("HPC_OPT", "/apps/ops/para/libs")
prepend_path("MODULEPATH", "/apps/ops/para/libs/modulefiles/compiler/intel/19.1.3.304")
prepend_path("MODULEPATH", "/apps/ops/para/libs/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.7")

jasper_ver=os.getenv("jasper_ver") or "2.0.25"
load(pathJoin("jasper", jasper_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.11"
load(pathJoin("zlib", zlib_ver))

libpng_ver=os.getenv("libpng_ver") or "1.6.37"
load(pathJoin("libpng", libpng_ver))

libjpeg_ver=os.getenv("libjpeg_ver") or "9c"
load(pathJoin("libjpeg", libjpeg_ver))
setenv("JPEG_LIBRARIES", "/apps/spack/libjpeg/9c/intel/19.1.3.304/jkr3isi257ktoouprwaxcn4twtye747z/lib")

hdf5_ver=os.getenv("hdf5_ver") or "1.10.6"
load(pathJoin("hdf5", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf", netcdf_ver))

pio_ver=os.getenv("pio_ver") or "2.5.7"
load(pathJoin("pio", pio_ver))

esmf_ver=os.getenv("esmf_ver") or "8.3.0b09"
load(pathJoin("esmf", esmf_ver))

fms_ver=os.getenv("fms_ver") or "2022.04"
load(pathJoin("fms", fms_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

crtm_ver=os.getenv("crtm_ver") or "2.4.0"
load(pathJoin("crtm", crtm_ver))

g2_ver=os.getenv("g2_ver") or "3.4.5"
load(pathJoin("g2", g2_ver))

g2tmpl_ver=os.getenv("g2tmpl_ver") or "1.10.2"
load(pathJoin("g2tmpl", g2tmpl_ver))

ip_ver=os.getenv("ip_ver") or "3.3.3"
load(pathJoin("ip", ip_ver))

nemsio_ver=os.getenv("nemsio_ver") or "2.5.2"
load(pathJoin("nemsio", nemsio_ver))

sp_ver=os.getenv("sp_ver") or "2.3.3"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
load(pathJoin("w3emc", w3emc_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nmc_ver))

g2c_ver=os.getenv("g2c_ver") or "1.6.4"
load(pathJoin("g2c", g2c_ver))

bufr_ver=os.getenv("bufr_ver") or "11.6.0"
load(pathJoin("bufr", bufr_ver))

gfsio_ver=os.getenv("gfsio_ver") or "1.4.1"
load(pathJoin("gfsio", gfsio_ver))

landsfcutil_ver=os.getenv("landsfcutil_ver") or "2.4.1"
load(pathJoin("landsfcutil", landsfcutil_ver))

nemsiogfs_ver=os.getenv("nemsiogfs_ver") or "2.5.3"
load(pathJoin("nemsiogfs", nemsiogfs_ver))

sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
load(pathJoin("sfcio", sfcio_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
load(pathJoin("wrf_io", wrf_io_ver))

prod_util_ver=os.getenv("prod_util_ver") or "2.0.13"
load(pathJoin("prod_util", prod_util_ver))

grib_util_ver=os.getenv("grib_util_ver") or "1.2.4"
load(pathJoin("grib_util", grib_util_ver))

wgrib2_ver=os.getenv("wgrib2_ver") or "2.0.8_wmo"
load(pathJoin("wgrib2", wgrib2_ver))

cfp_ver=os.getenv("cfp_ver") or "2.0.4"
load(pathJoin("cfp", cfp_ver))

gsl_ver=os.getenv("gsl_ver") or "2.7"
load(pathJoin("gsl", gsl_ver))

udunits_ver=os.getenv("udunits_ver") or "2.2.28"
load(pathJoin("udunits", udunits_ver))

nco_ver=os.getenv("nco_ver") or "4.7.9"
load(pathJoin("nco", nco_ver))

ncio_ver=os.getenv("ncio_ver") or "1.1.2"
load(pathJoin("ncio",ncio_ver))

ncdiag_ver=os.getenv("ncdiag_ver") or "1.0.0"
load(pathJoin("ncdiag",ncdiag_ver))

setenv("CMAKE_C_COMPILER", "cc")
setenv("CMAKE_CXX_COMPILER", "CC")
setenv("CMAKE_Fortran_COMPILER", "ftn")
setenv("CMAKE_Platform", "wcoss2")

prepend_path("MODULEPATH", "/apps/ops/test/nco/modulefiles")

rocoto_ver=os.getenv("rocoto_ver") or "1.3.5"
load(pathJoin("core", "rocoto", rocoto_ver))

cdo_ver=os.getenv("cdo_ver") or "1.9.8"
load(pathJoin("cdo", cdo_ver))

whatis("Description: HAFS Applicationenvironment")
