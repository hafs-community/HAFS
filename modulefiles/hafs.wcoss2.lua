help([[
loads HAFS prerequisites on Cactus and Dogwood
]])

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.3.3"
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))

intel_ver=os.getenv("intel_ver") or "19.1.3.304"
load(pathJoin("intel", intel_ver))

craype_ver=os.getenv("craype_ver") or "2.7.17"
load(pathJoin("craype", craype_ver))

cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.19"
load(pathJoin("cray-mpich", cray_mpich_ver))

cray_pals_ver=os.getenv("cray_pals_ver") or "1.2.2"
load(pathJoin("cray-pals", cray_pals_ver))

cmake_ver=os.getenv("cmake_ver") or "3.20.2"
load(pathJoin("cmake", cmake_ver))

python_ver=os.getenv("python_ver") or "3.8.6"
load(pathJoin("python", python_ver))

prepend_path("MODULEPATH", "/apps/test/hpc-stack/i-19.1.3.304__m-8.1.12__h-1.14.0__n-4.9.2__p-2.5.10__e-8.4.2/modulefiles/compiler/intel/19.1.3.304")
prepend_path("MODULEPATH", "/apps/test/hpc-stack/i-19.1.3.304__m-8.1.12__h-1.14.0__n-4.9.2__p-2.5.10__e-8.4.2/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.12")

jasper_ver=os.getenv("jasper_ver") or "2.0.25"
load(pathJoin("jasper", jasper_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.11"
load(pathJoin("zlib", zlib_ver))

libpng_ver=os.getenv("libpng_ver") or "1.6.37"
load(pathJoin("libpng", libpng_ver))

libjpeg_ver=os.getenv("libjpeg_ver") or "9c"
load(pathJoin("libjpeg", libjpeg_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.14.0"
load(pathJoin("hdf5", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.9.2"
load(pathJoin("netcdf", netcdf_ver))

fms_ver=os.getenv("fms_ver") or "2023.01"
load(pathJoin("fms", fms_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

crtm_ver=os.getenv("crtm_ver") or "2.4.0"
load(pathJoin("crtm", crtm_ver))

g2_ver=os.getenv("g2_ver") or "3.4.5"
load(pathJoin("g2", g2_ver))

g2tmpl_ver=os.getenv("g2tmpl_ver") or "1.10.2"
load(pathJoin("g2tmpl", g2tmpl_ver))

bufr_ver=os.getenv("bufr_ver") or "12.0.0"
load(pathJoin("bufr", bufr_ver))

ip_ver=os.getenv("ip_ver") or "4.0.0"
load(pathJoin("ip", ip_ver))

sp_ver=os.getenv("sp_ver") or "2.3.3"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
load(pathJoin("w3emc", w3emc_ver))

nemsio_ver=os.getenv("nemsio_ver") or "2.5.2"
load(pathJoin("nemsio", nemsio_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
load(pathJoin("sfcio", sfcio_ver))

wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
load(pathJoin("wrf_io", wrf_io_ver))

prod_util_ver=os.getenv("prod_util_ver") or "2.0.14"
load(pathJoin("prod_util", prod_util_ver))

grib_util_ver=os.getenv("grib_util_ver") or "1.2.4"
load(pathJoin("grib_util", grib_util_ver))

wgrib2_ver=os.getenv("wgrib2_ver") or "2.0.8_wmo"
load(pathJoin("wgrib2", wgrib2_ver))

cfp_ver=os.getenv("cfp_ver") or "2.0.4"
load(pathJoin("cfp", cfp_ver))

udunits_ver=os.getenv("udunits_ver") or "2.2.28"
load(pathJoin("udunits", udunits_ver))

gsl_ver=os.getenv("gsl_ver") or "2.7"
load(pathJoin("gsl", gsl_ver))

nco_ver=os.getenv("nco_ver") or "5.0.6"
load(pathJoin("nco", nco_ver))

bufr_dump_ver=os.getenv("bufr_dump_ver") or "1.1.2"
load(pathJoin("bufr_dump", bufr_dump_ver))

cdo_ver=os.getenv("cdo_ver") or "1.9.8"
load(pathJoin("cdo", cdo_ver))

ncdiag_ver=os.getenv("ncdiag_ver") or "1.0.0"
load(pathJoin("ncdiag", ncdiag_ver))

ncio_ver=os.getenv("ncio_ver") or "1.1.2"
load(pathJoin("ncio", ncio_ver))

pio_ver=os.getenv("pio_ver") or "2.5.10"
load(pathJoin("pio", pio_ver))

esmf_ver=os.getenv("esmf_ver") or "8.4.2"
load(pathJoin("esmf", esmf_ver))

gftl_shared_ver=os.getenv("gftl_shared_ver") or "v1.5.0"
load(pathJoin("gftl-shared", gftl_shared_ver))

mapl_ver=os.getenv("mapl_ver") or "2.35.2-esmf-8.4.2"
load(pathJoin("mapl", mapl_ver))

prepend_path("MODULEPATH", "/apps/prod/lmodules/INTEL_cray_mpich/19.1.3.304/cray-mpich/8.1.9")
scotch_ver=os.getenv("scotch_ver") or "7.0.4"
load(pathJoin("scotch", scotch_ver))

setenv("CC", "cc")
setenv("CXX", "CC")
setenv("FC", "ftn")
setenv("CMAKE_C_COMPILER", "cc")
setenv("CMAKE_CXX_COMPILER", "CC")
setenv("CMAKE_Fortran_COMPILER", "ftn")
setenv("CMAKE_Platform", "wcoss2")

whatis("Description: HAFS build/run environment")
