help([[
loads HAFS prerequisites on Cactus and Dogwood
]])

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver")
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))

intel_ver=os.getenv("intel_ver")
load(pathJoin("intel", intel_ver))

craype_ver=os.getenv("craype_ver")
load(pathJoin("craype", craype_ver))

cray_mpich_ver=os.getenv("cray_mpich_ver")
load(pathJoin("cray-mpich", cray_mpich_ver))

cray_pals_ver=os.getenv("cray_pals_ver")
load(pathJoin("cray-pals", cray_pals_ver))

cmake_ver=os.getenv("cmake_ver")
load(pathJoin("cmake", cmake_ver))

python_ver=os.getenv("python_ver")
load(pathJoin("python", python_ver))

jasper_ver=os.getenv("jasper_ver")
load(pathJoin("jasper", jasper_ver))

zlib_ver=os.getenv("zlib_ver")
load(pathJoin("zlib", zlib_ver))

libpng_ver=os.getenv("libpng_ver")
load(pathJoin("libpng", libpng_ver))

libjpeg_ver=os.getenv("libjpeg_ver")
load(pathJoin("libjpeg", libjpeg_ver))

hdf5_ver=os.getenv("hdf5_ver")
load(pathJoin("hdf5-A", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver")
load(pathJoin("netcdf-A", netcdf_ver))

fms_ver=os.getenv("fms_ver")
load(pathJoin("fms", fms_ver))

bacio_ver=os.getenv("bacio_ver")
load(pathJoin("bacio", bacio_ver))

crtm_ver=os.getenv("crtm_ver")
load(pathJoin("crtm", crtm_ver))

g2_ver=os.getenv("g2_ver")
load(pathJoin("g2", g2_ver))

g2tmpl_ver=os.getenv("g2tmpl_ver")
load(pathJoin("g2tmpl", g2tmpl_ver))

bufr_ver=os.getenv("bufr_ver")
load(pathJoin("bufr", bufr_ver))

ip_ver=os.getenv("ip_ver")
load(pathJoin("ip", ip_ver))

sp_ver=os.getenv("sp_ver")
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver")
load(pathJoin("w3emc", w3emc_ver))

w3nco_ver=os.getenv("w3nco_ver")
load(pathJoin("w3nco", w3nco_ver))

nemsio_ver=os.getenv("nemsio_ver")
load(pathJoin("nemsio", nemsio_ver))

sigio_ver=os.getenv("sigio_ver")
load(pathJoin("sigio", sigio_ver))

sfcio_ver=os.getenv("sfcio_ver")
load(pathJoin("sfcio", sfcio_ver))

wrf_io_ver=os.getenv("wrf_io_ver")
load(pathJoin("wrf_io", wrf_io_ver))

prod_util_ver=os.getenv("prod_util_ver")
load(pathJoin("prod_util", prod_util_ver))

grib_util_ver=os.getenv("grib_util_ver")
load(pathJoin("grib_util", grib_util_ver))

wgrib2_ver=os.getenv("wgrib2_ver")
load(pathJoin("wgrib2", wgrib2_ver))

cfp_ver=os.getenv("cfp_ver")
load(pathJoin("cfp", cfp_ver))

udunits_ver=os.getenv("udunits_ver")
load(pathJoin("udunits", udunits_ver))

gsl_ver=os.getenv("gsl_ver")
load(pathJoin("gsl", gsl_ver))

nco_ver=os.getenv("nco_ver")
load(pathJoin("nco", nco_ver))

bufr_dump_ver=os.getenv("bufr_dump_ver")
load(pathJoin("bufr_dump", bufr_dump_ver))

cdo_ver=os.getenv("cdo_ver")
load(pathJoin("cdo", cdo_ver))

ncdiag_ver=os.getenv("ncdiag_ver")
load(pathJoin("ncdiag-A", ncdiag_ver))

ncio_ver=os.getenv("ncio_ver")
load(pathJoin("ncio-A", ncio_ver))

pio_ver=os.getenv("pio_ver")
load(pathJoin("pio-B", pio_ver))

esmf_ver=os.getenv("esmf_ver")
load(pathJoin("esmf-B", esmf_ver))

gftl_shared_ver=os.getenv("gftl_shared_ver")
--load(pathJoin("gftl-shared", gftl_shared_ver))

mapl_ver=os.getenv("mapl_ver")
--load(pathJoin("mapl-B", mapl_ver))

scotch_ver=os.getenv("scotch_ver")
load(pathJoin("scotch", scotch_ver))

setenv("CC", "cc")
setenv("CXX", "CC")
setenv("FC", "ftn")
setenv("CMAKE_C_COMPILER", "cc")
setenv("CMAKE_CXX_COMPILER", "CC")
setenv("CMAKE_Fortran_COMPILER", "ftn")
setenv("CMAKE_Platform", "wcoss2")

whatis("Description: HAFS build/run environment")
