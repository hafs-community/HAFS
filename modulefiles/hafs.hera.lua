help([[
loads HAFS application level modulefile on Hera
]])

prepend_path("MODULEPATH", "/contrib/sutils/modulefiles")
load("sutils")
load("hpss")

cmake_ver=os.getenv("cmake_ver") or "3.20.1"
load(pathJoin("cmake", cmake_ver))

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/hwrf/noscrub/local/modulefiles")
load(pathJoin("python","wcoss2_env"))

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")
hpc_ver=os.getenv("hpc_ver") or "1.2.0"
load(pathJoin("hpc", hpc_ver))

hpc_intel_ver=os.getenv("hpc_intel_ver") or "2022.1.2"
load(pathJoin("hpc-intel", hpc_intel_ver))

hpc_impi_ver=os.getenv("hpc_impi_ver") or "2022.1.2"
load(pathJoin("hpc-impi", hpc_impi_ver))

jasper_ver=os.getenv("jasper_ver") or "2.0.25"
load(pathJoin("jasper", jasper_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.11"
load(pathJoin("zlib", zlib_ver))

zstd_ver=os.getenv("zstd_ver") or "1.5.0"
load(pathJoin("zstd", zstd_ver))

libpng_ver=os.getenv("libpng_ver") or "1.6.37"
load(pathJoin("libpng", libpng_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.10.6"
load(pathJoin("hdf5", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf", netcdf_ver))

pio_ver=os.getenv("pio_ver") or "2.5.7"
load(pathJoin("pio", pio_ver))

esmf_ver=os.getenv("esmf_ver") or "8.3.0b09"
load(pathJoin("esmf", esmf_ver))

fms_ver=os.getenv("fms_ver") or "2022.01"
load(pathJoin("fms",fms_ver))

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

nemsio_ver_ver=os.getenv("nemsio_ver") or "2.5.4"
load(pathJoin("nemsio", nemsio_ver))

sp_ver=os.getenv("sp_ver") or "2.3.3"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
load(pathJoin("w3emc", w3emc_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nco_ver))

gftl_shared_ver=os.getenv("gftl_shared_ver") or "v1.5.0"
load(pathJoin("gftl-shared", gftl_shared_ver))

yafyaml_ver=os.getenv("yafyaml_ver") or "v0.5.1"
load(pathJoin("yafyaml", yafyaml_ver))

mapl_ver=os.getenv("mapl_ver") or "2.22.0-esmf-8.3.0b09"
load(pathJoin("mapl", mapl_ver))

bufr_ver=os.getenv("bufr_ver") or "11.4.0"
load(pathJoin("bufr", bufr_ver))

sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
load(pathJoin("sfcio", sfcio_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

szip_ver=os.getenv("szip_ver") or "2.1.1"
load(pathJoin("szip", szip_ver))

wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
load(pathJoin("wrf_io", wrf_io_ver))

prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
load(pathJoin("prod_util", prod_util_ver))

grib_util_ver=os.getenv("grib_util_ver") or "1.2.4"
load(pathJoin("grib_util", grib_util_ver))

wgrib2_ver=os.getenv("wgrib2_ver") or "2.0.8"
load(pathJoin("wgrib2", wgrib2_ver))
setenv("WGRIB2", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/intel-18.0.5.274/impi-2018.0.4/wgrib2/2.0.8/bin/wgrib2")

gempak_ver=os.getenv("gempak_ver") or "7.4.2"
load(pathJoin("gempak", gempak_ver))

-- nco_ver=os.getenv("nco_ver") or "5.0.6"
nco_ver=os.getenv("nco_ver") or "4.9.3"
load(pathJoin("nco", nco_ver))

cdo_ver=os.getenv("cdo_ver") or "1.9.8"
load(pathJoin("cdo", cdo_ver))

rocoto_ver=os.getenv("rocoto_ver") or "1.3.3"
load("rocoto")

setenv("CMAKE_C_COMPILER", "mpiicc")
setenv("CMAKE_CXX_COMPILER", "mpiicpc")
setenv("CMAKE_Fortran_COMPILER", "mpiifort")
setenv("CMAKE_Platform", "jet.intel")

whatis("Description: HAFS Application environment")
