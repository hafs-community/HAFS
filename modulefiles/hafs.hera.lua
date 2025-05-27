help([[
loads HAFS application level modulefile on Hera
]])

prepend_path("MODULEPATH", "/contrib/sutils/modulefiles")
load("sutils")
load("hpss")

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/unified-env-rocky8/install/modulefiles/Core")

stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
load(pathJoin("stack-intel", stack_intel_ver)) 

stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.1" 
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

python_ver=os.getenv("python_ver") or "3.10.13"
load(pathJoin("python", python_ver))

cmake_ver=os.getenv("cmake_ver") or "3.23.1"
load(pathJoin("cmake", cmake_ver))

jasper_ver=os.getenv("jasper_ver") or "2.0.32"
load(pathJoin("jasper", jasper_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.13"
load(pathJoin("zlib", zlib_ver))

libpng_ver=os.getenv("libpng_ver") or "1.6.37"
load(pathJoin("libpng", libpng_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.14.0"
load(pathJoin("hdf5", hdf5_ver))

netcdf_c_ver=os.getenv("netcdf_c_ver") or "4.9.2"
load(pathJoin("netcdf-c", netcdf_c_ver))

netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "4.6.1"
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))

parallelio_ver=os.getenv("parallelio_ver") or "2.5.10"
load(pathJoin("parallelio", parallelio_ver))

esmf_ver=os.getenv("esmf_ver") or "8.6.0"
load(pathJoin("esmf", esmf_ver))

fms_ver=os.getenv("fms_ver") or "2023.04"
load(pathJoin("fms",fms_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

crtm_ver=os.getenv("crtm_ver") or "2.4.0.1"
load(pathJoin("crtm", crtm_ver))

g2_ver=os.getenv("g2_ver") or "3.4.5"
load(pathJoin("g2", g2_ver))

g2tmpl_ver=os.getenv("g2tmpl_ver") or "1.10.2"
load(pathJoin("g2tmpl", g2tmpl_ver))

ip_ver=os.getenv("ip_ver") or "4.3.0"
load(pathJoin("ip", ip_ver))

sp_ver=os.getenv("sp_ver") or "2.5.0"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.10.0"
load(pathJoin("w3emc", w3emc_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nco_ver))

gftl_shared_ver=os.getenv("gftl_shared_ver") or "1.6.1"
load(pathJoin("gftl-shared", gftl_shared_ver))

yafyaml_ver=os.getenv("yafyaml_ver") or "0.2.5"
load(pathJoin("libyaml", yafyaml_ver)) 

mapl_ver=os.getenv("mapl_ver") or "2.40.3-esmf-8.6.0"
--load(pathJoin("mapl", mapl_ver))

bufr_ver=os.getenv("bufr_ver") or "12.0.1"
load(pathJoin("bufr", bufr_ver))

sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
load(pathJoin("sfcio", sfcio_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

szip_ver=os.getenv("szip_ver") or "2.1"
load(pathJoin("szip", szip_ver))

wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
load(pathJoin("wrf-io", wrf_io_ver))

prod_util_ver=os.getenv("prod_util_ver") or "2.1.1"
load(pathJoin("prod_util", prod_util_ver))

grib_util_ver=os.getenv("grib_util_ver") or "1.3.0"
load(pathJoin("grib-util", grib_util_ver))

wgrib2_ver=os.getenv("wgrib2_ver") or "2.0.8"
load(pathJoin("wgrib2", wgrib2_ver))

gempak_ver=os.getenv("gempak_ver") or "7.17.0"
load(pathJoin("gempak", gempak_ver))

nco_ver=os.getenv("nco_ver") or "5.0.6"
--nco_ver=os.getenv("nco_ver") or "4.9.3"
load(pathJoin("nco", nco_ver))
--prepend_path("PATH", "/apps/nco/4.9.3/gnu/9.2.0/bin")

ncio_ver=os.getenv("ncio_ver") or "1.1.2"
load(pathJoin("ncio", ncio_ver)) 

cdo_ver=os.getenv("cdo_ver") or "2.2.0"
load(pathJoin("cdo", cdo_ver))

rocoto_ver=os.getenv("rocoto_ver") or "1.3.7"
load("rocoto")

xarray_ver=os.getenv("xarray_ver") or "2023.7.0"
load(pathJoin("py-xarray", xarray_ver))

scipy_ver=os.getenv("scipy_ver") or "1.11.3"
load(pathJoin("py-scipy", scipy_ver))

setenv("CMAKE_C_COMPILER", "mpiicc")
setenv("CMAKE_CXX_COMPILER", "mpiicpc")
setenv("CMAKE_Fortran_COMPILER", "mpiifort")
setenv("CMAKE_Platform", "hera.intel")

whatis("Description: HAFS Application environment")
