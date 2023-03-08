help([[
loads HAFS prerequisites on Cactus and Dogwood
]])

-- First, look for libraries in "prod" space
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

python_ver=os.getenv("python_ver") or "3.8.6"
load(pathJoin("python", python_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.10.6"
load(pathJoin("hdf5", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf", netcdf_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

crtm_ver=os.getenv("crtm_ver") or "2.4.0"
load(pathJoin("crtm", crtm_ver))

bufr_ver=os.getenv("bufr_ver") or "11.7.0"
load(pathJoin("bufr", bufr_ver))

ip_ver=os.getenv("ip_ver") or "3.3.3"
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

ncio_ver=os.getenv("ncio_ver") or "1.0.0"
load(pathJoin("ncio", ncio_ver))

setenv("HPC_OPT", "/apps/ops/para/libs")
prepend_path("MODULEPATH", "/apps/ops/para/libs/modulefiles/compiler/intel/19.1.3.304")
prepend_path("MODULEPATH", "/apps/ops/para/libs/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.7")

ncdiag_ver=os.getenv("ncdiag_ver") or "1.0.0"
load(pathJoin("ncdiag", ncdiag_ver))

prepend_path("MODULEPATH", "/apps/ops/test/nco/modulefiles")

rocoto_ver=os.getenv("rocoto_ver") or "1.3.5"
load(pathJoin("core", "rocoto", rocoto_ver))
whatis("Description: HAFS run environment")
