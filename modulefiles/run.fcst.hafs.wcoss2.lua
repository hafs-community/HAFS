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

jasper_ver=os.getenv("jasper_ver") or "2.0.25"
load(pathJoin("jasper", jasper_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.11"
load(pathJoin("zlib", zlib_ver))

libpng_ver=os.getenv("libpng_ver") or "1.6.37"
load(pathJoin("libpng", libpng_ver))

libjpeg_ver=os.getenv("libjpeg_ver") or "9c"
load(pathJoin("libjpeg", libjpeg_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.10.6"
load(pathJoin("hdf5", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf", netcdf_ver))

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

sp_ver=os.getenv("sp_ver") or "2.3.3"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
load(pathJoin("w3emc", w3emc_ver))

prod_util_ver=os.getenv("prod_util_ver") or "2.0.14"
load(pathJoin("prod_util", prod_util_ver))

pio_ver=os.getenv("pio_ver") or "2.5.3"
load(pathJoin("pio", pio_ver))

-- Finally, look for libraries in "dev" space
prepend_path("MODULEPATH", "/apps/dev/lmodules/intel/19.1.3.304")
prepend_path("MODULEPATH", "/apps/dev/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.9")

esmf_ver=os.getenv("esmf_ver") or "8.3.0b09"
load(pathJoin("esmf", esmf_ver))

mapl_ver=os.getenv("mapl_ver") or "2.23.1-esmf-8.3.0b09"
load(pathJoin("mapl", mapl_ver))

prepend_path("MODULEPATH", "/apps/ops/test/nco/modulefiles")

rocoto_ver=os.getenv("rocoto_ver") or "1.3.5"
load(pathJoin("core", "rocoto", rocoto_ver))
whatis("Description: HAFS run environment")
