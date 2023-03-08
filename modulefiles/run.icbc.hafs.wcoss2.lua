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

libjpeg_ver=os.getenv("libjpeg_ver") or "9c"
load(pathJoin("libjpeg", libjpeg_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.10.6"
load(pathJoin("hdf5", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf", netcdf_ver))

prod_util_ver=os.getenv("prod_util_ver") or "2.0.14"
load(pathJoin("prod_util", prod_util_ver))

wgrib2_ver=os.getenv("wgrib2_ver") or "2.0.8_wmo"
load(pathJoin("wgrib2", wgrib2_ver))

prepend_path("MODULEPATH", "/apps/ops/test/nco/modulefiles")

rocoto_ver=os.getenv("rocoto_ver") or "1.3.5"
load(pathJoin("core", "rocoto", rocoto_ver))
whatis("Description: HAFS run environment")
