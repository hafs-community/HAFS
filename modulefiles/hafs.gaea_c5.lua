help([[
  This module loads libraries required for building and running HAFS
  on the NOAA RDHPC machine Gaea C5 using Intel-2023.1.0.
]])

whatis("Description: HAFS Application environment")

load("PrgEnv-intel/8.3.3")
load("intel-classic/2023.1.0")
load("cray-mpich/8.1.25")
-- load("python/3.9.12")

prepend_path("MODULEPATH", "/lustre/f2/dev/wpo/role.epic/contrib/spack-stack/c5/spack-stack-dev-20230717/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/lustre/f2/dev/wpo/role.epic/contrib/spack-stack/c5/modulefiles")
prepend_path("MODULEPATH", "/lustre/f2/dev/Samuel.Trahan/hafs/modulefiles/")

miniconda_ver=os.getenv("miniconda_ver") or "23.5.2-hafs"
load(pathJoin("miniconda", miniconda_ver))

stack_intel_ver=os.getenv("stack_intel_ver") or "2023.1.0"
load(pathJoin("stack-intel", stack_intel_ver))

stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.25"
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))

-- stack_python_ver=os.getenv("stack_python_ver") or "3.9.12"
-- load(pathJoin("stack-python", stack_python_ver))

local ufs_modules = {
  {["jasper"]          = "2.0.32"},
  {["zlib"]            = "1.2.13"},
  {["libpng"]          = "1.6.37"},
  {["hdf5"]            = "1.14.0"},
  {["netcdf-c"]        = "4.9.2"},
  {["netcdf-fortran"]  = "4.6.0"},
  {["parallelio"]      = "2.5.10"},
  {["esmf"]            = "8.4.2"},
  {["fms"]             = "2023.01"},
  {["bacio"]           = "2.4.1"},
  {["crtm"]            = "2.4.0"},
  {["g2"]              = "3.4.5"},
  {["g2tmpl"]          = "1.10.2"},
  {["ip"]              = "3.3.3"},
  {["nemsio"]          = "2.5.4"},
  {["sp"]              = "2.3.3"},
  {["w3emc"]           = "2.9.2"},
  {["w3nco"]           = "2.4.1"},
  {["gftl-shared"]     = "1.5.0"},
-- no yafyaml
  {["mapl"]            = "2.35.2-esmf-8.4.2"},
  {["bufr"]            = "11.7.0"},
  {["sfcio"]           = "1.4.1"},
  {["sigio"]           = "2.3.2"},
-- no szip
  {["wrf-io"]          = "1.2.0"},
  {["prod-util"]       = "1.2.2"},
  {["grib-util"]       = "1.2.3"},
  {["wgrib2"]          = "2.0.8"},
-- no gempak
  {["nco"]             = "5.0.6"},
  {["cdo"]             = "2.0.5"},
}

for i = 1, #ufs_modules do
  for name, default_version in pairs(ufs_modules[i]) do
    local env_version_name = string.gsub(name, "-", "_") .. "_ver"
    load(pathJoin(name, os.getenv(env_version_name) or default_version))
  end
end

load("rocoto")

unload("darshan-runtime")
unload("cray-libsci")

setenv("CMAKE_C_COMPILER", "cc")
setenv("CMAKE_CXX_COMPILER", "CC")
setenv("CMAKE_Fortran_COMPILER", "ftn")
setenv("CMAKE_Platform", "gaea_c5.intel")
