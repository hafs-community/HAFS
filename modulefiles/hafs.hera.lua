help([[
loads HAFS application level modulefile on Hera
]])

prepend_path("MODULEPATH", "/contrib/sutils/modulefiles")
load("sutils")
load("hpss")

--- prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.5.0/envs/unified-env-noavx512/install/modulefiles/Core")

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.5.1/envs/unified-env/install/modulefiles/Core")

stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
load(pathJoin("stack-intel", stack_intel_ver))

stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.1"
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

cmake_ver=os.getenv("cmake_ver") or "3.23.1"
load(pathJoin("cmake", cmake_ver))

local hafs_modules = {
  {["jasper"]          = "2.0.32"},
  {["zlib"]            = "1.2.13"},
  {["zstd"]            = "1.5.2"},
  {["libpng"]          = "1.6.37"},
  {["hdf5"]            = "1.14.0"},
  {["netcdf-c"]        = "4.9.2"},
  {["netcdf-fortran"]  = "4.6.0"},
  {["parallelio"]      = "2.5.10"},
  {["esmf"]            = "8.5.0"},
  {["fms"]             = "2023.02.01"},
  {["bacio"]           = "2.4.1"},
  {["crtm"]            = "2.4.0"},
  {["g2"]              = "3.4.5"},
  {["g2tmpl"]          = "1.10.2"},
  {["ip"]              = "4.3.0"},
  {["nemsio"]          = "2.5.4"},
  {["sp"]              = "2.3.3"},
  {["w3emc"]           = "2.10.0"},
  {["w3nco"]           = "2.4.1"},
  {["gftl-shared"]     = "1.6.1"},
---  {["yafyaml"]         = "0.5.1"},
  {["mapl"]            = "2.40.3-esmf-8.5.0"},
  {["bufr"]            = "12.0.1"},
  {["sfcio"]           = "1.4.1"},
  {["sigio"]           = "2.3.2"},
  {["szip"]            = "2.1"},
  {["wrf-io"]          = "1.2.0"},
  {["prod_util"]       = "1.2.2"},
  {["grib-util"]       = "1.3.0"},
  {["wgrib2"]          = "2.0.8"},
  {["gempak"]          = "7.4.2"},
  {["nco"]             = "5.0.6"},
  {["cdo"]             = "2.0.5"},
  {["rocoto"]          = "1.3.6"},
  {["scotch"]          = "7.0.4"},
}

for i = 1, #hafs_modules do
  for name, default_version in pairs(hafs_modules[i]) do
    local env_version_name = string.gsub(name, "-", "_") .. "_ver"
    load(pathJoin(name, os.getenv(env_version_name) or default_version))
  end
end


prepend_path("MODULEPATH", "/scratch1/NCEPDEV/hwrf/noscrub/local/modulefiles")
load(pathJoin("python","wcoss2_env"))

setenv("CMAKE_C_COMPILER", "mpiicc")
setenv("CMAKE_CXX_COMPILER", "mpiicpc")
setenv("CMAKE_Fortran_COMPILER", "mpiifort")
setenv("CMAKE_Platform", "jet.intel")

whatis("Description: HAFS Application environment")
