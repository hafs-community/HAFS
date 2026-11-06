help([[
loads HAFS application level modulefile on NIMBUS
]])

prepend_path("MODULEPATH", "/opt/intel/oneapi/mpi/2021.18/etc/modulefiles/mpi")
prepend_path("MODULEPATH", "/lfs/work/alexander_richert/stack/spack-stack/envs/nco-core-gcc-11.5.0/modules_flat/Core")
prepend_path("MODULEPATH", "/lfs/work/alexander_richert/stack/spack-stack/envs/nco-sci-oneapi-2026.1.0/modules_flat/Core")


cmake_ver=os.getenv("cmake_ver") or "3.31.11"
load(pathJoin("cmake", cmake_ver))

zlib_ver=os.getenv("zlib_ver") or "1.3.2"
load(pathJoin("zlib", zlib_ver))

cdo_ver=os.getenv("cdo_ver") or "2.6.1"
load(pathJoin("cdo", cdo_ver))

ncio_ver=os.getenv("ncio_ver") or "1.1.2"
load(pathJoin("ncio", ncio_ver))

blas_ver=os.getenv("blas_ver") or "0.3.33"
load(pathJoin("openblas", blas_ver))


local ufs_modules = {
    {["python"]          = "3.11.15"},
    {["python-venv"]     = "1.0"},
    {["py-setuptools"]   = "73.0.1"},
    {["py-packaging"]    = "26.2"},
    {["py-numpy"]        = "1.26.4"},
    {["py-scipy"]        = "1.17.1"},
    {["py-certifi"]      = "2026.2.25"},
    {["py-cftime"]       = "1.6.4"},
    {["py-six"]          = "1.17.0"},
    {["py-python-dateutil"]        = "2.9.0.post0"},
    {["py-netcdf4"]      = "1.7.2"},
    {["py-pandas"]       = "3.0.3"},
    {["py-xarray"]       = "2022.3.0"},
    {["py-pyyaml"]       = "6.0.3"},
    {["libyaml"]         = "0.2.5"},
    {["jasper"]          = "4.2.8"},
    {["libpng"]          = "1.6.55"},
    {["libjpeg"]         = "3.1.3"},
    {["hdf5"]            = "1.14.5"},
    {["netcdf-c"]        = "4.9.2"},
    {["netcdf-fortran"]  = "4.6.1"},
    {["parallelio"]      = "2.6.2"},
    {["esmf"]            = "8.8.0-python"},
    {["fms"]             = "2024.03-gfs-constants"},
    {["bacio"]           = "2.4.1"},
    {["crtm"]            = "2.4.0.1"},
    {["crtm-fix"]        = "2.4.0.1_emc"},
    {["g2"]              = "3.5.1"},
    {["g2tmpl"]          = "1.17.0"},
    {["ip"]              = "5.4.0"},
    {["sp"]              = "2.5.0"},
    {["w3emc"]           = "2.13.0"},
    {["w3nco"]           = "2.4.1"},
    {["gftl-shared"]     = "1.12.0"},
    {["mapl"]            = "2.53.4-esmf-8.8.0"},
    {["bufr"]            = "12.3.0"},
    {["sigio"]           = "2.3.3"},
    {["sfcio"]           = "1.4.2"},
    {["wrf-io"]          = "1.3.0"},
    {["grib-util"]       = "1.4.0"},
    {["wgrib2"]          = "3.8.0"},
    {["nco"]             = "5.3.9"},
}

for i = 1, #ufs_modules do
  for name, default_version in pairs(ufs_modules[i]) do
    local env_version_name = string.gsub(name, "-", "_") .. "_ver"
    load(pathJoin(name, os.getenv(env_version_name) or default_version))
  end
end

prepend_path("PATH", "/home/biju_thomas_hpc_noaa_gov/opt/prod-util/bin")
prepend_path("PATH", "/lfs/work/alexander_richert/stack/rocoto-80f8ccc/bin")
prepend_path("PATH", "/home/biju_thomas_hpc_noaa_gov/opt/utils/bin")


setenv("CC","mpiicx")
setenv("CXX","mpiicpx")
setenv("FC","mpiifx")
setenv("I_MPI_CC", "icx")
setenv("I_MPI_CXX", "icpx")
setenv("I_MPI_F90", "ifx")
setenv("CMAKE_C_COMPILER", "mpiicx")
setenv("CMAKE_CXX_COMPILER", "mpiicpx")
setenv("CMAKE_Fortran_COMPILER", "mpiifx")
setenv("CMAKE_Platform", "nimbus.oneapi")

whatis("Description: HAFS Application environment")
