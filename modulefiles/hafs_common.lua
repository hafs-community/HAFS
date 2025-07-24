whatis("Description: HAFS build environment common libraries")

help([[Load HAFS Model common libraries]])

local ufs_modules = {
    {["jasper"]          = "2.0.32"},
    {["libpng"]          = "1.6.37"},
    {["hdf5"]            = "1.14.3"},
    {["netcdf-c"]        = "4.9.2"},
    {["netcdf-fortran"]  = "4.6.1"},
    {["parallelio"]      = "2.6.2"},
    {["esmf"]            = "8.8.0"},
    {["fms"]             = "2024.02"},
    {["bacio"]           = "2.4.1"},
    {["crtm"]            = "2.4.0.1"},
    {["g2"]              = "3.5.1"},
    {["g2tmpl"]          = "1.13.0"},
    {["ip"]              = "5.1.0"},
    {["sp"]              = "2.5.0"},
    {["w3emc"]           = "2.10.0"},
    {["w3nco"]           = "2.4.1"},
    {["gftl-shared"]     = "1.9.0"},
    {["mapl"]            = "2.53.4-esmf-8.8.0"},
    {["bufr"]            = "12.1.0"}, 
    {["sigio"]           = "2.3.3"},
    {["sfcio"]           = "1.4.2"},
    {["wrf-io"]          = "1.2.0"},
    {["prod_util"]       = "2.1.1"},
    {["grib-util"]       = "1.4.0"},
    {["wgrib2"]          = "3.6.0"},
    {["nco"]             = "5.2.4"},
    {["py-xarray"]       = "2024.7.0"},
    {["py-netcdf4"]      = "1.7.1.post2"},
    {["libyaml"]         = "0.2.5"},   
}

for i = 1, #ufs_modules do
  for name, default_version in pairs(ufs_modules[i]) do
    local env_version_name = string.gsub(name, "-", "_") .. "_ver"
    load(pathJoin(name, os.getenv(env_version_name) or default_version))
  end
end
