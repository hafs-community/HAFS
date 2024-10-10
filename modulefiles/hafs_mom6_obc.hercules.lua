help([[
loads HAFS/MOM6 OBC modulefile on Hercules
]])
unload("esmf")
unload("py-numpy")
unload("py-xarray")
unload("py-scipy")
unload("py-netcdf4")
prepend_path("PATH", "/work2/noaa/hwrf/maristiz/miniconda3/envs/OBC_env/bin")
prepend_path("PYTHONPATH", "/work2/noaa/hwrf/maristiz/miniconda3/envs/OBC_env")
setenv("ESMFMKFILE", "/work2/noaa/hwrf/maristiz/miniconda3/envs/OBC_env/lib/esmf.mk")

whatis("Description: HAFS/MOM6 OBC  environment")
