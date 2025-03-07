help([[
loads HAFS/MOM6 OBC modulefile on Hera
]])
unload("esmf")
unload("py-numpy")
unload("py-xarray")
unload("py-scipy")
unload("py-netcdf4")
prepend_path("PATH", "/scratch2/NCEPDEV/hwrf/save/Maria.Aristizabal/miniconda3/envs/OBC_env/bin")
prepend_path("PYTHONPATH", "/scratch2/NCEPDEV/hwrf/save/Maria.Aristizabal/miniconda3/envs/OBC_env")
setenv("ESMFMKFILE", "/scratch2/NCEPDEV/hwrf/save/Maria.Aristizabal/miniconda3/envs/OBC_env/lib/esmf.mk")

whatis("Description: HAFS/MOM6 OBC  environment")
