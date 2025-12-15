help([[
loads HAFS/MOM6 OBC modulefile on Ursa
]])
unload("esmf")
unload("py-numpy")
unload("py-pandas")
unload("py-scipy")
unload("py-xarray")
unload("py-netcdf4")
prepend_path("PATH", "/scratch3/NCEPDEV/hwrf/noscrub/local/miniconda3/envs/OBCmini_env/bin")
prepend_path("PYTHONPATH", "/scratch3/NCEPDEV/hwrf/noscrub/local/miniconda3/envs/OBCmini_env")
setenv("ESMFMKFILE", "/scratch3/NCEPDEV/hwrf/noscrub/local/miniconda3/envs/OBCmini_env/lib/esmf.mk")

whatis("Description: HAFS/MOM6 OBC  environment")
