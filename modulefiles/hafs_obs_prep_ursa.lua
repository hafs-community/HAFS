help([[
loads HAFS/OBS_PREP modulefile on Ursa
]])
unload("py-numpy")
unload("py-netcdf4")
unload("py-scipy")
prepend_path("PATH", "/scratch3/NCEPDEV/hwrf/noscrub/local/miniconda3/envs/OBSPREP_env/bin")
prepend_path("PYTHONPATH", "/scratch3/NCEPDEV/hwrf/noscrub/local/miniconda3/envs/OBSPREP_env")
whatis("Description: HAFS/OBS_PREP environment")
