help([[
loads HAFS/OBS_PREP modulefile on Orion
]])
unload("py-numpy")
unload("py-netcdf4")
unload("py-scipy")
prepend_path("PATH", "/work2/noaa/hwrf/noscrub/bthomas/miniconda3/envs/OBSPREP_env/bin")
prepend_path("PYTHONPATH", "/work2/noaa/hwrf/noscrub/bthomas/miniconda3/envs/OBSPREP_env")
whatis("Description: HAFS/OBS_PREP environment")
