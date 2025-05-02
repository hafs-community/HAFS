help([[
loads HAFS/OBS_PREP modulefile on GaeaC6
]])
unload("py-numpy")
unload("py-netcdf4")
unload("py-scipy")
prepend_path("PATH", "/autofs/ncrc-svm1_proj/hurr1/hafs/shared/miniconda3/envs/OBSPREP_env/bin")
prepend_path("PYTHONPATH", "/autofs/ncrc-svm1_proj/hurr1/hafs/shared/miniconda3/envs/OBSPREP_env/bin")
whatis("Description: HAFS/OBS_PREP environment")
