help([[
loads HAFS/MOM6 OBC modulefile on Jet
]])
unload("esmf")
unload("py-numpy")
unload("py-xarray")
unload("py-scipy")
unload("py-netcdf4")
prepend_path("PATH", "/lfs5/HFIP/hwrfv3/Maria.Aristizabal/miniconda3/envs/OBC_env/bin")
prepend_path("PYTHONPATH", "/lfs5/HFIP/hwrfv3/Maria.Aristizabal/miniconda3/envs/OBC_env")
setenv("ESMFMKFILE", "/lfs5/HFIP/hwrfv3/Maria.Aristizabal/miniconda3/envs/OBC_env/lib/esmf.mk")

whatis("Description: HAFS/MOM6 OBC  environment")
