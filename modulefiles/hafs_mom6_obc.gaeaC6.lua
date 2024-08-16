help([[
loads HAFS/MOM6 OBC modulefile on Orion
]])
unload("esmf")
unload("py-numpy") 
prepend_path("PATH", "/ncrc/proj/hurr1/hafs/shared/miniconda3/envs/OBCmini_env/bin")
prepend_path("PYTHONPATH", "/ncrc/proj/hurr1/hafs/shared/miniconda3/envs/OBCmini_env")
setenv("ESMFMKFILE", "/ncrc/proj/hurr1/hafs/shared/miniconda3/envs/OBCmini_env/lib/esmf.mk")

whatis("Description: HAFS/MOM6 OBC  environment")
