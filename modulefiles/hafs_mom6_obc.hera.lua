help([[
loads HAFS/MOM6 OBC modulefile on Hera
]])
unload("esmf")
prepend_path("MODULEPATH", "/scratch1/NCEPDEV/hwrf/save/Maria.Aristizabal/modulefiles")
load("modulefile.OBC.run.hera")
setenv("ESMFMKFILE", "/scratch1/NCEPDEV/hwrf/save/Maria.Aristizabal/miniconda3/envs/OBC_env/lib/esmf.mk")

whatis("Description: HAFS/MOM6 OBC  environment")
