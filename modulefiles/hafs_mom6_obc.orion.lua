help([[
loads HAFS/MOM6 OBC modulefile on Orion
]])
unload("esmf")
prepend_path("MODULEPATH", "/work/noaa/hwrf/save/maristiz/modulefiles")
load("modulefile.OBC.run.orion")
setenv("ESMFMKFILE", "/work/noaa/hwrf/save/maristiz/miniconda3/envs/OBC_env/lib/esmf.mk")

whatis("Description: HAFS/MOM6 OBC  environment")
