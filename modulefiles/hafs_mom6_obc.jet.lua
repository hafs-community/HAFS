help([[
loads HAFS/MOM6 OBC modulefile on Jet
]])
unload("esmf")
prepend_path("MODULEPATH", "/mnt/lfs4/HFIP/hwrfv3/Maria.Aristizabal/modulefiles")
load("modulefile.OBC.run.jet")
setenv("ESMFMKFILE", "/mnt/lfs4/HFIP/hwrfv3/Maria.Aristizabal/miniconda3/envs/OBC_env/lib/esmf.mk")

whatis("Description: HAFS/MOM6 OBC  environment")
