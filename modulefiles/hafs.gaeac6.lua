help([[
loads HAFS application level modulefile on Gaea C6
]])

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")

stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
load(pathJoin("stack-oneapi", stack_oneapi_ver))

stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.32"
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))

cmake_ver=os.getenv("cmake_ver") or "3.27.9"
load(pathJoin("cmake", cmake_ver))

load("hafs_common")

scipy_ver=os.getenv("scipy_ver") or "1.14.1"
load(pathJoin("py-scipy", scipy_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.13"
load(pathJoin("zlib", zlib_ver))

cdo_ver=os.getenv("cdo_ver") or "2.4.4"
load(pathJoin("cdo", cdo_ver))

ncio_ver=os.getenv("ncio_ver") or "1.1.2"
load(pathJoin("ncio", ncio_ver))

prepend_path("MODULEPATH", "/autofs/ncrc-svm1_proj/hurr1/hafs/shared/modulefiles")
rocoto_ver=os.getenv("rocoto_ver") or "1.3.7_fix"
load(pathJoin("rocoto", rocoto_ver))

prepend_path("MODULEPATH", "/usw/hpss/modulefiles")
load("hsi")

--unload("cray-libsci")

setenv("CC", "cc")
setenv("CXX", "CC")
setenv("FC", "ftn")
setenv("CMAKE_C_COMPILER", "cc")
setenv("CMAKE_CXX_COMPILER", "CC")
setenv("CMAKE_Fortran_COMPILER", "ftn")
setenv("CMAKE_Platform", "gaeac6.intel")

whatis("Description: HAFS Application environment")
