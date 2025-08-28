help([[
loads HAFS application level modulefile on Hera
]])

prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")
prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/intel-oneapi-mpi/2021.13-sbi3u54/gcc/13.3.0")

stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
load(pathJoin("stack-oneapi", stack_oneapi_ver))

stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

cmake_ver=os.getenv("cmake_ver") or "3.27.9"
load(pathJoin("cmake", cmake_ver))

load("hafs_common")

zlib_ver=os.getenv("zlib_ver") or "1.2.11"
load(pathJoin("zlib", zlib_ver))

cdo_ver=os.getenv("cdo_ver") or "2.3.0"
load(pathJoin("cdo", cdo_ver))

mkl_ver=os.getenv("mkl_ver") or "2024.2.1"
load(pathJoin("mkl", mkl_ver))

tar_ver=os.getenv("tar_ver") or "1.26"
--load(pathJoin("tar", tar_ver))

blas_ver=os.getenv("blas_ver") or "0.3.24"
--load(pathJoin("openblas", blas_ver))

rocoto_ver=os.getenv("rocoto_ver") or "1.3.7"
load(pathJoin("rocoto", rocoto_ver))

setenv("CC", "mpiicx")
setenv("CXX", "mpiicpx")
setenv("FC", "mpiifort")
setenv("I_MPI_CC", "icx")
setenv("I_MPI_CXX", "icpx")
setenv("I_MPI_F90", "ifort")
setenv("CMAKE_C_COMPILER", "mpiicx")
setenv("CMAKE_CXX_COMPILER", "mpiicpx")
setenv("CMAKE_Fortran_COMPILER", "mpiifort")
setenv("CMAKE_Platform", "ursa.intel")

whatis("Description: HAFS Application environment")
