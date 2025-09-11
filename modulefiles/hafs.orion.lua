help([[
loads HAFS application level modulefile on Orion
]])

prepend_path("MODULEPATH", "/apps/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.1.0/install/modulefiles/Core")
prepend_path("MODULEPATH", "/apps/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.1.0/install/modulefiles/intel-oneapi-mpi/2021.13-li242lf/gcc/12.2.0")

stack_intel_ver=os.getenv("stack_intel_ver") or "2024.2.1"
load(pathJoin("stack-oneapi", stack_intel_ver))

stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

cmake_ver=os.getenv("cmake_ver") or "3.27.9"
load(pathJoin("cmake", cmake_ver))

load("hafs_common")

zlib_ver=os.getenv("zlib_ver") or "1.2.13"
load(pathJoin("zlib", zlib_ver))

cdo_ver=os.getenv("cdo_ver") or "2.4.4"
load(pathJoin("cdo", cdo_ver))

mkl_ver=os.getenv("mkl_ver") or "2024.2.1"
load(pathJoin("intel-oneapi-mkl", mkl_ver))

tar_ver=os.getenv("tar_ver") or "1.34"
load(pathJoin("tar", tar_ver))

blas_ver=os.getenv("blas_ver") or "0.3.27"
--load(pathJoin("openblas", blas_ver))


prepend_path("PATH", "/apps/contrib/rocoto/1.3.7/bin")

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
