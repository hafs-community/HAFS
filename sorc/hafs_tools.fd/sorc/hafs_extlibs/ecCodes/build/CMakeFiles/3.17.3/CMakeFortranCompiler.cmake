set(CMAKE_Fortran_COMPILER "/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/bin/intel64/ifort")
set(CMAKE_Fortran_COMPILER_ARG1 "")
set(CMAKE_Fortran_COMPILER_ID "Intel")
set(CMAKE_Fortran_COMPILER_VERSION "18.0.5.20180823")
set(CMAKE_Fortran_COMPILER_WRAPPER "")
set(CMAKE_Fortran_PLATFORM_ID "Linux")
set(CMAKE_Fortran_SIMULATE_ID "")
set(CMAKE_Fortran_SIMULATE_VERSION "")



set(CMAKE_AR "/bin/ar")
set(CMAKE_Fortran_COMPILER_AR "")
set(CMAKE_RANLIB "/bin/ranlib")
set(CMAKE_Fortran_COMPILER_RANLIB "")
set(CMAKE_COMPILER_IS_GNUG77 )
set(CMAKE_Fortran_COMPILER_LOADED 1)
set(CMAKE_Fortran_COMPILER_WORKS TRUE)
set(CMAKE_Fortran_ABI_COMPILED TRUE)
set(CMAKE_COMPILER_IS_MINGW )
set(CMAKE_COMPILER_IS_CYGWIN )
if(CMAKE_COMPILER_IS_CYGWIN)
  set(CYGWIN 1)
  set(UNIX 1)
endif()

set(CMAKE_Fortran_COMPILER_ENV_VAR "FC")

set(CMAKE_Fortran_COMPILER_SUPPORTS_F90 1)

if(CMAKE_COMPILER_IS_MINGW)
  set(MINGW 1)
endif()
set(CMAKE_Fortran_COMPILER_ID_RUN 1)
set(CMAKE_Fortran_SOURCE_FILE_EXTENSIONS f;F;fpp;FPP;f77;F77;f90;F90;for;For;FOR;f95;F95)
set(CMAKE_Fortran_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_Fortran_LINKER_PREFERENCE 20)
if(UNIX)
  set(CMAKE_Fortran_OUTPUT_EXTENSION .o)
else()
  set(CMAKE_Fortran_OUTPUT_EXTENSION .obj)
endif()

# Save compiler ABI information.
set(CMAKE_Fortran_SIZEOF_DATA_PTR "8")
set(CMAKE_Fortran_COMPILER_ABI "ELF")
set(CMAKE_Fortran_LIBRARY_ARCHITECTURE "")

if(CMAKE_Fortran_SIZEOF_DATA_PTR AND NOT CMAKE_SIZEOF_VOID_P)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_Fortran_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_Fortran_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_Fortran_COMPILER_ABI}")
endif()

if(CMAKE_Fortran_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()





set(CMAKE_Fortran_IMPLICIT_INCLUDE_DIRECTORIES "/apps/intel-2018/netcdf-4.7.2-parallel/include;/apps/intel-2018/pnetcdf-1.12.0/include;/apps/intel-2018/hdf5-1.10.5-parallel/include;/apps/contrib/NCEP/libs/hpc-stack/v1.0.0-beta1/intel-2018.4/png/1.6.35/include;/apps/contrib/NCEP/libs/hpc-stack/v1.0.0-beta1/intel-2018.4/zlib/1.2.11/include;/apps/contrib/NCEP/libs/hpc-stack/v1.0.0-beta1/intel-2018.4/jasper/2.0.15/include;/apps/intel-2020.2/intel-2020.2/compilers_and_libraries_2020.2.254/linux/mkl/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/ipp/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/mkl/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/pstl/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/tbb/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/daal/include;/apps/intel-2018/wgrib-2.0.8/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/compiler/include/intel64;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/compiler/include/icc;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/compiler/include;/usr/lib/gcc/x86_64-redhat-linux/4.8.5/include;/usr/include")
set(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "ifport;ifcoremt;imf;svml;m;ipgo;irc;pthread;svml;c;gcc;gcc_s;irc_s;dl;c")
set(CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES "/apps/intel-2020/intel-2020/compilers_and_libraries_2020.0.166/linux/mpi/intel64/libfabric/lib;/apps/jasper-1.900.1/lib;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/compiler/lib/intel64_lin;/usr/lib/gcc/x86_64-redhat-linux/4.8.5;/usr/lib64;/lib64;/usr/lib;/lib")
set(CMAKE_Fortran_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")
