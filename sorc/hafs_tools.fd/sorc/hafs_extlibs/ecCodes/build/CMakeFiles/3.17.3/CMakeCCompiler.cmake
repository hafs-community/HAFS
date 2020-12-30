set(CMAKE_C_COMPILER "/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/bin/intel64/icc")
set(CMAKE_C_COMPILER_ARG1 "")
set(CMAKE_C_COMPILER_ID "Intel")
set(CMAKE_C_COMPILER_VERSION "18.0.5.20180823")
set(CMAKE_C_COMPILER_VERSION_INTERNAL "")
set(CMAKE_C_COMPILER_WRAPPER "")
set(CMAKE_C_STANDARD_COMPUTED_DEFAULT "90")
set(CMAKE_C_COMPILE_FEATURES "c_std_90;c_function_prototypes;c_std_99;c_restrict;c_variadic_macros;c_std_11;c_static_assert")
set(CMAKE_C90_COMPILE_FEATURES "c_std_90;c_function_prototypes")
set(CMAKE_C99_COMPILE_FEATURES "c_std_99;c_restrict;c_variadic_macros")
set(CMAKE_C11_COMPILE_FEATURES "c_std_11;c_static_assert")

set(CMAKE_C_PLATFORM_ID "Linux")
set(CMAKE_C_SIMULATE_ID "GNU")
set(CMAKE_C_COMPILER_FRONTEND_VARIANT "")
set(CMAKE_C_SIMULATE_VERSION "4.8.5")



set(CMAKE_AR "/bin/ar")
set(CMAKE_C_COMPILER_AR "")
set(CMAKE_RANLIB "/bin/ranlib")
set(CMAKE_C_COMPILER_RANLIB "")
set(CMAKE_LINKER "/bin/ld")
set(CMAKE_MT "")
set(CMAKE_COMPILER_IS_GNUCC )
set(CMAKE_C_COMPILER_LOADED 1)
set(CMAKE_C_COMPILER_WORKS TRUE)
set(CMAKE_C_ABI_COMPILED TRUE)
set(CMAKE_COMPILER_IS_MINGW )
set(CMAKE_COMPILER_IS_CYGWIN )
if(CMAKE_COMPILER_IS_CYGWIN)
  set(CYGWIN 1)
  set(UNIX 1)
endif()

set(CMAKE_C_COMPILER_ENV_VAR "CC")

if(CMAKE_COMPILER_IS_MINGW)
  set(MINGW 1)
endif()
set(CMAKE_C_COMPILER_ID_RUN 1)
set(CMAKE_C_SOURCE_FILE_EXTENSIONS c;m)
set(CMAKE_C_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_C_LINKER_PREFERENCE 10)

# Save compiler ABI information.
set(CMAKE_C_SIZEOF_DATA_PTR "8")
set(CMAKE_C_COMPILER_ABI "ELF")
set(CMAKE_C_LIBRARY_ARCHITECTURE "")

if(CMAKE_C_SIZEOF_DATA_PTR)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_C_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_C_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_C_COMPILER_ABI}")
endif()

if(CMAKE_C_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()

set(CMAKE_C_CL_SHOWINCLUDES_PREFIX "")
if(CMAKE_C_CL_SHOWINCLUDES_PREFIX)
  set(CMAKE_CL_SHOWINCLUDES_PREFIX "${CMAKE_C_CL_SHOWINCLUDES_PREFIX}")
endif()





set(CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES "/apps/intel-2018/netcdf-4.7.2-parallel/include;/apps/intel-2018/pnetcdf-1.12.0/include;/apps/intel-2018/hdf5-1.10.5-parallel/include;/apps/contrib/NCEP/libs/hpc-stack/v1.0.0-beta1/intel-2018.4/png/1.6.35/include;/apps/contrib/NCEP/libs/hpc-stack/v1.0.0-beta1/intel-2018.4/zlib/1.2.11/include;/apps/contrib/NCEP/libs/hpc-stack/v1.0.0-beta1/intel-2018.4/jasper/2.0.15/include;/apps/intel-2020.2/intel-2020.2/compilers_and_libraries_2020.2.254/linux/mkl/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/ipp/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/mkl/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/pstl/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/tbb/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/daal/include;/apps/intel-2018/wgrib-2.0.8/include;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/compiler/include/intel64;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/compiler/include/icc;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/compiler/include;/usr/lib/gcc/x86_64-redhat-linux/4.8.5/include;/usr/include")
set(CMAKE_C_IMPLICIT_LINK_LIBRARIES "imf;svml;irng;m;ipgo;decimal;cilkrts;stdc++;gcc;gcc_s;irc;svml;c;gcc;gcc_s;irc_s;dl;c")
set(CMAKE_C_IMPLICIT_LINK_DIRECTORIES "/apps/intel-2020/intel-2020/compilers_and_libraries_2020.0.166/linux/mpi/intel64/libfabric/lib;/apps/jasper-1.900.1/lib;/apps/intel-2018/intel-2018.u4/compilers_and_libraries_2018.5.274/linux/compiler/lib/intel64_lin;/usr/lib/gcc/x86_64-redhat-linux/4.8.5;/usr/lib64;/lib64;/usr/lib;/lib")
set(CMAKE_C_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")
