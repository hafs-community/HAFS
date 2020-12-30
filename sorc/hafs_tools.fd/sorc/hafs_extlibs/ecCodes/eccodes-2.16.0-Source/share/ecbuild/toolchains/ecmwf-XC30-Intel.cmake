####################################################################
# ARCHITECTURE
####################################################################

set( EC_HAVE_C_INLINE 1 )
set( EC_HAVE_FUNCTION_DEF 1 )
set( EC_HAVE_CXXABI_H 1 )
set( EC_HAVE_CXX_BOOL 1 )
set( EC_HAVE_CXX_SSTREAM 1 )
set( EC_HAVE_CXX_INT_128 1 )
set( CMAKE_SIZEOF_VOID_P 8 )
set( EC_SIZEOF_PTR 8 )
set( EC_SIZEOF_CHAR 1 )
set( EC_SIZEOF_SHORT 2 )
set( EC_SIZEOF_INT 4 )
set( EC_SIZEOF_LONG 8 )
set( EC_SIZEOF_LONG_LONG 8 )
set( EC_SIZEOF_FLOAT 4 )
set( EC_SIZEOF_DOUBLE 8 )
set( EC_SIZEOF_LONG_DOUBLE 16 )
set( EC_SIZEOF_SIZE_T 8 )
set( EC_SIZEOF_SSIZE_T 8 )
set( EC_SIZEOF_OFF_T 8 )
set( EC_BIG_ENDIAN 0 )
set( EC_LITTLE_ENDIAN 1 )
set( IEEE_BE 0 )
set( IEEE_LE 1 )
set( EC_HAVE_FSEEK 1 )
set( EC_HAVE_FSEEKO 1 )
set( EC_HAVE_FTELLO 1 )
set( EC_HAVE_LSEEK 0 )
set( EC_HAVE_FTRUNCATE 0 )
set( EC_HAVE_OPEN 0 )
set( EC_HAVE_FOPEN 1 )
set( EC_HAVE_FMEMOPEN 1 )
set( EC_HAVE_FUNOPEN 0 )
set( EC_HAVE_FLOCK 1 )
set( EC_HAVE_MMAP 1 )
set( EC_HAVE_POSIX_MEMALIGN 1 )
set( EC_HAVE_F_GETLK 1 )
set( EC_HAVE_F_SETLK 1 )
set( EC_HAVE_F_SETLKW 1 )
set( EC_HAVE_F_GETLK64 1 )
set( EC_HAVE_F_SETLK64 1 )
set( EC_HAVE_F_SETLKW64 1 )
set( EC_HAVE_MAP_ANONYMOUS 1 )
set( EC_HAVE_MAP_ANON 1 )
set( EC_HAVE_ASSERT_H 1 )
set( EC_HAVE_STDLIB_H 1 )
set( EC_HAVE_UNISTD_H 1 )
set( EC_HAVE_STRING_H 1 )
set( EC_HAVE_STRINGS_H 1 )
set( EC_HAVE_SYS_STAT_H 1 )
set( EC_HAVE_SYS_TIME_H 1 )
set( EC_HAVE_SYS_TYPES_H 1 )
set( EC_HAVE_MALLOC_H 1 )
set( EC_HAVE_SYS_MALLOC_H 0 )
set( EC_HAVE_SYS_PARAM_H 1 )
set( EC_HAVE_SYS_MOUNT_H 1 )
set( EC_HAVE_SYS_VFS_H 1 )
set( EC_HAVE_OFFT 1 )
set( EC_HAVE_OFF64T 1 )
set( EC_HAVE_STRUCT_STAT 1 )
set( EC_HAVE_STRUCT_STAT64 1 )
set( EC_HAVE_STAT 1 )
set( EC_HAVE_STAT64 1 )
set( EC_HAVE_FSTAT 1 )
set( EC_HAVE_FSTAT64 1 )
set( EC_HAVE_FSEEKO64 1 )
set( EC_HAVE_FTELLO64 1 )
set( EC_HAVE_LSEEK64 1 )
set( EC_HAVE_OPEN64 1 )
set( EC_HAVE_FOPEN64 1 )
set( EC_HAVE_FTRUNCATE64 1 )
set( EC_HAVE_FLOCK64 1 )
set( EC_HAVE_MMAP64 1 )
set( EC_HAVE_STRUCT_STATVFS 1 )
set( EC_HAVE_STRUCT_STATVFS64 1 )
set( EC_HAVE_FOPENCOOKIE 1 )
set( EC_HAVE_FSYNC 1 )
set( EC_HAVE_FDATASYNC 1 )
set( EC_HAVE_DIRFD 1 )
set( EC_HAVE_SYSPROC 0 )
set( EC_HAVE_SYSPROCFS 1 )
set( EC_HAVE_EXECINFO_BACKTRACE 1 )
set( EC_HAVE_GMTIME_R 1 )
set( EC_HAVE_GETPWUID_R 1 )
set( EC_HAVE_GETPWNAM_R 1 )
set( EC_HAVE_READDIR_R 1 )
set( EC_HAVE_DIRENT_D_TYPE 1 )
set( EC_HAVE_GETHOSTBYNAME_R 1 )
set( EC_HAVE_ATTRIBUTE_CONSTRUCTOR 1 )
set( EC_ATTRIBUTE_CONSTRUCTOR_INITS_ARGV 0 )
set( EC_HAVE_PROCFS 1 )
set( EC_HAVE_DLFCN_H 1 )
set( EC_HAVE_DLADDR 1 )

# Disable relative rpaths as aprun does not respect it
set( ENABLE_RELATIVE_RPATHS OFF CACHE STRING "Disable relative rpaths" FORCE )


####################################################################
# COMPILER
####################################################################

include(CMakeForceCompiler)

CMAKE_FORCE_C_COMPILER       ( cc  Intel )
CMAKE_FORCE_CXX_COMPILER     ( CC  Intel )
CMAKE_FORCE_Fortran_COMPILER ( ftn Intel )

set( ECBUILD_FIND_MPI OFF )
set( ECBUILD_TRUST_FLAGS ON )

####################################################################
# OpenMP FLAGS
####################################################################

set( OMP_C_FLAGS             "-qopenmp -qopenmp-threadprivate=compat" )
set( OMP_CXX_FLAGS           "-qopenmp -qopenmp-threadprivate=compat" )
set( OMP_Fortran_FLAGS       "-qopenmp -qopenmp-threadprivate=compat" )

####################################################################
# COMMON FLAGS
####################################################################

# for diagnostics:
#  -diag-enable=vec -diag-file -Winline

set( ECBUILD_C_FLAGS       "-fp-speculation=strict -fp-model precise -traceback")
set( ECBUILD_CXX_FLAGS     "-fp-speculation=strict -fp-model precise -traceback" )
set( ECBUILD_Fortran_FLAGS "-fp-speculation=strict -fp-model source  -convert big_endian -assume byterecl -traceback -fpe0" )

####################################################################
# BIT REPRODUCIBLE FLAGS
####################################################################

set( ECBUILD_C_FLAGS_BIT        "-O2 -xAVX -finline-functions -finline-limit=500 -diag-disable=10121" )
set( ECBUILD_CXX_FLAGS_BIT      "-O2 -xAVX -finline-functions -finline-limit=500 -diag-disable=10121" )
set( ECBUILD_Fortran_FLAGS_BIT  "-O2 -xAVX -finline-functions -finline-limit=500 -diag-disable=10121 -align array64byte" )

# Note: -diag-disable=10121 disables warning:
#    ifort: command line warning #10121: overriding '-xCORE-AVX2' with '-xAVX'

####################################################################
# DEBUG FLAGS
####################################################################

set( ECBUILD_C_FLAGS_DEBUG        "-O0 -g -traceback -fp-trap=common" )
set( ECBUILD_CXX_FLAGS_DEBUG      "-O0 -g -traceback -fp-trap=common" )
# -check all implies -check bounds
set( ECBUILD_Fortran_FLAGS_DEBUG  "-O0 -g -traceback -warn all -heap-arrays -fpe-all=0 -fpe:0 -check all" )

####################################################################
# LINK FLAGS
####################################################################

set( ECBUILD_SHARED_LINKER_FLAGS "-Wl,--eh-frame-hdr" )
set( ECBUILD_MODULE_LINKER_FLAGS "-Wl,--eh-frame-hdr -Wl,-Map,loadmap" )
set( ECBUILD_EXE_LINKER_FLAGS    "-Wl,--eh-frame-hdr -Wl,-Map,loadmap -Wl,--as-needed" )

####################################################################
# LIBRARIES
####################################################################

# Don't search for LAPACK as it is provided by the cray-libsci module which is
# loaded by default
set( LAPACK_FOUND $ENV{CRAY_LIBSCI_PREFIX_DIR} )
