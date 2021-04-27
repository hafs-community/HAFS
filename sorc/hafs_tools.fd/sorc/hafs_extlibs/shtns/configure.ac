#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.62])
AC_INIT([SHTns],[3.3.1],[],[shtns],[https://bitbucket.org/nschaeff/shtns])
AC_LANG([C])
AC_CONFIG_SRCDIR([sht_init.c])
AC_CONFIG_HEADERS([sht_config.h])
target="libshtns.a"		# by default, build the library.
objs="sht_fly.o"
install="install-lib"	# by default, install the library.

# optional variables :
AC_ARG_VAR(PYTHON, [the python interpreter (defaults to 'python')])
# optional features with --enable-XXX
AC_ARG_ENABLE([verbose],
	AS_HELP_STRING([--enable-verbose=0-3], [define verbosity level for the library: 0=silent, 1=default, 2=debug, 3=full]),
	[],[enable_verbose=1])
AC_ARG_ENABLE([openmp],
	AS_HELP_STRING([--enable-openmp], [Enable multi-threading with OpenMP]), [], [enable_openmp=default])
AC_ARG_ENABLE([mkl],
	AS_HELP_STRING([--enable-mkl], [Try to link with intel MKL instead of FFTW (can be slightly faster, but NOT THREAD SAFE)]), [], [enable_mkl=default])
AC_ARG_ENABLE([knl],
	AS_HELP_STRING([--enable-knl], [Enable compilation for Xeon Phi (KNL)]), [], [enable_knl=no])
AC_ARG_ENABLE([many-core],
	AS_HELP_STRING([--enable-many-core], [Enable optimizations for many-core systems (Blue Gene/Q, Xeon Phi,...) and remove single-threaded transforms]), [], [enable_many_core=no])
AC_ARG_ENABLE([cuda],
	AS_HELP_STRING([--enable-cuda], [Enable compilation for Nvidia gpu using cuda]), [], [enable_cuda=no])
AC_ARG_ENABLE([magic-layout],
	AS_HELP_STRING([--enable-magic-layout], [Compile specific version for the MagIC code]), [], [enable_magic_layout=no])
AC_ARG_ENABLE([python],
	AS_HELP_STRING([--enable-python], [Build Python interface]), [], [enable_python=no])
AC_ARG_ENABLE([long-double],
	AS_HELP_STRING([--enable-long-double], [Use long double during initialization for (maybe) slightly better precision]))
AC_ARG_ENABLE([f77],
	AS_HELP_STRING([--disable-f77], [Do not include F77 wrapper to call SHTns library from Fortran]))
AC_ARG_ENABLE([simd],
	AS_HELP_STRING([--disable-simd], [Do not use vector extensions (SSE2, AVX or MIC)]))
AC_ARG_ENABLE([ishioka],
	AS_HELP_STRING([--enable-ishioka], [Use the new recursion (faster but experimental)]), [], [enable_ishioka=no])

dnl Sanitize $prefix. Autoconf does this by itself, but so late in the
dnl generated configure script that the expansion does not occur until
dnl after our eval magic below.
AS_IF([test "$prefix" = "NONE"],[prefix=$ac_default_prefix])

# Checks for programs.
if test "x$CFLAGS" = "x"; then
	CFLAGS="-O2"
fi
CFLAGS="$CFLAGS -I$prefix/include -L$prefix/lib"
AC_PROG_CC
AC_PROG_SED
if test "$SED" = :; then
	AC_MSG_ERROR([sed program required.])
fi

# define macrao AX_CHECK_COMPILE_FLAG
AC_DEFUN([AX_CHECK_COMPILE_FLAG],
[AS_VAR_PUSHDEF([CACHEVAR],[ax_cv_check_[]_AC_LANG_ABBREV[]flags_$4_$1])dnl
AC_CACHE_CHECK([whether _AC_LANG compiler accepts $1], CACHEVAR, [
  ax_check_save_flags=$[]_AC_LANG_PREFIX[]FLAGS
  _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS $4 $1"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
    [AS_VAR_SET(CACHEVAR,[yes])],
    [AS_VAR_SET(CACHEVAR,[no])])
  _AC_LANG_PREFIX[]FLAGS=$ax_check_save_flags])
AS_IF([test x"AS_VAR_GET(CACHEVAR)" = xyes],
  [m4_default([$2], :)],
  [m4_default([$3], :)])
AS_VAR_POPDEF([CACHEVAR])dnl
])dnl AX_CHECK_COMPILE_FLAGS

AS_IF([test "x$enable_knl" != "xyes"], [
	# add gcc compile options if supported.
	AX_CHECK_COMPILE_FLAG([-march=native],[CC="$CC -march=native"], [
		AX_CHECK_COMPILE_FLAG([-mtune=native],[CC="$CC -mtune=native"])
	])
	# this is an icc compile option, try if it is supported:
	AX_CHECK_COMPILE_FLAG([-qopt-zmm-usage=high],[CC="$CC -qopt-zmm-usage=high"])
  ],[
	enable_many_core=yes		# optimize for many-core architecture
	target="libshtns_mic.a"
	# KNL native cross-compiling
	AX_CHECK_COMPILE_FLAG([-xMIC-AVX512],[CC="$CC -xMIC-AVX512"], [AC_MSG_ERROR(["Xeon Phi (KNL) not supported. Did you use the Intel Compiler?"])])
	# With KNL, enable mkl by default.
	AS_IF([test "x$enable_mkl" = "xdefault"], [enable_mkl=yes])
])
AX_CHECK_COMPILE_FLAG([-ffast-math],[CFLAGS="$CFLAGS -ffast-math"])
AX_CHECK_COMPILE_FLAG([-std=gnu99],[CFLAGS="$CFLAGS -std=gnu99"])

# with intel compiler, we should not use -O3 (bad performance with icc14 and icc15)
AC_EGREP_CPP(icc,
  [#ifdef __INTEL_COMPILER
   icc
   #endif
  ], [shtcc_flags="-O2"], [shtcc_flags="-O3"])

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h stdio.h string.h math.h complex.h])

# Checks for libraries.
AC_CHECK_LIB([m],[cos],,AC_MSG_ERROR([math library not found.]))

# With Python, enable openmp by default.
AS_IF([test "x$enable_python" != "xno"], [
	AS_IF([test "x$enable_openmp" = "xdefault"], [enable_openmp=yes])
])

# On many-core systems, enable openmp by default, and disable precomputed matrix support.
AS_IF([test "x$enable_many_core" != "xno"], [
  	enable_mem=no
	AS_IF([test "x$enable_openmp" = "xdefault"], [enable_openmp=yes])
])
# for MagIC code, also disable matrix transforms.
AS_IF([test "x$enable_magic_layout" != "xno"], [
  	enable_mem=no
	#enable_ishioka=no       # workaround unexplained bug
	#enable_many_core=yes		# optimize for many-core architecture
	AC_DEFINE([SHTNS4MAGIC],[1],[I need the transforms compatible with the MagIC code, to speed it up!])
])

nvcc_flags=""
# Disable precomputed matrix support with openmp: to avoid bugs arising with icc + openmp.
AS_IF([test "x$enable_openmp" = "xyes"], [
  	enable_mem=no
	nvcc_flags="-Xcompiler -fopenmp"
	AC_OPENMP
	CFLAGS="$CFLAGS $OPENMP_CFLAGS"
	dnl AC_CHECK_HEADERS([omp.h])
	objs="$objs sht_omp.o"
	AS_IF([test "x$enable_knl" != "xno"], [
		target="libshtns_mic_omp.a"
	],[
		target="libshtns_omp.a"
	])
])

# Check for CUDA
AS_IF([test "x$enable_cuda" = "xyes"], [
	CFLAGS="$CFLAGS -I$CUDA_PATH/include"
	# assume 64 bit mode, add default directory to find CUDA
	LDFLAGS="$LDFLAGS -L$CUDA_PATH/lib64"
	echo $LDFLAGS
	AC_CHECK_LIB([cudart],[cudaMalloc],,AC_MSG_ERROR([cudart library not found. Try adding LDFLAGS="-L/usr/local/cuda/lib64"]))
	AC_CHECK_LIB([cufft],[cufftPlanMany],,AC_MSG_ERROR([cufft library not found.]))
	objs="$objs sht_gpu.o"
	target="libshtns_cuda.a"

	cuda_arch=''
	AC_MSG_CHECKING([whether nvcc supports Kepler gpu])
	AS_IF([nvcc sht_gpu.cu -c -arch=sm_30 --dryrun > /dev/null 2>&1], [
	    cuda_arch=30
	    AC_MSG_RESULT(yes)
	],[ AC_MSG_RESULT(no) ])
	AC_MSG_CHECKING([whether nvcc supports Pascal gpu])
	AS_IF([nvcc sht_gpu.cu -c -arch=sm_60 --dryrun > /dev/null 2>&1], [
	    cuda_arch=60
	    AC_MSG_RESULT(yes)
	],[ AC_MSG_RESULT(no) ])
	AC_MSG_CHECKING([whether nvcc supports Volta gpu])
	AS_IF([nvcc sht_gpu.cu -c -arch=sm_70 --dryrun > /dev/null 2>&1], [
	    cuda_arch=70
	    AC_MSG_RESULT(yes)
	],[ AC_MSG_RESULT(no) ])

	AS_IF([test "x$cuda_arch" == "x"], [
	    AC_MSG_ERROR([nvcc does not support any compatible gpu. Is nvcc available ?])
	])
	nvcc_flags="$nvcc_flags -gencode=arch=compute_$cuda_arch,code=sm_$cuda_arch"
	LIBS="$LIBS -lstdc++"	# cuda uses the c++ standard library. This is needed to link with gcc
])
libname=$target		# set the libname befor checking for python.

# Check for FFTW (FFTW preferred, MKL optional)
AS_IF([test "x$enable_mkl" = "xyes"], [
  # optionally use MKL instead of fftw3
  AC_DEFINE(HAVE_LIBFFTW3_OMP, 1, [MKL has the multi-thread fftw interface.])
  AX_CHECK_COMPILE_FLAG([-mkl], [
	LIBS="-mkl $LIBS"	# easy way !
	AC_CHECK_FUNC(fftw_plan_many_dft, [AC_MSG_NOTICE(["FFTW interface found in MKL, link with -mkl"])],
		[AC_MSG_ERROR(["FFTW interface not found in MKL libraries (-mkl)."])])
  ],[
	# assume 64 bit mode, add default directory to find the MKL
	LDFLAGS="$LDFLAGS -L$MKLROOT/lib/intel64"
	AS_IF([test "x$enable_openmp" = "xyes"], [
		# multi-threaded MKL libs
		#mkl_libs="-Wl,--start-group -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -Wl,--end-group -liomp5 -lpthread"
		mkl_libs="-Wl,--start-group -lmkl_intel_lp64 -lmkl_gnu_thread -lmkl_core -Wl,--end-group"
	],[
		# sequential MKL libs
		mkl_libs="-Wl,--start-group -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -Wl,--end-group"
	])
	AC_CHECK_LIB(mkl_intel_lp64, fftw_plan_many_dft, [LIBS="$mkl_libs $LIBS"],
		AC_MSG_ERROR(["FFTW interface not found in MKL libraries ($mkl_libs). Please add 'LDFLAGS=-L/path/to/mkl' to ./configure command line."]),
	["$mkl_libs"])
	AC_MSG_NOTICE(["FFTW interface found in MKL, link with $mkl_libs"])
  ])
],[
	# fftw3
	AC_CHECK_LIB([fftw3],[fftw_plan_many_dft],,AC_MSG_ERROR([FFTW3 library required. Or try using the intel MKL library with --enable-mkl]))
	AS_IF([test "x$enable_openmp" = "xyes"], [
		AC_CHECK_LIB([fftw3_omp], [fftw_init_threads],,AC_MSG_ERROR([FFTW3 does not support OpenMP. Did you compile it with --enable-openmp ?.]))
	])
])


# Checks related to Python and NumPy paths:
AS_IF([test "x$enable_python" != "xno"], [
	AS_IF([test "x$PYTHON" = "x"], [
		AC_MSG_CHECKING(for python with numpy package)
		py_test="python python2 python3"
	],[
		AC_MSG_CHECKING(if $PYTHON has numpy package)
		py_test="$PYTHON"
	])
	for py in $py_test
	do :
		numpy_inc=`$py -c "from numpy import get_include; print(get_include())" 2>/dev/null`
		AS_IF([test "x$numpy_inc" != "x"],[break])
	done
	AS_IF([test "x$numpy_inc" = "x"], [
		AC_MSG_RESULT(no)
		AC_MSG_ERROR([NumPy package is required for the python interface.])
	],[
		AC_MSG_RESULT($py)
		PYTHON=$py
	])
	python_inc=`$PYTHON -c "from distutils.sysconfig import get_python_inc; print(get_python_inc())"  2>/dev/null`
	echo "  python include path = $python_inc"
	echo "  numpy include path = $numpy_inc"
	AC_SUBST([numpy_inc])
	AC_SUBST([python_inc])
	CFLAGS="$CFLAGS -fpic"	# required compile flag for python extensions.
	target="_shtns.so"	# build python extension instead of C library.
	install="install-py"	# install python extension instead of C library.
])

# Checks related to long double
AS_IF([test "x$enable_long_double" == "xyes"], [
	AC_TYPE_LONG_DOUBLE_WIDER])

# Disable Fortran interface ?
AS_IF([test "x$enable_f77" != "xno"], [
	AC_DEFINE([SHT_F77_API],[1],[Compile the Fortran API])
])

# Disable SIMD ?
AS_IF([test "x$enable_simd" != "xno"], [
	AC_DEFINE([_GCC_VEC_],[1],[I compile with GCC 4 or ICC 14 or later, and I would like fast vectorized code (if SSE2, AVX or MIC is supported) !])
])


AS_IF([test "x$enable_ishioka" == "xyes"], [
       AC_DEFINE([SHTNS_ISHIOKA],[1],[Enable the new recurrence proposed by Ishioka (2018) see https://doi.org/10.2151/jmsj.2018-019 (faster, but experimental)])
])

# Disable MEM ?
AS_IF([test "x$enable_mem" == "xyes"], [
	AC_DEFINE([SHTNS_MEM],[1],[Include algorithms using precomputed matrix stored in memory)])
	objs="$objs sht_mem.o"		# compile mem transforms
])

# Verbosity setting
AS_IF([test "x$enable_verbose" == "xno"], [enable_verbose=0],
	[test "x$enable_verbose" == "xyes"], [enable_verbose=1])
AC_DEFINE_UNQUOTED([SHT_VERBOSE],$enable_verbose,[0:no output, 1:output info to stdout, 2:more output (debug info), 3:also print fftw plans.])
echo "  verbose level = $enable_verbose"

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
#AC_FUNC_MALLOC
#AC_FUNC_REALLOC
AC_CHECK_FUNCS([fftw_cost])

### for cycle.h ###
   AC_C_INLINE
   AC_HEADER_TIME
   AC_CHECK_HEADERS([sys/time.h c_asm.h intrinsics.h mach/mach_time.h])

   AC_CHECK_TYPE([hrtime_t],[AC_DEFINE(HAVE_HRTIME_T, 1, [Define to 1 if hrtime_t is defined in <sys/time.h>])],,[#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif])

   AC_CHECK_FUNCS([gethrtime read_real_time time_base_to_time clock_gettime mach_absolute_time])

   dnl Cray UNICOS _rtc() (real-time clock) intrinsic
   AC_MSG_CHECKING([for _rtc intrinsic])
   rtc_ok=yes
   AC_TRY_LINK([#ifdef HAVE_INTRINSICS_H
#include <intrinsics.h>
#endif], [_rtc()], [AC_DEFINE(HAVE__RTC,1,[Define if you have the UNICOS _rtc() intrinsic.])], [rtc_ok=no])
   AC_MSG_RESULT($rtc_ok)
### end cycle.h ###

#### for the Fortran example ####
AC_PROG_FC
#### end Fortran #####

echo "prefix=$prefix"
AC_SUBST([target])
AC_SUBST([install])
AC_SUBST([objs])
AC_SUBST([libname])
AC_SUBST([shtcc_flags])
AC_SUBST([nvcc_flags])
AC_CONFIG_FILES([Makefile setup.py])
AC_OUTPUT
