# Python setup

from distutils.core import setup, Extension
from numpy import get_include

numpy_inc = get_include()		#  NumPy include path.
objs = "sht_init.o sht_kernels_a.o sht_fly.o"
shtns_o = objs.split()			# transform to list of objects
libdir = "/scratch1/NCEPDEV/hwrf/save/Biju.Thomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs"
if len(libdir) == 0:
	libdir = []
else:
	libdir = [libdir+"/lib"]
cargs = ""
largs = "-L/scratch1/NCEPDEV/hwrf/save/Biju.Thomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/lib"
libs = "-lfftw3 -lm "
libslist = libs.replace('-l','').split()	# transform to list of libraries

shtns_module = Extension('_shtns', sources=['shtns_numpy_wrap.c'],
	extra_objects=shtns_o, depends=shtns_o,
	extra_compile_args=cargs.split(),
	extra_link_args=largs.split(),
	library_dirs=libdir,
	libraries=libslist,
	include_dirs=[numpy_inc])

setup(name='SHTns',
	version='3.3.1',
	description='High performance Spherical Harmonic Transform',
	license='CeCILL',
	author='Nathanael Schaeffer',
	author_email='nschaeff@ujf-grenoble.fr',
	url='https://bitbucket.org/nschaeff/shtns',
	ext_modules=[shtns_module],
	py_modules=["shtns"],
	requires=["numpy"],
	)
