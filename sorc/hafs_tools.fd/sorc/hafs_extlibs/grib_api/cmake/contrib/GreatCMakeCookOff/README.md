The Great CMake CookOff
=======================


This is a repository of usefull and less than usefull cmake recipes.  It is distributed under the
[MIT License](http://opensource.org/licenses/MIT)


Adding [Eigen](http://eigen.tuxfamily.org/) to a project
========================================================

Looks for the Eigen installed on system. If not found, then uses external project to download it.
Usage is as follows:

```cmake
# Tell cmake to look into GreatCMakeCookOff for recipes
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/GreatCMakeCookOff) 

# Optionally, tell cmake where to download eigen, if needed.
# Defaults to value below.
set(EXTERNAL_ROOT ${PROJECT_BINARY_DIR}/external)

# Now look for cmake.
find_package(Eigen)
```

**NOTE:** After building the first time, run cmake again. It will find the eigen it downloaded
previously, and it will stop checking for updates. 

Adding [GTest](https://code.google.com/p/googletest/) to a project
==================================================================

For googly reasons, whether valid or 404, GTest prefers to be compiled for each an every project. 
This script does two things:

- it adds GTest as an external project
- it provides a function to add gtests to ctest

This implies that GTest is downloaded the first time that make runs. Furthermore, it will be
checked each and every time that makes runs. So, make now requires a working internet connection.
Unlike Eigen above, there is currently no option avoid checking for updates.

The CMakeLists.txt file could look like this:

```cmake
option(tests          "Enable testing."                         on)

if(tests) 
  find_package(GTest)
  enable_testing()
endif(tests)
```

And adding a test comes down to

```cmake
if(tests)

  add_gtest(testme testme.cc mylib)

endif(tests)
```

- first argument: name of the test
- second argument: list of source files
- other arguments: additional libraries to add during linking

The test do expect an explicit main function. See the test generated in ``tests/addgtest.cmake``.

**NOTE:** When using c++11, it is recommended to first include the c++11 flag script
``AddCPP11Flags.cmake`` (see below) so that the gtest can be compiled with ``GTEST_LANG_CXX11``. 

C++11
=====

Checking for specific features
------------------------------

Look for some c++11 features. Uses a script modified from [here](http://pageant.ghulbus.eu/?p=664).
Usage is given below.

```cmake
# First need to enable c++
enable_language(CXX)

# The following will print out all available features.
cxx11_find_all_features(ALL_FEATURES)
message(STATUS "[c++11] features we can check for: ${ALL_FEATURES}")

# The following checks for all features
cxx11_feature_check()

# An internal value is set if a particular feature exists.
if(HAS_CXX11_AUTO)
  message(STATUS "[c++11] has auto.")
endif()
if(HAS_CXX11_LAMBDA)
  message(STATUS "[c++11] has lambda.")
endif()
```

Alternatively, only a subset of features can be checked for, and some can be required:
```cmake
cxx11_feature_check(auto lambda REQUIRED long_long share_ptr variadic_templates)
```
The previous statement will fail if ``long long``, ``std::shared_ptr<...>``, and variadic templates
are not available. It will also check for the availability of ``auto`` and ``lambda``, but without
failing.

Figuring out flags for some compilers
-------------------------------------

The script checks the existence of a few flags to enable c++11 features on different compilers.
The output is somewhat verbose, but it seems to do the job for gcc, darwin-gcc, and microsoft visual
studio. In addition, the intel compilers have to be told to use an external c++11 standard library.
This script cannot figure where this library would be (hint: g++ provides it), so that is left up to
the user. The script can be activated with a one liner.

```cmake
include("path/to/cookoff/AddCPP11Flags.cmake")
```

**NOTE:** On windows + visual studio, disables warnings 4251 and ups fake variadic templates to 10.


Figure out ``isnan``
====================

Each and every vendor provides a different ``isnan``. There is a script to help define a portable
c++ macro. It is meant to be used within a configuration file as follows:

```cmake
include("path/to/cookoff/CheckIsNaN.cmake")
if(NOT ISNAN_VARIATION)
  message(STATUS "Could not find working isnan.")
endif(NOT ISNAN_VARIATION)

configure_file(/path/to/config.h.in /path/to/config.h)
```

Two cmake variables are defined:

- ISNAN_HEADERS will the header(s) relevant to the local ``isnan`` definition
- ISNAN_VARIATIOPN is the fully qualified name to the local ``isnan`` definition

They can be used as follows in a the configuration file ``config.h.in``:

``cpp
@ISNAN_HEADERS@

#define not_a_number(X) @ISNAN_VARIATION@
``

One should then use the macro ``not_a_number`` in-place of any ``isnan`` flavour.

In c++11, it is also possible to define a function that takes only arithmetic type, thus obviating
the need for a macro:

```cpp
@ISNAN_HEADERS@
#include <type_traits>

template<class T>
  typename std::enable_if<std::is_arithmetic<T>::value, bool> :: type 
    not_a_number(T const &_in) { return @ISNAN_VARIATION@(_in); }
```

Testing CMake scripts
=====================

The file ``TestCMake.cmake`` contains a function to test cmake scripts. It converts an input cmake
file into a project which is then configured, built, and run using ``ctest``. Unless an optional
"SOURCE" is provided as argument, the test program is an empty ``main`` function returning 0. If the
keyword is provided, then a ``main.cc`` or ``main.c`` file should provided the cmake script.

For examples, look at the tests in this package.
