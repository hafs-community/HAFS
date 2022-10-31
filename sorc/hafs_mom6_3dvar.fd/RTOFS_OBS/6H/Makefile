#!/bin/bash

ifort rtofs_obs_read.F90 -O2 -fpp -traceback -xHost -fno-alias -fp-model strict -no-fma -ftz -mkl -DLINUX -convert big_endian -assume byterecl -fixed -real_size 32 -i4 -o rtofs_obs_read

