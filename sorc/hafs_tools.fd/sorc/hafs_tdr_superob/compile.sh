#!/bin/sh


module use /work/noaa/aoml-hafsda/bdahl/HAFSv2_tdrswitch/modulefiles 
module load hafs.orion
module load bufr

ifort -O2  -g -traceback -warn unused -c -I/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/include/bufr_4 -L/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/lib64 -lbufr_4 module_tdr_common.f90
ifort -O2  -g -traceback -warn unused -c -I/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/include/bufr_4 -L/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/lib64 -lbufr_4 module_tdr_superob.f90
ifort -O2  -g -traceback -warn unused -c -I/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/include/bufr_4 -L/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/lib64 -lbufr_4 module_tdr_bufr.f90
ifort -O2  -g -traceback -warn unused -c -I/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/include/bufr_4 -L/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/lib64 -lbufr_4 tdr_so_main.f90
ifort -O2  -g -traceback -warn unused -I/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/include/bufr_4 -L/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/bufr-12.0.1-aakabgj/lib64 -lbufr_4 -o tdr_superob_tool.exe module_tdr_common.o module_tdr_bufr.o module_tdr_superob.o tdr_so_main.o
