#!/bin/bash
#SBATCH --account=hurricane
#SBATCH --qos=batch
#SBATCH --ntasks=1

#SBATCH -t 05:30:00
#SBATCH --job-name=compile_hafs
#SBATCH -o test_compilehafs.log
#SBATCH --open-mode=truncate

cd /work/noaa/hwrf/save/jcheng/HAFS_spacstack1p9/sorc
./install_hafs.sh &> install_hafs.log 

exit
