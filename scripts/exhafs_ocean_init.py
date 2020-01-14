#! /usr/bin/env python

##@namespace scripts.exhwrf_ocean_init
# Runs the ocean initialization for the chosen ocean model and sets
# the ocean status file accordingly.  Does nothing if ocean is disabled
# via [config] run_ocean=no

import os, sys, traceback, shutil
import produtil.log, produtil.setup

# copy files

ocean_init_temp_dir = os.getenv('OCEAN_INIT_TEMP_DIR')
ocean_init_dir = os.getenv('OCEAN_INIT_DIR')

#if os.path.isdir(ocean_init_dir)
#    shutil.rmtree(ocean_init_dir)

try:
    shutil.copytree(ocean_init_temp_dir, ocean_init_dir)
except shutil.Error as e:
    print('Directory not copied. Error: %s' % e)
except OSError as e:
    print('Directory not copied. Error: %s' % e)
