#! /usr/bin/env python3
################################################################################
# Script Name: hafs_calc_enkfgdas_analysis.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script calculate the enksgdas ensemble member analysis based on its
#   history output and corresponding increment file.
# History:
#   07/06/2025: Initial version of the script
# Usage:
#   ./hafs_calc_enkfgdas_analysis.py
#   Input Files:
#     ges.06 - Input first guess file
#     inc.06 - Analysis increment file
#   Output Files:
#     anl.06 - Updated analysis file
# Condition codes:
#    0 - Normal termination
#    1 - Fatal error happened
################################################################################
import sys
from netCDF4 import Dataset
import numpy as np

# Open NetCDF files
ges_nc = Dataset('ges.06', 'r')
inc_nc = Dataset('inc.06', 'r')
anl_nc = Dataset('anl.06', 'r+')

# Variable mapping
var_map = {
    'ugrd': 'u_inc',
    'vgrd': 'v_inc',
    'dpres': 'delp_inc',
    'delz': 'delz_inc',
    'o3mr': 'o3mr_inc',
    'tmp': 'T_inc',
    'spfh': 'sphum_inc',
    'clwmr': 'liq_wat_inc',
    'icmr': 'icmr_inc',
    'rwmr': 'rwmr_inc',
    'snmr': 'snmr_inc',
    'grle': 'grle_inc',
    # pressfc handled separately below
}

# Update all standard variables
for a_var, b_var in var_map.items():
    if a_var not in ges_nc.variables or b_var not in inc_nc.variables:
        print(f"Skipping {a_var}: {b_var}, one of them not found in the input files.")
        continue
    try:
        A = ges_nc.variables[a_var]
        B = inc_nc.variables[b_var]
        if len(A.shape) == 4:
            data_A = A[0, :, :, :]
            data_B = B[:, ::-1, :]  # reverse latitude, N->S in ges_nc, while S->N in inc_nc
            anl_nc.variables[a_var][0, :, :, :] = data_A + data_B
        elif len(A.shape) == 3:
            data_A = A[0, :, :]
            data_B = B[::-1, :]  # reverse latitude, N->S in ges_nc, while S->N in inc_nc
            anl_nc.variables[a_var][0, :, :] = data_A + data_B
        else:
            raise ValueError(f"Unexpected shape for variable {a_var}: {A.shape}")
        print(f"Updated: {a_var} = {a_var} + {b_var}")
    except Exception as e:
        print(f"FATAL ERROR: updating {a_var} with {b_var}: {e}")
        sys.exit(1)

# Special treatment for pressfc
try:
    delp_inc = inc_nc.variables['delp_inc'][:, ::-1, :]  # reverse latitude, N->S in ges_nc, while S->N in inc_nc
    bk = ges_nc.getncattr('bk')  # get bk from attributes
    denom = bk[-2] - bk[-3] # bk are on half levels, and has nlev + 1 levels
    pressfc_inc = delp_inc[-1, :, :] / denom
    pressfc_ges = ges_nc.variables['pressfc'][0, :, :]
    anl_nc.variables['pressfc'][0, :, :] = pressfc_ges + pressfc_inc
    print("Updated: pressfc = pressfc + delp_inc at sfc")
except Exception as e:
    print(f"FATAL ERROR: updating pressfc: {e}")
    sys.exit(1)

# Close files
ges_nc.close()
inc_nc.close()
anl_nc.close()

print("All variable updates complete. Output saved to anl.06.")
