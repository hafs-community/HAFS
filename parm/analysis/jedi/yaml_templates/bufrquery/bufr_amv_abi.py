#!/usr/bin/env python3

import os
import numpy as np

import bufr
from bufr.obs_builder import add_main_functions, map_path
from retrieval_amv_obs_builder import SatWndAmvObsBuilder


MAPPING_PATH = map_path('bufr_amv_abi_mapping.yaml')


class SatWndAmvAbiObsBuilder(SatWndAmvObsBuilder):
    def __init__(self):
        super().__init__(MAPPING_PATH, log_name=os.path.basename(__file__))

    def _get_obs_type(self, swcm, chanfreq):
        obstype = swcm.copy()

        # Use numpy vectorized operations
        obstype = np.where(swcm == 5, 247, obstype)  # WVCA/DL
        obstype = np.where(swcm == 4, 247, obstype)  # WVCA/DL
        obstype = np.where(swcm == 3, 246, obstype)  # WVCT
        obstype = np.where(swcm == 2, 251, obstype)  # VIS
        obstype = np.where(swcm == 1, 245, obstype)  # IRLW

        condition = np.logical_and(swcm == 1, chanfreq >= 5e13)  # IRSW
        obstype = np.where(condition, 240, obstype)

        if not np.any(np.isin(obstype, [247, 246, 251, 245, 240])):
            raise ValueError("Error: Unassigned ObsType found ... ")

        return obstype


# Add main functions create_obs_file and create_obs_group
add_main_functions(SatWndAmvAbiObsBuilder)
