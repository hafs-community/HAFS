#!/usr/bin/env python3

import os
import numpy as np

import bufr
from bufr.obs_builder import add_main_functions, map_path
from retrieval_amv_obs_builder import SatWndAmvObsBuilder


MAPPING_PATH = map_path('bufr_amv_seviri_mapping.yaml')


class SatWndAmvSeviriObsBuilder(SatWndAmvObsBuilder):
    def __init__(self):
        super().__init__(MAPPING_PATH, log_name=os.path.basename(__file__))

    def _get_obs_type(self, swcm, chan_freq):
        obstype = swcm.copy()

        # Use numpy vectorized operations
        obstype = np.where(swcm == 5, 254, obstype)  # WVCA/DL
        obstype = np.where(swcm == 3, 254, obstype)  # WVCT
        obstype = np.where(swcm == 2, 243, obstype)  # VIS
        obstype = np.where(swcm == 1, 253, obstype)  # IRLW

        if not np.any(np.isin(obstype, [243, 253, 254])):
            raise ValueError("Error: Unassigned ObsType found ... ")

        return obstype


# Add main functions create_obs_file and create_obs_group
add_main_functions(SatWndAmvSeviriObsBuilder)
