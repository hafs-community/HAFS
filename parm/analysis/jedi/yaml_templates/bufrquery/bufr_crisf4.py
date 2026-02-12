#!/usr/bin/env python3
import os
import numpy as np
import numpy.ma as ma

import bufr
from bufr.obs_builder import ObsBuilder, add_main_functions, map_path


MAPPING_PATH = map_path('bufr_crisf4_mapping.yaml')


class BufrCrisObsBuilder(ObsBuilder):
    def __init__(self):
        super().__init__(MAPPING_PATH, log_name=os.path.basename(__file__))

    def make_obs(self, comm, input_path):
        # Get container from mapping file first
        self.log.info('Get container from bufr')
        container = super().make_obs(comm, input_path)

        self.log.debug(f'container list (original): {container.list()}')
        self.log.debug(f'all_sub_categories =  {container.all_sub_categories()}')
        self.log.debug(f'category map =  {container.get_category_map()}')

        # Add new/derived data into container
        for cat in container.all_sub_categories():
            self.log.debug(f'category = {cat}')

            satId = container.get('satelliteId', cat)
            if not np.any(satId):
                self.log.warning(f'category {cat[0]} does not exist in input file')

        # Check
        self.log.debug(f'container list (updated): {container.list()}')
        self.log.debug(f'all_sub_categories {container.all_sub_categories()}')

        return container


add_main_functions(BufrCrisObsBuilder)
