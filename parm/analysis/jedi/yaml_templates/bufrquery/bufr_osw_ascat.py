#!/usr/bin/env python3

import os
import numpy as np
import calendar

import bufr
from bufr.obs_builder import ObsBuilder, add_main_functions, map_path, add_dummy_variable


MAPPING_PATH = map_path('bufr_osw_ascat_mapping.yaml')


class AscatwPrepbufrObsBuilder(ObsBuilder):
    """
    A builder class to generate satellite wind observations from ASCAT PrepBUFR input.

    """

    def __init__(self):
        super().__init__(MAPPING_PATH, log_name=os.path.basename(__file__))

    def _make_description(self):
        description = super()._make_description()

        description.add_variables([
            {
                'name': 'MetaData/stationElevation',
                'source': 'stationElevation',
                'longName': 'station Elevation (0 for ascat)',
            },
        ])

        return description

    def make_obs(self, comm, input_path):
        
        # Get container from mapping file first
        self.log.info('Get container from bufr')
        container = super().make_obs(comm, input_path)

        self.log.debug(f'container list (original): {container.list()}')

        # grep lon path
        lon = container.get('longitude')
        lon_paths = container.get_paths('longitude')

        # add stationElevation
        stationElevation = np.zeros(lon.shape, dtype=np.int32)
        container.add('stationElevation', stationElevation, lon_paths)

        # Check
        self.log.debug(f'container list (updated): {container.list()}')
        self.log.debug(f'all_sub_categories {container.all_sub_categories()}')

        return container

# Add main functions create_obs_file or create_obs_group
add_main_functions(AscatwPrepbufrObsBuilder)
