#!/usr/bin/env python3
import os
import time
import calendar
import numpy as np
import numpy.ma as ma

import bufr
from bufr.obs_builder import ObsBuilder, add_main_functions, map_path


MAPPING_PATH = map_path('bufr_crisf4_mapping.yaml')


def ComputeTimeOffset(obtime, cycle_time):
    """Compute observation time minus cycle time, in seconds."""

    cycleTimeSinceEpoch = np.int64(
        calendar.timegm(time.strptime(str(int(cycle_time)), '%Y%m%d%H'))
    )

    # Preserve missing/masked observation times.
    time_diff = ma.array(obtime, copy=True) - cycleTimeSinceEpoch

    return time_diff.astype(np.float32)


CYCLE_TIME = None


def get_cycle_time():
    """Return the cycle time supplied through the command line."""

    if CYCLE_TIME is None:
        raise ValueError(
            "Cycle time was not provided. Use --cycle-time YYYYMMDDHH."
        )

    return CYCLE_TIME


class BufrCrisObsBuilder(ObsBuilder):
    def __init__(self):
        super().__init__(MAPPING_PATH, log_name=os.path.basename(__file__))

    def _make_description(self):
        """Extend the mapping description with derived MetaData/timeOffset."""

        description = super()._make_description()

        description.add_variable(
            name='MetaData/timeOffset',
            source='timeOffset',
            units='s',
            longName='Observation Time Minus Reference Time'
        )

        return description

    def make_obs(self, comm, input_path):
        # Get container from mapping file first
        self.log.info('Get container from bufr')
        container = super().make_obs(comm, input_path)

        self.log.debug(f'container list (original): {container.list()}')

        self.log.debug(f'all_sub_categories =  {container.all_sub_categories()}')
        self.log.debug(f'category map =  {container.get_category_map()}')

        # Add new/derived data into container.
        # CrIS fields are stored by sub-category rather than in __MAIN__,
        # so timeOffset must be created separately for every category.
        cycle_time = get_cycle_time()

        for cat in container.all_sub_categories():
            self.log.debug(f'category = {cat}')

            satId = container.get('satelliteId', cat)
            if not np.any(satId):
                self.log.warning(f'category {cat[0]} does not exist in input file')

            paths = container.get_paths('timestamp', cat)
            obtime = container.get('timestamp', cat)
            time_diff = ComputeTimeOffset(obtime, cycle_time)

            if comm.rank() == 0:
                print(f'TIMEOFFSET DEBUG: cat = {cat}')
                print(f'TIMEOFFSET DEBUG: paths = {paths}')
                print(f'TIMEOFFSET DEBUG: obtime shape = {obtime.shape}')
                print(f'TIMEOFFSET DEBUG: time_diff shape = {time_diff.shape}')
                if obtime.size > 0:
                    print(f'TIMEOFFSET DEBUG: first obs times = {obtime[:5]}')
                    print(f'TIMEOFFSET DEBUG: first time offsets = {time_diff[:5]}')

            container.add('timeOffset', time_diff, paths, cat)

        # Check
        self.log.debug(f'container list (updated): {container.list()}')
        self.log.debug(f'all_sub_categories {container.all_sub_categories()}')

        return container


# add_main_functions() owns the normal ObsBuilder command line
# (--input, --output, --type).  Pull out our extra cycle-time option first
# so its parser does not reject it.
import sys

if '--cycle-time' in sys.argv:
    idx = sys.argv.index('--cycle-time')
    if idx + 1 >= len(sys.argv):
        raise ValueError('--cycle-time requires a YYYYMMDDHH value')
    CYCLE_TIME = sys.argv[idx + 1]
    del sys.argv[idx:idx + 2]
elif '--cdate' in sys.argv:
    idx = sys.argv.index('--cdate')
    if idx + 1 >= len(sys.argv):
        raise ValueError('--cdate requires a YYYYMMDDHH value')
    CYCLE_TIME = sys.argv[idx + 1]
    del sys.argv[idx:idx + 2]
else:
    raise ValueError(
        'Cycle time is required. Use --cycle-time YYYYMMDDHH '
        '(or --cdate YYYYMMDDHH).'
    )

add_main_functions(BufrCrisObsBuilder)
