#!/usr/bin/env python3
import calendar
import os
import numpy as np
import numpy.ma as ma

import bufr
from bufr.obs_builder import add_main_functions
from prepbufr_obs_builder import PrepbufrObsBuilder, map_path, check_include_tv


MAPPING_PATH = map_path('bufr_land_synop_mapping.yaml')
# Fixed number of temperature event levels handled by this builder.
NUM_T_EVENTS = 5
REF_TIME = os.environ.get("CYC_TIME")

# - If ObsType/virtualTemperature is in the encoder variables, use Tv if available otherwise Tdry.
#   This option mimics the GSI's TSENSIBLE=False default
# - If ObsType/virtualTemperature not in encoder variables, always use Tdry
#   This is what we want to do long-term


class AdpsfcPrepbufrObsBuilder(PrepbufrObsBuilder):
    def __init__(self):
        super().__init__(MAPPING_PATH, log_name=os.path.basename(__file__))

    def _make_description(self):
        description = super()._make_description()

        variables = [
            {
                'name': 'MetaData/sequenceNumber',
                'source': 'sequenceNumber',
                'longName': 'Sequence Number (Obs Subtype)',
            },
            {
                 'name': 'MetaData/timeOffset',
                 'source': 'timeOffset',
                 'units': 's',
                 'longName': 'Observation Time Minus Reference Time',
            },
            {
                'name': 'ObsSubType/stationPressure',
                'source': 'obsSubType',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/airTemperature',
                'source': 'obsSubType',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/specificHumidity',
                'source': 'obsSubType',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/windEastward',
                'source': 'obsSubType',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/windNorthward',
                'source': 'obsSubType',
                'longName': 'Observation SubType',
            }
        ]

        if check_include_tv(MAPPING_PATH):
            variables.append({
                'name': 'ObsSubType/virtualTemperatureM',
                'source': 'obsSubType',
                'longName': 'Observation SubType',
            })

        description.add_variables(variables)

        return description

    def make_obs(self, comm, input_path):
        """
        Create the ioda adpsfc prepbufr observations:
        - reads values
        - adds sequenceNum
        - adds ObsSubType
        - extracts Tdry from event stack (peels back through events if top is Tv)

        Parameters
        ----------
        comm: object
                The communicator object (e.g., MPI)
        input_path: str
                The input bufr file
        """

        container = super().make_obs(comm, input_path)

        # Get container from mapping file first
        self.log.info('Get container from bufr')
        container = bufr.Parser(input_path, MAPPING_PATH).parse(comm)

        self.log.debug(f'container list (original): {container.list()}')

        self.log.debug(f'Do DateTime calculation')
        dhr = container.get('obsTimeMinusCycleTime')
        dhr_paths = container.get_paths('obsTimeMinusCycleTime')
        dhr2 = np.array(dhr)
        #self._replace_timestamp(container, self._get_reference_time(input_path))
        self._replace_timestamp(container, self._get_reference_time(REF_TIME))

        self.log.debug('add timeOffset, in units of seconds')
        timeoffset = np.ma.array((dhr.filled(-9999) * 3600).astype(np.int64),
            mask=np.ma.getmask(dhr), fill_value=-9999)
        self.log.debug(f'timeOffset min/max = {timeoffset.min()} {timeoffset.max()}')

        self.log.debug(f'Make an array of 0s for MetaData/sequenceNumber and ObsSubType')
        sequenceNum = np.zeros(dhr.shape, dtype=np.int32)
        self.log.debug(f' sequenceNum min/max =  {sequenceNum.min()} {sequenceNum.max()}')

        include_tv = check_include_tv(MAPPING_PATH)
        self.log.debug(f'Extract temperature from event stack (include_tv={include_tv})')

        # get record-specific data
        toboe = container.get('airTemperatureObsError')

        # get event-specific data
        tpc_events = []
        tob_events = []
        tqm_events = []
        for i in range(1, NUM_T_EVENTS + 1):
            tpc_events.append(container.get(f'temperatureEventCode{i}'))
            tob_events.append(container.get(f'temperatureOb{i}'))
            tqm_events.append(container.get(f'temperatureQM{i}'))

        # Attach the computed temperatures at the report level (dhr_paths,
        # */DHR) - one value per report, matching stationPressure and the
        # other surface variables. Anchoring them to the temperature-event
        # sub-sequence path instead lets their missing pattern reshape the
        # shared Location dimension and drop obs from other variables.
        tsen, tsenqm, tsenoe, tvo, tvoqm, tvooe = self._select_temperature_events(
            tpc_events, tob_events, tqm_events, toboe, include_tv, NUM_T_EVENTS)

        self.log.debug(f'Update variables in container')
        container.add('airTemperatureObsValue', tsen, dhr_paths)
        container.add('airTemperatureQualityMarker', tsenqm, dhr_paths)
        container.replace('airTemperatureObsError', tsenoe)

        if include_tv:
            container.add('virtualTemperatureObsValue', tvo, dhr_paths)
            container.add('virtualTemperatureQualityMarker', tvoqm, dhr_paths)
            container.add('virtualTemperatureObsError', tvooe, dhr_paths)

        self.log.debug(f'Add variables to container')
        container.add('sequenceNumber', sequenceNum, dhr_paths)
        container.add('obsSubType', sequenceNum, dhr_paths)
        container.add('timeOffset', timeoffset, dhr_paths)

        self.log.debug(f'container list (updated): {container.list()}')

        return container


add_main_functions(AdpsfcPrepbufrObsBuilder)
