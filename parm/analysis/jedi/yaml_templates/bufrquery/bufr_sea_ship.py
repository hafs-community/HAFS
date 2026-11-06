#!/usr/bin/env python3
import calendar
import os
import numpy as np
import numpy.ma as ma

import bufr
from bufr.obs_builder import add_main_functions
from prepbufr_obs_builder import PrepbufrObsBuilder, map_path, check_include_tv


MAPPING_PATH = map_path('bufr_sea_ship_mapping.yaml')

# Fixed number of temperature event levels handled by this builder.
NUM_T_EVENTS = 5
REF_TIME = os.environ.get("CYC_TIME")
if not REF_TIME:
    raise ValueError(
        "CYC_TIME environment variable must be set in YYYYMMDDHH format"
    )

class SfcshpPrepbufrObsBuilder(PrepbufrObsBuilder):
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
                'name': 'ObsSubType/virtualTemperature',
                'source': 'obsSubType',
                'longName': 'Observation SubType',
            })

        description.add_variables(variables)

        return description

    def make_obs(self, comm, input_path):
        """
        Create the ioda sfcshp prepbufr observations:
        - reads values
        - adds ObsSubType and sequenceNumber

        Parameters
        ----------
        comm: object
                The communicator object (e.g., MPI)
        input_path: str
                The input bufr file
        """

        # Get container from mapping file first
        self.log.info('Get container from bufr')
        container = super().make_obs(comm, input_path)

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

        self.log.debug(f'Do ObsSubType and sequenceNumber calculations')
        typ = container.get('observationType')
        typ_paths = container.get_paths('observationType')
        t29 = container.get('observationSubTypeNum')
        t29_paths = container.get_paths('observationSubTypeNum')
        obsSubType = self._compute_obssubtype(typ, t29)
        self.log.debug(f' obsSubType min/max =  {obsSubType.min()} {obsSubType.max()}')

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

        # Attach the computed temperatures at the report level (typ_paths,
        # */TYP) - one value per report, matching stationPressure and the
        # other surface variables. Anchoring them to the temperature-event
        # sub-sequence path instead lets their missing pattern reshape the
        # shared Location dimension and drop obs from other variables.
        tsen, tsenqm, tsenoe, tvo, tvoqm, tvooe = self._select_temperature_events(
            tpc_events, tob_events, tqm_events, toboe, include_tv, NUM_T_EVENTS)

        self.log.debug(f'Update variables in container')
        container.add('airTemperatureObsValue', tsen, typ_paths)
        container.add('airTemperatureQualityMarker', tsenqm, typ_paths)
        container.replace('airTemperatureObsError', tsenoe)

        if include_tv:
            container.add('virtualTemperatureObsValue', tvo, typ_paths)
            container.add('virtualTemperatureQualityMarker', tvoqm, typ_paths)
            container.add('virtualTemperatureObsError', tvooe, typ_paths)

        # Re-anchor mean-sea-level pressure to report level path, and check OK
        self.log.debug(f'Re-anchor mean-sea-level pressure to the report level')
        pmo_raw = container.get('meanSeaLevelPressureRaw')
        pmq_raw = container.get('meanSeaLevelPressureQualityMarkerRaw')
        if pmo_raw.shape != typ.shape or pmq_raw.shape != typ.shape:
            raise ValueError(
                f'meanSeaLevelPressure arrays have shape {pmo_raw.shape}/{pmq_raw.shape} '
                f'but observationType has shape {typ.shape}; PMSL_SEQ likely reshaped the Location dimension.'
            )
        container.add('pressureReducedToMeanSeaLevelObsValue', pmo_raw, typ_paths)
        container.add('pressureReducedToMeanSeaLevelQualityMarker', pmq_raw, typ_paths)

        self.log.debug(f'Add variables to container')
        # Both 'sequenceNumber' and 'obsSubType' are populated with identical arrays.
        # This is intentional for compatibility with downstream consumers that may expect either field.
        container.add('sequenceNumber', obsSubType, typ_paths)
        container.add('obsSubType', obsSubType, typ_paths)
        container.add('timeOffset', timeoffset, typ_paths)

        # Check
        self.log.debug(f'container list (updated): {container.list()}')

        return container

    def _compute_obssubtype(self, typ, t29):
        """
        Compute obsSubType group

        Parameters:
            typ: observation Type (obsType)
            t29: data dump report type

        Returns:
            Masked array of obsSubType values
        """

        mask_typ = np.isin(typ, [180, 280])
        mask_t29 = (t29 > 555) & (t29 < 565)
        obsSubType = np.where(mask_typ & ~mask_t29, 1, 0).astype(np.int32)

        return obsSubType


add_main_functions(SfcshpPrepbufrObsBuilder)
