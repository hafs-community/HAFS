#!/usr/bin/env python3

import os
import numpy as np

import bufr
from bufr.obs_builder import ObsBuilder, add_dummy_variable
from bufr.transforms import compute_wind_components


class SatWndAmvObsBuilder(ObsBuilder):
    def __init__(self, mapping_path, log_name=os.path.basename(__file__)):
        super().__init__(mapping_path, log_name=log_name)

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
            if not satId.size:
                self.log.warning(f'category {cat[0]} does not exist in input file')

            self._add_wind_obs(container, cat)
            self._add_metadata(container, cat)

        # Check
        self.log.debug(f'container list (updated): {container.list()}')
        self.log.debug(f'all_sub_categories {container.all_sub_categories()}')

        return container

    # Provide defualt implementations for methods from the ObsBuilder class
    def _make_description(self):
        description = super()._make_description()
        self._add_wind_descriptions(description)
        self._add_metadata_descriptions(description)
        self._remove_descriptions(description)

        return description

    # Methods that are used to extend the export description
    def _add_wind_descriptions(self, description):
        description.add_variables([
            {
                'name': 'ObsType/windEastward',
                'source': 'obstype_uwind',
                'longName': 'Observation Type based on Satellite-derived Wind Computation Method and Spectral Band',
            },
            {
                'name': 'ObsType/windNorthward',
                'source': 'obstype_vwind',
                'longName': 'Observation Type based on Satellite-derived Wind Computation Method and Spectral Band',
            },
            {
                'name': 'ObsValue/windEastward',
                'source': 'windEastward',
                'units': 'm s-1',
                'longName': 'Eastward Wind Component',
            },
            {
                'name': 'ObsValue/windNorthward',
                'source': 'windNorthward',
                'units': 'm s-1',
                'longName': 'Northward Wind Component',
            }])

    def _add_metadata_descriptions(self, description):
        description.add_variables([
            {
                'name': 'MetaData/height',
                'source': 'height',
                'units': 'm',
                'longName': 'Height of Observation',
            },
            {
                'name': 'MetaData/stationElevation',
                'source': 'stationElevation',
                'units': 'm',
                'longName': 'Station Elevation',
            }])

    def _remove_descriptions(self, description):
        description.remove_variable('ObsValue/windDirection')
        description.remove_variable('ObsValue/windSpeed')

    def _add_quality_info_and_gen_app_descriptions(self, description):
        description.add_variables([
            {
                'name': 'MetaData/windGeneratingApplication',
                'source': 'windGeneratingApplication',
                'longName': 'Wind Generating Application',
            },
            {
                'name': 'MetaData/qiWithoutForecast',
                'source': 'qualityInformationWithoutForecast',
                'units': 'percent',
                'longName': 'Quality Information Without Forecast',
            }])

    # Methods that are used to extend the obs data container
    def _add_wind_obs(self, container, cat):
        satId = container.get('satelliteId', cat)
        if not satId.size:
            self.log.warning(f'category {cat[0]} does not exist in input file')

            dummy_mappings = [
                ('obstype_uwind', 'satelliteId'),
                ('obstype_vwind', 'satelliteId'),
                ('windEastward', 'windSpeed'),
                ('windNorthward', 'windSpeed')
            ]
            for target_var, source_var in dummy_mappings:
                add_dummy_variable(container, target_var, cat, source_var)

            return

        # Add new ObsType variables: ObsType/windEastward & ObsType/windNorthward
        swcm = container.get('windComputationMethod', cat)
        chanfreq = container.get('sensorCentralFrequency', cat)
        self.log.debug(f'swcm min/max = {swcm.min()} {swcm.max()}')
        self.log.debug('chanfreq min/max = {chanfreq.min()} {chanfreq.max()}')

        obstype = self._get_obs_type(swcm, chanfreq)

        self.log.debug(f'obstype = {obstype}')
        self.log.debug(f'obstype min/max =  {obstype.min()} {obstype.max()}')

        paths = container.get_paths('windComputationMethod', cat)
        container.add('obstype_uwind', obstype, paths, cat)
        container.add('obstype_vwind', obstype, paths, cat)

        # Add new ObsValue variables: ObsValue/windEastward & ObsValue/windNorthward
        wdir = container.get('windDirection', cat)
        wspd = container.get('windSpeed', cat)

        self.log.debug(f'wdir min/max = {wdir.min()} {wdir.max()}')
        self.log.debug(f'wspd min/max = {wspd.min()} {wspd.max()}')

        uob, vob = compute_wind_components(wspd, wdir)

        self.log.debug(f'uob min/max = {uob.min()} {uob.max()}')
        self.log.debug(f'vob min/max = {vob.min()} {vob.max()}')

        paths = container.get_paths('windSpeed', cat)
        container.add('windEastward', uob, paths, cat)
        container.add('windNorthward', vob, paths, cat)

    def _add_metadata(self, container, cat):
        satId = container.get('satelliteId', cat)
        if not satId.size:
            self.log.warning(f'category {cat[0]} does not exist in input file')

            dummy_mappings = [
                ('height', 'pressure'),
                ('stationElevation', 'pressure')
            ]
            for target_var, source_var in dummy_mappings:
                add_dummy_variable(container, target_var, cat, source_var)

            return

        # Add new MetaData variables: MetaData/height & MetaData/stationElevation
        pressure = container.get("pressure", cat)
        height = np.full_like(pressure, fill_value=pressure.fill_value, dtype=np.float32)
        stnelev = np.full_like(pressure, fill_value=pressure.fill_value, dtype=np.float32)

        paths = container.get_paths('pressure', cat)
        container.add('height', height, paths, cat)
        container.add('stationElevation', stnelev, paths, cat)

    def _add_quality_info_and_gen_app(self, findQi, container, cat):
        # Add new variables: MetaData/windGeneratingApplication and qiWithoutForecast
        gnap2D = container.get('generatingApplication', cat)
        pccf2D = container.get('qualityInformation', cat)
        satId = container.get('satelliteId', cat)

        if not satId.size:

            dummy_mappings = [
                ('windGeneratingApplication', 'windComputationMethod'),
                ('qualityInformationWithoutForecast', 'windSpeed')
            ]
            for target_var, source_var in dummy_mappings:
                add_dummy_variable(container, target_var, cat, source_var)

            return

        gnap, qifn = self._get_quality_info_and_gen_app(findQi, gnap2D, pccf2D)
        self.log.debug(f'gnap min/max = {gnap.min()} {gnap.max()}')
        self.log.debug(f'qifn min/max = {qifn.min()} {qifn.max()}')

        paths = container.get_paths('windComputationMethod', cat)
        container.add('windGeneratingApplication', gnap, paths, cat)
        paths = container.get_paths('windSpeed', cat)
        container.add('qualityInformationWithoutForecast', qifn, paths, cat)

    def _get_obs_type(self, swcm, chan_freq=0):
        """
        Determine the observation type based on `swcm` and `chanfreq`.

        Parameters:
            swcm (array-like): Switch mode values.
            chanfreq (array-like): Channel frequency values (Hz).

        Returns:
            numpy.ndarray: Observation type array.

        Raises:
            ValueError: If any `obstype` is unassigned.
        """

        raise NotImplementedError('Method _get_obs_type must be implemented in derived classes')

    # Private methods
    def _get_quality_info_and_gen_app(self, findQi, gnap2D, pccf2D):
        # For NOAA VIIRS data, qi w/o forecast (qifn) is packaged in same
        # vector of qi with ga = 5 (EUMETSAT QI without forecast). Must
        # conduct a search and extract the correct vector for gnap and qi

        # 1. Find dimension-sizes of ga and qi (should be the same!)
        gDim1, gDim2 = np.shape(gnap2D)
        qDim1, qDim2 = np.shape(pccf2D)
        self.log.info('Generating Application and Quality Information SEARCH')
        self.log.debug(f'Dimension size of GNAP ({gDim1},{gDim2})')
        self.log.debug(f'Dimension size of PCCF ({qDim1},{qDim2})')

        # 2. Initialize gnap and qifn as None, and search for dimension of
        #    ga with values of 5. If the same column exists for qi, assign
        #    gnap to ga[:,i] and qifn to qi[:,i], else raise warning that no
        #    appropriate GNAP/PCCF combination was found
        gnap = None
        qifn = None
        for i in range(gDim2):
            if np.all(np.unique(gnap2D[:, i]) == findQi):
                if i < qDim2:
                    self.log.info(f'GNAP/PCCF found for column {i}')
                    gnap = gnap2D[:, i].copy()
                    qifn = pccf2D[:, i].copy()
                else:
                    self.log.info(f'ERROR: GNAP column {i} outside of PCCF dimension {qDim2}')
        if (gnap is None) & (qifn is None):
            raise ValueError(f'GNAP == {findQi} NOT FOUND OR OUT OF PCCF DIMENSION-RANGE, WILL FAIL!')
        # If EE is needed, key search on np.unique(gnap2D[:,i].squeeze()) == 7 instead
        return gnap, qifn
