#!/usr/bin/env python3
import sys
import os
import argparse
import time
import numpy as np
import bufr
from bufr.bufr_python.encoders import *
from bufr.encoders.netcdf import Encoder as netcdfEncoder
from bufr.obs_builder import add_main_functions, map_path, add_dummy_variable
from wxflow import Logger

# Initialize Logger
log_level = os.getenv('LOG_LEVEL', 'INFO')
logger = Logger('BUFR2IODA_satwhr_amv_goes.py', level=log_level, colored_log=False)


def logging(comm, level, message):
    if comm.rank() == 0:
        log_methods = {
            'DEBUG': logger.debug,
            'INFO': logger.info,
            'WARNING': logger.warning,
            'ERROR': logger.error,
            'CRITICAL': logger.critical,
        }
        log_method = log_methods.get(level.upper(), logger.info)
        if log_method == logger.info and level.upper() not in log_methods:
            logger.warning(f'log level = {level}: not a valid level --> set to INFO')
        log_method(message)


def _make_description(mapping_path, update=False):
    description = bufr.encoders.Description(mapping_path)

    if update:
        variables = [
            {
                'name': 'ObsType/windEastward',
                'source': 'variables/obstype_uwind',
                'units': '1',
                'longName': 'Observation Type based on Satellite-derived Wind Computation Method and Spectral Band',
            },
            {
                'name': 'ObsType/windNorthward',
                'source': 'variables/obstype_vwind',
                'units': '1',
                'longName': 'Observation Type based on Satellite-derived Wind Computation Method and Spectral Band',
            },
            {
                'name': 'ObsValue/windEastward',
                'source': 'variables/windEastward',
                'units': 'm s-1',
                'longName': 'Eastward Wind Component',
            },
            {
                'name': 'ObsValue/windNorthward',
                'source': 'variables/windNorthward',
                'units': 'm s-1',
                'longName': 'Northward Wind Component',
            },
            {
                'name': 'MetaData/height',
                'source': 'variables/height',
                'units': 'm',
                'longName': 'Observation Height',
            },
            {
                'name': 'MetaData/stationElevation',
                'source': 'variables/stationElevation',
                'units': 'm',
                'longName': 'Station Height',
            },
            {
                'name': 'MetaData/expectedError',
                'source': 'variables/expectedError',
                'units': 'm s-1',
                'longName': 'Expected Error',
            },
            {
                'name': 'MetaData/coefficientOfVariation',
                'source': 'variables/coefficientOfVariation',
                'units': '',
                'longName': 'Coefficient of Variation',
            },
        ]

        for var in variables:
            description.add_variable(
                name=var['name'],
                source=var['source'],
                units=var['units'],
                longName=var['longName']
            )

    return description


def compute_wind_components(wdir, wspd):
    wdir_rad = np.radians(wdir)
    u = -wspd * np.sin(wdir_rad)
    v = -wspd * np.cos(wdir_rad)
    return u.astype(np.float32), v.astype(np.float32)


def _get_obs_type(swcm, chanfreq):
    if swcm.size == 0:
        return np.array([], dtype=swcm.dtype)

    obstype = np.array(swcm, copy=True)

    valid_mask = ~np.isnan(swcm) if np.issubdtype(swcm.dtype, np.floating) else np.ones(swcm.shape, dtype=bool)
    if hasattr(swcm, 'mask'):
        valid_mask &= ~swcm.mask

    condition = np.logical_and(swcm >= 1, swcm < 7) & valid_mask
    obstype = np.where(condition, 241, obstype)

    if not np.any(np.isin(obstype[valid_mask], [241])):
        raise ValueError("Error: Unassigned ObsType found ... ")

    return obstype


def _is_invalid_array(arr):
    """Utility helper to check if a BUFR data array is empty or contains only missing values."""
    if arr is None or arr.size == 0:
        return True
    if hasattr(arr, 'mask') and np.all(arr.mask):
        return True
    if np.issubdtype(arr.dtype, np.floating) and np.all(np.isnan(arr)):
        return True
    str_repr = str(arr)
    if str_repr.strip() == '--' or np.all(arr == '--'):
        return True
    return False


def _populate_dummies(container, cat):
    """Adds dummy placeholders for categories missing valid observation data."""
    dummy_mappings = [
        ('windEastward', 'windSpeed'),
        ('windNorthward', 'windSpeed'),
        ('coefficientOfVariation', 'windComputationMethod'),
        ('expectedError', 'windComputationMethod'),
        ('height', 'windComputationMethod'),
        ('stationElevation', 'windComputationMethod'),
        ('obstype_uwind', 'windComputationMethod'),
        ('obstype_vwind', 'windComputationMethod')
    ]
    for target_var, source_var in dummy_mappings:
        add_dummy_variable(container, target_var, cat, source_var)


def _make_obs(comm, input_path, mapping_path):
    logging(comm, 'INFO', 'Get container from bufr')
    try:
        container = bufr.Parser(input_path, mapping_path).parse(comm)
    except RuntimeError as e:
        logging(comm, 'ERROR', f"Failed to process BUFR file: {input_path}. Reason: {e}")
        return None

    logging(comm, 'DEBUG', f'container list (original): {container.list()}')
    logging(comm, 'DEBUG', f'all_sub_categories = {container.all_sub_categories()}')
    logging(comm, 'DEBUG', f'category map = {container.get_category_map()}')

    for cat in container.all_sub_categories():
        logging(comm, 'DEBUG', f'category = {cat}')

        satid = container.get('variables/satelliteId', cat)
        if _is_invalid_array(satid):
            logging(comm, 'WARNING', f'Skipping category {cat[0]}: satelliteId contains no valid records.')
            _populate_dummies(container, cat)
            continue

        swcm = container.get('variables/windComputationMethod', cat)
        chanfreq = container.get('variables/sensorCentralFrequency', cat)

        if _is_invalid_array(swcm) or _is_invalid_array(chanfreq):
            logging(comm, 'WARNING', f'Skipping category {cat[0]}: missing or unassigned wind computation/frequency data.')
            _populate_dummies(container, cat)
            continue

        logging(comm, 'DEBUG', f'swcm min/max = {swcm.min()} {swcm.max()}')
        logging(comm, 'DEBUG', f'chanfreq min/max = {chanfreq.min()} {chanfreq.max()}')

        try:
            obstype = _get_obs_type(swcm, chanfreq)
        except ValueError as e:
            logging(comm, 'WARNING', f'Skipping category {cat[0]}: {e}')
            _populate_dummies(container, cat)
            continue

        logging(comm, 'DEBUG', f'obstype = {obstype}')
        logging(comm, 'DEBUG', f'obstype min/max = {obstype.min()} {obstype.max()}')

        paths = container.get_paths('variables/windComputationMethod', cat)
        container.add('variables/obstype_uwind', obstype, paths, cat)
        container.add('variables/obstype_vwind', obstype, paths, cat)

        wdir = container.get('variables/windDirection', cat)
        wspd = container.get('variables/windSpeed', cat)

        if _is_invalid_array(wdir) or _is_invalid_array(wspd):
            logging(comm, 'WARNING', f'Skipping category {cat[0]}: wind direction or speed contains invalid data.')
            _populate_dummies(container, cat)
            continue

        logging(comm, 'DEBUG', f'wdir min/max = {wdir.min()} {wdir.max()}')
        logging(comm, 'DEBUG', f'wspd min/max = {wspd.min()} {wspd.max()}')

        uob, vob = compute_wind_components(wdir, wspd)

        logging(comm, 'DEBUG', f'uob min/max = {uob.min()} {uob.max()}')
        logging(comm, 'DEBUG', f'vob min/max = {vob.min()} {vob.max()}')

        paths = container.get_paths('variables/windSpeed', cat)
        container.add('variables/windEastward', uob, paths, cat)
        container.add('variables/windNorthward', vob, paths, cat)

        height = np.full_like(swcm, np.nan, dtype=np.float32)
        container.add('variables/height', height, paths, cat)
        stheight = np.full_like(swcm, np.nan, dtype=np.float32)
        container.add('variables/stationElevation', stheight, paths, cat)

        ee = np.full_like(swcm, 1.0, dtype=np.float32)
        container.add('variables/expectedError', ee, paths, cat)
        logging(comm, 'DEBUG', f'ee min/max = {ee.min()}, {ee.max()}')

        pct1 = np.full_like(swcm, 0.4, dtype=np.float32)
        container.add('variables/coefficientOfVariation', pct1, paths, cat)
        logging(comm, 'DEBUG', f'pct1 min/max = {pct1.min()}, {pct1.max()}')

        qifn = container.get('variables/qiWithoutForecast', cat)
        qifn_paths = container.get_paths('variables/qiWithoutForecast', cat)
        logging(comm, 'DEBUG', f'qifn min/max = {qifn.min()}, {qifn.max()}')

        satzenith = container.get('variables/satelliteZenithAngle', cat)
        satzenith_paths = container.get_paths('variables/satelliteZenithAngle', cat)
        logging(comm, 'DEBUG', f'satzenith min/max = {satzenith.min()}, {satzenith.max()}')

        satzenith = np.full(satzenith.shape, 61.23, dtype=satzenith.dtype)
        container.replace('variables/satelliteZenithAngle', satzenith, cat)

    logging(comm, 'DEBUG', f'container list (updated): {container.list()}')
    logging(comm, 'DEBUG', f'all_sub_categories {container.all_sub_categories()}')

    return container


def create_obs_group(input_path, mapping_path, category, env):
    comm = bufr.mpi.Comm(env["comm_name"])
    description = _make_description(mapping_path, update=True)

    logging(comm, 'DEBUG', f'Check if bufr.DataCache exists? {bufr.DataCache.has(input_path, mapping_path)}')
    if bufr.DataCache.has(input_path, mapping_path):
        container = bufr.DataCache.get(input_path, mapping_path)
        logging(comm, 'INFO', f'Encode {category} from cache')
        data = iodaEncoder(description).encode(container)[(category,)]
        logging(comm, 'INFO', f'Mark {category} as finished in the cache')
        bufr.DataCache.mark_finished(input_path, mapping_path, [category])
        logging(comm, 'INFO', f'Return the encoded data for {category}')
        return data

    container = _make_obs(comm, input_path, mapping_path)

    logging(comm, 'INFO', f'Gather data from all tasks into all tasks')
    container.all_gather(comm)

    logging(comm, 'INFO', f'Add container to cache')
    bufr.DataCache.add(input_path, mapping_path, container.all_sub_categories(), container)

    logging(comm, 'INFO', f'Encode {category}')
    data = iodaEncoder(description).encode(container)[(category,)]

    logging(comm, 'INFO', f'Mark {category} as finished in the cache')
    bufr.DataCache.mark_finished(input_path, mapping_path, [category])

    logging(comm, 'INFO', f'Return the encoded data for {category}')
    return data


def create_obs_file(input_path, mapping_path, output_path):
    comm = bufr.mpi.Comm("world")
    container = _make_obs(comm, input_path, mapping_path)

    container.gather(comm)
    description = _make_description(mapping_path, update=True)

    if comm.rank() == 0:
        netcdfEncoder(description).encode(container, output_path)

    logging(comm, 'INFO', f'Return the encoded data')


if __name__ == '__main__':

    start_time = time.time()

    bufr.mpi.App(sys.argv)
    comm = bufr.mpi.Comm("world")

    parser = argparse.ArgumentParser(description="Convert BUFR to NetCDF using a mapping file.")
    parser.add_argument('input', type=str, help='Input BUFR file')
    parser.add_argument('mapping', type=str, help='BUFR2IODA Mapping File')
    parser.add_argument('output', type=str, help='Output NetCDF file')

    args = parser.parse_args()
    infile = args.input
    mapping = args.mapping
    output = args.output

    create_obs_file(infile, mapping, output)

    end_time = time.time()
    running_time = end_time - start_time
    logging(comm, 'INFO', f'Total running time: {running_time}')
