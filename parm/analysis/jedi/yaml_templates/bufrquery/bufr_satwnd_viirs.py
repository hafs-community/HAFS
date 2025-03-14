#!/usr/bin/env python3
import sys
import os
import argparse
import time
import numpy as np
import bufr
from pyioda.ioda.Engines.Bufr import Encoder as iodaEncoder
from bufr.encoders.netcdf import Encoder as netcdfEncoder
from wxflow import Logger

# Initialize Logger
# Get log level from the environment variable, default to 'INFO it not set
log_level = os.getenv('LOG_LEVEL', 'INFO')
logger = Logger('BUFR2IODA_satwnd_amv_viirs.py', level=log_level, colored_log=False)


def logging(comm, level, message):

    if comm.rank() == 0:
        # Define a dictionary to map levels to logger methods
        log_methods = {
            'DEBUG': logger.debug,
            'INFO': logger.info,
            'WARNING': logger.warning,
            'ERROR': logger.error,
            'CRITICAL': logger.critical,
        }

        # Get the appropriate logging method, default to 'INFO'
        log_method = log_methods.get(level.upper(), logger.info)

        if log_method == logger.info and level.upper() not in log_methods:
            # Log a warning if the level is invalid
            logger.warning(f'log level = {level}: not a valid level --> set to INFO')

        # Call the logging method
        log_method(message)


def _make_description(mapping_path, update=False):
    description = bufr.encoders.Description(mapping_path)

    if update:
        # Define the variables to be added in a list of dictionaries
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
            # MetaData/windGeneratingApplication will be inferred from variables/generatingApplication
            # following a search for the proper variables/generatingApplication column
            {
                'name': 'MetaData/windGeneratingApplication',
                'source': 'variables/windGeneratingApplication',
                'units': '1',
                'longName': 'Wind Generating Application',
            },
            # MetaData/qualityInformationWithoutForecast will be inferred from variables/qualityInformation
            # following a search for the proper variables/generatingApplication column
            {
                'name': 'MetaData/qualityInformationWithoutForecast',
                'source': 'variables/qualityInformationWithoutForecast',
                'units': 'percent',
                'longName': 'Quality Information Without Forecast',
            }
        ]

        # Loop through each variable and add it to the description
        for var in variables:
            description.add_variable(
                name=var['name'],
                source=var['source'],
                units=var['units'],
                longName=var['longName']
            )

    return description


def compute_wind_components(wdir, wspd):
    """
    Compute the U and V wind components from wind direction and wind speed.

    Parameters:
        wdir (array-like): Wind direction in degrees (meteorological convention: 0° = North, 90° = East).
        wspd (array-like): Wind speed.

    Returns:
        tuple: U and V wind components as numpy arrays with dtype float32.
    """
    wdir_rad = np.radians(wdir)  # Convert degrees to radians
    u = -wspd * np.sin(wdir_rad)
    v = -wspd * np.cos(wdir_rad)

    return u.astype(np.float32), v.astype(np.float32)


def _get_quality_information_and_generating_application(comm, gnap2D, pccf2D):
    # For NOAA VIIRS data, qi w/o forecast (qifn) is packaged in same
    # vector of qi with ga = 5 (EUMETSAT QI without forecast). Must
    # conduct a search and extract the correct vector for gnap and qi
    findQI = 5
    # 1. Find dimension-sizes of ga and qi (should be the same!)
    gDim1, gDim2 = np.shape(gnap2D)
    qDim1, qDim2 = np.shape(pccf2D)
    logging(comm, 'INFO', 'Generating Application and Quality Information SEARCH')
    logging(comm, 'DEBUG', f'Dimension size of GNAP ({gDim1},{gDim2})')
    logging(comm, 'DEBUG', f'Dimension size of PCCF ({qDim1},{qDim2})')
    # 2. Initialize gnap and qifn as None, and search for dimension of
    #    ga with values of 5. If the same column exists for qi, assign
    #    gnap to ga[:,i] and qifn to qi[:,i], else raise warning that no
    #    appropriate GNAP/PCCF combination was found
    gnap = None
    qifn = None
    for i in range(gDim2):
        if np.unique(gnap2D[:, i].squeeze()) == findQI:
            if i <= qDim2:
                logging(comm, 'INFO', f'GNAP/PCCF found for column {i}')
                gnap = gnap2D[:, i].squeeze()
                qifn = pccf2D[:, i].squeeze()
            else:
                logging(comm, 'INFO', f'ERROR: GNAP column {i} outside of PCCF dimension {qDim2}')
    if (gnap is None) & (qifn is None):
        raise ValueError(f'GNAP == {findQI} NOT FOUND OR OUT OF PCCF DIMENSION-RANGE, WILL FAIL!')
    # If EE is needed, key search on np.unique(gnap2D[:,i].squeeze()) == 7 instead
    # NOTE: Make sure to return np.float32 or np.int32 types as appropriate!!!
    return gnap.astype(np.int32), qifn.astype(np.int32)


def _get_obs_type(swcm):
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

    obstype = swcm.copy()

    # Use numpy vectorized operations
    obstype = np.where(swcm == 1, 260, obstype)  # IRLW

    if not np.any(np.isin(obstype, [260])):
        raise ValueError("Error: Unassigned ObsType found ... ")

    return obstype


def _make_obs(comm, input_path, mapping_path):

    # Get container from mapping file first
    logging(comm, 'INFO', 'Get container from bufr')
    container = bufr.Parser(input_path, mapping_path).parse(comm)

    logging(comm, 'DEBUG', f'container list (original): {container.list()}')
    logging(comm, 'DEBUG', f'all_sub_categories =  {container.all_sub_categories()}')
    logging(comm, 'DEBUG', f'category map =  {container.get_category_map()}')

    # Add new/derived data into container
    for cat in container.all_sub_categories():

        logging(comm, 'DEBUG', f'category = {cat}')

        satid = container.get('variables/satelliteId', cat)
        if satid.size == 0:
            logging(comm, 'WARNING', f'category {cat[0]} does not exist in input file')
            paths = container.get_paths('variables/windComputationMethod', cat)
            obstype = container.get('variables/windComputationMethod', cat)
            container.add('variables/obstype_uwind', obstype, paths, cat)
            container.add('variables/obstype_vwind', obstype, paths, cat)

            paths = container.get_paths('variables/windSpeed', cat)
            wob = container.get('variables/windSpeed', cat)
            container.add('variables/windEastward', wob, paths, cat)
            container.add('variables/windNorthward', wob, paths, cat)

            paths = container.get_paths('variables/windComputationMethod', cat)
            dummy = container.get('variables/windSpeed', cat)
            container.add('variables/windGeneratingApplication', dummy, paths, cat)
            container.add('variables/qualityInformationWithoutForecast', dummy, paths, cat)

        else:
            # Add new variables: ObsType/windEastward & ObsType/windNorthward
            swcm = container.get('variables/windComputationMethod', cat)
            chanfreq = container.get('variables/sensorCentralFrequency', cat)

            logging(comm, 'DEBUG', f'swcm min/max = {swcm.min()} {swcm.max()}')
            logging(comm, 'DEBUG', f'chanfreq min/max = {chanfreq.min()} {chanfreq.max()}')

            obstype = _get_obs_type(swcm)

            logging(comm, 'DEBUG', f'obstype = {obstype}')
            logging(comm, 'DEBUG', f'obstype min/max =  {obstype.min()} {obstype.max()}')

            paths = container.get_paths('variables/windComputationMethod', cat)
            container.add('variables/obstype_uwind', obstype, paths, cat)
            container.add('variables/obstype_vwind', obstype, paths, cat)

            # Add new variables: ObsValue/windEastward & ObsValue/windNorthward
            wdir = container.get('variables/windDirection', cat)
            wspd = container.get('variables/windSpeed', cat)

            logging(comm, 'DEBUG', f'wdir min/max = {wdir.min()} {wdir.max()}')
            logging(comm, 'DEBUG', f'wspd min/max = {wspd.min()} {wspd.max()}')

            uob, vob = compute_wind_components(wdir, wspd)

            logging(comm, 'DEBUG', f'uob min/max = {uob.min()} {uob.max()}')
            logging(comm, 'DEBUG', f'vob min/max = {vob.min()} {vob.max()}')

            paths = container.get_paths('variables/windSpeed', cat)
            container.add('variables/windEastward', uob, paths, cat)
            container.add('variables/windNorthward', vob, paths, cat)

            # Add new variables: MetaData/windGeneratingApplication and qualityInformationWithoutForecast
            gnap2D = container.get('variables/generatingApplication', cat)
            pccf2D = container.get('variables/qualityInformation', cat)

            gnap, qifn = _get_quality_information_and_generating_application(comm, gnap2D, pccf2D)

            logging(comm, 'DEBUG', f'gnap min/max = {gnap.min()} {gnap.max()}')
            logging(comm, 'DEBUG', f'qifn min/max = {qifn.min()} {qifn.max()}')

            paths = container.get_paths('variables/windComputationMethod', cat)
            container.add('variables/windGeneratingApplication', gnap, paths, cat)
            container.add('variables/qualityInformationWithoutForecast', qifn, paths, cat)

    # Check
    logging(comm, 'DEBUG', f'container list (updated): {container.list()}')
    logging(comm, 'DEBUG', f'all_sub_categories {container.all_sub_categories()}')

    return container


def create_obs_group(input_path, mapping_path, category, env):

    comm = bufr.mpi.Comm(env["comm_name"])

    description = _make_description(mapping_path, update=True)

    # Check the cache for the data and return it if it exists
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

    # Gather data from all tasks into all tasks. Each task will have the complete record
    logging(comm, 'INFO', f'Gather data from all tasks into all tasks')
    container.all_gather(comm)

    logging(comm, 'INFO', f'Add container to cache')
    # Add the container to the cache
    bufr.DataCache.add(input_path, mapping_path, container.all_sub_categories(), container)

    # Encode the data
    logging(comm, 'INFO', f'Encode {category}')
    data = iodaEncoder(description).encode(container)[(category,)]

    logging(comm, 'INFO', f'Mark {category} as finished in the cache')
    # Mark the data as finished in the cache
    bufr.DataCache.mark_finished(input_path, mapping_path, [category])

    logging(comm, 'INFO', f'Return the encoded data for {category}')
    return data


def create_obs_file(input_path, mapping_path, output_path):

    comm = bufr.mpi.Comm("world")
    container = _make_obs(comm, input_path, mapping_path)
    container.gather(comm)

    description = _make_description(mapping_path, update=True)

    # Encode the data
    if comm.rank() == 0:
        netcdfEncoder(description).encode(container, output_path)

    logging(comm, 'INFO', f'Return the encoded data')


if __name__ == '__main__':

    start_time = time.time()

    bufr.mpi.App(sys.argv)
    comm = bufr.mpi.Comm("world")

    # Required input arguments as positional arguments
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
