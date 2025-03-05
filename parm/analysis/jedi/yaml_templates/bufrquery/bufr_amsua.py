#!/usr/bin/env python3
import argparse
import bufr
import netCDF4 as nc
import os
import sys
import time
from pyioda.ioda.Engines.Bufr import Encoder
from bufr.encoders.netcdf import Encoder as netcdfEncoder
from wxflow import Logger

INFO = 'INFO'
DEBUG = 'DEBUG'

YAML_NORMAL = False  # current as normal need remap for path2/1bama
INVALID = 1000.0

# Cosmic background temperature. Taken from Mather,J.C. et. al., 1999, "Calibrator Design for the COBE
# Far-Infrared Absolute Spectrophotometer (FIRAS)"Astrophysical Journal, vol 512, pp 511-520
COSMIC_BACKGROUND_TEMP = 2.7253

nc_dir = './aux'

# Initialize Logger
# Get log level from the environment variable, default to 'INFO it not set
log_level = os.getenv('LOG_LEVEL', INFO)
logger = Logger(os.path.basename(__file__), level=log_level, colored_log=True)


def logging(comm, message, level=INFO):
    """
    Logs a message to the console or log file, based on the specified logging level.

    This function ensures that logging is only performed by the root process (`rank 0`)
    in a distributed computing environment. The function maps the logging level to
    appropriate logger methods and defaults to the 'INFO' level if an invalid level is provided.

    Parameters:
        comm: object
            The communicator object, typically from a distributed computing framework
            (e.g., MPI). It must have a `rank()` method to determine the process rank.
        level: str
            The logging level as a string. Supported levels are:
                - 'DEBUG'
                - 'INFO'
                - 'WARNING'
                - 'ERROR'
                - 'CRITICAL'
            If an invalid level is provided, a warning will be logged, and the level
            will default to 'INFO'.
        message: str
            The message to be logged.

    Behavior:
        - Logs messages only on the root process (`comm.rank() == 0`).
        - Maps the provided logging level to a method of the logger object.
        - Defaults to 'INFO' and logs a warning if an invalid logging level is given.
        - Supports standard logging levels for granular control over log verbosity.

    Example:
        >>> logging(comm, 'DEBUG', 'This is a debug message.')
        >>> logging(comm, 'ERROR', 'An error occurred!')

    Notes:
        - Ensure that a global `logger` object is configured before using this function.
        - The `comm` object should conform to MPI-like conventions (e.g., `rank()` method).
    """

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


class ACCoeff:
    def __init__(self, ac_dir, sat_id='n19'):
        file_name = os.path.join(ac_dir, 'amsua_' + sat_id + '.ACCoeff.nc')
        nc_file = nc.Dataset(file_name)
        self.n_fovs = len(nc_file.dimensions['n_FOVs'])
        self.n_channels = len(nc_file.dimensions['n_Channels'])
        self.a_earth = nc_file.variables['A_earth'][:]
        self.a_platform = nc_file.variables['A_platform'][:]
        self.a_space = nc_file.variables['A_space'][:]
        self.a_ep = self.a_earth + self.a_platform
        self.a_sp = self.a_space * COSMIC_BACKGROUND_TEMP


def _remove_ant_corr(i, ac, ifov, t):
    # AC:             Structure containing the antenna correction coefficients for the sensor of interest.
    # iFOV:           The FOV index for a scanline of the sensor of interest.
    # T:              On input, this argument contains the brightness

    t = ac.a_ep[i, ifov] * t + ac.a_sp[i, ifov]
    t[(ifov < 1) | (ifov > ac.n_fovs)] = [INVALID]
    return t


def _apply_ant_corr(i, ac, ifov, t):
    # t:              on input, this argument contains the antenna temperatures for the sensor channels.
    t = (t - ac.a_sp[i, ifov]) / ac.a_ep[i, ifov]
    t[(ifov < 1) | (ifov > ac.n_fovs)] = [INVALID]
    return t


def _make_description(yaml_path):
    description = bufr.encoders.Description(yaml_path)
    return description


def _apply_corr(comm, sat_id, ta, ifov):
    ac = ACCoeff(nc_dir, sat_id=sat_id)
    if sat_id not in ['n15', 'n16']:
        # Convert antenna temperature to brightness temperature
        ifov = ifov.astype(int) - 1
        for i in range(ta.shape[1]):
            logging(comm, f'inside loop for allpy ta to tb: i = {i}', level=DEBUG)
            x = ta[:, i]
            if YAML_NORMAL:
                x = _apply_ant_corr(i, ac, ifov, x)
            else:
                x = _remove_ant_corr(i, ac, ifov, x)
            x[x >= INVALID] = INVALID
            ta[:, i] = x
    return ta


def _re_map_variable(comm, container):
    # read_bufrtovs.f90
    # antcorr_application.f90
    # search the keyword “ta2tb” for details
    sat_ids = container.all_sub_categories()
    for sat_id in sat_ids:
        logging(comm, f'Converting for {sat_id[0]}, ...')
        ta = container.get('variables/brightnessTemperature', sat_id)
        if ta.shape[0]:
            ifov = container.get('variables/fieldOfViewNumber', sat_id)
            tb = _apply_corr(comm, sat_id[0], ta, ifov)
            container.replace('variables/brightnessTemperature', tb, sat_id)


def _make_obs(comm, input_path, yaml_path):
    cache = bufr.DataCache.has(input_path, yaml_path)
    if cache:
        logging(comm, f'The cache existed get data container from it')
        container = bufr.DataCache.get(input_path, yaml_path)
    else:
        # If cacache does not exist, get data into cache
        # Get data info container first
        logging(comm, f'The cache is not existed')
        container = bufr.Parser(input_path, yaml_path).parse()
    return cache, container


def _mark_one_data(comm, cache, input_path, yaml_path, category, container=None):
    if cache:
        logging(comm, f'The cache existed get data container from it')
        bufr.DataCache.mark_finished(input_path, yaml_path, [category])
    else:
        logging(comm, f'add original container list into a cache = {container.list()}')
        bufr.DataCache.add(input_path, yaml_path, container.all_sub_categories(), container)
        bufr.DataCache.mark_finished(input_path, yaml_path, [category])


def create_obs_group(input_path1, input_path2, yaml_1b, yaml_es, category, env):
    comm = bufr.mpi.Comm(env["comm_name"])

    logging(comm, f'Imput_path: {input_path1}, {input_path2}, and category: {category}')
    logging(comm, f'Entering function to create obs group for {category} with yaml path {yaml_es} and {yaml_1b}')
    cache_1, container_1 = _make_obs(comm, input_path1, yaml_es)
    cache_2, container_2 = _make_obs(comm, input_path2, yaml_1b)

    container = container_1

    if not cache_2:
        logging(comm, f'No chahe, remap and append it')
        _re_map_variable(comm, container_2)
        container.append(container_2)
        logging(comm, 'Container append done')

    data = Encoder(_make_description(yaml_es)).encode(container)[(category,)]
    _mark_one_data(comm, cache_1, input_path1, yaml_es, category, container=container_1)
    _mark_one_data(comm, cache_2, input_path2, yaml_1b, category, container=container_2)
    return data


def create_obs_file(input_path1, input_path2, yaml_1b, yaml_es, output_path):

    comm = bufr.mpi.Comm("world")

    logging(comm, f'Imput_path: {input_path1} and {input_path2}')
    logging(comm, f'Entering function with yaml path {yaml_es} and {yaml_1b}')
    cache_1, container_1 = _make_obs(comm, input_path1, yaml_es)
    cache_2, container_2 = _make_obs(comm, input_path2, yaml_1b)

    _re_map_variable(comm, container_2)

    container = container_1
    container.append(container_2)
    logging(comm, 'Container append done')

    description = _make_description(yaml_es)

    # Encode the data
    if comm.rank() == 0:
        netcdfEncoder(description).encode(container, output_path)

    logging(comm, f'Return the encoded data')


if __name__ == '__main__':
    start_time = time.time()
    bufr.mpi.App(sys.argv)
    comm = bufr.mpi.Comm("world")

    # Required input arguments as positional arguments
    parser = argparse.ArgumentParser(description="Convert BUFR to NetCDF using a mapping file.")
    parser.add_argument('input_path1', type=str, help='Input BUFR file for 1b')
    parser.add_argument('input_path2', type=str, help='Input BUFR file for es')
    parser.add_argument('yaml_1b', type=str, help='BUFR2IODA Mapping File for 1b')
    parser.add_argument('yaml_es', type=str, help='BUFR2IODA Mapping File for es')
    parser.add_argument('output', type=str, help='Output NetCDF file')

    args = parser.parse_args()
    input_path1 = args.input_path1
    input_path2 = args.input_path2
    yaml_1b = args.yaml_1b
    yaml_es = args.yaml_es
    output = args.output

    create_obs_file(input_path1, input_path2, yaml_1b, yaml_es, output)

    end_time = time.time()
    running_time = end_time - start_time
    logging(comm, f'Total running time: {running_time}')
