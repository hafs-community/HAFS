#!/usr/bin/env python3
import sys
import os
import argparse
import time
import calendar
import bufr
from bufr.bufr_python.encoders import *
from bufr.encoders.netcdf import Encoder as netcdfEncoder 
from wxflow import Logger
import numpy as np
import numpy.ma as ma

# Initialize Logger
# Get log level from the environment variable, default to 'INFO it not set
log_level = os.getenv('LOG_LEVEL', 'INFO')
logger = Logger('BUFR_atms.py', level=log_level, colored_log=False)

def logging(comm, level, message):
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

def _make_description(mapping_path, update=False):

    description = bufr.encoders.Description(mapping_path)

    if update:
        description.add_variable(name='MetaData/timeOffset',
                                 source='variables/timeOffset',
                                 units='s',
                                 longName='Observation Time Minus Reference Time')

    return description

def ComputeTimeOffset(obtime, cycle_time):
    """
    Compute observation time minus cycle time.

    Parameters
    ----------
    obtime : array-like
        Observation timestamps in seconds since Unix epoch.
    cycle_time : str
        Data cycle time in YYYYMMDDHH format.

    Returns
    -------
    time_diff : numpy.ma.MaskedArray
        Observation time minus cycle time, in seconds.
    """

    cycleTimeSinceEpoch = np.int64(
        calendar.timegm(time.strptime(str(int(cycle_time)), '%Y%m%d%H'))
    )

    # Preserve the mask from the observation-time field.
    time_diff = ma.array(obtime, copy=True) - cycleTimeSinceEpoch

    return time_diff.astype(np.float32)


def _get_cycle_time(env=None, cycle_time=None):
    """Get cycle time from an explicit argument, env dictionary, or CDATE."""

    if cycle_time is not None:
        return str(cycle_time)

    if env is not None:
        for key in ('cycle_time', 'cycleTime', 'CDATE'):
            if key in env and env[key] is not None:
                return str(env[key])

    cdate = os.getenv('CDATE')
    if cdate:
        return cdate

    raise ValueError(
        "Cycle time is required to compute MetaData/timeOffset. "
        "Provide cycle_time in YYYYMMDDHH format or set CDATE."
    )


def _make_obs(comm, input_path, mapping_path, cycle_time):

    # Get container from mapping file first
    logging(comm, 'INFO', 'Get container from bufr')
    container = bufr.Parser(input_path, mapping_path).parse(comm)

    logging(comm, 'DEBUG', f'container list (original): {container.list()}')

    logging(comm, 'DEBUG', f'all_sub_categories =  {container.all_sub_categories()}')
    logging(comm, 'DEBUG', f'category map =  {container.get_category_map()}')

    # Add new/derived data into container.
    # ATMS fields are stored by sub-category, not in __MAIN__, so
    # timeOffset must be computed separately for each category.
    for cat in container.all_sub_categories():

        logging(comm, 'DEBUG', f'category = {cat}')

        satid = container.get('variables/satelliteId', cat)
        if satid.size == 0:
            logging(comm, 'WARNING', f'category {cat[0]} does not exist in input file')
            continue

        logging(comm, 'DEBUG', f'Add MetaData/timeOffset for category {cat}')
        paths = container.get_paths('variables/timestamp', cat)
        obtime = container.get('variables/timestamp', cat)
        time_diff = ComputeTimeOffset(obtime, cycle_time)

        # Temporary diagnostics: use print so these are visible even when LOG_LEVEL=INFO.
        if comm.rank() == 0:
            print(f'TIMEOFFSET DEBUG: cat = {cat}')
            print(f'TIMEOFFSET DEBUG: paths = {paths}')
            print(f'TIMEOFFSET DEBUG: obtime shape = {obtime.shape}')
            print(f'TIMEOFFSET DEBUG: time_diff shape = {time_diff.shape}')
            print(f'TIMEOFFSET DEBUG: first obs times = {obtime[:5]}')
            print(f'TIMEOFFSET DEBUG: first time offsets = {time_diff[:5]}')

        # ATMS data are stored under sub-categories, so explicitly add
        # timeOffset to the same category instead of the default __MAIN__.
        container.add('variables/timeOffset', time_diff, paths, cat)

    # Check
    logging(comm, 'DEBUG', f'container list (updated): {container.list()}')
    logging(comm, 'DEBUG', f'all_sub_categories {container.all_sub_categories()}')

    return container

def create_obs_group(input_path, mapping_path, category, env, cycle_time=None):

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

    cycle_time = _get_cycle_time(env=env, cycle_time=cycle_time)
    container = _make_obs(comm, input_path, mapping_path, cycle_time)

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

def create_obs_file(input_path, mapping_path, output_path, cycle_time):

    comm = bufr.mpi.Comm("world")
    cycle_time = _get_cycle_time(cycle_time=cycle_time)
    container = _make_obs(comm, input_path, mapping_path, cycle_time)
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
    parser.add_argument('cycle_time', type=str, help='Cycle time in YYYYMMDDHH format')

    args = parser.parse_args()
    mapping = args.mapping
    infile = args.input
    output = args.output
    cycle_time = args.cycle_time

    create_obs_file(infile, mapping, output, cycle_time)

    end_time = time.time()
    running_time = end_time - start_time
    logging(comm, 'INFO', f'Total running time: {running_time}')
