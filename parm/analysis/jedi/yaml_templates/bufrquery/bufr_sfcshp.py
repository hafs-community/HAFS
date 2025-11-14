#!/usr/bin/env python3
import os
import sys
import bufr
import argparse
import copy
import math
import calendar
import time
from datetime import datetime
from pyioda.ioda.Engines.Bufr import Encoder as iodaEncoder
from bufr.encoders.netcdf import Encoder as netcdfEncoder
from wxflow import Logger
import numpy as np
import numpy.ma as ma

# Initialize Logger
# Get log level from the environment variable, default to 'INFO it not set
log_level = os.getenv('LOG_LEVEL', 'INFO')
logger = Logger('bufr_sfcshp.py', level=log_level, colored_log=False)


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


def _compute_datetime(cycleTimeSinceEpoch, dhr):
    """
    Compute dateTime using the cycleTimeSinceEpoch and Cycle Time
        minus Cycle Time

    Parameters:
        cycleTimeSinceEpoch: Time of cycle in Epoch Time
        dhr: Observation Time Minus Cycle Time

    Returns:
        Masked array of dateTime values
    """

    int64_fill_value = np.int64(0)

    dateTime = np.zeros(dhr.shape, dtype=np.int64)
    for i in range(len(dateTime)):
        if ma.is_masked(dhr[i]):
            continue
        else:
            dateTime[i] = np.int64(dhr[i]*3600) + cycleTimeSinceEpoch

    dateTime = ma.array(dateTime)
    dateTime = ma.masked_values(dateTime, int64_fill_value)

    return dateTime


def _make_description(mapping_path, cycle_time, update=False):
    description = bufr.encoders.Description(mapping_path)

    ReferenceTime = np.int64(calendar.timegm(time.strptime(str(int(cycle_time)), '%Y%m%d%H')))

    if update:
        # Define the variables to be added in a list of dictionaries
        variables = [
            {
                'name': 'MetaData/sequenceNumber',
                'source': 'variables/sequenceNumber',
                'units': '1',
                'longName': 'Sequence Number (Obs Subtype)',
            },
            {
                'name': 'ObsSubType/stationPressure',
                'source': 'obsSubType',
                'units': '1',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/airTemperature',
                'source': 'obsSubType',
                'units': '1',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/virtualTemperature',
                'source': 'obsSubType',
                'units': '1',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/specificHumidity',
                'source': 'obsSubType',
                'units': '1',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/windEastward',
                'source': 'obsSubType',
                'units': '1',
                'longName': 'Observation SubType',
            },
            {
                'name': 'ObsSubType/windNorthward',
                'source': 'obsSubType',
                'units': '1',
                'longName': 'Observation SubType',
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

        # description.add_global(name='datetimeReference', value=str(ReferenceTime))

    return description


def _make_obs(comm, input_path, mapping_path, cycle_time):
    """
    Create the ioda adpupa prepbufr observations:
    - reads values
    - adds sequenceNum

    Parameters
    ----------
    comm: object
            The communicator object (e.g., MPI)
    input_path: str
            The input bufr file
    mapping_path: str
            The input bufr2ioda mapping file
    cycle_time: str
            The cycle in YYYYMMDDHH format
    """

    # Get container from mapping file first
    logging(comm, 'INFO', 'Get container from bufr')
    container = bufr.Parser(input_path, mapping_path).parse(comm)

    logging(comm, 'DEBUG', f'container list (original): {container.list()}')
    logging(comm, 'DEBUG', f'prepbufrDataLevelCategory')
    cat = container.get('variables/prepbufrDataLevelCategory')

    logging(comm, 'DEBUG', f'Change longitude range from [0,360] to [-180,180]')
    lon = container.get('variables/longitude')
    lon_paths = container.get_paths('variables/longitude')
    #lon[lon > 180] -= 360
    #lon = ma.round(lon, decimals=2)
    logging(comm, 'DEBUG', f'longitude max and min are {lon.max()}, {lon.min()}')

    logging(comm, 'DEBUG', f'Do DateTime calculation')
    otmct = container.get('variables/timeOffset')
    otmct_paths = container.get_paths('variables/timeOffset')
    otmct2 = np.array(otmct)
    cycleTimeSinceEpoch = np.int64(calendar.timegm(time.strptime(str(int(cycle_time)), '%Y%m%d%H')))
    dateTime = _compute_datetime(cycleTimeSinceEpoch, otmct2)
    min_dateTime_ge_zero = min(x for x in dateTime if x > -1)
    logging(comm, 'DEBUG', f'dateTime min/max = {min_dateTime_ge_zero} {dateTime.max()}')

    logging(comm, 'DEBUG', f'Do ObsSubType and sequenceNumber (Obs SubType) calculation')
    typ = container.get('observationType')
    typ_paths = container.get_paths('observationType')
    t29 = container.get('observationSubTypeNum')
    t29_paths = container.get_paths('observationSubTypeNum')
    obsSubType = _compute_obssubtype(typ, t29)
    logging(comm, 'DEBUG',f' obsSubType min/max =  {obsSubType.min()} {obsSubType.max()}')

    logging(comm, 'DEBUG', f'Do tsen and tv calculation')
    tpc = container.get('temperatureEventCode')
    tob = container.get('airTemperature')
    tsen = np.full(tob.shape[0], tob.fill_value)
    tsen = np.where(((tpc >=1) & (tpc < 8)), tob, tsen)
    tvo = np.full(tob.shape[0], tob.fill_value)
    tvo = np.where((tpc == 8), tob, tvo)

    logging(comm, 'DEBUG', f'Do tsen and tv QM calculations')
    tobqm = container.get('airTemperatureQualityMarker')
    tsenqm = np.full(tobqm.shape[0], tobqm.fill_value)
    tsenqm = np.where(((tpc >= 1) & (tpc < 8)), tobqm, tsenqm)
    tvoqm = np.full(tobqm.shape[0], tobqm.fill_value)
    tvoqm = np.where((tpc == 8), tobqm, tvoqm)

    logging(comm, 'DEBUG', f'Do tsen and tv ObsError calculations')
    toboe = container.get('airTemperatureError')
    tsenoe = np.full(toboe.shape[0], toboe.fill_value)
    tsenoe = np.where(((tpc >= 1) & (tpc < 8)), toboe, tsenoe)
    tvooe = np.full(toboe.shape[0], toboe.fill_value)
    tvooe = np.where((tpc == 8), toboe, tvooe)

    logging(comm, 'DEBUG', f'Update variables in container')
    container.replace('longitude', lon)
    container.replace('timestamp', dateTime)
    container.replace('airTemperature', tsen)
    container.replace('airTemperatureQualityMarker', tsenqm)
    container.replace('airTemperatureError', tsenoe)
    container.replace('virtualTemperature', tvo)
    container.replace('virtualTemperatureQualityMarker', tvoqm)
    container.replace('virtualTemperatureError', tvooe)

    logging(comm, 'DEBUG', f'Add variables to container')
    container.add('sequenceNumber', obsSubType, typ_paths)
    container.add('obsSubType', obsSubType, typ_paths)

    # Check
    logging(comm, 'DEBUG', f'container list (updated): {container.list()}')

    return container


def create_obs_group(input_path, mapping_path, cycle_time, env):

    comm = bufr.mpi.Comm(env["comm_name"])

    logging(comm, 'INFO', f'Make description and make obs')

    container = _make_obs(comm, input_path, mapping_path, cycle_time)
    description = _make_description(mapping_path, cycle_time, update=True)

    # Gather data from all tasks into all tasks. Each task will have the complete record
    logging(comm, 'INFO', f'Gather data from all tasks into all tasks')
    container.all_gather(comm)

    logging(comm, 'INFO', f'Encode the data')
    data = next(iter(iodaEncoder(description).encode(container).values()))

    logging(comm, 'INFO', f'Return the encoded data.')

    return data


def create_obs_file(input_path, mapping_path, output_path, cycle_time):

    comm = bufr.mpi.Comm("world")
    container = _make_obs(comm, input_path, mapping_path, cycle_time)
    container.gather(comm)

    description = _make_description(mapping_path, cycle_time, update=True)

    # Encode the data
    if comm.rank() == 0:
        netcdfEncoder(description).encode(container, output_path)

    logging(comm, 'INFO', f'Return the encoded data')

def _compute_obssubtype(typ, t29):
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

if __name__ == '__main__':
    start_time = time.time()

    bufr.mpi.App(sys.argv)
    comm = bufr.mpi.Comm("world")

    # Required input arguments as positional arguments
    parser = argparse.ArgumentParser(description="Convert BUFR to NetCDF using a mapping file.")
    parser.add_argument('input', type=str, help='Input BUFR file')
    parser.add_argument('mapping', type=str, help='BUFR2IODA Mapping File')
    parser.add_argument('output', type=str, help='Output NetCDF file')
    parser.add_argument('cycle_time', type=str, help='cycle time in YYYYMMDDHH format')

    args = parser.parse_args()
    infile = args.input
    mapping = args.mapping
    output = args.output
    cycle_time = args.cycle_time

    create_obs_file(infile, mapping, output, cycle_time)

    end_time = time.time()
    running_time = end_time - start_time
    logging(comm, 'INFO', f'Total running time: {running_time}')
