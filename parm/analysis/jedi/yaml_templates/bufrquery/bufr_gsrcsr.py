#!/usr/bin/env python3
import sys
import os
import argparse
import time
import bufr
from bufr.bufr_python.encoders import *
from bufr.encoders.netcdf import Encoder as netcdfEncoder 
from bufr.obs_builder import add_dummy_variable
from wxflow import Logger
import numpy as np
from netCDF4 import Dataset

# Initialize Logger
# Get log level from the environment variable, default to 'INFO it not set
log_level = os.getenv('LOG_LEVEL', 'DEBUG')
logger = Logger('BUFR_gsrcsr.py', level=log_level, colored_log=False)

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
        logging(comm, 'INFO', f'updating the netcdf file')
        variables = [
            {
                'name':'MetaData/sensorScanPosition',
                'source': 'variables/sensorScanPosition',
                'units':' ',
                'longName': 'Sensor Scan Position',
            },
            {
                'name':'MetaData/sensorChannelNumber',
                'source': 'variables/sensorChannelNumber',
                'units':'m-1',
                'longName': 'Sensor channel number',
            },
            {
                'name':'MetaData/cloudAmount',
                'source': 'variables/cloudAmount',
                'units':' ',
                'longName': 'Amount of cloud coverage in layer',
            },
            {
                'name':'ClearSkyStdDev/brightnessTemperature',
                'source': 'variables/clearstd',
                'units':'K',
                'longName': 'Brightness Temperature Standard Deviation',
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
def compute_sensor_channel_number(nlocs, nchannels):
    return np.tile(np.arange(7, nchannels + 1, dtype=np.int32), (nlocs,1))

def compute_scan_position(container, category):
    # Extract variables
    satzenang = container.get('variables/sensorZenithAngle', category)
    rounded_values = np.where(satzenang % 1 > 0.5, np.ceil(satzenang), np.floor(satzenang))
    scanpos = rounded_values.astype(np.int32) + 1
   
    return scanpos 
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
        logging(comm, 'INFO', f'satid is {satid}')
        if satid.size == 0:
            logging(comm, 'WARNING', f'category {cat[0]} does not exist in input file')
            dummy_mappings = [
            #    ('sensorCentralWavenumber', 'brightnessTemperature'),
                ('sensorScanPosition','brightnessTemperature'),
                ('sensorChannelNumber','brightnessTemperature'),
                ('cloudAmount', 'brightnessTemperature'),
                ('clearstd', 'brightnessTemperature')
            ]
            for target_var, source_var in dummy_mappings:
                add_dummy_variable(container, target_var, cat, source_var)
            continue
        else:
            # Add channel number and wavenumber
            sccf_paths = container.get_paths('variables/sensorCentralFrequency', cat)
            nlocs = satid.shape[0] # Number of locations
            nchannels = 16 # Number of channels
            # Add Ten Channels from 7 to 16
            sensor_channel_number = compute_sensor_channel_number(nlocs, nchannels)
            logging(comm, 'INFO', f'Adding derived variables: sensorChannelNumber for {nlocs} locations')
            container.add('variables/sensorChannelNumber', 
                          sensor_channel_number, 
                          sccf_paths, 
                          cat)
            
            # Compute sensorScanPosition from container variables
            location_path = container.get_paths('variables/longitude', cat)
            scanpos = compute_scan_position(container, cat)
            container.add('variables/sensorScanPosition',
                          scanpos,
                          location_path,
                          category=cat)
            # Compute Cloud Amount
            cloudfree = container.get('variables/cloudFree',cat)
            cloudAmount = np.where(
                  cloudfree == cloudfree.fill_value,
                  cloudfree.fill_value,
                  100. - cloudfree).astype(np.float32)
            container.add('variables/cloudAmount',
                          cloudAmount,
                          location_path,
                          category=cat)
            # add the standardDeviation
            twod_path = container.get_paths('variables/brightnessTemperature', cat)
            clearstd = container.get('variables/brightnessTemperatureStandardDeviation',cat)
            value = clearstd
            container.add('variables/clearstd',
                           value,
                           twod_path,
                           category=cat)
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
    mapping = args.mapping
    infile = args.input
    output = args.output

    create_obs_file(infile, mapping, output)

    end_time = time.time()
    running_time = end_time - start_time
    logging(comm, 'INFO', f'Total running time: {running_time}')
