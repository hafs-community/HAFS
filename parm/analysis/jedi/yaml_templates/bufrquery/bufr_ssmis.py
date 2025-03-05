#!/usr/bin/env python3
import sys
import os
import argparse
import time
import datetime
import numpy as np
import bufr
from pyioda.ioda.Engines.Bufr import Encoder as iodaEncoder 
from bufr.encoders.netcdf import Encoder as netcdfEncoder 
from wxflow import Logger
import math

# Initialize Logger
# Get log level from the environment variable, default to 'INFO it not set
log_level = os.getenv('LOG_LEVEL', 'INFO')
logger = Logger('BUFR_ssmis.py', level=log_level, colored_log=False)


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
        # Define the variables to be added in a list of dictionaries
        variables = [
            {
                'name': 'MetaData/solarZenithAngle',
                'source': 'variables/solarZenithAngle',
                'units': 'degree',
                'longName': 'Solar Zenith Angle',
            },
            {
                'name': 'MetaData/solarAzimuthAngle',
                'source': 'variables/solarAzimuthAngle',
                'units': 'degree',
                'longName': 'Solar Azimuth Angle',
            },
            {
                'name': 'MetaData/sensorZenithAngle',
                'source': 'variables/sensorZenithAngle',
                'units': 'degree',
                'longName': 'Sensor Zenith Angle',
            },
            {
                'name': 'MetaData/sensorAzimuthAngle',
                'source': 'variables/sensorAzimuthAngle',
                'units': 'degree',
                'longName': 'Sensor Azimuth Angle',
            },
            {
                'name': 'MetaData/satelliteAscendDescendInfo',
                'source': 'variables/satelliteAscendDescendInfo',
                'units': '1',
                'longName': 'Satellite Ascending/Descending Node Infomration (Ascend:1; Descend:-1)',
            },
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


def calculate_solar_zen_azi_angles(timestamp, lat, lon):
    # This is calculate the solar zenith and azimuthal angle based on the time of the day and lat/lon info
    # Constants
    dt = datetime.datetime.utcfromtimestamp(timestamp)
    # Compute day of the year (1-366)
    day = dt.timetuple().tm_yday
    # Compute fractional UTC time
    time_hours = round(dt.hour + dt.minute / 60.0 + dt.second / 3600.0, 6)

    deg2rad = math.pi / 180
    rad2deg = 180 / math.pi
    r60inv = 1 / 60  # To convert minutes to degrees
    one = 1.0
    zero = 0.0
    
    # Data arrays for days of year, equation of time, and declination
    nday = [
        1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 81, 86, 91, 96, 
        101, 106, 111, 116, 121, 126, 131, 136, 141, 146, 151, 156, 161, 166, 171, 176, 
        181, 186, 191, 196, 201, 206, 211, 216, 221, 226, 231, 236, 241, 246, 251, 256, 
        261, 266, 271, 276, 281, 286, 291, 296, 301, 306, 311, 316, 321, 326, 331, 336, 
        341, 346, 351, 356, 361, 366
    ]
    
    eqt = [
        -3.23, -5.49, -7.60, -9.48, -11.09, -12.39, -13.34, -13.95, -14.23, -14.19, 
        -13.85, -13.22, -12.35, -11.26, -10.01, -8.64, -7.18, -5.67, -4.16, -2.69, 
        -1.29, -0.02, 1.10, 2.05, 2.80, 3.33, 3.63, 3.68, 3.49, 3.09, 2.48, 1.71, 
        0.79, -0.24, -1.33, -2.41, -3.45, -4.39, -5.20, -5.84, -6.28, -6.49, -6.44, 
        -6.15, -5.60, -4.82, -3.81, -2.60, -1.19, 0.36, 2.03, 3.76, 5.54, 7.31, 9.04, 
        10.69, 12.20, 13.53, 14.65, 15.52, 16.12, 16.41, 16.36, 15.95, 15.19, 14.09, 
        12.67, 10.93, 8.93, 6.70, 4.32, 1.86, -0.62, -3.23
    ]
    
    dec = [
        -23.06, -22.57, -21.91, -21.06, -20.05, -18.88, -17.57, -16.13, -14.57, -12.91, 
        -11.16, -9.34, -7.46, -5.54, -3.59, -1.62, 0.36, 2.33, 4.28, 6.19, 8.06, 9.88, 
        11.62, 13.29, 14.87, 16.34, 17.70, 18.94, 20.04, 21.00, 21.81, 22.47, 22.95, 
        23.28, 23.43, 23.40, 23.21, 22.85, 22.32, 21.63, 20.79, 19.80, 18.67, 17.42, 
        16.05, 14.57, 13.00, 11.33, 9.60, 7.80, 5.95, 4.06, 2.13, 0.19, -1.75, -3.69, 
        -5.62, -7.51, -9.36, -11.16, -12.88, -14.53, -16.07, -17.50, -18.81, -19.98, 
        -20.99, -21.85, -22.52, -23.02, -23.33, -23.44, -23.35, -23.06
    ]

    # Fractional day
    tt = (day + time_hours / 24.0 - 1) % 365.25 + 1

    # Find the corresponding day index
    di = 0
    for i in range(73):
        if nday[i] <= tt <= nday[i + 1]:
            di = i 
            break

    # Initialize x matrix (2x5)
    x = [[one] * 5 for _ in range(2)]  # Equivalent to 2D array `x(1, :)` in Fortran
    # Check the conditions for setting y, y2, and x based on `di`

    if 2 <= di <= 71:  # Python di 2..71 corresponds to Fortran di 3..72
        y = [eqt[di - 2], eqt[di - 1], eqt[di], eqt[di + 1], eqt[di + 2]]
        y2 = [dec[di - 2], dec[di - 1], dec[di], dec[di + 1], dec[di + 2]]
        x[1] = [nday[di - 2]**3, nday[di - 1]**3, nday[di]**3, nday[di + 1]**3, nday[di + 2]**3]
    
    elif di == 1:  # Python di==1 corresponds to Fortran di==2
        y = [eqt[72]] + eqt[di - 1:di + 3]    # eqt[0:4] gives 4 elements → total 5
        y2 = [dec[72]] + dec[di - 1:di + 3]
        x[1][0] = nday[72]**3
        x[1][1:5] = [(365 + nday[i])**3 for i in range(di - 1, di + 3)]  # i from 0 to 3
    
    elif di == 0:  # Python di==0 corresponds to Fortran di==1
        y = eqt[71:73] + eqt[di:di + 3]      # eqt[71:73] → 2 elements, eqt[0:3] → 3 elements
        y2 = dec[71:73] + dec[di:di + 3]
        x[1][:2] = [nday[71]**3, nday[72]**3]
        x[1][2:5] = [(365 + nday[i])**3 for i in range(di, di + 3)]  # i from 0 to 2
    
    elif di == 72:  # Python di==72 corresponds to Fortran di==73
        # Fortran: y(1:4) = eqt(di-2:di+1) and y(5) = eqt(2)
        # In Python: for Fortran eqt(71:74) we use eqt[70:74]
        y = eqt[di - 2:di + 1] + [eqt[1]]  # di==72: eqt[70:73] gives 3 elements; then add eqt[1]
        y2 = dec[di - 2:di + 1] + [dec[1]]
        x[1][:4] = [nday[di - 2]**3, nday[di - 1]**3, nday[di]**3, nday[di + 1]**3]
        x[1][4] = (365 + nday[1])**3      # nday(2) in Fortran
    
    elif di == 73:  # Python di==73 corresponds to Fortran di==74
        # Fortran: y(1:3) = eqt(di-2:di) and y(4:5) = eqt(2:3)
        y = eqt[di - 2:di + 1] + eqt[1:3]   # di==73: eqt[71:74] gives 3 elements; eqt[1:3] gives 2 elements
        y2 = dec[di - 2:di + 1] + dec[1:3]
        x[1][:3] = [nday[di - 2]**3, nday[di - 1]**3, nday[di]**3]
        x[1][3:5] = [(365 + nday[i])**3 for i in range(1, 3)]

    # Transpose x to get Tx (5x2 matrix)
    Tx = [[None] * 2 for _ in range(5)]
    for i in range(5):
        Tx[i][0] = x[0][i]
        Tx[i][1] = x[1][i]

    # Compute xTx = MATMUL(x,Tx)  -> result is 2 x 2 matrix.
    xTx = [[None] * 2 for _ in range(2)]
    xTx[0][0] = x[0][0]*Tx[0][0] + x[0][1]*Tx[1][0] + x[0][2]*Tx[2][0] + x[0][3]*Tx[3][0] + x[0][4]*Tx[4][0]
    xTx[0][1] = x[0][0]*Tx[0][1] + x[0][1]*Tx[1][1] + x[0][2]*Tx[2][1] + x[0][3]*Tx[3][1] + x[0][4]*Tx[4][1]
    xTx[1][0] = x[1][0]*Tx[0][0] + x[1][1]*Tx[1][0] + x[1][2]*Tx[2][0] + x[1][3]*Tx[3][0] + x[1][4]*Tx[4][0]
    xTx[1][1] = x[1][0]*Tx[0][1] + x[1][1]*Tx[1][1] + x[1][2]*Tx[2][1] + x[1][3]*Tx[3][1] + x[1][4]*Tx[4][1]

    # Compute determinant and inverse matrix a (2x2)
    det = xTx[0][0]*xTx[1][1] - xTx[0][1]*xTx[1][0]
    a_mat = [[None] * 2 for _ in range(2)]
    a_mat[0][0] = xTx[1][1] / det
    a_mat[0][1] = -xTx[0][1] / det
    a_mat[1][0] = -xTx[1][0] / det
    a_mat[1][1] = xTx[0][0] / det

    # Compute aTx = MATMUL(Tx, a)  -> aTx is 5 x 2.
    aTx = [[None] * 2 for _ in range(5)]
    for i in range(5):
        aTx[i][0] = Tx[i][0]*a_mat[0][0] + Tx[i][1]*a_mat[1][0]
        aTx[i][1] = Tx[i][0]*a_mat[0][1] + Tx[i][1]*a_mat[1][1]

    # Compute beta = MATMUL(y, aTx)  -> beta is a 2-element vector.
    beta = [None, None]
    beta[0] = (y[0]*aTx[0][0] + y[1]*aTx[1][0] +
               y[2]*aTx[2][0] + y[3]*aTx[3][0] +
               y[4]*aTx[4][0])
    beta[1] = (y[0]*aTx[0][1] + y[1]*aTx[1][1] +
               y[2]*aTx[2][1] + y[3]*aTx[3][1] +
               y[4]*aTx[4][1])

    # Compute beta2 = MATMUL(y2, aTx)
    beta2 = [None, None]
    beta2[0] = (y2[0]*aTx[0][0] + y2[1]*aTx[1][0] +
                y2[2]*aTx[2][0] + y2[3]*aTx[3][0] +
                y2[4]*aTx[4][0])
    beta2[1] = (y2[0]*aTx[0][1] + y2[1]*aTx[1][1] +
                y2[2]*aTx[2][1] + y2[3]*aTx[3][1] +
                y2[4]*aTx[4][1])

    # eqtime and decang values
    eqtime = (beta[0] + beta[1] * tt**3) * r60inv
    decang = beta2[0] + beta2[1] * tt**3
    latsun = decang

    ut = time_hours
    noon = 12.0 - lon / 15.0
    lonsun = -15.0 * (ut - 12.0 + eqtime)

    t0 = (90.0 - lat) * deg2rad
    t1 = (90.0 - latsun) * deg2rad

    p0 = lon * deg2rad
    p1 = lonsun * deg2rad

    zz = math.cos(t0) * math.cos(t1) + math.sin(t0) * math.sin(t1) * math.cos(p1 - p0)
    xx = math.sin(t1) * math.sin(p1 - p0)
    yy = math.sin(t0) * math.cos(t1) - math.cos(t0) * math.sin(t1) * math.cos(p1 - p0)

    solar_zenith = 90.0 - math.acos(zz) * rad2deg
    solar_azimuth = math.atan2(xx, yy) * rad2deg
    if solar_azimuth < 0:
        solar_azimuth += 360.0

    solar_zenith=90. - solar_zenith #This is outside zensun in read_ssmis.f90 to make sure solar zenith angles are between 0 and 180
    return solar_zenith, solar_azimuth


def compute_solar_positions(container, category):
    """
    Extracts timestamp, latitude, and longitude from the container,
    converts the timestamp to day-of-year and hour (in UTC), then computes
    sun zenith and sun azimuth for each observation using zensun.
    """
    # Extract variables from the container using the defined keys.
    timestamps = container.get('variables/timestamp',category)  # seconds since epoch
    lat_arr = container.get('variables/latitude',category)
    lon_arr = container.get('variables/longitude',category)
    
    nobs = len(lat_arr)
    solar_zenith = np.empty(nobs, dtype=np.float32)
    solar_azimuth = np.empty(nobs, dtype=np.float32)
    
    for i in range(nobs):
        # Call zensun for this observation
        solar_zenith[i], solar_azimuth[i] = calculate_solar_zen_azi_angles(timestamps[i], lat_arr[i], lon_arr[i])
    
    return solar_zenith, solar_azimuth


def determine_satellite_ascend_descent_mode(container, category):
    """
    Determines if the satellite is in ascending or descending mode based on latitude changes.
    
    :param ephemeris_data: List of dictionaries with 'latitude' values in order of time.
    :return: "Ascending" if latitude increases, "Descending" if latitude decreases.
    """

    # Get data from container
    first_lat = container.get('variables/latitude1',category)
    second_lat = container.get('variables/latitude2',category)
    fovn = container.get('variables/fieldOfViewNumber',category)

    if isinstance(fovn, np.ma.MaskedArray):
        print("fovn is a masked array")

    # Determine ascending/descending mode
    # Compare latitude between the first and second records
    nodeinfo = np.where(second_lat > first_lat, 1, -1).astype(np.int32)

    print('emily checking fovn fill_vaue type = ', fovn.fill_value.dtype)
    print('emily checking fovn type           = ', fovn.dtype)
    print('emily checking first_lat type      = ', first_lat.dtype)
    print('emily checking second_lat type     = ', second_lat.dtype)
    print('emily checking nodeinfo type       = ', nodeinfo.dtype)

    return nodeinfo


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

        # Apply spatial averaging
        nodeinfo = determine_satellite_ascend_descent_mode(container, cat)

        # Compute solar angle fields from container variables
        paths = container.get_paths('variables/fieldOfViewNumber', cat)
        nodeinfo = determine_satellite_ascend_descent_mode(container, cat)
        container.add('variables/satelliteAscendDescendInfo',
                      nodeinfo,
                      paths,
                      category=cat)

        # Compute solar angle fields from container variables
        paths = container.get_paths('variables/latitude', cat)
        solar_zenith, solar_azimuth = compute_solar_positions(container, cat)
        container.add('variables/solarZenithAngle',
                      solar_zenith,
                      paths,
                      category=cat)
        container.add('variables/solarAzimuthAngle',
                      solar_azimuth,
                      paths,
                      category=cat)

        # Set sensorZenithAngle to fixed 53.0 for SSMIS
        sensor_zenith = np.full_like(solar_zenith, 53.0)
        container.add('variables/sensorZenithAngle',
                      sensor_zenith,
                      paths,
                      category=cat)

        # Set sensorAzimuthAngle to fixed missing -1 for SSMIS
        sensor_azimuth = np.full_like(solar_azimuth, -1)
        container.add('variables/sensorAzimuthAngle',
                      sensor_azimuth,
                      paths,
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
