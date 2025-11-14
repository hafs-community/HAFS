#!/usr/bin/env python3
import sys
import os
import argparse
import time
import numpy as np
import numpy.ma as ma
from datetime import datetime
import bufr
from pyioda.ioda.Engines.Bufr import Encoder as iodaEncoder 
from bufr.encoders.netcdf import Encoder as netcdfEncoder 
from wxflow import Logger

# Initialize Logger
# Get log level from the environment variable, default to 'INFO it not set
log_level = os.getenv('LOG_LEVEL', 'INFO')
logger = Logger('BUFR_tldplr.py', level=log_level, colored_log=False)

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

def _compute_timeoffset(obstime, cycletime):
    """
    Compute timeoffset using the datetime and Cycle Time

    Parameters:
        datetime: Observation Time of observation in Epoch Time
        cycletime: Cycle Time

    Returns:
        Masked array of timeoffset values
    """

    otmct2 = np.array(obstime)
    otmct3 = [datetime.fromtimestamp(ts) for ts in otmct2]
    cycleTimeSinceEpoch = datetime.strptime(str(int(cycletime)), '%Y%m%d%H')
    # calculate time offset
    timediff_objects = [item-cycleTimeSinceEpoch if item is not None else None for item in otmct3]
    timediff_seconds = [td.total_seconds() if td is not None else np.nan for td in timediff_objects]
    # DEBUG Purpose, remove in operation
    mindatetime = datetime.fromtimestamp(obstime.min()).strftime("%Y%m%H")
    maxdatetime = datetime.fromtimestamp(obstime.max()).strftime("%Y%m%H")
    timediff = np.array(timediff_seconds, dtype=np.float32)

    logging(comm,'DEBUG',f'datetime min/max = {mindatetime, maxdatetime}')
    return timediff

def _make_description(mapping_path, update=False):

    description = bufr.encoders.Description(mapping_path)

    if update:
        # Define the variables to be added in a list of dictionaries
        variables = [
            {
                'name':'MetaData/cosAzimuthCosTilt',
                'source': 'variables/cosazm_costilt',
                'units':'',
                'longName': 'cos tilt x cos azimuth',
            },
            {
                'name':'MetaData/sinAzimuthCosTilt',
                'source': 'variables/sinazm_costilt',
                'units':'',
                'longName': 'sin tilt x cos azimuth',
            },
            {
                'name':'MetaData/sinTilt',
                'source': 'variables/sintilt',
                'units':'',
                'longName': 'sin tilt',
            },
            {
                'name':'MetaData/height',
                'source': 'variables/height',
                'units':'m',
                'longName': 'observation height',
            },
            {
                'name': 'ObsError/radialVelocity',
                'source': 'variables/obserr',
                'units': 'm s-1',
                'longName': 'Observation error for radialVelocity',
            },
            {
                'name': 'ObsType/radialVelocity',
                'source': 'variables/obtype',
                'units': '1',
                'longName': 'Observation Type for radialVelocity',
            }
        ]
        # loop through each variable and add it to the description
        for var in variables:
            description.add_variable(
                name = var['name'],
                source = var['source'],
                units = var['units'],
                longName = var['longName']
            )

    return description
def invtllv(ALM,APH,TLMO,CTPH0,STPH0):
    # SUBROUTINE invtllv from read_l2bufr_mod.f90
    RELM = ALM
    SRLM = np.sin(RELM)
    CRLM = np.cos(RELM)
    SPH = np.sin(APH)
    CPH = np.cos(APH)
    CC = CPH*CRLM
    ANUM = CPH*SRLM
    DENOM = CTPH0*CC-STPH0*SPH
    TLM = TLMO+np.arctan2(ANUM,DENOM)
    TPH = np.arcsin(CTPH0*SPH+STPH0*CC)
    return TLM, TPH

def correct_tilt_azm(tilt, azm, stahgt, sta_lat, sta_lon,gaterange):
    # calculate each obs lat/lon based on the station lat/lon, and tilt/azm as well as gate range info.
    # define constant
    erad = np.float64(6371200)  # Earth's radius in meters
    rad_per_meter = 1./erad
    rad2deg = 180/np.pi
    deg2rad = np.pi/180

    # compute obs height: use 4/3rds rule to get elevation of radar beam 
    # (if local temperature, moisture available, then vertical position 
    #  might be estimated with greater accuracy by ray tracing)
    aactual = erad + stahgt
    a43=(4./3.)*aactual
    tiltr = np.radians(tilt) 
    # for TDR data, this range is equal to the gaterange read directly from bufr. 
    # read_radar.f90 line 2785 to line 2814
    thisrange = gaterange

    selev0=np.sin(tiltr)
    celev0=np.cos(tiltr)

    b=thisrange*(thisrange+2.*aactual*selev0)
    c=np.sqrt(aactual*aactual+b)
    ha=b/(aactual+c)
    epsh=(thisrange*thisrange-ha*ha)/(8.*aactual)
    h=ha-epsh
    thishgt = stahgt+h
    thishgt = thishgt.astype(np.float32)
    # add condition to check if thishgt < 0.
    #######################################

    # get elevation angle at obs location
    # get correct tilt angle
    celev=celev0
    selev=selev0
    for i in range(0, len(thisrange)):
        if thisrange[i] >=1:
            celev[i] = a43[i]*celev0[i]/(a43[i]+h[i])
            selev[i]=(thisrange[i]*thisrange[i]+h[i]*h[i]+2.*a43[i]*h[i])/(2*thisrange[i]*(a43[i]+h[i]))
        
    
    corrected_tilt = np.arctan2(selev, celev)*rad2deg

    # get correct azimuth
    gamma = 0.5*thisrange*(celev0+celev)
    rlon0 = deg2rad*sta_lon     #1
    clat0 = np.cos(sta_lat*deg2rad) #1
    slat0 = np.sin(sta_lat*deg2rad) #1

    # get earth lat lon of superob
    thisazimuthr = azm*deg2rad
    rlonloc = rad_per_meter*gamma*np.cos(thisazimuthr)
    rlatloc = rad_per_meter*gamma*np.sin(thisazimuthr)
    rlonglob, rlatglob = invtllv(rlonloc, rlatloc, rlon0, clat0, slat0)
    thislat = rlatglob*rad2deg
    thislon = rlonglob*rad2deg

    clat1 = np.cos(rlatglob)
    caz0 = np.cos(thisazimuthr)
    saz0 = np.sin(thisazimuthr)
    cdlon = np.cos(rlonglob-rlon0)
    sdlon = np.sin(rlonglob-rlon0)
    caz1 = clat0*caz0/clat1
    saz1 = saz0*cdlon - caz0*sdlon*slat0
    corrected_azimuth=np.arctan2(saz1,caz1)*rad2deg

    return corrected_tilt, corrected_azimuth, thislat, thislon, thishgt

def compute_radar_related(tilt, azm):
    # big circle correction
    # calculate
    costilt = np.cos(np.radians(tilt)).astype(np.float32)
    sintilt = np.sin(np.radians(tilt)).astype(np.float32)
    cosazm = np.cos(np.radians(azm)).astype(np.float32)
    sinazm = np.sin(np.radians(azm)).astype(np.float32)

    costilt = ma.array(costilt)
    costilt = ma.masked_values(costilt, costilt.fill_value)
    sintilt = ma.array(sintilt)
    sintilt = ma.masked_values(sintilt, sintilt.fill_value)
    cosazm = ma.array(cosazm)
    cosazm = ma.masked_values(cosazm, cosazm.fill_value)
    sinazm = ma.array(sinazm)
    sinazm = ma.masked_values(sinazm, sinazm.fill_value)

    cosazm_costilt = cosazm*costilt
    sinazm_costilt = sinazm*costilt

    cosazm_costilt = ma.array(cosazm_costilt)
    cosazm_costilt = ma.masked_values(cosazm_costilt,cosazm_costilt.fill_value)
    sinazm_costilt = ma.array(sinazm_costilt)
    sinazm_costilt = ma.masked_values(sinazm_costilt,sinazm_costilt.fill_value)

    return cosazm_costilt.astype(np.float32), sinazm_costilt.astype(np.float32), sintilt.astype(np.float32)

def _make_obs(comm, input_path, mapping_path, cycle_time):

    # Get container from mapping file first
    logging(comm, 'INFO', 'Get container from bufr')
    container = bufr.Parser(input_path, mapping_path).parse(comm)

    logging(comm, 'DEBUG', f'container list (original): {container.list()}')
    logging(comm, 'DEBUG', f'all_sub_categories =  {container.all_sub_categories()}')
    logging(comm, 'DEBUG', f'category map =  {container.get_category_map()}')

    # Add new/derived data into container
    for cat in container.all_sub_categories():  

        logging(comm, 'DEBUG', f'category = {cat}')

        # add timeOffset
        logging(comm, 'DEBUG', f'Do DateTime calculation')
        otmct = container.get('variables/timestamp', cat)
        timediff = _compute_timeoffset(otmct, cycle_time)
        #logging(comm,'DEBUG',f'datetime min/max = {mindatetime, maxdatetime}')
        logging(comm,'DEBUG',f'cycle time is {cycle_time}')
        logging(comm, 'DEBUG', f'timeOffset min/max = {np.nanmin(timediff)} {np.nanmax(timediff)}')
        # Replace the timeOffset variable
        logging(comm, 'DEBUG', f'Update timeoffset in container')
        container.replace('variables/timeOffset',timediff,cat)

        stationid = container.get('variables/stationIdentification')
        paths = container.get_paths('variables/stationIdentification',cat)
        obtype = 990+stationid.astype(int)
        obtype = obtype.astype(np.int32) 
        logging(comm, 'DEBUG', f'station id: {stationid}')
        container.add('variables/obtype',obtype, paths,cat)
       
        azm_raw = container.get('variables/beamAzimuthAngle')
        tilt_raw = container.get('variables/beamTiltAngle')
        station_height = container.get('variables/stationElevation')
        station_height = station_height.astype(np.float32)
        gate_range = container.get('variables/gateRange')
        station_lat = container.get('variables/latitude')
        station_lon = container.get('variables/longitude')

        tilt, azm, obslat, obslon, obshgt = correct_tilt_azm(tilt_raw, azm_raw, station_height,station_lat,station_lon, gate_range)

        obserr = np.full_like(azm_raw, np.nan, dtype=np.float32)
        obserr.fill(5.)

        container.replace('variables/stationElevation', station_height, cat)
        # replace the org lat and lon
        container.replace('variables/latitude', obslat,cat)
        container.replace('variables/longitude', obslon,cat)
        logging(comm, 'DEBUG', f'replace latitude, before: lat min/max = {station_lat.min()} {station_lat.min()} after: lat min/max={obslat.min()} {obslat.max()}')
        logging(comm, 'DEBUG', f'replace longitude, before: lon min/max = {station_lon.min()} {station_lon.min()} after: lon min/max={obslon.min()} {obslon.max()}')
        # replace the org tilt and azm
        container.replace('variables/beamAzimuthAngle', azm, cat)
        container.replace('variables/beamTiltAngle', tilt, cat)
        logging(comm, 'DEBUG', f'azm min/max = {azm.min()} {azm.max()}')
        logging(comm, 'DEBUG', f'tilt min/max = {tilt.min()} {tilt.max()}')

        cosazm_costilt, sinazm_costilt, sintilt = compute_radar_related(tilt,azm)
        
        logging(comm, 'DEBUG', f'cosazm_costilt min/max = {cosazm_costilt.min()} {cosazm_costilt.max()}')
        logging(comm, 'DEBUG', f'sinazm_costilt min/max = {sinazm_costilt.min()} {sinazm_costilt.max()}')
        logging(comm, 'DEBUG', f'sintilt min/max = {sintilt.min()} {sintilt.max()}')
        logging(comm, 'DEBUG', f'height min/max = {obshgt.min()} {obshgt.max()}')

        # add new variables
        paths = container.get_paths('variables/beamAzimuthAngle', cat)
        container.add('variables/cosazm_costilt', cosazm_costilt, paths, cat)
        container.add('variables/sinazm_costilt', sinazm_costilt, paths, cat)
        container.add('variables/sintilt', sintilt, paths, cat)
        container.add('variables/height', obshgt, paths, cat)
        container.add('variables/obserr', obserr, paths, cat)

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

def create_obs_file(input_path, mapping_path, output_path, cycle_time):

    comm = bufr.mpi.Comm("world")
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
    parser.add_argument('cycle_time', type=str, help='cycle time in YYYYMMDDHH format')

    args = parser.parse_args()
    mapping = args.mapping
    infile = args.input
    output = args.output
    cycle_time = args.cycle_time

    create_obs_file(infile, mapping, output,cycle_time)

    end_time = time.time()
    running_time = end_time - start_time
    logging(comm, 'INFO', f'Total running time: {running_time}')
