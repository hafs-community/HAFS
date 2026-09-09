#!/usr/bin/env python3
import os
import sys
import bufr
import numpy as np
import calendar
import time
import argparse
from bufr.bufr_python.encoders import *
from bufr.encoders import netcdf
import numpy.ma as ma
from wxflow import Logger

# Initialize Logger
# Get log level from the environment variable, default to 'INFO it not set
log_level = os.getenv('LOG_LEVEL', 'INFO')
logger = Logger('bufr_hdob.py', level=log_level, colored_log=False)

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


def get_description(yaml_path, update=False):

    description = bufr.encoders.Description(yaml_path)

    if update:

        description.add_variable(name='MetaData/timeOffset',
                                 source='variables/timeOffset',
                                 units='s',
                                 longName='Observation Time Minus Reference Time')

        description.add_variable(name='ObsValue/windEastward',
                                 source='variables/windEastward',
                                 units='m s-1',
                                 longName='Eastward Wind Component')

        description.add_variable(name='ObsValue/windNorthward',
                                 source='variables/windNorthward',
                                 units='m s-1',
                                 longName='Northward Wind Component')

        description.add_variable(name='ObsValue/specificHumidity',
                                 source='variables/specificHumidity',
                                 units='kg kg-1',
                                 longName='Specific Humidity')

        description.add_variable(name='ObsValue/virtualTemperature',
                                 source='variables/virtualTemperature',
                                 units='K',
                                 longName='virtual Temperature')

        description.add_variable(name='ObsValue/virtualTemperatureFlag',
                                 source='variables/virtualTemperatureFlag',
                                 units='',
                                 longName='virtual Temperature Flag')

        description.add_variable(name='ObsType/virtualTemperature',
                                 source='variables/obstype_t',
                                 units='',
                                 longName='Observation Type')

        description.add_variable(name='ObsType/airTemperature',
                                 source='variables/obstype_t',
                                 units='',
                                 longName='Observation Type')

        description.add_variable(name='ObsType/specificHumidity',
                                 source='variables/obstype_q',
                                 units='',
                                 longName='Observation Type')

        description.add_variable(name='ObsType/windEastward',
                                 source='variables/obstype_uwind',
                                 units='',
                                 longName='Observation Type')

        description.add_variable(name='ObsType/windNorthward',
                                 source='variables/obstype_vwind',
                                 units='',
                                 longName='Observation Type')

        description.add_variable(name='ObsType/windSpeedAt10M',
                                 source='variables/obstype_spd',
                                 units='',
                                 longName='Observation Type')

        description.add_variable(name='QualityMarker/airTemperature',
                                 source='variables/airTemperatureQM',
                                 units='',
                                 longName='Quality Indicator for air temperature')

        description.add_variable(name='QualityMarker/specificHumidity',
                                 source='variables/specificHumidityQM',
                                 units='',
                                 longName='Quality Indicator for Specific Humidity')

        description.add_variable(name='QualityMarker/windEastward',
                                 source='variables/windEastwardQM',
                                 units='',
                                 longName='Quality Indicator for wind')

        description.add_variable(name='QualityMarker/windNorthward',
                                 source='variables/windNorthwardQM',
                                 units='',
                                 longName='Quality Indicator for wind')

        description.add_variable(name='QualityMarker/windSpeedAt10M',
                                 source='variables/windSpeedAt10MQM',
                                 units='',
                                 longName='Quality indicator for SFMR wind speed')


        description.add_variable(name='ObsError/windEastward',
                                 source='variables/windEastward',
                                 units='m s-1',
                                 longName='Eastward Wind Component Error')

        description.add_variable(name='ObsError/windNorthward',
                                 source='variables/windNorthward',
                                 units='m s-1',
                                 longName='Northward Wind Component Error')

        description.add_variable(name='ObsError/specificHumidity',
                                 source='variables/specificHumidity',
                                 units='kg kg-1',
                                 longName='Specific Humidity Error')

        description.add_variable(name='ObsError/virtualTemperature',
                                 source='variables/virtualTemperature',
                                 units='K',
                                 longName='virtual Temperature Error')

        description.add_variable(name='ObsError/airTemperature',
                                 source='variables/virtualTemperature',
                                 units='K',
                                 longName='virtual Temperature Error')


    return description


def Compute_WindComponents_from_WindDirection_and_WindSpeed(wdir, wspd):

    uob = (-wspd * np.sin(np.radians(wdir))).astype(np.float32)
    vob = (-wspd * np.cos(np.radians(wdir))).astype(np.float32)

    uob = ma.array(uob)
    uob = ma.masked_values(uob, uob.fill_value)

    vob = ma.array(vob)
    vob = ma.masked_values(vob, vob.fill_value)
    
    return uob, vob

def Compute_SpecificHumidity_from_dewPoint_and_Pressure(tmdp, prlc):

    # est is saturated vapor pressure in Mb
    est = 6.1078 * np.exp((17.269 * (tmdp - 273.16))/((tmdp - 273.16)+237.3))
    # Change est from Mb to Pa
    est = est*100.

    # specificHumidity (qob) is in kg/kg
    qob = (0.622 * est)/(prlc - (0.378 * est)).astype(np.float32)
    qob = ma.array(qob)
    qob = ma.masked_values(qob, qob.fill_value)
#    qob = ma.masked_equal(qob, qob.fill_value)

    return qob

def Compute_SpecificHumidity_from_dewPoint_or_relativeH(tob, tmdp, rhob, prlc):
    rd = 287.04
    rv = 461.6
    eps = rd/rv
    omeps = 1.0-eps
    hvap = 2.5000e+6

    nob = tob.shape[0]
    qob = np.full(nob,0.).astype(np.float32)
    qob = ma.array(qob)

    for ii in range(nob):
        if ma.is_masked(tmdp[ii]):
            qob[ii] = tob.fill_value
        else:
            prlc_cb = prlc[ii]/1000
            if not ma.is_masked(rhob[ii]):
                es = Compute_SaturationVaporPressure(tob[ii])
                qsat = eps*es/(prlc_cb-omeps*es)
                qob[ii] = rhob[ii]*qsat
            else:
                rhob_calc = np.exp((1.0-tob[ii]/tmdp[ii])*(hvap/rv)/tob[ii])
                es = Compute_SaturationVaporPressure(tob[ii])
                qsat = eps*es/(prlc_cb-omeps*es)
                qob[ii] = rhob_calc*qsat

    qob = ma.masked_values(qob, tob.fill_value)

    return qob

def Compute_SaturationVaporPressure(t):
    ttp = 2.7316e+2
    tmix = ttp-20.
    cvap = 1.8460e+3
    cliq = 4190.0
    csol = 2.1060e+3
    dldti = cvap-csol
    rv = 461.6
    xai = -(dldti/rv)
    hvap = 2.5000e+6
    hfus = 3.3358e+5
    hsub = hvap+hfus
    dldt = cvap-cliq
    xbi = xai+hsub/(rv*ttp)
    xa = -(dldt/rv)
    xb = xa+hvap/(rv*ttp)
    psatk = 6.1078e+2*0.001

    tr = ttp/t
    if (t >= ttp):
        es = psatk*tr**xa*np.exp(xb*(1.0-tr))
    elif (t < tmix):
        es = psatk*tr**xai*np.exp(xbi*(1.0-tr))
    else:
        w = (t-tmix)/(ttp-tmix)
        es = w*psatk*tr**xa*np.exp(xb*(1.0-tr))+(1.0-w)*psatk*tr**xai*np.exp(xbi*(1.0-tr))

    return es

def Compute_airTemperatureNew(tob, qob):

    tob_new = tob.copy()
    nob = tob.shape[0]
    tvflg = np.full(nob,1).astype(np.int32)

    for ii in range(nob):
        if not ma.is_masked(qob[ii]):
            tvflg[ii] = 0
            tob_new[ii] = tob[ii]*(1.0+0.61*qob[ii])

    return tob_new, tvflg


def Compute_QualityMarker(mqm,sID):

    t_qm = np.full(mqm.shape[0], 0).astype(np.int32)
    q_qm = np.full(mqm.shape[0], 0).astype(np.int32)
    uv_qm = np.full(mqm.shape[0], 0).astype(np.int32)
    wspd_qm = np.full(mqm.shape[0], 0).astype(np.int32)

    t_qm = np.where((mqm==8192)|(mqm==12288)|(mqm==10240)|(mqm==14336),11,t_qm)
    q_qm = np.where((mqm==8192)|(mqm==12288)|(mqm==10240)|(mqm==14336),11,q_qm)
    uv_qm = np.where((mqm==4096)|(mqm==12288)|(mqm==6144)|(mqm==14336),11,uv_qm)
    wspd_qm = np.where((mqm==2048)|(mqm==10240)|(mqm==6144)|(mqm==14336),11,wspd_qm)
    
    kuas_mask = (sID == "KUAS") #XL Temp Fix for KUAS
    t_qm[kuas_mask] = 0
    q_qm[kuas_mask] = 0
    uv_qm[kuas_mask] = 0
    wspd_qm[kuas_mask] = 0

    return t_qm, q_qm, uv_qm, wspd_qm

def ComputeTimeOffset(obtime, cycle_time):
    """
    Compute TimeOffset using the cycleTimeSinceEpoch and dateTime
       obs dateTime minus Cycle Time

    Parameters:
        cycleTimeSinceEpoch: Time of cycle in Epoch Time
        obtime: Observation Time in Epoch Time

    Returns:
        Masked array of time difference between obs time and cycle time (in sec)
    """

    cycleTimeSinceEpoch = np.int64(calendar.timegm(time.strptime(str(int(cycle_time)), '%Y%m%d%H')))

    nob = obtime.shape[0]
    time_diff = np.full(nob,0.).astype(np.float32)
#    time_diff = np.zeros(obtime.shape,dtype=np.float)
    for ii in range(nob):
        if ma.is_masked(obtime[ii]):
            continue
        else:
            time_diff[ii] = obtime[ii] - cycleTimeSinceEpoch

    time_diff = ma.array(time_diff)
    time_diff = ma.masked_values(time_diff, obtime.fill_value)

    return time_diff


def _make_obs(comm, input_path, mapping_path, cycle_time):
    """
    Create the ioda hdob bufr observations:
    - reads values
    - adds windEastward, windNorthward
    - adds ObsValue/specificHumidity
    - adds ObsValue/virtualTemperature, ObsValue/virtualTemperatureFlag
    - adds quality marker
    - adds ObsType

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
#    description = get_description(mapping_path, update=True)

    logging(comm, 'DEBUG', f'container list (original): {container.list()}')

    # Add new variable: MetaData/timeOffset
    logging(comm, 'DEBUG', f'Add MetaData/timeOffset')
    paths = container.get_paths('variables/timestamp')
    obtime = container.get('variables/timestamp')
    otmct = ComputeTimeOffset(obtime, cycle_time)

    container.add('variables/timeOffset', otmct, paths)

    # Add new variables: ObsValue/windEastward & ObsValue/windNorthward
    logging(comm, 'DEBUG', f'Add ObsValue/windEastward and ObsValue/windNorthward')
    wdir = container.get('variables/windDirection')
    wspd = container.get('variables/windSpeed')

    uob, vob = Compute_WindComponents_from_WindDirection_and_WindSpeed(wdir, wspd)
    paths = container.get_paths('variables/windSpeed')
    container.add('variables/windEastward', uob, paths)
    container.add('variables/windNorthward', vob, paths)

    # Add new variables: ObsValue/specificHumidity
    tmdp = container.get('variables/dewpointTemperature')
    prlc = container.get('variables/pressure')
    rhob = container.get('variables/relativeHumidity')
    tob = container.get('variables/airTemperature')
    qob = Compute_SpecificHumidity_from_dewPoint_or_relativeH(tob, tmdp, rhob, prlc)
#    qob = Compute_SpecificHumidity_from_dewPoint_and_Pressure(tmdp, prlc)
#    qob = ma.masked_where(qob > 3e10, qob)
    print('checking qob =', qob[0:20])
    print('qob.fill_value = ', qob.fill_value)
    container.add('variables/specificHumidity',qob, paths)

    # Replace airTemperature t -> tv if qob exists
    tob_new, tvflg = Compute_airTemperatureNew(tob, qob)
    container.add('variables/virtualTemperature', tob_new, paths)
    container.add('variables/virtualTemperatureFlag', tvflg, paths)

    # Add new quality marker:
    mqm = container.get('variables/meteroQM')
    sID = container.get('variables/stationID')
    t_qm, q_qm, uv_qm, wspd_qm = Compute_QualityMarker(mqm,sID)

    container.add('variables/airTemperatureQM', t_qm, paths)
    container.add('variables/specificHumidityQM', q_qm, paths)
    container.add('variables/windEastwardQM', uv_qm, paths)
    container.add('variables/windNorthwardQM', uv_qm, paths)
    container.add('variables/windSpeedAt10MQM', wspd_qm, paths)

    # Add new Obs Type:
    obstype = np.full(uob.shape[0], np.int32(236))
    container.add('variables/obstype_uwind', obstype, paths)
    container.add('variables/obstype_vwind', obstype, paths)
    obstype = np.full(tob.shape[0], np.int32(136))
    container.add('variables/obstype_t', obstype, paths)
    container.add('variables/obstype_q', obstype, paths)
    obstype = np.full(uob.shape[0], np.int32(213))
    container.add('variables/obstype_spd', obstype, paths)    

    # Check
    logging(comm, 'DEBUG', f'container list (updated): {container.list()}')

    return container

def create_obs_file(input_path, mapping_path, output_path, cycle_time):

    comm = bufr.mpi.Comm("world")
    
    container = _make_obs(comm, input_path, mapping_path, cycle_time)
    container.gather(comm)
    description = get_description(mapping_path, update=True)    

    # Encode the data
    if comm.rank() == 0:
        netcdf.Encoder(description).encode(container, output_path)

    logging(comm, 'INFO', f'Return the encoded data')


if __name__ == '__main__':

    bufr.mpi.App(sys.argv)
    start_time = time.time()
    comm = bufr.mpi.Comm("world")

    parser = argparse.ArgumentParser()
    parser.add_argument('input', type=str, help='Input bufr file')
    parser.add_argument('mapping', type=str, help='BUFR2IODA Mapping File')
    parser.add_argument('output', type=str, help='Output NetCDF file')
    parser.add_argument('cycle_time', type=str, help='cycle time in YYYYMMDDHH format')
#   parser.add_argument('infiles', nargs='+', help='A list of strings')

    args = parser.parse_args()
    infile = args.input
    output = args.output
    mapping = args.mapping
    cycle_time = args.cycle_time

    print(f'checking infile ... ')
    print(f'{infile} ... ')

    create_obs_file(infile, mapping, output, cycle_time)

    end_time = time.time()
    running_time = end_time - start_time
    logging(comm,'INFO',f'Total running time: {running_time}')
