#! /usr/bin/env python3
################################################################################
# Script Name: hafs_era5_ic_download.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script downloads the global 0.25-degree ERA5 pressure-level and
#   single-level analyses needed by hafs_era5_to_gfsgrib2.py to generate
#   pseudo-GFS GRIB2 files for the HAFS atmospheric initial and lateral
#   boundary conditions. One pair of NetCDF files is downloaded per day:
#     ERA5_pl_YYYYMMDD.nc and ERA5_sfc_YYYYMMDD.nc
#   Two sources are supported (--source):
#     cds:  the Copernicus Climate Data Store, through the cdsapi module and
#           a CDS account (~/.cdsapirc). The requests wait in the CDS queue,
#           which can take many hours.
#     arco: the public ARCO-ERA5 copy of ERA5 in Google Cloud Storage
#           (https://github.com/google-research/arco-era5). No account and
#           no queue. Only the analysis times and fields needed are fetched,
#           over HTTPS. It needs the numcodecs (or blosc) and netCDF4 modules.
#           It has no rain and snow water content, which HAFS does not read
#           from GRIB2 input anyway (see the tracers of ictype=gfsgrib2_0p25
#           in exhafs_atm_ic.sh and exhafs_atm_lbc.sh).
#   (The ERA5 surface forcing for the CDEPS DATM component is downloaded by
#   cdeps_utils/hafs_era5_download.py instead.)
# History:
#   11/10/2026: Initial version for the HAFS reanalysis subproject
# Usage:
#   ./hafs_era5_ic_download.py [--source cds|arco] [-o outdir]
#                              [--hour-step 3] [-j jobs] day [...]
#   where a day is YYYYMMDD or a range YYYYMMDD-YYYYMMDD. Use -j to
#   submit several requests at once: each one waits in the CDS queue, so this
#   is faster than one request at a time. The CDS limits the queued requests
#   per user and dataset; -j 2 (one request per dataset) always fits, and a
#   request rejected for this limit with a larger -j is submitted again later.
#   With --source arco, -j is the number of parallel HTTPS transfers.
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
################################################################################
# This next line will abort in any version earlier than Python 3.6:
f'This script requires Python 3.6 or newer.'

import os
import sys
import argparse
import logging
import json
import time
import datetime
import urllib.error
import urllib.request
import http.client
import threading
import concurrent.futures

# Part of the CDS error message for too many queued requests (lower case)
QUEUE_LIMIT_MESSAGE = 'number queued requests'

UTILITY_NAME = 'hafs_era5_ic_download'
logger = logging.getLogger(UTILITY_NAME)

PRESSURE_LEVELS = [
    '1', '2', '3', '5', '7', '10', '20', '30', '50', '70', '100', '125',
    '150', '175', '200', '225', '250', '300', '350', '400', '450', '500',
    '550', '600', '650', '700', '750', '775', '800', '825', '850', '875',
    '900', '925', '950', '975', '1000']

DATASETS = {
    'pl': ('reanalysis-era5-pressure-levels', [
        'geopotential', 'temperature', 'specific_humidity',
        'u_component_of_wind', 'v_component_of_wind', 'vertical_velocity',
        'ozone_mass_mixing_ratio', 'specific_cloud_liquid_water_content',
        'specific_cloud_ice_water_content', 'specific_rain_water_content',
        'specific_snow_water_content']),
    'sfc': ('reanalysis-era5-single-levels', [
        'surface_pressure', 'geopotential', 'land_sea_mask',
        'skin_temperature', 'sea_surface_temperature', 'sea_ice_cover',
        'snow_depth', 'snow_density', '2m_temperature',
        '2m_dewpoint_temperature', '10m_u_component_of_wind',
        '10m_v_component_of_wind', 'mean_sea_level_pressure',
        'soil_temperature_level_1', 'soil_temperature_level_2',
        'soil_temperature_level_3', 'soil_temperature_level_4',
        'volumetric_soil_water_layer_1', 'volumetric_soil_water_layer_2',
        'volumetric_soil_water_layer_3', 'volumetric_soil_water_layer_4']),
}

# ARCO-ERA5: hourly 0.25-degree analyses on the 37 pressure levels, as a Zarr
# (version 2) store with one chunk per variable and time.
ARCO_URL = ('https://storage.googleapis.com/gcp-public-data-arco-era5/ar/'
            'full_37-1h-0p25deg-chunk-1.zarr-v3')
# ARCO-ERA5 variable -> (variable name in the output file, units), with the
# same variable names as in the CDS NetCDF files.
ARCO_VARIABLES = {
    'pl': [
        ('geopotential', 'z', 'm**2 s**-2'),
        ('temperature', 't', 'K'),
        ('specific_humidity', 'q', 'kg kg**-1'),
        ('u_component_of_wind', 'u', 'm s**-1'),
        ('v_component_of_wind', 'v', 'm s**-1'),
        ('vertical_velocity', 'w', 'Pa s**-1'),
        ('ozone_mass_mixing_ratio', 'o3', 'kg kg**-1'),
        ('specific_cloud_liquid_water_content', 'clwc', 'kg kg**-1'),
        ('specific_cloud_ice_water_content', 'ciwc', 'kg kg**-1')],
    'sfc': [
        ('surface_pressure', 'sp', 'Pa'),
        ('geopotential_at_surface', 'z', 'm**2 s**-2'),
        ('land_sea_mask', 'lsm', '(0 - 1)'),
        ('skin_temperature', 'skt', 'K'),
        ('sea_surface_temperature', 'sst', 'K'),
        ('sea_ice_cover', 'siconc', '(0 - 1)'),
        ('snow_depth', 'sd', 'm of water equivalent'),
        ('snow_density', 'rsn', 'kg m**-3'),
        ('2m_temperature', 't2m', 'K'),
        ('2m_dewpoint_temperature', 'd2m', 'K'),
        ('10m_u_component_of_wind', 'u10', 'm s**-1'),
        ('10m_v_component_of_wind', 'v10', 'm s**-1'),
        ('mean_sea_level_pressure', 'msl', 'Pa'),
        ('soil_temperature_level_1', 'stl1', 'K'),
        ('soil_temperature_level_2', 'stl2', 'K'),
        ('soil_temperature_level_3', 'stl3', 'K'),
        ('soil_temperature_level_4', 'stl4', 'K'),
        ('volumetric_soil_water_layer_1', 'swvl1', 'm**3 m**-3'),
        ('volumetric_soil_water_layer_2', 'swvl2', 'm**3 m**-3'),
        ('volumetric_soil_water_layer_3', 'swvl3', 'm**3 m**-3'),
        ('volumetric_soil_water_layer_4', 'swvl4', 'm**3 m**-3')],
}

def parse_days(specs):
    """Expands YYYYMMDD and YYYYMMDD-YYYYMMDD arguments to a sorted list of
    dates."""
    days = set()
    for spec in specs:
        first, _, last = spec.partition('-')
        start = datetime.datetime.strptime(first, '%Y%m%d').date()
        end = datetime.datetime.strptime(last or first, '%Y%m%d').date()
        if end < start:
            raise ValueError(f'Invalid range of days: {spec}')
        while start <= end:
            days.add(start)
            start += datetime.timedelta(days=1)
    return sorted(days)

def build_request(kind, day, hour_step):
    """Returns the (dataset, request) of the CDS API retrieval."""
    dataset, variables = DATASETS[kind]
    request = {
        'product_type': ['reanalysis'],
        'variable': variables,
        'year': [f'{day.year:04d}'],
        'month': [f'{day.month:02d}'],
        'day': [f'{day.day:02d}'],
        'time': [f'{hour:02d}:00' for hour in range(0, 24, hour_step)],
        'data_format': 'netcdf',
        'download_format': 'unarchived',
    }
    if kind == 'pl':
        request['pressure_level'] = PRESSURE_LEVELS
    return dataset, request

################################################################################
# ARCO-ERA5

def blosc_decompress():
    """Returns a function to decompress the Blosc-compressed Zarr chunks."""
    try:
        import numcodecs
        return numcodecs.Blosc().decode
    except ImportError:
        pass
    try:
        import blosc
        return blosc.decompress
    except ImportError:
        raise ValueError(
            'The arco source needs the numcodecs or the blosc module to '
            'decompress the data. Install one of them, e.g., with '
            '"pip install numcodecs --user".')

class ArcoStore(object):
    """Reads whole chunks of the ARCO-ERA5 Zarr store over HTTPS."""

    def __init__(self, url=ARCO_URL, retries=6):
        import numpy
        self.np = numpy
        self.url = url
        self.retries = retries
        self.decompress = blosc_decompress()
        self.meta = json.loads(self.fetch('.zmetadata'))['metadata']
        attrs = self.meta['.zattrs']
        self.start = datetime.datetime.strptime(
            attrs['valid_time_start'], '%Y-%m-%d')
        # Last day of the final ERA5 and of the preliminary ERA5T data
        self.stop_final = datetime.datetime.strptime(
            attrs['valid_time_stop'], '%Y-%m-%d').date()
        self.stop = datetime.datetime.strptime(
            attrs.get('valid_time_stop_era5t', attrs['valid_time_stop']),
            '%Y-%m-%d').date()
        self._time_chunks = {}
        self._lock = threading.Lock()

    def fetch(self, key):
        """Returns the content of an object of the store, trying again
        after transient errors."""
        url = f'{self.url}/{key}'
        for attempt in range(1, self.retries+1):
            try:
                with urllib.request.urlopen(url, timeout=300) as response:
                    return response.read()
            except urllib.error.HTTPError as e:
                if e.code == 404:
                    raise ValueError(f'{url}: not in the ARCO-ERA5 store.')
                error = e
                if e.code not in (408, 429) and e.code < 500:
                    break
            except (urllib.error.URLError, http.client.HTTPException,
                    OSError) as e:
                error = e
            if attempt < self.retries:
                wait = min(120, 5*2**(attempt-1))
                logger.warning(f'{url}: {error}. Trying again in {wait} s.')
                time.sleep(wait)
        raise ValueError(f'{url}: download failed: {error}')

    def array(self, name):
        """Returns the Zarr array metadata, after checking that the array
        has the layout this script can read."""
        try:
            zarray = self.meta[f'{name}/.zarray']
        except KeyError:
            raise ValueError(f'{name}: not in the ARCO-ERA5 store.')
        compressor = zarray.get('compressor') or {}
        if zarray.get('zarr_format') != 2 or zarray.get('filters') or \
           compressor.get('id') != 'blosc' or zarray.get('order') != 'C' or \
           zarray.get('dimension_separator', '.') != '.':
            raise ValueError(f'{name}: unsupported Zarr array layout in the '
                             f'ARCO-ERA5 store: {zarray}')
        return zarray

    def chunk(self, name, index):
        """Returns one chunk of an array as a numpy array."""
        zarray = self.array(name)
        key = '.'.join([str(index)] + ['0']*(len(zarray['shape'])-1))
        data = self.decompress(self.fetch(f'{name}/{key}'))
        return self.np.frombuffer(data, dtype=zarray['dtype']).reshape(
            zarray['chunks'])

    def coordinate(self, name):
        zarray = self.array(name)
        if zarray['chunks'] != zarray['shape']:
            raise ValueError(f'{name}: unsupported chunks: {zarray}')
        return self.chunk(name, 0)

    def _times(self, ichunk):
        """Returns a chunk of the time coordinate."""
        zarray = self.array('time')
        with self._lock:
            if ichunk not in self._time_chunks:
                data = self.decompress(self.fetch(f'time/{ichunk}'))
                self._time_chunks[ichunk] = self.np.frombuffer(
                    data, dtype=zarray['dtype'])
            return self._time_chunks[ichunk]

    def time_index(self, when):
        """Returns the index of a valid time along the time dimension, after
        checking it against the time coordinate of the store."""
        zarray = self.array('time')
        units = self.meta['time/.zattrs']['units']
        if units != 'hours since 1900-01-01 00:00:00':
            raise ValueError(f'Unsupported ARCO-ERA5 time units: {units}')
        if when < self.start or when.date() > self.stop:
            raise ValueError(
                f'{when:%Y%m%d%H}: outside of the ARCO-ERA5 period '
                f'{self.start:%Y%m%d}-{self.stop:%Y%m%d}.')
        epoch = datetime.datetime(1900, 1, 1)
        # The time coordinate is hourly, but does not start at the first
        # valid time of the data.
        hours = int((when - epoch).total_seconds()//3600)
        index = hours - int(self._times(0)[0])
        if index < 0 or index >= zarray['shape'][0]:
            raise ValueError(f'{when:%Y%m%d%H}: outside of the ARCO-ERA5 '
                             'time coordinate.')
        ichunk, offset = divmod(index, zarray['chunks'][0])
        found = int(self._times(ichunk)[offset])
        if found != hours:
            raise ValueError(
                f'Unexpected ARCO-ERA5 time coordinate at index {index}: '
                f'{epoch + datetime.timedelta(hours=found)} instead of '
                f'{when}.')
        return index

    def field(self, name, when):
        """Returns the field ([level,] latitude, longitude) of a variable at
        a valid time."""
        zarray = self.array(name)
        if zarray['chunks'][0] != 1 or \
           zarray['chunks'][1:] != zarray['shape'][1:]:
            raise ValueError(f'{name}: unsupported chunks: {zarray}')
        return self.chunk(name, self.time_index(when))[0]

def arco_download_file(store, kind, day, hours, filename, jobs):
    """Writes the ARCO-ERA5 fields of one day to a NetCDF file with the
    layout of the CDS NetCDF files."""
    import netCDF4
    np = store.np
    times = [datetime.datetime(day.year, day.month, day.day, hour)
             for hour in hours]
    lat = store.coordinate('latitude')
    lon = store.coordinate('longitude')
    with netCDF4.Dataset(filename, 'w', format='NETCDF4') as ds:
        ds.title = 'ERA5 analyses from the ARCO-ERA5 store'
        ds.source = store.url
        ds.Conventions = 'CF-1.7'
        ds.createDimension('valid_time', len(times))
        var = ds.createVariable('valid_time', 'i8', ('valid_time',))
        var.units = 'seconds since 1970-01-01'
        var.calendar = 'proleptic_gregorian'
        var.standard_name = 'time'
        epoch = datetime.datetime(1970, 1, 1)
        var[:] = [int((when - epoch).total_seconds()) for when in times]
        dims = ('valid_time',)
        if kind == 'pl':
            levels = store.coordinate('level')
            ds.createDimension('pressure_level', len(levels))
            var = ds.createVariable('pressure_level', 'f8',
                                    ('pressure_level',))
            var.units = 'hPa'
            var.standard_name = 'air_pressure'
            var[:] = levels
            dims += ('pressure_level',)
        for name, values, units in (('latitude', lat, 'degrees_north'),
                                    ('longitude', lon, 'degrees_east')):
            ds.createDimension(name, len(values))
            var = ds.createVariable(name, 'f8', (name,))
            var.units = units
            var.standard_name = name
            var[:] = values
        dims += ('latitude', 'longitude')
        shape = tuple(len(ds.dimensions[dim]) for dim in dims)

        with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
            for name, short, units in ARCO_VARIABLES[kind]:
                attrs = store.meta.get(f'{name}/.zattrs', {})
                var = ds.createVariable(
                    short, 'f4', dims, zlib=True, complevel=1, shuffle=True,
                    chunksizes=(1,) + shape[1:], fill_value=np.nan)
                var.units = units
                var.long_name = attrs.get('long_name', name)
                logger.info(f'{filename}: {short} ({name})...')
                # Hold at most jobs fields in memory at a time.
                for first in range(0, len(times), jobs):
                    batch = times[first:first+jobs]
                    fields = pool.map(
                        lambda when: store.field(name, when), batch)
                    for itime, field in enumerate(fields, first):
                        if field.shape != shape[1:]:
                            raise ValueError(
                                f'{name}: unexpected dimensions '
                                f'{field.shape} instead of {shape[1:]}.')
                        var[itime] = field

def arco_download(args, days):
    """Downloads the files of the given days from ARCO-ERA5."""
    hours = range(0, 24, args.hour_step)
    tasks = []
    for day in days:
        for kind in ('pl', 'sfc'):
            filename = os.path.join(args.outdir,
                                    f'ERA5_{kind}_{day:%Y%m%d}.nc')
            if args.dry_run:
                names = [name for name, _, _ in ARCO_VARIABLES[kind]]
                print(f'{filename}: {ARCO_URL} {names} hours {list(hours)}')
            elif os.path.exists(filename):
                logger.info(f'{filename}: already exists. Skipping.')
            else:
                tasks.append((kind, day, filename))
    if not tasks:
        return 0
    try:
        import netCDF4
    except ImportError:
        raise ValueError('The arco source needs the netCDF4 module.')
    store = ArcoStore()
    failed = []
    for kind, day, filename in tasks:
        if day > store.stop_final:
            logger.warning(f'{day:%Y%m%d}: after {store.stop_final:%Y%m%d}, '
                           'so preliminary (ERA5T) data.')
        partial = filename + '.partial'
        logger.info(f'{filename}: retrieving from ARCO-ERA5...')
        try:
            arco_download_file(store, kind, day, hours, partial, args.jobs)
            os.replace(partial, filename)
            logger.info(f'{filename}: done.')
        except ValueError as e:
            logger.error(f'{filename}: download failed: {e}')
            failed.append(filename)
        finally:
            if os.path.exists(partial):
                os.remove(partial)
    if failed:
        logger.error(f'FATAL ERROR: {len(failed)} of {len(tasks)} downloads '
                     'failed. Rerun to retry them: '
                     + ' '.join(sorted(failed)))
        return 1
    return 0

################################################################################

def main(argv=None):
    parser = argparse.ArgumentParser(
        description='Download ERA5 analyses for HAFS initial and lateral '
        'boundary conditions (see hafs_era5_to_gfsgrib2.py).')
    parser.add_argument('days', nargs='+',
                        help='YYYYMMDD or YYYYMMDD-YYYYMMDD')
    parser.add_argument('-s', '--source', default='cds',
                        choices=['cds', 'arco'],
                        help='where to download from: the Copernicus Climate '
                        'Data Store (cds, needs an account and waits in its '
                        'queue) or the public ARCO-ERA5 store in Google '
                        'Cloud Storage (arco, no account and no queue; '
                        'without rain and snow water content, which HAFS '
                        'does not use) (default: %(default)s)')
    parser.add_argument('-o', '--outdir', default='.',
                        help='output directory (default: %(default)s)')
    parser.add_argument('--hour-step', type=int, default=3,
                        choices=[1, 2, 3, 6],
                        help='interval in hours of the analyses, which '
                        'should divide NBDYHRS (default: %(default)s)')
    parser.add_argument('-j', '--jobs', type=int, default=None,
                        help='cds: number of requests to submit at the same '
                        'time, so that they wait in the CDS queue together. '
                        'They are split between the pressure-level and the '
                        'single-level dataset, since the CDS limits the '
                        'queued requests per dataset (default: 1; 2 is one '
                        'request per dataset at a time). arco: number of '
                        'parallel transfers (default: 4)')
    parser.add_argument('--retry-wait', type=float, default=300.,
                        help='seconds to wait before submitting a request '
                        'again that the CDS rejected for too many queued '
                        'requests (default: %(default)s)')
    parser.add_argument('-n', '--dry-run', action='store_true',
                        help='print the requests without downloading')
    parser.add_argument('-v', '--verbose', action='store_true')
    args = parser.parse_args(argv)
    if args.jobs is None:
        args.jobs = 4 if args.source == 'arco' else 1
    if args.jobs < 1:
        parser.error('--jobs must be at least 1')

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format='%(asctime)s %(name)s %(levelname)s: %(message)s')

    days = parse_days(args.days)
    os.makedirs(args.outdir, exist_ok=True)
    if args.source == 'arco':
        return arco_download(args, days)
    tasks = []
    for day in days:
        for kind in ('pl', 'sfc'):
            filename = os.path.join(args.outdir,
                                    f'ERA5_{kind}_{day:%Y%m%d}.nc')
            dataset, request = build_request(kind, day, args.hour_step)
            if args.dry_run:
                print(f'{filename}: {dataset} {request}')
            elif os.path.exists(filename):
                logger.info(f'{filename}: already exists. Skipping.')
            else:
                tasks.append((kind, filename, dataset, request))
    if not tasks:
        return 0

    try:
        import cdsapi
    except ImportError:
        logger.error(
            'You are missing the cdsapi module! Install it with '
            '"pip install cdsapi --user", register on the CDS '
            'website, accept the ERA5 license, and put your key '
            'in ~/.cdsapirc.')
        return 2

    # One CDS client per thread, since the clients are not known to be
    # thread safe.
    local = threading.local()
    def retrieve(task):
        kind, filename, dataset, request = task
        if not hasattr(local, 'client'):
            local.client = cdsapi.Client()
        partial = filename + '.partial'
        logger.info(f'{filename}: retrieving from {dataset}...')
        while True:
            try:
                local.client.retrieve(dataset, request, partial)
                os.replace(partial, filename)
                break
            except Exception as e:
                # The CDS rejects a request when too many of the user's
                # requests for the dataset are queued: wait for some of
                # them to finish and submit it again.
                if QUEUE_LIMIT_MESSAGE not in str(e).lower():
                    raise
                logger.warning(f'{filename}: the CDS queue limit for '
                               f'{dataset} is reached. Trying again in '
                               f'{args.retry_wait} s.')
                time.sleep(args.retry_wait)
            finally:
                if os.path.exists(partial):
                    os.remove(partial)
        logger.info(f'{filename}: done.')

    # The requests wait in the CDS queue at the same time with --jobs > 1. The
    # CDS limits the queued requests per dataset, so the jobs are split
    # between the pressure-level and the single-level dataset, each with its
    # own pool of threads. A failed request does not stop the other ones.
    if args.jobs == 1:
        groups = [(1, tasks)]
    else:
        groups = [((args.jobs+1)//2, [t for t in tasks if t[0] == 'pl']),
                  (args.jobs//2, [t for t in tasks if t[0] == 'sfc'])]
    failed = []
    pools = []
    futures = {}
    try:
        for workers, group in groups:
            if not group:
                continue
            pool = concurrent.futures.ThreadPoolExecutor(
                max_workers=min(workers, len(group)))
            pools.append(pool)
            for task in group:
                futures[pool.submit(retrieve, task)] = task
        for future in concurrent.futures.as_completed(futures):
            filename = futures[future][1]
            try:
                future.result()
            except Exception as e:
                logger.error(f'{filename}: download failed: {e}')
                failed.append(filename)
    finally:
        for pool in pools:
            pool.shutdown()
    if failed:
        logger.error(f'FATAL ERROR: {len(failed)} of {len(tasks)} downloads '
                     'failed. Rerun to retry them: '
                     + ' '.join(sorted(failed)))
        return 1
    return 0

if __name__ == '__main__':
    try:
        sys.exit(main())
    except ValueError as e:
        logger.error(f'FATAL ERROR: {e}')
        sys.exit(1)
