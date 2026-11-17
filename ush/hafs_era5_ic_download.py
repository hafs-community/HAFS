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
#   (The ERA5 surface forcing for the CDEPS DATM component is downloaded by
#   cdeps_utils/hafs_era5_download.py instead.)
# History:
#   11/10/2026: Initial version for the HAFS reanalysis subproject
# Usage:
#   ./hafs_era5_ic_download.py [-o outdir] [--hour-step 3] [-j jobs] day [...]
#   where a day is YYYYMMDD or a range YYYYMMDD-YYYYMMDD. Use -j (e.g., -j 4)
#   to submit several requests at once: each one waits in the CDS queue, so
#   this is much faster than one request at a time.
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
import datetime
import threading
import concurrent.futures

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

def main(argv=None):
    parser = argparse.ArgumentParser(
        description='Download ERA5 analyses for HAFS initial and lateral '
        'boundary conditions (see hafs_era5_to_gfsgrib2.py).')
    parser.add_argument('days', nargs='+',
                        help='YYYYMMDD or YYYYMMDD-YYYYMMDD')
    parser.add_argument('-o', '--outdir', default='.',
                        help='output directory (default: %(default)s)')
    parser.add_argument('--hour-step', type=int, default=3,
                        choices=[1, 2, 3, 6],
                        help='interval in hours of the analyses, which '
                        'should divide NBDYHRS (default: %(default)s)')
    parser.add_argument('-j', '--jobs', type=int, default=1,
                        help='number of requests to submit to the CDS at the '
                        'same time, so that they wait in its queue together '
                        '(default: %(default)s; the CDS limits the requests '
                        'per user, so more than about 4 gains little)')
    parser.add_argument('-n', '--dry-run', action='store_true',
                        help='print the requests without downloading')
    parser.add_argument('-v', '--verbose', action='store_true')
    args = parser.parse_args(argv)
    if args.jobs < 1:
        parser.error('--jobs must be at least 1')

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format='%(asctime)s %(name)s %(levelname)s: %(message)s')

    days = parse_days(args.days)
    os.makedirs(args.outdir, exist_ok=True)
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
                tasks.append((filename, dataset, request))
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
        filename, dataset, request = task
        if not hasattr(local, 'client'):
            local.client = cdsapi.Client()
        partial = filename + '.partial'
        logger.info(f'{filename}: retrieving from {dataset}...')
        try:
            local.client.retrieve(dataset, request, partial)
            os.replace(partial, filename)
        finally:
            if os.path.exists(partial):
                os.remove(partial)
        logger.info(f'{filename}: done.')

    # The requests wait in the CDS queue at the same time with --jobs > 1. A
    # failed request does not stop the other ones; rerun to retry it.
    failed = []
    jobs = min(args.jobs, len(tasks))
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
        futures = {pool.submit(retrieve, task): task for task in tasks}
        for future in concurrent.futures.as_completed(futures):
            filename = futures[future][0]
            try:
                future.result()
            except Exception as e:
                logger.error(f'{filename}: download failed: {e}')
                failed.append(filename)
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
