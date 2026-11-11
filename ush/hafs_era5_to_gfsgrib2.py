#! /usr/bin/env python3
################################################################################
# Script Name: hafs_era5_to_gfsgrib2.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script converts ERA5 reanalysis data (0.25-degree pressure-level and
#   single-level NetCDF files, e.g., from hafs_era5_ic_download.py) into
#   "pseudo-GFS" 0.25-degree GRIB2 files, which use the GFS directory layout,
#   file names and NCEP GRIB2 parameter definitions:
#     --gfsver PROD2026 (GFSv17, default):
#       ${outdir}/gfs.YYYYMMDD/HH/products/atmos/grib2/0p25/
#           gfs.tHHz.pres_a.0p25.fFFF.grib2
#     --gfsver PROD2021 (GFSv16):
#       ${outdir}/gfs.YYYYMMDD/HH/atmos/gfs.tHHz.pgrb2.0p25.fFFF
#   The --gfsver option needs to match the GFSVER of the HAFS configuration.
#   The file for cycle YYYYMMDDHH and forecast hour FFF holds the ERA5
#   *analysis* valid at cycle+FFF. HAFS can then generate its atmospheric
#   initial and lateral boundary conditions from ERA5 through the existing
#   ictype=gfsgrib2_0p25 and bctype=gfsgrib2_0p25 options (chgres_cube with
#   input_type="grib2"), for cases without GFS data in the current format.
#
#   The records written are those read by chgres_cube for GFS GRIB2 input:
#     isobaric: HGT TMP SPFH UGRD VGRD [VVEL O3MR CLWMR ICMR RWMR SNMR]
#     surface : PRES HGT TMP LAND ICEC WEASD SNOD, TSOIL and SOILW (4 layers)
#     other   : TMP and SPFH at 2 m, [UGRD and VGRD at 10 m], [PRMSL]
#   Fields in [] are optional and written only if present in the input.
#
#   Notes on the conversion (see doc of each function for details):
#     - Soil temperature/moisture are remapped from the ERA5 layers (0-7,
#       7-28, 28-100, 100-289 cm) onto the Noah layers (0-10, 10-40, 40-100,
#       100-200 cm) using thickness-weighted averages.
#     - Volumetric soil moisture is passed through and only limited to
#       [0.02, 0.468]; it is not rescaled for the differences between the
#       ECMWF and Noah land surface models.
#     - Glacier points (ERA5 snow depth of 10 m water equivalent) get the GFS
#       land-ice flag (total soil moisture of 1.0), which chgres_cube uses to
#       identify land ice when the vegetation type is not available.
#     - The surface temperature uses the ERA5 sea surface temperature at
#       ice-free water points and the skin temperature elsewhere.
#     - There is no NSST information, so use nstf_name=2,1,0,0,0.
# History:
#   11/10/2026: Initial version for the HAFS reanalysis subproject
# Usage:
#   ./hafs_era5_to_gfsgrib2.py --pl ERA5_pl_*.nc --sfc ERA5_sfc_*.nc \
#       --cycle 2005082800 --fhrs 0:126:3 --outdir /path/to/COMERA5
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

import numpy as np
import netCDF4

try:
    import eccodes
except ImportError as ie:
    sys.stderr.write("""You are missing the eccodes Python module!
You must install it to run this script, e.g.:

  conda install -c conda-forge python-eccodes
  (or: pip install eccodes --user)
""")
    raise

UTILITY_NAME = 'hafs_era5_to_gfsgrib2'
logger = logging.getLogger(UTILITY_NAME)

GRAV = 9.80665     # m s-2, used by ECMWF to define geopotential
RHO_WATER = 1000.  # kg m-3
ICE_MIN = 0.15     # sea ice fraction above which chgres_cube assumes ice

# ERA5 (IFS HTESSEL) and GFS (Noah) soil layer interfaces in meters
ERA5_SOIL_DEPTHS = (0.0, 0.07, 0.28, 1.00, 2.89)
NOAH_SOIL_DEPTHS = (0.0, 0.10, 0.40, 1.00, 2.00)
SOILM_MIN = 0.02
SOILM_MAX = 0.468
SOILM_LANDICE = 1.0     # GFS flag value at land-ice points
GLACIER_SWE = 9.99      # m; ERA5 sets the snow depth to 10 m w.e. at glaciers

# GRIB2 fixed surface types
SFC, ISBL, MSL, HAGL, DBLL = 1, 100, 101, 103, 106

# Pressure-level fields:
#   GFS name: (ERA5 short names, discipline, category, number, nbits, required)
PL_FIELDS = [
    ('HGT',   ('z',),    0,  3,   5, 20, True),
    ('TMP',   ('t',),    0,  0,   0, 16, True),
    ('SPFH',  ('q',),    0,  1,   0, 16, True),
    ('UGRD',  ('u',),    0,  2,   2, 16, True),
    ('VGRD',  ('v',),    0,  2,   3, 16, True),
    ('VVEL',  ('w',),    0,  2,   8, 16, False),
    ('O3MR',  ('o3',),   0, 14, 192, 16, False),
    ('CLWMR', ('clwc',), 0,  1,  22, 16, False),
    ('ICMR',  ('ciwc',), 0,  1,  23, 16, False),
    ('RWMR',  ('crwc',), 0,  1,  24, 16, False),
    ('SNMR',  ('cswc',), 0,  1,  25, 16, False),
]

# ERA5 single-level short names (with alternatives used by different sources)
SFC_NAMES = {
    'sp':    ('sp',),
    'zsfc':  ('z', 'orog_z', 'zsfc'),
    'lsm':   ('lsm',),
    'skt':   ('skt',),
    'sst':   ('sst',),
    'ci':    ('siconc', 'ci'),
    'sd':    ('sd',),
    'rsn':   ('rsn',),
    't2m':   ('t2m', '2t'),
    'd2m':   ('d2m', '2d'),
    'u10':   ('u10', '10u'),
    'v10':   ('v10', '10v'),
    'msl':   ('msl',),
    'stl':   (('stl1',), ('stl2',), ('stl3',), ('stl4',)),
    'swvl':  (('swvl1',), ('swvl2',), ('swvl3',), ('swvl4',)),
}
SFC_REQUIRED = ('sp', 'zsfc', 'lsm', 'skt', 't2m', 'd2m', 'stl', 'swvl')

################################################################################
# Input

class ERA5Files(object):
    """Indexes the valid times of a set of ERA5 NetCDF files, so that fields
    can be read by variable name and valid time regardless of which file
    holds them."""

    def __init__(self, filenames, what):
        self.what = what
        self.datasets = []
        self.index = {}  # valid datetime -> (dataset, time index)
        for filename in filenames:
            ds = netCDF4.Dataset(filename, 'r')
            ds.set_auto_maskandscale(True)
            self.datasets.append(ds)
            tname = self._find(ds, ('valid_time', 'time'), filename)
            tvar = ds.variables[tname]
            times = netCDF4.num2date(
                tvar[:], tvar.units, getattr(tvar, 'calendar', 'standard'),
                only_use_cftime_datetimes=False,
                only_use_python_datetimes=True)
            for itime, when in enumerate(np.atleast_1d(times)):
                when = when.replace(tzinfo=None, microsecond=0)
                self.index.setdefault(when, []).append((ds, itime, tname))
        if not self.index:
            raise ValueError(f'No valid times found in the {what} files.')

    @staticmethod
    def _find(ds, names, filename):
        for name in names:
            if name in ds.variables:
                return name
        raise KeyError(f'{filename}: none of the variables {names} found.')

    def close(self):
        for ds in self.datasets:
            ds.close()

    def grid(self):
        """Returns (lat, lon) 1D arrays of the first file."""
        ds = self.datasets[0]
        lat = ds.variables[self._find(ds, ('latitude', 'lat'), self.what)][:]
        lon = ds.variables[self._find(ds, ('longitude', 'lon'), self.what)][:]
        return np.asarray(lat, dtype='f8'), np.asarray(lon, dtype='f8')

    def levels(self):
        """Returns the pressure levels in Pa of the first file."""
        ds = self.datasets[0]
        lname = self._find(ds, ('pressure_level', 'level', 'plev'), self.what)
        lvar = ds.variables[lname]
        levs = np.asarray(lvar[:], dtype='f8')
        units = getattr(lvar, 'units', 'hPa').lower()
        if units in ('hpa', 'millibars', 'mb', 'millibar'):
            levs = levs * 100.
        return levs

    def has(self, names, when):
        return self._locate(names, when) is not None

    def _locate(self, names, when):
        for ds, itime, tname in self.index.get(when, []):
            for name in names:
                if name in ds.variables:
                    return ds, itime, tname, name
        return None

    def read(self, names, when):
        """Reads the field with one of the given names valid at the given
        time. Returns a float64 array of dimensions ([level,] lat, lon), with
        missing values set to NaN, or None if the field is not available."""
        found = self._locate(names, when)
        if found is None:
            return None
        ds, itime, tname, name = found
        var = ds.variables[name]
        # Index the time dimension, and take the first element of any other
        # dimension besides level/latitude/longitude (e.g., number, expver).
        keep = ('pressure_level', 'level', 'plev',
                'latitude', 'lat', 'longitude', 'lon')
        slices = []
        for dim in var.dimensions:
            if dim == tname or (dim == 'time' and tname == 'valid_time'):
                slices.append(itime)
            elif dim in keep:
                slices.append(slice(None))
            else:
                slices.append(0)
        data = var[tuple(slices)]
        return np.ma.filled(np.ma.asarray(data, dtype='f8'), np.nan)

################################################################################
# Physics and remapping

def soil_layer_weights(src=ERA5_SOIL_DEPTHS, dst=NOAH_SOIL_DEPTHS):
    """Returns the matrix w[dst layer, src layer] of thickness-weighted
    averaging weights to remap from the src to the dst soil layers. Each row
    sums to one."""
    nsrc, ndst = len(src)-1, len(dst)-1
    weights = np.zeros((ndst, nsrc))
    for k in range(ndst):
        for m in range(nsrc):
            overlap = min(dst[k+1], src[m+1]) - max(dst[k], src[m])
            weights[k, m] = max(overlap, 0.)
        total = weights[k, :].sum()
        if total <= 0.:
            raise ValueError(f'Target soil layer {k+1} is outside of the '
                             'source soil layers.')
        weights[k, :] /= total
    return weights

def remap_soil(layers, weights):
    """Remaps a list of source soil layer fields onto the target layers."""
    return [sum(weights[k, m]*layers[m] for m in range(len(layers)))
            for k in range(weights.shape[0])]

def spfh_from_dewpoint(dewpoint, pressure):
    """Specific humidity (kg/kg) from dew point (K) and pressure (Pa), with
    the saturation vapor pressure over water from Bolton (1980)."""
    tdc = dewpoint - 273.15
    vapor = 611.2*np.exp(17.67*tdc/(tdc + 243.5))
    vapor = np.minimum(vapor, 0.5*pressure)
    return 0.622*vapor/(pressure - 0.378*vapor)

def surface_fields(sfc, when):
    """Reads the ERA5 single-level fields valid at the given time and derives
    the GFS surface fields. Returns a list of records:
    (name, discipline, category, number, level, nbits, data)."""
    raw = {}
    for key, names in SFC_NAMES.items():
        if key in ('stl', 'swvl'):
            raw[key] = [sfc.read(n, when) for n in names]
            missing = any(x is None for x in raw[key])
        else:
            raw[key] = sfc.read(names, when)
            missing = raw[key] is None
        if missing and key in SFC_REQUIRED:
            raise KeyError(f'Required ERA5 single-level field "{key}" '
                           f'{names} is not available for {when}.')
        if missing:
            raw[key] = None

    land = np.where(np.nan_to_num(raw['lsm']) > 0.5, 1., 0.)

    # Sea ice: missing over land in ERA5.
    if raw['ci'] is None:
        logger.warning('No sea ice cover in input; ICEC is set to zero.')
        icec = np.zeros_like(land)
    else:
        icec = np.clip(np.nan_to_num(raw['ci']), 0., 1.)
    icec = np.where(land > 0.5, 0., icec)

    # Surface temperature: SST at ice-free water points, skin temperature at
    # land, sea-ice and lake (no ERA5 SST) points.
    tsfc = raw['skt'].copy()
    if raw['sst'] is not None:
        water = (land < 0.5) & (icec <= ICE_MIN) & np.isfinite(raw['sst'])
        tsfc[water] = raw['sst'][water]
    else:
        logger.warning('No SST in input; skin temperature is used instead.')

    # Snow: ERA5 snow depth is in m of water equivalent.
    if raw['sd'] is None:
        logger.warning('No snow depth in input; WEASD/SNOD are set to zero.')
        swe = np.zeros_like(land)
    else:
        swe = np.clip(np.nan_to_num(raw['sd']), 0., None)
    glacier = (land > 0.5) & (swe >= GLACIER_SWE)
    weasd = swe*RHO_WATER
    if raw['rsn'] is None:
        density = np.full_like(land, 300.)
    else:
        density = np.clip(np.nan_to_num(raw['rsn'], nan=300.), 50., 917.)
    snod = weasd/density

    # Soil: remap to the Noah layers.
    weights = soil_layer_weights()
    tsoil = remap_soil(raw['stl'], weights)
    soilw = remap_soil(raw['swvl'], weights)
    for k in range(len(soilw)):
        soilw[k] = np.clip(np.nan_to_num(soilw[k]), SOILM_MIN, SOILM_MAX)
        soilw[k][glacier] = SOILM_LANDICE
        tsoil[k] = np.where(np.isfinite(tsoil[k]), tsoil[k], tsfc)

    q2m = spfh_from_dewpoint(np.minimum(raw['d2m'], raw['t2m']), raw['sp'])

    none = (SFC, None, None)
    records = [
        ('PRES',  0, 3,  0, none, 20, raw['sp']),
        ('HGT',   0, 3,  5, none, 20, raw['zsfc']/GRAV),
        ('TMP',   0, 0,  0, none, 16, tsfc),
        ('LAND',  2, 0,  0, none,  8, land),
        ('ICEC', 10, 2,  0, none, 12, icec),
        ('WEASD', 0, 1, 13, none, 16, weasd),
        ('SNOD',  0, 1, 11, none, 16, snod),
        ('TMP',   0, 0,  0, (HAGL, 2., None), 16, raw['t2m']),
        ('SPFH',  0, 1,  0, (HAGL, 2., None), 16, q2m),
    ]
    if raw['u10'] is not None and raw['v10'] is not None:
        records.append(('UGRD', 0, 2, 2, (HAGL, 10., None), 16, raw['u10']))
        records.append(('VGRD', 0, 2, 3, (HAGL, 10., None), 16, raw['v10']))
    if raw['msl'] is not None:
        records.append(('PRMSL', 0, 3, 1, (MSL, None, None), 20, raw['msl']))
    # chgres_cube reads the soil layers in the order found: top to bottom.
    for k in range(len(tsoil)):
        layer = (DBLL, NOAH_SOIL_DEPTHS[k], NOAH_SOIL_DEPTHS[k+1])
        records.append(('TSOIL', 2, 0, 2, layer, 16, tsoil[k]))
    for k in range(len(soilw)):
        layer = (DBLL, NOAH_SOIL_DEPTHS[k], NOAH_SOIL_DEPTHS[k+1])
        records.append(('SOILW', 2, 0, 192, layer, 16, soilw[k]))
    return records

def isobaric_fields(pl, when, levels):
    """Reads the ERA5 pressure-level fields valid at the given time. Returns
    a list of records as surface_fields does."""
    records = []
    for name, era5_names, disc, cat, num, nbits, required in PL_FIELDS:
        data = pl.read(era5_names, when)
        if data is None:
            if required:
                raise KeyError(f'Required ERA5 pressure-level field '
                               f'{era5_names} is not available for {when}.')
            logger.info(f'{name}: not in input; skipped.')
            continue
        if data.ndim != 3 or data.shape[0] != len(levels):
            raise ValueError(f'{name}: unexpected dimensions {data.shape}.')
        if name == 'HGT':
            data = data/GRAV
        elif name not in ('TMP', 'UGRD', 'VGRD', 'VVEL'):
            data = np.clip(data, 0., None)  # mixing ratios
        for ilev, level in enumerate(levels):
            records.append((name, disc, cat, num, (ISBL, level, None),
                            nbits, data[ilev]))
    return records

################################################################################
# GRIB2 output

class Grib2Writer(object):
    """Writes fields on a regular latitude-longitude grid as NCEP GRIB2
    messages (product definition template 4.0)."""

    def __init__(self, lat, lon, cycle, fhr,
                 packing='grid_complex_spatial_differencing'):
        self.packing = packing
        # GFS convention: north to south, starting from the Greenwich meridian.
        self.flip_lat = bool(lat[0] < lat[-1])
        if self.flip_lat:
            lat = lat[::-1]
        lon = np.mod(lon, 360.)
        self.roll_lon = int(np.argmin(lon))
        lon = np.roll(lon, -self.roll_lon)
        if np.any(np.diff(lon) <= 0.) or np.any(np.diff(lat) >= 0.):
            raise ValueError('Unsupported input grid ordering.')
        dlon = float(np.mean(np.diff(lon)))
        dlat = float(-np.mean(np.diff(lat)))
        if abs(dlon*len(lon) - 360.) > 1.e-3:
            raise ValueError('The input grid must be global in longitude; '
                             'chgres_cube expects global GFS files.')
        self.shape = (len(lat), len(lon))

        gid = eccodes.codes_grib_new_from_samples('regular_ll_pl_grib2')
        s = eccodes.codes_set
        s(gid, 'centre', 7)       # NCEP: use the NCEP local GRIB2 tables
        s(gid, 'subCentre', 0)
        s(gid, 'tablesVersion', 2)
        s(gid, 'localTablesVersion', 1)
        s(gid, 'significanceOfReferenceTime', 1)  # start of forecast
        s(gid, 'dataDate', int(cycle.strftime('%Y%m%d')))
        s(gid, 'dataTime', int(cycle.strftime('%H%M')))
        s(gid, 'productionStatusOfProcessedData', 2)  # research products
        s(gid, 'typeOfProcessedData', 0 if fhr == 0 else 1)
        s(gid, 'Ni', len(lon))
        s(gid, 'Nj', len(lat))
        s(gid, 'iScansNegatively', 0)
        s(gid, 'jScansPositively', 0)
        s(gid, 'latitudeOfFirstGridPointInDegrees', float(lat[0]))
        s(gid, 'latitudeOfLastGridPointInDegrees', float(lat[-1]))
        s(gid, 'longitudeOfFirstGridPointInDegrees', float(lon[0]))
        s(gid, 'longitudeOfLastGridPointInDegrees', float(lon[-1]))
        s(gid, 'iDirectionIncrementInDegrees', dlon)
        s(gid, 'jDirectionIncrementInDegrees', dlat)
        s(gid, 'productDefinitionTemplateNumber', 0)
        s(gid, 'typeOfGeneratingProcess', 0 if fhr == 0 else 2)
        s(gid, 'generatingProcessIdentifier', 255)
        s(gid, 'indicatorOfUnitOfTimeRange', 1)  # hours
        s(gid, 'forecastTime', int(fhr))
        self.template = gid

    @staticmethod
    def _set_surface(gid, which, kind, value):
        s = eccodes.codes_set
        s(gid, f'typeOf{which}FixedSurface', kind)
        if value is None:
            eccodes.codes_set_missing(gid, f'scaleFactorOf{which}FixedSurface')
            eccodes.codes_set_missing(gid, f'scaledValueOf{which}FixedSurface')
            return
        # Find the smallest decimal scale factor that represents the value.
        for scale in range(0, 7):
            scaled = value*10**scale
            if abs(scaled - round(scaled)) < 1.e-6*max(1., abs(scaled)):
                break
        s(gid, f'scaleFactorOf{which}FixedSurface', scale)
        s(gid, f'scaledValueOf{which}FixedSurface', int(round(scaled)))

    def write(self, fileobj, record):
        name, disc, cat, num, (kind, first, second), nbits, data = record
        if data.shape != self.shape:
            raise ValueError(f'{name}: field dimensions {data.shape} do not '
                             f'match the grid {self.shape}.')
        if not np.all(np.isfinite(data)):
            raise ValueError(f'{name} (level {first}): field has missing or '
                             'non-finite values.')
        if self.flip_lat:
            data = data[::-1, :]
        if self.roll_lon:
            data = np.roll(data, -self.roll_lon, axis=1)
        gid = eccodes.codes_clone(self.template)
        try:
            s = eccodes.codes_set
            s(gid, 'discipline', disc)
            s(gid, 'parameterCategory', cat)
            s(gid, 'parameterNumber', num)
            self._set_surface(gid, 'First', kind, first)
            if second is None:
                self._set_surface(gid, 'Second', 255, None)
            else:
                self._set_surface(gid, 'Second', kind, second)
            s(gid, 'bitsPerValue', nbits)
            if self.packing != 'grid_simple':
                s(gid, 'packingType', self.packing)
            if self.packing == 'grid_complex_spatial_differencing':
                # Second order, as in the GFS files. This must be set
                # explicitly: the default of zero cannot be decoded.
                s(gid, 'orderOfSpatialDifferencing', 2)
            eccodes.codes_set_values(gid, np.ascontiguousarray(data).ravel())
            eccodes.codes_write(gid, fileobj)
        finally:
            eccodes.codes_release(gid)

    def close(self):
        eccodes.codes_release(self.template)

################################################################################

def parse_fhrs(text):
    """Parses forecast hours given as 'start:end:step' or 'h1,h2,...'."""
    if ':' in text:
        parts = [int(x) for x in text.split(':')]
        if len(parts) == 2:
            parts.append(1)
        if len(parts) != 3 or parts[2] <= 0 or parts[1] < parts[0]:
            raise ValueError(f'Invalid forecast hour range: {text}')
        return list(range(parts[0], parts[1]+1, parts[2]))
    return [int(x) for x in text.split(',')]

def output_path(outdir, cycle, fhr, gfsver='PROD2026'):
    """Returns the name of the 0.25-degree GRIB2 file for the given GFS
    version, as expected by exhafs_atm_ic.sh and exhafs_atm_lbc.sh."""
    cycdir = os.path.join(
        outdir, cycle.strftime('gfs.%Y%m%d'), cycle.strftime('%H'))
    if gfsver == 'PROD2021':
        return os.path.join(
            cycdir, 'atmos',
            cycle.strftime('gfs.t%Hz.pgrb2.0p25.f') + f'{fhr:03d}')
    elif gfsver == 'PROD2026':
        return os.path.join(
            cycdir, 'products', 'atmos', 'grib2', '0p25',
            cycle.strftime('gfs.t%Hz.pres_a.0p25.f') + f'{fhr:03d}.grib2')
    raise ValueError(f'Unsupported GFS version: {gfsver}')

def convert(pl, sfc, cycle, fhr, outdir, packing, force=False,
            gfsver='PROD2026'):
    """Writes the pseudo-GFS GRIB2 file for one cycle and forecast hour.
    Returns the name of the file."""
    when = cycle + datetime.timedelta(hours=fhr)
    filename = output_path(outdir, cycle, fhr, gfsver)
    if os.path.exists(filename) and not force:
        logger.info(f'{filename}: already exists. Skipping.')
        return filename
    for files, kind in ((pl, 'pressure-level'), (sfc, 'single-level')):
        if when not in files.index:
            raise KeyError(f'No ERA5 {kind} data valid at {when:%Y%m%d%H}.')

    lat, lon = pl.grid()
    slat, slon = sfc.grid()
    if lat.shape != slat.shape or lon.shape != slon.shape or \
       np.abs(lat-slat).max() > 1.e-4 or np.abs(lon-slon).max() > 1.e-4:
        raise ValueError('The pressure-level and single-level grids differ.')
    levels = pl.levels()

    records = isobaric_fields(pl, when, levels) + surface_fields(sfc, when)

    writer = Grib2Writer(lat, lon, cycle, fhr, packing)
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    partial = filename + '.partial'
    try:
        with open(partial, 'wb') as fileobj:
            for record in records:
                writer.write(fileobj, record)
        os.replace(partial, filename)
    finally:
        writer.close()
        if os.path.exists(partial):
            os.remove(partial)
    logger.info(f'{filename}: wrote {len(records)} records valid at '
                f'{when:%Y%m%d%H} on {len(levels)} pressure levels.')
    return filename

def main(argv=None):
    parser = argparse.ArgumentParser(
        description='Convert ERA5 NetCDF data into pseudo-GFS 0.25-degree '
        'GRIB2 files for HAFS (ictype/bctype=gfsgrib2_0p25).')
    parser.add_argument('--pl', nargs='+', required=True,
                        help='ERA5 pressure-level NetCDF file(s)')
    parser.add_argument('--sfc', nargs='+', required=True,
                        help='ERA5 single-level NetCDF file(s)')
    parser.add_argument('--cycle', nargs='+', required=True,
                        help='cycle(s) to generate, as YYYYMMDDHH')
    parser.add_argument('--fhrs', default='0:126:3',
                        help='forecast hours, as start:end:step or a comma '
                        'separated list (default: %(default)s)')
    parser.add_argument('--outdir', required=True,
                        help='output root directory (to be used as COMINgfs)')
    parser.add_argument('--gfsver', default='PROD2026',
                        choices=['PROD2021', 'PROD2026'],
                        help='GFS version of the output directory layout and '
                        'file names; it needs to match the GFSVER of the HAFS '
                        'configuration (default: %(default)s)')
    parser.add_argument('--packing',
                        default='grid_complex_spatial_differencing',
                        choices=['grid_complex_spatial_differencing',
                                 'grid_simple'],
                        help='GRIB2 packing type; the default is the one '
                        'used by the GFS files, and grid_simple gives larger '
                        'files (default: %(default)s)')
    parser.add_argument('-f', '--force', action='store_true',
                        help='overwrite existing output files')
    parser.add_argument('-v', '--verbose', action='store_true')
    args = parser.parse_args(argv)

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format='%(asctime)s %(name)s %(levelname)s: %(message)s')

    fhrs = parse_fhrs(args.fhrs)
    cycles = [datetime.datetime.strptime(c, '%Y%m%d%H') for c in args.cycle]
    pl = ERA5Files(args.pl, 'pressure-level')
    sfc = ERA5Files(args.sfc, 'single-level')
    try:
        for cycle in cycles:
            for fhr in fhrs:
                convert(pl, sfc, cycle, fhr, args.outdir, args.packing,
                        args.force, args.gfsver)
    finally:
        pl.close()
        sfc.close()
    return 0

if __name__ == '__main__':
    try:
        sys.exit(main())
    except (KeyError, ValueError, OSError) as e:
        logger.error(f'FATAL ERROR: {e}')
        sys.exit(1)
