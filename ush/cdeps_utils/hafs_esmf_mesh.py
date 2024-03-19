#!/usr/bin/env python3
################################################################################
# Script Name: hafs_esmf_mesh.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script generates ESMF mesh file.
################################################################################
import os, sys, getopt
import argparse
try:
    import numpy as np
    import xarray as xr
    import dask.array as da
    import dask.dataframe as dd
    from dask.diagnostics import ProgressBar
    from datetime import datetime
    import pandas as pd
except ImportError as ie:
    sys.stderr.write("""You are missing some modules!
The following commands can be used to install required Python modules to run this script

    pip install xarray --user
    pip install dask --user
    pip install "dask[array]" --upgrade --user
    pip install "dask[dataframe]" --upgrade --user
""")
    sys.stderr.write(str(ie))
    exit(2)


def calculate_corners(center_lat, center_lon):
    """Calculate corner coordinates by averaging neighbor cells
    """

    # get rank
    rank = len(center_lat.dims)

    if rank == 1:
        # get dimensions
        nlon = center_lon.size
        nlat = center_lat.size

        # convert center points from 1d to 2d
        center_lat2d = da.broadcast_to(center_lat.values[None,:], (nlon, nlat))
        center_lon2d = da.broadcast_to(center_lon.values[:,None], (nlon, nlat))
    elif rank == 2:
        # get dimensions
        dims = center_lon.shape
        nlon = dims[0]
        nlat = dims[1]

        # just rename and convert to dask array
        center_lat2d = da.from_array(center_lat)
        center_lon2d = da.from_array(center_lon)
    else:
        print('Unrecognized grid! The rank of coordinate variables can be 1 or 2 but it is {}.'.format(rank))
        sys.exit(2)

    # calculate corner coordinates for latitude, counterclockwise order, imposing Fortran ordering
    center_lat2d_ext = da.from_array(np.pad(center_lat2d.compute(), (1,1),  mode='reflect', reflect_type='odd'))

    ur = (center_lat2d_ext[1:-1,1:-1]+
          center_lat2d_ext[0:-2,1:-1]+
          center_lat2d_ext[1:-1,2:]+
          center_lat2d_ext[0:-2,2:])/4.0
    ul = (center_lat2d_ext[1:-1,1:-1]+
          center_lat2d_ext[0:-2,1:-1]+
          center_lat2d_ext[1:-1,0:-2]+
          center_lat2d_ext[0:-2,0:-2])/4.0
    ll = (center_lat2d_ext[1:-1,1:-1]+
          center_lat2d_ext[1:-1,0:-2]+
          center_lat2d_ext[2:,1:-1]+
          center_lat2d_ext[2:,0:-2])/4.0
    lr = (center_lat2d_ext[1:-1,1:-1]+
          center_lat2d_ext[1:-1,2:]+
          center_lat2d_ext[2:,1:-1]+
          center_lat2d_ext[2:,2:])/4.0

    # this looks clockwise ordering but it is transposed and becomes counterclockwise, bit-to-bit with NCL
    corner_lat = da.stack([ul.T.reshape((-1,)).T, ll.T.reshape((-1,)).T, lr.T.reshape((-1,)).T, ur.T.reshape((-1,)).T], axis=1)

    # calculate corner coordinates for longitude, counterclockwise order, imposing Fortran ordering
    center_lon2d_ext = da.from_array(np.pad(center_lon2d.compute(), (1,1),  mode='reflect', reflect_type='odd'))

    ur = (center_lon2d_ext[1:-1,1:-1]+
          center_lon2d_ext[0:-2,1:-1]+
          center_lon2d_ext[1:-1,2:]+
          center_lon2d_ext[0:-2,2:])/4.0
    ul = (center_lon2d_ext[1:-1,1:-1]+
          center_lon2d_ext[0:-2,1:-1]+
          center_lon2d_ext[1:-1,0:-2]+
          center_lon2d_ext[0:-2,0:-2])/4.0
    ll = (center_lon2d_ext[1:-1,1:-1]+
          center_lon2d_ext[1:-1,0:-2]+
          center_lon2d_ext[2:,1:-1]+
          center_lon2d_ext[2:,0:-2])/4.0
    lr = (center_lon2d_ext[1:-1,1:-1]+
          center_lon2d_ext[1:-1,2:]+
          center_lon2d_ext[2:,1:-1]+
          center_lon2d_ext[2:,2:])/4.0

    # this looks clockwise ordering but it is transposed and becomes counterclockwise, bit-to-bit with NCL
    corner_lon = da.stack([ul.T.reshape((-1,)).T, ll.T.reshape((-1,)).T, lr.T.reshape((-1,)).T, ur.T.reshape((-1,)).T], axis=1)

    return center_lat2d, center_lon2d, corner_lat, corner_lon

def write_to_esmf_mesh(filename, center_lat, center_lon, corner_lat, corner_lon, mask, area=None):
    """
    Writes ESMF Mesh to file
    dask array doesn't support order='F' for Fortran-contiguous (row-major) order
    the workaround is to arr.T.reshape.T
    """
    # create array with unique coordinate pairs
    # remove coordinates that are shared between the elements
    corner_pair = da.stack([corner_lon.T.reshape((-1,)).T, corner_lat.T.reshape((-1,)).T], axis=1)

    # REPLACED: corner_pair_uniq = dd.from_dask_array(corner_pair).drop_duplicates().to_dask_array(lengths=True)
    # following reduces memory by %17
    corner_pair_uniq = dd.from_dask_array(corner_pair).drop_duplicates().values
    corner_pair_uniq.compute_chunk_sizes()

    # check size of unique coordinate pairs
    dims = mask.shape
    nlon = dims[0]
    nlat = dims[1]
    elem_conn_size = nlon*nlat+nlon+nlat+1
    if corner_pair_uniq.shape[0] != elem_conn_size:
        print('The size of unique coordinate pairs is {} but expected size is {}!'.format(corner_pair_uniq.shape[0], elem_conn_size))
        print('Please check the input file or try to force double precision with --double option. Exiting ...')
        sys.exit(2)

    # create element connections
    corners = dd.concat([dd.from_dask_array(c) for c in [corner_lon.T.reshape((-1,)).T, corner_lat.T.reshape((-1,)).T]], axis=1)
    corners.columns = ['lon', 'lat']
    elem_conn = corners.compute().groupby(['lon','lat'], sort=False).ngroup()+1
    elem_conn = da.from_array(elem_conn.to_numpy())

    # create new dataset for output
    out = xr.Dataset()

    out['origGridDims'] = xr.DataArray(np.array(center_lon.shape, dtype=np.int32),
                                       dims=('origGridRank'))

    out['nodeCoords'] = xr.DataArray(corner_pair_uniq,
                                     dims=('nodeCount', 'coordDim'),
                                     attrs={'units': 'degrees'})

    out['elementConn'] = xr.DataArray(elem_conn.T.reshape((4,-1)).T,
                                      dims=('elementCount', 'maxNodePElement'),
     		                      attrs={'long_name': 'Node indices that define the element connectivity'})
    out.elementConn.encoding = {'dtype': np.int32}

    out['numElementConn'] = xr.DataArray(4*np.ones(center_lon.size, dtype=np.int32),
                                         dims=('elementCount'),
                                         attrs={'long_name': 'Number of nodes per element'})

    out['centerCoords'] = xr.DataArray(da.stack([center_lon.T.reshape((-1,)).T,
                                                 center_lat.T.reshape((-1,)).T], axis=1),
                                          dims=('elementCount', 'coordDim'),
                                          attrs={'units': 'degrees'})

    # add area if it is available
    if area:
        out['elementArea'] = xr.DataArray(area.T.reshape((-1,)).T,
                                          dims=('elementCount'),
                                          attrs={'units': 'radians^2',
                                                 'long_name': 'area weights'})

    # add mask
    out['elementMask'] = xr.DataArray(mask.T.reshape((-1,)).T,
                                      dims=('elementCount'),
                                      attrs={'units': 'unitless'})
    out.elementMask.encoding = {'dtype': np.int32}

    # force no '_FillValue' if not specified
    for v in out.variables:
        if '_FillValue' not in out[v].encoding:
            out[v].encoding['_FillValue'] = None

    # add global attributes
    out.attrs = {'title': 'ESMF unstructured grid file for rectangular grid with {} dimension'.format('x'.join(list(map(str,center_lat.shape)))),
                 'created_by': os.path.basename(__file__),
                 'date_created': '{}'.format(datetime.now()),
                 'conventions': 'ESMFMESH',
                }

    # write output file
    if filename is not None:
        print('Writing {} ...'.format(filename))
        out.to_netcdf(filename)

def write_to_scrip(filename, center_lat, center_lon, corner_lat, corner_lon, mask, area=None):
    """
    Writes SCRIP grid definition to file
    dask array doesn't support order='F' for Fortran-contiguous (row-major) order
    the workaround is to arr.T.reshape.T
    """
    # create new dataset for output
    out = xr.Dataset()

    out['grid_dims'] = xr.DataArray(np.array(center_lat.shape, dtype=np.int32),
                                    dims=('grid_rank',))
    out.grid_dims.encoding = {'dtype': np.int32}

    out['grid_center_lat'] = xr.DataArray(center_lat.T.reshape((-1,)).T,
                                          dims=('grid_size'),
                                          attrs={'units': 'degrees'})

    out['grid_center_lon'] = xr.DataArray(center_lon.T.reshape((-1,)).T,
                                          dims=('grid_size'),
                                          attrs={'units': 'degrees'})

    out['grid_corner_lat'] = xr.DataArray(corner_lat.T.reshape((4, -1)).T,
                                          dims=('grid_size','grid_corners'),
                                          attrs={'units': 'degrees'})

    out['grid_corner_lon'] = xr.DataArray(corner_lon.T.reshape((4, -1)).T,
                                          dims=('grid_size','grid_corners'),
                                          attrs={'units': 'degrees'})

    # include area if it is available
    if area:
        out['grid_area'] = xr.DataArray(area.T.reshape((-1,)).T,
                                        dims=('grid_size'),
                                        attrs={'units': 'radians^2',
                                               'long_name': 'area weights'})

    out['grid_imask'] = xr.DataArray(mask.T.reshape((-1,)).T,
                                     dims=('grid_size'),
                                     attrs={'units': 'unitless'})
    out.grid_imask.encoding = {'dtype': np.int32}

    # force no '_FillValue' if not specified
    for v in out.variables:
        if '_FillValue' not in out[v].encoding:
            out[v].encoding['_FillValue'] = None

    # add global attributes
    out.attrs = {'title': 'Rectangular grid with {} dimension'.format('x'.join(list(map(str,center_lat.shape)))),
                 'created_by': os.path.basename(__file__),
                 'date_created': '{}'.format(datetime.now()),
                 'conventions': 'SCRIP',
                }

    # write output file
    if filename is not None:
        print('Writing {} ...'.format(filename))
        out.to_netcdf(filename)


def file_type(x):
    if x.lower() == 'scrip' or x.lower() == 'esmf':
       return x
    else:
        raise argparse.ArgumentTypeError('SCRIP or ESMF value expected for output type.')

#@profile
def main(argv):
    """
    Main driver to calculate and write SCRIP and ESMF formatted grid represenation
    """
    # set defaults for command line arguments
    ifile = ''
    ofile = ''
    oformat = 'ESMF'
    overwrite = False
    flip  = False
    latrev = False
    latvar = 'lat'
    lonvar = 'lon'
    maskvar = 'mask'
    maskcal = False
    addarea = False
    double = False

    # read command line arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--ifile'    , help='Input grid file name', required=True)
    parser.add_argument('--ofile'    , help='Output file name', required=True)
    parser.add_argument('--oformat'  , help='Output data format [SCRIP, ESMF], defaults to ESMF', required=False, type=file_type, nargs='?', const='ESMF')
    parser.add_argument('--overwrite', help='Overwrites output file, defaults to not', required=False, action='store_true')
    parser.add_argument('--flip'     , help='Flip mask values. SCRIP requires 0/land and 1/ocean', required=False, action='store_true')
    parser.add_argument('--latrev'   , help='Reverse latitude axis', required=False, action='store_true')
    parser.add_argument('--latvar'   , help='Name of latitude variable, defults to ''lat''', required=False, nargs='?', const='lat')
    parser.add_argument('--lonvar'   , help='Name of longitude variable, defaults to ''lon''', nargs='?', const='lon')
    parser.add_argument('--maskvar'  , help='Name of mask variable, defaults to ''mask''', nargs='?', const='mask')
    parser.add_argument('--maskcal'  , help='Calculate mask using fill value from variable defined in maskvar - 0/land and 1/ocean', required=False, action='store_true')
    parser.add_argument('--addarea'  , help='Add area field to output file, defaults to not', required=False, action='store_true')
    parser.add_argument('--double'   , help='Double precision output, defaults to float', required=False, action='store_true')
    args = parser.parse_args()

    if args.ifile:
        ifile = args.ifile
    if args.ofile:
        ofile = args.ofile
    if args.oformat:
        oformat = args.oformat
    if args.overwrite:
        overwrite = args.overwrite
    if args.flip:
        flip = args.flip
    if args.latrev:
        latrev = args.latrev
    if args.latvar:
        latvar = args.latvar
    if args.lonvar:
        lonvar = args.lonvar
    if args.maskvar:
        maskvar = args.maskvar
    if args.maskcal:
        maskcal = args.maskcal
        if not args.maskvar:
            print('maskcal argument requires maskvar to calculate mask! exiting ...')
            sys.exit()
    if args.addarea:
        addarea = args.addarea
    if args.double:
        double = args.double

    # print out configuration
    print("Configuration:")
    print("ifile     = {}".format(ifile))
    print("ofile     = {}".format(ofile))
    print("oformat   = {}".format(oformat))
    print("overwrite = {}".format(overwrite))
    print("flip      = {}".format(flip))
    print("latrev    = {}".format(latrev))
    print("latvar    = {}".format(latvar))
    print("lonvar    = {}".format(lonvar))
    print("maskvar   = {}".format(maskvar))
    print("maskcal   = {} ({})".format(maskcal, maskvar))
    print("addarea   = {}".format(addarea))
    print("double    = {}".format(double))

    # open file, transpose() fixes dimension ordering and mimic Fortran
    if os.path.isfile(ifile):
        ds = xr.open_dataset(ifile, mask_and_scale=False, decode_times=False).transpose()
    else:
        print('Input file could not find!')
        sys.exit(2)

    # check output file
    if overwrite:
        if os.path.isfile(ofile):
            print('Removing existing output file {}.'.format(ofile))
            os.remove(ofile)
    else:
        if os.path.isfile(ofile):
            print('Output file exists. Please provide --overwrite flag.')
            sys.exit(2)

    # check coordinate variables
    if latvar not in ds.coords and latvar not in ds.data_vars:
        print('Input file does not have variable named {}.'.format(latvar))
        print('File has following {}'.format(ds.coords))
        print('File has following {}'.format(ds.data_vars))
        sys.exit(2)

    if lonvar not in ds.coords and lonvar not in ds.data_vars:
        print('Input file does not have variable named {}.'.format(latvar))
        print('File has following {}'.format(ds.coords))
        print('File has following {}'.format(ds.data_vars))
        sys.exit(2)

    # remove time dimension from coordinate variables
    hasTime = 'time' in ds[latvar].dims
    if hasTime:
        lat = ds[latvar][:,:,0]
    else:
        lat = ds[latvar]

    hasTime = 'time' in ds[lonvar].dims
    if hasTime:
        lon = ds[lonvar][:,:,0]
    else:
        lon = ds[lonvar]

    # reverse latitude dimension
    if latrev:
        lat_name = [x for x in lat.coords.dims if 'lat' in x]
        if lat_name:
            lat = lat.reindex({lat_name[0]: list(reversed(lat[lat_name[0]]))})

    # remove time dimension from mask variable and optionally flip mask values
    # this will also create artifical mask variable with all ones, if it is required
    if maskvar in ds.data_vars:
        print('Using mask values from the file.')

        # check mask has time dimension or not
        hasTime = 'time' in ds[maskvar].dims
        if hasTime:
            mask = ds[maskvar][:,:,0]
        else:
            mask = ds[maskvar][:]

        # use variable to construct mask information
        if maskcal:
            fill_value = None
            if '_FillValue' in mask.attrs:
                fill_value = mask._FillValue
            elif 'missing_value' in mask.attrs:
                fill_value = mask.missing_value

            if fill_value:
                mask = da.from_array(xr.where(mask == fill_value, 0, 1).astype(dtype=np.int8))
    else:
        print('Using artifical generated mask values, that are ones in everywhere.')
        if len(lat.dims) == 1:
          mask = da.from_array(np.ones((next(iter(lon.sizes.values())), next(iter(lat.sizes.values()))), dtype=np.int8))
        else:
          mask = da.from_array(np.ones(tuple(lat.sizes.values()), dtype=np.int8))

    # flip mask values
    if flip:
        print('Flipping mask values to 0 for land and 1 for ocean')
        mask = xr.where(mask > 0, 0, 1)

    # calculate corner coordinates, center coordinates are converted to 2d if it is 1d
    if double:
        center_lat, center_lon, corner_lat, corner_lon = calculate_corners(lat.astype(np.float64, copy=False), lon.astype(np.float64, copy=False))
    else:
        center_lat, center_lon, corner_lat, corner_lon = calculate_corners(lat, lon)

    # TODO: add support to calculate area
    if addarea:
        print('The area calculation is not supported! --addarea is reserved for future use.')

    # create output file
    if oformat.lower() == 'scrip':
        write_to_scrip(ofile, center_lat, center_lon, corner_lat, corner_lon, mask)
    else:
        write_to_esmf_mesh(ofile, center_lat, center_lon, corner_lat, corner_lon, mask)

if __name__== "__main__":
	main(sys.argv[1:])
