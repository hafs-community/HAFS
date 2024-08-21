#! /usr/bin/env python3

import netCDF4 as nc
import numpy as np

import lib_timemanager as tim
import lib_common as lc

# -----------------------------------------------------------------------------
# writing functions
# -----------------------------------------------------------------------------


def write_obc_file(list_segments, list_variables, list_vectvariables,
                   time_point, output='out.nc'):
    ''' write an open boundary condition file from a list
    of segments and associated variables '''

    # open file in write mode
    fid = nc.Dataset(output, 'w', format='NETCDF3_64BIT_OFFSET')
    fid.description = ''

    # define the list of dimensions we need to write
    fid.createDimension('time', None)

    # dimensions
    for segment in list_segments:
        xdimnam = 'nx_' + segment.segment_name
        ydimnam = 'ny_' + segment.segment_name
        fid.createDimension(xdimnam, segment.nx)
        fid.createDimension(ydimnam, segment.ny)
        xdimv = fid.createVariable(xdimnam, 'i4', (xdimnam))
        ydimv = fid.createVariable(ydimnam, 'i4', (ydimnam))
        xdimv.cartesian_axis = 'X'
        ydimv.cartesian_axis = 'Y'
        xdimv[:] = np.arange(0, segment.nx)
        ydimv[:] = np.arange(0, segment.ny)
    for variable in list_variables:
        if (variable.geometry == 'surface'):
            zdimnam = 'nz_' + variable.segment_name + '_' + \
                      variable.variable_name
            fid.createDimension(zdimnam, variable.nz)
            zdimv = fid.createVariable(zdimnam, 'i4', (zdimnam))
            zdimv.cartesian_axis = 'Z'
            zdimv[:] = np.arange(0, variable.nz)

    for variable in list_vectvariables:
        if (variable.geometry == 'surface'):
            zdimnamu = 'nz_' + variable.segment_name + '_' + \
                       variable.variable_name_u
            zdimnamv = 'nz_' + variable.segment_name + '_' + \
                       variable.variable_name_v
            fid.createDimension(zdimnamu, variable.nz)
            zdimv = fid.createVariable(zdimnamu, 'i4', (zdimnamu))
            zdimv.cartesian_axis = 'Z'
            zdimv[:] = np.arange(0, variable.nz)
            fid.createDimension(zdimnamv, variable.nz)
            zdimv = fid.createVariable(zdimnamv, 'i4', (zdimnamv))
            zdimv.cartesian_axis = 'Z'
            zdimv[:] = np.arange(0, variable.nz)

    # define time and coordinates
    nctime = fid.createVariable('time', 'f8', ('time',))
    nctime.units = time_point.units
    nctime.calendar = time_point.calendar

    ncsegments_lon = []
    ncsegments_lat = []
    ncsegments_ilist = []
    ncsegments_jlist = []

    for segment in list_segments:
        ncseg_lon = fid.createVariable('lon_' + segment.segment_name,
                                       'f8', segment.hdimensions_name)
        ncseg_lat = fid.createVariable('lat_' + segment.segment_name,
                                       'f8', segment.hdimensions_name)
        ncsegments_lon.append(ncseg_lon)
        ncsegments_lat.append(ncseg_lat)
        ncseg_ilist = fid.createVariable('ilist_' + segment.segment_name,
                                         'f8', segment.hdimensions_name)
        ncseg_jlist = fid.createVariable('jlist_' + segment.segment_name,
                                         'f8', segment.hdimensions_name)
        ncsegments_ilist.append(ncseg_ilist)
        ncsegments_jlist.append(ncseg_jlist)

    # define variables
    ncvariables = []
    ncvariables_vc = []
    ncvariables_dz = []
    list_variables_with_vc = []

    for variable in list_variables:
        ncvar = fid.createVariable(variable.variable_name + '_' +
                                   variable.segment_name, 'f8',
                                   variable.dimensions_name)
        ncvariables.append(ncvar)
        if variable.geometry == 'surface':
            list_variables_with_vc.append(variable)
            ncvar_vc = fid.createVariable('vc_' + variable.variable_name +
                                          '_' + variable.segment_name, 'f8',
                                          variable.dimensions_name)
            ncvariables_vc.append(ncvar_vc)
            ncvar_dz = fid.createVariable('dz_' + variable.variable_name +
                                          '_' + variable.segment_name, 'f8',
                                          variable.dimensions_name)
            ncvariables_dz.append(ncvar_dz)

    # define variables
    ncvectvariables = []
    ncvectvariables_vc = []
    ncvectvariables_dz = []
    list_vectvariables_with_vc = []

    for variable in list_vectvariables:
        ncvar = fid.createVariable(variable.variable_name_u + '_' +
                                   variable.segment_name, 'f8',
                                   variable.dimensions_name_u)
        ncvectvariables.append(ncvar)
        ncvar = fid.createVariable(variable.variable_name_v + '_' +
                                   variable.segment_name, 'f8',
                                   variable.dimensions_name_v)
        ncvectvariables.append(ncvar)

        if variable.geometry == 'surface':
            list_vectvariables_with_vc.append(variable)

            ncvar_vc = fid.createVariable('vc_' + variable.variable_name_u +
                                          '_' + variable.segment_name, 'f8',
                                          variable.dimensions_name_u)
            ncvectvariables_vc.append(ncvar_vc)
            ncvar_vc = fid.createVariable('vc_' + variable.variable_name_v +
                                          '_' + variable.segment_name, 'f8',
                                          variable.dimensions_name_v)
            ncvectvariables_vc.append(ncvar_vc)

            ncvar_dz = fid.createVariable('dz_' + variable.variable_name_u +
                                          '_' + variable.segment_name, 'f8',
                                          variable.dimensions_name_u)
            ncvectvariables_dz.append(ncvar_dz)
            ncvar_dz = fid.createVariable('dz_' + variable.variable_name_v +
                                          '_' + variable.segment_name, 'f8',
                                          variable.dimensions_name_v)
            ncvectvariables_dz.append(ncvar_dz)

    # fill time and coordinates
    nctime[:] = time_point.data

    for nseg in np.arange(len(list_segments)):
        ncsegments_lon[nseg][:] = list_segments[nseg].lon
        ncsegments_lat[nseg][:] = list_segments[nseg].lat
        ncsegments_ilist[nseg][:] = list_segments[nseg].ilist
        ncsegments_ilist[nseg].orientation = list_segments[nseg].orientation
        ncsegments_jlist[nseg][:] = list_segments[nseg].jlist
        ncsegments_jlist[nseg].orientation = list_segments[nseg].orientation

    # fill variables
    for nvar in np.arange(len(list_variables)):
        ncvariables[nvar][0, :] = list_variables[nvar].data

    for nvar in np.arange(len(list_variables_with_vc)):
        ncvariables_vc[nvar][0, :] = list_variables_with_vc[nvar].depth
        ncvariables_dz[nvar][0, :] = list_variables_with_vc[nvar].dz

    # fill vect variables
    for nvar in np.arange(len(list_vectvariables)):
        ncvectvariables[2*nvar][0, :] = list_vectvariables[nvar].data_u_out
        ncvectvariables[2*nvar+1][0, :] = list_vectvariables[nvar].data_v_out

    for nvar in np.arange(len(list_vectvariables_with_vc)):
        ncvectvariables_vc[2*nvar][0, :] = \
            list_vectvariables_with_vc[nvar].depth
        ncvectvariables_vc[2*nvar+1][0, :] = \
            list_vectvariables_with_vc[nvar].depth
        ncvectvariables_dz[2*nvar][0, :] = list_vectvariables_with_vc[nvar].dz
        ncvectvariables_dz[2*nvar+1][0, :] = \
            list_vectvariables_with_vc[nvar].dz

    # close file
    fid.close()

# -----------------------------------------------------------------------------
# reading functions
# -----------------------------------------------------------------------------

def read_field(file_name, variable_name, frame=None):
    fid = nc.Dataset(file_name, 'r')
    if frame is not None:
        out = fid.variables[variable_name][frame, :].squeeze()
    else:
        out = fid.variables[variable_name][:].squeeze()
    fid.close()
    return out

def read_vert_coord(file_name, vc_name, nx, ny):
    ''' read the vertical coordinate (vc) and reshape it if needed :
    vertical coordinate can be either a function of z or (x,y,z)
    '''
    vc_in = read_field(file_name, vc_name)
    nz = vc_in.shape[0]
    if len(vc_in.shape) == 1:
        vc = np.empty((nz, ny, nx))
        for kx in np.arange(nx):
            for ky in np.arange(ny):
                vc[:, ky, kx] = vc_in
    else:
        vc = vc_in
    # compute layer thickness
    dz = np.empty((nz, ny, nx))
    dz[:-1, :, :] = vc[1:, :, :] - vc[:-1, :, :]
    # test if bounds exist first (to do), else
    dz[-1, :, :] = dz[-2, :, :]
    return vc, nz, dz

def read_time(file_name, time_name='time', frame=0):
    ''' read time from the input file '''
    otime = tim.timeobject()
    fid = nc.Dataset(file_name, 'r')
    otime.data = fid.variables[time_name][frame]
    otime.units = fid.variables[time_name].units
    fid.close()
    return otime
