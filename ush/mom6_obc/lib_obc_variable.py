#! /usr/bin/env python3

import numpy as np
try:
    import esmpy as ESMF
except ImportError or ModuleNotFoundError:
    import ESMF as ESMF
from scipy import interpolate

import lib_ioncdf as ncdf
import lib_common as lc

class obc_variable():
    ''' A class describing an open boundary condition variable
    on an obc_segment '''

    def __init__(self, segment, variable_name, use_locstream=False, **kwargs):
        ''' constructor of obc_variable : import from segment and adds
        attributes specific to this variable

        *** args :

        * segment : existing obc_segment object

        * variable_name : name of the variable in output file

        *** kwargs (mandatory) :

        * geometry : shape of the output field (line, surface)

        * obctype : radiation, flather,...

        '''

        self.vector = False
        # read args
        self.variable_name = variable_name
        self.items = []
        self.items.append('variable_name')
        # iterate over all attributes of segment and copy them
        self.__dict__.update(segment.__dict__)
        # iterate over all kwargs and store them as attributes for the object
        if kwargs is not None:
            self.__dict__.update(kwargs)
            for key, value in kwargs.items():
                self.items.append(key)

        # boundary geometry
        if self.geometry == 'line':
            self.dimensions_name = ('time', 'ny_' + self.segment_name,
                                    'nx_' + self.segment_name,)
        elif self.geometry == 'surface':
            self.dimensions_name = ('time', 'nz_' + self.segment_name +
                                    '_' + self.variable_name,
                                    'ny_' + self.segment_name,
                                    'nx_' + self.segment_name,)

        # RD : move this
        # default parameters for land extrapolation
        # can be modified by changing the attribute of object
        self.xmsg = -99
        self.guess = 1        # guess = 1 zonal mean
        self.gtype = 1        # cyclic or not
        self.nscan = 1500     # usually much less than this
        self.epsx = 1.e-4     # variable dependent / not with reduced var
        self.relc = 0.6       # relaxation coefficient

        # Create a field on the centers of the grid
        self.use_locstream = use_locstream
        if use_locstream:
            self.field_target = ESMF.Field(self.locstream_target)
        else:
            self.field_target = ESMF.Field(self.grid_target,
                                            staggerloc=ESMF.StaggerLoc.CENTER)
        return None

    def allocate(self):
        #Allocate the output array 
        if self.geometry == 'surface':
            data = np.empty((self.nz, self.ny, self.nx))
        elif self.geometry == 'line':
            data = np.empty((self.ny, self.nx))
        return data
    
    def interpolate_from(self, filename, variable, frame=None,
                         maskfile=None, maskvar=None, missing_value=None,
                         from_global=True, depthname='z', timename='time',
                         coord_names=['lon', 'lat'], x_coords=None,
                         y_coords=None, method='bilinear', interpolator=None,
                         autocrop=True, use_gridspec=False):
        ''' interpolate_from performs a serie of operation :
        * read input data
        * perform extrapolation over land if desired
            * read or create mask if extrapolation
        * call ESMF for regridding

        Optional arguments (=default) :

        * frame=None : time record from input data (e.g. 1,2,..,12) when input
                       file contains more than one record.
        * maskfile=None : to read mask from a file (else uses missing value
                                                    of variable)
        * maskvar=None : if maskfile is defined, we need to provide name of
                         mask array in maskfile
        * missing_value=None : when missing value attribute not defined in
                               input file, this allows to pass it
        * use_locstream=False : interpolate from ESMF grid to ESMF locstream
                                instead of ESMF grid, a bit faster.
                                use only to interpolate to boundary.
        * from_global=True : if input file is global leave to true. If input
                             is regional, set to False.
                             interpolating from a regional extraction can
                             significantly speed up processing.
        '''
        # 1. Create ESMF source grid
        if maskfile is not None:
            self.gridsrc = self.create_source_grid(maskfile,
                                                   from_global,
                                                   coord_names,
                                                   x_coords=x_coords,
                                                   y_coords=y_coords,
                                                   autocrop=autocrop,
                                                   use_gridspec=use_gridspec)
        else:
            self.gridsrc = self.create_source_grid(filename,
                                                   from_global,
                                                   coord_names,
                                                   x_coords=x_coords,
                                                   y_coords=y_coords,
                                                   autocrop=autocrop,
                                                   use_gridspec=use_gridspec)

        # 2. read the original field
        datasrc = ncdf.read_field(filename, variable, frame=frame)
        if self.geometry == 'surface':
            datasrc = datasrc[:, self.jmin_src:self.jmax_src,
                              self.imin_src:self.imax_src]
            self.depth, self.nz, self.dz = ncdf.read_vert_coord(filename,
                                                                 depthname,
                                                                 self.nx,
                                                                 self.ny)
        else:
            datasrc = datasrc[self.jmin_src:self.jmax_src,
                              self.imin_src:self.imax_src]
            self.depth = 0.
            self.nz = 1
            self.dz = 0.
        # read time
        try:
            self.timesrc = ncdf.read_time(filename, timename, frame=frame)
        except:
            print('input data time variable not read')

        dataextrap = datasrc.copy()

        # 4. ESMF interpolation
        # Create a field on the centers of the grid
        field_src = ESMF.Field(self.gridsrc, staggerloc=ESMF.StaggerLoc.CENTER)

        # Set up a regridding object between source and destination
        if interpolator is None:
            if method == 'bilinear':
                regridme = ESMF.Regrid(field_src, self.field_target,
                                        unmapped_action=ESMF.UnmappedAction.IGNORE,
                                        regrid_method=ESMF.RegridMethod.BILINEAR)
            elif method == 'patch':
                regridme = ESMF.Regrid(field_src, self.field_target,
                                        unmapped_action=ESMF.UnmappedAction.IGNORE,
                                        regrid_method=ESMF.RegridMethod.PATCH)
            elif method == 'conserve':
                regridme = ESMF.Regrid(field_src, self.field_target,
                                        unmapped_action=ESMF.UnmappedAction.IGNORE,
                                        regrid_method=ESMF.RegridMethod.CONSERVE)
        else:
            regridme = interpolator

        self.data = self.perform_interpolation(dataextrap, regridme, field_src,
                                               self.field_target, self.grid_target,
                                               self.use_locstream)

        # free memory (ESMPy has memory leak)
        self.gridsrc.destroy()
        field_src.destroy()
        return regridme

    def perform_interpolation(self, dataextrap, regridme, field_src,
                              field_target, grid_target,use_locstream):
                
        if self.orientation == 0: # south segment
            coord_boundary = grid_target.coords[0][0][:,0] # lon at south segment
        if self.orientation == 1: # east segment
            coord_boundary = grid_target.coords[0][1][-1,:] # lat at east segment
        if self.orientation == 2: # north segment
            coord_boundary = grid_target.coords[0][0][:,-1] # lon at north segment
        if self.orientation == 3: # west segment
            coord_boundary = grid_target.coords[0][1][0,:] # lat at west segment

        data = self.allocate()
        if self.geometry == 'surface':
            for kz in np.arange(self.nz):
                field_src.data[:] = dataextrap[kz, :, :].transpose()
                field_src.data[field_src.data == dataextrap.fill_value] = np.nan
                field_target = regridme(field_src, field_target)

                # Interpolating values on land points
                data_with_gaps = field_target.data.copy()
                nogaps = np.isfinite(data_with_gaps)
                nogaps_pos = np.where(nogaps)[0]
                oknans = np.where(np.isnan(data_with_gaps))[0]
                data_interp = np.empty((len(data_with_gaps)))
                data_interp[:] = np.nan
                if len(data_with_gaps[nogaps]) >= 2:
                    if np.min(coord_boundary) <= np.min(coord_boundary[nogaps]):
                        interp = interpolate.interp1d(coord_boundary[nogaps],data_with_gaps[nogaps],kind='nearest')
                        data_interp[nogaps_pos[0]:nogaps_pos[-1]+1] = interp(coord_boundary[nogaps_pos[0]:nogaps_pos[-1]+1])
                        data_interp[0:nogaps_pos[0]] = data_interp[nogaps_pos[0]]
                        data_interp[nogaps_pos[-1]:] = data_interp[nogaps_pos[-1]]
                    else:
                        interp = interpolate.interp1d(coord_boundary[nogaps],data_with_gaps[nogaps],kind='nearest')
                        data_interp = interp(coord_boundary)
                else:
                    if len(data_with_gaps[nogaps]) == 1:
                        data_interp[:] = data_with_gaps[nogaps]
                    else:
                        if self.variable_name == 'salt':
                            data_interp[:] = 35.0
                        else:
                            data_interp[:] = 0.0

                if use_locstream:
                    if self.nx == 1:
                        data[kz, :, 0] = data_interp.copy()
                    elif self.ny == 1:
                        data[kz, 0, :] = data_interp.copy()
                else:
                    data[kz, :, :] = data_interp.transpose()[self.jmin:self.jmax+1,
                                                                   self.imin:self.imax+1]
        elif self.geometry == 'line':
            field_src.data[:] = dataextrap[:, :].transpose()
            field_src.data[field_src.data == dataextrap.fill_value] = np.nan
            field_target = regridme(field_src, field_target)

            # Interpolating values on land points
            data_with_gaps = field_target.data.copy()
            nogaps = np.isfinite(data_with_gaps)
            nogaps_pos = np.where(nogaps)[0]
            oknans = np.where(np.isnan(data_with_gaps))[0]
            data_interp = np.empty((len(data_with_gaps)))
            data_interp[:] = np.nan
            if len(data_with_gaps[nogaps]) >= 2:
                if np.min(coord_boundary) <= np.min(coord_boundary[nogaps]):
                    interp = interpolate.interp1d(coord_boundary[nogaps],data_with_gaps[nogaps],kind='nearest')
                    data_interp[nogaps_pos[0]:nogaps_pos[-1]+1] = interp(coord_boundary[nogaps_pos[0]:nogaps_pos[-1]+1])
                    data_interp[0:nogaps_pos[0]] = data_interp[nogaps_pos[0]]
                    data_interp[nogaps_pos[-1]:] = data_interp[nogaps_pos[-1]]
                else:
                    interp = interpolate.interp1d(coord_boundary[nogaps],data_with_gaps[nogaps],kind='nearest')
                    data_interp = interp(coord_boundary)
            else:
                if len(data_with_gaps[nogaps]) == 1:
                    data_interp[:] = data_with_gaps[nogaps]
                else:
                    data_interp[:] = 0.0

            if use_locstream:
                data[:, :] = np.reshape(data_interp.transpose(),
                                         (self.ny, self.nx))
            else:
                data[:, :] = data_interp.transpose()[self.jmin:self.jmax+1,
                                                           self.imin:self.imax+1]
        return data

    def create_source_grid(self, filename, from_global, coord_names,
                           x_coords=None, y_coords=None, autocrop=True,
                           use_gridspec=False):
        # create ESMF grid object for source grid
        # new way to create source grid
        # TO DO : move into separate function, has to be called before drown
        # so that we know the periodicity

        if use_gridspec:
            gridsrc = ESMF.Grid(filename=filename, filetype=ESMF.FileFormat.GRIDSPEC,
                                 is_sphere=from_global, coord_names=coord_names,
                                 add_corner_stagger=True)
            self.imin_src = 0
            self.imax_src = gridsrc.coords[0][0].shape[0]
            self.jmin_src = 0
            self.jmax_src = gridsrc.coords[0][0].shape[1]
            if from_global and not autocrop:
                self.gtype = 1  # 1 = periodic for drown NCL
                self.kew = 0  # 0 = periodic for drown sosie
            else:
                self.gtype = 0  # 1 = non periodic for drown NCL
                self.kew = -1  # -1 = non periodic for drown sosie
        else:
            # Allow to provide lon/lat from existing array
            if x_coords is not None and y_coords is not None:
                lon_src = x_coords
                lat_src = y_coords
            else:
                lons = ncdf.read_field(filename, coord_names[0])
                lats = ncdf.read_field(filename, coord_names[1])
                if len(lons.shape) == 1:
                    lon_src, lat_src = np.meshgrid(lons, lats)
                else:
                    lon_src = lons
                    lat_src = lats

            # autocrop
            if autocrop:
                self.imin_src, self.imax_src, self.jmin_src, self.jmax_src = \
                    lc.find_subset(self.grid_target, lon_src, lat_src)
                lon_src = lon_src[self.jmin_src:self.jmax_src,
                                  self.imin_src:self.imax_src]
                lat_src = lat_src[self.jmin_src:self.jmax_src,
                                  self.imin_src:self.imax_src]

            ny_src, nx_src = lon_src.shape
            if not autocrop:
                self.imin_src = 0
                self.imax_src = nx_src
                self.jmin_src = 0
                self.jmax_src = ny_src

            if from_global and not autocrop:
                gridsrc = ESMF.Grid(np.array([nx_src, ny_src]),
                                     num_peri_dims=1)
                self.gtype = 1  # 1 = periodic for drown NCL
                self.kew = 0  # 0 = periodic for drown sosie
            else:
                gridsrc = ESMF.Grid(np.array([nx_src, ny_src]))
                self.gtype = 0  # 1 = non periodic for drown NCL
                self.kew = -1  # -1 = non periodic for drown sosie

            gridsrc.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])
            gridsrc.coords[ESMF.StaggerLoc.CENTER][0][:] = lon_src.T
            gridsrc.coords[ESMF.StaggerLoc.CENTER][1][:] = lat_src.T

        return gridsrc
