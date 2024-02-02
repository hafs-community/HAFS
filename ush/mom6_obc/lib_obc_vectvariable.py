#! /usr/bin/env python3

import numpy as np
try:
    import esmpy as ESMF
except ImportError or ModuleNotFoundError:
    import ESMF as ESMF
from scipy import interpolate

import lib_ioncdf as ncdf
import lib_common as lc


class obc_vectvariable():
    #A class describing an open boundary condition vector variable
    #on an obc_segment

    def __init__(self, segment, variable_name_u, variable_name_v,
                 use_locstream=False, **kwargs):
        ''' constructor of obc_variable : import from segment and adds attributes
        specific to this variable

        *** args :

        * segment : existing obc_segment object

        * variable_name_u : name of the zonal component in output file

        * variable_name_v : name of the meridional component in output file

        *** kwargs (mandatory) :

        * geometry : shape of the output field (line, surface)

        * obctype : radiation, flather,...

        '''

        self.vector = True
        # read args
        self.variable_name_u = variable_name_u
        self.variable_name_v = variable_name_v
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
            self.dimensions_name_u = ('time', 'ny_' + self.segment_name,
                                      'nx_' + self.segment_name,)
            self.dimensions_name_v = ('time', 'ny_' + self.segment_name,
                                      'nx_' + self.segment_name,)
        elif self.geometry == 'surface':
            self.dimensions_name_u = ('time', 'nz_' + self.segment_name +
                                      '_' + self.variable_name_u,
                                      'ny_' + self.segment_name,
                                      'nx_' + self.segment_name,)
            self.dimensions_name_v = ('time', 'nz_' + self.segment_name +
                                      '_' + self.variable_name_v,
                                      'ny_' + self.segment_name,
                                      'nx_' + self.segment_name,)

        # default parameters for land extrapolation
        # can be modified by changing the attribute of object
        self.xmsg = -99
        self.guess = 1              # guess = 1 zonal mean
        self.gtype = 1              # cyclic or not
        self.nscan = 1500           # usually much less than this
        self.epsx = 1.e-4           # variable dependent / not with reduced var
        self.relc = 0.6             # relaxation coefficient

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

    def interpolate_from(self, filename, variable_u, variable_v, frame=None,
                         maskfile=None, maskvar=None,
                         missing_value=None, from_global=True, depthname='z',
                         timename='time', coord_names_u=['lon', 'lat'],
                         coord_names_v=['lon', 'lat'], x_coords_u=None,
                         y_coords_u=None, x_coords_v=None, y_coords_v=None,
                         method='bilinear', interpolator_u=None,
                         interpolator_v=None, autocrop=True):
        ''' interpolate_from performs a serie of operation :
        * read input data
        * perform extrapolation over land if desired
            * read or create mask if extrapolation
        * call ESMF for regridding

        Optional arguments (=default) :

        * frame=None : time record from input data (e.g. 1,2,..,12) when input
                       file contains more than one record.
        * drown=True : perform extrapolation of ocean values onto land
        * maskfile=None : to read mask from a file (else uses missing
                          value of variable)
        * maskvar=None : if maskfile is defined, we need to provide name of
                         mask array in maskfile
        * missing_value=None : when missing value attribute not defined
                               in input file, this allows to pass it
        * use_locstream=False : interpolate from ESMF grid to ESMF locstream
                                instead of ESMF grid, a bit faster.
                                use only to interpolate to boundary.
        * from_global=True : if input file is global leave to true. If input
                             is regional, set to False.
                             interpolating from a regional extraction can
                             significantly speed up processing.
        '''
        # 1. Create ESMF source grids
        if maskfile is not None:
            self.gridsrc_u, imin_src_u, imax_src_u, jmin_src_u, jmax_src_u = \
                self.create_source_grid(maskfile, from_global, coord_names_u,
                                        x_coords=x_coords_u,
                                        y_coords=y_coords_u,
                                        autocrop=autocrop)
            self.gridsrc_v, imin_src_v, imax_src_v, jmin_src_v, jmax_src_v = \
                self.create_source_grid(maskfile, from_global, coord_names_v,
                                        x_coords=x_coords_v,
                                        y_coords=y_coords_v,
                                        autocrop=autocrop)
        else:
            self.gridsrc_u, imin_src_u, imax_src_u, jmin_src_u, jmax_src_u = \
                self.create_source_grid(filename, from_global, coord_names_u,
                                        x_coords=x_coords_u,
                                        y_coords=y_coords_u,
                                        autocrop=autocrop)
            self.gridsrc_v, imin_src_v, imax_src_v, jmin_src_v, jmax_src_v = \
                self.create_source_grid(filename, from_global, coord_names_v,
                                        x_coords=x_coords_v,
                                        y_coords=y_coords_v,
                                        autocrop=autocrop)

        # 2. read the original field
        datasrc_u = ncdf.read_field(filename, variable_u, frame=frame)
        datasrc_v = ncdf.read_field(filename, variable_v, frame=frame)
        if self.geometry == 'surface':
            datasrc_u = datasrc_u[:, jmin_src_u:jmax_src_u,
                                  imin_src_u:imax_src_u]
            datasrc_v = datasrc_v[:, jmin_src_v:jmax_src_v,
                                  imin_src_v:imax_src_v]
            self.depth, self.nz, self.dz = ncdf.read_vert_coord(filename,
                                                                 depthname,
                                                                 self.nx,
                                                                 self.ny)
        else:
            datasrc_u = datasrc_u[jmin_src_u:jmax_src_u, imin_src_u:imax_src_u]
            datasrc_v = datasrc_v[jmin_src_v:jmax_src_v, imin_src_v:imax_src_v]
            self.depth = 0.
            self.nz = 1
            self.dz = 0.
        # read time
        try:
            self.timesrc = ncdf.read_time(filename, timename, frame=frame)
        except:
            print('input data time variable not read')

        # TODO !! make rotation to east,north from source grid.
        # important : if the grid is regular, we don't need to colocate u,v and
        # the interpolation will be more accurate.
        # Run colocation only if grid is non-regular.
#       angle_src_u = lc.compute_angle_from_lon_lat(self.gridsrc_u.coords[0][0].T,\
#                                                    self.gridsrc_u.coords[0][1].T)
#       angle_src_v = lc.compute_angle_from_lon_lat(self.gridsrc_v.coords[0][0].T,\
#                                                    self.gridsrc_v.coords[0][1].T)

        dataextrap_u = datasrc_u.copy()
        dataextrap_v = datasrc_v.copy()

        # 4. ESMF interpolation
        # Create a field on the centers of the grid
        field_src_u = ESMF.Field(self.gridsrc_u,
                                  staggerloc=ESMF.StaggerLoc.CENTER)
        field_src_v = ESMF.Field(self.gridsrc_v,
                                  staggerloc=ESMF.StaggerLoc.CENTER)

        # Set up a regridding object between source and destination
        if interpolator_u is None:
            print('create regridding for u')
            if method == 'bilinear':
                regridme_u = ESMF.Regrid(field_src_u, self.field_target,
                                          unmapped_action=ESMF.UnmappedAction.IGNORE,
                                          regrid_method=ESMF.RegridMethod.BILINEAR)
            elif method == 'patch':
                regridme_u = ESMF.Regrid(field_src_u, self.field_target,
                                          unmapped_action=ESMF.UnmappedAction.IGNORE,
                                          regrid_method=ESMF.RegridMethod.PATCH)
        else:
            regridme_u = interpolator_u

        if interpolator_v is None:
            print('create regridding for v')
            if method == 'bilinear':
                regridme_v = ESMF.Regrid(field_src_v, self.field_target,
                                          unmapped_action=ESMF.UnmappedAction.IGNORE,
                                          regrid_method=ESMF.RegridMethod.BILINEAR)
            elif method == 'patch':
                regridme_v = ESMF.Regrid(field_src_v, self.field_target,
                                          unmapped_action=ESMF.UnmappedAction.IGNORE,
                                          regrid_method=ESMF.RegridMethod.PATCH)
        else:
            regridme_v = interpolator_v

        print('regridding u')
        self.data_u = self.perform_interpolation(dataextrap_u, regridme_u,
                                                 field_src_u,
                                                 self.field_target,
                                                 self.grid_target,
                                                 self.use_locstream)
        print('regridding v')
        self.data_v = self.perform_interpolation(dataextrap_v, regridme_v,
                                                 field_src_v,
                                                 self.field_target,
                                                 self.grid_target,
                                                 self.use_locstream)

        # vector rotation to output grid
        self.data_u_out = self.data_u * np.cos(self.angle_dx[self.jmin:self.jmax+1,self.imin:self.imax+1]) + \
                          self.data_v * np.sin(self.angle_dx[self.jmin:self.jmax+1,self.imin:self.imax+1])
        self.data_v_out = self.data_v * np.cos(self.angle_dx[self.jmin:self.jmax+1,self.imin:self.imax+1]) - \
                          self.data_u * np.sin(self.angle_dx[self.jmin:self.jmax+1,self.imin:self.imax+1])

        # free memory (ESMPy has memory leak)
        self.gridsrc_u.destroy()
        self.gridsrc_v.destroy()
        field_src_u.destroy()
        field_src_v.destroy()
        return regridme_u, regridme_v

    def perform_interpolation(self, dataextrap, regridme, field_src,
                              field_target, grid_target, use_locstream):

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
                           x_coords=None, y_coords=None, autocrop=True):
        #create ESMF grid object for source grid
        # new way to create source grid
        # TO DO : move into separate function, has to be called before drown
        # so that we know the periodicity

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
            imin_src, imax_src, jmin_src, jmax_src = \
                lc.find_subset(self.grid_target, lon_src, lat_src)
            lon_src = lon_src[jmin_src:jmax_src, imin_src:imax_src]
            lat_src = lat_src[jmin_src:jmax_src, imin_src:imax_src]

        ny_src, nx_src = lon_src.shape
        if not autocrop:
            imin_src = 0
            imax_src = nx_src
            jmin_src = 0
            jmax_src = ny_src

        if from_global and not autocrop:
            gridsrc = ESMF.Grid(np.array([nx_src, ny_src]), num_peri_dims=1)
            self.gtype = 1  # 1 = periodic for drown NCL
            self.kew = 0    # 0 = periodic for drown sosie
        else:
            gridsrc = ESMF.Grid(np.array([nx_src, ny_src]))
            self.gtype = 0  # 1 = non periodic for drown NCL
            self.kew = -1   # -1 = non periodic for drown sosie
        gridsrc.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])
        gridsrc.coords[ESMF.StaggerLoc.CENTER][0][:] = lon_src.T
        gridsrc.coords[ESMF.StaggerLoc.CENTER][1][:] = lat_src.T

        return gridsrc, imin_src, imax_src, jmin_src, jmax_src
