#! /usr/bin/env python3

import numpy as np
try:
    import esmpy as ESMF
except ImportError or ModuleNotFoundError:
    import ESMF as ESMF

import lib_ioncdf as ncdf

class obc_segment():
    ''' A class describing an open boundary condtion segment '''

    def __init__(self, segment_name, target_grid_file, target_model='MOM6',
                 **kwargs):
        ''' read target grid and create associated ESMF grid and
            locstream objects.

        Args:
            segment_name (string): name of the segment
            target_grid_file (netcdf file): grid file of the ocean model
            target_model: name of the ocean model (default MOM6)

        Examples:
            south = obc_segment('segment_001','./ocean_hgrid.nc',
                                imin=0,imax=360,jmin=0,jmax=0)
                        north = obc_segment('segment_002','./ocean_hgrid.nc',
                                            imin=0,imax=360,jmin=960,jmax=960)

            south = obc_segment('south','./roms_grd.nc',target_model='ROMS',
                                imin=0,imax=180,jmin=0,jmax=0)
            north = obc_segment('north','./roms_grd.nc',target_model='ROMS',
                                imin=0,imax=180,jmin=480,jmax=480)
        '''

#       * istart : along x axis, where the segment begins
#
#       * iend : along x axis, where the segment ends
#
#       * jstart : along y axis, where the segment begins
#
#       * jend : along y axis, where the segment end
#
#       '''

        # read args
        self.segment_name = segment_name
        self.target_grid_file = target_grid_file
        self.items = []
        self.items.append('segment_name')
        self.items.append('target_grid')
        self.debug = False

        # iterate over all kwargs and store them as attributes for the object
        if kwargs is not None:
            self.__dict__.update(kwargs)
            for key, value in kwargs.items():
                self.items.append(key)

        if self.istart > self.iend:
            self.imin = self.iend
            self.imax = self.istart
            self.orientation = 2  # E-W
        elif self.iend > self.istart:
            self.imin = self.istart
            self.imax = self.iend
            self.orientation = 0  # W-E
        else:
            self.imin = self.istart
            self.imax = self.istart
        if self.jstart > self.jend:
            self.jmin = self.jend
            self.jmax = self.jstart
            self.orientation = 3  # N-S
        elif self.jend > self.jstart:
            self.jmin = self.jstart
            self.jmax = self.jend
            self.orientation = 1  # S-N
        else:
            self.jmin = self.jstart
            self.jmax = self.jstart

        # compute dimensions
        self.nx = self.imax - self.imin + 1
        self.ny = self.jmax - self.jmin + 1

        self.ilist = np.empty((self.ny, self.nx))
        self.jlist = np.empty((self.ny, self.nx))

        for kx in np.arange(self.nx):
            self.jlist[:, kx] = np.arange(self.jmin, self.jmax+1) / 2.
        for ky in np.arange(self.ny):
            self.ilist[ky, :] = np.arange(self.imin, self.imax+1) / 2.

        # coordinate names depend on ocean model
        # MOM6 has all T,U,V points in one big grid
        # ROMS has in 3 separate ones.
        if target_model == 'MOM6':
            coord_names = ["x", "y"]
            self.angle_dx = ncdf.read_field(target_grid_file, 'angle_dx')
            lon_target = ncdf.read_field(target_grid_file, 'x')
            lat_target = ncdf.read_field(target_grid_file, 'y')
            ny_target, nx_target = lat_target.shape
            self.grid_target = ESMF.Grid(np.array([nx_target, ny_target]))
            self.grid_target.add_coords(staggerloc=[ESMF.StaggerLoc.CENTER])
            self.grid_target.coords[ESMF.StaggerLoc.CENTER]
            self.grid_target.coords[ESMF.StaggerLoc.CENTER][0][:] = \
                lon_target.T
            self.grid_target.coords[ESMF.StaggerLoc.CENTER][1][:] = \
                lat_target.T

        elif target_model == 'ROMS':
            coord_names = ["lon_rho", "lat_rho"]
            self.angle_dx = ncdf.read_field(target_grid_file, 'angle')

            # import target grid into ESMF grid object
            self.grid_target = ESMF.Grid(filename=target_grid_file,
                                          filetype=ESMF.FileFormat.GRIDSPEC,
                                          coord_names=coord_names)

        elif target_model == 'regular':
            coord_names = ["lon", "lat"]
            self.angle_dx = 0.  # for now

            # import target grid into ESMF grid object
            self.grid_target = ESMF.Grid(filename=target_grid_file,
                                          filetype=ESMF.FileFormat.GRIDSPEC,
                                          coord_names=coord_names,
                                          add_corner_stagger=True)

        # import same target grid into ESMF locstream object
        self.locstream_target = \
            ESMF.LocStream(self.nx * self.ny,
                            coord_sys=ESMF.CoordSys.SPH_DEG)
        self.locstream_target["ESMF:Lon"] = \
            self.grid_target.coords[0][0][self.imin:self.imax+1,
                                          self.jmin:self.jmax+1].flatten()
        self.locstream_target["ESMF:Lat"] = \
            self.grid_target.coords[0][1][self.imin:self.imax+1,
                                          self.jmin:self.jmax+1].flatten()

        # save lon/lat on this segment
        self.lon = self.grid_target.coords[0][0][self.imin:self.imax+1,
                                                 self.jmin:self.jmax+1]
        self.lon = self.lon.transpose().squeeze()
        self.lat = self.grid_target.coords[0][1][self.imin:self.imax+1,
                                                 self.jmin:self.jmax+1]
        self.lat = self.lat.transpose().squeeze()
        # nc dimensions for horizontal coords
        self.hdimensions_name = ('ny_' + self.segment_name,
                                 'nx_' + self.segment_name,)

        return None
