#! /usr/bin/env python3

import numpy as np

def distance_on_unit_sphere(lat1, long1, lat2, long2):
    ''' compute the distance on the sphere between points (or arrays)'''
    # Convert latitude and longitude to
    # spherical coordinates in radians.
    degrees_to_radians = np.pi/180.0

    # phi = 90 - latitude
    phi1 = (90.0 - lat1)*degrees_to_radians
    phi2 = (90.0 - lat2)*degrees_to_radians

    # theta = longitude
    theta1 = long1*degrees_to_radians
    theta2 = long2*degrees_to_radians

    # Compute spherical distance from spherical coordinates.

    # For two locations in spherical coordinates
    # (1, theta, phi) and (1, theta, phi)
    # cosine( arc length ) =
    #    sin phi sin phi' cos(theta-theta') + cos phi cos phi'
    # distance = rho * arc length

    cos = (np.sin(phi1)*np.sin(phi2)*np.cos(theta1 - theta2) +
           np.cos(phi1)*np.cos(phi2))
    dist = np.arccos(cos)

    return dist

def find_subset(target_grid, lon_src, lat_src):
    ''' for a given regional target grid, find the subset of the source grid
    (usually global) containing the target grid to reduce compute time '''
    ny, nx = lon_src.shape

    lon_bl_tgt = target_grid.coords[0][0][0, 0]
    lat_bl_tgt = target_grid.coords[0][1][0, 0]  # bottom left
    lon_br_tgt = target_grid.coords[0][0][-1, 0]
    lat_br_tgt = target_grid.coords[0][1][-1, 0]  # bottom right
    lon_ul_tgt = target_grid.coords[0][0][0, -1]
    lat_ul_tgt = target_grid.coords[0][1][0, -1]  # upper left
    lon_ur_tgt = target_grid.coords[0][0][-1, -1]
    lat_ur_tgt = target_grid.coords[0][1][-1, -1]  # upper right

    dist_2_bottom_left_corner = distance_on_unit_sphere(lon_bl_tgt, lat_bl_tgt,
                                                        lon_src, lat_src)
    j_bl_src, i_bl_src = np.unravel_index(dist_2_bottom_left_corner.argmin(),
                                           dist_2_bottom_left_corner.shape)

    dist_2_bottom_right_corner = distance_on_unit_sphere(lon_br_tgt,
                                                         lat_br_tgt,
                                                         lon_src, lat_src)
    j_br_src, i_br_src = np.unravel_index(dist_2_bottom_right_corner.argmin(),
                                           dist_2_bottom_right_corner.shape)

    dist_2_upper_left_corner = distance_on_unit_sphere(lon_ul_tgt, lat_ul_tgt,
                                                       lon_src, lat_src)
    j_ul_src, i_ul_src = np.unravel_index(dist_2_upper_left_corner.argmin(),
                                           dist_2_upper_left_corner.shape)

    dist_2_upper_right_corner = distance_on_unit_sphere(lon_ur_tgt, lat_ur_tgt,
                                                        lon_src, lat_src)
    j_ur_src, i_ur_src = np.unravel_index(dist_2_upper_right_corner.argmin(),
                                           dist_2_upper_right_corner.shape)

    imin = min(i_bl_src, i_ul_src)
    imax = max(i_br_src, i_ur_src)
    jmin = min(j_bl_src, j_br_src)
    jmax = max(j_ul_src, j_ur_src)

    # for safety
    imin = max(imin-2, 0)
    jmin = max(jmin-2, 0)
    imax = min(imax+2, nx)
    jmax = min(jmax+2, ny)

    print('Subset source grid : full dimension is ', nx, ny, ' subset is ',
          imin, imax, jmin, jmax)

    return imin, imax, jmin, jmax

def supergrid_to_staggered(field, pointtype):
    ''' extract field from supergrid on its staggered location,
        given by pointtype T,U,V '''
    # this is what I assumed for lower left grid cell
    #
    #     |   V(0,0)
    #     |
    #     |   T(0,0)   U(0,0)
    #     |
    #     |------------------
    #
    if pointtype == 'T':
        if len(field.shape) == 2:
            fieldout = field[1::2, 1::2]
        elif len(field.shape) == 3:
            fieldout = field[:, 1::2, 1::2]
    elif pointtype == 'U':
        if len(field.shape) == 2:
            fieldout = field[1::2, 0::2]
        elif len(field.shape) == 3:
            fieldout = field[:, 1::2, 0::2]
    elif pointtype == 'V':
        if len(field.shape) == 2:
            fieldout = field[::2, 1::2]
        elif len(field.shape) == 3:
            fieldout = field[:, ::2, 1::2]
    return fieldout
