#!/usr/bin/env python
import netCDF4 as nc
import numpy as np
from scipy.spatial import Delaunay
from timeit import default_timer as timer
import argparse
import warnings
import os
from multiprocessing import Pool

warnings.filterwarnings('ignore')


def tic():
    return timer()


def toc(tic=tic, label=""):
    toc_now = timer()
    elapsed = toc_now - tic
    hrs = int(elapsed // 3600)
    mins = int((elapsed % 3600) // 60)
    secs = int(elapsed % 3600 % 60)
    print(f"{label}({elapsed:.2f}s), {hrs:02}:{mins:02}:{secs:02}")


def alpha_shape(points, alpha, only_outer=True):
    """
    Compute alpha shape (concave hull) of a set of points.
    """
    assert points.shape[0] > 3, "Need at least four points"

    def add_edge(edges, i, j):
        if (i, j) in edges or (j, i) in edges:
            if only_outer and (j, i) in edges:
                edges.remove((j, i))
            return
        edges.add((i, j))

    points = points.data
    tri = Delaunay(points)
    edges = set()

    for ia, ib, ic in tri.simplices:
        pa = points[ia]
        pb = points[ib]
        pc = points[ic]

        a = np.sqrt((pa[0] - pb[0]) ** 2 + (pa[1] - pb[1]) ** 2)
        b = np.sqrt((pb[0] - pc[0]) ** 2 + (pb[1] - pc[1]) ** 2)
        c = np.sqrt((pc[0] - pa[0]) ** 2 + (pc[1] - pa[1]) ** 2)
        s = (a + b + c) / 2.0
        area_term = s * (s - a) * (s - b) * (s - c)
        if area_term <= 0:
            continue
        area = np.sqrt(area_term)
        circum_r = a * b * c / (4.0 * area)

        if circum_r < alpha:
            add_edge(edges, ia, ib)
            add_edge(edges, ib, ic)
            add_edge(edges, ic, ia)

    return edges


def find_edges_with(i, edge_set):
    i_first = [j for (x, j) in edge_set if x == i]
    i_second = [j for (j, x) in edge_set if x == i]
    return i_first, i_second


def stitch_boundaries(edges):
    """
    Sort edges computed by alpha_shape.
    """
    edge_set = edges.copy()
    boundary_lst = []

    while len(edge_set) > 0:
        boundary = []
        edge0 = edge_set.pop()
        boundary.append(edge0)
        last_edge = edge0

        while len(edge_set) > 0:
            i, j = last_edge
            j_first, j_second = find_edges_with(j, edge_set)

            if j_first:
                edge_set.remove((j, j_first[0]))
                edge_with_j = (j, j_first[0])
                boundary.append(edge_with_j)
                last_edge = edge_with_j
            elif j_second:
                edge_set.remove((j_second[0], j))
                edge_with_j = (j, j_second[0])
                boundary.append(edge_with_j)
                last_edge = edge_with_j
            else:
                break

            if edge0[0] == last_edge[1]:
                break

        boundary_lst.append(boundary)

    return boundary_lst


def shrink_boundary(points, centroid, factor=0.01):
    new_points = []
    for point in points:
        direction = point - centroid
        distance_to_centroid = np.linalg.norm(direction)
        if distance_to_centroid == 0:
            new_points.append(point)
            continue
        direction_normalized = direction / distance_to_centroid
        new_point = point - factor * direction_normalized * distance_to_centroid
        new_points.append(new_point)
    return np.array(new_points)


def polygon_from_edge_points(edge_points):
    """
    Ensure polygon is closed and ready for point-in-polygon.
    """
    poly = np.asarray(edge_points, dtype=np.float64)
    if poly.shape[0] < 3:
        raise ValueError("Polygon has fewer than 3 points.")
    if not np.allclose(poly[0], poly[-1]):
        poly = np.vstack([poly, poly[0]])
    return poly


def points_in_polygon_chunk(poly, pts):
    """
    Vectorized ray-casting point-in-polygon for one chunk.
    poly: (M,2), closed polygon
    pts : (N,2)
    returns bool array of shape (N,)
    """
    x = pts[:, 0]
    y = pts[:, 1]

    x1 = poly[:-1, 0]
    y1 = poly[:-1, 1]
    x2 = poly[1:, 0]
    y2 = poly[1:, 1]

    inside = np.zeros(len(pts), dtype=bool)
    eps = 1.0e-12

    for i in range(len(x1)):
        cond = ((y1[i] > y) != (y2[i] > y))
        xinters = (x2[i] - x1[i]) * (y - y1[i]) / (y2[i] - y1[i] + eps) + x1[i]
        inside ^= cond & (x < xinters)

    return inside


def _worker_contains_points(args):
    poly, chunk = args
    return points_in_polygon_chunk(poly, chunk)


def parallel_contains_points(poly, obs_coords, nproc):
    nobs = obs_coords.shape[0]
    if nobs == 0:
        return np.zeros((0,), dtype=bool)

    nproc = max(1, int(nproc))
    nproc = min(nproc, nobs)

    if nproc == 1:
        return points_in_polygon_chunk(poly, obs_coords)

    chunks = np.array_split(obs_coords, nproc)
    work = [(poly, chunk) for chunk in chunks]

    with Pool(processes=nproc) as pool:
        masks = pool.map(_worker_contains_points, work)

    return np.concatenate(masks)


def create_dimensions_like(src_obj, dst_obj, location_size):
    """
    Copy dimensions from src_obj to dst_obj, except replace Location size.
    """
    for dim_name, dim in src_obj.dimensions.items():
        if dim_name == "Location":
            if "Location" not in dst_obj.dimensions:
                dst_obj.createDimension("Location", location_size)
        else:
            if dim_name not in dst_obj.dimensions:
                if dim.isunlimited():
                    dst_obj.createDimension(dim_name, None)
                else:
                    dst_obj.createDimension(dim_name, len(dim))


def subset_var_along_location(invar, inside_indices):
    """
    Subset variable only along the Location dimension, preserve all other dims.
    """
    data = invar[:]
    dims = invar.dimensions

    if "Location" not in dims:
        return data

    loc_axis = dims.index("Location")
    return np.take(data, inside_indices, axis=loc_axis)


def main():
    tic1 = tic()

    parser = argparse.ArgumentParser()
    parser.add_argument('-g', '--grid', type=str, help='grid file', required=True)
    parser.add_argument('-o', '--obs', type=str, help='ioda observation file', required=True)
    parser.add_argument('-s', '--shrink', type=float, help='hull shrink factor', required=True)
    parser.add_argument('-clon', '--cenlon', type=float, help='Domain center lon (kept for interface compatibility)', required=True)
    parser.add_argument('-clat', '--cenlat', type=float, help='Domain center lat (kept for interface compatibility)', required=True)
    parser.add_argument('-n', '--nproc', type=int, default=1,
                        help='number of processes for point-in-polygon check')
    parser.add_argument('-out', '--out', type=str, required=False,
                        help='Output filename (default: input filename + _dc)')
    args = parser.parse_args()

    obs_filename = args.obs
    grid_filename = args.grid
    hull_shrink_factor = args.shrink
    nproc = args.nproc

    print(f"Obs file: {obs_filename}")
    print(f"Grid file: {grid_filename}")
    print(f"Hull shrink factor: {hull_shrink_factor}")
    print(f"Nproc: {nproc}")

    grid_ds = nc.Dataset(grid_filename, 'r')
    obs_ds = nc.Dataset(obs_filename, 'r')

    # Extract grid latitude and longitude
    if 'grid_lat' in grid_ds.variables and 'grid_lon' in grid_ds.variables:
        grid_lat = grid_ds.variables['grid_lat'][:, :]
        grid_lon = grid_ds.variables['grid_lon'][:, :]
        grid_lat = grid_lat.flatten()
        grid_lon = grid_lon.flatten()
        dycore = "FV3"
    elif 'latCell' in grid_ds.variables and 'lonCell' in grid_ds.variables:
        grid_lat = np.degrees(grid_ds.variables['latCell'][:])
        grid_lon = np.degrees(grid_ds.variables['lonCell'][:])
        dycore = "MPAS"
    else:
        raise ValueError("Unrecognized grid format: 'grid_lat'/'grid_lon' or 'latCell'/'lonCell' not found.")

    print(f"Max/Min Lat: {np.max(grid_lat)}, {np.min(grid_lat)}")
    print(f"Max/Min Lon: {np.max(grid_lon)-360}, {np.min(grid_lon)-360}")
    print(f"Dycore: {dycore}\n")

    # Build boundary
    points = np.vstack([grid_lon, grid_lat]).T
    edges = alpha_shape(points, alpha=0.25, only_outer=True)
    edges_sorted = stitch_boundaries(edges)

    if len(edges_sorted) == 0:
        raise RuntimeError("No boundary edges found.")

    edge_points = []
    for idx in edges_sorted[0]:
        ipt = idx[0]
        jpt = idx[1]
        point_1 = points[ipt]
        point_2 = points[jpt]
        edge_points.append(point_1)
        edge_points.append(point_2)
    edge_points = np.asarray(edge_points)

    centroid = np.nanmean(edge_points, axis=0)
    edge_points = shrink_boundary(edge_points, centroid, factor=hull_shrink_factor)
    polygon = polygon_from_edge_points(edge_points)

    # Extract observation lat/lon
    obs_lat = obs_ds.groups['MetaData'].variables['latitude'][:]
    obs_lon = obs_ds.groups['MetaData'].variables['longitude'][:]
    obs_lon = np.where(obs_lon < 0, obs_lon + 360, obs_lon)

    obs_coords = np.vstack((obs_lon, obs_lat)).T

    # Point-in-polygon
    inside_domain = parallel_contains_points(polygon, obs_coords, nproc)
    inside_indices = np.where(inside_domain)[0]

    toc(tic1, label="Time to find obs within domain: ")

    included_count = len(inside_indices)
    total_count = len(obs_lat)
    excluded_count = total_count - included_count

    print("Ob counts:")
    print(f"  Excluded: {excluded_count}")
    print(f"  Included: {included_count}")
    print(f"  Total:    {total_count}")

    tic2 = tic()

    # Output filename
    if args.out is not None:
        outfile = args.out
    else:
        if obs_filename.endswith('.nc4'):
            outfile = obs_filename.replace('.nc4', '_dc.nc4')
        elif obs_filename.endswith('.nc'):
            outfile = obs_filename.replace('.nc', '_dc.nc')
        else:
            outfile = obs_filename + '_dc.nc'

    fout = nc.Dataset(outfile, 'w')

    # Copy root dimensions
    create_dimensions_like(obs_ds, fout, len(inside_indices))

    # Copy root variables
    for var_name, invar in obs_ds.variables.items():
        vartype = invar.dtype
        dims = invar.dimensions

        kwargs = {}
        if '_FillValue' in invar.ncattrs():
            kwargs['fill_value'] = invar.getncattr('_FillValue')

        outvar = fout.createVariable(var_name, vartype, dims, **kwargs)
        data_out = subset_var_along_location(invar, inside_indices)
        outvar[:] = data_out

        for attr in invar.ncattrs():
            if attr == '_FillValue':
                continue
            outvar.setncattr(attr, invar.getncattr(attr))

    # Copy root global attributes
    for attr in obs_ds.ncattrs():
        if attr != '_FillValue':
            fout.setncattr(attr, obs_ds.getncattr(attr))

    # Copy groups and variables
    for group_name, src_group in obs_ds.groups.items():
        dst_group = fout.createGroup(group_name)

        create_dimensions_like(src_group, dst_group, len(inside_indices))

        for attr in src_group.ncattrs():
            if attr != '_FillValue':
                dst_group.setncattr(attr, src_group.getncattr(attr))

        for var_name, invar in src_group.variables.items():
            vartype = invar.dtype
            dims = invar.dimensions

            kwargs = {}
            if '_FillValue' in invar.ncattrs():
                kwargs['fill_value'] = invar.getncattr('_FillValue')

            outvar = dst_group.createVariable(var_name, vartype, dims, **kwargs)
            data_out = subset_var_along_location(invar, inside_indices)
            outvar[:] = data_out

            for attr in invar.ncattrs():
                if attr == '_FillValue':
                    continue
                outvar.setncattr(attr, invar.getncattr(attr))

    obs_ds.close()
    fout.close()
    grid_ds.close()

    toc(tic2, label="Time to create new obs file: ")
    toc(tic1, label="Total elapsed time: ")
    print(f"Output file: {outfile}")


if __name__ == "__main__":
    main()
