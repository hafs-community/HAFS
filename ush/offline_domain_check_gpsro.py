#!/usr/bin/env python3
"""
offline_domain_check_gpsro.py

DESCRIPTION:
Reads a HAFS domain grids (grid_spec.nc) and filter out the gpsro observation outside
of model domain (FV3), including the profile that across the model boundary.
The FV3 domain grid is computed through the alpha shape (concave hull) of a set of points 
Solution from Iddo Hanniel (https://stackoverflow.com/questions/50549128/boundary-enclosing-a-given-set-of-points)

USEAGE:
python offline_domain_check_gpsro.py -i $obs_file, -o $output_file -g grid_file

e.g.
#obs_file = 'hafs.t12z.gnssro_cosmic2.nc'
#grid_file = 'grid_spec.nc'
#output_file = 'gnssro_domain_filtered.nc'
"""
import argparse
import sys
import netCDF4 as nc
import numpy as np
from scipy.spatial import Delaunay
from timeit import default_timer as timer

def parse_args():
    p = argparse.ArgumentParser(description="FV3 Alpha-Shape Domain Check for GPS RO")
    p.add_argument("-i", "--input", required=True, help="Input GPS RO data netCDF")
    p.add_argument("-o", "--output", required=True, help="Output filtered IODA file")
    p.add_argument("-g", "--gridspec", required=True, help="HAFS grid info (grid_spec.nc)")
    p.add_argument("--alpha", type=float, default=0.25, help="Alpha value for concave hull (degrees)")
    p.add_argument("--shrink", type=float, default=0.01, help="Factor to shrink the hull")
    return p.parse_args()

# --- 1. User's Reference Alpha Shape Functions ---
def alpha_shape(points, alpha, only_outer=True):
    assert points.shape[0] > 3, "Need at least four points"
    def add_edge(edges, i, j):
        if (i, j) in edges or (j, i) in edges:
            assert (j, i) in edges, "Can't go twice over same directed edge right?"
            if only_outer:
                edges.remove((j, i))
            return
        edges.add((i, j))

    tri = Delaunay(points)
    edges = set()
    for ia, ib, ic in tri.simplices:
        pa, pb, pc = points[ia], points[ib], points[ic]
        a = np.sqrt((pa[0] - pb[0]) ** 2 + (pa[1] - pb[1]) ** 2)
        b = np.sqrt((pb[0] - pc[0]) ** 2 + (pb[1] - pc[1]) ** 2)
        c = np.sqrt((pc[0] - pa[0]) ** 2 + (pc[1] - pa[1]) ** 2)
        s = (a + b + c) / 2.0
        area = np.sqrt(s * (s - a) * (s - b) * (s - c))
        # Prevent division by zero for degenerate triangles
        if area > 1e-10: 
            circum_r = a * b * c / (4.0 * area)
            if circum_r < alpha:
                add_edge(edges, ia, ib)
                add_edge(edges, ib, ic)
                add_edge(edges, ic, ia)
    return edges

def find_edges_with(i, edge_set):
    i_first = [j for (x,j) in edge_set if x==i]
    i_second = [j for (j,x) in edge_set if x==i]
    return i_first, i_second

def stitch_boundaries(edges):
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
                break # Dead end
            if edge0[0] == last_edge[1]:
                break
        boundary_lst.append(boundary)
    return boundary_lst

def shrink_boundary(points, factor=0.01):
    centroid = np.mean(points, axis=0)
    new_points = []
    for point in points:
        direction = point - centroid
        dist = np.linalg.norm(direction)
        if dist > 0:
            dir_norm = direction / dist
            new_point = point - factor * dir_norm * dist
        else:
            new_point = point
        new_points.append(new_point)
    return np.array(new_points)

# --- 2. Matplotlib-Free Ray Casting ---
def is_inside_vectorized(x, y, poly_x, poly_y):
    n = len(poly_x)
    inside = np.zeros(len(x), dtype=bool)
    p1x, p1y = poly_x[0], poly_y[0]
    for i in range(1, n + 1):
        p2x, p2y = poly_x[i % n], poly_y[i % n]
        mask = (y > min(p1y, p2y)) & (y <= max(p1y, p2y))
        if np.any(mask):
            x_inters = (y[mask] - p1y) * (p2x - p1x) / (p2y - p1y + 1e-12) + p1x
            inside[mask] = inside[mask] ^ (x[mask] < x_inters)
        p1x, p1y = p2x, p2y
    return inside

def normalize_lons(lons, target_min):
    return (lons - target_min) % 360 + target_min

# --- 3. Main Logic ---
def main():
    args = parse_args()
    start_time = timer()

    print(f"Reading grid from {args.gridspec}...")
    with nc.Dataset(args.gridspec, 'r') as grid_ds:
        glat = grid_ds.variables['grid_lat'][:]
        glon = grid_ds.variables['grid_lon'][:]

    # Subsample grid to save memory and Delaunay compute time. 
    # For a boundary, we only need the perimeter points anyway.
    # To be safe and capture curvature, we extract the outer 5 "rings" of the grid.
    perim_lons = np.concatenate([glon[0:5, :].flatten(), glon[-5:, :].flatten(), 
                                 glon[:, 0:5].flatten(), glon[:, -5:].flatten()])
    perim_lats = np.concatenate([glat[0:5, :].flatten(), glat[-5:, :].flatten(), 
                                 glat[:, 0:5].flatten(), glat[:, -5:].flatten()])

    # Normalize longitudes before Delaunay to prevent dateline spanning triangles
    grid_min_lon = np.min(perim_lons)
    perim_lons_norm = normalize_lons(perim_lons, grid_min_lon)

    points = np.column_stack((perim_lons_norm, perim_lats))
    points = np.unique(points, axis=0) # Remove duplicates from overlapping rings

    print(f"Computing Alpha Shape (Concave Hull) with alpha={args.alpha}...")
    edges = alpha_shape(points, alpha=args.alpha, only_outer=True)
    boundaries = stitch_boundaries(edges)
    
    # Pick the largest continuous boundary (in case of detached artifacts)
    longest_boundary = max(boundaries, key=len)
    
    # Extract ordered vertices
    ordered_poly_points = np.array([points[edge[0]] for edge in longest_boundary])
    
    print(f"Shrinking hull by factor {args.shrink}...")
    final_poly = shrink_boundary(ordered_poly_points, factor=args.shrink)
    poly_lon, poly_lat = final_poly[:, 0], final_poly[:, 1]

    print("Loading observations...")
    with nc.Dataset(args.input, 'r') as src_ds:
        meta = src_ds.groups['MetaData']
        obs_lat = meta.variables['latitude'][:]
        obs_lon = meta.variables['longitude'][:]
        obs_seq = meta.variables['sequenceNumber'][:]

        lat_vals = np.ma.filled(obs_lat, np.nan)
        lon_vals = np.ma.filled(obs_lon, np.nan)
        seq_vals = np.ma.filled(obs_seq, -999)

        lon_vals_norm = normalize_lons(lon_vals, grid_min_lon)

        print("Executing Ray Casting against Concave Hull...")
        valid_mask = np.isfinite(lat_vals) & np.isfinite(lon_vals)
        
        is_inside = np.zeros(len(lat_vals), dtype=bool)
        is_inside[valid_mask] = is_inside_vectorized(
            lon_vals_norm[valid_mask], lat_vals[valid_mask], poly_lon, poly_lat
        )

        bad_profiles = np.unique(seq_vals[valid_mask & ~is_inside])
        bad_profiles = bad_profiles[bad_profiles != -999]

        keep_mask = ~np.isin(seq_vals, bad_profiles)
        valid_indices = np.where(keep_mask)[0]

        print(f"Profiles dropped: {len(bad_profiles)}")
        print(f"Observations kept: {len(valid_indices)} / {len(obs_lat)}")

        if len(valid_indices) == 0:
            print("No observations found in domain. Exiting.")
            sys.exit(0)

        print(f"Writing to {args.output}...")
        with nc.Dataset(args.output, 'w', format='NETCDF4') as dst:
            dst.setncatts({a: src_ds.getncattr(a) for a in src_ds.ncattrs()})
            dst.createDimension('Location', len(valid_indices))

            def copy_v(src_v, target, name, idx):
                fill = getattr(src_v, '_FillValue', None)
                dv = target.createVariable(name, src_v.datatype, src_v.dimensions, fill_value=fill)
                dv.setncatts({a: src_v.getncattr(a) for a in src_v.ncattrs() if a != '_FillValue'})
                dv[:] = src_v[idx]

            for v_name, v_obj in src_ds.variables.items():
                copy_v(v_obj, dst, v_name, valid_indices)

            for g_name, g_grp in src_ds.groups.items():
                new_g = dst.createGroup(g_name)
                new_g.setncatts({a: g_grp.getncattr(a) for a in g_grp.ncattrs()})
                for v_name, v_obj in g_grp.variables.items():
                    copy_v(v_obj, new_g, v_name, valid_indices)

    print(f"Done in {timer() - start_time:.2f} seconds.")

if __name__ == "__main__":
    main()
