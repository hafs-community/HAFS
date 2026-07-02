#!/usr/bin/env python
"""
vad_thinning.py

Offline VAD wind thinning script replicating NOAA-EMC GSI logic.
Reads an IODA VAD wind file, does:
  - Horizontal grouping by stationIdentification and time
  - Vertical averaging of groups of 6 observations sorted by height
  - Height QC (drops superobservations with averaged height > 7000 m)
Writes a new IODA file with the thinned superobservations, preserving all original groups and variables.
"""

import argparse
import numpy as np
from netCDF4 import Dataset
from scipy.stats import mode
import datetime

# GSI thinning parameters
AVG_GROUP_SIZE = 6  # Number of observations to average vertically
MAX_HEIGHT = 7000.0  # Maximum allowed averaged height in meters
BOX_SIZE = AVG_GROUP_SIZE * 50  # 300m spacing if 6*50m
TOL = 1e-3  # Tolerance for float comparisons


def parse_args():
    p = argparse.ArgumentParser(description="Thin VAD wind obs in an IODA file like GSI")
    p.add_argument("-i", "--input", required=True, help="Input IODA VAD wind netCDF")
    p.add_argument("-o", "--output", required=True, help="Output thinned IODA file")
    p.add_argument("-s", "--station", help="Optional station ID to filter (e.g., KOUN)")
    p.add_argument("--near", dest="vad_near_analtime", action="store_true",
                   help="For new VADs: only keep obs within +/-.25 h; otherwise use discrete windows")
    return p.parse_args()


def reable_time(timestamp):
    # Convert to a datetime object (in UTC)
    dt_object = datetime.datetime.fromtimestamp(timestamp)
    # Format into a human-readable string
    return dt_object.strftime("%Y-%m-%d %H:%M:%S UTC")


def group_by_station_time(stn_ids, datetimes, timeoffsets):
    """
    Group observations by station ID, dateTime, and timeOffset.

    Parameters
    ----------
    stn_ids : array-like of bytes or str
        stationIdentification for each observation
    datetimes : array-like of int
        dateTime (seconds since epoch) for each observation
    timeoffsets : array-like of float
        timeOffset (seconds from analysis time) for each observation

    Returns
    -------
    dict
        keys are tuples (stationID, dateTime, timeOffset)
        values are lists of integer indices into the original arrays
    """
    groups = {}
    for idx, (st, dt, to) in enumerate(zip(stn_ids, datetimes, timeoffsets)):
        # decode station ID, handling both byte-strings and str
        try:
            sid = st.tostring().decode('utf-8').strip() if hasattr(st, 'tostring') else str(st).strip()
        except (AttributeError, UnicodeDecodeError):
            sid = str(st).strip()
        if not sid:
            continue  # skip empty station IDs

        # use integer and float conversions to ensure keys hash consistently
        key = (sid, int(dt), int(to))
        groups.setdefault(key, []).append(idx)
    return groups


def thin_vad_obs(ds, station_filter=None, vad_near_analtime=False):
    """Thin VAD observations by averaging groups of observations vertically."""
    # Collect all variables from all groups
    all_vars = {}
    for grp_name, group in ds.groups.items():
        for var_name, var in group.variables.items():
            all_vars[(grp_name, var_name)] = var[:]

    # Determine variable types and fill values
    var_types = {}
    fill_values = {}
    for (grp_name, var_name), var in all_vars.items():
        if var.dtype.kind in ('f', 'd'):
            var_types[(grp_name, var_name)] = 'float'
        elif var.dtype.kind in ('i', 'u'):
            var_types[(grp_name, var_name)] = 'int'
        elif var.dtype.kind == 'S' or isinstance(var.dtype, str):
            var_types[(grp_name, var_name)] = 'string'
        else:
            var_types[(grp_name, var_name)] = 'other'
        fill_values[(grp_name, var_name)] = getattr(var, '_FillValue', None)

    # Group by station and time
    stn = all_vars[('MetaData', 'stationIdentification')]
    datetime = all_vars[('MetaData', 'dateTime')]
    timeoffset = all_vars[('MetaData', 'timeOffset')]
    station_time_groups = group_by_station_time(stn, datetime, timeoffset)

    superobs_data = []
    print(f"Processing {len(station_time_groups)} station-time groups")
    for (sid, t, toff), idxs in station_time_groups.items():
        if station_filter and sid != station_filter:
            continue

        # GSI new-VAD time-level thinning; t is timeOffset in seconds; GSI expects hours:
        t_hr = abs(toff) / 3600.0

        if vad_near_analtime:
            # only the +/-0.25 h bin
            if t_hr >= 0.25:
                continue
        else:
            # the discrete windows GSI uses:
            windows = [
                (0.17, 0.32),
                (0.67, 0.82),
                (1.17, 1.32),
                (1.67, 1.82),
                (2.17, 2.62),
                (2.67, 2.82)
            ]
            # drop if NOT in any of those windows
            if not any((lo - TOL) <= t_hr <= (hi + TOL) for lo, hi in windows):
                continue

        # TSB FILTER (use dumpReportSubType)
        tsb = all_vars.get(('MetaData', 'dumpReportSubType'), None)
        if tsb is None:
            raise RuntimeError("Cannot find dumpReportSubType in MetaData; TSB test cannot be applied.")

        # Every level in this station-time group shares the same TSB, so look at the first index:
        tsb0 = int(tsb[idxs[0]])
        if tsb0 != 2:
            # GSI drops any station-time group whose TSB != 2
            continue

        timestamp = reable_time(t)
        print(f"Station: {sid}, Time: {timestamp}, Num obs: {len(idxs)}")

        # Sort indices by height
        hgt = all_vars[('MetaData', 'height')][idxs]
        valid_mask = ~np.isnan(hgt)
        if hasattr(hgt, 'mask'):
            valid_mask &= ~hgt.mask
        sorted_idxs = [idxs[i] for i in np.argsort(hgt) if valid_mask[i]]

        # Skip the lowest 5 observations to mimic GSI's vertical thinning, which starts processing at k=6 (mod(k,6)==0)
        heights = all_vars[('MetaData', 'height')]
        stationElevations = all_vars[('MetaData', 'stationElevation')]
        sorted_idxs = [i for i in sorted_idxs if stationElevations[i] + BOX_SIZE <= heights[i]]

        # Skip this station-time group if no valid height observations remain after filtering
        if not sorted_idxs:
            print(f"Skipping {sid} at {timestamp}: No valid height observations")
            continue

        # Now chunk into vertical blocks of size AVG_GROUP_SIZE (six)
        for start in range(0, len(sorted_idxs), AVG_GROUP_SIZE):
            end = min(start + AVG_GROUP_SIZE, len(sorted_idxs))
            block_idxs = sorted_idxs[start:end]   # up to six levels

            # Skip if the block has no valid indices (safety check, usually shouldn't happen)
            if not block_idxs:
                continue

            # Skip any block with fewer than 6 elements.
            # While GSI nominally uses a minimum of 3 levels, its if(klev>levs) cycle loop_readsb
            # logic effectively enforces grouping in sets of 6, so we require full blocks of 6
            # to match that behavior.
            if len(block_idxs) < AVG_GROUP_SIZE:
                continue

            # Useful debug prints
            print(f"\nHeights used for superob from {sid} at time {reable_time(t)}:")
            #for i in block_idxs:  # noqa: E265
            #    print(f"  {all_vars[('MetaData', 'height')][i]:.2f} m"\
            #          f"  {all_vars[('ObsValue', 'windEastward')][i]:.2f} m/s"\
            #          f"  {all_vars[('ObsValue', 'windNorthward')][i]:.2f} m/s"\
            #          f"  {all_vars[('ObsValue', 'bestwindEastward')][i]:.2f} m/s"\
            #          f"  {all_vars[('ObsValue', 'bestwindNorthward')][i]:.2f} m/s")

            # Apply vertical height-gating to mimic GSI's 301m threshold.
            # GSI uses 301m (~6 levels at 50m spacing), but 251m would likely suffice since
            # the actual spacing from k=6 to k=11 is closer to 250m, not 300m.
            base_h = heights[block_idxs[0]]
            gated_idxs = [i for i in block_idxs if (heights[i] - base_h) < 301.0]
            block_idxs = gated_idxs

            # If any level in block is more than 5 m/s away from the block-mean, drop the whole block:
            u_mean = np.mean([all_vars[('ObsValue', 'windEastward')][i] for i in block_idxs])
            v_mean = np.mean([all_vars[('ObsValue', 'windNorthward')][i] for i in block_idxs])
            raw_us = all_vars[('ObsValue', 'windEastward')]
            raw_vs = all_vars[('ObsValue', 'windNorthward')]
            if any(abs(raw_us[i] - u_mean) > 5.0 + TOL or abs(raw_vs[i] - v_mean) > 5.0 + TOL for i in block_idxs):
                continue

            # === BEGIN GSI-exact deviation logic (check only the first level of block) ===
            k0 = block_idxs[0]  # the bottom level index in this 6-point block
            uu = all_vars[('ObsValue', 'windEastward')][k0]
            vv = all_vars[('ObsValue', 'windNorthward')][k0]
            bgU = all_vars[('ObsValue', 'bestwindEastward')][k0]
            bgV = all_vars[('ObsValue', 'bestwindNorthward')][k0]
            h_k0 = all_vars[('MetaData', 'height')][k0]

            # If any of (uu, vv, bgU, bgV) is NaN, treat dev as 0
            if np.isnan(uu) or np.isnan(vv) or np.isnan(bgU) or np.isnan(bgV):
                dev_uv_k0 = 0.0
                dev_v_k0 = 0.0
            else:
                dev_uv_k0 = np.hypot(uu - bgU, vv - bgV)
                dev_v_k0 = abs(vv - bgV)

            # GSI's first test at level k0: if dev_uv_k0 > 10.0 ? drop entire 6-level block
            if dev_uv_k0 > 10.0 + TOL:
                continue

            # GSI's second test at level k0: if dev_v_k0 > 8.0 ? drop block
            if dev_v_k0 > 8.0 + TOL:
                continue

            # GSI's third test at level k0: if (dev_v_k0 > 5.0 AND h_k0 < 5000) ? drop block
            if (dev_v_k0 > 5.0 + TOL) and (h_k0 < 5000.0 + TOL):
                continue

            # GSI's fourth test at level k0: if (h_k0 > 7000.0) ? drop block
            if h_k0 > MAX_HEIGHT + TOL:
                continue
            # === END GSI-exact deviation logic ===

            # If we reach here, none of the four tests triggered ? we build a super-obs
            superob = {}
            for (grp_name, var_name), var in all_vars.items():
                data = var[block_idxs]
                vtype = var_types[(grp_name, var_name)]
                fill = fill_values[(grp_name, var_name)]

                if vtype in ('float', 'int'):
                    if hasattr(data, 'mask'):
                        valid_data = data[~data.mask]
                    else:
                        valid_data = data[~np.isnan(data)]
                    if len(valid_data) == 0:
                        superob[(grp_name, var_name)] = fill if fill is not None else np.nan
                        continue
                    if vtype == 'float':
                        superob[(grp_name, var_name)] = np.mean(valid_data)
                    else:  # vtype == 'int'
                        if grp_name == 'QualityMarker':  # GSI takes the QM from the lowest level in the block
                            superob[(grp_name, var_name)] = int(valid_data[0])  # GSI method
                        elif grp_name == 'MetaData' and var_name == 'height':  # Sometimes height superob will be float
                            superob[(grp_name, var_name)] = np.mean(valid_data)
                        else:
                            superob[(grp_name, var_name)] = int(np.round(np.mean(valid_data)))
                elif vtype == 'string':
                    decoded = [d.decode('utf-8').strip() if isinstance(d, bytes) else str(d).strip()
                               for d in data if d != fill]
                    superob[(grp_name, var_name)] = mode(decoded)[0][0] if decoded else ''
                else:
                    superob[(grp_name, var_name)] = data[0] if len(data) > 0 else fill

            # Finally check wind-speed sanity (v(uob**2+vob**2) > 60.0)
            uob = superob.get(('ObsValue', 'windEastward'), np.nan)
            vob = superob.get(('ObsValue', 'windNorthward'), np.nan)
            if not np.isnan(uob) and not np.isnan(vob):
                if np.hypot(uob, vob) > 60.0 + TOL:
                    continue

            # Finally, append superob
            superobs_data.append(superob)

    return superobs_data


def write_ioda(outfile, superobs_data, template):
    """Write thinned superobservations to a new IODA file, preserving all groups and variables."""
    if not superobs_data:
        print("No superobservations to write.")
        return

    nloc = len(superobs_data)
    print(f"Writing {nloc} superobservations to {outfile}")
    with Dataset(outfile, 'w') as f_out:
        # Create the Location dimension
        f_out.createDimension('Location', nloc)

        # Copy global attributes from the template
        for att in template.ncattrs():
            f_out.setncattr(att, template.getncattr(att))

        # Create and populate the Location variable
        loc = f_out.createVariable('Location', 'int64', ('Location',))
        loc[:] = np.arange(nloc)

        # Recreate all groups and variables
        for grp_name in template.groups:
            grp_out = f_out.createGroup(grp_name)
            for vn, var_in in template.groups[grp_name].variables.items():
                # Handle stationIdentification explicitly as a string variable
                if vn == 'stationIdentification':
                    var_out = grp_out.createVariable(vn, str, ('Location',))
                elif grp_name == 'MetaData' and vn == 'height':
                    # Always write height as float32
                    fill = getattr(var_in, '_FillValue', None)
                    var_out = grp_out.createVariable(vn, 'f4', ('Location',), fill_value=fill)
                else:
                    # For other variables, use the original dtype and fill value if present
                    dt = var_in.dtype
                    fill = getattr(var_in, '_FillValue', None)  # Safely get _FillValue if it exists
                    var_out = grp_out.createVariable(vn, dt, ('Location',), fill_value=fill)

                # Copy variable attributes (excluding _FillValue)
                for att in var_in.ncattrs():
                    if att != '_FillValue':
                        var_out.setncattr(att, var_in.getncattr(att))

        # Write data from superobs_data
        for i, superob in enumerate(superobs_data):
            for (grp_name, var_name), value in superob.items():
                if (isinstance(value, (int, float, np.number)) and np.isnan(value)) or value == '':
                    continue  # Skip invalid values
                var_out = f_out.groups[grp_name].variables[var_name]
                try:
                    # If the variable is a string type, convert the value to string
                    if var_out.dtype == str:
                        var_out[i] = str(value)
                    else:
                        var_out[i] = value
                except Exception as e:
                    print(f"Error writing {grp_name}/{var_name} at index {i}: {e}")

        # Add processing history attribute
        f_out.setncattr('processing_history',
                        f'VAD thinning: averaging every {AVG_GROUP_SIZE} observations vertically, hmax={MAX_HEIGHT}m')


def main():
    args = parse_args()
    with Dataset(args.input, 'r') as ds:
        superobs_data = thin_vad_obs(ds, station_filter=args.station,
                                     vad_near_analtime=args.vad_near_analtime)
        write_ioda(args.output, superobs_data, ds)
    print(f"Wrote thinned VAD winds to {args.output}, {len(superobs_data)} superobservations created.")


if __name__ == '__main__':
    main()
