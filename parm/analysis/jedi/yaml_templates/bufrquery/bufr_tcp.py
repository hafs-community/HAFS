#!/usr/bin/env python3
import sys
import argparse
import numpy as np
import netCDF4 as nc
from datetime import datetime

def parse_tcvitals(filepath):
    """
    Parses a tcvitals text file and extracts arrays for IODA conversion.
    Based on standard TC Vitals format and the provided read_tcps.f90 logic.
    """
    lats = []
    lons = []
    times = []
    station_ids = []
    pressures = []

    with open(filepath, 'r') as f:
        for line in f:
            parts = line.split()
            if len(parts) < 11:
                continue
            
            date_element = next((p for p in parts if ':' in p), None)
            if date_element:
                date_part = date_element.split(':')[-1]
                idx = parts.index(date_element)
                time_part = parts[idx + 1]
                lat_str = parts[idx + 2]
                lon_str = parts[idx + 3]
                # Shift elements up to pull pressure correctly later
                pmin_hpa = float(parts[idx + 6]) 
            else:
                # Standard case
                date_part = parts[3]
                time_part = parts[4]
                lat_str = parts[5]
                lon_str = parts[6]
                pmin_hpa = float(parts[9])

            # 1. Center and Storm ID (e.g., NHC_10E or NHC_02L)
            center = parts[0]
            storm_id = parts[1]
            station_ids.append(f"{center}_{storm_id}")
            
            # 2. Date and Time (YYYYMMDD HHMM)
            # handle cases like ':20240924'
            dt_str = date_part + time_part
            dt = datetime.strptime(dt_str, '%Y%m%d%H%M')
            # Convert to epoch seconds for IODA MetaData/dateTime
            epoch_sec = int((dt - datetime(1970, 1, 1)).total_seconds())
            times.append(epoch_sec)
            
            # 3. Latitude and Longitude (e.g., 169N -> 16.9, 0993W -> -99.3)
            lat = float(lat_str[:-1]) / 10.0
            if lat_str[-1] == 'S':
                lat = -lat
                
            lon = float(lon_str[:-1]) / 10.0
            if lon_str[-1] == 'W':
                lon = -lon
                
            lats.append(lat)
            lons.append(lon)
            
            # 4. Central Pressure (e.g., 0973 -> 973 hPa)
            # IODA standard is usually Pascals, so multiply by 100
            pressures.append(pmin_hpa * 100.0)

    return {
        'lats': np.array(lats, dtype=np.float32),
        'lons': np.array(lons, dtype=np.float32),
        'times': np.array(times, dtype=np.int64),
        'station_ids': np.array(station_ids, dtype=object),
        'pressures': np.array(pressures, dtype=np.float32)
    }

def parse_atcf_fhr06(filepath):
    """
    Parses an ATCF file and extracts the lat/lon for forecast hour 06.
    Returns a dictionary mapping the storm ID (e.g. '02L') to (lat, lon).
    """
    atcf_data = {}
    # Mapping ATCF basin codes to tcvitals suffix letters
    basin_map = {'AL': 'L', 'EP': 'E', 'CP': 'C', 'WP': 'W', 'SH': 'S', 'IO': 'A', 'LS': 'S'}
    
    with open(filepath, 'r') as f:
        for line in f:
            parts = [p.strip() for p in line.split(',')]
            if len(parts) < 8:
                continue
                
            basin = parts[0]
            cyc_num = parts[1]
            fhr_str = parts[5]
            lat_str = parts[6]
            lon_str = parts[7]
            
            try:
                fhr = int(fhr_str)
            except ValueError:
                continue
                
            if fhr == 6:
                # Parse lat
                lat = float(lat_str[:-1]) / 10.0
                if lat_str[-1] == 'S': lat = -lat
                
                # Parse lon
                lon = float(lon_str[:-1]) / 10.0
                if lon_str[-1] == 'W': lon = -lon
                
                # Construct matching storm_id, e.g. ATCF AL & 02 -> '02L'
                suffix = basin_map.get(basin.upper(), basin.upper()[-1] if basin else '')
                cyc_key = f"{int(cyc_num):02d}{suffix}"
                
                atcf_data[cyc_key] = (lat, lon)
                
    return atcf_data

def get_domain_bounds(domain_file):
    """
    Reads the domain NetCDF file and returns the min/max lat and lon.
    Also detects if the domain longitudes are 0-360 or -180 to 180.
    """
    with nc.Dataset(domain_file, 'r') as ds:
        # Load arrays (ignoring masked missing values if any)
        domain_lats = ds.variables['geolat'][:]
        domain_lons = ds.variables['geolon'][:]
        
        min_lat, max_lat = np.min(domain_lats), np.max(domain_lats)

        lons_360 = domain_lons % 360
        lons_180 = (domain_lons + 180) % 360 - 180
        
        span_360 = np.max(lons_360) - np.min(lons_360)
        span_180 = np.max(lons_180) - np.min(lons_180)
        if span_180 < span_360:
            min_lon, max_lon = np.min(lons_180), np.max(lons_180)
            use_360 = False
        else:
            min_lon, max_lon = np.min(lons_360), np.max(lons_360)
            use_360 = True

    return min_lat, max_lat, min_lon, max_lon, use_360

def filter_by_domain(data, bounds):
    """
    Filters the data dictionary keeping only points within the domain bounds.
    """
    min_lat, max_lat, min_lon, max_lon, use_360 = bounds
    
    lats = data['lats']
    if use_360:
        check_lons = data['lons'] % 360
    else:
        check_lons = (data['lons'] + 180) % 360 - 180
 
    # Create a boolean mask of points inside the bounding box
    mask = (lats >= min_lat) & (lats <= max_lat) & (check_lons >= min_lon) & (check_lons <= max_lon)
   
    print(f"min lon {min_lon}, max lon {max_lon}") 
    # Apply the mask to all data arrays
    filtered_data = {
        'lats': data['lats'][mask],
        'lons': data['lons'][mask],
        'times': data['times'][mask],
        'station_ids': data['station_ids'][mask],
        'pressures': data['pressures'][mask]
    }
    
    kept_count = np.sum(mask)
    return filtered_data, kept_count

def write_ioda_netcdf(data, out_filepath):
    """
    Writes the extracted data into an IODA-compliant NetCDF4 file.
    """
    nlocs = len(data['lats'])
    
    # Create NetCDF file
    rootgrp = nc.Dataset(out_filepath, 'w', format='NETCDF4')
    
    # 1. Global Attributes
    rootgrp.date_time = datetime.now().strftime('%Y-%m-%dT%H:%M:%SZ')
    rootgrp.nlocs = np.int32(nlocs)
    rootgrp.history = "Generated by tcvitals2ioda.py"
    rootgrp.source = "TC Vitals"
    rootgrp._ioda_layout_version = np.int32(0)
    rootgrp._ioda_layout = "ObsGroup"
    
    # 2. Dimensions
    rootgrp.createDimension('Location', nlocs)
    
    # 3. Create Groups
    meta_grp = rootgrp.createGroup('MetaData')
    obs_grp = rootgrp.createGroup('ObsValue')
    err_grp = rootgrp.createGroup('ObsError')
    qm_grp = rootgrp.createGroup('QualityMarker')
    type_grp = rootgrp.createGroup('ObsType')

    # 4. MetaData Variables
    # Latitude
    var_lat = meta_grp.createVariable('latitude', 'f4', ('Location',))
    var_lat.units = 'degrees_north'
    var_lat.long_name = 'Latitude'
    var_lat[:] = data['lats']

    # Longitude
    var_lon = meta_grp.createVariable('longitude', 'f4', ('Location',))
    var_lon.units = 'degrees_east'
    var_lon.long_name = 'Longitude'
    var_lon[:] = data['lons']

    # station Elevation
    var_hgt = meta_grp.createVariable('stationElevation', 'f4', ('Location',))
    var_hgt.units = 'm'
    var_hgt.long_name = 'station height'
    var_hgt[:] = 0
    # Height
    var_hgt = meta_grp.createVariable('height', 'f4', ('Location',))
    var_hgt.units = 'm'
    var_hgt.long_name = 'station height'
    var_hgt[:] = 0

    # DateTime
    var_time = meta_grp.createVariable('dateTime', 'i8', ('Location',))
    var_time.units = 'seconds since 1970-01-01T00:00:00Z'
    var_time.long_name = 'Datetime'
    var_time[:] = data['times']

    # Station ID (String array)
    var_stid = meta_grp.createVariable('stationIdentification', str, ('Location',))
    var_stid.long_name = 'Station Identification'
    # For netCDF4 string variables, we write the numpy object array directly
    var_stid[:] = data['station_ids']

    # 5. ObsValue Variable (Central Pressure)
    var_pmin = obs_grp.createVariable('stationPressure', 'f4', ('Location',))
    var_pmin.units = 'Pa'
    var_pmin.long_name = 'Sea Level Pressure'
    var_pmin[:] = data['pressures']

    # 6. ObsError Variable
    var_err = err_grp.createVariable('stationPressure', 'f4', ('Location',))
    var_err.units = 'Pa'
    var_err[:] = np.full(nlocs, 75.0, dtype=np.float32)

    # 7. PreQC/QualityMarker Variable
    var_qc = qm_grp.createVariable('stationPressure', 'i4', ('Location',))
    var_qc[:] = np.zeros(nlocs, dtype=np.int32)

    # 8. ObsType Variable <-- Added ObsType variable mapping to 112
    var_type = type_grp.createVariable('stationPressure', 'i4', ('Location',))
    var_type.long_name = 'Observation Type'
    var_type[:] = np.full(nlocs, 112, dtype=np.int32)

    # Close file to write to disk
    rootgrp.close()
    print(f"Successfully converted {nlocs} records to IODA NetCDF: {out_filepath}")

def main():
    parser = argparse.ArgumentParser(description="Convert tcvitals text file to IODA NetCDF format.")
    parser.add_argument('input_file', type=str, help="Path to the input tcvitals file")
    parser.add_argument('output_file', type=str, help="Path to the output IODA NetCDF file")
    parser.add_argument('--atcf_file', type=str, help="Optional path to an ATCF file to override lat/lon with FHR 06 values", default=None)
    parser.add_argument('--domain_file', type=str, help="Optional path to a model domain NetCDF file to filter obs geographically", default=None)
    
    args = parser.parse_args()

    try:
        parsed_data = parse_tcvitals(args.input_file)
        if len(parsed_data['lats']) == 0:
            print("No valid tcvitals records found in the input file.")
            sys.exit(0)
            
        # Replace lat/lons with hour 06 predictions from ATCF if provided
        if args.atcf_file:
            atcf_coords = parse_atcf_fhr06(args.atcf_file)
            
            for i, stid in enumerate(parsed_data['station_ids']):
                # Extract the standard storm ID (e.g. from NHC_02L -> 02L)
                storm_id = stid.split('_')[-1]
                cyc_num_only = ''.join(filter(str.isdigit, storm_id))
                
                # Check for an exact storm ID match (e.g. '02L' == '02L')
                if storm_id in atcf_coords:
                    parsed_data['lats'][i] = atcf_coords[storm_id][0]
                    parsed_data['lons'][i] = atcf_coords[storm_id][1]
                    print(f"Updated {stid} with ATCF FHR 06 lat/lon: {atcf_coords[storm_id]}")
                else:
                    # Fallback check: match just the digits (e.g., '02' == '02')
                    fallback_matches = [k for k in atcf_coords if ''.join(filter(str.isdigit, k)) == cyc_num_only]
                    if fallback_matches:
                        match_key = fallback_matches[0]
                        parsed_data['lats'][i] = atcf_coords[match_key][0]
                        parsed_data['lons'][i] = atcf_coords[match_key][1]
                        print(f"Updated {stid} with ATCF FHR 06 lat/lon (matched by number '{cyc_num_only}'): {atcf_coords[match_key]}")

        # Filter observations by domain if domain_file is provided
        if args.domain_file:
            bounds = get_domain_bounds(args.domain_file)
            original_count = len(parsed_data['lats'])
            
            parsed_data, kept_count = filter_by_domain(parsed_data, bounds)
            print(f"Domain filter applied: Kept {kept_count} of {original_count} observations.")
            
            if kept_count == 0:
                print("No observations fell within the provided domain bounds. Exiting.")
                sys.exit(0)
                        
        write_ioda_netcdf(parsed_data, args.output_file)
        
    except Exception as e:
        print(f"Error processing file: {e}")
        sys.exit(1)

if __name__ == '__main__':
    main()
