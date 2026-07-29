#!/usr/bin/env python3
import netCDF4
import numpy as np
import argparse

def process_netcdf(input_file, output_file):
    # Open the source file for reading and the destination file for writing
    with netCDF4.Dataset(input_file, 'r') as src, netCDF4.Dataset(output_file, 'w', format='NETCDF4') as dst:
        
        # 1. Copy global attributes
        dst.setncatts(src.__dict__)

        # 2. Copy dimensions (Skip 'stationIdlength')
        for name, dimension in src.dimensions.items():
            if name == 'stationIdlength':
                continue
            dst.createDimension(name, (len(dimension) if not dimension.isunlimited() else None))

        # 3. Copy Root Variables (like 'Location')
        for name, var in src.variables.items():
            fill_val = getattr(var, '_FillValue', None)
            out_var = dst.createVariable(name, var.datatype, var.dimensions, fill_value=fill_val)
            # Copy all attributes except _FillValue (since it's set in createVariable)
            out_var.setncatts({k: var.getncattr(k) for k in var.ncattrs() if k != '_FillValue'})
            out_var[:] = var[:]

        # 4. Copy Groups and their Variables
        for grp_name, src_grp in src.groups.items():
            dst_grp = dst.createGroup(grp_name)
            dst_grp.setncatts(src_grp.__dict__) # Copy group attributes

            for var_name, var in src_grp.variables.items():
                # --- SPECIAL HANDLING FOR stationIdentification ---
                if grp_name == 'MetaData' and var_name == 'stationIdentification':
                    char_data = var[:]
                    
                    # Convert the 2D byte/char array into a list of Python strings
                    try:
                        str_data = netCDF4.chartostring(char_data)
                        str_array = np.array([str(s).strip() for s in str_data], dtype=object)
                    except Exception:
                        # Fallback parsing just in case chartostring fails on your specific formatting
                        str_array = np.array(
                            ["".join([c.decode('utf-8') if hasattr(c, 'decode') else str(c) for c in row]).strip() 
                             for row in char_data], dtype=object
                        )
                    
                    # Create the new variable as a string type ('str'), mapped only to ('Location',)
                    out_var = dst_grp.createVariable(var_name, str, ('Location',))
                    out_var.setncatts({k: var.getncattr(k) for k in var.ncattrs() if k != '_FillValue'})
                    out_var[:] = str_array
                
                # --- STANDARD HANDLING FOR ALL OTHER VARIABLES ---
                else:
                    fill_val = getattr(var, '_FillValue', None)
                    out_var = dst_grp.createVariable(var_name, var.datatype, var.dimensions, fill_value=fill_val)
                    out_var.setncatts({k: var.getncattr(k) for k in var.ncattrs() if k != '_FillValue'})
                    out_var[:] = var[:]

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Convert stationIdentification to a 1D string array.")
    parser.add_argument('-i', '--input', required=True, help='Input NetCDF file (e.g., superob_radar.nc)')
    parser.add_argument('-o', '--output', required=True, help='Output NetCDF file')
    args = parser.parse_args()
    
    print(f"Processing {args.input}...")
    process_netcdf(args.input, args.output)
    print(f"Success! Output saved to {args.output}")
