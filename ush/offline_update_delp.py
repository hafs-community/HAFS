#!/usr/bin/env python3
# offline_compute_delp_inc.py

import argparse
import sys
import numpy as np
from netCDF4 import Dataset

def read_bk(akbk_path: str, bk_var: str) -> np.ndarray:
    with Dataset(akbk_path, "r") as ds:
        if bk_var not in ds.variables:
            raise KeyError(f"Missing '{bk_var}' in {akbk_path}. Vars: {list(ds.variables.keys())}")
        bk = np.asarray(ds.variables[bk_var][...])
    if bk.ndim == 2:
        bk = bk[0, :]
    if bk.ndim != 1:
        raise ValueError(f"Unexpected bk shape {bk.shape}, expected (82,) or (Time,82).")
    return bk

def main() -> int:
    ap = argparse.ArgumentParser(description="Create/overwrite delp increment and optionally apply to a restart file.")
    ap.add_argument("--sfc_inc", required=True, help="inc_jedi.sfc_data.nc (contains ps increment).")
    ap.add_argument("--core_inc", required=True, help="inc_jedi.fv_core.res.nc (target for delp increment).")
    ap.add_argument("--akbk", required=True, help="fv3_akbk file (contains bk).")
    
    # New argument for the restart file
    ap.add_argument("--rst_file", help="Optional: fv_core.res.nc restart file to update (delp = delp + delp_inc).")

    ap.add_argument("--ps_var", default="ps", help="ps variable name in sfc_inc.")
    ap.add_argument("--bk_var", default="bk", help="bk variable name in akbk.")
    ap.add_argument("--delp_var", default="delp", help="delp variable name.")

    ap.add_argument("--scale", type=float, default=1.0, help="Optional multiplier applied to delp_inc.")
    ap.add_argument("--dry_run", action="store_true", help="Compute and report stats, do not write.")
    args = ap.parse_args()

    # 1. Read ps increment
    with Dataset(args.sfc_inc, "r") as sfc:
        if args.ps_var not in sfc.variables:
            raise KeyError(f"Missing '{args.ps_var}' in {args.sfc_inc}.")
        ps = np.asarray(sfc.variables[args.ps_var][...])
        ps_units = getattr(sfc.variables[args.ps_var], "units", "")
    if ps.ndim == 2:
        ps = ps[None, :, :]

    # 2. Read bk and form dbk
    bk = read_bk(args.akbk, args.bk_var)
    if bk.size != 82:
        raise ValueError(f"bk length is {bk.size}, expected 82.")
    dbk = bk[1:] - bk[:-1]

    # 3. Compute delp_inc
    # Broadcast dbk(81) across Time, Y, X
    delp_inc = (dbk[None, :, None, None] * ps[:, None, :, :]) * float(args.scale)
    delp_inc = delp_inc.astype(np.float32)

    print(f"delp_inc stats: min={np.nanmin(delp_inc):.6g} max={np.nanmax(delp_inc):.6g}")

    if args.dry_run:
        print("Dry run: exiting without writing.")
        return 0

    # 4. Write increment to core_inc file
    with Dataset(args.core_inc, "r+") as core:
        if args.delp_var not in core.variables:
            v = core.createVariable(args.delp_var, "f4", ("Time", "zaxis_1", "yaxis_1", "xaxis_1"))
            v.long_name = "air_pressure_thickness_increment"
            v.units = "Pa"
        else:
            v = core.variables[args.delp_var]
        v[:, :, :, :] = delp_inc

    # 5. Optional: Apply increment to the restart file
    if args.rst_file:
        print(f"Updating restart file: {args.rst_file}")
        with Dataset(args.rst_file, "r+") as rst:
            if args.delp_var not in rst.variables:
                raise KeyError(f"Variable '{args.delp_var}' not found in restart file {args.rst_file}")
            
            # Read existing delp
            # We assume dimensions match core_inc (Time, zaxis_1, yaxis_1, xaxis_1)
            current_delp = rst.variables[args.delp_var][...]
            
            if current_delp.shape != delp_inc.shape:
                raise ValueError(f"Restart delp shape {current_delp.shape} != inc shape {delp_inc.shape}")

            # Add increment back to background
            rst.variables[args.delp_var][:, :, :, :] = current_delp + delp_inc
            print(f"Successfully added delp_inc to {args.rst_file}")

    return 0

if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as e:
        print(f"ERROR: {e}", file=sys.stderr)
        sys.exit(2)
