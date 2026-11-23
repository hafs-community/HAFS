# Running HAFS from ERA5 reanalysis

How to cold start a HAFS forecast from ERA5 instead of GFS, for storms without
GFS/GDAS data in the current format (HAFS reanalysis subproject, branch
`feature/hafsv2p2p1b_reanalysis`).

**What this runs:** atm_ic, atm_lbc, atm_init, atm_vi (vortex initialization)
and the forecast, from ERA5 analyses for both the initial and the lateral
boundary conditions.
**What it does not run yet:** data assimilation, ocean coupling and wave
coupling (their GDAS, RTOFS and GFS-wave inputs do not exist for these cases).

Tested so far: 12-h forecast (2005), Charley 2004081012 03L (126 h, CDS data),
Andrew 1992 (ARCO data and best-track vitals, in progress).

## 1. Python dependencies

Use one conda (or other Python 3.6+) environment for the two ERA5 scripts.

| Module | Needed by | Notes |
|---|---|---|
| `numpy` | converter, ARCO download | |
| `netCDF4` | converter, ARCO download | |
| `eccodes` | converter | Python module plus the ecCodes library (`conda install -c conda-forge python-eccodes`) |
| `numcodecs` | ARCO download | `pip install numcodecs --user`; the `blosc` module works as an alternative |
| `cdsapi` | CDS download only | Also needs a CDS account, the ERA5 license accepted, and the key in `~/.cdsapirc` |

`xarray`, `zarr` and `gcsfs` are **not** needed. Quick check:

```bash
python3 -c "import numpy, netCDF4, eccodes, numcodecs; print('ok')"
```

## 2. Download the ERA5 data

Download every day the forecast touches (cycle day through cycle + forecast
length). One pair of files is written per day: `ERA5_pl_YYYYMMDD.nc` and
`ERA5_sfc_YYYYMMDD.nc`. Existing files are skipped, so an interrupted download
can simply be rerun.

**Recommended: ARCO-ERA5** (public copy in Google Cloud Storage; no account,
no queue; 1940 to about five months ago, preliminary data after that):

```bash
nohup ush/hafs_era5_ic_download.py -s arco -o $ERA5dir 19920821-19920826 > era5_download.log 2>&1 &
```

- `-n` prints what would be downloaded without downloading.
- `-j N` sets the number of parallel transfers (default 4).
- About 5.5 GB per pressure-level file at the default 3-hourly interval. The
  files are larger than the CDS ones because of lighter compression; the
  content is the same apart from the two fields below.
- ARCO-ERA5 has no rain and snow water content. HAFS does not read them from
  GRIB2 input, so this has no effect (the converter logs
  `RWMR: not in input; skipped` and `SNMR: not in input; skipped`).

**Alternative: Copernicus Climate Data Store (CDS)**, the default source:

```bash
nohup ush/hafs_era5_ic_download.py -j 2 -o $ERA5dir 20040810-20040815 > era5_download.log 2>&1 &
```

- Requests wait in the CDS queue; a 5-6 day case can take more than a day.
- `-j 2` runs one pressure-level and one surface request at a time. The CDS
  limits queued requests per dataset; a request rejected for that limit is
  resubmitted after `--retry-wait` seconds (a warning is logged each time).
- Stopping the script does not cancel requests already submitted. Delete
  leftovers on the CDS "Your requests" page, or they keep blocking the queue.

Files from the two sources can be mixed in one directory. The process runs on
the login node where it was started; note the node name to check on it later.

## 3. Convert to pseudo-GFS GRIB2 files

```bash
ush/hafs_era5_to_gfsgrib2.py --pl $ERA5dir/ERA5_pl_*.nc --sfc $ERA5dir/ERA5_sfc_*.nc \
    --cycle 1992082100 --fhrs 0:126:3 --outdir $COMera5
```

- `--cycle` is the HAFS cycle (several can be given); `--fhrs` is
  `start:end:step` with the forecast length and the boundary interval.
- `--gfsver` must match `GFSVER` of the HAFS configuration. The default,
  `PROD2026`, matches the default of this branch; use `PROD2021` only if the
  configuration sets it.
- Each output file holds the ERA5 analysis valid at cycle + forecast hour:
  353 records from ARCO data, 427 from CDS data.
- The downloaded NetCDF files can be deleted once the GRIB2 files exist.

## 4. TC vitals

The launcher reads `syndat_tcvitals.YYYY` from the `[dir] syndat` directory.

- Years in the archive (e.g., 2004, 2005): nothing to do.
- Earlier years: generate the file from the ATCF best track (b-deck):

```bash
ush/hafs_bdeck_to_tcvitals.py -o $VITALSdir/syndat_tcvitals.1992 bal041992.dat
```

Then add `dir.syndat=$VITALSdir` to the run command below. For old best
tracks the storm size values (radius of maximum wind, outermost closed
isobar) are estimates; `--poci` and `--roci` change the defaults.

## 5. Run HAFS

Add the ERA5 overlay after the base configuration, and point it at the
pseudo-GFS directory, e.g., in `rocoto/cronjob_hafs.sh`:

```bash
./run_hafs.py ${opts} 1992082100 04L HISTORY config.EXPT=${EXPT} \
    ../parm/hfsa.conf ../parm/examples/hafs_era5_init.conf \
    dir.COMera5=$COMera5 dir.syndat=$VITALSdir
```

(Leave out `dir.syndat` for years in the archive.) The overlay
`parm/examples/hafs_era5_init.conf`:

- reads the pseudo-GFS files (`ictype=bctype=gfsgrib2_0p25`, `NBDYHRS=3`);
- sets `gfs_datadep=no`, so atm_ic and atm_lbc do not wait for GFS files;
- turns off DA, FGAT, ocean and wave;
- requests only the atmosphere tasks for the forecast job
  (`all_tasks={forecast/atm_tasks}`);
- uses `nstf_name=2,1,0,0,0`, since ERA5 has no NSST fields.

`NBDYHRS` must be a multiple of the download `--hour-step` (default 3).

## Known limitations

- Soil moisture is passed from the ECMWF land model without rescaling for
  Noah; not yet evaluated in long runs.
- The multistorm workflow still has the GFS file dependencies.
- HFSB on Ursa needs a `FORECAST_RESOURCES` entry for its atmosphere-only
  task count.
- A GFS-versus-ERA5 comparison for a recent storm (Beryl 2024) is planned to
  separate the effect of the ERA5 input from model behaviour.
