v2.3.0- released 11/15/2026 (tentatively)

Release Notes: HAFS v2.3.0 (Minor)

## Model: Hurricane Analysis and Forecast System (HAFS)

Version: hafs.v2.3.0

Location on GitHub server: https://github.com/hafs-community/HAFS/tree/production/hafs.v2.3
```
git clone -b production/hafs.v2.3 --recursive https://github.com/hafs-community/HAFS.git ./hafs.v2.3.0
```
Implementation date: November 15, 2026 1200UTC (tentatively)

Purpose: As part of Global Forecast System version 17 (GFSv17) downstream
upgrade, adopt GFSv17 input data directory and file naming changes. Meanwhile,
optimize and finetune model physics and dynamics with GFSv17 input data to
provide skillful hurricane track and intensity forecast guidance.

Developed by: NOAA/NWS Office of Modeling and Development (OMD) and the Unified
Forecast System (UFS) Hurricane Application Team. See currently active HAFS
developers
[here](https://docs.google.com/presentation/d/1rvbBUpiRj4Lw-6agjQ0GYQyY2_H2aL6MF0gjOoqPR04/edit?usp=sharing).

Runs on WCOSS2

Input: Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis
for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for
atmospheric lateral boundary conditions and atmospheric forcing for oceanic and
wave model components. Global RTOFS for oceanic initial and boundary conditions
for MOM6. GFS/GDAS global wave analysis/forecast for HFSA wave component
initial and lateral boundary conditions. OBSPROC observational data for HAFS
data assimilation.

Output: ATCF track forecast files, 4x/day at 00Z/06Z/12Z/18Z.

Primary users: NHC, JTWC, MAG, and private sectors.

Where to find output:
- on WCOSS2
  - HFSA: `/lfs/h1/ops/prod/com/hafs/v2.3/hfsa.yyyymmdd/hh`
  - HFSB: `/lfs/h1/ops/prod/com/hafs/v2.3/hfsb.yyyymmdd/hh`
- on servers: NCEP ftp server, NOMADS, NWS Gateway/SBN

### 1. Background:

The Global Forecast System (GFS) serves as the parent model, providing the
Initial Conditions (ICs) and Lateral Boundary Conditions (LBCs) for HAFS. As
GFS version 16 (GFSv16) is scheduled to be upgraded to GFS version 17 (GFSv17),
HAFSv2.2 must be updated accordingly and upgraded to HAFSv2.3 to:
 - Support the new input data directory structure and file naming conventions
   introduced in GFSv17.
 - Recalibrate model dynamics and physics to ensure compatibility with GFSv17.

Scientific enhancements aimed at improving tropical cyclone track and intensity
forecast skill include:
 - Adjust the linear scheme limiting factor (lim_fac) for the horizontal
   advection scheme used in model dynamics.
 - Update the Scale-Aware Simplified Arakawa-Schubert (SA-SAS) convection
   scheme with finetuned detrainment conversion parameters (c1_deep/shal),
   updated momentum transport reduction factors (pgcon_deep/shal), and reduced
   convective adjustment timescales (cat_adj_deep/shal).
 - Incorporate a bug fix related to solar zenith angle calculation in some
   satellite products in the Unified Post Processor (UPP).

Software dependencies: remain unchanged from HAFSv2.2

Output changes: remain unchanged from HAFSv2.2

### 2. Computer resource information: remain unchanged from HAFSv2.2
 - Computation resource updates: (no change, same as HAFSv2.2)
   - More instructions on using/testing alternative computation resources for
     the HAFS forecast job can be found [here](https://docs.google.com/document/d/1IX9Gu9Dbnl45sbPqeXjaJlpnzeivTz81I1VvQ1Ey6vU/edit?usp=sharing)
 - List of the module versions used in HAFSv2.3: See [here](https://github.com/hafs-community/HAFS/blob/production/hafs.v2.3/versions/run.ver)
 - Data retention for files in
   `/lfs/h1/ops/prod/com/hafs/v2.3/hfsa.yyyymmdd/hh` and
   `/lfs/h1/ops/prod/com/hafs/v2.3/hfsb.yyyymmdd/hh`
   - Disk space usage in com directory: no change, same as HAFSv2.2
   - HPSS disk requirement: no change, same as HAFSv2.2
   - Preferred to keep the files in COM for 7 days
 - Input datasets (no change, same as HAFSv2.2)
   - Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis
     for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h)
     for atmospheric lateral boundary conditions and atmospheric forcing for oceanic
     and wave model components. Global RTOFS for oceanic initial and boundary
     conditions for the ocean model component. GFS/GDAS global wave
     analysis/forecast for HFSA wave component initial and lateral boundary
     conditions. OBSPROC observational data for HAFS data assimilation.
 - Input DCOM dependencies (no change, same as HAFSv2.2)
   - The following scripts require input files from DCOM directory:
     - `./scripts/exhafs_obs_prep.sh`: to decode/dump TDR/HDOB/TEMPDROP data
     - `./scripts/exhafs_msg_check.py`: to check the message files are properly
       created by checking the original NHC/JTWC message files in the DCOM directory
     - `./ush/setup_hurricane`: for SDM to set up hurricane message files by
       extracting info from the original NHC/JTWC message files in the DCOM directory

### 3. Pre-implementation testing requirements:
 - All jobs should be tested as part of this implementation.
 - Does this change require a 30-day evaluation (for major implementations only)?
   - No, but should be tested as part of GFSv17 downstream.
 - Who are the suggested evaluators?
   - NHC and JTWC for model products and MAG for GEMPAK files.

### 4. Dissemination information:
 - The ATCF files to NHC for NATL, EPAC, and CPAC basin storms, and JTWC for
   all global basins, including SH basins, GRIB files for NOMADS and GEMPAK
   files for MAG
 - The users are NHC, JTWC, MAG and NOMADS
 - All the files in COM directory should be transferred from PROD WCOSS2 to DEV WCOSS2
 - None of the codes are proprietary and there is no restricted data

### 5. Archive to HPSS
 - All the files in the COM directory are preferred to be archived in 5-year retention HPSS directory

### 6. Implementation instructions: For NCO only, please see [here](https://docs.google.com/document/d/1jIYl57IKqbgUizRHRUslH826mS2X3KUzhR-eYP9sciQ/edit?usp=sharing)
