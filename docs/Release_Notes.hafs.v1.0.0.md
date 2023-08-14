v1.0.0- released 07/01/2023

Release Notes:  HAFS v1.0.0 (Major)

## Model: Hurricane Analysis and Forecast System (HAFS)

Version: hafs.v1.0.0

Location on GitHub server: https://github.com/hafs-community/HAFS/tree/production/hafs.v1
```
git clone -b production/hafs.v1 --recursive  https://github.com/hafs-community/HAFS.git ./hafs.v1.0.0
```
Implementation date: July 01, 2023 1200UTC

Purpose: Run HAFS.v1 HFSA configuration to provide hurricane track and intensity forecast guidance to 5.25 days over all global oceanic basins, including NATL, EPAC, CPAC, WPAC, NIO, and SH basins. Run HAFS.v1 HFSB configuration to provide hurricane track and intensity forecast guidance for 5.25 days over NHC/CPHC basins, including NATL, EPAC, CPAC basins.

Developed by: EMC and the UFS Hurricane Application Team. See currently active HAFS developers [here](https://docs.google.com/presentation/d/1ws6RlY2GNdoEYQvFuFoJWynGVZIS3A56KuD0VeMSw-k/edit?usp=sharing).

Runs on WCOSS2

Input: Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for atmospheric lateral boundary conditions and atmospheric forcing for oceanic and wave model components. Global RTOFS for oceanic initial and boundary conditions for HYCOM. GFS/GDAS global wave analysis/forecast for HFSA wave component initial and lateral boundary conditions. OBSPROC observational data for HAFS data assimilation.
	
Output: ATCF track forecast files, 4x/day at 00Z/06Z/12Z/18Z. 

Primary users: NHC, CPHC, JTWC, MAG, and private sectors.

Where to find output: 
- on WCOSS2 
  - HFSA: `/lfs/h1/ops/prod/com/hafs/v1.0/hfsa.yyyymmdd/hh`
  - HFSB: `/lfs/h1/ops/prod/com/hafs/v1.0/hfsb.yyyymmdd/hh`
- on servers: NCEP ftp server, NOMADS, NWS Gateway/SBN

### 1. Background:

Hurricane Analysis and Forecast System (HAFS), as the Unified Forecast System (UFS) hurricane application, is the FV3 (Finite Volume Cubed-Sphere Dynamical Core) based multi-scale model and data assimilation system capable of providing tropical cyclone (TC, including hurricane and typhoon) analyses and forecasts of the inner core structure key to improving storm size and intensity predictions, as well as the large-scale environment that is known to influence the TC’s motion. The HAFS development targets an operational data assimilation and modeling system, capable of providing reliable, robust and skillful model guidances for TC track, intensity (including rapid intensification), size, structure, rainfall and cyclone genesis, as well as for TC associated storm surge, sea surface waves, and tornadoes. HAFS is a community-based coupled earth modeling system specially calibrated for hurricane prediction with TC dynamics and physics, sophisticated vortex initialization and advanced inner-core data assimilation techniques, and various air-sea interaction processes.

HAFSv1.0 will replace NCEP's current operational hurricane forecast systems, HWRF and HMON in the 2023 hurricane season. The ultimate goal of this project is to implement a UFS-based atmospheric model, coupled with ocean and wave models, with a multi-scale data assimilation system, HAFSv1.0 into operations in 2023. The system will provide comparable and/or improved tropical cyclone track and intensity forecast guidance in all global oceanic basins, as demonstrated through evaluation of retrospective and real time experiments compared against the current operational HWRF/HMON systems (HWRFv13.2.5 and HMONv3.2.2). 

 - Scientific changes to improve track and intensity forecast skill:
   - UFS-FV3 dynamic core, Sync with latest ufs-weather-model repository
   - Moving nest capability
   - Vortex initialization for nest domain
   - Inner-core data assimilation
   - TC-calibrated model physics, Surface, PBL and Micro Physics
   - CMEPS-based ocean coupling with an extended HYCOM ocean domain
   - Parent domain coupled with HYCOM, downscaling SST to moving nest
   - Improved write-grid component, capable of outputting two domains
   - Updated GFDL vortex tracker
   - Updated workflow
 - Software dependencies: See [here](https://github.com/hafs-community/HAFS/blob/production/hafs.v1/versions/run.ver)
 - Output changes: See [here](https://docs.google.com/presentation/d/1DlPcBQeymdG8d8j95aqbeXYFTDErkJvodfj8d_qmvls/edit?usp=sharing)

### 2. Computer resource information: See [here](https://docs.google.com/spreadsheets/d/15Jiezlv8W-_sm3STh2YoCiUfvZvYqoyVfGXUNghWoBQ/edit?usp=sharing)
 - Resource changes:
   - forecast: changes from 21 nodes/2688 cores to 49 nodes/6272 cores.
   - analysis (GSI): changes from 3 nodes/384 cores to 10 nodes/1280 cores
 - All Shell/Python scripts in ./jobs, ./scripts, ./ush, have been re-written 
 - The configuration files in ./parm are completely different from original HWRF/HMON
 - List of the module versions used in HAFS: See [here](https://github.com/hafs-community/HAFS/blob/production/hafs.v1/versions/run.ver)
 - Data retention for files in `/lfs/h1/ops/prod/com/hafs/v1.0/hfsa.yyyymmdd/hh` and `/lfs/h1/ops/prod/com/hafs/v1.0/hfsb.yyyymmdd/hh`
   - Disk space usage in com directory, ~145/140 GB (HFSA/HFSB) per cycle 
   - Preferred to keep the files in COM for 7 days 
 - Input datasets 
   - Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for atmospheric lateral boundary conditions and atmospheric forcing for oceanic and wave model components. Global RTOFS for oceanic initial and boundary conditions for HYCOM. GFS/GDAS global wave analysis/forecast for HFSA wave component initial and lateral boundary conditions. OBSPROC observational data for HAFS data assimilation.
 - Input DCOM dependencies
   - The following scripts require input files from DCOM directory:
     - `./scripts/exhafs_obs_prep.sh`: to decode/dump TDR/HDOB/TEMPDROP data  
     - `./scripts/exhafs_msg_check.py`: to check the message files are properly created by checking the original NHC/JTWC message files in the DCOM directory.
     - `./ush/setup_hurricane`: for SDM to set up hurricane message files by extracting info from the original NHC/JTWC message files in the DCOM directory.

### 3. Pre-implementation testing requirements: 
 - All jobs should be tested as part of this implementation. 
 - Does this change require a 30-day evaluation (for major implementations only)?
   - No but all changes have been tested successfully using available input as per the following list: [IT tests list](https://docs.google.com/presentation/d/1qttu6HhZC3I2yEWLIcsTM7BUGxGyevhtZMUU61kSyYI/edit?usp=sharing)
 - Who are the suggested evaluators?
   - NHC, CPHC, and JTWC for model products and MAG for GEMPAK files.

### 4. Dissemination information:
 - The ATCF files to NHC (and CPHC for Central Pacific Basin storms), and JTWC (for all global basins, including SH basins), GRIB files for NOMADS and GEMPAK files for MAG
 - The users are NHC, CPHC, JTWC, MAG and NOMADS
 - All the files in COM directory should be transferred from PROD WCOSS2 to DEV WCOSS2
 - None of the codes are proprietary and there is no restricted data

### 5. Archive to HPSS
 - All the files in the COM directory are preferred to be archived in 5-year retention HPSS directory

### 6. Implementation instructions: See [here](https://docs.google.com/document/d/1ttYRyooodJ6SMysGzvQiqcSZMJId_h5wCzdSuQdCREQ/edit?usp=sharing)
