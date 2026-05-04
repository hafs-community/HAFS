v2.2.0- released 07/15/2026 (tentatively)

Release Notes: HAFS v2.2.0 (Minor)

## Model: Hurricane Analysis and Forecast System (HAFS)

Version: hafs.v2.2.0

Location on GitHub server: https://github.com/hafs-community/HAFS/tree/production/hafs.v2.2
```
git clone -b production/hafs.v2.2 --recursive https://github.com/hafs-community/HAFS.git ./hafs.v2.2.0
```
Implementation date: July 15, 2026 1200UTC (tentatively)

Purpose: Run HAFSv2.2 HFSA configuration to provide hurricane track and
intensity forecast guidance to 5.25 days over all global tropical cyclone
basins, including NATL, EPAC, CPAC, WPAC, NIO, and SH basins. Run HAFSv2.2 HFSB
configuration to provide hurricane track and intensity forecast guidance for
5.25 days over NHC/CPHC basins, including NATL, EPAC, CPAC basins.

Developed by: EMC and the UFS Hurricane Application Team. See currently active HAFS developers [here](https://docs.google.com/presentation/d/1S-kCYPwKo0puf0RxDYWTU4NTvEzlNEsokG3jdEEvLXA/edit?usp=sharing).

Runs on WCOSS2

Input: Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis
for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for
atmospheric lateral boundary conditions and atmospheric forcing for oceanic and
wave model components. Global RTOFS for oceanic initial and boundary conditions
for MOM6. GFS/GDAS global wave analysis/forecast for HFSA wave component
initial and lateral boundary conditions. OBSPROC observational data for HAFS
data assimilation.

Output: ATCF track forecast files, 4x/day at 00Z/06Z/12Z/18Z.

Primary users: NHC, CPHC, JTWC, MAG, and private sectors.

Where to find output:
- on WCOSS2
  - HFSA: `/lfs/h1/ops/prod/com/hafs/v2.2/hfsa.yyyymmdd/hh`
  - HFSB: `/lfs/h1/ops/prod/com/hafs/v2.2/hfsb.yyyymmdd/hh`
- on servers: NCEP ftp server, NOMADS, NWS Gateway/SBN

### 1. Background:

Hurricane Analysis and Forecast System (HAFS), as the Unified Forecast System
(UFS) hurricane application, is the FV3 (Finite­ Volume Cubed-Sphere Dynamical
Core) based multi-scale model and data assimilation system capable of providing
tropical cyclone (TC, including hurricane and typhoon) analyses and forecasts
of the inner-core storm structure (which is key to improving storm size and
intensity predictions) and the large-scale environment that is known to
influence the TC’s motion. The HAFS development targets an operational data
assimilation and modeling system, capable of providing reliable, robust and
skillful model guidances for TC track, intensity (including rapid
intensification), size, structure, rainfall and cyclone genesis, as well as for
TC associated storm surge, sea surface waves, and tornadoes. HAFS is a
community-based coupled earth modeling system specially calibrated for
hurricane prediction with TC dynamics and physics, sophisticated vortex
initialization and advanced inner-core data assimilation techniques, and
various air-sea interaction processes.

HAFSv2.2 will be an upgrade of NCEP's current operational HAFSv2.1 in the 2026
hurricane season. The system will provide improved and comparable tropical
cyclone track and intensity forecast guidance in all global TC basins, as
demonstrated through evaluation of retrospective and real-time experiments
compared against the two current configurations of operational HAFSv2.1.

 - Scientific changes to improve track and intensity forecast skills include:
   - System and infrastructure upgrade
     - Sync with latest UFS development (as of 02/12/2026)
     - Restart capability for coupled atmosphere-ocean forecast
   - Vortex initialization and data assimilation improvement
     - Adjusted warm cycling intensity threshold from 40 to 30 kt
     - Improved vortex initialization with storm perturbation smoothing
     - 3-Dimensional Incremental Analysis Update (3DIAU) interface unification
       and fine-tuning with the weighting function change from constant to
       Lanczos-like temporal filter and reduced IAU window from 3h to 1h
     - Implement Tail Doppler Radar (TDR) superobbing algorithm
     - Assimilate additional observations: commercial GPS Radio Occultation
       (RO) and Unmanned Aircraft System (UAS) HDOB
   - Physics and dynamics enhancement
     - Implement the Scale-aware 3-Dimensional Turbulent Kinetic Energy (3DTKE)
       Eddy-Diffusivity/Mass-Flux (EDMF) GFS PBL scheme (HFSA)
     - Implement variable mixing length scale limits with wind speed and PBLH
       dependent elmx/rlmx (HFSA)
     - Upgrade from Noah to NoahMP Land Surface Model
     - Transition from RRTMG to RRTMGP for radiation scheme
     - Use GFDL MPv1 for EPAC (HFSB)
   - Ocean and wave coupling advancement
     - Latest MOM6 submodule with updated configure options
     - Updated WW3 submodule (HFSA)
   - Post-processing and product update
     - Updated GFDL vortex tracker
     - New wind gust variable in grib2 outputs

 - Software dependencies: See [here](https://github.com/hafs-community/HAFS/blob/production/hafs.v2.2/versions/run.ver)
 - Output changes: See [here](https://docs.google.com/presentation/d/1wDRl95_i4bdPuiYEoONBm7WRq7s83mEKSfWAckMwuWI/edit?slide=id.g3be52c5d8fc_0_0#slide=id.g3be52c5d8fc_0_0)

### 2. Computer resource information: See [here](https://docs.google.com/spreadsheets/d/1gFPtvQHUTM1IFnMwr5aGddrYML17mJlH0QAtFGrMKUo/edit?usp=sharing)
 - Computation resource updates: See [here](https://docs.google.com/presentation/d/1wDRl95_i4bdPuiYEoONBm7WRq7s83mEKSfWAckMwuWI/edit?slide=id.g3ade4893afa_0_528#slide=id.g3ade4893afa_0_528) (no change, same as HAFSv2.1)
   - More instructions on using/testing alternative computation resources for the HAFS forecast job can be found [here](https://docs.google.com/document/d/1IX9Gu9Dbnl45sbPqeXjaJlpnzeivTz81I1VvQ1Ey6vU/edit?usp=sharing)
 - List of the module versions used in HAFSv2.2: See [here](https://github.com/hafs-community/HAFS/blob/production/hafs.v2.2/versions/run.ver)
 - Data retention for files in `/lfs/h1/ops/prod/com/hafs/v2.2/hfsa.yyyymmdd/hh` and `/lfs/h1/ops/prod/com/hafs/v2.2/hfsb.yyyymmdd/hh`
   - Disk space usage in com directory: no change, same as HAFSv2.1
   - HPSS disk requirement: no change, same as HAFSv2.1
   - Preferred to keep the files in COM for 7 days
 - Input datasets (no change, same as HAFSv2.1)
   - Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for atmospheric lateral boundary conditions and atmospheric forcing for oceanic and wave model components. Global RTOFS for oceanic initial and boundary conditions for the ocean model component. GFS/GDAS global wave analysis/forecast for HFSA wave component initial and lateral boundary conditions. OBSPROC observational data for HAFS data assimilation.
 - Input DCOM dependencies (no change, same as HAFSv2.1)
   - The following scripts require input files from DCOM directory:
     - `./scripts/exhafs_obs_prep.sh`: to decode/dump TDR/HDOB/TEMPDROP data
     - `./scripts/exhafs_msg_check.py`: to check the message files are properly created by checking the original NHC/JTWC message files in the DCOM directory
     - `./ush/setup_hurricane`: for SDM to set up hurricane message files by extracting info from the original NHC/JTWC message files in the DCOM directory

### 3. Pre-implementation testing requirements:
 - All jobs should be tested as part of this implementation.
 - Does this change require a 30-day evaluation (for major implementations only)?
   - No but all changes have been tested successfully using available input as per the following list: [IT tests list](https://docs.google.com/presentation/d/1wDRl95_i4bdPuiYEoONBm7WRq7s83mEKSfWAckMwuWI/edit?slide=id.g3d7613b68b7_10_0#slide=id.g3d7613b68b7_10_0)
 - Who are the suggested evaluators?
   - NHC, CPHC, and JTWC for model products and MAG for GEMPAK files.

### 4. Dissemination information:
 - The ATCF files to NHC (and CPHC for Central Pacific Basin storms), and JTWC (for all global basins, including SH basins), GRIB files for NOMADS and GEMPAK files for MAG
 - The users are NHC, CPHC, JTWC, MAG and NOMADS
 - All the files in COM directory should be transferred from PROD WCOSS2 to DEV WCOSS2
 - None of the codes are proprietary and there is no restricted data

### 5. Archive to HPSS
 - All the files in the COM directory are preferred to be archived in 5-year retention HPSS directory

### 6. Implementation instructions: See [here](https://docs.google.com/document/d/1z9pHV7rESskDfEiDC3q6rgwWWXLwPfTjkiXfyofiVVA/edit?usp=sharing)
