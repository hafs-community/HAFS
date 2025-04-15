v2.1.0- released 07/14/2025

Release Notes: HAFS v2.1.0 (Minor)

## Model: Hurricane Analysis and Forecast System (HAFS)

Version: hafs.v2.1.0

Location on GitHub server: https://github.com/hafs-community/HAFS/tree/production/hafs.v2.1
```
git clone -b production/hafs.v2.1 --recursive https://github.com/hafs-community/HAFS.git ./hafs.v2.1.0
```
Implementation date: July 14, 2025 1200UTC (tentatively)

Purpose: Run HAFSv2.1 HFSA configuration to provide hurricane track and
intensity forecast guidance to 5.25 days over all global tropical cyclone
basins, including NATL, EPAC, CPAC, WPAC, NIO, and SH basins. Run HAFSv2.1 HFSB
configuration to provide hurricane track and intensity forecast guidance for
5.25 days over NHC/CPHC basins, including NATL, EPAC, CPAC basins.

Developed by: EMC and the UFS Hurricane Application Team. See currently active HAFS developers [here](https://docs.google.com/presentation/d/1GW4HpvRFXyP_oT_K-wGaPIvQ_oWNu1DJZT1RA2hYQgE/edit?usp=sharing).

Runs on WCOSS2

Input: Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis
for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for
atmospheric lateral boundary conditions and atmospheric forcing for oceanic and
wave model components. Global RTOFS for oceanic initial and boundary conditions
for MOM6 and HYCOM. GFS/GDAS global wave analysis/forecast for HFSA wave
component initial and lateral boundary conditions. OBSPROC observational data
for HAFS data assimilation.

Output: ATCF track forecast files, 4x/day at 00Z/06Z/12Z/18Z.

Primary users: NHC, CPHC, JTWC, MAG, and private sectors.

Where to find output:
- on WCOSS2
  - HFSA: `/lfs/h1/ops/prod/com/hafs/v2.1/hfsa.yyyymmdd/hh`
  - HFSB: `/lfs/h1/ops/prod/com/hafs/v2.1/hfsb.yyyymmdd/hh`
- on servers: NCEP ftp server, NOMADS, NWS Gateway/SBN

### 1. Background:

Hurricane Analysis and Forecast System (HAFS), as the Unified Forecast System
(UFS) hurricane application, is the FV3 (Finite Volume Cubed-Sphere Dynamical
Core) based multi-scale model and data assimilation system capable of providing
tropical cyclone (TC, including hurricane and typhoon) analyses and forecasts
of the inner-core storm structure (which is key to improving storm size and
intensity predictions) and the large-scale environment that is known to
influence the TC's motion. The HAFS development targets an operational data
assimilation and modeling system, capable of providing reliable, robust and
skillful model guidances for TC track, intensity (including rapid
intensification), size, structure, rainfall and cyclone genesis, as well as for
TC associated storm surge, sea surface waves, and tornadoes. HAFS is a
community-based coupled earth modeling system specially calibrated for
hurricane prediction with TC dynamics and physics, sophisticated vortex
initialization and advanced inner-core data assimilation techniques, and
various air-sea interaction processes.

HAFSv2.1 will be an upgrade of NCEP's current operational HAFSv2 in the 2025
hurricane season. The system will provide improved and comparable tropical
cyclone track and intensity forecast guidance in all global TC basins, as
demonstrated through evaluation of retrospective and real-time experiments
compared against the two current configurations of operational HAFSv2.

 - Scientific changes to improve track and intensity forecast skills include:
   - System and infrastructure upgrade
     - Sync HAFS submodules with their authoritative branches as of July, 2024
     - Improve application/workflow scripts according to implementation standards
     - Update 2025 CO2 fix files
   - Vortex initialization and data assimilation improvement
     - Improve vortex initialization to better handle Pmin adjustment and intensity enhancement
     - Introduce wavenumber filtering for DA increment
     - Enable storm-following 3DIAU for inner-core DA
     - Turn on assimilating NOAA-21 ATMS and CrIS observations
     - Turn off assimilation of P3 and C-130 SFMR surface wind speed observation
     - Switch to use new crtm/2.4.0.1 version
   - Physics and dynamics enhancement
     - Improve GFS TKE-EDMF PBL by using liquid/ice water potential temperature in local mixing/diffusion processes
     - Adopt the scale-adaptive convective cloud water calculation in saSAS and reduce the minimum background diffusivity in the inversion layer near the PBL top
     - Unify and turn on the progsigma option for saSAS for all TC basins
     - Use new exponential-random cloud overlap option in RRTMG
     - Further finetune the horizontal advection scheme's lim_fac option
   - Air-sea coupling advancement
     - Initialize HAFS ocean model component with RTOFSv2.5 input data
     - Upgrade to use the ePBL mixed layer scheme (instead of KPP) with the OM4 related options for HFSA
     - Switch from HYCOM coupling to MOM6 coupling for HFSB and use the ePBL mixed layer scheme with the REICHL_H2018 option
   - Post-processing and product update
     - Update NHC storm grib2 subset files to better facilitate AWIPS support

 - Software dependencies: See [here](https://github.com/hafs-community/HAFS/blob/production/hafs.v2.1/versions/run.ver)
 - Output changes: See [here](https://docs.google.com/presentation/d/1cd1JS7P17fKH4jgdgVLjMRePwmBlJotqm2z2ZSSXdos/edit?usp=sharing)

### 2. Computer resource information: See [here](https://docs.google.com/spreadsheets/d/1esWZGlR0369lqEdkBfjyZY_WredMh3gwJ3xQKWJyAGE/edit?usp=sharing)
 - Computation resource updates: See [here](https://docs.google.com/presentation/d/1_0TtMQEmOocsFOdoEqiYUd9FaDuSLXUdF2LZVBxsD5I/edit?usp=sharing)
   - More instructions on using/testing alternative computation resources for the HAFS forecast job can be found [here](https://docs.google.com/document/d/1l_EpBFWNK0YPhDRAos28L4sRa3RGWZomu02fUazZ0_Q/edit?usp=sharing)
 - List of the module versions used in HAFSv2.1: See [here](https://github.com/hafs-community/HAFS/blob/production/hafs.v2.1/versions/run.ver)
 - Data retention for files in `/lfs/h1/ops/prod/com/hafs/v2.1/hfsa.yyyymmdd/hh` and `/lfs/h1/ops/prod/com/hafs/v2.1/hfsb.yyyymmdd/hh`
   - Disk space usage in com directory, changed from 303/188 GB (HFSA/HFSB) per cycle for HAFSv2 to ~270/240 GB (HFSA/HFSB) for HAFSv2.1
   - HPSS disk requirement changed from ~228/133 GB (HFSA/HFSB) per storm per cycle for HAFSv2 to ~195/166 GB (HFSA/HFSB) for HAFSv2.1
   - Preferred to keep the files in COM for 7 days
 - Input datasets
   - Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for atmospheric lateral boundary conditions and atmospheric forcing for oceanic and wave model components. Global RTOFS for oceanic initial and boundary conditions for the ocean model component. GFS/GDAS global wave analysis/forecast for HFSA wave component initial and lateral boundary conditions. OBSPROC observational data for HAFS data assimilation.
 - Input DCOM dependencies
   - The following scripts require input files from DCOM directory:
     - `./scripts/exhafs_obs_prep.sh`: to decode/dump TDR/HDOB/TEMPDROP data
     - `./scripts/exhafs_msg_check.py`: to check the message files are properly created by checking the original NHC/JTWC message files in the DCOM directory
     - `./ush/setup_hurricane`: for SDM to set up hurricane message files by extracting info from the original NHC/JTWC message files in the DCOM directory

### 3. Pre-implementation testing requirements:
 - All jobs should be tested as part of this implementation.
 - Does this change require a 30-day evaluation (for major implementations only)?
   - No but all changes have been tested successfully using available input as per the following list: [IT tests list](https://docs.google.com/presentation/d/1fmEaORvO3h2o1njXBt_Tez3kvJp4fSgR98VJ2OChMig/edit?usp=sharing)
 - Who are the suggested evaluators?
   - NHC, CPHC, and JTWC for model products and MAG for GEMPAK files.

### 4. Dissemination information:
 - The ATCF files to NHC (and CPHC for Central Pacific Basin storms), and JTWC (for all global basins, including SH basins), GRIB files for NOMADS and GEMPAK files for MAG
 - The users are NHC, CPHC, JTWC, MAG and NOMADS
 - All the files in COM directory should be transferred from PROD WCOSS2 to DEV WCOSS2
 - None of the codes are proprietary and there is no restricted data

### 5. Archive to HPSS
 - All the files in the COM directory are preferred to be archived in 5-year retention HPSS directory

### 6. Implementation instructions: See [here](https://docs.google.com/document/d/1acDFVTkHarBr_UIwwY5qBp3dAs1LOStkYHUTAPymx08/edit?usp=sharing)
