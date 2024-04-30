.. _ReleaseNotes:

HAFS v2.0.0 Release Notes (Major)
=================================

**Model:** Hurricane Analysis and Forecast System (HAFS)

**Version:** hafs.v2.0.0

**Released:** July 01, 2024, 1200UTC

**Location on GitHub:** `HAFS GitHub Repository <https://github.com/hafs-community/HAFS/tree/production/hafs.v2>`

**Clone the repository:**

.. code-block:: bash

   git clone -b production/hafs.v2 --recursive  https://github.com/hafs-community/HAFS.git ./hafs.v2.0.0

**Purpose:**
    Run HAFS.v2 HFSA configuration to provide hurricane track and intensity forecast guidance to 5.25 days over all global oceanic basins, including NATL, EPAC, CPAC, WPAC, NIO, and SH basins. Run HAFS.v2 HFSB configuration to provide hurricane track and intensity forecast guidance for 5.25 days over NHC/CPHC basins, including NATL, EPAC, CPAC basins.

**Developed by:**
    EMC and the UFS Hurricane Application Team. See currently active HAFS developers `here <https://docs.google.com/presentation/d/1xBNxvAG8-Kk3GS93PndaPVZp_L8U-KqGRGNhnviXZMg/edit?usp=sharing>`.
    
**Runs on:** WCOSS2

Input/Output
------------

**Input:**
    - Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for atmospheric lateral boundary conditions and atmospheric forcing for oceanic and wave model components.
    - Global RTOFS for oceanic initial and boundary conditions for MOM6 and HYCOM.
    - GFS/GDAS global wave analysis/forecast for HFSA wave component initial and lateral boundary conditions.
    - OBSPROC observational data for HAFS data assimilation.

**Output:**
    - ATCF track forecast files, 4x/day at 00Z/06Z/12Z/18Z.

Locations for Output
--------------------

- **WCOSS2:**
    - HFSA: ``/lfs/h1/ops/prod/com/hafs/v2.0/hfsa.yyyymmdd/hh``
    - HFSB: ``/lfs/h1/ops/prod/com/hafs/v2.0/hfsb.yyyymmdd/hh``
- **Other servers:**
    - NCEP ftp server, NOMADS, NWS Gateway/SBN

Background
----------

Hurricane Analysis and Forecast System (HAFS), as the Unified Forecast System (UFS) hurricane application, is the FV3 (Finite­ Volume Cubed-Sphere Dynamical Core) based multi-scale model and data assimilation system capable of providing tropical cyclone (TC, including hurricane and typhoon) analyses and forecasts of the inner core structure key to improving storm size and intensity predictions, as well as the large-scale environment that is known to influence the TC's motion. HAFS development targets an operational data assimilation and modeling system, capable of providing reliable, robust and skillful model guidances for TC track, intensity (including rapid intensification), size, structure, rainfall and cyclone genesis, as well as for TC associated storm surge, sea surface waves, and tornadoes. HAFS is a community-based coupled earth modeling system specially calibrated for hurricane prediction with TC dynamics and physics, sophisticated vortex initialization and advanced inner-core data assimilation techniques, and various air-sea interaction processes.

HAFSv2 will replace NCEP's current operational hurricane forecast systems, HAFSv1 in the 2024 hurricane season. The goal of this project is to upgrade the HAFSv1 that was running in operations in 2023. The system will provide improved and comparable tropical cyclone track and intensity forecast guidance in all global oceanic basins, as demonstrated through evaluation of retrospective and real time experiments compared against the two current configurations of operational HFSAv1 and HFSBv1.

Scientific Changes to Improve Track and Intensity Forecast Skills
-----------------------------------------------------------------

**System and Infrastructure Upgrades:**
    - Latest version of ufs-weather-model, HAFSv2 final scientific configuration freeze on 02/08/2024.
    - Increase moving nesting horizontal resolution from 6-2 km to 5.4-1.8 km (HFSA only).
    - Reduce model time step from 90 to 72s (HFSB only).
    - Updated horizontal advection options.
    - Improved model stability and runtime efficiency.

**Vortex Initialization Improvement:**
    - Enhance vortex initialization to cycle hydrometeor variables and vertical velocity (HFSA only).
    - Update compsitie vortex and reduce warm-cycling Vmax threshold from 50 to 40 kt (HFSA only).

**Data Assimilation Improvement:**
    - Ingest new high-resolution GOES-R mesoscale AMVs.
    - Scale-Dependent Localization for innercore DA.
    - Refine GPS RO (Radio Occultation) DA.

**Model Physics Advancement:**
    - Upgrade Thompson MP with bug fixes.
    - Thompson Microphysics for NATL basin, GFDL Microphysics for EPAC/CPAC and JTWC basins (HFSA only).
    - Update TKE EDMF PBL and SASAS CP schemes with vertical wind shear impacts.
    - Change the radiation calling time step from 720s to 900s (HFSA only).
    - Reduce radiation time step from 1800 to 720s (HFSB only).
    - Update CO2 fix files.

**Ocean/Wave Coupling:**
    - MOM6 ocean model coupling (HFSA only).
    - CMEPS with inline-CDEPS coupling (HFSA only).

**Post-processing:**
    - Upgrade GFDL Tracker.

**Software Dependencies:**
    - See `here <https://github.com/hafs-community/HAFS/blob/production/hafs.v2/versions/run.ver>`

**Output Changes:**
    - See `here <https://docs.google.com/presentation/d/1dGWu_k-CdiX_ndaRe89iDznyNwkEJfjVTagvvEVZQaw/edit?usp=sharing>`

Computer Resource Information
-----------------------------

**Computation resource updates:**
    - See `here <https://docs.google.com/presentation/d/1otBHAi3hfB1Vu5lk9bfjOE-GZYalZiu9wjnFlevQRQw/edit?usp=sharing>`

**List of the module versions used in HAFS:**
    - See `here <https://github.com/hafs-community/HAFS/blob/production/hafs.v2/versions/run.ver>`

**Data retention for files in:** 
    - ``/lfs/h1/ops/prod/com/hafs/v2.0/hfsa.yyyymmdd/hh`` and ``/lfs/h1/ops/prod/com/hafs/v2.0/hfsb.yyyymmdd/hh``
        - **Disk space usage:** Increased from 145/140 GB (HFSAv1/HFSBv1) per cycle to ~295/195 GB (HFSAv2/HFSBv2).
        - **HPSS disk requirement:** Increased from ~142/126 GB (HFSAv1/HFSBv1) to ~228/133 GB (HFSAv2/HFSBv2).
        - **Preferred data retention in COM:** Files to be kept for 7 days.

**Input Datasets**
    - Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis for atmospheric initial condition, and 3-hourly GRIB2 files (up to 129 h) for atmospheric lateral boundary conditions and atmospheric forcing for oceanic and wave model components. Global RTOFS for oceanic initial and boundary conditions for the ocean model component. GFS/GDAS global wave analysis/forecast for HFSA wave component initial and lateral boundary conditions. OBSPROC observational data for HAFS data assimilation.


**Input DCOM Dependencies**

- The following scripts require input files from DCOM directory:
    - ``./scripts/exhafs_obs_prep.sh``: to decode/dump TDR/HDOB/TEMPDROP data
    - ``./scripts/exhafs_msg_check.py``: to check the message files are properly created by checking the original NHC/JTWC message files in the DCOM directory.
    - ``./ush/setup_hurricane``: for SDM to set up hurricane message files by extracting info from the original NHC/JTWC message files in the DCOM directory.


Pre-implementation Testing
--------------------------

All jobs should be tested as part of this implementation.

**Does this change require a 30-day evaluation (for major implementations only)?**
    No, but all changes have been tested successfully using available input as per the following list: `IT tests list <https://docs.google.com/presentation/d/1qttu6HhZC3I2yEWLIcsTM7BUGxGyevhtZMUU61kSyYI/edit?usp=sharing>`

**Who are the suggested evaluators?**
    NHC, CPHC, and JTWC for model products and MAG for GEMPAK files.

Dissemination and Archiving
---------------------------

**Dissemination Information:**
    - The ATCF files to NHC (and CPHC for Central Pacific Basin storms), and JTWC (for all global basins, including SH basins), GRIB files for NOMADS and GEMPAK files for MAG.
    - The users are NHC, CPHC, JTWC, MAG and NOMADS
    - All the files in COM directory should be transferred from PROD WCOSS2 to DEV WCOSS2
    - None of the codes are proprietary and there is no restricted data

**Archive to HPSS:**
    All the files in the COM directory are preferred to be archived in 5-year retention HPSS directory.

Implementation Details
----------------------

See `here <https://docs.google.com/document/d/1dKV_jAHs9TkOhS_qA-Nr3PPE_NtbGxaAgC5FSPI6BwQ/edit?usp=sharing>`

