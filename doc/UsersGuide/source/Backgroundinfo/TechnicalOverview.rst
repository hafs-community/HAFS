.. _TechOverview:

*******************
Technical Overview
*******************

This chapter contains an overview of HAFS prerequisites, HAFS repository management and structure, workflow design, and HAFS v2.0.0 operational updates.

.. _Prerequisites:

===================================================
Prerequisites for Working with the HAFS Repository
===================================================

* Git version 1.8.2 or newer, which added the possibility to track branches of submodules. The following versions of Git are used on NOAA HPCs: 

  * **Orion**: 

    * Git version 1.8.3.1 (system default)
    * Git version 2.21.0 (available via ``module load git/2.21.0``)

  * **Jet/Hera**: Git version 2.18.0
  * **WCOSS Dell**: Git version 1.8.3.1
  * **WCOSS Cray** (cannot access GitHub)

    * Git version 1.7.12.4 (system default) 
    * Git version 2.14.2 (available)

      * ``module use /gpfs/hps3/emc/hwrf/noscrub/soft/modulefiles``
      * ``module load git/2.14.2``

* `GitHub <https://github.com/>`__ account (e.g., FirstLast-NOAA)

.. _RepositoryManagement:

==================================
HAFS GitHub Repository Management
==================================

The authoritative HAFS repository is publicly available on GitHub at: https://github.com/hafs-community/HAFS. 
It is maintained by EMC and DTC with developments and contributions from the :term:`UFS` HAFS application team and the hurricane research community.

**HAFS Branching/Tagging Conventions:**

  - ``develop``: The main development branch
  - ``support/[name]``: Branches used by operational implementations or HFIP real-time parallel experiments (e.g., ``support/hafs.v0.2.0``)
  - ``release/vx.x.x``: Public release branches (e.g., ``release/v1.0.0``)
  - ``[name].v#.#.#`` (*tags*): hafs.v0.2.0, hafs.v1.0.0, public.v1.0.0, etc.
  - ``hotfix/[name]``: Temporary bug fix branches
  - ``feature/[name]``: Feature branches for adding new capabilities or enhancements

The HAFS repository only hosts major feature branches (e.g., ``feature/hafs_nesting``) that require active group collaborations. Individual developers can work on feature branches in their personal HAFS forks. 

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_branching_diagram.png
    :width: 50 %
    :alt: Example HAFS Branching Diagram

    Example HAFS Branching Diagram

.. _dir-structure:

========================
HAFS Directory Structure
========================

The following shows the names of the files and directories in the ``HAFS`` repository.

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_directory_structure.png
    :width: 50 %
    :alt: HAFS directory names and explanations (updated 06/29/2023)


.. _Submodules:

================
HAFS Submodules
================

HAFS contains the following subcomponents:

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_submodules.png
    :width: 75 %
    :alt: HAFS Subcomponents/Submodules (updated 11/02/2021)

.. _Workflow:

========================
HAFS Workflow Schematic
========================

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_workflow_schematic.png
    :width: 75 %
    :alt: HAFS Workflow Schematic

.. _OperationalUpdates:

===============================
HAFS v2.0.0 Operational Updates 
===============================

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
    EMC and the UFS Hurricane Application Team. See currently active HAFS developers `here <https://docs.google.com/presentation/d/1xBNxvAG8-Kk3GS93PndaPVZp_L8U-KqGRGNhnviXZMg/edit?usp=sharing>`__
    
**Runs on:** WCOSS2; pre-implementation testing performed on Hera and Orion.

Input/Output
------------

**Input:**
    - Global Forecast System (GFS, including GFS/GDAS) NetCDF format analysis for atmospheric initial conditions and 3-hourly GRIB2 files (up to 129 h) for atmospheric lateral boundary conditions and atmospheric forcing for oceanic and wave model components.
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

.. _scientific-updates:

Scientific Changes to Improve Track and Intensity Forecast Skills
-----------------------------------------------------------------

**System and Infrastructure Upgrades:**
    - Latest version of ``ufs-weather-model``, HAFSv2 final scientific configuration freeze on 02/08/2024.
    - Increase moving nesting horizontal resolution from 6-2 km to 5.4-1.8 km (HFSA only).
    - Reduce model time step from 90 to 72s (HFSB only).
    - Updated horizontal advection options.
    - Improved model stability and runtime efficiency.

**Vortex Initialization Improvement:**
    - Enhance vortex initialization to cycle hydrometeor variables and vertical velocity (HFSA only).
    - Update compsitie vortex and reduce warm-cycling Vmax threshold from 50 to 40 kt (HFSA only).

**Data Assimilation Improvement:**
    - Ingest new high-resolution GOES-R mesoscale AMVs.
    - Scale-Dependent Localization for inner core DA.
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
    - See `here <https://github.com/hafs-community/HAFS/blob/production/hafs.v2/versions/run.ver>`__

**Output Changes:**
    - See `here <https://docs.google.com/presentation/d/1dGWu_k-CdiX_ndaRe89iDznyNwkEJfjVTagvvEVZQaw/edit?usp=sharing>`__

Computer Resource Information
-----------------------------

**Computation resource updates:**
    - See `here <https://docs.google.com/presentation/d/1otBHAi3hfB1Vu5lk9bfjOE-GZYalZiu9wjnFlevQRQw/edit?usp=sharing>`__

**List of the module versions used in HAFS:**
    - See `here <https://github.com/hafs-community/HAFS/blob/production/hafs.v2/versions/run.ver>`__

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
    No, but all changes have been tested successfully using available input as per the following list: `IT tests list <https://docs.google.com/presentation/d/1qttu6HhZC3I2yEWLIcsTM7BUGxGyevhtZMUU61kSyYI/edit?usp=sharing>`__

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

See `here <https://docs.google.com/document/d/1dKV_jAHs9TkOhS_qA-Nr3PPE_NtbGxaAgC5FSPI6BwQ/edit?usp=sharing>`__