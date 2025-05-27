.. _TechOverview:

*******************
Technical Overview
*******************

This chapter contains an overview of HAFS prerequisites, HAFS repository management and structure, workflow design, and HAFS |latestr| operational updates.

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

**Location on GitHub:** `HAFS GitHub Repository <https://github.com/hafs-community/HAFS/tree/production/hafs.v2>`_

**Clone the repository:**

.. code-block:: bash

   git clone -b production/hafs.v2 --recursive  https://github.com/hafs-community/HAFS.git

.. attention::
   HAFSv2 runs in production on WCOSS2; pre-implementation testing was performed on Hera and Orion. It is likely possible to run on other systems, but additional code modifications may be required to run an experiment, and this capability has not yet been tested.

**Purpose:**
   * The :term:`HFSA` configuration provides hurricane track and intensity forecast guidance to 5.25 days over all global oceanic basins, including :term:`NATL`, :term:`EPAC`, :term:`CPAC`, :term:`WPAC`, :term:`NIO`, and :term:`SH` basins. 
   * The :term:`HFSB` configuration provides hurricane track and intensity forecast guidance for 5.25 days over :term:`NHC`/:term:`CPHC` basins, including :term:`NATL`, :term:`EPAC`, :term:`CPAC` basins.


**Developed by:**
    EMC and the UFS Hurricane Application Team. See active HAFSv2 developers in :numref:`Table %s <HAFSDevs>`.
    
.. _HAFSDevs:

.. list-table:: HAFS Developers
   :header-rows: 1

   * - Area of Expertise
     - Institution
     - Developers
   * - Atmospheric model dynamics/configurations/workflow
     - NCEP/EMC
     - Bin Liu, Dusan Jovic, Avichal Mehra, JungHoon Shin, Vijay Tallapragada, Biju Thomas, Jun Wang, Zhan Zhang, Yangxing Zheng
   * - Atmospheric model dynamics/configurations/workflow
     - AOML/HRD
     - Ghassan Alaka, S. Gopalakrishnan, William Ramstrom, Xuejin Zhang
   * - Atmospheric model dynamics/configurations/workflow
     - DTC
     - Mrinal Biswas, Kathryn Newman, Linlin Pan
   * - Atmospheric model dynamics/configurations/workflow
     - GFDL
     - Rusty Benson, Lucas Harris, Joseph Mouallem
   * - Atmospheric model dynamics/configurations/workflow
     - GSL
     - Samuel Trahan
   * - Ocean/Wave coupling through CMEPS
     - NCEP/EMC
     - Maria Aristizabal, Bin Li, Matthew Masarik, Jessica Meixner, John Steffen
   * - Ocean/Wave coupling through CMEPS
     - AOML/HRD
     - Lew Gramer
   * - Ocean/Wave coupling through CMEPS
     - AMOL/PhOD
     - HeeSook Kang, Hyun-Sook Kim, Ming Ming Shao
   * - Ocean/Wave coupling through CMEPS
     - NCAR/ESMF
     - Dan Rosen, Gerhard Theurich, Ufuk Turuncoglu, Ann Tsay
   * - Data Assimilation
     - NCEP/EMC
     - Jing Cheng, Daryl Kleist, Ting Lei, Shun Liu, Xu Lu, Yonghui Weng, Sho Yokota
   * - Data Assimilation
     - AOML/HRD
     - Sarah D. Ditchek, Jason Sippel
   * - Data Assimilation
     - OU
     - Xuguang Wang
   * - Data Assimilation
     - UM/CIMAS
     - Altug Aksoy, Dan Wu
   * - Data Assimilation
     - UMD
     - Joseph Knisely, Kenta Kurosawa, Jonathan Poterjoy
   * - Data Assimilation
     - SUNY/U at Albany
     - Ryan Torn, Eun-Gyeong Yang
   * - Model Pre- and Post-processes
     - NCEP/EMC
     - George Gayno, Hui-Ya Chuang, Nathalie Rivera-Torres, Qingfu Liu, Chuan-Kai Wang, Wen Meng, Lin Zhu
   * - Model Pre- and Post-processes
     - GFDL
     - Timothy Marchok
   * - Atmospheric Physics
     - NCEP/EMC
     - Jongil Han, Ruiyu Sun, Xu Li, Weiguo Wang, Fanglin Yang
   * - Atmospheric Physics
     - AOML/HRD
     - Andrew Hazelton
   * - Atmospheric Physics
     - UAH
     - Xiaomin Chen
   * - Atmospheric Physics
     - PSL
     - Lisa Bengtsson
   * - Verification/Evaluation
     - NCEP/EMC
     - Olivia Ostwald, Hananeh Jafary, Jiayi Peng
   * - Verification/Evaluation
     - NHC
     - Michael Brennan, Jon Martinez, Ben Trabing, David Zelinsky, Wallace Hogsett, Jamie Rhome, Richard Pasch
   * - Verification/Evaluation
     - JTWC
     - Brian Strahl, Levi Cowan
   

Input/Output
------------

**Input Data:**
    - Global Forecast System (GFS, including GFS/GDAS) :term:`NetCDF` format analysis for atmospheric initial conditions and 3-hourly :term:`GRIB2` files (up to 129 h) for atmospheric lateral boundary conditions and atmospheric forcing for oceanic and wave model components.
    - Global :term:`RTOFS` files for oceanic initial and boundary conditions for :term:`MOM6` and :term:`HYCOM`.
    - GFS/GDAS global wave analysis/forecast files for :term:`HFSA` wave component initial and lateral boundary conditions.
    - :term:`OBSPROC` observational data for HAFS data assimilation.


**Input DCOM Dependencies**
The following scripts require input files from DCOM (data input) directory:

    - ``./scripts/exhafs_obs_prep.sh``: To decode/dump TDR/HDOB/TEMPDROP data
    - ``./scripts/exhafs_msg_check.py``: To check the message files are properly created by checking the original NHC/JTWC message files in the DCOM directory.
    - ``./ush/setup_hurricane``: For SDM to set up hurricane message files by extracting info from the original NHC/JTWC message files in the DCOM directory.

**Output:**
    - ATCF track forecast files, 4x/day at 00Z/06Z/12Z/18Z

Locations for Output
--------------------

- **WCOSS2:**
    - HFSA: ``/lfs/h1/ops/prod/com/hafs/v2.0/hfsa.yyyymmdd/hh``
    - HFSB: ``/lfs/h1/ops/prod/com/hafs/v2.0/hfsb.yyyymmdd/hh``
- **Other servers:**
    - NCEP ftp server, NOMADS, NWS Gateway/SBN
    - See `NOMADS here <https://nomads.ncep.noaa.gov/pub/data/nccf/com/hafs/prod/>`_.

Background
----------

HAFSv2 will replace NCEP's current operational hurricane forecast systems, HAFSv1 in the 2024 hurricane season. The upgraded system will provide improved and comparable tropical cyclone track and intensity forecast guidance in all global oceanic basins, as demonstrated through evaluation of retrospective and real time experiments compared against the two current configurations of operational HFSAv1 and HFSBv1.

.. _scientific-updates:

Scientific Changes to Improve Track and Intensity Forecast Skills
-----------------------------------------------------------------

**System and Infrastructure Upgrades:**
    - Updated version of ``hafs-community/ufs-weather-model``, HAFSv2 final scientific configuration freeze on 02/08/2024.
    - Increase moving nesting horizontal resolution from 6-2 km to 5.4-1.8 km (HFSA only).
    - Reduce model time step from 90 to 72s (HFSB only).
    - Updated horizontal advection options.
    - Improved model stability and runtime efficiency.

**Vortex Initialization Improvement:**
    - Enhance vortex initialization to cycle hydrometeor variables and vertical velocity (HFSA only).
    - Update composite vortex and reduce warm-cycling Vmax threshold from 50 to 40 kt (HFSA only).

**Data Assimilation (DA) Improvement:**
    - Ingest new high-resolution :term:`GOES-R` mesoscale :term:`AMVs`.
    - Scale-Dependent Localization for inner core data assimilation.
    - Refine GPS Radio Occultation (RO) DA.

**Model Physics Advancement:**
    - Upgrade Thompson :term:`MP` with bug fixes.
    - Thompson Microphysics for :term:`NATL` basin, :term:`GFDL` Microphysics for :term:`EPAC`/:term:`CPAC` and :term:`JTWC` basins (:term:`HFSA` only).
    - Update :term:`TKE` :term:`EDMF` :term:`PBL` and :term:`SASAS` CP schemes with vertical wind shear impacts.
    - Change the radiation calling time step from 720s to 900s (HFSA only).
    - Reduce radiation time step from 1800 to 720s (HFSB only).
    - Update CO2 fix files.

**Ocean/Wave Coupling:**
    - MOM6 ocean model coupling (HFSA only).
    - CMEPS with inline-CDEPS coupling (HFSA only).

**Post-processing:**
    - Upgrade GFDL Tracker.

**Software Dependencies:**
    - See `list of dependencies here <https://github.com/hafs-community/HAFS/blob/production/hafs.v2/versions/run.ver>`_.

**Output Changes:**

- *New variables in HAFSv2:* No
- *Removed variables:* No

Computer Resource Information
-----------------------------

**Computation resource updates:**
    - HAFSv2 will use similar computing resources as HAFSv1.

    - HAFS Weather Model (HWM): No significant node increase expected for the forecast job.

**List of the module versions used in HAFS:**
    - See `list of module version here <https://github.com/hafs-community/HAFS/blob/production/hafs.v2/versions/run.ver>`_.

**Data retention for files in:** 
    - ``/lfs/h1/ops/prod/com/hafs/v2.0/hfsa.yyyymmdd/hh`` and ``/lfs/h1/ops/prod/com/hafs/v2.0/hfsb.yyyymmdd/hh``
        - **Disk space usage:** Increased from 145/140 GB (HFSAv1/HFSBv1) per cycle to ~295/195 GB (HFSAv2/HFSBv2).
        - **HPSS disk requirement:** Increased from ~142/126 GB (HFSAv1/HFSBv1) to ~228/133 GB (HFSAv2/HFSBv2).
        - **Preferred data retention in COM:** Files to be kept for 7 days.

Pre-Implementation Testing
--------------------------

All changes have been tested successfully using available input from :numref:`Table %s <TestingInput>`.

.. _TestingInput:

.. list-table:: Test Objectives and Comments
   :header-rows: 1

   * - Test Objective
     - Comment
   * - Test two consecutive cycles for one storm in each global basin
     - Basic tests for each basin
   * - Very weak storm (<8 m/s)
     - Use GFS analysis as input (No VI/DA)
   * - Missing ICs from GDAS data
     - HAFS fails with proper error message
   * - Missing BCs from GFS data
     - HAFS fails with proper error message
   * - Missing previous cycle’s 6-hr forecast output
     - HAFS runs to completion in cold start mode
   * - Zero length data files for GSI
     - Initialization and analysis runs to completion
   * - Missing input data files for GSI
     - Initialization and analysis runs to completion
   * - Failed ocean initialization
     - HAFS runs in un-coupled mode
   * - Tracker fails to identify initial storm location
     - Swath generator fails with proper error message
   * - Cross dateline and Greenwich tests
     - Ensure HAFS model and scripts properly handle the specific situations
   * - Bugzilla Entries
     - Operational failure


NHC, CPHC, and JTWC were suggested to evaluate model products and MAG was suggested to evaluate GEMPAK files.

Dissemination and Archiving
---------------------------

**Dissemination Information:**
Files were disseminated as follows:

- ATCF files to NHC (and CPHC for Central Pacific Basin storms) and JTWC (for all global basins, including SH basins) 
- GRIB files to NOMADS
- GEMPAK files to MAG

All the files in the :term:`COM` directory should be transferred from PROD WCOSS2 to DEV WCOSS2. None of the code is proprietary, and there is no restricted data.

**Archive to HPSS:**
All the files in the :term:`COM` directory are preferred to be archived in 5-year retention :term:`HPSS` directory.