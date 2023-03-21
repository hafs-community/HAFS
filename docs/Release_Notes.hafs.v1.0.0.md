v1.0.0- released 07/01/2023
Release Notes:  HAFS v1.0.0 (Major)

Summary

This version (HAFS v.1.0.0) is the first version of Hurricane Analysis and Forecast System (HAFS).  HAFS, as the Unified Forecast System (UFS) hurricane application, is the FV3 (Finite Volume Cubed-Sphere Dynamical Core) based multi-scale model and data assimilation system capable of providing tropical cyclone (TC, including hurricane and typhoon) analyses and forecasts of the inner core structure key to improving storm size and intensity predictions, as well as the large-scale environment that is known to influence the TC's motion. The HAFS development targets an operational data assimilation and modeling system, capable of providing reliable, robust and skillful model guidances for TC track, intensity (including rapid intensification), size, structure, rainfall and cyclone genesis, as well as for TC associated storm surge, sea surface waves, and tornadoes. HAFS is a community-based coupled earth modeling system specially calibrated for hurricane prediction with TC dynamics and physics, sophisticated vortex initialization and advanced inner-core data assimilation techniques, and various air-sea interaction processes.
 
HAFSv.1.0.0 will replace NCEP's current operational hurricane forecast systems, HWRF and HMON in the 2023 hurricane season. The ultimate goal of this project is to implement a UFS-based atmospheric model, coupled with ocean and wave models, with a multi-scale data assimilation system, HAFSv1.0 into operations in 2023. The system will provide comparable and/or improved tropical cyclone track and intensity forecast guidance in all global oceanic basins, as demonstrated through evaluation of retrospective and real time experiments compared against the current operational HWRF/HMON systems (HWRFv13.1.5 and HMONv3.1.2). 


The new release has been fully tested and compared with the forecast results with current operational HWRFv13.1.5. It has shown significant skill improvement in track forecasts (~10%) and neutral to positive impact on intesity forecasts in all global basins. The scientific and technique upgrades and changes are highlighted as follows:

1. UFS-FV3 dynamic core, Sync with latest ufs-weather-model repository
2. Moving nest capability
3. Vortex initialization for nest domain
4. Inner-core data assimilation
5. TC-calibrated model physics, Surface, PBL and Micro Physics
6. CMEPS-based ocean coupling with an extended HYCOM ocean domain
7. Parent domain coupled with HYCOM, downscaling SST to moving nest
8. Improved write-grid component, capable of outputting two domains
9. Updated GFDL vortex tracker
10.Updated workflow

Description of Change

The system change includes:
There are two configurations of HAFS, HFSA and HFSB. HFSA replaces HWRF, which runs 7 maximum storms in all global oceanic basins. HFSB replaces HMON, which runs 5 maximum storms in NHC/CPHC basins.  

Benefit of Change

The new system will provide improved hurricane/tropical cyclone track, intensity, and storm structure forecasts for all global basins, especially for Atlantic and Eastern-Pacific basins. The new system will provide additional model output for downstream application;

User Impact Statement

No user impact, the delivery time will be same as HWRF/HMON, i.e. around T+6:00

Technique Impact Statement

The new system requires more computer resources:

The forecast job requires up to 6272 cores (49 nodes) per cycle per storm;
The new forecast job runs ~100 min, similar run time to the current system;
The new system requires 145G disk space per cycle. 

Risk
       None

Proposed Implementation
July 01, 2023

