.. _PhysicsSchemes:

***********************
Physics Schemes in HAFS
***********************

This chapter contains information regarding the various physics schemes in HAFS.

.. _SuiteComparison:

================
Suite Comparison
================

:numref:`Table %s <PhysicsComparison>` compares the different physics configurations used for Suite 1 (HFSAv2) and Suite 2 (HFSBv2).

.. _PhysicsComparison:

.. list-table:: Suite Comparison
   :header-rows: 1

   * - Component
     - Suite 1 (HFSAv2)
     - Suite 2 (HFSBv2)
   * - Land/ocean Surface
     - NOAH LSM VIIRS veg type, **MOM6**
     - NOAH LSM VIIRS veg type, **HYCOM**
   * - Surface Layer
     - GFS, HWRF TC-specific sea surface roughnesses
     - GFS, HWRF TC-specific sea surface roughnesses
   * - Boundary Layer
     - Sa-TKE-EDMF+shear:
       **sfc_rlm=1**
       **tc_pbl= 0**
       **elmx/rlmx=250 (nest)**
     - Sa-TKE-EDMF+shear:
       **sfc_rlm=0**
       **tc_pbl=1**
       **elmx/rlmx=75 (nest)**
   * - Microphysics
     - **Thompson, dt_inner=45s (AL)** 
       **GFDL MPv1 (EP)**
     - **Thompson, dt_inner=36s**
   * - Radiation
     - **RRTMG Calling frequency 900 s**
     - **RRTMG Calling frequency 720 s**
   * - Cumulus convection (deep&shallow)
     - sa-SAS +shear progsigma=F (AL), T (EP) **entrainment:clam_deep=0.15**
     - sa-SAS +shear progsigma=F (AL), T (EP) **entrainment: clam_deep=0.1**
   * - Gravity wave drag
     - Improved UGWPv1 (orographic on/convective off)
     - Improved UGWPv1 (orographic on/convective off)

.. _Land/OceanSurfaceSchemes:

==========================
Land/Ocean Surface Schemes
==========================


Land/Ocean surface schemes provide surface temperature, heat and moisture fluxes over land, sea-ice, and ocean points. These serve as a lower boundary condition for the vertical transport in Planetary Boundary Layer (PBL) schemes.

**HAFS uses:**

* Noah Multi-Physics (NOAH-MP) land surface model
* :term:`HYCOM`, :term:`MOM6` for ocean models

.. _SurfaceLayerScheme:

====================
Surface-Layer Scheme
====================

Surface layer schemes represent processes near the surface (<~100 m), where vertical gradients are too large to be resolved by vertical grids. They are usually based on Monin-Obukhov similarity theory.
They provide atmospheric exchange coefficients and stability functions (surface fluxes) needed by land/ocean models and PBL.

HAFS uses the GFS surface-layer model, with observation-based roughness length (particularly for strong wind conditions). Modified Zo is important for a successful simulation of TCs. Hurricane intensity is proportional to (Ck/Cd) over the ocean (Emanuel, 1995), where Cd and Ck are the sea-surface momentum and enthalpy exchange coefficients. Larger z0 leads to larger Cd and Ck.

.. _PBLScheme:

==========
PBL Scheme
==========

PBL parameterization schemes handle subgrid-scale processes near the surface. In HAFS, the PBL scheme is responsible for handling turbulent scale mixing and diffusion. HAFS uses the GFS TKE-EDMF PBL scheme, with a prognostic Turbulent Kinetic Energy (TKE) equation.

.. code-block:: console

   Sub-grid scale turbulent flux = Flux of Large eddies + flux of local small eddies
                                                          mass-flux (MF)           eddy-diffusivity (ED)

TC intensity is sensitive to mixing/diffusion, so two modifications were made:

  1. Adjustment to mixing length near the surface so that it is consistent with that used in the surface-layer scheme (HFSA)
  2. LES-based TC PBL adjustment including model coefficients, Richardson number calculation, high-wind mass flux, etc. (HFSB)

.. _ScaleAwareConvection:

===============================================
Scale-Aware Deep and Shallow Convection Schemes
===============================================

Scale-aware deep and shallow convection schemes are used when moisture convection is too small to be resolved explicitly by grid spacing, reducing the thermodynamic instability. HAFS uses GFS Scale-Aware SAS. 

Main Assumptions
-----------------------

**Deep Convection:**

A single entraining and detraining plume model is used. The base mass flux operates under a quasi-equilibrium assumption for dx greater than 8 km, determined by a mean updraft velocity for dx less than 8 km. Triggering conditions include the distance between the convection starting level and the level of free convection, sub-cloud convective inhibition, and mean relative humidity.

**Shallow Convection:**

Shallow convection is similar to deep convection, except that the base mass flux is averaged based on the updraft velocity in a cloud layer. Only convection updrafts are considered in the shallow scheme. HAFS tests show that HAFS is sensitive to entrainment and detrainment rates. HFSA uses an entrainment rate larger than that in NCEP GFS.

.. _MicrophysicsSchemes:

====================
Microphysics Schemes
====================

HAFS uses GFDL microphysics and Thompson microphysics, which account for the effects of vapor-liquid-ice phase changes. HAFS performs well with the Thompson scheme for TCs in the NATL basin and with GFDL for TCs in the EAPC basin.

**GFDL:**

The GFDL microphysics is a single-moment scheme that includes cloud water content (QC), cloud ice content (QI), rain water content (QR), snow content (QS), and graupel content (QG). It is based on the Lin-Lord-Krueger cloud microphysics scheme.

**Thompson:**

The Thompson microphysics scheme includes QC, QI, QR, QS, QG, and the number concentration of cloud ice, cloud water, and rain. It also considers the impact of aerosols. 

.. _RadiationSchemes:

=================
Radiation Schemes
=================

Radiation schemes are used for heating and cooling due to short- and long-wave radiation.

HAFS uses the RRTMG radiation scheme. For computational efficiency, the correlated K-method is used. The shortwave algorithm includes 112 g-points (quadrature points) in 14 bands, while the longwave algorithm includes 140 unevenly distributed g-points in 16 broad spectral bands. Aerosol optical properties, cloud liquid water and ice paths, and effective radius are used to represent the radiative effects of aerosols and clouds in the calculation. The effects of sub-grid-scale clouds are treated by a Monte-Carlo Independent Column Approximation (McICA) method, with a decorrelation length overlap assumption for multi-layered clouds.

.. _GravityWaveDrag:

=================
Gravity Wave Drag
=================

HAFS uses the Unified Gravity Wave Drag (GWD) scheme, which considers:

* Mesoscale orographic GWD
* Low-level flow blocking by subgrid-scale orography
* Effects of gravity waves produced by horizontal terrain variations
* Non-topographic GWD from convection and frontal instability

