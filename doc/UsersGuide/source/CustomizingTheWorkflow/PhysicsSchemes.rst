.. _PhysicsSchemes:

***********************
Physics Schemes in HAFS
***********************

This chapter contains information regarding the various physics schemes in HAFS.

.. _Land/OceanSurfaceSchemes:

==========================
Land/Ocean Surface Schemes
==========================


Land/Ocean surface schemes provide surface temperature, heat and moisture fluxes over land, sea-ice, and ocean points. These serve as a lower boundary condition for the vertical transport in PBL schemes.

**HAFS uses:**

* Noah Multi-Physics (NOAH-MP) land surface model
* HYCOM, MOM6 for ocean models

.. _SurfaceLayerScheme:

====================
Surface-Layer Scheme
====================

Surface layer schemes represent processes near the surface (<~100 m), where vertical gradients are too large to be resolved by vertical grids. They are usually based on Monin-Obukhov similarity theory.
They provide atmospheric exchange coefficients, stability functions (surface fluxes) needed by land/ocean models and PBL.

HAFS uses the GFS surface-layer model, with observation-based roughness length (particularly for strong wind conditions). Modified Zo is important for a successful simulation of TCs. Hurricane intensity is proportional to (Ck/Cd) over the ocean (Emanuel, 1995), where Cd and Ck are the sea-surface momentum and enthalpy exchange coefficients. Larger z0 leads to larger Cd and Ck.

.. _PBLScheme:

==========
PBL Scheme
==========

Turbulent scale mixing/diffusion.

HAFS uses the GFS TKE-EDMF PBL scheme, with a prognostic TKE equation.

Sub-grid scale turbulent flux = Flux of Large eddies + flux of local small eddies
                                   mass-flux (MF)       eddy-diffusivity (ED)

**TC intensity is sensitive to mixing/diffusion. We have two modifications:**

  1. Adjust mixing length near the surface, so that it is consistent with that used in the surface-layer scheme. (HFSA)
  2. LES-based TC PBL adjustment including model coefficients, Richardson# calculation, high-wind mass flux, etc. (HFSB)

.. _ScaleAwareConvection:

===============================================
Scale-aware Deep and Shallow Convection Schemes
===============================================

Scale-aware deep and shallow convection schemes are used when moisture convections are too small to be resolved explicitly by grid spacing; reducing the thermodynamic instability.
HAFS uses GFS Scale-aware SAS. 

**Main assumptions:**

**Deep Convection:**
  - A single entraining and detraining plume model.
  - Base mass flux ~ quasi-equilibrium assumption for dx > 8 km, by a mean updraft velocity dx < 8 km.
  - Triggering conditions include distance between the convection starting level and the level of free convection, sub-cloud convective inhibition & mean relative humidity.

**Shallow convection is similar to deep convection, except:**
  - Base mass flux ~ updraft velocity averaged in a cloud layer.
  - Only convection updrafts are considered in the shallow scheme.
   
HAFS tests show HAFS is sensitive to entrainment & detrainment rates. HFSA uses an entrainment rate larger than that in NCEP GFS.

.. _MicrophysicsSchemes:

====================
Microphysics Schemes
====================

**Effects of vapor-liquid-ice phase changes:**

HAFS uses GFDL microphysics and Thompson microphysics.

**GFDL:**
  - A single-moment scheme. QC, QI, QR, QS, QG.
  - Based on the Lin-Lord-Krueger cloud microphysics scheme.

**Thompson:**
  - QC, QI, QR, QS, QG, + number concentration of cloud ice, cloud water, & rain.
  - Impact of aerosols.

HAFS performs well with the Thompson scheme for TCs in the NATL basin, and with GFDL for TCs in the EAPC basin.

.. _RadiationSchemes:

=================
Radiation Schemes
=================

Radiation Schemes are used for heating and cooling due to short and long wave radiation.

**HAFS uses RRTMG.**

**For computational efficiency, the correlated K-method is used:**
- The SW algorithm includes 112 g-points (quadrature points) in 14 bands.
- The LW algorithm includes 140 unevenly distributed g-points in 16 broad spectral bands.
- Aerosol optical properties, cloud liquid water and ice paths, and effective radius are used to represent the radiative effects of aerosols and clouds in the calculation.
- The effects of sub-grid scale clouds are treated by a Monte-Carlo Independent Column Approximation (McICA) method, with a decorrelation length overlap assumption for multi-layered clouds.

.. _GravityWaveDrag:

=================
Gravity Wave Drag
=================

Impact of sub-grid scale perturbations excited by orography and convection.

**HAFS uses the unified GWD scheme:**
- Mesoscale orographic gravity wave drag
- Low-level flow blocking by subgrid-scale orography
- Effects of gravity waves produced by horizontal terrain variations
- Non-topographic GWD: convection, frontal instability


