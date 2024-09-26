.. _Glossary:

**********
Glossary
**********

.. glossary::

   AMVs
      Atmospheric Motion Vectors
   
   CCPP
      The `Common Community Physics Package <https://dtcenter.org/community-code/common-community-physics-package-ccpp>`_ is a forecast-model agnostic, vetted collection of code containing atmospheric physical parameterizations and suites of parameterizations for use in Numerical Weather Prediction (NWP) along with a framework that connects the physics to the host forecast model.

   CDEPS
      The `Community Data Models for Earth Predictive Systems <https://github.com/NOAA-EMC/CDEPS/>`__ repository (CDEPS) contains a set of :term:`NUOPC`-compliant data components and :term:`ESMF`-based "stream" code that selectively removes feedback in coupled model systems. In essence, CDEPS handles the static Data Atmosphere (:term:`DATM`) integration with dynamic coupled model components (e.g., :term:`MOM6`). The CDEPS data models perform the basic function of reading external data files, modifying those data, and then sending the data back to the :term:`CMEPS` mediator. The fields sent to the :term:`mediator` are the same as those that would be sent by an active component. This takes advantage of the fact that the mediator and other CMEPS-compliant model components have no fundamental knowledge of whether another component is fully active or just a data component. More information about DATM is available in the `CDEPS Documentation <https://escomp.github.io/CDEPS/index.html>`__.

   CESM
      The `Community Earth System Model <https://www.cesm.ucar.edu/>`__ (CESM) is a fully-coupled global climate model developed at the National Center for Atmospheric Research (:term:`NCAR`) in collaboration with colleagues in the research community. 

   CMEPS
      The `Community Mediator for Earth Prediction Systems <https://github.com/NOAA-EMC/CMEPS>`__ (CMEPS) is a :term:`NUOPC`-compliant :term:`mediator` used for coupling Earth system model components. It is currently being used in NCAR's Community Earth System Model (:term:`CESM`) and NOAA's subseasonal-to-seasonal (S2S) coupled system. More information is available in the `CMEPS Documentation <https://escomp.github.io/CMEPS/versions/master/html/index.html>`__.
   
   COM
      On operational systems such as WCOSS, this is the name of the directory used to store output files and other data that need to be archived.
   
   CPHC
      Central Pacific Hurricane Center
   
   CPAC
      Central Pacific
   
   cron
   crontab
   cron table
      Cron is a job scheduler accessed through the command-line on UNIX-like operating systems. It is useful for automating workflow tasks. Cron periodically checks a cron table (aka crontab) to see if any tasks are ready to execute. If so, it runs them. 

   data assimilation
      Data assimilation is the process of combining observations, model data, and error statistics to achieve the best estimate of the state of a system. One of the major sources of error in weather and climate forecasts is uncertainty related to the initial conditions that are used to generate future predictions. Even the most precise instruments have a small range of unavoidable measurement error, which means that tiny measurement errors (e.g., related to atmospheric conditions and instrument location) can compound over time. These small differences result in very similar forecasts in the short term (i.e., minutes, hours), but they cause widely divergent forecasts in the long term. Errors in weather and climate forecasts can also arise because models are imperfect representations of reality. Data assimilation systems seek to mitigate these problems by combining the most timely observational data with a "first guess" of the atmospheric state (usually a previous forecast) and other sources of data to provide a "best guess" analysis of the atmospheric state to start a weather or climate simulation. When combined with an "ensemble" of model runs (many forecasts with slightly different conditions), data assimilation helps predict a range of possible atmospheric states, giving an overall measure of uncertainty in a given forecast.

   DATM
      DATM is the *Data Atmosphere* component of :term:`CDEPS`. It uses static atmospheric forcing files (derived from observations or previous atmospheric model runs) instead of output from an active atmospheric model. This reduces the complexity and computational cost associated with coupling to an active atmospheric model. The *Data Atmosphere* component is particularly useful when employing computationally intensive Data Assimilation (:term:`DA <data assimilation>`) techniques to update ocean and/or sea ice fields in a coupled model. In general, use of DATM in place of an active atmospheric component can be appropriate when users are running a coupled model and only want certain components of the model to be active. More information about DATM is available in the `CDEPS Documentation <https://escomp.github.io/CDEPS/versions/master/html/datm.html>`__.

   DOCN
      DOCN is the *Data Ocean* component of :term:`CDEPS`. It uses static ocean forcing files (derived from observations or previous ocean model runs) instead of output from an active ocean model. This reduces the complexity and computational cost associated with coupling to an active ocean model. The *Data Ocean* component is particularly useful when employing computationally intensive Data Assimilation (:term:`DA <data assimilation>`) techniques to update atmospheric fields in a coupled model. In general, use of DOCN in place of an active ocean model (e.g., :term:`MOM6` or :term:`HYCOM`) can be appropriate when users are running a coupled model and only want certain components of the model to be active. More information about DOCN is available in the `CDEPS Documentation <https://escomp.github.io/CDEPS/versions/master/html/docn.html>`__.

   DWAV
      DWAV is the *Data Wave* component of :term:`CDEPS`. It uses static wave forcing files (derived from observations or previous wave model runs) instead of output from an active wave model. This reduces the complexity and computational cost associated with coupling to an active wave model. The *Data Wave* component is particularly useful when employing computationally intensive Data Assimilation (:term:`DA <data assimilation>`) techniques. In general, use of DWAV in place of an active wave model (:term:`WW3`) can be appropriate when users are running a coupled model and only want certain components of the model to be active. More information about DWAV is available in the `CDEPS Documentation <https://escomp.github.io/CDEPS/versions/master/html/dwav.html>`__.

   dycore
   dynamical core
      Global atmospheric model based on fluid dynamics principles, including Euler's equations of motion.

   EDMF
      Eddy-Diffusivity Mass-Flux

   EPAC
      Eastern Pacific

   ESMF
      `Earth System Modeling Framework <https://earthsystemmodeling.org/docs/release/latest/ESMF_usrdoc/>`__. The ESMF defines itself as "a suite of software tools for developing high-performance, multi-component Earth science modeling applications." It is a community-developed software infrastructure for building and coupling models. 

   FGAT
      First Guess at Approximate Time

   FV3
      The Finite-Volume Cubed-Sphere :term:`dynamical core` (dycore). Developed at NOAA's `Geophysical 
      Fluid Dynamics Laboratory <https://www.gfdl.noaa.gov/>`__ (GFDL), it is a scalable and flexible dycore capable of both hydrostatic and non-hydrostatic atmospheric simulations. It is the dycore used in the UFS Weather Model.
   
   GDAS
      Global Data Assimilation System
   
   GFDL
      `Geophysical 
      Fluid Dynamics Laboratory <https://www.gfdl.noaa.gov/>`_
   
   GFS
      `Global Forecast System <https://www.ncei.noaa.gov/products/weather-climate-models/global-forecast>`_. The GFS is a National Centers for Environmental Prediction (:term:`NCEP`) weather forecast model that generates data for dozens of atmospheric and land-soil variables, including temperatures, winds, precipitation, soil moisture, and atmospheric ozone concentration. The system couples several models (including atmosphere, ocean, land/soil, and sea ice) that work together to accurately depict weather conditions.
   
   GOES-R
      Geostationary Operational Environmental Satellite-R Series
   
   GRIB2
      Gridded Binary 2; the second version of the World Meterological Organization's (WMO) standard for distributing gridded data.
   
   HAFS
      Hurricane Analysis and Forecast System
   
   HFSA
      Hurricane Forecast System Configuration A
   
   HFSB
      Hurricane Forecast System Configuration B
   
   HPC
      High-Performance Computing

   HPSS
      NOAA's High Performance Storage System (HPSS)

   HYCOM
      The HYbrid Coordinate Ocean Model (`HYCOM <https://www.hycom.org/>`__) was developed to address known shortcomings in the vertical coordinate scheme of the Miami Isopycnic-Coordinate Ocean Model (MICOM). HYCOM is a primitive equation, general circulation model with vertical coordinates that remain isopycnic in the open, stratified ocean. However, the isopycnal vertical coordinates smoothly transition to z-coordinates in the weakly stratified upper-ocean mixed layer, to terrain-following sigma coordinates in shallow water regions, and back to z-level coordinates in very shallow water. The latter transition prevents layers from becoming too thin where the water is very shallow. See the `HYCOM User's Guide <https://www.hycom.org/attachments/063_hycom_users_guide.pdf>`__ for more information on using the model. The `HYCOM model code <https://github.com/NOAA-EMC/HYCOM-src>`__ is publicly available on GitHub. 
   
   JTWC
      Joint Typhoon Warning Center

   Mediator
      A mediator, sometimes called a coupler, is a software component that includes code for representing component interactions. Typical operations include merging data fields, ensuring consistent treatment of coastlines, computing fluxes, and temporal averaging.

   MOM
   MOM6
   Modular Ocean Model
      MOM6 is the latest generation of the Modular Ocean Model. It is numerical model code for simulating the ocean general circulation. MOM6 was originally developed by the `Geophysical Fluid Dynamics Laboratory <https://www.gfdl.noaa.gov/mom-ocean-model/>`__. Currently, `MOM6 code <https://github.com/mom-ocean/MOM6>`__ and an `extensive suite of test cases <https://github.com/NOAA-GFDL/MOM6-examples/wiki>`__ are available under an open-development software framework. Although there are many public forks of MOM6, the `NOAA EMC fork <https://github.com/NOAA-EMC/MOM6>`__ is used in the UFS Weather Model. 
   
   MP
      Microphysics
   
   NATL
      North Atlantic
   
   NCAR
      The `National Center for Atmospheric Research <https://ncar.ucar.edu/>`__. 

   NCEP
      National Centers for Environmental Prediction (NCEP) is an arm of the National Weather Service
      consisting of nine centers. More information can be found at https://www.ncep.noaa.gov.

   netCDF
      NetCDF (`Network Common Data Form <https://www.unidata.ucar.edu/software/netcdf/>`__) is a file format and community standard for storing multidimensional scientific data. It includes a set of software libraries and machine-independent data formats that support the creation, access, and sharing of array-oriented scientific data.
   
   NHC
      National Hurricane Center
   
   NIO
      Northern Indian Ocean

   NUOPC
   National Unified Operational Prediction Capability
      The `National Unified Operational Prediction Capability <https://earthsystemmodeling.org/nuopc/>`__ is a consortium of Navy, NOAA, and Air Force modelers and their research partners. It aims to advance the weather modeling systems used by meteorologists, mission planners, and decision makers. NUOPC partners are working toward a common model architecture --- a standard way of building models --- in order to make it easier to collaboratively build modeling systems.

   NUOPC Layer
      The :term:`NUOPC` Layer "defines conventions and a set of generic components for building coupled models using the Earth System Modeling Framework (:term:`ESMF`)." 
      NUOPC applications are built on four generic components: driver, model, :term:`mediator`, and connector. For more information, visit the `NUOPC website <https://earthsystemmodeling.org/nuopc/>`__.

   NWP
      Numerical Weather Prediction (NWP) takes current observations of weather and processes them with computer models to forecast the future state of the weather.
   
   OBSPROC
      Observation Processing
   
   PBL
      Planetary Boundary Layer

   RTOFS
      Real-Time Ocean Forecast System

   SASAS
      Simplified Arakawa-Schubert and Adjusted Sigma   

   SH
      Southern Hemisphere

   TC
      Tropical cyclone
   
   TKE
      Turbulent Kinetic Energy

   UFS
      The Unified Forecast System is a community-based, coupled, comprehensive Earth modeling 
      system consisting of several applications (apps). These apps span regional to global 
      domains and sub-hourly to seasonal time scales. The UFS is designed to support the :term:`Weather Enterprise` and to be the source system for NOAA's operational numerical weather prediction applications. For more information, visit https://ufscommunity.org/.

   UFS_UTILS
      A collection of code used by multiple :term:`UFS` applications (e.g., HAFS, the UFS Short-Range Weather App,
      the UFS Medium-Range Weather App). The ``chgres_cube`` code used by HAFS is 
      part of this collection. The code is `publicly available <https://github.com/ufs-community/UFS_UTILS>`__ on Github.

   UPP
      The `Unified Post Processor <https://github.com/NOAA-EMC/UPP>`__ is software developed at :term:`NCEP` and used operationally to 
      post-process raw output from a variety of :term:`NCEP`'s :term:`NWP` models, including the :term:`FV3`. See https://epic.noaa.gov/unified-post-processor/ for more information. 

   Weather Enterprise
      Individuals and organizations from public, private, and academic sectors that contribute to the research, development, and production of weather forecast products; primary consumers of these weather forecast products.

   Weather Model
      A prognostic model that can be used for short- and medium-range research and
      operational forecasts. It can be an atmosphere-only model or an atmospheric
      model coupled with one or more additional components, such as a wave or ocean model. HAFS uses the `UFS Weather Model <https://github.com/ufs-community/ufs-weather-model>`__.
   
   WPAC
      Western Pacific
   
   WW3
   WWIII
   WaveWatch III
      WAVEWATCH III (WW3) is a community wave modeling framework that includes the latest scientific advancements in the field of wind-wave modeling and dynamics. The core of the framework consists of the WAVEWATCH III third-generation wave model (WAVE-height, WATer depth and Current Hindcasting), developed at NOAA/:term:`NCEP`. WAVEWATCH III differs from its predecessors in many important points such as governing equations, model structure, numerical methods, and physical parameterizations. The model code is publicly available on GitHub at https://github.com/NOAA-EMC/WW3. 