.. _Glossary:

**********
Glossary
**********

.. glossary::

   CCPP
      The `Common Community Physics Package <https://dtcenter.org/community-code/common-community-physics-package-ccpp>`_ is a forecast-model agnostic, vetted collection of code containing atmospheric physical parameterizations and suites of parameterizations for use in Numerical Weather Prediction (NWP) along with a framework that connects the physics to the host forecast model.

   cron
   crontab
   cron table
      Cron is a job scheduler accessed through the command-line on UNIX-like operating systems. It is useful for automating workflow tasks. Cron periodically checks a cron table (aka crontab) to see if any tasks are are ready to execute. If so, it runs them. 

   data assimilation
      Data assimilation is the process of combining observations, model data, and error statistics to achieve the best estimate of the state of a system. One of the major sources of error in weather and climate forecasts is uncertainty related to the initial conditions that are used to generate future predictions. Even the most precise instruments have a small range of unavoidable measurement error, which means that tiny measurement errors (e.g., related to atmospheric conditions and instrument location) can compound over time. These small differences result in very similar forecasts in the short term (i.e., minutes, hours), but they cause widely divergent forecasts in the long term. Errors in weather and climate forecasts can also arise because models are imperfect representations of reality. Data assimilation systems seek to mitigate these problems by combining the most timely observational data with a "first guess" of the atmospheric state (usually a previous forecast) and other sources of data to provide a "best guess" analysis of the atmospheric state to start a weather or climate simulation. When combined with an "ensemble" of model runs (many forecasts with slightly different conditions), data assimilation helps predict a range of possible atmospheric states, giving an overall measure of uncertainty in a given forecast.

   dycore
   dynamical core
      Global atmospheric model based on fluid dynamics principles, including Euler's equations of motion.

   FV3
      The Finite-Volume Cubed-Sphere :term:`dynamical core` (dycore). Developed at NOAA's `Geophysical 
      Fluid Dynamics Laboratory <https://www.gfdl.noaa.gov/>`__ (GFDL), it is a scalable and flexible dycore capable of both hydrostatic and non-hydrostatic atmospheric simulations. It is the dycore used in the UFS Weather Model.

   HPC
      High-Performance Computing.

   HPSS
      NOAA's High Performance Storage System (HPSS).

   NCEP
      National Centers for Environmental Prediction (NCEP) is an arm of the National Weather Service
      consisting of nine centers. More information can be found at https://www.ncep.noaa.gov.

   NWP
      Numerical Weather Prediction (NWP) takes current observations of weather and processes them with computer models to forecast the future state of the weather. 

   TC
      Tropical cyclone.

   UFS
      The Unified Forecast System is a community-based, coupled, comprehensive Earth modeling 
      system consisting of several applications (apps). These apps span regional to global 
      domains and sub-hourly to seasonal time scales. The UFS is designed to support the :term:`Weather Enterprise` and to be the source system for NOAA's operational numerical weather prediction applications. For more information, visit https://ufscommunity.org/.

   UFS_UTILS
      A collection of code used by multiple :term:`UFS` apps (e.g., the UFS Short-Range Weather App,
      the UFS Medium-Range Weather App). The grid, orography, surface climatology, and initial 
      and boundary condition generation codes used by the UFS Short-Range Weather App are all 
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

