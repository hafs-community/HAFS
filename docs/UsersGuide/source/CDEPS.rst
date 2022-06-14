.. _CDEPS:

*******************
Using CDEPS in HAFS
*******************

============
Introduction
============

The Community Data Models for Earth Prediction Systems (CDEPS) allows users of coupled Earth system models to reduce the number of feedbacks by replacing one or more active model components with canned datasets. These datasets can be generated from a variety of sources, including prior model runs, reanalyses, and gridded data constructed from observations. An example is initializing and forcing a coupled atmosphere-ocean model (e.g., the HYbrid Coordinate Ocean Model, HYCOM) with the 5th Generation European Centre for Medium-Range Weather Forecasts Reanalysis [#]_ (ERA5), instead of using the UFS Weather Model itself.

The CDEPS implementation in HAFS currently supports data atmosphere (DATM) and data ocean (DOCN) models, and the workflow supports the ERA5 dataset for DATM and the Optimally-Interpolated Sea-Surface Temperature v2.1 [#]_ (OISST) and Group for High-Resolution Sea-Surface Temperature [#]_ (GHRSST) datasets for DOCN. Before running the workflow, the user must stage the required datasets on disk. Download scripts are provided for the supported datasets. Advice for adding support for additional datasets is provided in the appendix.

CDEPS has been added to HAFS under the Improve Workflow Usability, Portability, and Testing Capabilities project being executed by the National Center for Atmospheric Research/Climate and Global Dynamics Laboratory (NCAR/CGD) and the University of Colorado/Cooperative Institute for Research in Environmental Sciences (CU/CIRES). These efforts are funded under the Infrastructure portfolio of the Fiscal Year 2018 Disaster Related Appropriation Supplemental (DRAS), commonly referred to as the Hurricane Supplemental (HSUP).

==================
Obtaining the Code
==================

The HAFS-CDEPS capability is contained in the **develop** branch of **hafs-community/HAFS**.

To obtain the code:

.. code-block:: console

    git clone --recursive -b develop https://github.com/hafs-community/HAFS

===========
Data Access
===========

Before you can perform a DATM run with ERA5, you must create a Climate Data Store account (https://cds.climate.copernicus.eu) and digitally sign the ERA5 license agreement. You will be assigned a key, which should be added to a file called .cdsapirc in your home directory on the machine(s) you plan to use. The process is described in more detail in https://cds.climate.copernicus.eu/api-how-to.

There are no prerequisites to downloading supported datasets for DOCN.

=============
Data Download
=============

Before running the workflow, the user must download the necessary input data. Three scripts are provided for this purpose in the **ush/cdeps_utils/** directory:

- hafs_era5_download.py
- hafs_oisst_download.py
- hafs_ghrsst_download.py

The scripts must be run with Python 3.6 or higher. The required Python packages are listed at the top of each script. The hafs_era5_download.py script also requires the Climate Data Operators (CDO) module to be loaded beforehand.

The scripts can be run in the following manner:

.. code-block:: console

    ./hafs_X_download.py [options] day [day [...]]

where day can be specified in any of the following ways:

- 20210815 = specify one day: August 15, 2021
- 20210815-20210819 = specify a range of days: August 15th to 19th, 2021
- 2018 = specify an entire year (2018)

Data must be downloaded for the entire length of the forecast, plus one day before and one day after. For example, if you are running a 126-hr forecast initialized at 2021081518, you should run the download script like this:

.. code-block:: console

    ./hafs_X_download.py 20210814-20210822

After downloading the data, specify its location using **DATMdir** or **DOCNdir** in **parm/system.conf**.

======================
Building CDEPS in HAFS
======================

The DAPP keyword in the call to **./compile.sh** in **./sorc/build_forecast.sh** should be set to **-DAPP=HAFS-ALL** to build HAFS with support for data models. The resulting executable can also be used for HAFS runs with active atmosphere and ocean models. 

Unless you are working on wcoss_cray, the DAPP keyword should already be set to HAFS-ALL.
 
The remainder of the build process is the same as described in the :ref:`HAFS installation guide <hafs_build>`.

================================
Using CDEPS in the HAFS Workflow
================================

The HAFS workflow can be used to run data model experiments with minimal modifications, which are described below.

Modify the **./rocoto/cronjob_hafs_cdeps.sh** script:

1) Uncomment the definitions of HOMEhafs, dev, and PYTHON3 appropriate for the HPC platform that you are using.

2) Set HOMEhafs to the top-level directory that contains the HAFS scripts and source codes.

3) Near the bottom of the script, review the commands for the three DATM and DOCN experiments, and comment out the commands for any experiments that you do not want to run:

    a) To run the DATM with ERA5, the command is:

    .. code-block:: console

        ${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY \
     			config.EXPT=${EXPT} \
			config.SUBEXPT=${EXPT}_era5 \
			forecast.output_history=.true. \
     			../parm/hafs_regional_static.conf \
     			../parm/hafs_hycom.conf \
     			../parm/hafs_datm.conf \
     			../parm/hafs_datm_era5.conf

    b) To run the DOCN with OISST, the command is:

    .. code-block:: console

        ${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY \
     			config.EXPT=${EXPT} \
			config.SUBEXPT=${EXPT}_oisst \
			forecast.output_history=.true. \
     			../parm/hafs_regional_static.conf \
     			../parm/hafs_docn.conf \
     			../parm/hafs_docn_oisst.conf

    c) To run the DOCN with GHRSST, the command is:

    .. code-block:: console

        ${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY \
     			config.EXPT=${EXPT} \
			config.SUBEXPT=${EXPT}_ghrsst \
			forecast.output_history=.true. \
     			../parm/hafs_regional_static.conf \
     			../parm/hafs_docn.conf \
     			../parm/hafs_docn_ghrsst.conf

The cycle (e.g., 2019082900) and storm (e.g., 00L) can be modified. The final two files in each command configure the CDEPS data models (see :ref:`appendix-a`). It is probably not necessary to change the configuration unless you want to customize the experiment.

Before submitting the cron script, remember to create the **./parm/system.conf** file and to link the fix files using **./sorc/link_fix.sh**. This process is described by the HAFS Developers Guide. (*come back and link to the correct portion of the developers guide*)

After the above steps are complete, submit the cron script repeatedly (or add the script to your crontab) until the workflow completes. The steps that will be executed are shown below (:ref:`DATM <fig_datm>` and :ref:`DOCN <fig_docn>`) for a simple workflow without vortex initialization or data assimilation. However, those options are supported for DOCN.

.. _fig_datm:

.. figure:: images/hafs_cdeps_workflow_datm.png
    :scale: 50 %
    :alt: HAFS-CDEPS workflow for DATM

    Schematic diagram of the HAFS-CDEPS workflow for DATM. Blue text indicates the jobs that will run. Gray text indicates jobs that only run when data models are not used.

.. _fig_docn:

.. figure:: images/hafs_cdeps_workflow_docn.png
    :scale: 50 %
    :alt: HAFS-CDEPS workflow for DOCN

    Schematic diagram of the HAFS-CDEPS workflow for DOCN. Blue text indicates the jobs that will run. Gray text indicates jobs that only run when data models are not used.

====================================
Limitations and Other Considerations
====================================

HAFS-CDEPS can only be used in the HAFS regional configuration, since the ocean coupling for the global-nesting configuration was still being developed at the time of this project. In addition, the CDEPS DATM and DOCN are mutually exclusive and cannot be run at the same time in HAFS. Finally, the only fully-supported datasets are ERA5 for DATM and OISST and GHRSST for DOCN. Some tips for adding a custom dataset are discussed in :ref:`appendix-b`.

====================
For More Information
====================

The official documentation for CDEPS is available from https://escomp.github.io/CDEPS/html/index.html.


.. _appendix-a:

============================================
Appendix A: HAFS-CDEPS Configuration Options
============================================

The following table describes variables that are relevant to the HAFS-CDEPS configuration, along with some recommendations for setting them. The recommended settings have already been applied in the various configuration files.

.. csv-table:: HAFS-CDEPS Configuration Options
    :file: tables/hafs_cdeps_config.csv
    :widths: auto
    :header-rows: 1

.. _appendix-b:

===================================================
Appendix B: Considerations for Adding a New Dataset
===================================================

While it is impossible to formally support every dataset in HAFS-CDEPS, developers who wish to use a dataset of their own choosing are encouraged to follow these steps:

1) If you want to prepare a data atmosphere from a custom dataset, consider running the DATM with ERA5 first so that you have a reference. Likewise, if preparing a data ocean, run the DOCN with either OISST or GHRSST data first.

2) You may wish to write your own script (or modify the existing scripts) to download the dataset of interest. See the three **ush/cdeps_utils/*download.py** scripts. You should also set DATMdir or DOCNdir in **./parm/system.conf** to the location of your staged data.

3) The input data you provide must be in netcdf format, and the time axis in the file(s) must be CF-1.0 compliant.

4) You will probably need to modify **scripts/exhafs_datm_prep.sh** or **scripts/exhafs_docn_prep.sh** to add a new data source and corresponding script to the workflow to preprocess your data files. Alternatively, if you have already preprocessed your data outside of the workflow and simply need to copy them to the working directory, you could simply modify an existing if statement in the script. For example, for a DOCN run,

    .. code-block:: console

        if [[ "$docn_source" == OISST ]] ; then    			
        $USHhafs/produtil_deliver.py -c "$DOCNdir/my_dataset.nc" "$docn_input_path/DOCN_input_00000.nc"

    where **my_dataset.nc** is your input dataset. This command will copy your input data file from DOCNdir to the correct working directory during the ocn_prep job.

5) The mapping between the variable names in your dataset and the names used internally by CDEPS is described by the **stream_data_variables** keys in **./parm/cdeps/datm_era5.streams** (DATM) and **./parm/cdeps/docn_oisst.streams** and **./parm/cdeps/docn_ghrsst.streams** (DOCN). You should make the first entry in each pair of variable names correspond to the name of the variable in your dataset.

6) For a run that couples DATM to HYCOM, the variables that must be present in your input dataset (along with the expected units) are as follows:

.. csv-table:: Required Input Variable(s) for DATM to HYCOM
    :file: tables/input_vars_datm.csv
    :widths: auto
    :header-rows: 1

For a run that couples DOCN to the UFS Weather Model, the only variable that must be present in your input dataset (along with the expected unit) is as follows:

.. csv-table:: Required Input Variable(s) for DOCN to UFS Weather Model
    :file: tables/input_vars_docn.csv
    :widths: auto
    :header-rows: 1

7) In addition to preparing the input data, you will also need to create a mesh file that describes the input data grid. It should be possible to leverage the existing **./ush/cdeps_utils/hafs_esmf_mesh.py** script for this purpose, but it has only been tested with ERA5 (DATM) and OISST and GHRSST (DOCN) data. Tri-polar grids, such as those used in the Real-Time Ocean Forecast System (RTOFS) dataset, may require modifications to **hafs_esmf_mesh.py**. If you generate your own mesh, you should set **make_mesh_atm** or **make_mesh_ocn** to no and provide the path to the mesh using **mesh_atm_in** or **mesh_ocn_in** (see :ref:`appendix-a`).


.. rubric:: Footnotes

.. [#] https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels
.. [#] https://www.ncdc.noaa.gov/oisst/optimum-interpolation-sea-surface-temperature-oisst-v21
.. [#] https://www.ghrsst.org/about-ghrsst/overview/
