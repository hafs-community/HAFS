.. _ExtQuickStart:

*******************************
HAFS Extended Quick Start Guide
*******************************

Currently, the HAFS application works on these NOAA HPC platforms: 

* wcoss_dell_p3
* wcoss_cray
* hera
* jet
* orion

=================================
Clone and Checkout the Repository
=================================

.. code-block:: console

    git clone <-b BRANCH> --recursive https://github.com/hafs-community/HAFS.git

Select the branch to clone by setting the ``<-b BRANCH>`` option to the branch name.

.. note::
   ``develop`` is the default branch.

======================
Build and Install HAFS
======================

.. code-block:: console

    cd /path/to/HAFS/sorc
    ./install_hafs.sh > install_hafs.log 2>&1

.. Hint::
   Got errors? Look into the ``HAFS/sorc/logs`` directory.

------------------------
Parts of install_hafs.sh
------------------------

* ``machine-setup.sh`` Determine shell, identify machine, and load modules

* ``build_all.sh`` Compile components: forecast, post, tracker, utils, tools, hycom, ww3, and gsi

* ``install_all.sh`` Copy executables to ``exec`` directory

* ``link_fix.sh`` Link fix files (fix files are available on disk on supported platforms)

===================
Run the HAFS System
===================

----------------
Edit system.conf
----------------

To configure an experiment, run: 

.. code-block:: console

    cd /path/to/HAFS/parm
    cp system.conf.<system> system.conf
    vi system.conf

where ``<system>`` is replaced by the name of one of the supported platforms listed :ref:`above <QuickStart>`.

Edit the following:

* ``disk_project``: Project name for disk space. 

* ``tape_project``: :term:`HPSS` project name.

* ``cpu_account``: CPU account name for submitting jobs to the batch system (may be the same as ``disk_project``)

* ``archive=disk``: Archive location (make sure you have write permission)

* ``CDSAVE``: HAFS parent directory

* ``CDNOSCRUB``: Track files will be copied to this location --- contents will not be scrubbed (user must have write permission)

* ``CDSCRUB`` If scrub is set to yes, this directory will be removed (user must have write permission)

.. _physics:

---------------------------
HAFS Physics Configuration
---------------------------

Look in ``HAFS/parm/hafs.conf`` to determine what physics suites are running.

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_ccpp_suites.png
    :width: 50%
    :alt: CCPP suites listed in hafs.conf (updated 06/29/2023)

To determine what physics schemes are included in the suites mentioned above, run:

.. code-block:: console

    more HAFS/sorc/hafs_forecast.fd/FV3/ccpp/suites/suite_FV3_HAFS_v1_gfdlmp_tedmf_nonsst.xml

.. _namelist-files:

---------------------------
HAFS Nesting Configuration
---------------------------

Two types of nesting configurations are available: (i) regional* and (ii) globnest.

* Two namelist files (templates) for regional configuration are:

  * ``HAFS/parm/forecast/regional/imput.nml.tmp``
  * ``HAFS/parm/forecast/regional/input_nest.nml.tmp``

* One namelist file (template) for globnest configuration is:

  * ``HAFS/parm/forecast/globnest/input.nml.tmp``

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_namelist_files.png
    :width: 50 %
    :alt: Example namelist file for HAFS (updated 06/29/2023)

\* operational implementation

----------------------------
XML File to Run the Workflow
----------------------------

.. code-block:: console

    cd /path/to/HAFS/rocoto
    vi system.conf

In ``HAFS/rocoto/hafs_workflow.xml.in`` the following can be modified to set the number of cycles and tasks.

* ``<!ENTITY CYCLE THROTTLE “5”>``: The number of cycles that can be activated at one time
* ``<!ENTITY TASK_THROTTLE “120”>``: The number of tasks that can be activated at one time
* ``<!ENTITY MAX_TRIES “1”>``: The maximum number of tries for all tasks

-------------------------------
Edit the Cron Job Driver Script
-------------------------------

Change the cron job driver script to set up the experiment and storm.

.. code-block:: console

    cd /path/to/HAFS/rocoto
    vi cronjob_hafs.sh

Make sure to check ``HOMEhafs`` and edit as appropriate.

--------
Run HAFS
--------

Add the driver script to cron or simply run the driver script.

.. code-block:: console

    ./cronjob_hafs.sh

To add the script to cron:

.. code-block:: console

    crontab -e
    */5 * * * * <path-to-HAFS>/rocoto/cronjob_hafs.sh

.. note::

   On Orion, cron is only available on the orion-login-1 node.
