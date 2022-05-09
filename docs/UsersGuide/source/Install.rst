.. _Install:

***************************************
Build, Install, and Run the HAFS System
***************************************

Currently, the HAFS application works on these NOAA HPC platforms: wcoss_dell_p3, wcoss_cray, hera, jet, orion.

=================================
Clone and Checkout the Repository
=================================

.. code-block:: console

    git clone --recursive https://github.com/hafs-community/HAFS.git


.. _hafs_build:

=================
Build and Install
=================

.. code-block:: console

    cd HAFS/sorc
    ./install_hafs.sh > install_hafs.log 2>&1

Check and edit **../parm/system.conf** to use your project account and disk areas if needed

===================
Run the HAFS System
===================

Change the cron job driver script to set up your experiment and storm

.. code-block:: console

    cd ../rocoto
    vi cronjob_hafs.sh

Add the driver script in your cron tasks or simply run the driver script

.. code-block:: console

    ./cronjob_hafs.sh
