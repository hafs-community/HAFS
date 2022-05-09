.. _RegressionTest:

***************
Regression Test
***************

Example HAFS workflow regression tests can be seen in **rocoto/cronjob_hafs_rt.sh**.

To run the hafs workflow level regression tests:

.. code-block:: console

    cd ../rocoto

Change the cron job driver script for the regression tests and enable the tests you want run

.. code-block:: console

    vi cronjob_hafs_rt.sh

Add the driver script in your cron tasks or simply run the driver script

.. code-block:: console

    ./cronjob_hafs_rt.sh
