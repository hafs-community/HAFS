.. _RegressionTest:

***************
Regression Test
***************

Users can view example HAFS workflow regression tests in ``rocoto/cronjob_hafs_rt.sh``.

To run the HAFS workflow-level regression tests, first navigate to the ``rocoto`` directory.

.. code-block:: console

    cd /path/to/HAFS/rocoto

Open ``cronjob_hafs_rt.sh`` (e.g., ``vi cronjob_hafs_rt.sh``), change the :term:`cron` job driver script for the regression tests, and enable the tests you want run. Then, add the driver script to your cron table or simply run the driver script.

.. code-block:: console

    ./cronjob_hafs_rt.sh

Run the following script to check whether the regression tests finished succesfully or not.

.. code-block:: console

    ./hafs_rt_status.sh
