.. _TechOverview:

*******************
Technical Overview
*******************

This chapter contains an overview of HAFS prerequisites, HAFS repository management and structure, and the workflow design. 

.. _Prerequisites:

===================================================
Prerequisites for Working with the HAFS Repository
===================================================

* Git version 1.8.2 or newer, which added the possibility to track branches of submodules. The following versions of Git are used on NOAA HPCs: 

  * **Orion**: 

    * Git version 1.8.3.1 (system default)
    * Git version 2.21.0 (available via ``module load git/2.21.0``)

  * **Jet/Hera**: Git version 2.18.0
  * **WCOSS Dell**: Git version 1.8.3.1
  * **WCOSS Cray** (cannot access GitHub)

    * Git version 1.7.12.4 (system default) 
    * Git version 2.14.2 (available)

      * ``module use /gpfs/hps3/emc/hwrf/noscrub/soft/modulefiles``
      * ``module load git/2.14.2``

* `GitHub <https://github.com/>`__ account (e.g., FirstLast-NOAA)

.. _RepositoryManagement:

==================================
HAFS GitHub Repository Management
==================================

The authoritative HAFS repository is publicly available on GitHub at: https://github.com/hafs-community/HAFS. 
It is maintained by EMC and DTC with developments and contributions from the :term:`UFS` HAFS application team and the hurricane research community.

**HAFS Branching/Tagging Conventions:**

  - ``develop``: The main development branch
  - ``support/[name]``: Branches used by operational implementations or HFIP real-time parallel experiments (e.g., ``support/hafs.v0.2.0``)
  - ``release/vx.x.x``: Public release branches (e.g., ``release/v1.0.0``)
  - ``[name].v#.#.#`` (*tags*): hafs.v0.2.0, hafs.v1.0.0, public.v1.0.0, etc.
  - ``hotfix/[name]``: Temporary bug fix branches
  - ``feature/[name]``: Feature branches for adding new capabilities or enhancements

The HAFS repository only hosts major feature branches (e.g., ``feature/hafs_nesting``) that require active group collaborations. Individual developers can work on feature branches in their personal HAFS forks. 

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_branching_diagram.png
    :width: 50 %
    :alt: Example HAFS Branching Diagram

    Example HAFS Branching Diagram

.. _dir-structure:

========================
HAFS Directory Structure
========================

The following shows the names of the files and directories in the ``HAFS`` repository.

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_directory_structure.png
    :width: 50 %
    :alt: HAFS directory names and explanations (updated 06/29/2023)


.. _Submodules:

================
HAFS Submodules
================

HAFS contains the following subcomponents:

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_submodules.png
    :width: 75 %
    :alt: HAFS Subcomponents/Submodules (updated 11/02/2021)

.. _Workflow:

========================
HAFS Workflow Schematic
========================

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_workflow_schematic.png
    :width: 75 %
    :alt: HAFS Workflow Schematic
