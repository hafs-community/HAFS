.. _RepositoryManagement:

*********************************
HAFS GitHub Repository Management
*********************************

The authoritative HAFS repository is publicly available on GitHub at: https://github.com/hafs-community/HAFS. 
It is maintained by EMC and DTC with developments and contributions from the UFS-HAFS application team and the hurricane research community.

**HAFS Branching/Tagging Conventions:**

  - ``develop``: the main development branch
  - ``support/[name]``: branches used by operational implementations or HFIP real-time parallel experiments (e.g., support/hafs.v0.2.0)
  - ``release/vx.x.x``: public release branches (e.g., release/v1.0.0)
  - ``[name].v#.#.#`` (*tags*): hafs.v0.2.0, hafs.v1.0.0, public.v1.0.0, etc.
  - ``hotfix/[name]``: temporary bug fix branches
  - ``feature/[name]``: feature branches for adding new capabilities or enhancements

The HAFS repository only hosts major feature branches (e.g., ``feature/hafs_nesting``) that require active group collaborations. Individual developers can work on feature branches in their personal HAFS forks. 

.. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/hafs_branching_diagram.png
    :width: 50 %
    :alt: Example HAFS Branching Diagram

    Example HAFS Branching Diagram
