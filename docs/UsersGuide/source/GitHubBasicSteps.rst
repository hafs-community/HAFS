.. _BasicSteps:

************************************************************
Introduction to Working with the HAFS Repository in GitHub
************************************************************

(Please see the :ref:`Prerequisites`)

1. Fork the HAFS repository and clone the forked repository locally 
2. Branch from the develop branch for new developments

  a. Create a feature branch, add development, complete testing
  b. Commit changes into the local branch and push the local branch to your personal fork

3. Submit a PR (Pull Request) to request a code review and merge with the authoritative HAFS repository

================================================================
Fork the HAFS repository and clone the forked repository locally
================================================================

1. Navigate to https://github.com/hafs-community/HAFS, locate and click on the "fork" button towards the top right of the page.
2. Clone your new created HAFS fork to get a local copy, e.g.,

    .. code-block:: console

        git clone --recursive https://USERNAME@github.com/USERNAME/HAFS.git

Hint: `GitHub forking projects <https://guides.github.com/activities/forking/>`_

===================================================
Branch from the develop branch for new developments
===================================================

1. Check out the develop branch

    .. code-block:: console

        git checkout develop

2. Create a new branch from the develop branch

    .. code-block:: console

        git checkout -b feature/mybranch

3. Update the submodules

    *If there are changes for the URLs of the submodules, run this command to update the remotes of the submodules*

    .. code-block:: console

        git submodule sync --recursive

    *Note: every time after your clone a git repository or check out a new branch, if submodule changes are expected, you need to update the submodules properly by*

    .. code-block:: console

        git submodule update --init --recursive

4. Make modifications and commit your changes locally

    .. code-block:: console

        vi newcode
        git add newcode
        git commit -m “Add newcode”

5. Push your changes to your GitHub HAFS fork

    .. code-block:: console

        git push origin feature/mybranch

==========================================================================
Submit a pull request for inclusion into the authoritative HAFS repository
==========================================================================

To propose changes for inclusion into the authoritative HAFS repository, you need to create a pull request. 

1. Navigate to https://github.com/hafs-community/HAFS and click on 'new pull request'
2. Select 'compare across forks', set base repository to 'hafs-community/HAFS', base to 'develop', head repository to 'YOUR_GITHUB_USERNAME/HAFS', compare to 'feature/mybranch'
3. Add a descriptive title and short description in the text boxes
4. If this were a real development, you would now click on 'Create pull request'
