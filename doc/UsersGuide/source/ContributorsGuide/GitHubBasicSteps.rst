.. _BasicSteps:

************************************************************
Introduction to Working with the HAFS Repository in GitHub
************************************************************

(Please see the :ref:`Prerequisites` before beginning.)

General steps to add new developments to the HAFS GitHub repository:

#. Fork the HAFS repository and clone the forked repository locally.
#. Create a feature branch from the ``develop`` branch to add new developments.

   a. Add developments.
   b. Complete testing.
   c. Commit changes into the local branch and push the local branch to your personal fork.

#. Submit a Pull Request (PR) to request a code review and merge new developments with the authoritative HAFS repository.

=================================================================
Fork the HAFS Repository and Clone the Forked Repository Locally
=================================================================

Users need a GitHub account to work with the HAFS repository. Instructions for creating an account are available `on GitHub <https://github.com/signup>`__ and via the `Earth Prediction Innovation Center website <https://epic.noaa.gov/wp-content/uploads/2023/01/Registration-and-Posting-on-the-UFS-Community-GitHub-Repository.pdf>`__. 

#. Navigate to https://github.com/hafs-community/HAFS. Locate and click on the "fork" button towards the top right of the page. Users can create a fork by selecting the default settings; more advanced users may opt to alter the defaults. 

   .. figure:: https://github.com/hafs-community/HAFS/wiki/docs_images/ForkHAFS.png
      :width: 90%
      :alt: Yellow box highlights the "fork" button toward the top right of the page. 

#. Clone the newly created HAFS fork, which will copy the repository onto the user's local system. 

    .. code-block:: console

        git clone --recursive https://USERNAME@github.com/USERNAME/HAFS.git

   Users should replace ``USERNAME`` with their GitHub username in the command above. 

   .. hint:: See `GitHub forking projects <https://docs.github.com/en/get-started/exploring-projects-on-github/contributing-to-a-project>`__

====================================================
Create a Feature Branch from the ``develop`` Branch
====================================================

#. Check out the ``develop`` branch:

    .. code-block:: console

        git checkout develop

#. Create a new branch from the ``develop`` branch:

    .. code-block:: console

        git checkout -b feature/mybranch

#. Update the submodules:

    *If there are changes for the URLs of the submodules, run this command to update the remote versions of the submodules*

    .. code-block:: console

        git submodule sync --recursive

    *Note: every time after you clone a git repository or check out a new branch, if submodule changes are expected, you need to update the submodules properly by*

    .. code-block:: console

        git submodule update --init --recursive

#. Make modifications and commit changes locally:

    .. code-block:: console

        vi NewCodeFile
        git add NewCodeFile
        git commit -m “Add newcode description”

#. Push changes to your GitHub HAFS fork:

    .. code-block:: console

        git push origin feature/mybranch

==========================================================================
Submit a Pull Request for Inclusion into the Authoritative HAFS Repository
==========================================================================

To propose changes for inclusion into the authoritative HAFS repository, developers need to create a pull request. 

#. Navigate to :hafs-repo:`pulls` and click on *New pull request*.
#. Click on *Compare across forks*. 
   a. Set the base repository to *hafs-community/HAFS* and the base branch to ``develop``.
   b. Set the head repository to *YOUR_GITHUB_USERNAME/HAFS* and the compare branch to ``feature/mybranch``.
#. Add a descriptive title and short description in the text boxes describing the changes.
#. Click on *Create pull request*.
