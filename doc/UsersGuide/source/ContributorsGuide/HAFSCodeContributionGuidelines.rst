.. _HAFSCodeContributionGuidelines:

***********************************************
UFS Repository Code Management Guidance (Draft)
***********************************************

The authoritative HAFS repository is located at https://github.com/hafs-community/HAFS. The HAFS repository maintains a main branch for development called ``develop``. The HEAD of ``develop`` reflects the latest peer-reviewed development changes and is read-only for users. It points to regularly updated hashes for individual subcomponents. The ``develop`` branch is protected; changes can only be made by pull request, not by pushing directly to the repository.

.. _gitflow:

GitFlow
========

Contributors who have write access to the `HAFS <https://github.com/hafs-community/HAFS>`_ project in GitHub should follow `GitFlow development guidelines <https://nvie.com/posts/a-successful-git-branching-model/>`_ for any development performed directly in the ``hafs-community/HAFS`` repository. Changes to the ``develop`` branch require a pull request (see :ref:`Fork and PR Overview <fork-pr-overview>`). 

Contributors who do not have write permissions for the HAFS repository must conduct all development in a fork and submit changes via pull request (PR) to the authoritative repository. This process is summarized in the :ref:`Fork and PR Overview <fork-pr-overview>` below. 

.. _fork-pr-overview:

Fork and PR Overview
=====================

.. note:: 
   
   Thank you to the Unified Workflow (UW) team for allowing us to adapt their Fork and PR Model overview for use in HAFS. The original can be viewed in the `uwtools` :uw:`documentation <sections/contributor_guide/fork_pr_model.html>`.


Contributions to the HAFS project are made via a :github-docs:`Fork<pull-requests/collaborating-with-pull-requests/working-with-forks/about-forks>` and :github-docs:`Pull Request (PR)<pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests>` model. GitHub provides a thorough description of this contribution model in their `Contributing to a project` :github-docs:`Quickstart<get-started/exploring-projects-on-github/contributing-to-a-project>`, but the steps, with respect to HAFS contributions, can be summarized as:

#. :github-docs:`Create an issue <issues/tracking-your-work-with-issues/creating-an-issue>` to document proposed changes.
#. :github-docs:`Fork<get-started/exploring-projects-on-github/contributing-to-a-project#forking-a-repository>` the HAFS repository into your personal GitHub account.
#. :github-docs:`Clone<get-started/exploring-projects-on-github/contributing-to-a-project#cloning-a-fork>` your fork onto your development system.
#. :github-docs:`Create a branch<get-started/exploring-projects-on-github/contributing-to-a-project#creating-a-branch-to-work-on>` in your clone for your changes. All development should take place on a branch, *not* on ``develop``. 
#. :github-docs:`Make, commit, and push changes<get-started/exploring-projects-on-github/contributing-to-a-project#making-and-pushing-changes>` in your clone / to your fork. 
#. When your work is complete, :github-docs:`create a pull request (PR)<get-started/exploring-projects-on-github/contributing-to-a-project#making-a-pull-request>` to merge your changes. 

For future contributions, you may delete and then recreate your fork or configure the official ``HAFS`` repository as a :github-docs:`remote repository<pull-requests/collaborating-with-pull-requests/working-with-forks/configuring-a-remote-repository-for-a-fork>` on your clone and :github-docs:`sync upstream changes<pull-requests/collaborating-with-pull-requests/working-with-forks/syncing-a-fork>` to stay up-to-date with the official repository.

General Coding Standards
=========================

* The HAFS repository follows the :term:`NCEP` Central Operations (NCO) :nco:`WCOSS Implementation Standards <>`. 
* The HAFS repository must not contain source code for compiled programs. Only scripts and configuration files should reside in this repository.
* All bash scripts must explicitly be ``#!/bin/bash`` scripts. They should not be login-enabled (i.e., scripts should not use the ``-l`` flag).
* All code must be indented appropriately and conform to the style of existing scripts (e.g., local variables should be lowercase, global variables should be uppercase).
* No personal software installations (including libraries or code) or personal directories may be incorporated into HAFS repository code. 

Development and Testing Process
=================================

#. **Create issue:** Open an :hafs-repo:`issue <issues/new/choose>` in the HAFS repository to document proposed changes. See :ref:`Opening an Issue <open-issue>` for detailed instructions.
#. **Fork & Clone HAFS:** :github-docs:`Fork<get-started/exploring-projects-on-github/contributing-to-a-project#forking-a-repository>` the :hafs-repo:`HAFS repository<>` into your personal GitHub account and :github-docs:`clone<get-started/exploring-projects-on-github/contributing-to-a-project>` your fork onto your development system if you have not already done so.
#. **Create a branch** in your clone for your changes. All development should take place on a branch, not on ``develop``. Branches are typically named as follows, where ``[name]`` is a one-word description of the branch:

   * ``bugfix/[name]``: Fixes a demonstrably incorrect portion of code
   * ``feature/[name]``: Adds a new feature to the code or improves an existing portion of the code
   * ``text/[name]``: Changes elements of the repository that do not impact the compiled code in any way (e.g., changes to README, documentation, comments, changing quoted Registry elements, white space alignment). 
   * Only code managers may create ``release/*`` or ``production/*`` branches, which are used for public or operational releases, respectively. `Information on versioning <https://github.com/ufs-community/ufs/wiki/Versioning>`_ is available on the *ufs-community* wiki. 

   Users will need to sync the branches in their fork with the authoritative HAFS repository periodically. 
#. **Development:** Perform and test changes in the feature branch (not on ``develop``!). Document changes to the workflow and capabilities in the RST files so that the HAFS documentation stays up-to-date. 
#. **Testing:** Test code modifications on as many platforms as possible, and request help with further testing from the code management team when unable to test on all Level 1 platforms. The bare minimum testing required before opening a PR is to run the regression tests on at least one supported machine. :numref:`Section %s <RegressionTest>` of the HAFS User's Guide provides instructions on HAFS regression testing. 
#. **Pull Request:** When your work is complete, :github-docs:`create a pull request<get-started/exploring-projects-on-github/contributing-to-a-project#making-a-pull-request>` to merge your changes. When a PR is initiated, the :ref:`PR template <pr-template>` autofills. Developers should use the template to provide information about the PR in the proper fields. See the guidelines in the :ref:`Making a Pull Request <make-pr>` section for more details on making a good pull request. 
#. **Merge** - When review and testing are complete, a code manager will merge the PR into ``develop`` (or another specified branch). 
#. **Cleanup** - After the PR is merged, the code developer should delete the branch on their fork and close the issue. Feature branches are intended to be short-lived, concentrated on code with one sole purpose, and applicable to a single PR. A new feature branch should be created when subsequent code development continues.

.. note:: 
   
   Communication with code managers and the repository code management team throughout the process is encouraged.

.. _open-issue:

Opening an Issue
=================

All changes to HAFS should be associated with a :hafs-repo:`GitHub Issue <issues>`. Developers should search the existing issues in the HAFS repository before beginning their work. If an issue does not exist for the work they are doing, they should create one prior to opening a new pull request. If an issue does exist, developers should be sure to collaborate to avoid duplicative work. 

To open an issue, click on :hafs-repo:`"New Issue"<issues/new/choose>` within the HAFS GitHub repository. 

Choose from three options: 

#. :hafs-repo:`Bug Report <issues/new?assignees=&labels=bug&projects=&template=bug_report.md&title=>`: Report specific problems ("bugs") in the code using the following template:

   .. code-block:: console

      ## Description
      Provide a clear and concise description of what the bug is.
      Also give a description of what behavior you expected to happen.

      ### To Reproduce:
      What machines are you seeing this with?
      Give explicit steps to reproduce the behavior if possible.
      1. do this
      2. then that
      3. then, oops, look at the bug

      ## Additional context (optional)
      Add any other context about the problem here.
      Directly reference any issues or PRs in this or other repositories that this is related to, and describe how they are related. Examples:
      - needs to be fixed also in ufs-community/ufs-weather-model/issues/<issue_number>
      - dependent upon noaa-emc/upp/pull/<pr_number>

      ## Output (optional)

      **Screenshots**
      If applicable, drag and drop screenshots to help explain your problem.

      **output logs**
      If applicable, include relevant output logs.
      Either drag and drop the entire log file here (if a long log) or

      ```
      paste the code here (if a short section of log)
      ```

#. :hafs-repo:`Feature Request <issues/new?assignees=&labels=enhancement&projects=&template=feature_request.md&title=>`: New features and feature enhancements fall under this category. Propose features and enhancements using the following template. Optional sections may be deleted.

   .. code-block:: console

      ## Description
      Provide a clear and concise description of the requested feature/capability.

      ## Proposed solution
      How should the new feature/capability be added? If you have thoughts on the implementation strategy, please share them here.

      ## Status (optional)
      Do you (or a colleague) plan to work on adding this feature?

      ## Related to (optional)
      Directly reference any related issues or PRs in this or other repositories, and describe how they are related. Examples:
      - fixed by hafs-community/hafs/pull/<pr_number>
      - dependent upon ufs-community/ufs-weather-model/pull/<pr_number>
      - associated with noaa-emc/upp/pull/<pr_number>
      - related to hafs-community/GSI/issues/<issue_number>

#. :hafs-repo:`Other <issues/new>`: Open a blank issue, and use the "Feature Request" template above as a starting point to describe the issue. 

For all issue reports, indicate whether this is: 
   #. A problem that you plan to work on and submit a PR for
   #. A problem that you will **not** work on but that requires attention
   #. A suggested improvement 

After filling out the issue report, click on "Submit new issue."

.. _make-pr:

Making a Pull Request
======================

All changes to the HAFS ``develop`` branch should be handled via GitHub’s "Pull Request" (PR) functionality. When creating your PR, please follow these guidelines, specific to the HAFS project:

* Ensure that your PR is targeting the base repository ``hafs-community/HAFS`` and an appropriate base branch (usually ``develop``).
* Before making a pull request, ensure that your branch is sync'd with the corresponding branch in the authoritative repository (usually ``develop``). All conflicts must be resolved, and regression tests should be passing on at least one supported platform.
* **Complete PR template.** Your PR will appear pre-populated with a :ref:`template <pr-template>` that you should complete. Provide an informative synopsis of your contribution, crosslink the issue(s) and dependencies, and indicate what testing has been conducted. You may tidy up the description by removing boilerplate text and non-selected checklist items.  
* **Create draft PR.** Use the pull-down arrow on the green button below the description to initially create a :github-docs:`draft pull request<pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests#draft-pull-requests>`. 

   * Once your draft PR is open, visit its *Files changed* tab and add comments to any lines of code where you think reviewers will benefit from more explanation. Try to save time by proactively answering questions you suspect reviewers will ask.

* **Open PR.** Once your draft PR is marked up with your comments and ready for review, return to the *Conversation* tab and click the *Ready for review* button.

   * A default set of reviewers will automatically be added to your PR. You may add or request others, if appropriate. Pull requests will be reviewed and approved by at least two code reviewers, at least one of whom must have write permissions on the repository. Reviewers may make comments, ask questions, or request changes on your PR. Respond to these as needed, making commits in your clone and pushing to your fork/branch. Your PR will automatically be updated when commits are pushed to its source branch in your fork, so reviewers will immediately see your updates. When a PR has met the contribution and testing requirements and has been approved by two code reviewers, a code manager will merge the PR.

.. _pr-template:

PR Template
------------

Here is the template that is provided when developers click "Create pull request":

.. code-block:: console

   ## Description of changes
   Provide a description of what this PR does. What bug does it fix, or what feature does it add? Do you expect that this PR will change answers, and if so, under what circumstances? If this PR is for a physics innovation, please provide references to any relevant scientific papers.

   ## Issues addressed (optional)
   If this PR addresses one or more issues, please provide link(s) to the issue(s) here.
   - fixes hafs-community/HAFS/issues/<issue_number>

   ## Dependencies (optional)
   If submodule PRs are required, please link them below. For example:
   - hafs-community/ufs-weather-model/pull/<pr_number>
   - hafs-community/UPP/pull/<pr_number>
   - hafs-community/UFS_UTILS/pull/<pr_number>
   - hafs-community/GSI/pull/<pr_number>

   ## Contributors (optional)
   If others worked on this PR besides the author, please include their user names here (using @Mention if possible).

   ## Tests conducted
   What testing has been conducted on the PR thus far? Describe the nature of any scientific or technical tests, including relevant details about the configuration(s) (e.g., cold versus warm start, number of cycles, forecast length, whether data assimilation was performed, etc). What platform(s) were used for testing?

   ## Application-level regression test status
   Running the HAFS application-level regression tests is currently performed by code reviewers after the developer creates the initial PR. As regression tests are conducted, the testers should use the checklist below to indicate **successful** regression tests. You may add other tests as needed. If a test fails, do not check the box. Instead, describe the failure in the PR comments, noting the platform where the test failed.

   - [ ] Jet
   - [ ] Hera
   - [ ] Orion
   - [ ] WCOSS2

Merging
========

Your PR is ready to merge when:

#. It has been approved by a required number of HAFS reviewers, including at least one reviewer with write permissions.
#. All conversations have been marked as resolved.
#. Regression tests have passed on all supported platforms.

These criteria and their current statuses are detailed in a section at the bottom of your PR's *Conversation* tab. Checks take some time to run, so please be patient.

Need Help?
===========

For assistance directly related to a PR, please use comments in the *Conversation* tab of your PR to ask for help with any difficulties you encounter! 
