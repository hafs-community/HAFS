.. _HAFSCodeContributionGuidelines:

***********************************************
UFS Repository Code Management Guidance (Draft)
***********************************************


Repository
==========

- Authoritative repositories are read-only for users.
- Branches are protected, i.e., changes can only be made through merging pull requests, not by pushing (including admins).
- All the UFS app develop branches are pointing to subcomponents’ official branches for development at different revisions. The subcomponent branch should not be a temporary branch in a user’s or developer group’s personal fork.
- No personal installed software including libraries or code are allowed in the UFS application code. No personal directory will be used in the UFS application.
- A simple and clear commit message is required when merging a pull request. (see here for standard log format)

Forking
-------

- For new development users will create a fork from the repositories, pull requests will be made from the branch in developers’ fork.
- When a fork is created, all the branches in the parent repository are copied automatically. It is suggested that users keep the develop branch and the branches that they are interested in and ignore the rest branches entirely or delete them
- It is suggested that users create their own feature branch from the development branch (e.g. develop or master branch) in the authoritative (or official) repository. Basically, Users can add remote upstream to the authoritative repository, check out upstream develop/master branch and then create a new feature branch from upstream develop/master branch.

Branching
---------

- It is suggested that users create their own feature branch in their fork.
- Users’ develop/feature branches in user’s fork should be sync-ed with authoritative (or official) repositories periodically.
- When development work is done, users will sync their feature branch with the latest develop/master branch in the authoritative repository, run regression tests and make a pull request to the authoritative (or official) repository.
- It is suggested to delete the feature branch in users’ personal fork when the code changes are merged into an authoritative repository.
- For new development work, users will sync their development with an authoritative repository and start from step 1.

Tagging/Versioning
------------------

- In an authoritative repository, only code managers can create production or public release branches.
- For the production branch, a suggested branch name is production/app.vxx, xx is the operational implementation version number (e.g. GFS.v16). Annotated tags will be created with names app.vxx.yy.zz (yy is feature upgrade, zz is bugfix upgrade) (e.g. GFS.v15.2.1).
- For the public release branch, suggested branch names: release/public-vxx.yy. Annotated Tags will be created with names: app.vxx.yy.zz.
- For develop branch, benchmark tags can be created as benchmark/app.vxx.yy.zz
- Code managers are encouraged to delete the old production and public release branches. Annotated tags will stay when the production/public release branches are deleted.
- It is suggested not to create a tag for every commit on the develop branch. Tags will only be created for special features/milestones.

Pull Request
------------

- An issue needs to be created associated with the pull request. The issue should cover all the features in detail.
- Before making a pull request, users’ feature branch must be merged with the corresponding branch in the authoritative repository. All the conflicts must be resolved. Regression tests need to be passed on at least one supported platform.
- Users submit a pull request (PR) for code commit.
- Pull request creators will add reviewers, if needed, ask code managers to add reviewers.
- At least one reviewer with write permission needs to give approval before the code can be committed.
- If needed, developers can ask code manager(s) to run the regression test on the required platforms that users don’t have access to.
- When the PR is reviewed and no further changes are required, the PR can be put in the commit queue.
- For PRs that involve several repositories:
  i) All PRs need to be cross-referenced in the description of each repository PR; and a note needs to be placed in each PR.
  ii) During the code commit, all PRs need to be ready for commit. This requires that all the related commit branches are merged with the top of repositories and no other commit will be committed into each repository until the current commit is finished (repository freeze for the commit). If any of the repositories is updated, the code manager of that repository needs to notify all the related repository code managers to redo RT.
  iii) The top application RTs need to be finished and RT information needs to be posted in all the related repositories before the PR can be merged in each individual repository.

How to Write Pull Request
--------------------------

- Pull request size: should be small
- Feature breaking: if possible, one feature for each PR..
- Make a self-explanatory title describing what the PR does.
- Description: Details with what was changed, why it was changed, and how it was changed

Code Commit
-----------

- The code changes will be reviewed and approved by at least one code reviewer.
- The CI tests need to pass.
- Regression tests need to pass on all the supported platforms. All the regression test log files need to be updated. The exception is when a certain platform is under maintenance. For such a time, the regression test will be skipped on that platform. When the platform becomes available, the following PR will be tested and verified. If issues come up with the platform, then PRs will be suspended until the issue is resolved.
- At least one code manager needs to review and approve the code changes before the code is merged to the develop branch.
- Code managers will discuss the current PRs in UFS and sub-component repositories and a commit queue will be determined and put in the ufs wiki page.

Commit Procedure
----------------

Assuming that proper testing has been completed, code managers must also check the following list for any PR created in an application or subcomponent repository:

- The developer followed PR template instructions, and provided required information.
- The code is merged to the top of develop/master branch.
- Add labels such as "Bug fix," "bug," "enhancement," "good first issue," "invalid," "Ready to Merge," or "question."

Requirements for Adding a PR to the Commit Queue:
---------------------------------------------------

- When a PR requester (or code manager) sets the "ready for review" label, reviewers are assigned.
- Reviewers review and approve code changes.

Steps to Merge the PR Listed at the Top of the Commit Queue:
------------------------------------------------------------

- Developers merge file changes to match the development branch and coordinate with code managers to trigger CI Git labels (e.g., run-ci and/or jenkins-ci).
- Monitor the results of CI runs and ask reviewers' final comments to start approval procedures.
- Start RTs on Tier-1 platforms.
- If all RTs pass, the PR can be merged with final approvals from two code managers.
- If RT cases fail, some simple fixes can be added. Reviewers must approve the fixes. The CI and RT run steps must be repeated.
- If more time is required to fix issues found at commit time, the PR will be removed from the Commit Queue. It will be added to the top of the Commit Queue when the issue is fixed.

CM Daily PR Merging Steps:
---------------------------

- Assign reviewers, check review status, and check test status.
- If input data needs to be added, copy the data to the RT input data directory.
- Check whether the PR and subcomponent PRs are approved; reassign reviewers if needed.
- Make sure to run CI tests after the code review is done.
- If a new baseline is required, decide on a baseline directory name and communicate with PR owners and CM groups when new baselines are created on certain HPC platforms.
- Validate RT results. Coordinate with PR owners and reviewers to confirm baseline creation and RT runs on supported platforms.
- Merge the PR.

General Guidance on Code Changes:
---------------------------------

- All new features should be implemented as options so that there is no impact to current UFS applications. It is suggested to add a regression test to demonstrate how to use the new feature.
- Bug fixes will not be implemented as options. They may change results. Developers need to make sure that all the UFS applications will work with bug fixes.
