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
- [ ] WCOSS Dell P3
- [ ] WCOSS Cray
