# pfsrsdk 1.0.0

PFSRSDK 1.0.0 is a major release that completely refactors the `CoreAPIV2`
project into the new naming convention, `pfsrsdk`, while updating the SDK
functions for compatibility with Platform for Science (PFS) versions 5.3.8,
5.3.9, 6.0.1, and 6.0.2. Unit tests have been added to cover nearly all SDK
functions and executed successfully against the listed PFS versions.

## Breaking changes

* Package name `coreapiv2` changed to `pfsrsdk`.
* Package functions called via `CoreAPIV2::functionName()` are now called by
  `pfsrsdk::functionName()`.
* Updated `coreAPI.R` function, where the authentication parameters present in a
  user's `Auth.json` configuration file need to be updated:
  * `alias` (previously `TenantShortName`)
  * `tenant` (previously `account`)
  * `host` (previously `coreUrl`)
  * `username` (previously `user`)
  * `password` (previously `pwd`)
  * `semver`, a new parameter for PFS semantic version that throws a warning if
    not configured

## New functions and major changes

* Added `getSemVer` function to test the PFS semantic version being used and
  adapt commands when necessary.
* Created `getExperimentContainerContents` to get any container contents of
  EXPERIMENT_CONTAINER base type.
* Created `getExperimentContainerCellIds` to get cell Ids of experiment
  containers.
* Created `getExperimentWellContents` to retrieve contents of a specific well in
  any experiment container.
* Added `case` utility function to provide vectorized IF operations.

## Deprecated functions

* Deprecated `getAttachedFile` for `getAttachedAttributeFile`.
* Deprecated `getExperimentSamplesIntermediateData` for
  `getExperimentSampleIntermediateData`.
* Deprecated `ODATAcleanName` for `odataCleanName`.
* Deprecated `setExperimentSamplesAssayFileData` for
  `setExperimentSampleAssayFileData`.
* Deprecated `updateCellContents` for existing `setCellContents` function, as
  the latter provides the same functionality via PFS OData that the former
  executes in the PFS JSON API.

## Minor improvements and fixes

* Added `packrat` to package structure for dependency management.
* Added Gradle to execute package build, test, documentation process.
* Used `styler` to enforce clean code syntax.
* Used `lintr` as a tool for static code analysis.
* Added `serviceRoot` to coreAPI to enable SDK use against Platform Admin.
* Enabled automated testing via Jenkins against multiple versions of PFS for
  compatibility validation.
* Updated `getContainerContents` to handle generic CONTAINER contents requests.
* Created `loadXmlConfigFile` utility function to help prepare PFS test
  environments by loading application configuration files (internal calls only).
* Updated `setup.r` and `teardown.r` for tests. `setup` will now load the
  `CoreApp.SDK` XML in a test environement, and `teardown` hides information
  that is not needed during log out. 
* Updated `getWellContents` to handle generic CONTAINER well content requests.
* Updated `helper.r` to execute against specific-version test environments
  without interactive prompting.
