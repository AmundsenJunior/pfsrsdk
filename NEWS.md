<!---
 (Keep the following line at the top of this document)
--->
###### This project follows [semantic versioning](https://semver.org/)
<!---
[//]: # (Use the commented section below for news on each release)
[//]: # (# pfsrsdk x.x.x.9000)
[//]: # ( )
[//]: # (## Breaking changes)
[//]: # ( )
[//]: # (## New functions and minor changes)
[//]: # ( )
[//]: # (## Fixes)
--->

# pfsrsdk 1.0.1.9000

## Breaking changes

* Fixed defect in `coreAPI()` that was overwriting `coreAPI` configuration
  values with environment variables. Renamed `username` and `password`
  configuration values to `api_username` and `api_password` to ensure
  compatibility with the [prohibited environment variables](https://docs.rstudio.com/connect/admin/appendix-configuration.html#Applications.Settings)
  of RStudio-Connect.
* Added parameter fullMetadata to getEntityLocation, getEntityProject, getExperimentSampleAssayData, getExperimentSamplesRawData. This changes the order of verbose parameter.
* Changed default fullMetadata value.
* apiGET(), apiPOST(), apiPUT() and ()authBasic will not stop the execution of 
  the R code during an error. Warnings will show information about the error and
  the functions will return objects with the error details.
* Changed default fullMetadata value.

## New functions and minor changes

* Updated license information.
* Changes and enhancements to automated internal testing.
* Updated publish functions to use the odata options available in PFS v6. 
* Updated `createExperimentContainer()` to warn the user when accessing a published experiment.
* Documentation updates.
* Updated `updateEntityLocation()` to use the *InventoryMove* action available
  in the PFS OData API.
* Removed `# Completed regression for 5.3.8 and 6.0.1` message from tests.  
* Added author and updated author e-mails.
* Replaced switch() with case().
* Updated `packrat` dependencies with newer minor versions.
* Enabled automated testing of `pfsrsdk` against PFS 5.3.10.
* General code cleanup

# pfsrsdk 1.0.1

## Fixes

* Fixed issue where 401 error was thrown when using `apiGET()` and verbose is
  set to true.
* Fixed issue with checking to see if a particular PFS version has been tested
  with this package.
* Fixed issue where `odataCleanName()` added an underscore to the tenant alias
  name. (new parameter added for `odataCleanName()`)
* Added sinking to `setup.R` to minimize test output.
* Added tests to `test-httpFunctions.R` (GET, POST, PUT) for more than 100
  results.
* Added `xml2` as an import since we use the `httr::content()` with an XML MIME
  type. `httr` only lists `xml2` as "Suggest" in its `DESCRIPTION`.

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
* Deprecated `getAttachedFile` for `getAttachedAttributeFile`.
* Deprecated `getExperimentSamplesIntermediateData` for
  `getExperimentSampleIntermediateData`.
* Deprecated `ODATAcleanName` for `odataCleanName`.
* Deprecated `setExperimentSamplesAssayFileData` for
  `setExperimentSampleAssayFileData`.
* Deprecated `updateCellContents` for existing `setCellContents` function, as
  the latter provides the same functionality via PFS OData that the former
  executes in the PFS JSON API.

## New functions and minor changes

* Added `getSemVer` function to test the PFS semantic version being used and
  adapt commands when necessary.
* Created `getExperimentContainerContents` to get any container contents of
  EXPERIMENT_CONTAINER base type.
* Created `getExperimentContainerCellIds` to get cell Ids of experiment
  containers.
* Created `getExperimentWellContents` to retrieve contents of a specific well in
  any experiment container.
* Added `case` utility function to provide vectorized IF operations.

## Fixes

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
