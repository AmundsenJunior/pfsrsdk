# coreapiv2 0.9.0.9000

## New Features

* Added `getSemVer` function to test the PFS version being used and adapt commands when necessary as in `createSampleLot.R`.
* Created `getExperimentContainerContents` to get any container contents of EXPERIMENT_CONTAINER base type.
* Created `getExperimentContainerCellIds` to get cell Ids of experiment containers.
* Created `getExperimentWellContents` to retrieve contents of a specific well in any experiment container.
* Added `case` utility function to provide improved switch-case operations in package functions.

## Bug fixes and other changes

* Added `packrat` to package structure for dependency management
* Added Gradle to execute package build, test, documentation process
* Added `styler` to enforce clean code syntax
* Added `lintr` as a tool for static code analysis
* Added `serviceRoot` to coreAPI to enable SDK use against Platform Admin
* Replaced `TenantShortName` with `alias` on *.json files and corresponding R/ and tests/ files. Everyone shall update auth files to match new standard.
* Replaced `account` to `tenant`, `coreUrl` to `host`, `user` to `username` and `pwd` to `password` in the CoreApi class.
* Enabled automated testing via Jenkins against multiple versions of PFS for compatibility validation.
* Updated `getContainerContents` to handle generic CONTAINER contents requests.
* Created `getExperimentContainerContents` to get any container contents of EXPERIMENT_CONTAINER base type.
* Updated `setup.r` and `teardown.r` for tests. Setup will now load the CoreApp.SDK XML in a test environement, and teardown hides information that is not needed during log out. 
* Updated `getWellContents` to handle generic CONTAINER well content requests.
