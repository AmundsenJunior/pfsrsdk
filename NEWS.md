# coreapiv2 1.0.0.9000

## Bug fixes and other changes

* Added `packrat` to package structure for dependency management
* Added Gradle to execute package build, test, documentation process
* Added `styler` to enforce clean code syntax
* Added `lintr` as a tool for static code analysis
* Added `serviceRoot` to coreAPI to enable SDK use against Platform Admin
* Replaced `TenantShortName` with `alias` on *.json files and corresponding R/ and tests/ files. Everyone shall update auth files to match new standard.
* Replaced `account` to `tenant`, `coreUrl` to `host`, `user` to `username` and `pwd` to `password` in the CoreApi class.