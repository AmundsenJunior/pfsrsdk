# pfsrsdk 

*R package to interact with the Thermo Fisher Scientific Platform for Science OData API* 

## Installation

Install the package from Bitbucket with `devtools`:
```r
install.packages("devtools")
devtools::install_bitbucket("corelims/pfsrsdk", auth_user="BITBUCKET_USER", password="BITBUCKET_PASSWORD")
```

## Usage

See package documentation for usage. 

## Contributing

Reference any of the below resources for developing in this package.

### Managing Package Dependencies

#### Development Dependencies

When working on the package, development tasks - such as building, documenting,
or testing - require other packages to execute. These packages are available in
the `packrat` environment (see below). Any new development dependencies should
be installed in the packrat environment and a new snapshot taken. Please update
the below list with any additional development dependencies:

* `devtools`
* `knitr`
* `lintr`
* `pkgdown`
* `rmarkdown`
* `shiny`
* `styler`
* `testthat`
* `testthis`

#### Packrat

The [`packrat` package](https://github.com/rstudio/packrat) manages all R
package dependencies that this project uses. `packrat` provides a local
repository of packages, based upon a sweep of all `R/` code for any `library()`
or `required()` calls. It stores versioned TARs of those packages, retrieved
from CRAN or other defined repositories in the `packrat/src/` directory, along
with a `packrat.lock` file that specifies the packages and their versions.

##### `packrat::init()`

These files are generated on a new project by calling:
```r
install.packages("packrat")
packrat::init()
```
and are part of source control. Git ignores all installed instances of packages
in the `packrat/lib*/` directories.

##### `packrat::restore()`

In a new clone of this project, initialize your environment into the `packrat` 
repository by calling:
```r
packrat::restore()
```
This will put all calls to dependencies in your source files towards the
`packrat` context.

##### `packrat::snapshot()`

If you are adding any new package dependencies to the project, you can install
them with regular install commands (e.g. `install.packages()` or
`devtools::install_github()`). To record this update to the package
dependencies, call
```r
packrat::snapshot()
```
in order to have `packrat` update the `packrat.lock` file.

### Testing the Package

TODO: Testing process description

#### Running Individual Tests

As the testing suite uses `setup.R` and `teardown.R` for pre- and post-test
actions, invoking such helpers is only available from the `testthat::test_dir()`
or `testthat::test_package()` functions. To test an individual file, execute
using a filter for that test file name:
```r
testthat::test_dir("tests/testthat", filter = "updateEntityLocation")
```

### Building the Package

The [`devtools` package](https://github.com/r-lib/devtools) provides crucial
package creation functionality, as it is used to build and install this package
from source.

#### `devtools::build()`

More useful for publishing, a built TAR of the package is generated with
`devtools::build()`. By default, this is executed from the project root
directory, and creates the file in the parent directory (i.e., the directory
that contains the project). In practice, it is useful to generate the built
package in a specified directory, such as:
```r
devtools::build(path = "someDir")
```

#### `devtools::install()`

When working with the package in a local environment, it is useful to make the
package available as-is to other development projects. This is achieved by
installing the package from source into your R environment.
```r
devtools::install()
```

### Documentation

This package currently creates two forms of documentation:

#### `man/`

We use a call to `devtools::document()` (which wraps the 
[`roxygen2`package](https://github.com/klutometis/roxygen)) to generate the 
`man/*.Rd` files. The `*.Rd` files are generated from the formatted comments,
part of all `*.R` source code. These files are under source-control, so it
should be a part of your pre-commit development workflow to run
`devtools::document()` and commit any updates to the `man/` directory, with
their corresponding `R/` changes.

#### `pkgdown`

From a built and installed package, we also can generate a static HTML
documentation site, using the
[`pkgdown` package](https://github.com/r-lib/pkgdown). This package builds the
HTML, CSS, and JS files from the various package documentation sources: `man/`,
`vignettes/`, `DESCRIPTION`, `README.md`, and `NEWS.md`. The site is created by
calling:
```r
install.packages("pkgdown")
pkgdown::build_site()
```
This function generates a `docs/` directory, which can then be published and 
hosted for end-users.

### Code Style

We maintain a consistent style of R code with the following two packages:

#### `lintr`

Use the [`lintr` package](https://github.com/jimhester/lintr) to receive syntax 
and static code analysis feedback on R code.

Install the package and use on one file:
```r
install.packages("lintr")
lintr::lint("R/authBasic.R")
```
This will bring up a list of linting messages in the RStudio "Markers" pane.

You can also lint the whole package at once, and/or filter for higher priority
`warning` messages:
```r
lintr::lint_package("R/")
warnings()
```

#### `styler`

Use the [`styler` package](https://github.com/r-lib/styler) to auto-apply a 
standard code style to R and Rmd code.

Install the package and use on one file:
```r
install.packages("styler")
styler::style_file("R/authBasic.R")
```
The output will show if your code style has been modified by the tool.

You can also apply style to directories or the whole package:
```r
styler::style_dir("R/")
styler::style_pkg()
```

## Continuous Integration

Outside of local development, the package must be exercised as part of our SDLC
process. As part of the integration with other software products, we extend upon
those common tools used elsewhere to enable R package creation and validation
as easily as possible.

### Jenkins

#### System Dependencies

The CI process runs on Jenkins _agent_ Docker containers. These containers are
Debian-based, and have a specific version of R installed (see the
[`jenkins-agent`](https://bitbucket.org/corelims/jenkins-agent) repo for more
details).

Included in that Docker image are a number of system dependencies specifically
needed to build and publish this package.

R Package|Debian Dependency
---|---
`XML`|`libxml2-dev`
`curl`|`libcurl4-gnutls-dev`
`git2r`|`libgit2-dev`<br>`libssl-dev`
`pkgdown`|`pandoc`

### Gradle

#### `gradle-plugin-r`

Gradle is used to wrap a number of common R-package and software-development
tasks in Groovy-defined tasks and plugins. For this project, we use and extend
upon the [`gradle-plugin-r` project](https://github.com/umayrh/gradle-plugin-r)
to execute some of the above-described R commands in a Gradle context. That
context affords us other functionality, such as creating publishing artifacts.

Between the plugin and the `gradle/r-package.gradle` file extensions on that
plugin, we are able to execute the following R command with corresponding
Gradle tasks:

R Command|Gradle Task
---|---
`devtools::document()`|`./gradlew document`
`packrat::clean()`|`./gradlew packratClean`
`packrat::restore()`|`./gradlew packratRestore`
`devtools::build(path = "build/package")`|`./gradlew buildPackage`
`devtools::install(pkg = "build/package")`|`.gradlew installPackage`
`devtools::test(reporter = JunitReporter)`|`./gradlew testPackage`
`pkgdown::build_site(override = list(destination = "build/docs"))`|`./gradlew pkgdownSite`

#### Documentation ZIP publishing

Gradle has also been extended to parse the `DESCRIPTION` file for the package
name ("Package") and the package version ("Version"). These values are then used
to generate a ZIP file of the `pkgdown`-generated documentation static HTML
site. This ZIP is generated by calling:
```
./gradlew docsDistZip
```

The documentation ZIP is then published to the legacy Core Informatics
[Artifactory server](https://software.corelims.com/artifactory), via Gradle:
```
./gradlew artifactoryPublish
```

#### Docker

Using the Gradle [`docker-plugin`](https://bitbucket.org/corelims/docker-plugin)
we are also able to generate Docker images of the installed package with a basic
Shiny app (from [inst/shiny](inst/shiny)) and of the `pkgdown` documentation
site. These two images are built on top of the 
[`rocker/tidyverse`](https://hub.docker.com/r/rocker/tidyverse/) and 
[`nginx`](https://hub.docker.com/_/nginx/) public images, respectively. The
images can be generated on a system with Docker installed by calling:
```
./gradlew buildROdataSdkShinyImage
./gradlew buildROdataSdkDocsImage
```

Both of these images can be run as webapps:
```
docker run -d -p 8080:80 quay.coredev.cloud/core-informatics/r-odata-sdk-docs:latest
docker run -d -p 3838:3838 quay.coredev.cloud/core-informatics/r-odata-sdk-shiny:latest
```
