# coreapiv2
*R package to interact with the Thermo Fisher Scientific Platform for Science OData API* 

## Installation
Install the package from Bitbucket with `devtools`:
```r
devtools::install_bitbucket("corelims/coreapiv2", auth_user="BITBUCKET_USER", password="BITBUCKET_PASSWORD")
```

## Usage
See package documentation for usage. 

## Contributing

### Code Style

#### `lintr`
Use the [`lintr` package](https://github.com/jimhester/lintr) to receive syntax and static code analysis feedback on R code.

Install the package and use on one file:
```r
install.packages("lintr")
lintr::lint("R/authBasic.R")
```
This will bring up a list of linting messages in the RStudio "Markers" pane.

You can also lint the whole package at once, and/or filter for higher priority `warning` messages:
```r
lintr::lint_package("R/")
warnings()
```

#### `styler`
Use the [`styler` package](https://github.com/r-lib/styler) to auto-apply a standard code style to R and Rmd code.

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
