#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description  This file is run before any tests are executed.
#' It sets up the environment to run against specific PFS semantic versions.

verbose <- FALSE

# setup to test against multiple environments
# name files Auth-[semver].json example Auth-2.7.1.json for this to pick them up.
versionsRegex <- "[0-9]+\\.[0-9]+\\.[0-9]+"
environments <- list.files("test_environment", paste0("^(Auth-)", versionsRegex, "\\.json$"), full.names = TRUE)
envVersions <- lapply(environments, function(x) {stringr::str_extract(x, versionsRegex)})

# create env list to contain Auth and Data structures used in test execution
env <- list()

# select environment to test by environment variable when in non-interactive mode
selection <- ifelse(!stringi::stri_isempty(Sys.getenv("TEST_ENV")), Sys.getenv("TEST_ENV"), envVersions[[1]])

# Set environment to be tested.
if (length(envVersions) > 1 & interactive()) {
  # Offer the user the option to select the environments that they would like to test.
  cat("Choose the environment (by number) that you want to test from the list below:\n")
  print.simple.list(envVersions)
  
  option <- readline()
  selection <- ifelse((nchar(option) > 0), envVersions[[as.numeric(option)]], selection)
}

env$auth <- paste0("test_environment/Auth-", selection, ".json")
env$data <- paste0("test_environment/Data-", selection, ".json")

