#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description  This file is run before any tests are executed.
#' It sets up the environment to run against specific PFS versions.

verbose <- FALSE

# setup to test against multiple environments
# name files Auth-[pfsversion].json example Auth-5.3.8.json for this to pick them up.
versions <- "[0-9]+\\.[0-9]+\\.[0-9]+"
environments <- list.files("test_environment", paste0("^(Auth-)", versions, "\\.json$"), full.names = TRUE)

env <- list()
selection <- "1"

# Set environment to be tested.
#if (length(environments) > 1) {
if (length(environments) > 1 & interactive()) {
  # Offer the user the option to select the environments that they would like to test.
  cat("Choose the environment (by number) that you want to test from the list below:\n")
  print.simple.list(environments)
  
  #TODO provide noninteractive means to select environment via command-line argument (for CI execution)
  option <- readline()
  selection <- ifelse((nchar(option) > 0), option, selection)
}

env$auth <- environments[lapply(selection, as.numeric)[[1]]]
env$data <- paste0("test_environment/Data-", stringr::str_extract(env$auth, versions), ".json")
