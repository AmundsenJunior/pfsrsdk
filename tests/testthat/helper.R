
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description  This file is run before any tests are executed. 
#' It sets up the environment to run against specific PFS versions. 


verbose <- TRUE
# setup to test against multiple environments
# name files Auth-[pfsversion].json example Auth-5.3.8 for this to pick them up.


environments <- list.files("test_environment", "^(Auth-)[0-9]+\\.[0-9]+\\.[0-9]+\\.json$", full.names = TRUE)

## Set environments to be tested.
if (length(environments) > 1) {

  # Offer the user the option to select the environments that they would like to test.
  cat("Choose the environments that you want to test from the list below. separate each number with a space. hit return to test all\n")
  print.simple.list(environments)
  selection <- readline()
  if (nchar(selection) > 0) environments <- environments[lapply(strsplit(selection, " "), as.numeric)[[1]]]
}


POCO60NAME <- "TE1"
TESTPOCO <- "TESTENTITY"
POCOASSOC <- "TESTASSOC"
POCOASSOC1NAME <- "TA1"
POCOASSOC2NAME <- "TA2"
ASSOCIATIONCONTEXT <- "TEST"
ASSOCIATIONCONTEXTLISTNAME <- "TEST"


Connect <- function(envString) {
  api <- CoreAPIV2::coreAPI(envString)
  con <- CoreAPIV2::authBasic(api, useVerbose = verbose)
}

