


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

POCO <- "POCO"

Connect <- function(envString) {
  api <- CoreAPIV2::coreAPI(envString)
  con <- CoreAPIV2::authBasic(api, useVerbose = verbose)
}




# })
#
# testthat::teardown({
#
#   POCO <- NULL
#   EnvSelection <- NULL
#   selection <-NULL
#   environments <-NULL
#   verbose <-NULL
#   rm(list=ls())
# })
