
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description  This file is run before any tests are executed.
#' It sets up the environment to run against specific PFS versions.

## TODO Add this editable section to a list with an association to
## an environment file so that this becomes flexible in the future...


### Edit this section to match your tenants you will be testing

verbose <- FALSE

# POCO entity
TESTPOCOTYPE <- "BEER_ORDER"
TESTPOCONAME <- "BO1"
TESTPOCOLOC <- "LC3"
TESTPOCOPROJ <- "PJ1"

# for file tests
TESTPOCOFILEATTRNAME <- "CI_INVOICE_FILE"

# for create entity tests
TESTPOCOCREATEBOOL <- "BOOLEAN"
TESTPOCOCREATEBOOLATTRLIST <- list(CI_READONLY = TRUE)
TESTPOCOCREATEBOOLASSOCLIST <- NULL

TESTPOCOCREATEDEC <- "BEER_ORDER"
TESTPOCOCREATEDECATTRLIST <- list(CI_NUM_UNITS = 10.0, CI_UNIT_PRICE = 5.99, CI_INVOICE_FILE = NULL)
TESTPOCOCREATEDECASSOCLIST <- list(BEER_ORDERED = "BEER('BEER1')")

TESTPOCOCREATEINT <- "LOCATION"
TESTPOCOCREATEINTATTRLIST <- list(CI_CAPACITY = 100, CI_COUNT = 2)
TESTPOCOCREATEINTASSOCLIST <- NULL

TESTPOCOCREATESTR <- "ACCESS_LEVEL"
TESTPOCOCREATESTRATTRLIST <- list(COMMENTS = "Test comment")
TESTPOCOCREATESTRASSOCLIST <- NULL

# for get entity tests
TESTPOCOGETASSOCTYPE <- "BEER"
TESTPOCOGETASSOCNAME <- "Sarges Best Dark Lager"
TESTPOCOGETASSOCCONTEXT <- "BEER_ORDERED"

# for update entity tests
TESTPOCOUPDATETYPE <- "BEER"
TESTPOCOUPDATENAME <- "Sarges Best Dark Lager"
TESTPOCOUPDATEASSOC <- "HOPS"
TESTPOCOUPDATEASSOCCONTEXT <- "BEER_HOPS"
TESTPOCOUPDATEASSOCNAME <- "Apollo"
TESTPOCOUPDATELOC <- "LC1"
TESTPOCOUPDATEPROJ <- "PJ2"
TESTPOCOUPDATEATTRLIST <- list(CI_TARGET_ABV = 4.8, CI_TARGET_SED_G_L = 0.73)

# for error tests
TESTPOCONONEXISTENTTYPE <- "WATER"

### Stop editing this section now... or else...

# a persistant entity that will always exist in Every PFS instance
PERSISTENTENTITYTYPE <- "ACCESS_LEVEL"
PERSISTENTENTITYBARCODE <- "ALC1"
PERSISTENTENTITYNAME <- "ADMIN ACCESS"

# setup to test against multiple environments
# name files Auth-[pfsversion].json example Auth-5.3.8 for this to pick them up.
environments <- list.files("test_environment", "^(Auth-)[0-9]+\\.[0-9]+\\.[0-9]+\\.json$", full.names = TRUE)

## Set environments to be tested.
if (length(environments) > 1) {

  # Offer the user the option to select the environments that they would like to test.
  cat("Choose the environments that you want to test from the list below. Separate each number with a space. Hit return to test all:\n")
  print.simple.list(environments)
  selection <- readline()
  if (nchar(selection) > 0) environments <- environments[lapply(strsplit(selection, " "), as.numeric)[[1]]]
}

Connect <- function(envString) {
  api <- CoreAPIV2::coreAPI(envString)
  con <- CoreAPIV2::authBasic(api, useVerbose = verbose)
}
