
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description  This file is run before any tests are executed.
#' It sets up the environment to run against specific PFS versions.

## TODO Add this editable section to a list with an association to
## an environment file so that this becomes flexible in the future...


### Edit this section to match your tenants you will be testing

verbose <- FALSE

# Entity name of a POCO in the system
POCO60NAME <- "Sarges Best Amber Lager"
# location barcode of the POCO entity
POCO60LOC <- "LC3"
# project barcode of the POCO entity
POCO60PROJ <- "PJ1"

# name of the poco's entitytype
TESTPOCO <- "BEER"

# The name of the association on the poco
POCOASSOC <- "BEER"
# Two entities on the pocoassoc type
POCOASSOC1NAME <- "Sarges Best Dark Lager"
POCOASSOC2NAME <- "Sarges Best Amber Lager"
# context set on the assocication
ASSOCIATIONCONTEXT <- "BEER_ORDERED"
ASSOCIATIONCONTEXTLISTNAME <- "BEER_ORDERED"

# for update tests
POCO60LOC2 <- "LC1"
POCO60PROJ2 <- "PJ2"
TESTPOCOUPDATEATTRLIST <- list(CI_BRAND = "Duff", CI_TARGET_SED_G_L = 0.52)

# for create entity tests
TESTPOCOCREATEBOOL <- "TRIGGER_EXPT_CREATE_SAMPLE_LOT"
TESTPOCOCREATEBOOLATTRLIST <- list(CI_TRIGGER_ACTION_TYPE = "Test action type", CI_CREATE_LOT = FALSE)
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

### Stop editing this section now... or else...

# a singe persistant barcode and entity that will always exist in Every PFS instance
PERSISTENTBARCODE <- "ALC1"
PERSISTENTBARCODEENTITY <- "ACCESS_LEVEL"

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
