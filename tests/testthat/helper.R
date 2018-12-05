
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description  This file is run before any tests are executed. 
#' It sets up the environment to run against specific PFS versions. 

##TODO Add this editable section to a list with an association to 
##an environment file so that this becomes flexible in the future...


### Edit this section to match your tenants you will be testing

verbose <- TRUE

# name of a poco in the system
POCO60NAME <- "TE1"
# name of the poco's entitytype
TESTPOCO <- "TESTENTITY"
# The File attribute on the test poco
TESTPOCOFILEATTRNAME <- "TST_FILE"
TESTPOCOSTRINGATTRNAME <- "TST_STRING"
TESTPOCOINTEGERATTRNAME <-"TST_INTEGER"
TESTPOCOBOOLATTRNAME <- "TST_BOOL"
###TODO Add a decimal type test. We found that this 
###was once broken so it is worth specifically testing

#The name of the association on the poco
POCOASSOC <- "TESTASSOC"
#Two entitys on the pocoassoc type
POCOASSOC1NAME <- "TA1"
POCOASSOC2NAME <- "TA2"
#context set on the assocication 
ASSOCIATIONCONTEXT <- "TEST"
ASSOCIATIONCONTEXTLISTNAME <- "TEST"

###Stop editing this section now... or else...

#a singe persistant barcode and entity that will always exist in Every PFS instance
PERSISTENTBARCODE <- "ALC1"
PERSISTENTBARCODEENTITY <- "ACCESS_LEVEL"
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

Connect <- function(envString) {
  api <- CoreAPIV2::coreAPI(envString)
  con <- CoreAPIV2::authBasic(api, useVerbose = verbose)
}

