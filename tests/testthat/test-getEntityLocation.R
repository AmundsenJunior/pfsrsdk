#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityLocation
context("Tests for getEntityLocation")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getEntityLocation for: ", env$auth), {
  barcode <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, useVerbose = verbose)$entity[[1]]$Barcode
  loc <- getEntityLocation(con$coreApi, data$testPocoType, barcode, useVerbose = verbose)

  expect_equivalent(httr::status_code(loc$response), 200)
  expect_match(loc$entity[[1]]$Barcode, data$testPocoLoc)
})
