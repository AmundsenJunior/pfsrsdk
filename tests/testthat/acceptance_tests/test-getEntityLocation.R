#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityLocation

context("Tests for getEntityLocation")

test_that(paste("test getEntityLocation for semantic version:", con$coreApi$semVer), {
  barcode <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, fullMetadata = FALSE, useVerbose = verbose)$entity[[1]]$Barcode
  loc <- getEntityLocation(con$coreApi, data$testPocoType, barcode, fullMetadata = FALSE, useVerbose = verbose)

  expect_equivalent(httr::status_code(loc$response), 200)
  expect_match(loc$entity[[1]]$Barcode, data$testPocoLoc)
})

test_that(paste("getEntityLocation returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  barcode <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, fullMetadata = FALSE, useVerbose = verbose)$entity[[1]]$Barcode
  loc <- getEntityLocation(con$coreApi, data$testPocoType, barcode, fullMetadata = TRUE, useVerbose = verbose)

  expect_true(!is.null(loc$entity[[1]]$`Id@odata.type`))
})
