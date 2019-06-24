#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for updateEntityLocation
context("Tests for updateEntityLocation")

test_that(paste("test updateEntityLocation for semantic version:", con$coreApi$semVer), {
  barcode <- getEntityByName(con$coreApi, data$testPocoUpdateType, data$testPocoUpdateName, fullMetadata = FALSE, useVerbose = verbose)$entity[[1]]$Barcode

  updateLoc <- updateEntityLocation(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoUpdateLoc, useVerbose = verbose)
  expect_equivalent(httr::status_code(updateLoc$response), 200)

  loc <- getEntityLocation(con$coreApi, data$testPocoUpdateType, barcode, fullMetadata = FALSE, useVerbose = verbose)
  expect_match(data$testPocoUpdateLoc, loc$entity[[1]]$Barcode)

  # update back to original value
  updateEntityLocation(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoLoc, useVerbose = verbose)
})
