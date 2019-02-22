#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for updateEntityLocation
context("Tests for updateEntityLocation")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test updateEntityLocation for:", env$auth), {
  barcode <- getEntityByName(con$coreApi, data$testPocoUpdateType, data$testPocoUpdateName, useVerbose = verbose)$entity[[1]]$Barcode

  updateLoc <- updateEntityLocation(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoUpdateLoc, useVerbose = verbose)
  expect_equivalent(httr::status_code(updateLoc$response), 200)

  loc <- getEntityLocation(con$coreApi, data$testPocoUpdateType, barcode, useVerbose = verbose)
  expect_match(data$testPocoUpdateLoc, loc$entity[[1]]$Barcode)

  # update back to original value
  updateEntityLocation(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoLoc, useVerbose = verbose)
})
