#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for updateEntityProject
context("Tests for updateEntityProject")

test_that(paste("test updateEntityProject for: ", env$auth), {
  barcode <- getEntityByName(con$coreApi, data$testPocoUpdateType, data$testPocoUpdateName, fullMetadata = FALSE, useVerbose = verbose)$entity[[1]]$Barcode

  updateProj <- updateEntityProject(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoUpdateProj, useVerbose = verbose)
  expect_equivalent(httr::status_code(updateProj$response), 200)

  proj <- getEntityProject(con$coreApi, data$testPocoUpdateType, barcode, fullMetadata = FALSE, useVerbose = verbose)
  expect_match(data$testPocoUpdateProj, proj$entity[[1]]$Barcode)

  updateEntityProject(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoProj, useVerbose = verbose)
})
