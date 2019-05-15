#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityProject
context("Tests for getEntityProject")

test_that(paste("test getEntityProject for:", env$auth), {
  barcode <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, useVerbose = verbose)$entity[[1]]$Barcode
  pro <- getEntityProject(con$coreApi, data$testPocoType, barcode, useVerbose = verbose)

  expect_equivalent(httr::status_code(pro$response), 200)
  expect_match(pro$entity[[1]]$Barcode, data$testPocoProj)
})
