#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityProject

context("Tests for getEntityProject")

test_that(paste("test getEntityProject for semantic version:", con$coreApi$semVer), {
  barcode <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, fullMetadata = FALSE, useVerbose = verbose)$entity[[1]]$Barcode
  pro <- getEntityProject(con$coreApi, data$testPocoType, barcode, fullMetadata = FALSE, useVerbose = verbose)

  expect_equivalent(httr::status_code(pro$response), 200)
  expect_match(pro$entity[[1]]$Barcode, data$testPocoProj)
})

test_that(paste("getEntityProject returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  barcode <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, fullMetadata = FALSE, useVerbose = verbose)$entity[[1]]$Barcode
  pro <- getEntityProject(con$coreApi, data$testPocoType, barcode, fullMetadata = TRUE, useVerbose = verbose)

  expect_true(!is.null(pro$entity[[1]]$`Id@odata.type`))
})
