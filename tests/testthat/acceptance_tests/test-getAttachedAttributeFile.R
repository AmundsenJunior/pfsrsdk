#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for getAttachedAttributeFile

context("Tests for getAttachedAttributeFile()")

test_that(paste("test getAttachedAttributeFile() OData call on:", env$auth), {
  barcode <- getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, FALSE, useVerbose = FALSE)$entity[[1]]$Barcode

  t <- getAttachedAttributeFile(con$coreApi, data$testPocoType, barcode, data$testPocoFileAttrName, useVerbose = TRUE)

  expect_equal(t$response$status_code, 200)
  expect_gt(length(t$entity), 0)
})
