#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description Tests for getAttachedAttributeFile
#'
context("Tests for getAttachedAttributeFile()")

test_that(paste("test getAttachedAttributeFile() OData call on:", env$auth), {
  barcode <- CoreAPIV2::getEntityByName(con$coreApi, data$testPocoType, data$testPocoName, FALSE, FALSE)$entity[[1]]$Barcode

  t <- getAttachedAttributeFile(con$coreApi, data$testPocoType, barcode, data$testPocoFileAttrName, useVerbose = TRUE)
  expect_true(grepl("x", rawToChar(t$entity)))
})
