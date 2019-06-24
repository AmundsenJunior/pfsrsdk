#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateEntityAttributes} Tests for updateEntityAttributes.

context("Tests for updateEntityAttributes")

test_that(paste("test updateEntityAttributes() on semantic version:", con$coreApi$semVer), {
  barcode <- getEntityByName(con$coreApi, data$testPocoUpdateType, data$testPocoUpdateName, FALSE, useVerbose = FALSE)$entity[[1]]$Barcode

  ue <- updateEntityAttributes(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoUpdateAttrList, useVerbose = verbose)
  expect_equal(ue$entity[[names(data$testPocoUpdateAttrList)[1]]], data$testPocoUpdateAttrList[[names(data$testPocoUpdateAttrList)[1]]], all = verbose)
})
