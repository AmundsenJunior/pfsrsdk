#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{updateEntityAttributes} Tests for updateEntityAttributes.

context("Tests for updateEntityAttributes")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test updateEntityAttributes() on: ", env$auth), {
  barcode <- CoreAPIV2::getEntityByName(con$coreApi, data$testPocoUpdateType, data$testPocoUpdateName, FALSE, FALSE)$entity[[1]]$Barcode
    
  ue <- CoreAPIV2::updateEntityAttributes(con$coreApi, data$testPocoUpdateType, barcode, data$testPocoUpdateAttrList, useVerbose = verbose)
  expect_equal(ue$entity[[names(data$testPocoUpdateAttrList)[1]]], data$testPocoUpdateAttrList[[names(data$testPocoUpdateAttrList)[1]]], all = verbose)
})
