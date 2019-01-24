#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityBarcode.

context("Tests for getEntityByBarcode")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getEntityByBarcode() on: ", env$auth), {
  b <- CoreAPIV2::getEntityByBarcode(con$coreApi, data$persistentEntityType, data$persistentEntityBarcode,
    useVerbose = verbose
  )$entity
    
  expect_match(b$Barcode, data$persistentEntityBarcode, all = verbose)
})
