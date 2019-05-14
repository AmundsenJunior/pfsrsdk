#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityBarcode.

context("Tests for getEntityByBarcode")

test_that(paste("test getEntityByBarcode() on: ", env$auth), {
  b <- getEntityByBarcode(con$coreApi, data$persistentEntityType, data$persistentEntityBarcode,
    useVerbose = verbose
  )$entity

  expect_match(b$Barcode, data$persistentEntityBarcode, all = verbose)
})
