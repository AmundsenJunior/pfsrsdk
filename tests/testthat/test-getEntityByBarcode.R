#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getEntityBarcode.

context("Tests for getEntityByBarcode")

test_that(paste("test getEntityByBarcode() on: ", env$auth), {
  b <- getEntityByBarcode(con$coreApi, data$persistentEntityType, data$persistentEntityBarcode,
    fullMetadata = FALSE,
    useVerbose = verbose
  )$entity

  expect_match(b$Barcode, data$persistentEntityBarcode, all = verbose)

  b2 <- getEntityByBarcode(con$coreApi, data$persistentEntityType, data$persistentEntityBarcode,
    fullMetadata = TRUE,
    useVerbose = verbose
  )
  expect_true(!is.null(b2$entity$`Id@odata.type`))
})
