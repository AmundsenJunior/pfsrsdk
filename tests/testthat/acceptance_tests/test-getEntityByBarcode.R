#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getEntityBarcode.

context("Tests for getEntityByBarcode")

test_that(paste("getEntityByBarcode returns successful without fullMetadata on semantic version:", con$coreApi$semVer), {
  b <- getEntityByBarcode(con$coreApi, data$persistentEntityType, data$persistentEntityBarcode,
    fullMetadata = FALSE,
    useVerbose = verbose
  )$entity

  expect_match(b$Barcode, data$persistentEntityBarcode, all = verbose)
})

test_that(paste("getEntityByBarcode returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  b <- getEntityByBarcode(con$coreApi, data$persistentEntityType, data$persistentEntityBarcode,
    fullMetadata = TRUE,
    useVerbose = verbose
  )
  expect_true(!is.null(b$entity$`Id@odata.type`))
})

test_that(paste("getEntityByBarcode receives warning when requesting non-existent entity on semantic version:", con$coreApi$semVer), {
  expect_warning(
    {
      b <- getEntityByBarcode(con$coreApi,
        data$nonExistingEntityType,
        data$nonExistingEntityBarcode,
        fullMetadata = FALSE,
        useVerbose = verbose,
        fullReturn = FALSE
      )
      b$error$message
    },
    data$nonExistingEntityErrorMessage
  )
})
