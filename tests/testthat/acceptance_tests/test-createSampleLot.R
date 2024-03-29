#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for sample lot creation.

context("Tests for createSampleLot")

test_that(paste("test createSampleLot() on semantic version:", con$coreApi$semVer), {
  result <- createSampleLot(con$coreApi,
    data$sampleType,
    data$sampleBarcode,
    body = NULL,
    fullMetadata = FALSE,
    useVerbose = verbose
  )

  expect_gt(httr::content(result$response)$CI_LOT_NUM, 0)
})

test_that(paste("createSampleLot returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  result <- createSampleLot(con$coreApi,
    data$sampleType,
    data$sampleBarcode,
    body = NULL,
    fullMetadata = TRUE,
    useVerbose = verbose
  )

  expect_true(!is.null(result$entity$`Id@odata.type`))
})
