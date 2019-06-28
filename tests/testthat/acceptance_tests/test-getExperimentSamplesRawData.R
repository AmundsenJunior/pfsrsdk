#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSamplesRawData.

context("Tests for getExperimentSamplesRawData")

test_that(paste("test getExperimentSamplesRawData() on semantic version:", con$coreApi$semVer), {
  result <- getExperimentSamplesRawData(con$coreApi, data$experimentContainerBarcode, fullMetadata = FALSE, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(length(result$entity$DATA_VALUE), 0)
})

test_that(paste("getExperimentSamplesRawData returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  result <- getExperimentSamplesRawData(con$coreApi, data$experimentContainerBarcode, fullMetadata = TRUE, useVerbose = verbose)

  expect_true(!is.null(result$entity$`DATA_VALUE@odata.type`[1]))
})
