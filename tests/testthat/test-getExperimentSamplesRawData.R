#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSamplesRawData.

context("Tests for getExperimentSamplesRawData")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getExperimentSamplesRawData() on:", env$auth), {
  result <- getExperimentSamplesRawData(con$coreApi, data$experimentContainerBarcode, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(length(result$entity$DATA_VALUE), 0)
})
