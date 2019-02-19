#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for updateExperimentSampleRawData.

context("Tests for updateExperimentSampleRawData")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test updateExperimentSampleRawData() on:", env$auth), {
  result <- CoreAPIV2::updateExperimentSampleRawData(con$coreApi, data$experimentContainerBarcode, data$rawDataCellNum, data$rawDataValues, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_equal(result$entity[names(data$rawDataValues[1])][[1]], data$rawDataValues[[1]])
})
