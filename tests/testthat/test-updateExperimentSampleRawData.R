#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for updateExperimentSampleRawData.

context("Tests for updateExperimentSampleRawData")

test_that(paste("test updateExperimentSampleRawData() on:", env$auth), {
  result <- updateExperimentSampleRawData(con$coreApi, data$experimentContainerBarcode, data$rawDataCellNum, data$rawDataValues, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_equal(result$entity[names(data$rawDataValues[1])][[1]], data$rawDataValues[[1]])
})
