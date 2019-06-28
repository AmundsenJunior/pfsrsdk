#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSampleIntermediateData.

context("Tests for getExperimentSampleIntermediateData")

test_that(paste("test getExperimentSampleIntermediateData() on semantic version:", con$coreApi$semVer), {
  result <- getExperimentSampleIntermediateData(con$coreApi, data$experimentType, data$experimentAssayType, data$intermediateDataName, data$experimentSampleBarcode, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(length(result$entity$barcodes), 0)

  expect_false(is.null(result$entity[[data$intermediateDataName]]), FALSE)
})
