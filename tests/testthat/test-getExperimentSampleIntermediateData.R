#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSampleIntermediateData.

context("Tests for getExperimentSampleIntermediateData")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getExperimentSampleIntermediateData() on:", env$auth), {
  result <- CoreAPIV2::getExperimentSampleIntermediateData(con$coreApi, data$experimentType, data$experimentAssayType, data$intermediateDataName, data$experimentSampleBarcode, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(length(result$entity$barcodes), 0)

  expect_false(is.null(result$entity$CI_BITTERNESS_IBU_INTERMEDIATE), FALSE)
})
