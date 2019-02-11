#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSampleAssayData.

context("Tests for getExperimentSampleAssayData")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getExperimentSampleAssayData() on:", env$auth), {
  result <- CoreAPIV2::getExperimentSampleAssayData(con$coreApi, data$experimentAssayType, data$experimentSampleBarcode, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(
      length(
        result$entity[[1]]$Barcode
      ), 0
  )
})
