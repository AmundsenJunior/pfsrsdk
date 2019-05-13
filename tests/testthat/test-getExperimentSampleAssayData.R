#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSampleAssayData.

context("Tests for getExperimentSampleAssayData")

test_that(paste("test getExperimentSampleAssayData() on:", env$auth), {
  result <- getExperimentSampleAssayData(con$coreApi, data$experimentAssayType, data$experimentSampleBarcode, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(
    length(
      result$entity[[1]]$Barcode
    ), 0
  )
})
