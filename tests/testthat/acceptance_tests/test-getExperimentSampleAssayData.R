#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSampleAssayData.

context("Tests for getExperimentSampleAssayData")

test_that(paste("test getExperimentSampleAssayData() on:", env$auth), {
  result <- getExperimentSampleAssayData(con$coreApi, data$experimentAssayType, data$experimentSampleBarcode, fullMetadata = FALSE, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(
    length(
      result$entity[[1]]$Barcode
    ), 0
  )
})

test_that(paste("getExperimentSampleAssayData returns successful with fullMetadata on:", env$auth), {
  result <- getExperimentSampleAssayData(con$coreApi, data$experimentAssayType, data$experimentSampleBarcode, fullMetadata = TRUE, useVerbose = verbose)

  expect_true(!is.null(result$entity[[1]]$`CI_BITTERNESS_IBU@odata.type`))
})
