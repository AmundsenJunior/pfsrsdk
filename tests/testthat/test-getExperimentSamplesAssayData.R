#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSampleAssayData.

context("Tests for getExperimentSampleAssayData")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getExperimentSampleAssayData() on:", env$auth), {
  result <- CoreAPIV2::getExperimentSamplesAssayData(con$coreApi, data$assayType, data$experimentSampleBarcode, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(
    unlist(
      length(
        lapply(
          result$entity,
          FUN = function(x)
            x$Barcode
        )
      )
    ), 0
  )
})
