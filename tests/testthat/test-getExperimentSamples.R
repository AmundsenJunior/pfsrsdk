#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSamples.

context("Tests for getExperimentSamples")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getExperimentSamples() on:", env$auth), {
  result <- CoreAPIV2::getExperimentSamples(con$coreApi, data$experimentType, data$experimentBarcode, useVerbose = verbose)

  expect_equal(result$response$response$status_code, 200)
})
