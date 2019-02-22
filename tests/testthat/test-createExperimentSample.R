#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description Tests for Experiment creation.
#'
context("Tests for createExperimentSample")

test_that(paste("test createExperimentSample() on: ", env$auth), {
  samp <- createExperimentSample(
    con$coreApi,
    data$experimentType,
    data$experimentBarcode,
    data$sampleLotBarcode
  )

  expect_that(httr::http_status(samp$response)$reason, equals("Created"))
})
