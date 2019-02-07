#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for Experiment creation.
#'
context("Tests for createExperiment")

test_that(paste("test createExperiment() on: ", env$auth), {
  expt <- CoreAPIV2::createExperiment(con$coreApi,
    data$experimentType,
    data$experimentAssayType,
    data$experimentAssayBarcode,
    data$experimentProtocolType,
    data$experimentProtocolBarcode,
    body = NULL, useVerbose = verbose
  )

  expect_that(httr::http_status(expt$response)$reason, equals("Created"))
})
