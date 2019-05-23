#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for Experiment creation.

context("Tests for createExperimentSample")

test_that(paste("test createExperimentSample() on: ", env$auth), {
  samp <- createExperimentSample(
    con$coreApi,
    data$experimentType,
    data$experimentBarcode,
    data$sampleLotBarcode,
    fullMetadata = FALSE,
    useVerbose = verbose
  )

  expect_that(httr::http_status(samp$response)$reason, equals("Created"))
})

test_that(paste("createExperimentSample returns successful with fullMetadata on:", env$auth), {
  samp <- createExperimentSample(
    con$coreApi,
    data$experimentType,
    data$experimentBarcode,
    data$sampleLotBarcode,
    fullMetadata = TRUE,
    useVerbose = verbose
  )

  expect_true(!is.null(samp$entity$`Id@odata.type`))
})
