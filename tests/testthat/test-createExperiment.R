#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for Experiment creation.
#'
context("Tests for createExperiment")

test_that(paste("test createExperiment() on: ", env$auth), {
  expt <- createExperiment(con$coreApi,
    data$experimentType,
    data$experimentAssayType,
    data$experimentAssayBarcode,
    data$experimentProtocolType,
    data$experimentProtocolBarcode,
    body = NULL,
    fullMetadata = TRUE,
    useVerbose = verbose
  )

  expect_that(httr::http_status(expt$response)$reason, equals("Created"))
  expect_true(!is.null(expt$entity$`Id@odata.type`))
})
