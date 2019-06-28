#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for Experiment creation.

context("Tests for createExperimentContainer")

test_that(paste("test createExperimentContainer() on a single well container in semantic version:", con$coreApi$semVer), {
  ec <- createExperimentContainer(con$coreApi,
    data$experimentType,
    data$experimentBarcodeUnpublishedExperiment,
    data$singleWellContainerBarcode,
    body = NULL,
    fullMetadata = FALSE,
    useVerbose = FALSE
  )

  expect_that(httr::http_status(ec$response)$category, equals("Success"))
})

test_that(paste("test createExperimentContainer() on a multi well container in semantic version:", con$coreApi$semVer), {
  # add multi well container

  ec <- createExperimentContainer(con$coreApi,
    data$experimentType,
    data$experimentBarcodeUnpublishedExperiment,
    data$multiWellContainerBarcode,
    body = NULL,
    fullMetadata = FALSE,
    useVerbose = FALSE
  )

  expect_that(httr::http_status(ec$response)$category, equals("Success"))
})

test_that(paste("createExperimentContainer returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  ec <- createExperimentContainer(con$coreApi,
    data$experimentType,
    data$experimentBarcodeUnpublishedExperiment,
    data$multiWellContainerBarcode,
    body = NULL,
    fullMetadata = TRUE,
    useVerbose = FALSE
  )

  expect_true(!is.null(ec$entity$`Id@odata.type`))
})
