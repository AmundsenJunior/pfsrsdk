#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for Experiment creation.
#'
context("Tests for createExperimentContainer")

test_that(paste("test createExperimentContainer() on a single well container in:", env$auth), {
  
  
  ec <- CoreAPIV2::createExperimentContainer(con$coreApi,
                                             data$experimentType,
                                             data$experimentBarcode,
                                             data$singleWellContainerBarcode,
                                             body = NULL,
                                             useVerbose = FALSE
  )
  expect_that(httr::http_status(ec$response)$category, equals("Success"))
})

test_that(paste("test createExperimentContainer() on a multi well container in:", env$auth), {
  # add multi well container
  
  ec <- CoreAPIV2::createExperimentContainer(con$coreApi,
                                             data$experimentType,
                                             data$experimentBarcode,
                                             data$multiWellContainerBarcode,
                                             body = NULL,
                                             useVerbose = FALSE
  )
  
  expect_that(httr::http_status(ec$response)$category, equals("Success"))
})
