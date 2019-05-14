#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for isExperimentPublished.

context("Tests for isExperimentPublished")

test_that(paste("test isExperimentPublished() on:", env$auth), {
  result <- isExperimentPublished(con$coreApi, data$experimentType, data$experimentBarcode, useVerbose = verbose)

  expect_type(result$status, "logical")
})
