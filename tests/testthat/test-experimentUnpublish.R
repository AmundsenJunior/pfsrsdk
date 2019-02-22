#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for experiment Unpublish.
#'
context("Tests for experimentUnpublish")

test_that(paste("test experimentUnpublish() on:", env$auth), {
  result <- experimentUnpublish(con$coreApi, data$experimentType, data$experimentBarcode, useVerbose = verbose)
  expect_equal(httr::content(result$response)$response$data$values$PUBLISHED$stringData, "false")
})
