#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for experiment publish.
#'
context("Tests for experimentPublish")

test_that(paste("test experimentPublish() on:", env$auth), {
  result <- CoreAPIV2::experimentPublish(con$coreApi, data$experimentType, data$experimentBarcode, useVerbose = verbose)
  expect_equal(httr::content(result$response)$response$data$values$PUBLISHED$stringData, "true")
})
