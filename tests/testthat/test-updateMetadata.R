#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for updateMetadata

context("Tests for updateMetadata")

test_that(paste("test updateMetadata() on: ", env$auth), {
  meta <- updateMetadata(con$coreApi, useVerbose = verbose)
  expect_equivalent(meta$response$status_code, 200, all = verbose)
})
