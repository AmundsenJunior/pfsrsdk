#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getContainerCellIds.

context("Tests for getContainerCellIds")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getContainerCellIds() on:", env$auth), {
  result <- CoreAPIV2::getContainerCellIds(con$coreApi, data$containerBarcode, containerType = data$containerType, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(unlist(length(result$entity)), 0)
})
