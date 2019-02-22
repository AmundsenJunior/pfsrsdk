#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getExperimentContainerCellIds.

context("Tests for getExperimentContainerCellIds")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getExperimentContainerCellIds() on:", env$auth), {
  result <- getExperimentContainerCellIds(con$coreApi, data$experimentContainerBarcode, data$experimentContainerType, verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(unlist(length(result$entity)), 0)
})
