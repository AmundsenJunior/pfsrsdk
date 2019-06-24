#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getExperimentContainerCellIds.

context("Tests for getExperimentContainerCellIds")

test_that(paste("test getExperimentContainerCellIds() on semantic version:", con$coreApi$semVer), {
  result <- getExperimentContainerCellIds(con$coreApi, data$experimentContainerBarcode, data$experimentContainerType, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(unlist(length(result$entity)), 0)
})
