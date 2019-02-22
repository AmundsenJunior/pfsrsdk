#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getExperimentContainerContents.

context("Tests for getExperimentContainerContents")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getExperimentContainerContents() on:", env$auth), {
  result <- getExperimentContainerContents(con$coreApi, data$experimentContainerBarcode, data$experimentContainerType, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expansion <- switch(EXPR = substr(con$coreApi$semVer, 1, 1),
    "2" = "REV_IMPL_CONTAINER_CELL",
    "3" = "CELLS",
    print("CELLS")
  )

  expect_gt(length(result$entity[[1]][[expansion]]), 0)
})
