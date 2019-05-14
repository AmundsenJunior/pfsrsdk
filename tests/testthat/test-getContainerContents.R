#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getContainerContents.

context("Tests for getContainerContents")

test_that(paste("test getContainerContents() on:", env$auth), {
  result <- getContainerContents(con$coreApi, data$containerBarcode, data$containerType, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expansion <- switch(EXPR = substr(con$coreApi$semVer, 1, 1),
    "2" = "REV_IMPL_CONTAINER_CELL",
    "3" = "CELLS",
    print("CELLS")
  )

  expect_gt(length(result$entity[[expansion]][[1]]), 0)
})
