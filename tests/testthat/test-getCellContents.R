#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getCellContents.

context("Tests for getCellContents")

test_that(paste("test getCellContents() on:", env$auth), {
  result <- getCellContents(con$coreApi, data$containerCellId, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expansion <- switch(EXPR = substr(con$coreApi$semVer, 1, 1),
    "2" = "CONTENT",
    "3" = "CELL_CONTENTS",
    print("CELL_CONTENTS")
  )

  expect_gt(length(result$entity[[expansion]][[1]]), 0)
})
