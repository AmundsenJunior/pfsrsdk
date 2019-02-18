#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getWellContents.

context("Tests for getWellContents")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getWellContents() on:", env$auth), {
  result <- CoreAPIV2::getWellContents(con$coreApi, data$containerBarcode, data$containerWellNum, data$containerType, verbose)

  expect_equal(result$response$status_code, 200)

  CoreAPIV2::case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "CONTENT"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "CELL_CONTENTS"
    }
  )

  expect_gt(length(result$entity[[expansion]][[1]]), 0)
})
