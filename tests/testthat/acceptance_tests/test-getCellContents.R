#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getCellContents.

context("Tests for getCellContents")

test_that(paste("test getCellContents() on:", env$auth), {
  result <- getCellContents(con$coreApi, data$containerCellId, fullMetadata = FALSE, useVerbose = verbose)
  expect_equal(result$response$status_code, 200)

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "CONTENT"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "CELL_CONTENTS"
    }
  )

  expect_gt(length(result$entity[[expansion]][[1]]), 0)
})

test_that(paste("getCellContents returns successful with fullMetadata on:", env$auth), {
  result <- getCellContents(con$coreApi, data$containerCellId, fullMetadata = TRUE, useVerbose = verbose)

  expect_true(!is.null(result$entity$`Id@odata.type`))
})
