#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentWellContents.

context("Tests for getExperimentWellContents")

test_that(paste("test getWellContents() on:", env$auth), {
  result <- getExperimentWellContents(con$coreApi, data$experimentContainerBarcode, data$experimentContainerWellNum, data$experimentContainerType, fullMetadata = TRUE, verbose)

  expect_equal(result$response$status_code, 200)
  expect_true(!is.null(result$entity$`Id@odata.type`))

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
