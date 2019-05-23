#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getContainerContents.

context("Tests for getContainerContents")

test_that(paste("test getContainerContents() on:", env$auth), {
  result <- getContainerContents(con$coreApi, data$containerBarcode, data$containerType, fullMetadata = FALSE, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "REV_IMPL_CONTAINER_CELL"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "CELLS"
    }
  )

  expect_gt(length(result$entity[[expansion]][[1]]), 0)
})

test_that(paste("getContainerContents returns successful with fullMetadata on:", env$auth), {
  result <- getContainerContents(con$coreApi, data$containerBarcode, data$containerType, fullMetadata = TRUE, useVerbose = verbose)

  expect_true(!is.null(result$entity$`Id@odata.type`))
})
