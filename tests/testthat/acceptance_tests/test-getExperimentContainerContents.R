#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentContainerContents.

context("Tests for getExperimentContainerContents")

test_that(paste("test getExperimentContainerContents() on semantic version:", con$coreApi$semVer), {
  result <- getExperimentContainerContents(con$coreApi, data$experimentContainerBarcode, data$experimentContainerType, fullMetadata = FALSE, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "REV_IMPL_CONTAINER_CELL"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "CELLS"
    }
  )

  expect_gt(length(result$entity[[1]][[expansion]]), 0)
})

test_that(paste("getExperimentContainerContents returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  result <- getExperimentContainerContents(con$coreApi, data$experimentContainerBarcode, data$experimentContainerType, fullMetadata = TRUE, useVerbose = verbose)

  expect_true(!is.null(result$entity[[1]]$`Id@odata.type`))
})
