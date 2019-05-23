#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getExperimentSamples.

context("Tests for getExperimentSamples")

test_that(paste("test getExperimentSamples() on:", env$auth), {
  result <- getExperimentSamples(con$coreApi, data$experimentType, data$experimentBarcode, fullMetadata = FALSE, useVerbose = verbose)

  expect_equal(result$response$response$status_code, 200)

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "REV_EXPERIMENT_EXPERIMENT_SAMPLE"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "EXPERIMENT_SAMPLES"
    }
  )

  expect_gt(
    length(
      lapply(
        result$response$content[[expansion]],
        FUN = function(x)
          x$Barcode
      )
    ), 0
  )
})

test_that(paste("getExperimentSamples returns successful with fullMetadata on:", env$auth), {
  result <- getExperimentSamples(con$coreApi, data$experimentType, data$experimentBarcode, fullMetadata = TRUE, useVerbose = verbose)

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "REV_EXPERIMENT_EXPERIMENT_SAMPLE"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "EXPERIMENT_SAMPLES"
    }
  )

  expect_true(!is.null(result$response$content[[expansion]][[1]]$`Id@odata.type`))
})
