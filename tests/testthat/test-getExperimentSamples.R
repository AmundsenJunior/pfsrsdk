#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getExperimentSamples.

context("Tests for getExperimentSamples")

test_that(paste("test getExperimentSamples() on:", env$auth), {
  result <- getExperimentSamples(con$coreApi, data$experimentType, data$experimentBarcode, useVerbose = verbose)

  expect_equal(result$response$response$status_code, 200)

  expansion <- switch(EXPR = substr(con$coreApi$semVer, 1, 1),
    "2" = "REV_EXPERIMENT_EXPERIMENT_SAMPLE",
    "3" = "EXPERIMENT_SAMPLES",
    print("EXPERIMENT_SAMPLES")
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
