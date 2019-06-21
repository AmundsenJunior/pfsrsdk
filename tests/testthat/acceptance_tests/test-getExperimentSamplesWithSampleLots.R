#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentSamplesWithSampleLots

context("Tests for getExperimentSamplesWithSampleLots")

test_that(paste("test getExperimentSamplesWithSampleLots() returns successful on semantic version:", con$coreApi$semVer), {
  result <- getExperimentSamplesWithSampleLots(con$coreApi,
    data$experimentType,
    data$experimentBarcode,
    fullMetadata = FALSE,
    useVerbose = verbose
  )

  expect_equal(result$response$status_code, 200)

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "REV_EXPERIMENT_EXPERIMENT_SAMPLE"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "EXPERIMENT_SAMPLES"
    }
  )

  sampleLotName <- sapply(
    result$entity[[expansion]],
    FUN = function(x) {
      x$ENTITY$Name
    }
  )

  expect_gt(length(sampleLotName), 0)
})

test_that(paste("getExperimentSamplesWithSampleLots returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  result <- getExperimentSamplesWithSampleLots(con$coreApi,
    data$experimentType,
    data$experimentBarcode,
    fullMetadata = TRUE,
    useVerbose = verbose
  )

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "REV_EXPERIMENT_EXPERIMENT_SAMPLE"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expansion <- "EXPERIMENT_SAMPLES"
    }
  )

  expect_true(!is.null(result$entity[[expansion]][[1]]$`Id@odata.type`))
})
