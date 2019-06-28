#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentContainersAndExperimentSamples.

context("Tests for getExperimentContainersAndExperimentSamples")

test_that(paste("test getExperimentContainersAndExperimentSamples() returns successful on semantic version:", con$coreApi$semVer), {
  result <- getExperimentContainersAndExperimentSamples(con$coreApi,
    data$experimentType,
    data$experimentBarcode,
    includeExperimentSamples = FALSE,
    fullMetadata = FALSE,
    useVerbose = verbose
  )

  expect_equal(result$response$status_code, 200)

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      contExpansion <- "REV_CONTAINER_EXPERIMENT_EXPERIMENT_CONTAINER"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      contExpansion <- "EXPERIMENT_CONTAINERS"
    }
  )

  containerBarcode <- sapply(
    result$entity[[contExpansion]],
    FUN = function(x) {
      x$Barcode
    }
  )

  expect_gt(length(containerBarcode), 0)

  containerType <- sapply(
    result$entity[[contExpansion]],
    FUN = function(x) {
      x$CONTAINER$Barcode
    }
  )

  expect_gt(length(containerType), 0)
})

test_that(paste("getExperimentContainersAndExperimentSamples including experiment samples on semantic version:", con$coreApi$semVer), {
  result <- getExperimentContainersAndExperimentSamples(con$coreApi,
    data$experimentType,
    data$experimentBarcode,
    includeExperimentSamples = TRUE,
    fullMetadata = FALSE,
    useVerbose = verbose
  )

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      contExpansion <- "REV_CONTAINER_EXPERIMENT_EXPERIMENT_CONTAINER"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      contExpansion <- "EXPERIMENT_CONTAINERS"
    }
  )

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      exptSampleExpansion <- "REV_EXPERIMENT_CONTAINER_EXPERIMENT_SAMPLE"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      exptSampleExpansion <- "EXPERIMENT_SAMPLES"
    }
  )

  exptSample <- unlist(lapply(
    result$entity[[contExpansion]],
    FUN = function(x) {
      sapply(x[[exptSampleExpansion]],
        FUN = function(y) {
          y$Barcode
        }
      )
    }
  ))

  expect_gt(length(exptSample), 0)
})

test_that(paste("getExperimentContainersAndExperimentSamples returns successful with fullMetadata on semantic version:", con$coreApi$semVer), {
  result <- getExperimentContainersAndExperimentSamples(con$coreApi,
    data$experimentType,
    data$experimentBarcode,
    includeExperimentSamples = FALSE,
    fullMetadata = TRUE,
    useVerbose = verbose
  )

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      contExpansion <- "REV_CONTAINER_EXPERIMENT_EXPERIMENT_CONTAINER"
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      contExpansion <- "EXPERIMENT_CONTAINERS"
    }
  )

  expect_true(!is.null(result$entity[[contExpansion]][[1]]$`Id@odata.type`))
})
