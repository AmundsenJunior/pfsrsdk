#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for experiment publish.
#'
context("Tests for experimentPublish")

test_that(paste("test experimentPublish() on:", env$auth), {
  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      result <- experimentPublish(con$coreApi, data$experimentType, data$experimentPublishUnpublishBarcode, useVerbose = verbose)

      expect_type(as.logical(httr::content(result$response)$response$data$values$PUBLISHED$stringData), "logical")
      expect_equal(httr::content(result$response)$response$data$values$PUBLISHED$stringData, "true")
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      booleanResult <- isExperimentPublished(con$coreApi, data$experimentType, data$experimentPublishUnpublishBarcode, useVerbose = verbose)

      if (booleanResult$status == TRUE) {
        experimentUnpublish(con$coreApi, data$experimentType, data$experimentPublishUnpublishBarcode, useVerbose = verbose)
      }

      result <- experimentPublish(con$coreApi, data$experimentType, data$experimentPublishUnpublishBarcode, useVerbose = verbose)

      expect_type(httr::content(result$response)$PUBLISHED, "logical")
      expect_true(httr::content(result$response)$PUBLISHED, TRUE)
      expect_error(experimentPublish(con$coreApi, data$experimentType, data$experimentPublishFailBarcode, useVerbose = verbose))
    }
  )
})
