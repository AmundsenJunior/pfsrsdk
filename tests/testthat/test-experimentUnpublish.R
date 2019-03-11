#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for experiment Unpublish.
#'
context("Tests for experimentUnpublish")

test_that(paste("test experimentUnpublish() on:", env$auth), {
  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      result <- experimentUnpublish(con$coreApi, data$experimentType, data$experimentPublishUnpublishBarcode, useVerbose = verbose)

      expect_type(as.logical(httr::content(result$response)$response$data$values$PUBLISHED$stringData), "logical")
      expect_equal(httr::content(result$response)$response$data$values$PUBLISHED$stringData, "false")
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      booleanResult <- isExperimentPublished(con$coreApi, data$experimentType, data$experimentPublishUnpublishBarcode, useVerbose = verbose)

      if (booleanResult$status == FALSE) {
        experimentPublish(con$coreApi, data$experimentType, data$experimentPublishUnpublishBarcode, useVerbose = verbose)
      }

      result <- experimentUnpublish(con$coreApi, data$experimentType, data$experimentPublishUnpublishBarcode, useVerbose = verbose)

      expect_type(httr::content(result$response)$PUBLISHED, "logical")
      expect_false(httr::content(result$response)$PUBLISHED, FALSE)
      expect_error(experimentUnpublish(con$coreApi, data$experimentType, data$experimentUnpublishFailBarcode, useVerbose = verbose))
    }
  )
})
