#' @author Natasha Mora natasha.mora@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for experiment publish.
#'
context("Tests for experimentPublish")

test_that(paste("test experimentPublish() on:", env$auth), {
  result <- experimentPublish(con$coreApi, data$experimentType, data$experimentPublishBarcode, useVerbose = verbose)

  case(
    grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expect_equal(httr::content(result$response)$response$data$values$PUBLISHED$stringData, "true")
    },
    grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
      expect_equal(httr::content(result$response)$PUBLISHED, TRUE)
      expect_error(experimentPublish(con$coreApi, data$experimentType, data$experimentPublishFailBarcode, useVerbose = verbose))
    }
  )
})
