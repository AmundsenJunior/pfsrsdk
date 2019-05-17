#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for sample lot creation.
#'
context("Tests for createSampleLot")

test_that(paste("test createSampleLot() on:", env$auth), {
  result <- createSampleLot(con$coreApi,
    data$sampleType,
    data$sampleBarcode,
    body = NULL,
    fullMetadata = TRUE,
    useVerbose = verbose
  )
  expect_gt(httr::content(result$response)$CI_LOT_NUM, 0)
  expect_true(!is.null(result$entity$`Id@odata.type`))
})
