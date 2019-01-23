
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for experiment Unpublish.
#'
context("Tests for experimentUnpublish")


lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test experimentUnpublish() on:", x), {
    result <- CoreAPIV2::experimentUnpublish(con$coreApi, EXPERIMENTTYPE, EXPERIMENTBARCODE, useVerbose = verbose)
    expect_equal(httr::content(result$response)$response$data$values$PUBLISHED$stringData, 'false')
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
