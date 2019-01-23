
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description Tests for experiment publish.
#'
context("Tests for experimentPublish")


lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test experimentPublish() on:", x), {
    result <- CoreAPIV2::experimentPublish(con$coreApi, EXPERIMENTTYPE, EXPERIMENTBARCODE, useVerbose = verbose)
    expect_equal(httr::content(result$response)$response$data$values$PUBLISHED$stringData, 'true')
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
